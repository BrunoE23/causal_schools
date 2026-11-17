# Directory-only national school/preschool series; no SAE files or SAE caches.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
clean <- file.path(root, "data/clean/school_directory_preschool")
figures <- file.path(root, "output/figures/kinder_capacity")
reports <- file.path(root, "output/reports/kinder_capacity")
for (p in c(clean, figures, reports)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
manifest <- list()
field_audit <- list()
d <- rbindlist(lapply(2015:2025, function(year) {
  path <- list.files(file.path(data_root, "data/raw/school_directory", year), pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(path) == 1L, file.info(path)$size < 1e8)
  header <- names(fread(path, nrows = 0L))
  fields <- toupper(header)
  selected <- c("RBD", "NOM_RBD", "COD_REG_RBD", "COD_COM_RBD", "NOM_COM_RBD", "COD_DEPE", "ESTADO_ESTAB", "MATRICULA", grep("^ENS_", fields, value = TRUE))
  selected <- header[match(selected, fields)]
  stopifnot(!anyNA(selected))
  x <- fread(path, select = selected, showProgress = FALSE)
  setnames(x, toupper(names(x)))
  stopifnot(!anyDuplicated(x$RBD), !anyNA(x$ESTADO_ESTAB), !anyNA(x$MATRICULA))
  ens <- grep("^ENS_", names(x), value = TRUE)
  x[, preschool := rowSums(.SD == 10L, na.rm = TRUE) > 0, .SDcols = ens]
  manifest[[as.character(year)]] <<- data.table(year, path, bytes = file.info(path)$size)
  field_audit[[as.character(year)]] <<- data.table(year, has_total_enrollment = "MAT_TOTAL" %in% fields,
    has_preschool_enrollment_count = "MAT_ENS_1" %in% fields,
    enrollment_by_teaching_fields = paste(grep("^MAT_ENS_", fields, value = TRUE), collapse = ","),
    capacity_like_fields = paste(grep("CUPO|VACANT|CAPAC", fields, value = TRUE), collapse = ","))
  x[, .(rbd = RBD, year, school_name = NOM_RBD, region = COD_REG_RBD,
    comuna_code = COD_COM_RBD, comuna = NOM_COM_RBD, dependency = COD_DEPE,
    state = ESTADO_ESTAB, enrollment_indicator = MATRICULA,
    operating = ESTADO_ESTAB == 1L & MATRICULA == 1L, preschool)]
}))
d[, active_preschool := operating & preschool]
stopifnot(!anyDuplicated(d[, .(rbd, year)]))
history <- d[, .(first_record = min(year),
  first_operating = if (any(operating)) min(year[operating]) else NA_integer_,
  first_active_preschool = if (any(active_preschool)) min(year[active_preschool]) else NA_integer_), by = rbd]
first_op <- merge(d[operating == TRUE][order(year), .SD[1L], by = rbd], history[, .(rbd, first_record)], by = "rbd")
new_schools <- first_op[year > 2015L]
stopifnot(!anyNA(new_schools$dependency))
new_counts <- new_schools[, .(first_observed_operating = .N,
  new_operation_with_preschool = sum(preschool),
  new_operation_with_preschool_nonprivate = sum(preschool & dependency != 4L),
  new_operation_with_preschool_private = sum(preschool & dependency == 4L),
  new_record_same_year_as_operation = sum(first_record == year),
  previously_registered_first_operation = sum(first_record < year)), by = year]
stock <- d[, .(operating_schools = sum(operating), active_preschool_schools = sum(active_preschool),
  active_preschool_nonprivate = sum(active_preschool & dependency != 4L),
  active_preschool_private = sum(active_preschool & dependency == 4L),
  preschool_flag_without_operation = sum(preschool & !operating)), by = year]
flow_rows <- list()
for (current_year in 2016:2025) {
  before <- d[year == current_year - 1L, .(rbd, op_before = operating, pre_before = active_preschool)]
  after <- d[year == current_year, .(rbd, op_after = operating, pre_after = active_preschool)]
  z <- merge(before, after, by = "rbd", all = TRUE)
  for (v in c("op_before", "pre_before", "op_after", "pre_after")) set(z, which(is.na(z[[v]])), v, FALSE)
  z <- merge(z, history, by = "rbd", all.x = TRUE)
  z[, `:=`(year = current_year, gain = !pre_before & pre_after, loss = pre_before & !pre_after)]
  z[, change_type := fcase(
    gain & first_operating == current_year, "First observed school operation with preschool",
    gain & op_before, "Continuing operating school adds preschool enrollment",
    gain, "Previously observed school returns with preschool",
    loss & op_after, "Continuing operating school drops preschool enrollment",
    loss, "Preschool school no longer observed operating",
    default = "No change in active preschool status")]
  flow_rows[[as.character(current_year)]] <- z[gain | loss]
}
transitions <- rbindlist(flow_rows)
flows <- transitions[, .(
  new_operation_with_preschool = sum(change_type == "First observed school operation with preschool"),
  existing_add_preschool = sum(change_type == "Continuing operating school adds preschool enrollment"),
  of_which_first_preschool_observation = sum(change_type == "Continuing operating school adds preschool enrollment" & first_active_preschool == year),
  returning_operation_with_preschool = sum(change_type == "Previously observed school returns with preschool"),
  existing_drop_preschool = sum(change_type == "Continuing operating school drops preschool enrollment"),
  no_longer_operating_with_preschool = sum(change_type == "Preschool school no longer observed operating"),
  gaining_preschool_schools = sum(gain), losing_preschool_schools = sum(loss)), by = year]
annual <- merge(stock, new_counts, by = "year", all.x = TRUE)
annual <- merge(annual, flows[, !"new_operation_with_preschool"], by = "year", all.x = TRUE)
setorder(annual, year)
annual[, net_preschool_school_change := active_preschool_schools - shift(active_preschool_schools)]
annual[, accounting_residual := net_preschool_school_change - (new_operation_with_preschool + existing_add_preschool + returning_operation_with_preschool - existing_drop_preschool - no_longer_operating_with_preschool)]
stopifnot(all(annual[year > 2015L, accounting_residual] == 0L),
  identical(flows[order(year)]$new_operation_with_preschool, new_counts[order(year)]$new_operation_with_preschool))
fwrite(annual, file.path(clean, "annual_directory_preschool_accounting.csv"))
# Prefer current metadata; retain preceding-year names for directory exits.
labels <- unique(rbindlist(list(d[, .(rbd, year, school_name, comuna, dependency)],
  d[, .(rbd, year = year + 1L, school_name, comuna, dependency)])), by = c("rbd", "year"))
fwrite(merge(transitions, labels, by = c("rbd", "year"), all.x = TRUE),
  file.path(clean, "school_preschool_status_changes.csv"))
fwrite(new_schools[order(year, rbd)], file.path(clean, "schools_first_observed_operating.csv"))
fwrite(d, file.path(clean, "directory_school_year.csv"))
fwrite(rbindlist(manifest), file.path(clean, "input_manifest.csv"))
fwrite(rbindlist(field_audit), file.path(clean, "directory_field_availability.csv"))

blue <- "#2878A0"
green <- "#39958C"
red <- "#C05D54"
ink <- "#243347"
plot_directory <- function() {
  par(mfrow = c(2, 1), mar = c(4.3, 4.4, 3.2, 1.2), oma = c(4.0, 0, 0.4, 0),
    family = "sans", las = 1, col.axis = ink, col.lab = ink, col.main = ink)
  plot(annual$year, annual$active_preschool_schools, type = "o", pch = 19, col = blue, lwd = 2,
    xaxt = "n", ylim = range(annual$active_preschool_schools) + c(-40, 65),
    xlab = "Directory year", ylab = "Number of schools", main = "Operating schools with preschool enrollment: national stock")
  axis(1, at = annual$year)
  text(annual$year, annual$active_preschool_schools, annual$active_preschool_schools, pos = 3, cex = 0.8, col = ink)
  z <- annual[year > 2015L]
  v <- rbind(z$new_operation_with_preschool, z$existing_add_preschool, z$existing_drop_preschool)
  bp <- barplot(v, beside = TRUE, names.arg = z$year, col = c(blue, green, red), border = NA,
    ylim = c(0, max(v) * 1.32), xlab = "Directory year", ylab = "Number of schools",
    main = "New school operation and changes at continuing schools")
  text(bp, v, labels = v, pos = 3, cex = 0.75, col = ink)
  legend("topright", c("New operation with preschool", "Existing school adds preschool", "Existing school drops preschool"),
    fill = c(blue, green, red), bty = "n", cex = 0.82)
  mtext("Directories only, 2015-2025. Preschool covers all educacion parvularia levels; school counts do not measure available seats.",
    side = 1, outer = TRUE, line = 1, cex = 0.79, col = ink)
  mtext("First operation is first observed since 2015, not a verified founding date. Returning/ceasing operation is accounted for in the report.",
    side = 1, outer = TRUE, line = 2.3, cex = 0.76, col = ink)
}
png(file.path(figures, "directory_only_preschool_changes.png"), width = 2300, height = 1650, res = 180)
plot_directory()
dev.off()
pdf(file.path(figures, "directory_only_preschool_changes.pdf"), width = 12.8, height = 9.2)
plot_directory()
dev.off()

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
htmltable <- function(x) paste0('<div class="table"><table><thead><tr>', paste0("<th>", esc(names(x)), "</th>", collapse = ""),
  "</tr></thead><tbody>", paste(apply(x, 1L, function(row) paste0("<tr>", paste0("<td>", esc(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table></div>")
tab <- annual[year > 2015L, .(Year = year, `First school operation: all` = first_observed_operating,
  `Of these: preschool` = new_operation_with_preschool, `Existing schools add preschool` = existing_add_preschool,
  `Returning schools with preschool` = returning_operation_with_preschool,
  `Existing schools drop preschool` = existing_drop_preschool,
  `Preschool schools cease observed operation` = no_longer_operating_with_preschool,
  `Net preschool school change` = net_preschool_school_change, `Active preschool schools` = active_preschool_schools)]
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>School directories: preschool provision and school entry</title><style>body{font:15px/1.5 system-ui,sans-serif;background:#f4f6f8;color:#243347;margin:0}main{max-width:1350px;margin:24px auto;background:white;padding:28px;border-radius:8px}h1{font-size:27px}h2{font-size:21px}.table{overflow:auto}table{border-collapse:collapse;width:100%;font-size:13px;font-variant-numeric:tabular-nums}th,td{padding:9px;border-bottom:1px solid #dfe5eb;text-align:right}th{background:#e9eff4}img{width:100%;height:auto}li{margin:8px 0}</style></head><body><main>',
  '<h1>School directories only: preschool provision and school entry, 2015-2025</h1>',
  '<p>All years are annual directory snapshots. No SAE data, participation restrictions, or process-year definitions enter this exercise.</p>',
  '<p><strong>The directories do not provide a consistent annual series of kinder capacity or available seats.</strong> They support school counts and changes in observed preschool enrollment status. The earlier greater-than-10-seat calculation cannot be performed from these directories alone.</p>',
  '<img src="../../figures/kinder_capacity/directory_only_preschool_changes.png" alt="Directory-only preschool stock and school status changes over time">',
  '<h2>Exact accounting of the change in preschool school numbers</h2>', htmltable(tab),
  '<p>First operation with preschool + additions at continuing operating schools + returning operations with preschool - drops at continuing schools - preschool schools ceasing observed operation equals the net annual change exactly. Baseline 2015 has ', annual[year == 2015L, active_preschool_schools], ' operating preschool schools.</p>',
  '<h2>Definitions and data limits</h2><ul>',
  '<li>Operating means ESTADO_ESTAB=1 and MATRICULA=1. Preschool means at least one ENS teaching code=10. Both conditions are required for the active preschool stock. ENS=10 covers all educacion parvularia levels, including nursery/middle levels, pre-kinder and kinder; it cannot isolate kinder.</li>',
  '<li>First observed operation is the first operating/enrollment record in the available 2015-2025 history. It is a candidate opening, not a verified legal founding date. The left-censored 2015 baseline has no creation count. Identifier changes and operation before the baseline remain possible.</li>',
  '<li>An existing school adds/drops preschool when it operates in both adjacent years and its preschool enrollment flag changes. Addition need not mean first-ever provision; it can reintroduce preschool. It does not quantify places or classrooms.</li>',
  '<li>Returning operation requires an earlier operating observation, no operation in the preceding year, and current operation with preschool. Ceasing observed operation includes directory absence or failure of the operating-with-enrollment condition and does not establish permanent closure.</li>',
  '<li>MAT_TOTAL, available from 2022, is whole-school enrollment, not capacity. MAT_ENS fields appear in 2025, preventing a consistent preschool enrollment-change series. Neither is substituted for available seats.</li>',
  '<li>School-level records and the complete exact accounting are saved locally. A separate audit retains preschool flags not satisfying operating/enrollment status; these do not enter the active stock.</li></ul>',
  '<p>Downloads: <a href="../../../data/clean/school_directory_preschool/annual_directory_preschool_accounting.csv">annual accounting</a>; ',
  '<a href="../../../data/clean/school_directory_preschool/schools_first_observed_operating.csv">first-operation school list</a>; ',
  '<a href="../../../data/clean/school_directory_preschool/school_preschool_status_changes.csv">preschool status changes</a>; ',
  '<a href="../../figures/kinder_capacity/directory_only_preschool_changes.pdf">figure PDF</a>.</p></main></body></html>')
writeLines(html, file.path(reports, "directory_only_preschool_changes.html"))
print(annual)
