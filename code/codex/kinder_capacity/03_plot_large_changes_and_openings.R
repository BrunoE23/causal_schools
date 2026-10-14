# Large seat changes and a longer directory series of first observed operation.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
clean <- file.path(root, "data/clean/kinder_capacity")
figures <- file.path(root, "output/figures/kinder_capacity")
reports <- file.path(root, "output/reports/kinder_capacity")
for (p in c(clean, figures, reports)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
tr <- fread(file.path(clean, "school_capacity_transitions.csv"))
ky <- fread(file.path(clean, "school_year_kinder_capacity.csv"))
directory <- as.data.table(readRDS(file.path(clean, "directory_history.rds")))
stopifnot(!anyDuplicated(directory[, .(rbd, directory_year)]), !anyNA(directory$operating))

# Keep entry/exit separate from intensive changes at positive-capacity incumbents.
# Strict >10 excludes changes of exactly 10 seats.
summarize_changes <- function(x, scope) {
  z <- x[margin == "Continuing kinder offering"]
  z[, .(scope = scope, continuing_schools = .N,
    expanding_gt10 = sum(change_capacity > 10),
    contracting_gt10 = sum(change_capacity < -10),
    share_changing_gt10 = mean(abs(change_capacity) > 10),
    seats_added_gt10 = sum(change_capacity[change_capacity > 10]),
    seats_removed_gt10 = sum(-change_capacity[change_capacity < -10]),
    exactly10 = sum(abs(change_capacity) == 10),
    net_capacity_change_all_continuers = sum(change_capacity),
    vacancies_increase_gt10 = sum(change_vacancies > 10),
    vacancies_decrease_gt10 = sum(change_vacancies < -10)),
    by = .(process_before, process_after, entry_before, entry_after)]
}
rollout_regions <- unique(ky[process_year == 2017L, coverage_region])
stopifnot(identical(sort(rollout_regions), c(1L, 4L, 6L, 10L, 12L)))
changes <- rbindlist(list(
  summarize_changes(tr[geography == "National"], "National"),
  summarize_changes(tr[coverage_region %in% rollout_regions], "Fixed 2017 rollout regions"),
  summarize_changes(tr[geography != "National"], "Pairwise common rollout geography")
))
setorder(changes, scope, entry_after)
fwrite(changes, file.path(clean, "large_capacity_changes_by_year.csv"))
fwrite(tr[margin == "Continuing kinder offering" & abs(change_capacity) > 10][order(entry_after, -abs(change_capacity))],
  file.path(clean, "schools_with_capacity_changes_gt10.csv"))

# Directory years are April snapshots. Exclude left-censored baseline year 2015.
first_records <- directory[, .(first_record_year = min(directory_year)), by = rbd]
first_active <- directory[operating == TRUE][order(directory_year), .SD[1L], by = rbd]
first_active <- merge(first_active, first_records, by = "rbd")
first_active[, `:=`(private = dependency == 4L,
  same_year_new_directory_record = directory_year == first_record_year,
  present_in_2015 = first_record_year == 2015L)]
stopifnot(!anyNA(first_active$dependency))
openings <- first_active[directory_year > 2015L]
opening_summary <- openings[, .(first_observed_operating = .N,
  nonprivate_first_observed_operating = sum(!private),
  private_first_observed_operating = sum(private),
  with_preschool_at_first_operation = sum(preschool_enrolled),
  nonprivate_with_preschool = sum(preschool_enrolled & !private),
  private_with_preschool = sum(preschool_enrolled & private),
  first_directory_record_same_year = sum(same_year_new_directory_record),
  previously_registered = sum(!same_year_new_directory_record),
  previously_registered_since_baseline2015 = sum(present_in_2015)), by = .(year = directory_year)]
setorder(opening_summary, year)
stopifnot(identical(opening_summary$year, 2016:2025),
  all(opening_summary$first_observed_operating == opening_summary$nonprivate_first_observed_operating + opening_summary$private_first_observed_operating),
  all(opening_summary$with_preschool_at_first_operation == opening_summary$nonprivate_with_preschool + opening_summary$private_with_preschool),
  all(opening_summary$first_observed_operating == opening_summary$first_directory_record_same_year + opening_summary$previously_registered))
fwrite(opening_summary, file.path(clean, "directory_first_operating_by_year.csv"))
fwrite(openings[order(directory_year, rbd)], file.path(clean, "directory_first_operating_schools.csv"))
stock <- directory[, .(operating_schools = sum(operating),
  operating_preschool_schools = sum(operating & preschool_enrolled)), by = .(year = directory_year)]
fwrite(stock[order(year)], file.path(clean, "directory_operating_stock_by_year.csv"))

blue <- "#2878A0"
red <- "#C05D54"
purple <- "#9575B3"
ink <- "#243347"
draw_capacity <- function() {
  par(mfrow = c(1, 2), mar = c(4.4, 4.2, 4.2, 1.2), oma = c(3.2, 0, 0.8, 0),
    family = "sans", col.axis = ink, col.lab = ink, col.main = ink, las = 1)
  for (scope_name in c("National", "Fixed 2017 rollout regions")) {
    z <- changes[scope == scope_name][order(entry_after)]
    v <- rbind(z$expanding_gt10, z$contracting_gt10)
    bp <- barplot(v, beside = TRUE, names.arg = z$entry_after, col = c(blue, red), border = NA,
      ylim = c(0, max(v) * 1.40), ylab = "Number of schools",
      xlab = "Entry year (change from preceding year)",
      main = paste0(scope_name, "\nCapacity changes larger than 10 seats"), cex.main = 0.98)
    text(bp, v, labels = v, pos = 3, cex = 0.92, col = ink)
    legend("topright", c("Increase >10", "Decrease >10"), fill = c(blue, red), bty = "n", cex = 0.86)
  }
  mtext("Continuing SAE kinder offerings only. Entry/exit excluded. Strict threshold: 11 or more seats.",
    side = 1, outer = TRUE, line = 1, cex = 0.82, col = ink)
  mtext("Fixed regions: Tarapaca, Coquimbo, O'Higgins, Los Lagos and Magallanes.",
    side = 1, outer = TRUE, line = 2.2, cex = 0.80, col = ink)
}
draw_openings <- function() {
  par(mfrow = c(2, 1), mar = c(4.6, 4.3, 3, 1.2), oma = c(4.3, 0, 1.0, 0),
    family = "sans", col.axis = ink, col.lab = ink, col.main = ink, las = 1)
  for (with_pre in c(FALSE, TRUE)) {
    v <- if (with_pre) rbind(opening_summary$nonprivate_with_preschool, opening_summary$private_with_preschool) else
      rbind(opening_summary$nonprivate_first_observed_operating, opening_summary$private_first_observed_operating)
    title <- if (with_pre) "Of these: schools with preschool enrollment when first operating" else
      "All schools first observed operating with enrollment"
    bp <- barplot(v, names.arg = opening_summary$year, col = c(blue, purple), border = NA,
      ylim = c(0, max(colSums(v)) * 1.28), ylab = "Number of schools", xlab = "Directory year", main = title, cex.main = 0.98)
    text(bp, colSums(v), labels = colSums(v), pos = 3, cex = 0.90, col = ink)
    legend("topright", c("Public / subsidized", "Private paid"), fill = c(blue, purple), bty = "n", cex = 0.85)
  }
  mtext("National directories, 2015-2025; 2015 is the baseline and excluded from opening counts.",
    side = 1, outer = TRUE, line = 1, cex = 0.80, col = ink)
  mtext("First observed operation is a candidate opening; no legal founding date. Preschool combines pre-kinder and kinder.",
    side = 1, outer = TRUE, line = 2.2, cex = 0.78, col = ink)
}
for (kind in c("png", "pdf")) {
  if (kind == "png") png(file.path(figures, "capacity_changes_gt10.png"), width = 2200, height = 1000, res = 180) else
    pdf(file.path(figures, "capacity_changes_gt10.pdf"), width = 12.2, height = 5.6)
  draw_capacity()
  dev.off()
  if (kind == "png") png(file.path(figures, "school_first_operation_timeseries.png"), width = 2200, height = 1800, res = 180) else
    pdf(file.path(figures, "school_first_operation_timeseries.pdf"), width = 12.2, height = 10)
  draw_openings()
  dev.off()
}

fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
htmltable <- function(x) paste0('<div class="table"><table><thead><tr>',
  paste0("<th>", esc(names(x)), "</th>", collapse = ""), "</tr></thead><tbody>",
  paste(apply(x, 1L, function(row) paste0("<tr>", paste0("<td>", esc(row), "</td>", collapse = ""), "</tr>")), collapse = ""),
  "</tbody></table></div>")
change_table <- changes[scope != "Pairwise common rollout geography", .(
  Geography = scope, `Entry years` = paste0(entry_before, " to ", entry_after),
  `Continuing schools` = fmt(continuing_schools), `Increase >10: schools` = fmt(expanding_gt10),
  `Increase >10: added seats` = fmt(seats_added_gt10), `Decrease >10: schools` = fmt(contracting_gt10),
  `Decrease >10: removed seats` = fmt(seats_removed_gt10), `Share changing >10` = sprintf("%.1f%%", 100 * share_changing_gt10))]
opening_table <- opening_summary[, .(Year = year, `First observed operating` = first_observed_operating,
  `With preschool` = with_preschool_at_first_operation, `With preschool: public / subsidized` = nonprivate_with_preschool,
  `With preschool: private paid` = private_with_preschool,
  `New directory record in same year` = first_directory_record_same_year,
  `Previously registered` = previously_registered)]
national_list <- tr[geography == "National" & margin == "Continuing kinder offering" & abs(change_capacity) > 10][order(entry_after, -abs(change_capacity))]
school_table <- national_list[, .(`Entry year` = entry_after, RBD = rbd, School = school_name,
  Comuna = comuna, Before = capacity_before, After = capacity_after, Change = change_capacity)]
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Kinder: large capacity changes and school openings</title><style>',
  'body{font:15px/1.5 system-ui,sans-serif;background:#f4f6f8;color:#243347;margin:0}main{max-width:1250px;margin:24px auto;background:white;padding:28px;border-radius:8px}h1{font-size:27px}h2{font-size:21px;margin-top:30px}.table{overflow:auto}table{border-collapse:collapse;width:100%;font-size:13px;font-variant-numeric:tabular-nums}th,td{padding:9px;border-bottom:1px solid #dfe5eb;text-align:right}th{background:#e9eff4}th:first-child,td:first-child{text-align:left}img{width:100%;height:auto}li{margin:7px 0}details{padding:12px;border:1px solid #ddd;margin-top:18px}</style></head><body><main>',
  '<h1>Kinder: capacity changes greater than 10 seats and school openings</h1>',
  '<p>The threshold applies to declared total kinder capacity at schools with positive capacity in both adjacent SAE years. Changes of exactly 10 seats and entry/exit are excluded from these incumbent-change counts.</p>',
  '<h2>Large changes at continuing schools</h2><img src="../../figures/kinder_capacity/capacity_changes_gt10.png" alt="Counts of schools increasing or decreasing capacity by more than 10 seats over time">',
  htmltable(change_table),
  '<p>National changes are observed for entry in 2021 and 2022. The fixed-region series uses only the five regions already participating in SAE 2017: Tarapaca, Coquimbo, O\'Higgins, Los Lagos and Magallanes, for all four changes. Newly participating regions are never counted as school openings.</p>',
  '<h2>Longer national series from school directories</h2><img src="../../figures/kinder_capacity/school_first_operation_timeseries.png" alt="Annual counts of schools first observed operating, all schools and those offering preschool">',
  htmltable(opening_table),
  '<h2>Interpretation</h2><ul>',
  '<li>First observed operating means ESTADO_ESTAB=1 and MATRICULA=1 for the first time in the available annual directory history. These are candidate openings, not confirmed legal founding dates. A school previously operating before the 2015 baseline, or receiving a new RBD, could be misclassified.</li>',
  '<li>Previously registered schools may have been authorized but awaiting enrollment or inactive. We do not call these confirmed reactivations. The 2015 directory is the baseline; no opening counts are shown for that left-censored year.</li>',
  '<li>Preschool means an ENS teaching code of 10 in the first operating year; this combines pre-kinder and kinder. Public/subsidized means COD_DEPE other than 4; private paid means COD_DEPE=4. The directory series covers its registered RBD universe, not every JUNJI/Integra provider.</li>',
  '<li>SAE cupos_totales measure declared total capacity. Vacantes measure places available to SAE applicants after continuity. The CSV also reports school counts with vacancy changes greater than 10, but these do not measure capacity expansion.</li>',
  '<li>Directory opening counts are broader than SAE participation, refer to actual directory year, and must not be aligned mechanically with SAE entry years. Directory-only data cannot tell us the kinder seat capacity of all candidate openings.</li>',
  '<li>School entry/exit and its capacity remain in the <a href="kinder_capacity_changes.html">full capacity accounting</a>. No causal effect of the compulsory-kinder proposal is identified by these series.</li></ul>',
  '<details><summary>All national continuing-school changes greater than 10 seats</summary>', htmltable(school_table), '</details>',
  '<p>Downloads: <a href="../../../data/clean/kinder_capacity/large_capacity_changes_by_year.csv">large-change totals</a>; ',
  '<a href="../../../data/clean/kinder_capacity/directory_first_operating_by_year.csv">annual first-operation counts</a>; ',
  '<a href="../../../data/clean/kinder_capacity/directory_first_operating_schools.csv">candidate-opening school list</a>; ',
  '<a href="../../figures/kinder_capacity/school_first_operation_timeseries.pdf">opening figure PDF</a>.</p>',
  '</main></body></html>')
writeLines(html, file.path(reports, "large_changes_and_school_openings.html"))
print(changes[scope == "National"])
print(opening_summary)
