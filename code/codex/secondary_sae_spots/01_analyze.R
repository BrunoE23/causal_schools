# Annual secondary-school spots in SAE regular-round A1 offers, 2016-2025.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
out <- file.path(root, "data/clean/secondary_sae_spots")
fig <- file.path(root, "output/figures/secondary_sae_spots")
report <- file.path(root, "output/reports/secondary_sae_spots")
for (p in c(out, fig, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
check <- function(ok, msg) { if (!isTRUE(ok)) stop(msg); message("PASS: ", msg) }

# Directories supply labels/geography only; no directory operation or enrollment
# condition restricts the SAE sample. Stable geography prevents Nuble's creation
# from being counted as SAE geographic expansion.
directory_path <- file.path(root, "data/clean/school_directory_preschool/directory_school_year.csv")
d <- fread(directory_path, select = c("rbd", "year", "school_name", "region", "comuna"))
setorder(d, rbd, -year)
labels <- unique(d, by = "rbd")[, .(rbd, school_name, region, comuna)]
labels[, region := fifelse(region == 16L, 8L, region)]
manifest <- list()
audits <- list()
all_listings <- list()
offers <- rbindlist(lapply(2016:2025, function(y) {
  folder <- if (y <= 2021L) file.path(data_root, "data/raw", y, paste0("SAE_", y)) else file.path(out, "source_offers", y)
  p <- list.files(folder, pattern = "^A1.*[.]csv$", full.names = TRUE, recursive = TRUE)
  check(length(p) == 1L, paste(y, "has exactly one A1 source"))
  check(file.info(p)$size < 1e8, paste(y, "source is under 100 MB"))
  x <- fread(p, sep = ";", showProgress = FALSE)
  if ("nivel" %in% names(x)) setnames(x, "nivel", "cod_nivel")
  key <- if ("cod_curso" %in% names(x)) c("rbd", "cod_curso") else c("rbd", "cod_nivel", "cod_ense", "cod_grado")
  check(!anyDuplicated(x[, ..key]), paste(y, "program keys unique"))
  all_listings[[as.character(y)]] <<- unique(x[, .(rbd, process_year = y)])
  x <- x[cod_nivel %in% 9:12]
  check(all(x$cod_grado == x$cod_nivel - 8L), paste(y, "secondary grade mapping"))
  check(all(x$cod_ense %in% c(310L, 410L, 510L, 610L, 710L, 810L, 910L)), paste(y, "regular secondary teaching codes"))
  check(!anyNA(x[, .(rbd, cupos_totales, vacantes)]) && all(x$cupos_totales >= 0) && all(x$vacantes >= 0) && all(x$vacantes <= x$cupos_totales), paste(y, "valid capacity and vacancies"))
  manifest[[as.character(y)]] <<- data.table(process_year = y, entry_year = y + 1L, path = p,
    bytes = file.info(p)$size, md5 = unname(tools::md5sum(p)))
  audits[[as.character(y)]] <<- data.table(process_year = y, secondary_program_rows = nrow(x),
    program_key = paste(key, collapse = "+"), zero_capacity_rows = sum(x$cupos_totales == 0L))
  x[, .(capacity = sum(cupos_totales), vacancies = sum(vacantes), programs = .N), by = .(rbd, grade = cod_nivel)][, process_year := y]
}))
offers <- merge(offers, labels, by = "rbd", all.x = TRUE)
check(!anyNA(offers$region), "All secondary offers have directory geography")
check(!anyDuplicated(offers[, .(rbd, grade, process_year)]), "School-grade-year keys unique")
all_listings <- rbindlist(all_listings)
coverage <- unique(offers[, .(process_year, region, grade)])[order(process_year, region, grade)]
school_year <- offers[, .(capacity = sum(capacity), vacancies = sum(vacancies),
  grades_listed = paste(sort(grade), collapse = ","), grade_count = uniqueN(grade)),
  by = .(process_year, rbd, region, school_name, comuna)]
annual <- school_year[, .(schools_listed = .N, schools_positive_capacity = sum(capacity > 0),
  schools_positive_vacancies = sum(vacancies > 0), capacity = sum(capacity), vacancies = sum(vacancies),
  regions_normalized = uniqueN(region)), by = process_year][order(process_year)]
annual[, `:=`(entry_year = process_year + 1L,
  scope = fcase(process_year == 2016L, "Magallanes; grade 9 only",
    process_year == 2017L, "5 regions; grades phased in",
    process_year == 2018L, "Outside Metropolitana; grades phased in",
    process_year == 2019L, "National grade 9; grades 10-12 outside Metropolitana",
    default = "National; grades 9-12"))]

transition_rows <- list()
summary_rows <- list()
compare <- function(y, scope, grade9 = FALSE) {
  before <- offers[process_year == y - 1L]
  after <- offers[process_year == y]
  if (grade9) { before <- before[grade == 9L]; after <- after[grade == 9L] }
  cells <- merge(unique(before[, .(region, grade)]), unique(after[, .(region, grade)]), by = c("region", "grade"))
  before <- merge(before, cells, by = c("region", "grade"))
  after <- merge(after, cells, by = c("region", "grade"))
  b <- before[, .(capacity_before = sum(capacity), vacancies_before = sum(vacancies), listed_before = TRUE), by = rbd]
  a <- after[, .(capacity_after = sum(capacity), vacancies_after = sum(vacancies), listed_after = TRUE), by = rbd]
  z <- merge(b, a, by = "rbd", all = TRUE)
  for (v in c("capacity_before", "vacancies_before", "capacity_after", "vacancies_after")) set(z, which(is.na(z[[v]])), v, 0)
  for (v in c("listed_before", "listed_after")) set(z, which(is.na(z[[v]])), v, FALSE)
  z[, `:=`(process_year = y, entry_year = y + 1L, scope = scope,
    delta_capacity = capacity_after - capacity_before, delta_vacancies = vacancies_after - vacancies_before,
    continuing = capacity_before > 0 & capacity_after > 0,
    capacity_entry = capacity_before == 0 & capacity_after > 0,
    capacity_exit = capacity_before > 0 & capacity_after == 0,
    listing_exit = listed_before & !listed_after,
    listed_any_grade_after = rbd %in% all_listings[process_year == y, rbd])]
  z <- merge(z, labels, by = "rbd", all.x = TRUE)
  s <- with(z, data.table(process_year = y, entry_year = y + 1L, scope = scope[1L],
    compared_region_grade_cells = nrow(cells), compared_regions = uniqueN(cells$region),
    schools_before = sum(capacity_before > 0), schools_after = sum(capacity_after > 0),
    continuing = sum(continuing), expanding = sum(continuing & delta_capacity > 0),
    downsizing = sum(continuing & delta_capacity < 0), unchanged = sum(continuing & delta_capacity == 0),
    expanding_gt10 = sum(continuing & delta_capacity > 10), downsizing_gt10 = sum(continuing & delta_capacity < -10),
    entrants_positive_capacity = sum(capacity_entry), exits_positive_capacity = sum(capacity_exit),
    listings_disappeared = sum(listing_exit), exits_still_listed_with_zero_capacity = sum(capacity_exit & listed_after),
    exits_absent_from_all_sae = sum(capacity_exit & !listed_any_grade_after),
    more_vacancies = sum(continuing & delta_vacancies > 0), fewer_vacancies = sum(continuing & delta_vacancies < 0),
    unchanged_vacancies = sum(continuing & delta_vacancies == 0),
    capacity_before = sum(capacity_before), capacity_after = sum(capacity_after),
    vacancies_before = sum(vacancies_before), vacancies_after = sum(vacancies_after),
    seats_added_continuing = sum(delta_capacity[continuing & delta_capacity > 0]),
    seats_removed_continuing = -sum(delta_capacity[continuing & delta_capacity < 0]),
    seats_in_entries = sum(capacity_after[capacity_entry]), seats_lost_exits = sum(capacity_before[capacity_exit])))
  s[, `:=`(net_capacity = capacity_after - capacity_before, net_vacancies = vacancies_after - vacancies_before)]
  check(s$continuing == s$expanding + s$downsizing + s$unchanged, paste(y, scope, "school categories add up"))
  check(s$net_capacity == s$seats_added_continuing - s$seats_removed_continuing + s$seats_in_entries - s$seats_lost_exits,
    paste(y, scope, "seat accounting exact"))
  check(s$schools_after - s$schools_before == s$entrants_positive_capacity - s$exits_positive_capacity,
    paste(y, scope, "school accounting exact"))
  transition_rows[[length(transition_rows) + 1L]] <<- z
  summary_rows[[length(summary_rows) + 1L]] <<- s
}
for (y in 2017:2025) {
  compare(y, "Secondary: common region-grade coverage")
  compare(y, "Grade 9: common regions", grade9 = TRUE)
}
transitions <- rbindlist(transition_rows)
changes <- rbindlist(summary_rows)
fwrite(annual, file.path(out, "annual_secondary_spots.csv"))
fwrite(changes, file.path(out, "annual_comparable_changes.csv"))
fwrite(transitions, file.path(out, "school_transitions.csv"))
fwrite(school_year, file.path(out, "school_year_spots.csv"))
fwrite(offers, file.path(out, "school_grade_year_spots.csv"))
fwrite(coverage, file.path(out, "region_grade_coverage.csv"))
fwrite(rbindlist(audits), file.path(out, "source_audit.csv"))
fwrite(rbindlist(manifest), file.path(out, "input_manifest.csv"))

plot_results <- function() {
  par(mfrow = c(2, 1), mar = c(4.2, 4.6, 3.2, 1), oma = c(2.5, 0, 1, 0), las = 1, family = "sans")
  a <- annual[process_year >= 2020L]
  matplot(a$process_year, cbind(a$capacity, a$vacancies)/1000, type = "b", pch = c(16, 17),
    lty = 1, col = c("#235789", "#E18A24"), xlab = "SAE process year (entry in following year)",
    ylab = "Thousand spots", main = "National secondary spots: consistent coverage from 2020", ylim = c(0, max(a$capacity)/1000*1.35), xaxt = "n")
  axis(1, at = a$process_year)
  legend("topleft", c("Declared capacity", "Advertised vacancies"), col = c("#235789", "#E18A24"), lty = 1, pch = c(16, 17), bty = "n")
  s <- changes[scope == "Secondary: common region-grade coverage"]
  barplot(t(as.matrix(s[, .(expanding, downsizing)])), beside = TRUE, names.arg = s$process_year,
    col = c("#2A8B75", "#C45E49"), border = NA, ylab = "Continuing schools", xlab = "SAE process year",
    main = "Schools increasing or reducing declared secondary capacity")
  legend("topleft", c("Increasing", "Reducing"), fill = c("#2A8B75", "#C45E49"), bty = "n")
  mtext("Annual comparisons hold region-grade coverage fixed within each pair. Earlier pairs cover a smaller sample.\nSource: MINEDUC SAE regular-round A1 files. Changes in listings do not establish legal closure or an SAE causal effect.", side = 1, outer = TRUE, cex = 0.72, line = 0.5)
}
png(file.path(fig, "secondary_sae_spots.png"), width = 1800, height = 1400, res = 180)
plot_results(); dev.off()

esc <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
html_table <- function(x) paste0("<div class='table'><table><thead><tr>", paste0("<th>", esc(names(x)), "</th>", collapse = ""),
  "</tr></thead><tbody>", paste(apply(as.data.frame(x), 1, function(r) paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table></div>")
main_changes <- changes[scope == "Secondary: common region-grade coverage", .(Process = process_year, Continuing = continuing,
  Increasing = expanding, Reducing = downsizing, Unchanged = unchanged, Entries = entrants_positive_capacity,
  Exits = exits_positive_capacity, More_vacancies = more_vacancies, Fewer_vacancies = fewer_vacancies)]
latest_exits <- transitions[scope == "Secondary: common region-grade coverage" & process_year == 2025L & capacity_exit == TRUE,
  .(RBD = rbd, School = school_name, Comuna = comuna, Prior_capacity = capacity_before, Still_in_SAE = listed_any_grade_after)]
html <- paste0("<!doctype html><html><head><meta charset='utf-8'><title>Secondary-school spots in SAE, 2016-2025</title><style>",
  "body{font:16px/1.55 system-ui,sans-serif;color:#173047;background:#f4f6f8;margin:0}main{max-width:1180px;margin:auto;padding:36px;background:white}h1{font-size:32px}h2{margin-top:34px}img{width:100%}.table{overflow-x:auto}table{border-collapse:collapse;font-size:14px;width:100%}th,td{padding:9px;border-bottom:1px solid #dce3e8;text-align:right}th{background:#eaf0f5}td:nth-child(2){text-align:left}.note{padding:16px;background:#eef4f8}a{color:#235789}</style></head><body><main>",
  "<h1>Secondary-school spots listed in SAE, every year 2016-2025</h1>",
  "<p>Regular-round A1 offers for grades 9-12 (1-4 medio). Process year t admits students for year t+1. The latest published archive checked on 11 November 2026 is process 2025, admission 2026.</p>",
  "<p class='note'><b>Capacity and vacancies are different.</b> cupos_totales is declared total capacity; vacantes is advertised available places. Changes in vacancies can reflect continuing enrollment rather than changes in capacity. An exit below means loss of positive capacity in the comparable secondary listings, not a verified school closure. No causal effect of SAE is estimated.</p>",
  "<img src='../../figures/secondary_sae_spots/secondary_sae_spots.png' alt='Annual secondary capacity and vacancy totals and counts of increasing and reducing schools'>",
  "<h2>Annual totals: all coverage present in each release</h2>", html_table(annual[, .(Process = process_year, Entry = entry_year, Schools = schools_positive_capacity, Capacity = capacity, Vacancies = vacancies, Coverage = scope)]),
  "<p>Raw totals before 2020 are affected by the regional and grade rollout. Grade 9 becomes national in process 2019; all four secondary grades are national from process 2020. The 2024 release also has an A3 waitlist-stage revision; this series consistently uses initial regular-round A1 offers in every year.</p>",
  "<h2>Annual changes using comparable coverage</h2>", html_table(main_changes),
  "<p>For each adjacent-year pair, retain region-grade combinations observed in both releases, then sum all programs to RBD. Increasing/reducing/unchanged require positive declared capacity in both years. Entries/exits include movements from/to zero capacity or no listing. All changes count, including one seat; the CSV also reports changes greater than 10 seats. Zero advertised vacancies alone never defines an exit.</p>",
  "<h2>Secondary listings exiting positive capacity in process 2025</h2>", html_table(latest_exits),
  "<p>Still_in_SAE indicates any A1 listing at any grade in 2025. Even absence from all SAE does not establish permanent closure.</p>",
  "<h2>Downloads and reproducibility</h2><p><a href='../../../data/clean/secondary_sae_spots/annual_secondary_spots.csv'>Annual totals</a> | <a href='../../../data/clean/secondary_sae_spots/annual_comparable_changes.csv'>Annual changes, including separate grade-9 comparisons</a> | <a href='../../../data/clean/secondary_sae_spots/school_transitions.csv'>School-by-school changes</a> | <a href='https://datosabiertos.mineduc.cl/sistema-de-admision-escolar-sae/'>Official sources</a></p>",
  "<p>Program keys, valid capacity/vacancies, geography completeness, and exact seat/school accounting are checked by the script. Directory data supply geography and names only; their operation and enrollment fields do not define the sample.</p></main></body></html>")
writeLines(html, file.path(report, "annual_secondary_sae_spots.html"), useBytes = TRUE)
print(annual)
print(main_changes)
