suppressPackageStartupMessages(library(data.table))
root <- normalizePath(".", winslash = "/")
clean <- file.path(root, "data/clean/kinder_capacity")
reports <- file.path(root, "output/reports/kinder_capacity")
annual <- fread(file.path(clean, "annual_capacity_and_vacancies.csv"))
d <- fread(file.path(clean, "capacity_change_accounting.csv"))
t <- fread(file.path(clean, "school_capacity_transitions.csv"))
stopifnot(all(d$accounting_residual == 0L))
num <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
signed <- function(x) ifelse(x > 0, paste0("+", num(x)), num(x))
esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
htmltable <- function(x) paste0('<div class="table"><table><thead><tr>',
  paste0("<th>", esc(names(x)), "</th>", collapse = ""), "</tr></thead><tbody>",
  paste(apply(x, 1L, function(row) paste0("<tr>", paste0("<td>", esc(row), "</td>", collapse = ""), "</tr>")), collapse = ""),
  "</tbody></table></div>")
totals <- annual[process_year >= 2019L, .(`SAE process` = process_year, `Entry year` = entry_year,
  `Schools with capacity` = num(schools_with_positive_capacity), `Total declared capacity` = num(declared_capacity),
  `Vacancies available through SAE` = num(available_vacancies), `Vacancies / capacity` = sprintf("%.1f%%", 100 * vacancy_share))]
flows <- d[, .(`SAE processes` = paste0(process_before, " to ", process_after), Geography = geography,
  `Capacity before` = num(capacity_before), `Capacity after` = num(capacity_after),
  `Added at continuing schools` = num(seats_added_by_continuing),
  `Removed at continuing schools` = num(seats_removed_by_continuing),
  `Capacity in new kinder listings` = num(seats_in_new_kinder_offerings),
  `Lost from departing listings` = num(seats_lost_from_exiting), `Net capacity change` = signed(net_capacity_change))]
entry <- d[, .(`SAE processes` = paste0(process_before, " to ", process_after), Geography = geography,
  `Candidate school openings` = num(candidate_newly_operating_schools),
  `Seats at candidate openings` = num(seats_candidate_newly_operating_schools),
  `Existing schools newly listing kinder` = num(existing_schools_new_kinder_listing),
  `Seats at existing new listings` = num(seats_existing_schools_new_kinder_listing),
  `Unresolved new listings` = num(unresolved_entrants), `Seats unresolved` = num(seats_unresolved_entrants))]
counts <- d[, .(`SAE processes` = paste0(process_before, " to ", process_after), Geography = geography,
  `Continuing schools expanding` = num(expanding_schools), `Continuing schools contracting` = num(contracting_schools),
  `Continuing unchanged` = num(unchanged_schools), `New listings` = num(new_kinder_offerings),
  `Departing listings` = num(exiting_offerings))]
entrants <- t[process_after >= 2020L & margin == "New SAE kinder offering"][order(process_after, -capacity_after)]
entrants_display <- entrants[, .(`SAE process` = process_after, RBD = rbd, School = school_name, Comuna = comuna,
  Capacity = capacity_after, Vacancies = vacancies_after, Classification = entrant_type,
  `First observed operating` = first_operating_year, `Earlier preschool enrollment` = preschool_enrollment_before_process)]
notes <- c(
  "Capacity is CUPOS_TOTALES in regular-round SAE A1 files, aggregated across unique RBD-COD_CURSO programs with COD_NIVEL=0 (ordinary preschool COD_ENSE=10, COD_GRADO=5). It is declared admission capacity rather than independently certified physical capacity.",
  "VACANTES are seats available through SAE after accounting for continuing pupils and repeaters. Vacancies may rise without any increase in total capacity. Programs can encompass several classrooms; program counts are not classroom counts.",
  "Process year t admits pupils for calendar year t+1. National kinder comparisons start in process 2019, when the Metropolitan Region joined. Earlier transitions retain only rollout regions observed in both years. Nuble is grouped with its previous Biobio boundary for this geographic restriction.",
  "A continuing offering has positive kinder capacity in both years. New and departing listings refer to appearance or disappearance of positive kinder capacity in SAE; they do not automatically imply school openings or closures.",
  "Candidate school openings are entrant RBDs first observed operating with enrollment in annual directories in the current SAE process year or its following entry year, with no operating observation in the available history since 2015. These are candidate openings, not verified legal founding dates; historical reopenings and identifier changes may remain. For example, RBD 41859 first operates in the April 2021 directory and first lists SAE kinder for entry in 2022; this is not evidence that the school itself opened in 2022.",
  "Previously operating schools newly listing kinder may represent actual grade additions, entry into SAE, or reporting changes. Directory preschool codes do not distinguish kinder from pre-kinder. The underlying transition CSV retains prior preschool enrollment and earlier SAE participation for assessment.",
  "The directory coverage begins in 2015 and A1 files used here in 2017; this exercise cannot test an expansion immediately around the 2013 constitutional reform. It is descriptive capacity accounting and does not establish that anticipated compulsory kinder caused provider entry."
)
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  "<title>Kinder capacity: school expansion and entry</title>",
  '<style>body{font:15px/1.5 system-ui,sans-serif;background:#f4f6f8;color:#243347;margin:0}main{max-width:1450px;background:white;margin:25px auto;padding:30px;border-radius:8px}h1{font-size:28px}h2{font-size:21px;margin-top:28px}.table{overflow:auto;margin:12px 0}table{border-collapse:collapse;width:100%;font-size:13px;font-variant-numeric:tabular-nums}th,td{padding:9px;border-bottom:1px solid #dfe5eb;text-align:right}th{background:#e9eff4;vertical-align:bottom}th:first-child,td:first-child{text-align:left}.notes{font-size:13px}details{margin-top:20px;padding:12px;border:1px solid #dfe5eb;border-radius:6px}</style></head><body><main>',
  "<h1>Kinder capacity: school expansion and entry</h1>",
  "<p>Declared seats, vacancies, and changes at continuing and newly listed schools. SAE process 2019-2021 corresponds to entry years 2020-2022.</p>",
  "<h2>National capacity and available vacancies</h2>", htmltable(totals),
  "<h2>Where capacity changes come from</h2><p>Added at continuing schools minus removed at continuing schools, plus capacity in new listings minus capacity lost from departing listings, equals the net capacity change exactly.</p>", htmltable(flows),
  "<h2>Who supplies new kinder listings</h2>", htmltable(entry),
  "<h2>Number of schools changing capacity</h2>", htmltable(counts),
  "<details><summary>New national kinder listings in SAE 2020 and 2021</summary>", htmltable(entrants_display), "</details>",
  '<h2>Definitions and limits</h2><ul class="notes">', paste0("<li>", esc(notes), "</li>", collapse = ""), "</ul>",
  '<p class="notes">Rollout references: <a href="https://metropolitana.mineduc.cl/2019/07/22/el-2019-comienza-a-implementarse-el-sistema-de-admision-en-la-region-metropolitana/">Mineduc 2019</a>; <a href="https://metropolitana.mineduc.cl/2020/07/28/mineduc-presenta-novedades-en-la-implementacion-del-sistema-de-admision-escolar-en-el-contexto-covid-19/">Mineduc 2020</a>. All input paths and sizes, program checks, and exact school-level transitions are saved under data/clean/kinder_capacity/.</p>',
  "</main></body></html>")
writeLines(html, file.path(reports, "kinder_capacity_changes.html"))
