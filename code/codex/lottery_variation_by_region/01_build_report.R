# School-level persistence of simulated grade-9 lottery variation by region.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
task <- "lottery_variation_by_region"
clean <- file.path(repo, "data/clean", task)
reports <- file.path(repo, "output/reports", task)
tables <- file.path(repo, "output/tables", task)
for (p in c(clean, reports, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
years <- 2018:2020
tol <- 1e-10 # Only protects the endpoints from floating-point summation error.
one_file <- function(folder, pattern) {
  p <- list.files(folder, pattern = pattern, full.names = TRUE)
  stopifnot(length(p) == 1L)
  p
}
prob_paths <- file.path(data_root, "data/clean/DA_probs", paste0("DA_probs_", years, ".csv"))
offer_paths <- vapply(years, function(y) one_file(file.path(data_root, "data/raw", y, paste0("SAE_", y)), "^A1_.*[.]csv$"), character(1))
directory_paths <- vapply(years, function(y) one_file(file.path(data_root, "data/raw/school_directory", y), "[.]csv$"), character(1))
roster_path <- file.path(repo, "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds")
inputs <- c(prob_paths, offer_paths, directory_paths, roster_path)
stopifnot(all(file.exists(inputs)), all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
fwrite(data.table(path = normalizePath(inputs, winslash = "/"), bytes = file.info(inputs)$size, md5 = hashes), file.path(clean, "input_manifest.csv"))
roster <- as.data.table(readRDS(roster_path))[, .(student_id = mrun, sae_proceso)]
stopifnot(nrow(roster) == 294198L, !anyDuplicated(roster))

metadata <- rbindlist(lapply(seq_along(years), function(j) {
  d <- fread(directory_paths[j], select = c("RBD", "COD_REG_RBD", "COD_COM_RBD"), showProgress = FALSE)
  setnames(d, c("rbd", "region_code", "comuna_code"))
  unique(d)[, directory_year := years[j]]
}))
stopifnot(!anyDuplicated(metadata[, .(rbd, directory_year)]))
# One fixed regional assignment per RBD: latest directory observation in-window.
# This places schools transferred from Biobio to Nuble in their 2020 region.
setorder(metadata, rbd, -directory_year)
school_region <- unique(metadata, by = "rbd")
fwrite(metadata[, .(n_region_codes = uniqueN(region_code), codes = paste(sort(unique(region_code)), collapse = ";")), by = rbd][n_region_codes > 1],
       file.path(clean, "region_code_changes.csv"))

school_years <- list(); audits <- list(); extra <- list()
for (j in seq_along(years)) {
  year <- years[j]
  message("SAE ", year, ": read three probability columns; aggregate programs to RBD")
  offers <- fread(offer_paths[j], select = c("rbd", "cod_nivel", "vacantes"), showProgress = FALSE)
  offers <- offers[cod_nivel == 9 & !is.na(rbd) & rbd > 0,
    .(offered_programs = .N, vacancies = sum(vacantes, na.rm = TRUE)), by = rbd]
  p <- fread(prob_paths[j], select = c("student_id", "school_id", "prob"), showProgress = FALSE)
  stopifnot(!anyNA(p), all(p$prob >= 0 & p$prob <= 1), !anyDuplicated(p[, .(student_id, school_id)]))
  mass <- p[, .(mass = sum(prob)), by = student_id]
  stopifnot(max(abs(mass$mass - 1)) < 1e-6)
  paper_ids <- roster[sae_proceso == year, student_id]
  stopifnot(all(paper_ids %in% mass$student_id))
  p[, unmatched := tolower(trimws(school_id)) %chin% c("unmatched", "-99")]
  p[, rbd := suppressWarnings(as.integer(sub("_.*$", "", school_id)))]
  stopifnot(p[unmatched == FALSE, all(!is.na(rbd) & rbd > 0)])
  p <- p[unmatched == FALSE, .(probability = sum(prob), program_rows = .N), by = .(student_id, rbd)]
  stopifnot(all(p$probability >= 0 & p$probability <= 1 + tol))
  p[, `:=`(stochastic = probability > tol & probability < 1 - tol,
           paper = student_id %in% paper_ids)]
  counts <- p[, .(n_positive = sum(probability > tol),
    n_stochastic = sum(stochastic), n_positive_paper = sum(probability > tol & paper),
    n_stochastic_paper = sum(stochastic & paper)), by = rbd]
  extra[[j]] <- counts[!rbd %in% offers$rbd][, sae_proceso := year]
  panel <- merge(offers, counts, by = "rbd", all.x = TRUE)
  for (v in c("n_positive", "n_stochastic", "n_positive_paper", "n_stochastic_paper")) set(panel, which(is.na(panel[[v]])), v, 0L)
  panel[, `:=`(sae_proceso = year, variation = n_stochastic > 0L,
               variation_paper = n_stochastic_paper > 0L)]
  school_years[[j]] <- panel
  audits[[j]] <- data.table(sae_proceso = year, simulated_students = nrow(mass),
    paper_students = length(paper_ids), max_mass_error = max(abs(mass$mass - 1)),
    offering_schools = nrow(panel), varying_schools = sum(panel$variation),
    varying_schools_paper = sum(panel$variation_paper), schools_without_positive_prob = sum(panel$n_positive == 0),
    probability_schools_outside_offerings = nrow(extra[[j]]),
    stochastic_schools_outside_offerings = sum(extra[[j]]$n_stochastic > 0))
}
panel <- rbindlist(school_years)
panel <- merge(panel, school_region, by = "rbd", all.x = TRUE)
stopifnot(!anyNA(panel$region_code), all(panel$region_code %in% 1:16), !anyDuplicated(panel[, .(rbd, sae_proceso)]))
region_order <- c(15L, 1L, 2L, 3L, 4L, 5L, 13L, 6L, 7L, 16L, 8L, 9L, 14L, 10L, 11L, 12L)
region_labels <- c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso", "Metropolitana", "O'Higgins", "Maule", "Nuble", "Biobio", "La Araucania", "Los Rios", "Los Lagos", "Aysen", "Magallanes")
region_map <- data.table(region_code = region_order, region = region_labels, region_order = seq_along(region_order))
panel <- merge(panel, region_map, by = "region_code", all.x = TRUE)
fwrite(panel, file.path(clean, "school_year_variation.csv"))
fwrite(rbindlist(audits), file.path(clean, "probability_audit.csv"))
fwrite(rbindlist(extra), file.path(clean, "probability_schools_outside_offerings.csv"))
fwrite(panel[, .(schools = .N, with_variation = sum(variation), with_variation_paper = sum(variation_paper)),
             keyby = .(region_order, region_code, region, sae_proceso)], file.path(clean, "region_year_counts.csv"))

classify <- function(d, v) {
  x <- d[, .(years_observed = .N, years_with_variation = sum(get(v)),
             first_year = min(sae_proceso), last_year = max(sae_proceso)),
         by = .(rbd, region_code, region, region_order)]
  x[, classification := fifelse(years_with_variation == 0L, "Never",
    fifelse(years_with_variation == years_observed, "Always", "Sometimes"))]
  x
}
summarize <- function(x) {
  s <- x[, .(schools = .N, always = sum(classification == "Always"),
    sometimes = sum(classification == "Sometimes"), never = sum(classification == "Never"),
    observed_one_year = sum(years_observed == 1L), observed_two_years = sum(years_observed == 2L),
    observed_three_years = sum(years_observed == 3L)), by = .(region_order, region_code, region)]
  s <- rbind(s, x[, .(region_order = 17L, region_code = 0L, region = "Total", schools = .N,
    always = sum(classification == "Always"), sometimes = sum(classification == "Sometimes"),
    never = sum(classification == "Never"), observed_one_year = sum(years_observed == 1L),
    observed_two_years = sum(years_observed == 2L), observed_three_years = sum(years_observed == 3L))])
  for (k in c("always", "sometimes", "never")) s[, (paste0(k, "pct")) := 100 * get(k) / schools]
  stopifnot(all(s$always + s$sometimes + s$never == s$schools),
    abs(s[region_code > 0, sum(schools)] - s[region_code == 0, schools]) < 1e-10)
  s[order(region_order)]
}
main <- classify(panel, "variation")
paper <- classify(panel, "variation_paper")
balanced_ids <- panel[sae_proceso %in% 2019:2020, .N, by = rbd][N == 2L, rbd]
balanced <- classify(panel[rbd %in% balanced_ids & sae_proceso %in% 2019:2020], "variation")
main_summary <- summarize(main); paper_summary <- summarize(paper); balanced_summary <- summarize(balanced)
fwrite(main, file.path(clean, "school_classification.csv"))
fwrite(paper, file.path(clean, "school_classification_paper_roster.csv"))
fwrite(main_summary, file.path(clean, "region_distribution.csv"))
fwrite(paper_summary, file.path(clean, "region_distribution_paper_roster.csv"))
fwrite(balanced_summary, file.path(clean, "region_distribution_balanced_2019_2020.csv"))
fwrite(summarize(main[years_observed >= 2]), file.path(clean, "region_distribution_at_least_two_years.csv"))
fwrite(main[, .(schools = .N), keyby = .(years_observed, years_with_variation, classification)], file.path(clean, "years_variation_distribution.csv"))
# Direct independent counts from annual flags must reproduce each classification.
direct <- panel[, .(all_years = all(variation), any_year = any(variation)), by = rbd]
check <- merge(main, direct, by = "rbd")
stopifnot(all((check$classification == "Always") == check$all_years),
  all((check$classification == "Never") == !check$any_year),
  all(panel$n_stochastic_paper <= panel$n_stochastic),
  identical(unname(tools::md5sum(inputs)), hashes))
fwrite(data.table(check = c("Unique school-year keys", "All regions matched", "Probability mass equals one",
  "Paper roster probability coverage", "Independent category replication", "Regional counts sum to totals", "Inputs unchanged"),
  passed = TRUE), file.path(clean, "verification.csv"))

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
cell <- function(n, pct) sprintf("%s (%.1f%%)", fmt_n(n), pct)
html_table <- function(s) paste0('<table><thead><tr><th>Region</th><th>Schools</th><th>Always</th><th>Sometimes</th><th>Never</th><th>Observed 1 / 2 / 3 years</th></tr></thead><tbody>',
  paste(vapply(seq_len(nrow(s)), function(i) paste0('<tr><th>', s$region[i], '</th><td>', fmt_n(s$schools[i]), '</td><td>', cell(s$always[i], s$alwayspct[i]), '</td><td>', cell(s$sometimes[i], s$sometimespct[i]), '</td><td>', cell(s$never[i], s$neverpct[i]), '</td><td>',
    paste(c(s$observed_one_year[i], s$observed_two_years[i], s$observed_three_years[i]), collapse = ' / '), '</td></tr>'), character(1)), collapse = ''), '</tbody></table>')
notes <- c(
  "Reconstruction for SAE processes 2018-2020. The earlier always/sometimes/never tally could not be located, so exact replication of its denominator is not asserted.",
  "Main denominator: distinct RBDs listed as offering grade 9 in at least one regular-round SAE A1 file in 2018-2020, including zero listed vacancies. Each school counts once. Percentages sum to 100 within region before rounding.",
  "For each student-year, sum program/campus probabilities within RBD before determining whether 0 < p < 1. A school-year has variation if at least one simulated grade-9 applicant meets this condition. No minimum count such as k=100 is imposed. This is simulated assignment variation, not cross-student heterogeneity in deterministic admission probabilities.",
  "Always means variation in every observed grade-9-offering year; sometimes means at least one but not all; never means none. Years without a grade-9 offering are outside the denominator, not zero-filled. The table reports observation counts; schools seen only once can only be always or never.",
  "Region is the school's location from its latest directory observation in 2018-2020, with one fixed region per RBD. This assigns Nuble schools to their latest regional code. Metropolitana has no 2018 grade-9 offering in these files. The balanced 2019-2020 table gives all schools exactly two observation years.",
  "The main table uses every student in the existing grade-9 simulation files. The supplement restricts the probability evidence to the 294,198 paper-roster applicants while keeping the same school denominator. No math-score, higher-education, or VA-availability restriction is applied.",
  "Never denotes no observed interior assignment probability in the saved simulations. Finite simulations cannot exclude very rare assignment risk. Probabilities outside the official offering roster, if any, are saved in an audit and do not expand the denominator.")
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Lottery variation by school region</title>',
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1250px;margin:32px auto;padding:0 24px}h1{font-size:28px}h2{font-size:23px;margin-top:36px}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px 12px;border-bottom:1px solid #dce3e9;text-align:right;white-space:nowrap}th:first-child{text-align:left}thead{background:#edf3f6}tbody tr:last-child{font-weight:bold;background:#edf3f6}.scroll{overflow-x:auto}.note{font-size:14px;color:#526171}</style></head><body>',
  '<h1>Which schools have lottery variation every year, sometimes, or never?</h1><p>SAE grade-9 assignment, 2018-2020. Counts and percentages within each school region.</p>',
  paste0('<p><b>', fmt_n(nrow(main)), ' schools:</b> ', cell(main_summary[region_code == 0, always], main_summary[region_code == 0, alwayspct]),
    ' always; ', cell(main_summary[region_code == 0, sometimes], main_summary[region_code == 0, sometimespct]), ' sometimes; ',
    cell(main_summary[region_code == 0, never], main_summary[region_code == 0, neverpct]), ' never. ', sum(main$years_observed == 1L),
    ' schools have only one observed year; a common two-year comparison appears below.</p>'),
  '<div class="scroll">', html_table(main_summary), '</div><h2>Definition and coverage</h2>', paste0('<p class="note">', notes, '</p>'),
  '<h2>Common two-year window: schools offering grade 9 in both 2019 and 2020</h2><div class="scroll">', html_table(balanced_summary), '</div>',
  '<h2>Probability evidence restricted to the paper applicant roster</h2><div class="scroll">', html_table(paper_summary), '</div>',
  '<p><a href="../../../data/clean/lottery_variation_by_region/region_distribution.csv">Regional counts and shares</a> | <a href="../../../data/clean/lottery_variation_by_region/school_classification.csv">School classifications</a> | <a href="../../../code/codex/lottery_variation_by_region/README.md">Reproduction instructions</a></p></body></html>')
writeLines(html, file.path(reports, "report.html"), useBytes = TRUE)
tex <- c('\\begin{table}[!htbp]\\centering', '\\caption{Persistence of school lottery variation by region, SAE 2018--2020}',
  '\\label{tab:lottery-variation-region}', '\\small', '\\begin{tabular}{lrrrr}\\hline\\hline',
  'Region & Schools & Always (\\%) & Sometimes (\\%) & Never (\\%) \\\\ \\hline',
  vapply(seq_len(nrow(main_summary)), function(i) paste0(main_summary$region[i], ' & ', fmt_n(main_summary$schools[i]), ' & ',
    paste(sprintf('%.1f', unlist(main_summary[i, .(alwayspct, sometimespct, neverpct)])), collapse = ' & '), ' \\\\'), character(1)),
  '\\hline\\hline\\end{tabular}', '\\begin{minipage}{0.98\\textwidth}\\footnotesize',
  '\\textit{Notes:} Schools are distinct RBDs offering grade 9 in regular-round SAE in 2018--2020. Variation means at least one simulated applicant has an RBD-level assignment probability strictly between zero and one. Always/sometimes/never refers to the years in which the school offers grade 9; years outside SAE are not counted as zero. Regions use the latest school directory in the window. Each school receives equal weight. Metropolitana is observed in 2019--2020. This is a reconstruction; the earlier tally could not be located.',
  '\\end{minipage}', '\\end{table}')
writeLines(tex, file.path(tables, "region_distribution.tex"), useBytes = TRUE)
print(main_summary)
print(rbindlist(audits))
print(main[, .N, keyby = .(years_observed, classification)])
