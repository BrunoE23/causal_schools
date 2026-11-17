# Rank SAE destination schools by distinct applicants in the paper sample.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))

root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
pair <- "va_2017_2020__sae_2018_2020"
sample_path <- file.path(root, "data/clean/empirical_bayes_school_va", pair, "by_continuation", "all_applicant_expected_va.rds")
apps_path <- file.path(data_root, "data/clean/sae_grade9_unique_proceso.RData")
out_table <- file.path(root, "output/tables/applicant_priority_comparison")
out_report <- file.path(root, "output/reports/applicant_priority_comparison")
dir.create(out_table, recursive = TRUE, showWarnings = FALSE)
dir.create(out_report, recursive = TRUE, showWarnings = FALSE)

sample <- as.data.table(readRDS(sample_path))[, .(mrun, sae_proceso)]
stopifnot(nrow(sample) == 294198L, !anyDuplicated(sample[, .(mrun, sae_proceso)]))
apps_env <- new.env()
load(apps_path, envir = apps_env)
apps <- as.data.table(apps_env$sae_apps_grade9)[sample, on = .(mrun, sae_proceso), nomatch = 0L]
# A student can have multiple grade-9 program rows at one RBD. The ranking unit
# is one applicant-school pair, matching the interpretation given to the user.
apps <- unique(apps[, .(mrun, sae_proceso, rbd)])
stopifnot(uniqueN(apps$mrun) == nrow(sample))

ranking <- apps[, .(applicants = uniqueN(mrun)), by = rbd]
setorder(ranking, -applicants, rbd)
ranking[, rank := .I]

offer_list <- lapply(2018:2020, function(year) {
  folder <- file.path(data_root, "data/raw", year, paste0("SAE_", year))
  path <- list.files(folder, pattern = "^A1_.*[.]csv$", full.names = TRUE)
  stopifnot(length(path) == 1L)
  x <- fread(path, sep = ";", select = c("rbd", "cod_nivel", "vacantes"), showProgress = FALSE)
  x[cod_nivel == 9L, .(spots = sum(vacantes, na.rm = TRUE)), by = rbd][, sae_proceso := year]
})
offers <- rbindlist(offer_list)
capacity <- offers[, .(spots = sum(spots)), by = rbd]

# Use the latest available name during the three application cycles.
metadata <- rbindlist(lapply(2018:2020, function(year) {
  folder <- file.path(data_root, "data/raw/school_directory", year)
  path <- list.files(folder, pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(path) == 1L)
  x <- fread(path, select = c("RBD", "NOM_RBD", "NOM_COM_RBD"), encoding = "UTF-8", showProgress = FALSE)
  setnames(x, c("rbd", "school_name", "comuna"))
  x[, directory_year := year]
  x
}), use.names = TRUE, fill = TRUE)
setorder(metadata, rbd, -directory_year)
metadata <- unique(metadata, by = "rbd")

ranking <- merge(ranking, capacity, by = "rbd", all.x = TRUE, sort = FALSE)
ranking <- merge(ranking, metadata, by = "rbd", all.x = TRUE, sort = FALSE)
ranking[is.na(spots), spots := 0]
setorder(ranking, rank)

total_applications <- nrow(apps)
total_students <- uniqueN(apps$mrun)
total_spots <- offers[, sum(spots)]
ranking[, `:=`(
  applicant_share = applicants / total_students,
  application_share = applicants / total_applications,
  spot_share = spots / total_spots,
  applicants_per_spot = fifelse(spots > 0, applicants / spots, Inf),
  cumulative_application_share = cumsum(applicants) / total_applications,
  cumulative_spot_share = cumsum(spots) / total_spots
)]
top100 <- ranking[1:100]
setcolorder(top100, c("rank", "rbd", "school_name", "comuna", "applicants", "spots",
  "applicants_per_spot", "applicant_share", "application_share", "spot_share",
  "cumulative_application_share", "cumulative_spot_share"))
fwrite(top100, file.path(out_table, "top_100_sae_destination_schools.csv"))

top_students <- uniqueN(apps[rbd %in% top100$rbd, mrun])
summary <- data.table(
  schools = 100L,
  summed_applicant_school_pairs = sum(top100$applicants),
  total_applicant_school_pairs = total_applications,
  application_share = sum(top100$applicants) / total_applications,
  students_listing_any_top100 = top_students,
  total_students = total_students,
  student_share = top_students / total_students,
  spots = sum(top100$spots),
  total_spots = total_spots,
  spot_share = sum(top100$spots) / total_spots
)
fwrite(summary, file.path(out_table, "top_100_sae_destination_schools_summary.csv"))

# Threshold audit and overlap in applicant sets.
threshold_audit <- top100[, .(
  schools = .N,
  at_least_100_applicants = sum(applicants >= 100),
  at_least_50_spots = sum(spots >= 50),
  meets_both = sum(applicants >= 100 & spots >= 50)
)]
fwrite(top100[applicants < 100 | spots < 50,
  .(rank, rbd, school_name, comuna, applicants, spots)],
  file.path(out_table, "top_100_sae_destination_schools_below_threshold.csv"))

top_apps <- apps[rbd %in% top100$rbd, .(mrun, rbd)]
student_overlap <- top_apps[, .(n_top100_schools = .N), by = mrun]
overlap_distribution <- student_overlap[, .(students = .N), by = n_top100_schools][order(n_top100_schools)]
overlap_distribution[, `:=`(
  share_among_students_with_top100 = students / sum(students),
  share_among_all_applicants = students / total_students
)]
fwrite(overlap_distribution, file.path(out_table, "top_100_sae_destination_schools_overlap_distribution.csv"))

pair_overlap <- merge(top_apps, top_apps, by = "mrun", allow.cartesian = TRUE)[
  rbd.x < rbd.y, .(shared_applicants = uniqueN(mrun)), by = .(rbd_1 = rbd.x, rbd_2 = rbd.y)]
school_lookup <- top100[, .(rbd, school_name, comuna, applicants)]
pair_overlap <- merge(pair_overlap,
  school_lookup[, .(rbd_1 = rbd, school_1 = school_name, comuna_1 = comuna, applicants_1 = applicants)], by = "rbd_1")
pair_overlap <- merge(pair_overlap,
  school_lookup[, .(rbd_2 = rbd, school_2 = school_name, comuna_2 = comuna, applicants_2 = applicants)], by = "rbd_2")
pair_overlap[, `:=`(
  jaccard = shared_applicants / (applicants_1 + applicants_2 - shared_applicants),
  overlap_smaller_school = shared_applicants / pmin(applicants_1, applicants_2)
)]
setorder(pair_overlap, -shared_applicants, rbd_1, rbd_2)
fwrite(pair_overlap, file.path(out_table, "top_100_sae_destination_schools_pairwise_overlap.csv"))

overlap_summary <- data.table(
  students_listing_any_top100 = nrow(student_overlap),
  students_listing_multiple_top100 = student_overlap[n_top100_schools >= 2L, .N],
  share_multiple_among_top100_applicants = student_overlap[n_top100_schools >= 2L, .N] / nrow(student_overlap),
  share_multiple_among_all_applicants = student_overlap[n_top100_schools >= 2L, .N] / total_students,
  mean_top100_schools_conditional_any = mean(student_overlap$n_top100_schools),
  nonzero_school_pairs = nrow(pair_overlap),
  possible_school_pairs = choose(100L, 2L)
)
fwrite(overlap_summary, file.path(out_table, "top_100_sae_destination_schools_overlap_summary.csv"))

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}
fmt_int <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
fmt_pct <- function(x) sprintf("%.2f%%", 100 * x)
fmt_ratio <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "n/a")
rows <- vapply(seq_len(nrow(top100)), function(i) paste0(
  "<tr><td>", top100$rank[i], "</td><td>", top100$rbd[i], "</td><td>", esc(top100$school_name[i]),
  "</td><td>", esc(top100$comuna[i]), "</td><td>", fmt_int(top100$applicants[i]),
  "</td><td>", fmt_int(top100$spots[i]), "</td><td>", fmt_ratio(top100$applicants_per_spot[i]),
  "</td><td>", fmt_pct(top100$application_share[i]), "</td><td>", fmt_pct(top100$cumulative_application_share[i]),
  "</td><td>", fmt_pct(top100$cumulative_spot_share[i]), "</td></tr>"
), character(1))
pair_rows <- vapply(seq_len(min(15L, nrow(pair_overlap))), function(i) paste0(
  "<tr><td>", esc(pair_overlap$school_1[i]), "</td><td>", esc(pair_overlap$comuna_1[i]),
  "</td><td>", esc(pair_overlap$school_2[i]), "</td><td>", esc(pair_overlap$comuna_2[i]),
  "</td><td>", fmt_int(pair_overlap$shared_applicants[i]), "</td><td>", fmt_pct(pair_overlap$jaccard[i]),
  "</td><td>", fmt_pct(pair_overlap$overlap_smaller_school[i]), "</td></tr>"
), character(1))
distribution_rows <- vapply(seq_len(nrow(overlap_distribution)), function(i) paste0(
  "<tr><td>", overlap_distribution$n_top100_schools[i], "</td><td>", fmt_int(overlap_distribution$students[i]),
  "</td><td>", fmt_pct(overlap_distribution$share_among_students_with_top100[i]),
  "</td><td>", fmt_pct(overlap_distribution$share_among_all_applicants[i]), "</td></tr>"
), character(1))
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  "<title>Top 100 SAE destination schools</title>",
  '<style>body{font:14px/1.45 system-ui,sans-serif;background:#f5f7fa;color:#243347;margin:0}main{max-width:1350px;margin:24px auto;background:#fff;padding:28px;border-radius:10px}h1{font-size:27px}.cards{display:flex;gap:14px;flex-wrap:wrap;margin:18px 0}.card{background:#eef3f7;padding:12px 16px;border-radius:7px}.card b{display:block;font-size:20px}.table{overflow:auto;max-height:75vh;border:1px solid #dfe5eb}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:8px;border-bottom:1px solid #e1e6eb;text-align:right;white-space:nowrap}th:nth-child(3),td:nth-child(3),th:nth-child(4),td:nth-child(4){text-align:left}thead th{position:sticky;top:0;background:#e7eef4;z-index:1}.notes{font-size:13px}</style></head><body><main>',
  "<h1>Top 100 SAE destination schools</h1><p>Ranked by distinct applicants listing the school anywhere in their grade-9 SAE portfolio, 2018–2020.</p>",
  '<div class="cards"><div class="card"><b>', fmt_pct(summary$application_share), '</b>of applicant–school pairs</div>',
  '<div class="card"><b>', fmt_pct(summary$student_share), '</b>of students list at least one</div>',
  '<div class="card"><b>', fmt_pct(summary$spot_share), '</b>of grade-9 SAE vacancies</div>',
  '<div class="card"><b>', threshold_audit$meets_both, '/100</b>have ≥100 applicants and ≥50 spots</div>',
  '<div class="card"><b>', fmt_pct(overlap_summary$share_multiple_among_top100_applicants), '</b>of top-100 applicants list multiple</div></div>',
  '<div class="table"><table><thead><tr><th>Rank</th><th>RBD</th><th>School</th><th>Comuna</th><th>Applicants</th><th>Spots</th><th>Apps/spot</th><th>Application share</th><th>Cumulative application share</th><th>Cumulative spot share</th></tr></thead><tbody>',
  paste(rows, collapse = ""), "</tbody></table></div>",
  '<h2>Overlap across top-100 schools</h2><p>Number of top-100 schools listed by each applicant who lists at least one.</p>',
  '<table><thead><tr><th>Top-100 schools listed</th><th>Students</th><th>Share among students listing any</th><th>Share among all applicants</th></tr></thead><tbody>',
  paste(distribution_rows, collapse = ""), '</tbody></table>',
  '<h2>Largest pairwise overlaps</h2><table><thead><tr><th>School 1</th><th>Comuna</th><th>School 2</th><th>Comuna</th><th>Shared applicants</th><th>Jaccard share</th><th>Share of smaller applicant set</th></tr></thead><tbody>',
  paste(pair_rows, collapse = ""), '</tbody></table>',
  '<p class="notes">Applicants are unique within school. A student listed at multiple schools contributes one applicant–school pair to each. Spots are grade-9 vacancies (`vacantes`) summed over the 2018–2020 regular SAE offering files and all quota categories. Application shares use ',
  fmt_int(total_applications), " unique applicant–school pairs; student shares use ", fmt_int(total_students),
  " applicants; spot shares use ", fmt_int(total_spots), " vacancies.</p></main></body></html>")
writeLines(html, file.path(out_report, "top_100_sae_destination_schools.html"))

print(summary)
print(threshold_audit)
print(overlap_summary)
print(top100)
