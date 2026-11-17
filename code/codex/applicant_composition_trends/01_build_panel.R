# Applicant composition, by SAE process and destination school.
# Run from the repository root. Raw inputs and existing clean files are read-only.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
dest <- file.path(root, "data/clean/applicant_composition_trends")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
years <- 2018:2020
roster_path <- file.path(root, "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds")
universe_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
legacy_path <- file.path(root, "output/tables/applicant_priority_comparison/top_100_sae_destination_schools.csv")
raw_paths <- vapply(years, function(y) {
  p <- list.files(file.path(data_root, "data/raw", y, paste0("SAE_", y)), "^C1_.*[.]csv$", full.names = TRUE)
  stopifnot(length(p) == 1L); p
}, character(1))
metadata_paths <- vapply(years, function(y) {
  p <- list.files(file.path(data_root, "data/raw/school_directory", y), "[.]csv$", full.names = TRUE)
  stopifnot(length(p) == 1L); p
}, character(1))
paths <- c(roster_path, universe_path, legacy_path, raw_paths, metadata_paths)
stopifnot(all(file.exists(paths)))
manifest <- data.table(path = paths, bytes = file.info(paths)$size, modified = as.character(file.info(paths)$mtime))
fwrite(manifest, file.path(dest, "input_manifest.csv"))
print(manifest[, .(file = basename(path), bytes)])

roster <- as.data.table(readRDS(roster_path))[, .(mrun, sae_proceso, cohort_gr8)]
stopifnot(nrow(roster) == 294198L, !anyDuplicated(roster$mrun),
          all(roster$sae_proceso == roster$cohort_gr8), all(roster$sae_proceso %in% years))

# Preflight the 1.23 GB CSV, then select 12 columns and immediately filter.
cols <- c("mrun", "cohort_gr8", "GEN_ALU", "simce_year", "z_sim_mat_4to", "z_sim_leng_4to",
          "ptje_mate4b_alu", "ptje_lect4b_alu", "father_educ_years", "mother_educ_years",
          "father_educ_years_imputed", "mother_educ_years_imputed")
stopifnot(all(cols %in% names(fread(universe_path, nrows = 0L))))
message("Reading selected baseline columns; filtering to the cached applicant roster.")
u <- fread(universe_path, select = cols, na.strings = c("", "NA"), showProgress = FALSE)
u <- u[mrun %in% roster$mrun & cohort_gr8 %in% years]
stopifnot(!anyDuplicated(u$mrun), nrow(u) == nrow(roster))
u <- merge(roster, u, by = c("mrun", "cohort_gr8"), all.x = TRUE, sort = FALSE)
u[, female := fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2), NA_real_)]
u[, simce_baseline := (z_sim_mat_4to + z_sim_leng_4to) / 2]
u[, simce_raw_points := (ptje_mate4b_alu + ptje_lect4b_alu) / 2]
parent_mean <- function(a, b) {
  n <- as.integer(!is.na(a)) + as.integer(!is.na(b))
  out <- (fifelse(is.na(a), 0, a) + fifelse(is.na(b), 0, b)) / n
  out[n == 0L] <- NA_real_; out
}
u[, parents_education := parent_mean(mother_educ_years, father_educ_years)]
u[, parents_both_reported := (mother_educ_years + father_educ_years) / 2]
u[, parents_imputed := parent_mean(mother_educ_years_imputed, father_educ_years_imputed)]
stopifnot(all(u$parents_education >= 0 & u$parents_education <= 21, na.rm = TRUE))

audits <- list()
apps <- rbindlist(lapply(seq_along(years), function(i) {
  y <- years[i]
  message("Reading genuine grade-9 choices in SAE ", y)
  x <- fread(raw_paths[i], sep = ";", select = c("mrun", "rbd", "cod_nivel", "agregada_por_continuidad"), showProgress = FALSE)
  source_n <- nrow(x)
  x <- x[cod_nivel == 9L & mrun %in% roster[sae_proceso == y, mrun]]
  stopifnot(!anyNA(x$agregada_por_continuidad), all(x$agregada_por_continuidad %in% 0:1),
            !anyNA(x$rbd), all(x$rbd > 0))
  all_pairs <- unique(x[, .(mrun, rbd)])
  real_pairs <- unique(x[agregada_por_continuidad == 0L, .(mrun, rbd)])
  audits[[i]] <<- data.table(sae_proceso = y, source_rows = source_n,
    roster_students = roster[sae_proceso == y, .N], grade9_roster_program_rows = nrow(x),
    automatic_program_rows = x[agregada_por_continuidad == 1L, .N],
    all_school_pairs = nrow(all_pairs), genuine_school_pairs = nrow(real_pairs),
    automatic_only_school_pairs = nrow(all_pairs) - nrow(real_pairs),
    students_with_genuine_choices = uniqueN(real_pairs$mrun))
  real_pairs[, sae_proceso := y]
}))
stopifnot(!anyDuplicated(apps[, .(mrun, sae_proceso, rbd)]))
fwrite(rbindlist(audits), file.path(dest, "application_audit.csv"))

# Fix one school set over all years; count each student-school pair once.
ranking <- apps[, .(applications = .N, applicant_cohorts = uniqueN(sae_proceso)), by = rbd]
setorder(ranking, -applications, rbd)
ranking[, rank := .I]
meta <- rbindlist(lapply(seq_along(years), function(i) {
  x <- fread(metadata_paths[i], select = c("RBD", "NOM_RBD", "NOM_COM_RBD"), encoding = "UTF-8", showProgress = FALSE)
  setnames(x, c("rbd", "school_name", "comuna")); x[, directory_year := years[i]]
}))
setorder(meta, rbd, -directory_year)
meta <- unique(meta, by = "rbd")
ranking <- merge(ranking, meta, by = "rbd", all.x = TRUE, sort = FALSE)
setorder(ranking, rank)
top <- ranking[rank <= 100L]
stopifnot(nrow(top) == 100L, !anyNA(top$school_name))
legacy <- fread(legacy_path)[, .(rbd, previous_rank = rank, previous_applicants = applicants)]
comparison <- merge(top[, .(rbd, rank, school_name, applications)], legacy, by = "rbd", all = TRUE)
comparison[, membership := fcase(is.na(rank), "previous_only", is.na(previous_rank), "genuine_only", default = "both")]
fwrite(comparison, file.path(dest, "previous_top100_comparison.csv"))
fwrite(ranking, file.path(dest, "all_school_application_ranking.csv"))
fwrite(top, file.path(dest, "top100_schools.csv"))

d <- merge(apps[rbd %in% top$rbd], u, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == sum(top$applications), !anyNA(d$cohort_gr8))
saveRDS(d, file.path(dest, "applicant_school_frame.rds"))
specs <- data.table(
  outcome = c("simce_baseline", "female", "parents_education", "z_sim_mat_4to", "z_sim_leng_4to",
              "mother_educ_years", "father_educ_years", "parents_both_reported", "parents_imputed", "simce_raw_points"),
  label = c("Baseline SIMCE", "Female share", "Parents' education", "SIMCE math", "SIMCE verbal",
            "Mother's education", "Father's education", "Parents: both reported", "Parents: observed plus imputed", "Baseline SIMCE: raw points"),
  primary = c(rep(TRUE, 3), rep(FALSE, 7)),
  scale = c(1, 100, rep(1, 8)),
  unit = c("average subject z-score", "percentage points", "years", "subject SD", "subject SD", rep("years", 4), "test points"))
fwrite(specs, file.path(dest, "outcome_definitions.csv"))

grid <- CJ(rbd = top$rbd, sae_proceso = years)
counts <- d[, .(n_applicants = .N), by = .(rbd, sae_proceso)]
panel <- merge(grid, counts, by = c("rbd", "sae_proceso"), all.x = TRUE)
panel[is.na(n_applicants), n_applicants := 0L]
stats <- rbindlist(lapply(specs$outcome, function(v) {
  x <- d[, .(n_observed = sum(!is.na(get(v))), mean = mean(get(v), na.rm = TRUE),
             sd = sd(get(v), na.rm = TRUE)), by = .(rbd, sae_proceso)]
  x <- merge(panel, x, by = c("rbd", "sae_proceso"), all.x = TRUE)
  x[is.na(n_observed), n_observed := 0L]
  x[!is.finite(mean), mean := NA_real_]
  x[, `:=`(outcome = v, coverage = fifelse(n_applicants > 0, n_observed / n_applicants, NA_real_))]
}))
stats <- merge(stats, top, by = "rbd", all.x = TRUE)
stats <- merge(stats, specs, by = "outcome", all.x = TRUE)
setorder(stats, rank, outcome, sae_proceso)
stats[, display_mean := mean * scale]
fwrite(stats, file.path(dest, "school_cohort_means_long.csv"))
wide <- dcast(stats, rank + rbd + school_name + comuna + sae_proceso + n_applicants ~ outcome,
              value.var = c("mean", "n_observed", "coverage"))
fwrite(wide, file.path(dest, "school_cohort_panel.csv"))
fwrite(u[, .(students = .N), by = .(sae_proceso, simce_year)], file.path(dest, "baseline_test_year_audit.csv"))
fwrite(d[, .(applicant_school_pairs = .N, distinct_applicants = uniqueN(mrun), schools = uniqueN(rbd)), by = sae_proceso],
       file.path(dest, "sample_by_cohort.csv"))
message("Completed panel: ", nrow(wide), " school-cohort rows; ", nrow(d), " applicant-school pairs.")
print(rbindlist(audits))
print(comparison[, .N, by = membership])
print(top[, .N, by = applicant_cohorts])
