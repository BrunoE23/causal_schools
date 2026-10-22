# User-requested proxy: baseline composition weighted by simulated assignment
# probability to the destination RBD. Preserve the original school set and roster.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
base <- "data/clean/applicant_composition_trends"
dest <- file.path(base, "probability_weighted")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
frame_path <- file.path(base, "applicant_school_frame.rds")
d <- as.data.table(readRDS(frame_path))
top <- fread(file.path(base, "top100_schools.csv"))
specs <- fread(file.path(base, "outcome_definitions.csv"))
paths <- file.path(data_root, "data/clean/DA_probs", paste0("DA_probs_", 2018:2020, ".csv"))
stopifnot(all(file.exists(paths)))
manifest <- c(frame_path, paths)
fwrite(data.table(path = manifest, bytes = file.info(manifest)$size,
                 modified = as.character(file.info(manifest)$mtime)), file.path(dest, "input_manifest.csv"))
audits <- list(); person_audits <- list()
probs <- rbindlist(lapply(seq_along(paths), function(j) {
  year <- (2018:2020)[j]
  message("Reading assignment probabilities for SAE ", year)
  # Files are 8-12 MB; select three columns and filter before any grouped work.
  ids <- unique(d[sae_proceso == year, mrun])
  p <- fread(paths[j], select = c("student_id", "school_id", "prob"), showProgress = FALSE)
  p <- p[student_id %in% ids]
  stopifnot(!anyNA(p$prob), all(p$prob >= 0 & p$prob <= 1),
            !anyDuplicated(p[, .(student_id, school_id)]))
  p[, unmatched := tolower(trimws(school_id)) %chin% c("unmatched", "-99")]
  p[, rbd := suppressWarnings(as.integer(sub("_.*$", "", school_id)))]
  stopifnot(p[unmatched == FALSE, all(!is.na(rbd) & rbd > 0)])
  p[unmatched == TRUE, rbd := NA_integer_]
  mass <- p[, .(probability_mass = sum(prob), unmatched_mass = sum(prob[unmatched])), by = student_id]
  roster_check <- merge(data.table(student_id = ids), mass, by = "student_id", all.x = TRUE)
  roster_check[, sae_proceso := year]
  person_audits[[j]] <<- roster_check
  # An absent school row can be zero only after verifying the student's complete
  # distribution, including unmatched. Do not recode absent students as zero.
  stopifnot(!anyNA(roster_check$probability_mass), max(abs(roster_check$probability_mass - 1)) < 1e-6)
  school <- p[unmatched == FALSE, .(admission_probability = sum(prob), probability_program_rows = .N), by = .(student_id, rbd)]
  stopifnot(all(school$admission_probability >= 0 & school$admission_probability <= 1 + 1e-12))
  listed <- d[sae_proceso == year, .(student_id = mrun, rbd)]
  extra <- school[rbd %in% top$rbd][!listed, on = .(student_id, rbd)]
  z <- merge(listed, school, by = c("student_id", "rbd"), all.x = TRUE)
  z[, probability_row_present := !is.na(admission_probability)]
  z[is.na(admission_probability), `:=`(admission_probability = 0, probability_program_rows = 0L)]
  audits[[j]] <<- data.table(sae_proceso = year, applicants = length(ids),
    probability_students = nrow(mass), max_mass_error = max(abs(mass$probability_mass - 1)),
    listed_school_pairs = nrow(z), positive_probability_pairs = sum(z$admission_probability > 0),
    zero_probability_pairs = sum(z$admission_probability == 0),
    absent_school_rows_recoded_zero = sum(!z$probability_row_present),
    pairs_with_multiple_programs = sum(z$probability_program_rows > 1L),
    listed_top100_probability_mass = sum(z$admission_probability),
    nonlisted_top100_probability_pairs = nrow(extra),
    nonlisted_top100_probability_mass = sum(extra$admission_probability))
  setnames(z, "student_id", "mrun")
  z[, sae_proceso := year]
}))
fwrite(rbindlist(audits), file.path(dest, "probability_linkage_audit.csv"))
saveRDS(rbindlist(person_audits), file.path(dest, "student_probability_mass_audit.rds"))
d <- merge(d, probs, by = c("mrun", "sae_proceso", "rbd"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == 212608L, !anyNA(d$admission_probability), !anyDuplicated(d[, .(mrun, sae_proceso, rbd)]))
saveRDS(d, file.path(dest, "applicant_school_frame.rds"))
fwrite(top, file.path(dest, "top100_schools.csv"))
fwrite(specs, file.path(dest, "outcome_definitions.csv"))

grid <- CJ(rbd = top$rbd, sae_proceso = 2018:2020)
school_counts <- d[, .(n_applicants = .N, n_positive_probability = sum(admission_probability > 0),
  expected_admissions = sum(admission_probability)), by = .(rbd, sae_proceso)]
school_counts <- merge(grid, school_counts, by = c("rbd", "sae_proceso"), all.x = TRUE)
for (v in c("n_applicants", "n_positive_probability", "expected_admissions")) {
  set(school_counts, which(is.na(school_counts[[v]])), v, 0)
}
cells <- rbindlist(lapply(specs$outcome, function(v) {
  x <- d[, {
    keep <- !is.na(get(v)) & admission_probability > 0
    weight <- admission_probability[keep]
    y <- get(v)[keep]
    mass <- sum(weight)
    mu <- if (mass > 0) sum(weight * y) / mass else NA_real_
    .(n_observed = sum(keep), n_characteristic_observed = sum(!is.na(get(v))),
      observed_probability_mass = mass, squared_weight_sum = sum(weight^2),
      effective_n = if (mass > 0) mass^2 / sum(weight^2) else NA_real_, mean = mu,
      weighted_population_sd = if (mass > 0) sqrt(sum(weight * (y - mu)^2) / mass) else NA_real_)
  }, by = .(rbd, sae_proceso)]
  x <- merge(school_counts, x, by = c("rbd", "sae_proceso"), all.x = TRUE)
  for (col in c("n_observed", "n_characteristic_observed", "observed_probability_mass", "squared_weight_sum")) {
    set(x, which(is.na(x[[col]])), col, 0)
  }
  x[, `:=`(outcome = v,
    coverage = fifelse(expected_admissions > 0, observed_probability_mass / expected_admissions, NA_real_),
    applicant_coverage = fifelse(n_applicants > 0, n_characteristic_observed / n_applicants, NA_real_),
    positive_probability_coverage = fifelse(n_positive_probability > 0, n_observed / n_positive_probability, NA_real_))]
  x
}))
cells <- merge(cells, top, by = "rbd", all.x = TRUE)
cells <- merge(cells, specs, by = "outcome", all.x = TRUE)
cells[, display_mean := mean * scale]
setorder(cells, rank, outcome, sae_proceso)
fwrite(cells, file.path(dest, "school_cohort_means_long.csv"))
wide <- dcast(cells, rank + rbd + school_name + comuna + sae_proceso + n_applicants + expected_admissions ~ outcome,
  value.var = c("mean", "n_observed", "observed_probability_mass", "effective_n", "coverage"))
fwrite(wide, file.path(dest, "school_cohort_panel.csv"))
coverage <- cells[expected_admissions > 0, .(expected_admissions = sum(expected_admissions),
  observed_probability_mass = sum(observed_probability_mass),
  weighted_coverage = sum(observed_probability_mass) / sum(expected_admissions),
  min_school_weighted_coverage = min(coverage), median_effective_n = median(effective_n, na.rm = TRUE),
  minimum_effective_n = min(effective_n, na.rm = TRUE)), by = .(outcome, label, sae_proceso)]
fwrite(coverage, file.path(dest, "weighted_coverage_summary.csv"))
fwrite(cells[n_applicants > 0 & observed_probability_mass == 0], file.path(dest, "unsupported_cells.csv"))
print(rbindlist(audits))
print(cells[primary == TRUE, .(populated_cells = sum(observed_probability_mass > 0),
  minimum_effective_n = min(effective_n, na.rm = TRUE)), by = outcome])
