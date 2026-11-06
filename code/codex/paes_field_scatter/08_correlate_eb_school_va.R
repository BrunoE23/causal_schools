suppressPackageStartupMessages(library(data.table))
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "paes_field_scatter", "school_va_correlations")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
read_va <- function(key, expected_outcome) {
  folder <- file.path(repo, "output", "tables", "empirical_bayes_school_va")
  d <- fread(file.path(folder, paste0("stata_eb_school_values_", key, ".csv")))
  d <- d[analysis_sample == "All" & is.finite(va_eb_centered)]
  stopifnot(!anyDuplicated(d$school_rbd), !anyNA(d$school_rbd), all(d$outcome == expected_outcome),
    all(d$n_students > 0), all(is.finite(d$va_eb)),
    all(d$eb_reliability >= 0 & d$eb_reliability <= 1),
    diff(range(d$va_eb_centered - d$va_eb)) < 1e-10)
  raw <- fread(file.path(folder, paste0("stata_va_eb_input_", key, ".csv")))[
    analysis_sample == "All" & is.finite(va_raw)]
  check <- merge(d, raw, by = c("school_rbd", "analysis_sample", "outcome"), suffixes = c("_eb_file", "_input"))
  stopifnot(nrow(check) == nrow(d), nrow(check) == nrow(raw),
    all(check$n_students_eb_file == check$n_students_input),
    max(abs(check$va_raw_eb_file - check$va_raw_input)) < 1e-6)
  d
}
math <- read_va("math", "z_year_math_max")
keys <- c(high_paying_field_m1 = "highpay", high_inst_m1 = "highinst")
labels <- c(high_paying_field_m1 = "High-premium field", high_inst_m1 = "High-premium institution")
results <- rbindlist(lapply(names(keys), function(o) {
  other <- read_va(keys[[o]], o)
  pair <- merge(math, other, by = "school_rbd", suffixes = c("_math", "_other"))
  # Equal school weights, including private schools, on the same pairwise
  # common-school support as the preceding raw-VA calculation.
  old <- fread(file.path(out, paste0("matched_math_", keys[[o]], "_school_values.csv")))
  stopifnot(setequal(pair$school_rbd, old$school_rbd))
  rho <- cor(pair$va_eb_centered_math, pair$va_eb_centered_other, method = "pearson")
  rho_uncentered <- cor(pair$va_eb_math, pair$va_eb_other, method = "pearson")
  stopifnot(nrow(pair) > 2L, is.finite(rho), abs(rho - rho_uncentered) < 1e-10)
  fwrite(pair, file.path(out, paste0("matched_math_", keys[[o]], "_eb_school_values.csv")))
  data.table(outcome = o, label = labels[[o]], n_matched_schools = nrow(pair),
    pearson_correlation_eb = rho, pearson_correlation_eb_before_recentering = rho_uncentered,
    preceding_raw_correlation = cor(old$va_raw_math, old$va_raw_other),
    weighting = "Equal school weights", estimate_type = "Saved Stata All-sample EB VA")
}))
fwrite(results, file.path(out, "eb_school_va_correlations.csv"))
print(results)
