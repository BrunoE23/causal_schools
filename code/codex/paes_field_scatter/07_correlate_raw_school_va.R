suppressPackageStartupMessages(library(data.table))
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "paes_field_scatter", "school_va_correlations")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
read_va <- function(key, expected_outcome) {
  p <- file.path(repo, "output", "tables", "empirical_bayes_school_va",
                 paste0("stata_va_eb_input_", key, ".csv"))
  d <- fread(p, select = c("school_rbd", "analysis_sample", "outcome", "va_raw", "va_centered", "n_students"))
  d <- d[analysis_sample == "All" & is.finite(va_raw)]
  stopifnot(!anyDuplicated(d$school_rbd), !anyNA(d$school_rbd), all(d$outcome == expected_outcome),
    all(d$n_students > 0), all(is.finite(d$va_centered)), diff(range(d$va_raw - d$va_centered)) < 1e-6)
  d
}
math <- read_va("math", "z_year_math_max")
keys <- c(high_paying_field_m1 = "highpay", high_inst_m1 = "highinst")
labels <- c(high_paying_field_m1 = "High-premium field", high_inst_m1 = "High-premium institution")
results <- rbindlist(lapply(names(keys), function(o) {
  other <- read_va(keys[[o]], o)
  # Pairwise common schools, equal school weights; include private schools.
  pair <- merge(math, other, by = "school_rbd", suffixes = c("_math", "_other"))
  rho <- cor(pair$va_raw_math, pair$va_raw_other, method = "pearson")
  rho_centered <- cor(pair$va_centered_math, pair$va_centered_other, method = "pearson")
  stopifnot(nrow(pair) > 2L, is.finite(rho), abs(rho - rho_centered) < 1e-10)
  fwrite(pair, file.path(out, paste0("matched_math_", keys[[o]], "_school_values.csv")))
  data.table(outcome = o, label = labels[[o]], n_math_schools = nrow(math),
    n_other_schools = nrow(other), n_matched_schools = nrow(pair),
    pearson_correlation_raw = rho, pearson_correlation_centered = rho_centered,
    weighting = "Equal school weights", estimate_type = "Saved raw Stata All-sample VA, before EB")
}))
fwrite(results, file.path(out, "raw_school_va_correlations.csv"))
print(results)
