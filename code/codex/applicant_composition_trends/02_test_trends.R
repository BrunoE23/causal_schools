suppressPackageStartupMessages({library(data.table); library(sandwich)})
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
dest <- "data/clean/applicant_composition_trends"
d <- as.data.table(readRDS(file.path(dest, "applicant_school_frame.rds")))
cells <- fread(file.path(dest, "school_cohort_means_long.csv"))
specs <- fread(file.path(dest, "outcome_definitions.csv"))
top <- fread(file.path(dest, "top100_schools.csv"))
fits <- list(); results <- list()

for (s in top$rbd) {
  for (v in specs$outcome) {
    z <- cells[rbd == s & outcome == v & n_observed > 0L & is.finite(mean)]
    setorder(z, sae_proceso)
    z[, t := sae_proceso - 2018L]
    stopifnot(nrow(z) >= 2L)
    fit <- lm(mean ~ t, data = z)
    fits[[paste(s, v, sep = "_")]] <- fit
    b <- unname(coef(fit)["t"])
    k <- nrow(z); constant <- max(z$mean) == min(z$mean)
    test <- k > 2L && !constant
    se <- if (test) sqrt(vcov(fit)["t", "t"]) else NA_real_
    p <- if (test) 2 * pt(-abs(b / se), df = k - 2L) else NA_real_
    crit <- if (test) qt(.975, df = k - 2L) else NA_real_

    # Equal total weight for each cohort. This student-level regression gives
    # exactly the same coefficient as OLS on the cohort means. HC2 is computed
    # from the regression, retaining within-cohort individual residual variation.
    x <- d[rbd == s & !is.na(get(v)), .(y = get(v), t = sae_proceso - 2018L)]
    x[, w := 1 / .N, by = t]
    individual <- lm(y ~ t, data = x, weights = w)
    ib <- unname(coef(individual)["t"])
    stopifnot(abs(b - ib) < 1e-9)
    individual_constant <- max(x$y) == min(x$y)
    ise <- if (!individual_constant) sqrt(vcovHC(individual, type = "HC2")["t", "t"]) else NA_real_
    ip <- if (is.finite(ise) && ise > 0) 2 * pt(-abs(ib / ise), df = nrow(x) - 2L) else NA_real_
    icrit <- qt(.975, df = nrow(x) - 2L)
    results[[length(results) + 1L]] <- data.table(rbd = s, outcome = v,
      n_cohorts = k, n_students = nrow(x), first_cohort = min(z$sae_proceso), last_cohort = max(z$sae_proceso),
      first_mean = z$mean[1L], last_mean = z$mean[k], change = z$mean[k] - z$mean[1L],
      cohort_mean_range = max(z$mean) - min(z$mean), slope_per_year = b,
      cohort_ols_se = se, cohort_ols_df = k - 2L, cohort_ols_p = p,
      cohort_ols_ci_low = b - crit * se, cohort_ols_ci_high = b + crit * se,
      cohort_ols_status = if (constant) "constant cohort means" else if (k == 2L) "two cohorts: no residual degrees of freedom" else "estimated",
      independent_applicant_hc2_se = ise, independent_applicant_df = nrow(x) - 2L,
      independent_applicant_p = ip,
      independent_applicant_ci_low = b - icrit * ise, independent_applicant_ci_high = b + icrit * ise,
      min_coverage = min(z$coverage), max_coverage = max(z$coverage),
      coverage_change = z$coverage[k] - z$coverage[1L],
      independent_applicant_status = if (individual_constant) "constant individual outcome" else "estimated")
  }
}
trends <- merge(rbindlist(results), specs, by = "outcome")
trends <- merge(trends, top, by = "rbd")
setorder(trends, rank, outcome)
trends[, `:=`(display_slope = scale * slope_per_year, display_change = scale * change)]
fwrite(trends, file.path(dest, "school_trend_tests.csv"))
fwrite(trends[primary == TRUE], file.path(dest, "primary_school_trend_tests.csv"))
saveRDS(fits, file.path(dest, "cohort_ols_models.rds"))

# School-level overlap across the three requested outcomes, using raw p < .05.
# Schools with no testable outcome are excluded from the zero-detection count.
overlap <- rbindlist(lapply(c("cohort_ols_p", "independent_applicant_p"), function(pcol) {
  x <- trends[primary == TRUE]
  x[, .(method = pcol, n_cohorts = unique(n_cohorts),
    n_testable = sum(!is.na(get(pcol))),
    n_significant = sum(get(pcol) < .05, na.rm = TRUE),
    significant_outcomes = paste(outcome[!is.na(get(pcol)) & get(pcol) < .05], collapse = "; "),
    simce_significant = ifelse(is.na(get(pcol)[outcome == "simce_baseline"]), NA, get(pcol)[outcome == "simce_baseline"] < .05),
    female_significant = ifelse(is.na(get(pcol)[outcome == "female"]), NA, get(pcol)[outcome == "female"] < .05),
    parents_significant = ifelse(is.na(get(pcol)[outcome == "parents_education"]), NA, get(pcol)[outcome == "parents_education"] < .05)),
    by = .(rank, rbd, school_name, comuna)]
}))
overlap_summary <- overlap[, .(total_schools = .N, schools_with_any_test = sum(n_testable > 0L),
  no_testable_outcomes = sum(n_testable == 0L), all_three_p_values_available = sum(n_testable == 3L),
  zero_detected_among_testable = sum(n_testable > 0L & n_significant == 0L),
  exactly_one = sum(n_significant == 1L), exactly_two = sum(n_significant == 2L),
  all_three = sum(n_significant == 3L), at_least_one = sum(n_significant >= 1L),
  at_least_two = sum(n_significant >= 2L)), by = method]
fwrite(overlap, file.path(dest, "school_significant_trend_counts.csv"))
fwrite(overlap_summary, file.path(dest, "significant_trend_count_summary.csv"))
print(overlap_summary)

# Report counts of raw p-values only, as explicitly requested by the user.
summary <- trends[, .(schools = .N,
  three_cohort_schools = sum(n_cohorts == 3L), two_cohort_schools = sum(n_cohorts == 2L),
  cohort_ols_testable = sum(!is.na(cohort_ols_p)), cohort_ols_p_lt_05 = sum(cohort_ols_p < .05, na.rm = TRUE),
  independent_applicant_testable = sum(!is.na(independent_applicant_p)),
  independent_applicant_p_lt_05 = sum(independent_applicant_p < .05, na.rm = TRUE),
  median_annual_slope = median(display_slope), median_absolute_annual_slope = median(abs(display_slope)),
  p90_absolute_annual_slope = unname(quantile(abs(display_slope), .90)),
  mean_min_coverage = mean(min_coverage), minimum_coverage = min(min_coverage)),
  by = .(outcome, label, primary, unit)]
fwrite(summary, file.path(dest, "trend_summary.csv"))

# Separate endpoint windows: a 2019-2020 change is not a 2018-2020 change.
changes <- trends[, .(schools = .N, median_change = median(display_change),
  median_absolute_change = median(abs(display_change)),
  p90_absolute_change = unname(quantile(abs(display_change), .9)),
  maximum_absolute_change = max(abs(display_change))), by = .(outcome, label, unit, first_cohort, last_cohort)]
fwrite(changes, file.path(dest, "endpoint_change_summary.csv"))
rank_stability <- rbindlist(lapply(specs$outcome, function(v) {
  rbindlist(lapply(list(c(2018L, 2019L), c(2019L, 2020L), c(2018L, 2020L)), function(yy) {
    a <- cells[outcome == v & sae_proceso == yy[1] & is.finite(mean), .(rbd, a = mean)]
    b <- cells[outcome == v & sae_proceso == yy[2] & is.finite(mean), .(rbd, b = mean)]
    x <- merge(a, b, by = "rbd")
    data.table(outcome = v, year_from = yy[1], year_to = yy[2], schools = nrow(x),
      pearson = cor(x$a, x$b), spearman = cor(x$a, x$b, method = "spearman"))
  }))
}))
fwrite(rank_stability, file.path(dest, "cross_cohort_school_correlations.csv"))
coverage <- cells[n_applicants > 0L, .(applicant_school_pairs = sum(n_applicants), observed_pairs = sum(n_observed),
  coverage = sum(n_observed) / sum(n_applicants), minimum_school_coverage = min(coverage)),
  by = .(outcome, label, sae_proceso)]
fwrite(coverage, file.path(dest, "coverage_summary.csv"))
print(summary[primary == TRUE])
print(changes[outcome %in% specs[primary == TRUE, outcome]])
print(rank_stability[outcome %in% specs[primary == TRUE, outcome]])
writeLines(capture.output(sessionInfo()), file.path(dest, "session_info.txt"))
