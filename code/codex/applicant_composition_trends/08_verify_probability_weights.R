suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
base <- "data/clean/applicant_composition_trends"
dest <- file.path(base, "probability_weighted")
d <- as.data.table(readRDS(file.path(dest, "applicant_school_frame.rds")))
original <- as.data.table(readRDS(file.path(base, "applicant_school_frame.rds")))
cells <- fread(file.path(dest, "school_cohort_means_long.csv"))
trends <- fread(file.path(dest, "school_trend_tests.csv"))
specs <- fread(file.path(dest, "outcome_definitions.csv"))
top <- fread(file.path(dest, "top100_schools.csv"))
checks <- list()
check <- function(label, valid, error = 0) {
  checks[[length(checks) + 1L]] <<- data.table(check = label, passed = isTRUE(valid), max_error = error)
  if (!isTRUE(valid)) stop(label)
}
check("same top100 schools and ranking", identical(top, fread(file.path(base, "top100_schools.csv"))))
keys <- c("mrun", "sae_proceso", "rbd")
setorderv(d, keys); setorderv(original, keys)
check("same applicant-school rows and baseline characteristics", isTRUE(all.equal(as.data.frame(d[, names(original), with = FALSE]), as.data.frame(original), tolerance = 0)))
check("valid probabilities", all(is.finite(d$admission_probability)) && all(d$admission_probability >= 0 & d$admission_probability <= 1 + 1e-12))
mass <- as.data.table(readRDS(file.path(dest, "student_probability_mass_audit.rds")))
check("complete distributions including unmatched", nrow(mass) == uniqueN(d$mrun) && max(abs(mass$probability_mass - 1)) < 1e-6)
check("zero-mass cells are missing means", cells[observed_probability_mass == 0, all(is.na(mean))])
check("concentration diagnostic bounds", cells[observed_probability_mass > 0, all(effective_n >= 1 - 1e-10 & effective_n <= n_observed + 1e-8)])
check("missing outcomes excluded from weight denominator", cells[, all(observed_probability_mass <= expected_admissions + 1e-9)])

for (v in specs$outcome) {
  # Independently sum numerators and denominators with base aggregate.
  x <- as.data.frame(d[admission_probability > 0 & !is.na(get(v)),
    .(rbd, sae_proceso, numerator = admission_probability * get(v), denominator = admission_probability)])
  a <- aggregate(cbind(numerator, denominator) ~ rbd + sae_proceso, data = x, FUN = sum)
  a$independent_mean <- a$numerator / a$denominator
  m <- merge(cells[outcome == v & observed_probability_mass > 0,
    .(rbd, sae_proceso, mean, observed_probability_mass)], a, by = c("rbd", "sae_proceso"))
  err <- max(abs(m$mean - m$independent_mean))
  check(paste(v, "weighted means from base aggregation"), nrow(m) == nrow(a) && err < 1e-10, err)
  err <- max(abs(m$observed_probability_mass - m$denominator))
  check(paste(v, "weight denominators"), err < 1e-9, err)
}

max_b_error <- 0; max_se_error <- 0; max_p_error <- 0; fits_checked <- 0L
for (i in seq_len(nrow(trends))) {
  row <- trends[i]
  id <- row$rbd; v <- row$outcome
  z <- cells[rbd == id & outcome == v & is.finite(mean)]
  if (nrow(z) < 2L) {
    check(paste(id, v, "insufficient cohorts produce no trend"), is.na(row$slope_per_year) && is.na(row$cohort_ols_p) && is.na(row$independent_applicant_p))
    next
  }
  tt <- z$sae_proceso - mean(z$sae_proceso)
  beta <- sum(tt * (z$mean - mean(z$mean))) / sum(tt^2)
  check(paste(id, v, "cohort slope"), abs(beta - row$slope_per_year) < 1e-9, abs(beta - row$slope_per_year))
  if (!is.na(row$cohort_ols_se)) {
    residual <- z$mean - mean(z$mean) - beta * tt
    se <- sqrt(sum(residual^2) / (nrow(z) - 2) / sum(tt^2))
    check(paste(id, v, "cohort regression covariance"), abs(se - row$cohort_ols_se) < 1e-9, abs(se - row$cohort_ols_se))
  }
  # Exact weighted-regression HC2 covariance, calculated without lm/sandwich.
  x <- d[rbd == id & admission_probability > 0 & !is.na(get(v)),
    .(y = get(v), year = sae_proceso - 2018, p = admission_probability)]
  x[, weight := p / sum(p), by = year]
  X <- cbind(1, x$year)
  inv <- solve(crossprod(X, X * x$weight))
  b <- inv %*% crossprod(X, x$weight * x$y)
  max_b_error <- max(max_b_error, abs(b[2] - row$slope_per_year))
  if (is.na(row$independent_applicant_p)) next
  e <- x$y - drop(X %*% b)
  h <- x$weight * rowSums((X %*% inv) * X)
  V <- inv %*% crossprod(X, X * (x$weight^2 * e^2 / (1 - h))) %*% inv
  se <- sqrt(V[2, 2])
  pvalue <- 2 * pt(-abs(b[2] / se), nrow(x) - 2L)
  max_se_error <- max(max_se_error, abs(se - row$independent_applicant_hc2_se))
  max_p_error <- max(max_p_error, abs(pvalue - row$independent_applicant_p))
  fits_checked <- fits_checked + 1L
}
check("all student WLS slopes match cohort OLS", max_b_error < 1e-9, max_b_error)
check("all estimable HC2 covariance matrices", max_se_error < 1e-9, max_se_error)
check("all estimable applicant p-values", max_p_error < 1e-8, max_p_error)

counts <- fread(file.path(dest, "school_significant_trend_counts.csv"))
summary <- fread(file.path(dest, "significant_trend_count_summary.csv"))
for (method_id in unique(counts$method)) {
  x <- trends[primary == TRUE, .(rbd, outcome, p = get(method_id))]
  wide <- dcast(x, rbd ~ outcome, value.var = "p")
  expected <- rowSums(as.matrix(wide[, -"rbd"]) < .05, na.rm = TRUE)
  got <- counts[method == method_id][match(wide$rbd, rbd), n_significant]
  check(paste(method_id, "per-school significance counts"), identical(as.integer(expected), as.integer(got)))
  row <- summary[method == method_id]
  check(paste(method_id, "summary partitions"), row$at_least_one == row$exactly_one + row$exactly_two + row$all_three &&
    row$at_least_two == row$exactly_two + row$all_three &&
    row$zero_detected_among_testable + row$at_least_one + row$no_testable_outcomes == 100L)
}
fwrite(rbindlist(checks), file.path(dest, "verification.csv"))
message("Passed ", length(checks), " checks; independently verified ", fits_checked, " HC2 regressions.")
