# Independent numerical checks of the panel, regressions, and school counts.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
dest <- "data/clean/applicant_composition_trends"
d <- as.data.table(readRDS(file.path(dest, "applicant_school_frame.rds")))
c <- fread(file.path(dest, "school_cohort_means_long.csv"))
t <- fread(file.path(dest, "school_trend_tests.csv"))
s <- fread(file.path(dest, "outcome_definitions.csv"))
top <- fread(file.path(dest, "top100_schools.csv"))
counts <- fread(file.path(dest, "school_significant_trend_counts.csv"))
checks <- list()
check <- function(name, ok, error = 0) {
  checks[[length(checks) + 1L]] <<- data.table(check = name, passed = isTRUE(ok), max_error = error)
  if (!isTRUE(ok)) stop(name)
}
check("unique applicant-school-cohort rows", !anyDuplicated(d[, .(mrun, sae_proceso, rbd)]))
check("one SAE process per applicant", d[, max(uniqueN(sae_proceso)), by = mrun][, max(V1)] == 1L)
check("300 cells and 1000 trend rows", nrow(c) == 3000L && nrow(t) == 1000L && uniqueN(c$rbd) == 100L)
check("pooled school counts", identical(as.integer(d[, .N, by = rbd][match(top$rbd, rbd), N]), as.integer(top$applications)))
check("empty cohorts have missing means", c[n_applicants == 0L, all(is.na(mean) & n_observed == 0L)] && c[n_applicants == 0L, .N] == 140L)

for (v in s$outcome) {
  # Base R aggregation provides a separate route from the data.table constructor.
  z <- as.data.frame(d[, .(rbd, sae_proceso, y = get(v))])
  z <- z[!is.na(z$y), ]
  a <- aggregate(y ~ rbd + sae_proceso, data = z, FUN = mean)
  n <- aggregate(y ~ rbd + sae_proceso, data = z, FUN = length)
  m <- merge(c[outcome == v & n_observed > 0L, .(rbd, sae_proceso, mean, n_observed)], a, by = c("rbd", "sae_proceso"))
  err <- max(abs(m$mean - m$y))
  check(paste(v, "means from base R aggregation"), nrow(m) == 286L && err < 1e-10, err)
  m <- merge(m, n, by = c("rbd", "sae_proceso"), suffixes = c("", "_n"))
  check(paste(v, "observed denominators"), all(m$n_observed == m$y_n))
}

# For three equally spaced cohorts, these closed forms are exact OLS objects.
z <- dcast(c[n_observed > 0L], rbd + outcome ~ sae_proceso, value.var = "mean")
z <- merge(z, t, by = c("rbd", "outcome"))
z[, closed_slope := (get("2020") - fifelse(is.na(get("2018")), get("2019"), get("2018"))) /
                      fifelse(is.na(get("2018")), 1, 2)]
err <- max(abs(z$closed_slope - z$slope_per_year))
check("all slopes equal endpoint closed form", err < 1e-10, err)
z <- z[n_cohorts == 3L & !is.na(cohort_ols_se)]
z[, closed_se := abs(get("2018") - 2 * get("2019") + get("2020")) / sqrt(12)]
err <- max(abs(z$closed_se - z$cohort_ols_se))
check("three-cohort OLS SE closed form", err < 1e-10, err)
check("two-cohort OLS p-values missing", t[n_cohorts == 2L, all(is.na(cohort_ols_p) & cohort_ols_df == 0L)])

# Independently construct the weighted-regression HC2 covariance using matrices.
# Cover a regular three-year school, a two-year school, and the constant-gender cases.
ids <- unique(c(top$rbd[1L], top[applicant_cohorts == 2L, rbd][1L],
                t[outcome == "female" & is.na(independent_applicant_p), rbd]))
for (id in ids) for (v in s$outcome) {
  x <- d[rbd == id & !is.na(get(v)), .(y = get(v), year = sae_proceso - 2018)]
  row <- t[rbd == id & outcome == v]
  if (max(x$y) == min(x$y)) {
    check(paste(id, v, "constant has no artificial p-value"), is.na(row$independent_applicant_p))
    next
  }
  X <- cbind(1, x$year)
  w <- 1 / as.numeric(table(x$year)[as.character(x$year)])
  inv <- solve(crossprod(X, X * w))
  b <- inv %*% crossprod(X, w * x$y)
  e <- x$y - drop(X %*% b)
  h <- w * rowSums((X %*% inv) * X)
  V <- inv %*% crossprod(X, X * (w^2 * e^2 / (1 - h))) %*% inv
  err <- abs(sqrt(V[2, 2]) - row$independent_applicant_hc2_se)
  check(paste(id, v, "HC2 matrix covariance"), err < 1e-9, err)
}

for (method_id in unique(counts$method)) {
  x <- t[primary == TRUE, .(rbd, outcome, p = get(method_id))]
  x <- dcast(x, rbd ~ outcome, value.var = "p")
  expected <- rowSums(as.matrix(x[, -"rbd"]) < .05, na.rm = TRUE)
  got <- counts[method == method_id][match(x$rbd, rbd), n_significant]
  check(paste(method_id, "school significance overlap"), identical(as.integer(expected), as.integer(got)))
}
check("primary outcomes exactly three per school", t[primary == TRUE, .N, by = rbd][, all(N == 3L)])
check("all p-values unadjusted and valid", all(t$cohort_ols_p >= 0 & t$cohort_ols_p <= 1, na.rm = TRUE) &&
      all(t$independent_applicant_p >= 0 & t$independent_applicant_p <= 1, na.rm = TRUE))
fwrite(rbindlist(checks), file.path(dest, "verification.csv"))
message("Passed ", length(checks), " numerical and data-integrity checks.")
