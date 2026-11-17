# Cubic-score LPMs and income's incremental share of full-model R-squared.
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setDTthreads(min(4L, getDTthreads())); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% c("--high-math-main", "--high-math-main-region-d5")))
region_d5 <- "--high-math-main-region-d5" %in% args
high_math_main <- region_d5 || "--high-math-main" %in% args
income_reference <- if (region_d5) 5L else 1L
task <- if (region_d5) "high_math_main_region_cubic_income_contribution" else
  if (high_math_main) "high_math_main_cubic_income_contribution" else "premium_cubic_income_contribution"
clean <- file.path(repo, "data/clean", task)
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
inputs <- file.path(repo, "data/clean", c("premium_enrollment_scores_income/analysis_data.rds",
  "premium_field_groups_scores_income/analysis_data.rds"))
if (high_math_main) inputs <- c(inputs, file.path(repo,
  "data/clean/high_math_income_enrollment_cubic_income_contribution/specification.rds"))
if (region_d5) inputs <- c(inputs, file.path(clean, "analysis_data.rds"), file.path(repo,
  "data/clean/high_math_main_cubic_income_contribution/income_contribution.csv"))
hashes <- unname(tools::md5sum(inputs))
fwrite(data.table(path = inputs, bytes = file.info(inputs)$size, md5 = hashes), file.path(clean, "source_manifest.csv"))
a <- as.data.table(readRDS(inputs[1L])); fields <- as.data.table(readRDS(inputs[2L]))
main_outcomes <- c("high_inst_m1", "high_paying_field_m1", "high_either", "high_both")
field_outcomes <- c("science", "law", "engineering", "medicine_plus")
outcomes <- if (high_math_main) main_outcomes else c(main_outcomes, field_outcomes)
labels <- c("Premium institution", "Premium field", "Either (or both)", "Both",
            "Science", "Law", "Engineering / Manufacturing / Construction", "Medicine+")
labels <- labels[seq_along(outcomes)]
stopifnot(!anyNA(a$mrun), !anyDuplicated(a$mrun), !anyNA(fields$mrun), !anyDuplicated(fields$mrun),
          nrow(a) == nrow(fields), setequal(a$mrun, fields$mrun))
fields <- fields[match(a$mrun, mrun)]
shared <- c("mrun", "most_time_RBD", "cohort_gr8", "enrolled", "psu_year",
            "income_decile_imputed", "z_math", "z_verbal", "high_paying_field_m1")
stopifnot(all(vapply(shared, function(v) identical(a[[v]], fields[[v]]), logical(1))))
for (o in field_outcomes) set(a, j = o, value = fields[[o]])
rm(fields)
stopifnot(all(unlist(a[, ..outcomes]) %in% 0:1),
          all(a$high_either == a$high_inst_m1 + a$high_paying_field_m1 - a$high_both),
          all(rowSums(a[, ..field_outcomes]) == a$high_paying_field_m1),
          all(unlist(a[enrolled == FALSE, ..outcomes]) == 0L),
          all(is.finite(a$z_math)), all(is.finite(a$z_verbal)),
          all(a$income_decile_imputed %in% 1:10), all(a$most_time_RBD > 0))
stopifnot(nrow(a) == 565040L, uniqueN(a$most_time_RBD) == 3372L)
if (high_math_main) {
  # Preserve the original within-admission-year scale and prior high-math IDs.
  a <- a[z_math >= 1]
  prior_selection <- readRDS(inputs[3L])
  stopifnot(isTRUE(prior_selection$high_math_only),
            identical(a$mrun, prior_selection$ids$he_enrollment), all(a$z_math >= 1))
  fwrite(data.table(N_parent = 565040L, N_retained = nrow(a), selection = "original z_math >= 1",
    min_z_math = min(a$z_math), n_exactly_one = sum(a$z_math == 1),
    exact_prior_high_math_ids = TRUE), file.path(clean, "sample_selection.csv"))
}
if (region_d5) {
  region_data <- as.data.table(readRDS(inputs[4L]))
  stopifnot(identical(a$mrun, region_data$mrun),
    all(vapply(shared, function(v) identical(a[[v]], region_data[[v]]), logical(1))),
    all(region_data$region_source_matched), all(region_data$region_observed),
    setequal(unique(region_data$region_origin_code), 1:16))
  a[, region_origin := factor(region_data$region_origin_code, levels = 1:16)]
  prior_no_region <- fread(inputs[5L])
  stopifnot(all(prior_no_region$N == nrow(a)))
  rm(region_data)
}
# OLS retains the all-zero admission-2026 cell among the selected students.
a[, `:=`(math2 = z_math^2, verbal2 = z_verbal^2, math3 = z_math^3, verbal3 = z_verbal^3,
         math_verbal = z_math * z_verbal, math2_verbal = z_math^2 * z_verbal,
         math_verbal2 = z_math * z_verbal^2,
         income_decile = relevel(factor(income_decile_imputed, levels = 1:10), ref = as.character(income_reference)),
         exam_year = factor(psu_year))]
score_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3",
                 "math2_verbal", "math_verbal2")
rhs_full <- c(score_terms, "income_decile", "exam_year", if (region_d5) "region_origin")
rhs_reduced <- c(score_terms, "exam_year", if (region_d5) "region_origin")
# Reuse one common design (~18/29 MB high-math without/with region; ~109 MB unrestricted).
X <- model.matrix(reformulate(rhs_full), a)
N <- nrow(X); K <- ncol(X); G <- uniqueN(a$most_time_RBD)
income_terms <- paste0("income_decile", setdiff(1:10, income_reference))
keep <- which(!colnames(X) %in% income_terms); K0 <- length(keep)
XtX <- crossprod(X)
stopifnot(N == if (high_math_main) 93423L else 565040L,
          G == if (high_math_main) 2735L else 3372L,
          K == if (region_d5) 39L else 24L, K0 == if (region_d5) 30L else 15L, qr(XtX)$rank == K)
bread <- chol2inv(chol(XtX)); bread0 <- chol2inv(chol(XtX[keep, keep]))
cluster_index <- match(a$most_time_RBD, unique(a$most_time_RBD))
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
correction <- (G / (G - 1)) * ((N - 1) / (N - K))
correction0 <- (G / (G - 1)) * ((N - 1) / (N - K0))
fwrite(a[, c(list(N = .N, n_enrolled = sum(enrolled)), lapply(.SD, sum)),
         keyby = psu_year, .SDcols = outcomes], file.path(clean, "outcome_counts_by_admission_year.csv"))
models <- list(); reduced_models <- list(); covariances <- list(); coefficients <- list()
summaries <- list(); checks <- list(); tests <- list(); score_effects <- list()
reference_checks <- list(); region_comparison <- list()
if (region_d5) {
  a_ref1 <- copy(a)
  a_ref1[, income_decile := relevel(income_decile, ref = "1")]
}
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]; y <- a[[outcome]]
  message("Cubic OLS and income deletion: ", labels[j])
  fit <- feols(reformulate(rhs_full, response = outcome), data = a,
               vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  fit0 <- feols(reformulate(rhs_reduced, response = outcome), data = a,
                vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit); e0 <- resid(fit0)
  stopifnot(nobs(fit) == N, nobs(fit0) == N, identical(names(b), colnames(X)),
            identical(names(coef(fit0)), colnames(X)[keep]), all(is.finite(b)))
  normal_error <- max(abs(crossprod(X, e))) / N
  normal_error0 <- max(abs(crossprod(X, e0)[keep])) / N
  cluster_scores <- rowsum(X * as.numeric(e), cluster_index, reorder = FALSE)
  V_check <- correction * bread %*% crossprod(cluster_scores) %*% bread
  cluster_scores0 <- rowsum(X * as.numeric(e0), cluster_index, reorder = FALSE)[, keep]
  V_check0 <- correction0 * bread0 %*% crossprod(cluster_scores0) %*% bread0
  cov_error <- max(abs(V - V_check)); cov_error0 <- max(abs(vcov(fit0) - V_check0))
  stopifnot(normal_error < 1e-8, normal_error0 < 1e-8, cov_error < 1e-9, cov_error0 < 1e-9)
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(outcome = outcome, outcome_label = labels[j], df = G - 1L,
            lower95 = estimate - qt(.975, G - 1L) * std_error,
            upper95 = estimate + qt(.975, G - 1L) * std_error)]
  coefficients[[outcome]] <- ct
  F_income <- as.numeric(crossprod(b[income_terms], solve(V[income_terms, income_terms], b[income_terms]))) / 9
  tests[[outcome]] <- data.table(outcome = outcome, block = "income_deciles", F = F_income,
    df1 = 9L, df2 = G - 1L, p_value = pf(F_income, 9L, G - 1L, lower.tail = FALSE))
  # Average score derivatives are linear functions of the cubic OLS coefficients.
  for (subject in c("math", "verbal")) {
    g <- setNames(numeric(K), names(b))
    if (subject == "math") {
      g[c("z_math", "math2", "math_verbal", "math3", "math2_verbal", "math_verbal2")] <-
        c(1, 2 * mean(a$z_math), mean(a$z_verbal), 3 * mean(a$math2),
          2 * mean(a$math_verbal), mean(a$verbal2))
    } else {
      g[c("z_verbal", "verbal2", "math_verbal", "verbal3", "math2_verbal", "math_verbal2")] <-
        c(1, 2 * mean(a$z_verbal), mean(a$z_math), 3 * mean(a$verbal2),
          mean(a$math2), 2 * mean(a$math_verbal))
    }
    estimate <- sum(g * b); se <- sqrt(as.numeric(crossprod(g, V %*% g)))
    score_effects[[paste(outcome, subject)]] <- data.table(outcome = outcome, term = paste0("ame_", subject),
      estimate = estimate, std_error = se, lower95 = estimate - qt(.975, G - 1L) * se,
      upper95 = estimate + qt(.975, G - 1L) * se, df = G - 1L,
      p_value = 2 * pt(-abs(estimate / se), df = G - 1L))
  }
  sst <- sum((y - mean(y))^2); sse <- sum(e^2); sse0 <- sum(e0^2)
  r2 <- 1 - sse / sst; r20 <- 1 - sse0 / sst
  delta <- (sse0 - sse) / sst
  projection_delta <- sum((fitted(fit) - fitted(fit0))^2) / sst
  stopifnot(r2 > 0, r20 >= 0, delta >= -1e-12, delta <= r2 + 1e-12,
            abs(delta - (r2 - r20)) < 1e-12, abs(delta - projection_delta) < 1e-10)
  if (region_d5) {
    # Independently refit with D1 omitted, then transform its coefficients/covariance to D5.
    fit_ref1 <- feols(reformulate(rhs_full, response = outcome), data = a_ref1,
      vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
    b1 <- coef(fit_ref1); V1 <- vcov(fit_ref1)
    T <- matrix(0, nrow = K, ncol = K, dimnames = list(names(b), names(b1)))
    for (t in setdiff(names(b), income_terms)) T[t, t] <- 1
    T["(Intercept)", "income_decile5"] <- 1
    for (q in setdiff(1:10, 5)) {
      t <- paste0("income_decile", q)
      if (q != 1) T[t, t] <- 1
      T[t, "income_decile5"] <- -1
    }
    coefficient_error <- max(abs(b - as.vector(T %*% b1)))
    reference_cov_error <- max(abs(V - T %*% V1 %*% t(T)))
    fitted_error <- max(abs(fitted(fit) - fitted(fit_ref1)))
    stopifnot(coefficient_error < 1e-7, reference_cov_error < 1e-8, fitted_error < 1e-8)
    reference_checks[[outcome]] <- data.table(outcome = outcome,
      max_rebased_coefficient_error = coefficient_error, max_rebased_covariance_error = reference_cov_error,
      max_fitted_difference = fitted_error, r2_difference = abs(r2 - (1 - sum(resid(fit_ref1)^2) / sst)))
    old <- prior_no_region[outcome == outcomes[j]]
    stopifnot(nrow(old) == 1L, r2 >= old$r2_full - 1e-10, r20 >= old$r2_without_income - 1e-10)
    region_comparison[[outcome]] <- data.table(outcome = outcome,
      r2_without_region = old$r2_full, r2_with_region = r2, region_r2_gain = r2 - old$r2_full,
      income_share_without_region = old$income_share_explained, income_share_with_region = delta / r2)
  }
  summaries[[outcome]] <- data.table(outcome = outcome, outcome_label = labels[j],
    outcome_set = if (outcome %in% main_outcomes) "main" else "fields", N = N, n_schools = G,
    n_parameters_full = K, n_parameters_no_income = K0, mean_outcome = mean(y),
    r2_full = r2, r2_without_income = r20, delta_r2_income = delta,
    income_share_explained = delta / r2, partial_r2_income = (sse0 - sse) / sse0,
    sst = sst, sse_full = sse, sse_without_income = sse0,
    n_non_enrollees = sum(!a$enrolled), n_admission_2026 = sum(a$psu_year == 2026L),
    share_predictions_below_zero = mean(fitted(fit) < 0), share_predictions_above_one = mean(fitted(fit) > 1))
  checks[[outcome]] <- data.table(outcome = outcome, N = N,
    max_normal_equation_full = normal_error, max_normal_equation_reduced = normal_error0,
    max_covariance_error_full = cov_error, max_covariance_error_reduced = cov_error0,
    nested_projection_delta_error = abs(delta - projection_delta))
  models[[outcome]] <- fit; reduced_models[[outcome]] <- fit0; covariances[[outcome]] <- V
}
# Same-design linear coefficients preserve outcome identities in both specifications.
identities <- list()
for (spec in c("full", "without_income")) {
  m <- if (spec == "full") models else reduced_models
  identity_union <- coef(m$high_either) - coef(m$high_inst_m1) - coef(m$high_paying_field_m1) + coef(m$high_both)
  stopifnot(max(abs(identity_union)) < 1e-9)
  identities[[spec]] <- data.table(specification = spec, term = names(identity_union),
                                   union_error = identity_union)
  if (!high_math_main) {
    identity_fields <- Reduce(`+`, lapply(m[field_outcomes], coef)) - coef(m$high_paying_field_m1)
    stopifnot(max(abs(identity_fields)) < 1e-9)
    identities[[spec]][, field_sum_error := identity_fields]
  }
}
current <- rbindlist(coefficients)
if (!high_math_main) {
  # The unrestricted field models must reproduce the previous cubic OLS exactly.
  previous <- fread(file.path(repo, "data/clean/premium_field_groups_cubic_scores_income/coefficients.csv"))
  comparison <- merge(current[outcome %in% field_outcomes, .(outcome, term, estimate, std_error)],
                      previous[, .(outcome, term, prior_estimate = estimate, prior_se = std_error)], by = c("outcome", "term"))
  stopifnot(nrow(comparison) == length(field_outcomes) * K,
            max(abs(comparison$estimate - comparison$prior_estimate)) < 1e-10,
            max(abs(comparison$std_error - comparison$prior_se)) < 1e-10)
  fwrite(comparison, file.path(clean, "prior_field_replication.csv"))
}
fwrite(rbindlist(identities), file.path(clean, "coefficient_identity_verification.csv"))
fwrite(current, file.path(clean, "coefficients.csv"))
fwrite(rbindlist(score_effects), file.path(clean, "score_average_derivatives.csv"))
fwrite(rbindlist(summaries), file.path(clean, "income_contribution.csv"))
fwrite(rbindlist(checks), file.path(clean, "regression_verification.csv"))
fwrite(rbindlist(tests), file.path(clean, "income_joint_tests.csv"))
if (region_d5) {
  fwrite(rbindlist(reference_checks), file.path(clean, "reference_category_verification.csv"))
  fwrite(rbindlist(region_comparison), file.path(clean, "region_control_comparison.csv"))
}
saveRDS(models, file.path(clean, "models_full.rds"))
saveRDS(reduced_models, file.path(clean, "models_without_income.rds"))
saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
saveRDS(list(sources = inputs, source_md5 = hashes, ids = a$mrun, rhs_full = rhs_full,
  rhs_reduced = rhs_reduced, cluster = "most_time_RBD",
  high_math_main = high_math_main, outcomes = outcomes,
  income_reference = income_reference, region_fixed_effects = region_d5,
  region_source = if (region_d5) "same-admission-year CODIGO_REGION_DOMICILIO from PAES/PDT socioeconomics" else "none",
  math_selection = if (high_math_main) "original z_math >= 1" else "none",
  income_fraction = "(R2_full - R2_without_income) / R2_full"), file.path(clean, "specification.rds"))
stopifnot(identical(unname(tools::md5sum(inputs)), hashes))
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(rbindlist(summaries)[, .(outcome, N, r2_full, r2_without_income, delta_r2_income,
                              income_share_pct = 100 * income_share_explained, partial_r2_income)])
