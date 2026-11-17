# Flexible-score OLS for enrollee-only log projected income and HE enrollment.
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setDTthreads(min(4L, getDTthreads())); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--high-math"))
high_math_only <- "--high-math" %in% args
task <- if (high_math_only) "high_math_income_enrollment_cubic_income_contribution" else
  "income_enrollment_cubic_income_contribution"
clean <- file.path(repo, "data/clean", task)
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
inputs <- file.path(repo, "data/clean", c("premium_enrollment_scores_income/analysis_data.rds",
  "all_students_by_attended_va/broad_panel_analysis_data.rds"))
hashes <- unname(tools::md5sum(inputs))
fwrite(data.table(path = inputs, bytes = file.info(inputs)$size, md5 = hashes), file.path(clean, "source_manifest.csv"))
a <- as.data.table(readRDS(inputs[1L]))
# The 23 MB cached broad panel avoids rereading the >1 GB master. Retain only needed columns.
shared <- c("mrun", "most_time_RBD", "cohort_gr8", "enrolled", "psu_year",
            "income_decile_imputed", "z_math", "z_verbal")
income_columns <- c("program_income_full_clp_m1", "log_program_income_full_clp_m1")
d <- as.data.table(readRDS(inputs[2L]))[, c(shared, income_columns), with = FALSE]
stopifnot(!anyNA(a$mrun), !anyDuplicated(a$mrun), !anyNA(d$mrun), !anyDuplicated(d$mrun),
          all(a$mrun %in% d$mrun))
d <- d[match(a$mrun, mrun)]
stopifnot(all(vapply(shared, function(v) identical(a[[v]], d[[v]]), logical(1))))
for (v in income_columns) set(a, j = v, value = d[[v]])
rm(d)
a[, `:=`(he_enrollment = as.integer(enrolled), log_projected_income = log_program_income_full_clp_m1,
  valid_projected_income = is.finite(program_income_full_clp_m1) & program_income_full_clp_m1 > 0 &
                           is.finite(log_program_income_full_clp_m1))]
stopifnot(nrow(a) == 565040L, uniqueN(a$most_time_RBD) == 3372L,
          !anyNA(a$enrolled), all(a$he_enrollment %in% 0:1),
          all(a[enrolled == TRUE, valid_projected_income]),
          all(is.finite(a$z_math)), all(is.finite(a$z_verbal)), all(a$income_decile_imputed %in% 1:10),
          max(abs(a[enrolled == TRUE, log_projected_income - log(program_income_full_clp_m1)])) < 1e-10)
parent_n <- nrow(a)
expected_ids <- if (high_math_only) a[z_math >= 1, mrun] else a$mrun
# Apply the original broad-panel, within-admission-year cutoff without restandardizing.
if (high_math_only) a <- a[z_math >= 1]
stopifnot(identical(a$mrun, expected_ids), nrow(a) > 0L,
          !high_math_only || all(a$z_math >= 1))
fwrite(data.table(N_parent = parent_n, N_retained = nrow(a), high_math_only = high_math_only,
  selection = if (high_math_only) "original z_math >= 1" else "all prior complete-score students",
  min_z_math = min(a$z_math), max_z_math = max(a$z_math), n_exactly_one = sum(a$z_math == 1)),
  file.path(clean, "sample_selection.csv"))
coverage <- a[, .(N_base = .N, n_enrolled = sum(enrolled), n_non_enrolled = sum(!enrolled),
  n_enrolled_valid_income = sum(enrolled & valid_projected_income),
  n_enrolled_missing_income = sum(enrolled & !valid_projected_income),
  n_admission_2026 = sum(psu_year == 2026L), n_enrolled_admission_2026 = sum(enrolled & psu_year == 2026L))]
fwrite(coverage, file.path(clean, "sample_coverage.csv"))
stopifnot(coverage$n_enrolled_missing_income == 0L)
saveRDS(a[, c(shared, "he_enrollment", "log_projected_income", "valid_projected_income"), with = FALSE],
        file.path(clean, "analysis_data.rds"))
a[, `:=`(math2 = z_math^2, verbal2 = z_verbal^2, math3 = z_math^3, verbal3 = z_verbal^3,
         math_verbal = z_math * z_verbal, math2_verbal = z_math^2 * z_verbal,
         math_verbal2 = z_math * z_verbal^2)]
score_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3",
                 "math2_verbal", "math_verbal2")
rhs_full <- c(score_terms, "income_decile", "exam_year")
rhs_reduced <- c(score_terms, "exam_year")
income_terms <- paste0("income_decile", 2:10)
outcomes <- c("log_projected_income", "he_enrollment")
labels <- c("Log projected income among HE enrollees", "HE enrollment")
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
models <- list(); reduced_models <- list(); covariances <- list(); coefficients <- list()
summaries <- list(); checks <- list(); tests <- list(); score_effects <- list(); sample_ids <- list(); by_year <- list()
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]
  x <- if (outcome == "log_projected_income") a[enrolled == TRUE & valid_projected_income == TRUE] else copy(a)
  # Rebuild factor levels in each outcome sample; no empty-year dummy is introduced.
  x[, `:=`(income_decile = factor(income_decile_imputed, levels = 1:10), exam_year = factor(psu_year))]
  y <- x[[outcome]]; X <- model.matrix(reformulate(rhs_full), x)
  N <- nrow(X); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
  keep <- which(!colnames(X) %in% income_terms); K0 <- length(keep); XtX <- crossprod(X)
  stopifnot(qr(XtX)$rank == K, all(is.finite(y)), K - K0 == 9L,
            N == if (outcome == "log_projected_income") coverage$n_enrolled else coverage$N_base)
  bread <- chol2inv(chol(XtX)); bread0 <- chol2inv(chol(XtX[keep, keep]))
  ci <- match(x$most_time_RBD, unique(x$most_time_RBD))
  correction <- (G / (G - 1)) * ((N - 1) / (N - K))
  correction0 <- (G / (G - 1)) * ((N - 1) / (N - K0))
  message("Cubic OLS and income deletion: ", labels[j], " (N=", N, ", G=", G, ")")
  fit <- feols(reformulate(rhs_full, response = outcome), data = x,
               vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  fit0 <- feols(reformulate(rhs_reduced, response = outcome), data = x,
                vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit); e0 <- resid(fit0)
  stopifnot(nobs(fit) == N, nobs(fit0) == N, identical(names(b), colnames(X)),
            identical(names(coef(fit0)), colnames(X)[keep]), all(is.finite(b)))
  normal_error <- max(abs(crossprod(X, e))) / N
  normal_error0 <- max(abs(crossprod(X, e0)[keep])) / N
  cluster_scores <- rowsum(X * as.numeric(e), ci, reorder = FALSE)
  V_check <- correction * bread %*% crossprod(cluster_scores) %*% bread
  cluster_scores0 <- rowsum(X * as.numeric(e0), ci, reorder = FALSE)[, keep]
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
  for (subject in c("math", "verbal")) {
    g <- setNames(numeric(K), names(b))
    if (subject == "math") {
      g[c("z_math", "math2", "math_verbal", "math3", "math2_verbal", "math_verbal2")] <-
        c(1, 2 * mean(x$z_math), mean(x$z_verbal), 3 * mean(x$math2),
          2 * mean(x$math_verbal), mean(x$verbal2))
    } else {
      g[c("z_verbal", "verbal2", "math_verbal", "verbal3", "math2_verbal", "math_verbal2")] <-
        c(1, 2 * mean(x$z_verbal), mean(x$z_math), 3 * mean(x$verbal2),
          mean(x$math2), 2 * mean(x$math_verbal))
    }
    estimate <- sum(g * b); se <- sqrt(as.numeric(crossprod(g, V %*% g)))
    score_effects[[paste(outcome, subject)]] <- data.table(outcome = outcome, term = paste0("ame_", subject),
      estimate = estimate, std_error = se, lower95 = estimate - qt(.975, G - 1L) * se,
      upper95 = estimate + qt(.975, G - 1L) * se, df = G - 1L,
      p_value = 2 * pt(-abs(estimate / se), df = G - 1L))
  }
  sst <- sum((y - mean(y))^2); sse <- sum(e^2); sse0 <- sum(e0^2)
  r2 <- 1 - sse / sst; r20 <- 1 - sse0 / sst; delta <- (sse0 - sse) / sst
  projection_delta <- sum((fitted(fit) - fitted(fit0))^2) / sst
  stopifnot(r2 > 0, r20 >= 0, delta >= -1e-12, delta <= r2 + 1e-12,
            abs(delta - (r2 - r20)) < 1e-12, abs(delta - projection_delta) < 1e-10)
  summaries[[outcome]] <- data.table(outcome = outcome, outcome_label = labels[j],
    outcome_unit = if (outcome == "log_projected_income") "natural_log_CLP_per_month" else "binary",
    N = N, n_schools = G, n_parameters_full = K, n_parameters_no_income = K0, mean_outcome = mean(y),
    r2_full = r2, r2_without_income = r20, delta_r2_income = delta,
    income_share_explained = delta / r2, partial_r2_income = (sse0 - sse) / sse0,
    sst = sst, sse_full = sse, sse_without_income = sse0,
    n_non_enrollees = sum(!x$enrolled), n_admission_2026 = sum(x$psu_year == 2026L),
    share_predictions_below_zero = if (outcome == "he_enrollment") mean(fitted(fit) < 0) else NA_real_,
    share_predictions_above_one = if (outcome == "he_enrollment") mean(fitted(fit) > 1) else NA_real_)
  checks[[outcome]] <- data.table(outcome = outcome, N = N,
    max_normal_equation_full = normal_error, max_normal_equation_reduced = normal_error0,
    max_covariance_error_full = cov_error, max_covariance_error_reduced = cov_error0,
    nested_projection_delta_error = abs(delta - projection_delta))
  models[[outcome]] <- fit; reduced_models[[outcome]] <- fit0; covariances[[outcome]] <- V
  sample_ids[[outcome]] <- x$mrun
  by_year[[outcome]] <- x[, .(outcome = outcome, N = .N, n_enrolled = sum(enrolled), mean_outcome = mean(get(outcome))), keyby = psu_year]
  rm(X, x, cluster_scores, cluster_scores0)
}
stopifnot(setequal(sample_ids$log_projected_income, a[enrolled == TRUE, mrun]),
          identical(sample_ids$he_enrollment, a$mrun))
fwrite(rbindlist(coefficients), file.path(clean, "coefficients.csv"))
fwrite(rbindlist(score_effects), file.path(clean, "score_average_derivatives.csv"))
fwrite(rbindlist(summaries), file.path(clean, "income_contribution.csv"))
fwrite(rbindlist(checks), file.path(clean, "regression_verification.csv"))
fwrite(rbindlist(tests), file.path(clean, "income_joint_tests.csv"))
fwrite(rbindlist(by_year), file.path(clean, "samples_by_admission_year.csv"))
saveRDS(models, file.path(clean, "models_full.rds"))
saveRDS(reduced_models, file.path(clean, "models_without_income.rds"))
saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
saveRDS(list(sources = inputs, source_md5 = hashes, ids = sample_ids, rhs_full = rhs_full,
  rhs_reduced = rhs_reduced, cluster = "most_time_RBD",
  high_math_only = high_math_only, math_selection = if (high_math_only) "original z_math >= 1" else "none",
  income_fraction = "(R2_full - R2_without_income) / R2_full"), file.path(clean, "specification.rds"))
stopifnot(identical(unname(tools::md5sum(inputs)), hashes))
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(coverage)
print(rbindlist(summaries)[, .(outcome, N, n_schools, r2_full, r2_without_income, delta_r2_income,
                              income_share_pct = 100 * income_share_explained)])
