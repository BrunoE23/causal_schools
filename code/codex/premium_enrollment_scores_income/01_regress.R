# Four OLS linear probability models: premium enrollment on scores and income deciles.
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setDTthreads(min(4L, getDTthreads()))
setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/premium_enrollment_scores_income")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(repo, "data/clean/all_students_by_attended_va/broad_panel_analysis_data.rds")
input_hash <- unname(tools::md5sum(cache))
fwrite(data.table(path = cache, bytes = file.info(cache)$size, md5 = input_hash),
       file.path(clean, "source_manifest.csv"))
d <- as.data.table(readRDS(cache))
outcomes <- c("high_inst_m1", "high_paying_field_m1", "high_either", "high_both")
labels <- c("Premium institution", "Premium field", "Either (or both)", "Both")
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun),
          all(unlist(d[, ..outcomes]) %in% 0:1),
          all(d$high_either == d$high_inst_m1 + d$high_paying_field_m1 - d$high_both),
          all(unlist(d[enrolled == FALSE, ..outcomes]) == 0L))
sample_flow <- data.table(stage = c("Paper broad panel", "Observed family-income decile",
  "Income and math score observed", "Income and both scores observed", "Complete common regression sample"),
  N = c(nrow(d), sum(d$income_available), sum(d$income_available & d$math_observed),
        sum(d$income_available & d$math_observed & d$verbal_observed), NA_integer_))
required <- c(outcomes, "z_math", "z_verbal", "income_decile_imputed", "psu_year", "most_time_RBD")
a <- d[income_available & math_observed & verbal_observed & complete.cases(d[, ..required])]
sample_flow[stage == "Complete common regression sample", N := nrow(a)]
stopifnot(nrow(a) > 0L, all(a$income_decile_imputed %in% 1:10),
          all(is.finite(a$z_math)), all(is.finite(a$z_verbal)), all(a$most_time_RBD > 0))
a[, `:=`(income_decile = factor(income_decile_imputed, levels = 1:10),
         exam_year = factor(psu_year))]
# OLS retains constant-outcome year cells; no logit separation exclusion applies.
year_support <- a[, c(list(N = .N, n_enrolled = sum(enrolled)),
  lapply(.SD, sum)), by = psu_year, .SDcols = outcomes]
setorder(year_support, psu_year)
fwrite(sample_flow, file.path(clean, "sample_flow.csv"))
fwrite(year_support, file.path(clean, "outcome_counts_by_admission_year.csv"))
fwrite(a[, .(N = .N, n_enrolled = sum(enrolled), mean_math = mean(z_math), mean_verbal = mean(z_verbal)),
         keyby = income_decile_imputed], file.path(clean, "sample_by_income_decile.csv"))

rhs <- c("z_math", "z_verbal", "income_decile", "exam_year")
X <- model.matrix(reformulate(rhs), data = a)
N <- nrow(X); K <- ncol(X); G <- uniqueN(a$most_time_RBD)
stopifnot(qr(X)$rank == K, G > K)
cluster_index <- match(a$most_time_RBD, unique(a$most_time_RBD))
correction <- (G / (G - 1)) * ((N - 1) / (N - K))
bread <- solve(crossprod(X))
models <- list(); coefs <- list(); summaries <- list(); checks <- list()
covariances <- list(); joint_tests <- list()
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]
  message("OLS: ", labels[j])
  fit <- feols(reformulate(rhs, response = outcome), data = a,
               vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  stopifnot(nobs(fit) == N, identical(names(b), colnames(X)), all(is.finite(b)),
            max(abs(crossprod(X, e))) / N < 1e-9)
  # Audit the actual OLS school-clustered sandwich covariance, including CR1 correction.
  cluster_scores <- rowsum(X * as.numeric(e), cluster_index, reorder = FALSE)
  V_check <- correction * bread %*% crossprod(cluster_scores) %*% bread
  stopifnot(max(abs(V - V_check)) < 1e-9, all(diag(V) > 0))
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(outcome = outcome, outcome_label = labels[j], df = G - 1L,
            lower95 = estimate - qt(.975, G - 1L) * std_error,
            upper95 = estimate + qt(.975, G - 1L) * std_error)]
  coefs[[outcome]] <- ct
  for (block in c("scores", "income_deciles")) {
    terms <- if (block == "scores") c("z_math", "z_verbal") else grep("^income_decile", names(b), value = TRUE)
    q <- length(terms)
    wald <- as.numeric(crossprod(b[terms], solve(V[terms, terms, drop = FALSE], b[terms])))
    joint_tests[[paste(outcome, block)]] <- data.table(outcome = outcome, block = block,
      F = wald / q, df1 = q, df2 = G - 1L, p_value = pf(wald / q, q, G - 1L, lower.tail = FALSE))
  }
  pred <- fitted(fit)
  summaries[[outcome]] <- data.table(outcome = outcome, outcome_label = labels[j], N = N,
    n_schools = G, n_parameters = K, mean_outcome = mean(a[[outcome]]),
    r_squared = 1 - sum(e^2) / sum((a[[outcome]] - mean(a[[outcome]]))^2),
    share_predictions_below_zero = mean(pred < 0), share_predictions_above_one = mean(pred > 1),
    n_non_enrollees = sum(!a$enrolled), n_admission_2026 = sum(a$psu_year == 2026L))
  checks[[outcome]] <- data.table(outcome = outcome, N = N, n_schools = G,
    max_ols_normal_equation = max(abs(crossprod(X, e))) / N,
    max_cluster_covariance_difference = max(abs(V - V_check)))
  models[[outcome]] <- fit
  covariances[[outcome]] <- V
}
# The same linear operator must preserve the identity either = institution + field - both.
identity_error <- coef(models$high_either) - coef(models$high_inst_m1) -
  coef(models$high_paying_field_m1) + coef(models$high_both)
stopifnot(max(abs(identity_error)) < 1e-9)
fwrite(data.table(term = names(identity_error), identity_error = as.numeric(identity_error)),
       file.path(clean, "coefficient_identity_verification.csv"))
fwrite(rbindlist(coefs), file.path(clean, "coefficients.csv"))
fwrite(rbindlist(summaries), file.path(clean, "model_summary.csv"))
fwrite(rbindlist(checks), file.path(clean, "regression_verification.csv"))
fwrite(rbindlist(joint_tests), file.path(clean, "joint_tests.csv"))
saveRDS(models, file.path(clean, "models.rds"))
saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
saveRDS(a[, c("mrun", "most_time_RBD", "cohort_gr8", "enrolled", "psu_year",
               "income_decile_imputed", "z_math", "z_verbal", outcomes), with = FALSE],
        file.path(clean, "analysis_data.rds"))
stopifnot(unname(tools::md5sum(cache)) == input_hash)
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(sample_flow)
print(rbindlist(summaries))
print(rbindlist(coefs)[term %in% c("z_math", "z_verbal", "income_decile10"),
  .(outcome_label, term, estimate_pp = 100 * estimate, se_pp = 100 * std_error)])
