# Replace the outcome LPM with logit, reusing the exact saved b_i/g_i sample.
suppressPackageStartupMessages(library(data.table))
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
base_dir <- file.path(repo_wd, "data/clean/simce4_exam_predictiveness/bg_decomposition")
clean_dir <- file.path(base_dir, "logit")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
report_dir <- file.path(repo_wd, "output/reports/simce4_exam_predictiveness")
table_dir <- file.path(repo_wd, "output/tables/simce_prediction")
archive_pairs <- list(
  c(file.path(report_dir, "predicted_score_and_gap.html"), file.path(report_dir, "predicted_score_and_gap_lpm.html")),
  c(file.path(table_dir, "predicted_score_and_gap.csv"), file.path(table_dir, "predicted_score_and_gap_lpm.csv"))
)
for (paths in archive_pairs) if (!file.exists(paths[2L])) {
  stopifnot(file.exists(paths[1L]), file.copy(paths[1L], paths[2L], overwrite = FALSE))
  stopifnot(unname(tools::md5sum(paths[1L])) == unname(tools::md5sum(paths[2L])))
}
# Keep the archived LPM report's download link attached to its own table.
archived_html <- readLines(archive_pairs[[1L]][2L], warn = FALSE)
archived_html_fixed <- gsub("predicted_score_and_gap.csv", "predicted_score_and_gap_lpm.csv", archived_html, fixed = TRUE)
if (!identical(archived_html, archived_html_fixed)) writeLines(archived_html_fixed, archive_pairs[[1L]][2L], useBytes = TRUE)
preserve_paths <- c(file.path(base_dir, c("student_scores.rds", "prediction_roster.rds",
  "prediction_diagnostics.csv", "score_normalization.csv", "coefficients.csv", "regression_summary.csv",
  "second_stage_models.rds", "covariance_matrices.rds")),
  list.files(file.path(base_dir, "prediction_models"), full.names = TRUE),
  vapply(archive_pairs, `[`, character(1), 2L))
stopifnot(all(file.exists(preserve_paths)))
preservation <- data.table(path = preserve_paths, md5_before = unname(tools::md5sum(preserve_paths)))
checks <- data.table(check = character(), passed = logical())
check <- function(label, value) {
  checks <<- rbind(checks, data.table(check = label, passed = isTRUE(value)))
  if (!isTRUE(value)) stop("Failed: ", label)
  message("PASS: ", label)
}
d <- as.data.table(readRDS(file.path(base_dir, "student_scores.rds")))
lpm_summary <- fread(file.path(base_dir, "regression_summary.csv"))
check("Reuse the identical complete student sample", !anyDuplicated(d$mrun) &&
  all(lpm_summary$N == nrow(d)) && all(is.finite(d$b_i) & is.finite(d$g_i)))
check("Both outcomes and middle-school clusters remain observed", all(complete.cases(d[, .(he_applied, premium_field, most_time_rbd_middle)])) &&
  all(d$he_applied %in% 0:1) && all(d$premium_field %in% 0:1))
check("The saved score decomposition remains exact", max(abs(d$actual_score - d$b_i - d$g_i)) < 1e-12)

models <- list(); covariances <- list(); ame_covariances <- list()
coefs <- list(); marginal_effects <- list(); summaries <- list(); diagnostics <- list()
for (outcome in c("he_applied", "premium_field")) {
  message("Fitting logistic regression for ", outcome, "...")
  fit <- glm(reformulate(c("b_i", "g_i"), response = outcome), family = binomial(link = "logit"),
    data = d, x = TRUE, y = TRUE, model = TRUE,
    control = glm.control(epsilon = 1e-10, maxit = 100L))
  x <- fit$x; y <- fit$y; p <- fitted(fit); w <- p * (1 - p)
  beta <- coef(fit)
  N <- nrow(x); K <- ncol(x)
  cluster_scores <- rowsum(x * as.numeric(y - p), d$most_time_rbd_middle, reorder = FALSE)
  G <- nrow(cluster_scores)
  H <- crossprod(x, x * as.numeric(w))
  bread <- solve(H)
  V <- G / (G - 1) * (N - 1) / (N - K) * bread %*% crossprod(cluster_scores) %*% bread
  dimnames(V) <- list(names(beta), names(beta))
  se <- sqrt(diag(V)); critical <- qt(.975, df = G - 1)
  score_max <- max(abs(crossprod(x, y - p)))
  check(paste(outcome, "logit converges on the full sample"), isTRUE(fit$converged) && nobs(fit) == nrow(d) && fit$rank == 3L)
  check(paste(outcome, "likelihood score is near zero"), score_max < 1e-5)
  check(paste(outcome, "information and clustered covariance are positive definite"),
    all(is.finite(beta)) && min(eigen(H, symmetric = TRUE, only.values = TRUE)$values) > 0 &&
      all(is.finite(V)) && min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) > 0)
  coefs[[outcome]] <- data.table(outcome = outcome, term = names(beta), estimate = as.numeric(beta),
    std_error = se, p_value = 2 * pt(-abs(beta / se), df = G - 1),
    lower95 = beta - critical * se, upper95 = beta + critical * se, odds_ratio = exp(beta))

  # Average derivatives, not finite one-SD changes. Both b and g keep the
  # actual-score units; the common probability multiplier is mean[p(1-p)].
  mean_weight <- mean(w)
  d_mean_weight <- colMeans(x * as.numeric(w * (1 - 2 * p)))
  terms <- c("b_i", "g_i")
  D <- do.call(rbind, lapply(terms, function(term) {
    unit <- as.numeric(names(beta) == term)
    mean_weight * unit + beta[term] * d_mean_weight
  }))
  dimnames(D) <- list(terms, names(beta))
  ame <- beta[terms] * mean_weight
  V_ame <- D %*% V %*% t(D)
  ame_se <- sqrt(diag(V_ame))
  marginal_effects[[outcome]] <- data.table(outcome = outcome, term = terms, estimate = as.numeric(ame),
    std_error = ame_se, p_value = 2 * pt(-abs(ame / ame_se), df = G - 1),
    lower95 = ame - critical * ame_se, upper95 = ame + critical * ame_se)
  contrast <- c(0, 1, -1)
  beta_difference <- beta["b_i"] - beta["g_i"]
  beta_difference_se <- sqrt(as.numeric(t(contrast) %*% V %*% contrast))
  ame_difference_se <- sqrt(V_ame[1L, 1L] + V_ame[2L, 2L] - 2 * V_ame[1L, 2L])
  ll <- as.numeric(logLik(fit))
  ll0 <- sum(dbinom(y, 1L, mean(y), log = TRUE))
  check(paste(outcome, "likelihood improves on the intercept-only model"), is.finite(ll) && ll >= ll0)
  check(paste(outcome, "average marginal effects have finite delta-method SEs"), all(is.finite(ame)) && all(is.finite(ame_se) & ame_se > 0))
  summaries[[outcome]] <- data.table(outcome = outcome, N = N, middle_schools = G,
    mean_outcome = mean(y), log_likelihood = ll, null_log_likelihood = ll0,
    mcfadden_r2 = 1 - ll / ll0, iterations = fit$iter,
    b_equals_g_p = 2 * pt(-abs(beta_difference / beta_difference_se), df = G - 1),
    ame_b_equals_g_p = 2 * pt(-abs((ame[1L] - ame[2L]) / ame_difference_se), df = G - 1))
  diagnostics[[outcome]] <- data.table(outcome = outcome, max_abs_score = score_max,
    min_probability = min(p), max_probability = max(p), mean_probability = mean(p),
    mean_logistic_weight = mean_weight, hessian_condition = kappa(H, exact = TRUE))
  models[[outcome]] <- fit
  covariances[[outcome]] <- V
  ame_covariances[[outcome]] <- V_ame
}
fwrite(rbindlist(coefs), file.path(clean_dir, "coefficients.csv"))
fwrite(rbindlist(marginal_effects), file.path(clean_dir, "marginal_effects.csv"))
fwrite(rbindlist(summaries), file.path(clean_dir, "model_summary.csv"))
fwrite(rbindlist(diagnostics), file.path(clean_dir, "fit_diagnostics.csv"))
saveRDS(models, file.path(clean_dir, "models.rds"))
saveRDS(covariances, file.path(clean_dir, "covariance_matrices.rds"))
saveRDS(ame_covariances, file.path(clean_dir, "marginal_covariance_matrices.rds"))
preservation[, md5_after := unname(tools::md5sum(path))]
check("Prediction inputs, first-stage models and LPM results are unchanged", all(preservation$md5_before == preservation$md5_after))
fwrite(preservation, file.path(clean_dir, "source_preservation.csv"))
fwrite(checks, file.path(clean_dir, "fit_verification.csv"))
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
print(rbindlist(marginal_effects))
print(rbindlist(summaries))
message("Finished both logits; ", nrow(checks), " fit checks passed.")
