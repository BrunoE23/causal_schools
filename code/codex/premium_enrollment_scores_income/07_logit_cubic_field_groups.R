# Four cubic-score logits, clustered inference, probability effects, and common-sample OLS fit comparison.
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setDTthreads(min(4L, getDTthreads())); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
prior <- file.path(repo, "data/clean/premium_field_groups_scores_income")
clean <- file.path(repo, "data/clean/premium_field_groups_logit_cubic_scores_income")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(prior, "analysis_data.rds")
input_hash <- unname(tools::md5sum(cache))
fwrite(data.table(path = cache, bytes = file.info(cache)$size, md5 = input_hash), file.path(clean, "source_manifest.csv"))
a <- as.data.table(readRDS(cache)); original_n <- nrow(a)
outcomes <- c("science", "law", "engineering", "medicine_plus")
labels <- c("Science", "Law", "Engineering / Manufacturing / Construction", "Medicine+")
stopifnot(!anyDuplicated(a$mrun), !anyNA(a$mrun), all(unlist(a[, ..outcomes]) %in% 0:1),
          all(rowSums(a[, ..outcomes]) == a$high_paying_field_m1))
support <- a[, c(list(N = .N), lapply(.SD, function(y) uniqueN(y) == 2L)),
             keyby = psu_year, .SDcols = outcomes]
support[, retained := Reduce(`&`, .SD), .SDcols = outcomes]
keep_years <- support[retained == TRUE, psu_year]
fwrite(support, file.path(clean, "admission_year_support.csv"))
a <- a[psu_year %in% keep_years]
stopifnot(nrow(a) > 0L, all(is.finite(a$z_math)), all(is.finite(a$z_verbal)),
          all(unlist(a[enrolled == FALSE, ..outcomes]) == 0L))
a[, `:=`(math2 = z_math^2, verbal2 = z_verbal^2, math3 = z_math^3, verbal3 = z_verbal^3,
         math_verbal = z_math * z_verbal, math2_verbal = z_math^2 * z_verbal,
         math_verbal2 = z_math * z_verbal^2,
         income_decile = factor(income_decile_imputed, levels = 1:10), exam_year = factor(psu_year))]
score_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3", "math2_verbal", "math_verbal2")
rhs <- c(score_terms, "income_decile", "exam_year")
# One ~103 MB common design is reused; effects use crossproducts, not copied counterfactual matrices.
X <- model.matrix(reformulate(rhs), a)
N <- nrow(X); K <- ncol(X); G <- uniqueN(a$most_time_RBD)
stopifnot(qr(crossprod(X))$rank == K, all(is.finite(X)))
ci <- match(a$most_time_RBD, unique(a$most_time_RBD))
correction <- (G / (G - 1)) * ((N - 1) / (N - K))
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
income_terms <- paste0("income_decile", 2:10)
income_cols <- match(income_terms, colnames(X))
cluster_sizes <- a[, .N, by = most_time_RBD]
fwrite(data.table(original_n = original_n, N = N, excluded_n = original_n - N, n_schools = G,
  excluded_years = paste(support[retained == FALSE, psu_year], collapse = ","),
  n_non_enrollees = sum(!a$enrolled), n_parameters = K), file.path(clean, "sample_counts.csv"))
fwrite(cluster_sizes, file.path(clean, "cluster_sizes.csv"))

derivative_basis <- function(subject) {
  if (subject == "math") list(z_math = rep(1, N), math2 = 2 * a$z_math,
    math_verbal = a$z_verbal, math3 = 3 * a$z_math^2,
    math2_verbal = 2 * a$z_math * a$z_verbal, math_verbal2 = a$z_verbal^2)
  else list(z_verbal = rep(1, N), verbal2 = 2 * a$z_verbal,
    math_verbal = a$z_math, verbal3 = 3 * a$z_verbal^2,
    math2_verbal = a$z_math^2, math_verbal2 = 2 * a$z_math * a$z_verbal)
}
dm <- derivative_basis("math"); dv <- derivative_basis("verbal")
effect_record <- function(outcome_key, effect_key, estimate, gradient, V, type, adjusted_probability = NA_real_) {
  se <- sqrt(as.numeric(crossprod(gradient, V %*% gradient)))
  data.table(outcome = outcome_key, effect = effect_key, type = type, estimate = estimate, std_error = se,
    lower95 = estimate - qnorm(.975) * se, upper95 = estimate + qnorm(.975) * se,
    p_value = 2 * pnorm(-abs(estimate / se)), adjusted_probability = adjusted_probability)
}
auc_rank <- function(y, p) {
  n1 <- as.numeric(sum(y)); n0 <- as.numeric(length(y)) - n1
  (sum(rank(p, ties.method = "average")[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
models <- list(); ols_models <- list(); coefs <- list(); effects <- list(); gradients <- list()
summaries <- list(); checks <- list(); covariances <- list(); probability_sums <- numeric(N)
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]; y <- a[[outcome]]
  message("Logit: ", labels[j])
  fit <- feglm(reformulate(rhs, response = outcome), data = a, family = "binomial",
    vcov = ~most_time_RBD, ssc = ssc_rule, glm.iter = 100L, glm.tol = 1e-12, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); eta <- as.vector(X %*% b); p <- plogis(eta); w <- p * (1 - p)
  stopifnot(isTRUE(fit$convStatus), nobs(fit) == N, identical(names(b), colnames(X)),
    all(is.finite(b)), all(p > 0 & p < 1), max(abs(fitted(fit) - p)) < 1e-10)
  score_error <- max(abs(crossprod(X, y - p))) / N
  H <- crossprod(X, X * w); bread <- chol2inv(chol(H))
  cluster_scores <- rowsum(X * (y - p), ci, reorder = FALSE)
  V_check <- correction * bread %*% crossprod(cluster_scores) %*% bread
  cov_difference <- max(abs(V - V_check))
  cov_relative_difference <- max(abs(diag(V) - diag(V_check)) / diag(V_check))
  stopifnot(score_error < 1e-8, cov_relative_difference < 1e-5, all(diag(V_check) > 0))
  # Evaluate the sandwich at the converged coefficients. Package IRLS covariance
  # can retain the preceding iteration's nearly identical Hessian weights.
  dimnames(V_check) <- list(names(b), names(b)); V <- V_check
  se <- sqrt(diag(V))
  coefs[[outcome]] <- data.table(outcome = outcome, term = names(b), estimate = as.numeric(b),
    std_error = se, odds_ratio = exp(b), lower95 = b - qnorm(.975) * se, upper95 = b + qnorm(.975) * se,
    p_value = 2 * pnorm(-abs(b / se)))
  probability_sums <- probability_sums + p
  # Income: average discrete contrasts Dq versus D1, preserving each student's scores/year.
  income_contribution <- numeric(N)
  for (q in 2:10) income_contribution[a$income_decile_imputed == q] <- b[paste0("income_decile", q)]
  eta0 <- eta - income_contribution; p0 <- plogis(eta0); w0 <- p0 * (1 - p0)
  effects[[paste(outcome, "D1")]] <- data.table(outcome = outcome, effect = "income_decile1", type = "reference",
    estimate = 0, std_error = NA_real_, lower95 = NA_real_, upper95 = NA_real_, p_value = NA_real_, adjusted_probability = mean(p0))
  gradient_error <- 0
  for (q in 2:10) {
    term <- paste0("income_decile", q)
    pq <- plogis(eta0 + b[term]); wq <- pq * (1 - pq)
    g <- as.vector(crossprod(X, wq - w0)) / N; names(g) <- names(b)
    g[income_cols] <- 0; g[term] <- mean(wq)
    effects[[paste(outcome, term)]] <- effect_record(outcome, term, mean(pq - p0), g, V, "discrete_contrast", mean(pq))
    gradients[[paste(outcome, term)]] <- g
  }
  # Scores: average partial derivatives, accounting for every cubic and interaction term.
  for (subject in c("math", "verbal")) {
    D <- if (subject == "math") dm else dv
    slope <- Reduce(`+`, lapply(names(D), function(t) D[[t]] * b[t]))
    g <- as.vector(crossprod(X, w * (1 - 2 * p) * slope)) / N; names(g) <- names(b)
    for (t in names(D)) g[t] <- g[t] + mean(w * D[[t]])
    key <- paste0("ame_", subject)
    effects[[paste(outcome, key)]] <- effect_record(outcome, key, mean(w * slope), g, V, "average_partial_derivative")
    gradients[[paste(outcome, key)]] <- g
  }
  # Directional finite differences check the complete delta-method gradients.
  direction <- sin(seq_along(b)); direction <- direction / sqrt(sum(direction^2)); names(direction) <- names(b)
  h <- 1e-5; eta_direction <- as.vector(X %*% direction)
  dir0 <- eta_direction
  for (q in 2:10) dir0[a$income_decile_imputed == q] <- dir0[a$income_decile_imputed == q] - direction[paste0("income_decile", q)]
  for (key in c(income_terms, "ame_math", "ame_verbal")) {
    if (grepl("^income_decile", key)) {
      objective <- function(sign) mean(plogis(eta0 + sign * h * dir0 + b[key] + sign * h * direction[key]) -
                                       plogis(eta0 + sign * h * dir0))
    } else {
      D <- if (key == "ame_math") dm else dv
      slope <- Reduce(`+`, lapply(names(D), function(t) D[[t]] * b[t]))
      ds <- Reduce(`+`, lapply(names(D), function(t) D[[t]] * direction[t]))
      objective <- function(sign) {
        pp <- plogis(eta + sign * h * eta_direction)
        mean(pp * (1 - pp) * (slope + sign * h * ds))
      }
    }
    numeric_derivative <- (objective(1) - objective(-1)) / (2 * h)
    analytic_derivative <- sum(gradients[[paste(outcome, key)]] * direction)
    gradient_error <- max(gradient_error, abs(numeric_derivative - analytic_derivative))
  }
  stopifnot(gradient_error < 1e-7)
  message("Common-sample OLS comparison: ", labels[j])
  ols <- feols(reformulate(rhs, response = outcome), data = a, vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  prevalence <- mean(y); baseline_brier <- mean((y - prevalence)^2)
  ll <- sum(y * eta - softplus(eta))
  ll0 <- sum(y) * log(prevalence) + sum(1 - y) * log1p(-prevalence)
  logit_brier <- mean((y - p)^2); ols_brier <- mean(resid(ols)^2)
  auc <- auc_rank(y, p)
  stopifnot(abs(ll - as.numeric(logLik(fit))) < 1e-5, ll >= ll0, nobs(ols) == N,
            is.finite(auc), auc >= 0, auc <= 1)
  summaries[[outcome]] <- data.table(outcome = outcome, outcome_label = labels[j], N = N, n_schools = G,
    n_parameters = K, mean_outcome = prevalence, mcfadden_r2 = 1 - ll / ll0,
    tjur_r2 = mean(p[y == 1]) - mean(p[y == 0]), logit_brier = logit_brier,
    ols_brier = ols_brier, intercept_only_brier = baseline_brier,
    logit_brier_r2 = 1 - logit_brier / baseline_brier, ols_r2_same_sample = 1 - ols_brier / baseline_brier,
    auc = auc, log_likelihood = ll, null_log_likelihood = ll0,
    mean_predicted = mean(p), n_non_enrollees = sum(!a$enrolled))
  checks[[outcome]] <- data.table(outcome = outcome, converged = fit$convStatus,
    max_logit_score_equation = score_error, max_cluster_covariance_difference = cov_difference,
    max_relative_variance_difference = cov_relative_difference,
    max_delta_gradient_error = gradient_error, mean_calibration_error = abs(mean(p) - mean(y)), auc_verified = TRUE)
  models[[outcome]] <- fit; ols_models[[outcome]] <- ols; covariances[[outcome]] <- V
  # Checkpoint each finished outcome; large student-level objects stay in ignored clean data.
  fwrite(rbindlist(summaries), file.path(clean, "model_summary.csv"))
  fwrite(rbindlist(effects), file.path(clean, "probability_effects.csv"))
}
fwrite(rbindlist(coefs), file.path(clean, "coefficients_log_odds.csv"))
fwrite(rbindlist(checks), file.path(clean, "verification.csv"))
fwrite(data.table(max_sum_fitted_probabilities = max(probability_sums),
  share_sum_above_one = mean(probability_sums > 1)), file.path(clean, "independent_logits_diagnostic.csv"))
saveRDS(models, file.path(clean, "models.rds")); saveRDS(ols_models, file.path(clean, "ols_same_sample_models.rds"))
saveRDS(covariances, file.path(clean, "covariance_matrices.rds")); saveRDS(gradients, file.path(clean, "effect_gradients.rds"))
saveRDS(list(source = cache, source_md5 = input_hash, retained_years = keep_years, rhs = rhs, ids = a$mrun),
        file.path(clean, "estimation_sample_specification.rds"))
stopifnot(unname(tools::md5sum(cache)) == input_hash)
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(rbindlist(summaries))
print(rbindlist(effects)[effect %in% c("income_decile10", "ame_math", "ame_verbal"),
  .(outcome, effect, estimate_pp = 100 * estimate, se_pp = 100 * std_error)])
