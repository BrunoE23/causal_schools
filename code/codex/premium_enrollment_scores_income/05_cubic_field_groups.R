# Full bivariate cubic score controls, retaining the prior field-group OLS sample.
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setDTthreads(min(4L, getDTthreads())); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
prior <- file.path(repo, "data/clean/premium_field_groups_scores_income")
clean <- file.path(repo, "data/clean/premium_field_groups_cubic_scores_income")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
inputs <- file.path(prior, c("analysis_data.rds", "coefficients.csv", "model_summary.csv"))
hashes <- unname(tools::md5sum(inputs))
fwrite(data.table(path = inputs, bytes = file.info(inputs)$size, md5 = hashes), file.path(clean, "source_manifest.csv"))
a <- as.data.table(readRDS(inputs[1L]))
prior_coefs <- fread(inputs[2L]); prior_summary <- fread(inputs[3L])
outcomes <- c("science", "law", "engineering", "medicine_plus")
labels <- c("Science", "Law", "Engineering / Manufacturing / Construction", "Medicine+")
stopifnot(!anyNA(a$mrun), !anyDuplicated(a$mrun), all(prior_summary$N == nrow(a)),
          all(is.finite(a$z_math)), all(is.finite(a$z_verbal)),
          all(rowSums(a[, ..outcomes]) == a$high_paying_field_m1),
          all(unlist(a[enrolled == FALSE, ..outcomes]) == 0L))
original_ids <- a$mrun
# Raw powers of the original year-standardized scores: total degree at most three.
a[, `:=`(math2 = z_math^2, verbal2 = z_verbal^2, math3 = z_math^3, verbal3 = z_verbal^3,
         math_verbal = z_math * z_verbal, math2_verbal = z_math^2 * z_verbal,
         math_verbal2 = z_math * z_verbal^2,
         income_decile = factor(income_decile_imputed, levels = 1:10), exam_year = factor(psu_year))]
score_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal",
                 "math3", "verbal3", "math2_verbal", "math_verbal2")
rhs <- c(score_terms, "income_decile", "exam_year")
X <- model.matrix(reformulate(rhs), data = a)
N <- nrow(X); K <- ncol(X); G <- uniqueN(a$most_time_RBD)
stopifnot(K == 24L, all(is.finite(X)), all(prior_summary$n_schools == G), identical(a$mrun, original_ids))
# A single 24-column design (~109 MB) is reused for four fits and covariance audits.
XtX <- crossprod(X)
stopifnot(qr(XtX)$rank == K)
bread <- chol2inv(chol(XtX))
dimnames(bread) <- dimnames(XtX)
cluster_index <- match(a$most_time_RBD, unique(a$most_time_RBD))
correction <- (G / (G - 1)) * ((N - 1) / (N - K))
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
models <- list(); coefs <- list(); summaries <- list(); checks <- list(); covariances <- list(); tests <- list()
score_blocks <- list(added_flexibility = setdiff(score_terms, c("z_math", "z_verbal")),
                     score_interactions = c("math_verbal", "math2_verbal", "math_verbal2"),
                     cubic_terms = c("math3", "verbal3", "math2_verbal", "math_verbal2"))
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]
  message("Cubic OLS field group: ", labels[j])
  fit <- feols(reformulate(rhs, response = outcome), data = a,
               vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  stopifnot(nobs(fit) == N, identical(names(b), colnames(X)), all(is.finite(b)))
  normal_error <- max(abs(crossprod(X, e))) / N
  cluster_scores <- rowsum(X * as.numeric(e), cluster_index, reorder = FALSE)
  V_check <- correction * bread %*% crossprod(cluster_scores) %*% bread
  stopifnot(normal_error < 1e-8, max(abs(V - V_check)) < 1e-9, all(diag(V) > 0))
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(outcome = outcome, outcome_label = labels[j], df = G - 1L,
            lower95 = estimate - qt(.975, G - 1L) * std_error,
            upper95 = estimate + qt(.975, G - 1L) * std_error)]
  coefs[[outcome]] <- ct
  for (block in names(score_blocks)) {
    tt <- score_blocks[[block]]; q <- length(tt)
    F_stat <- as.numeric(crossprod(b[tt], solve(V[tt, tt, drop = FALSE], b[tt]))) / q
    tests[[paste(outcome, block)]] <- data.table(outcome = outcome, block = block, F = F_stat,
      df1 = q, df2 = G - 1L, p_value = pf(F_stat, q, G - 1L, lower.tail = FALSE))
  }
  pred <- fitted(fit)
  r2 <- 1 - sum(e^2) / sum((a[[outcome]] - mean(a[[outcome]]))^2)
  old <- prior_summary[outcome == outcomes[j]]
  stopifnot(nrow(old) == 1L, r2 >= old$r_squared - 1e-12,
            abs(mean(a[[outcome]]) - old$mean_outcome) < 1e-12)
  summaries[[outcome]] <- data.table(outcome = outcome, outcome_label = labels[j], N = N,
    n_schools = G, n_parameters = K, mean_outcome = mean(a[[outcome]]), r_squared = r2,
    previous_r_squared = old$r_squared, r_squared_gain = r2 - old$r_squared,
    share_predictions_below_zero = mean(pred < 0), share_predictions_above_one = mean(pred > 1),
    n_non_enrollees = sum(!a$enrolled), n_admission_2026 = sum(a$psu_year == 2026L))
  checks[[outcome]] <- data.table(outcome = outcome, N = N, n_schools = G,
    max_ols_normal_equation = normal_error, max_cluster_covariance_difference = max(abs(V - V_check)))
  models[[outcome]] <- fit; covariances[[outcome]] <- V
}
# Audit component additivity against the exact aggregate OLS normal-equation solution.
summed_coefs <- Reduce(`+`, lapply(models, coef))
aggregate_b <- as.vector(bread %*% crossprod(X, a$high_paying_field_m1))
comparison <- data.table(term = names(summed_coefs), sum_components = as.numeric(summed_coefs),
                         aggregate_cubic_ols = aggregate_b)
comparison[, difference := sum_components - aggregate_cubic_ols]
stopifnot(max(abs(comparison$difference)) < 1e-8)
fwrite(comparison, file.path(clean, "coefficient_additivity_verification.csv"))
all_coefs <- rbindlist(coefs)
income_comparison <- merge(all_coefs[grepl("^income_decile", term), .(outcome, term, estimate, std_error)],
  prior_coefs[grepl("^income_decile", term), .(outcome, term, previous_estimate = estimate,
                                             previous_std_error = std_error)], by = c("outcome", "term"))
income_comparison[, change := estimate - previous_estimate]
fwrite(income_comparison, file.path(clean, "income_coefficient_comparison.csv"))
fwrite(all_coefs, file.path(clean, "coefficients.csv"))
fwrite(rbindlist(summaries), file.path(clean, "model_summary.csv"))
fwrite(rbindlist(checks), file.path(clean, "regression_verification.csv"))
fwrite(rbindlist(tests), file.path(clean, "joint_tests.csv"))
saveRDS(models, file.path(clean, "models.rds")); saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
saveRDS(list(source_sample = inputs[1L], source_md5 = hashes[1L], rhs = rhs,
             score_basis = score_terms, n_parameters = K, cluster = "most_time_RBD"),
        file.path(clean, "specification.rds"))
fwrite(fread(file.path(prior, "sample_flow.csv")), file.path(clean, "sample_flow.csv"))
stopifnot(identical(unname(tools::md5sum(inputs)), hashes))
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(rbindlist(summaries)[, .(outcome, N, n_schools, previous_r_squared, r_squared, r_squared_gain)])
print(all_coefs[term == "income_decile10", .(outcome, estimate_pp = 100 * estimate, se_pp = 100 * std_error)])
print(rbindlist(tests))
