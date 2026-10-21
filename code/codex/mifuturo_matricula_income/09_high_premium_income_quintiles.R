# Binary high-premium outcomes by parental-income quintile, with score controls.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
root <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", "C:/Users/brunem/Research/causal_schools")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
cache <- file.path(root, "data/clean/projected_income_quintiles/analysis_data.rds")
master <- file.path(data_root, "data/clean/univ_gr8_df.csv")
clean <- file.path(root, "data/clean/high_premium_income_quintiles")
tables <- file.path(root, "output/tables/high_premium_income_quintiles")
reports <- file.path(root, "output/reports/high_premium_income_quintiles")
for (p in c(clean, tables, reports)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
original_hash <- unname(tools::md5sum(cache))
d <- as.data.table(readRDS(cache))
message("Linking the two existing master outcomes...")
fwrite(data.table(path = master, bytes = file.info(master)$size, selected_columns = 3L), file.path(clean, "source_manifest.csv"))
o <- fread(master, select = c("mrun", "high_paying_field_m1", "high_inst_m1"), na.strings = c("", "NA"))
o <- o[mrun %in% d$mrun]
stopifnot(nrow(o) == nrow(d), !anyDuplicated(o$mrun))
d <- merge(d, o, by = "mrun", all.x = TRUE, sort = FALSE)
outcomes <- c("high_paying_field_m1", "high_inst_m1")
stopifnot(all(unlist(d[, ..outcomes]) %in% c(0L, 1L, NA_integer_)))
N_before <- nrow(d)
d <- d[complete.cases(d[, ..outcomes])]
stopifnot(all(d[enrolled == FALSE, high_paying_field_m1] == 0), all(d[enrolled == FALSE, high_inst_m1] == 0))
samples <- list(all_students = d, enrolled_only = d[enrolled == TRUE])
coefs <- list(); effects <- list(); summaries <- list(); covariances <- list(); fitted_models <- list(); sample_flow <- list()
for (s in names(samples)) {
  x <- samples[[s]]
  N_initial <- nrow(x)
  # Fixed-effect logit has no information from years with constant outcomes.
  year_support <- x[, .(field_varies = uniqueN(high_paying_field_m1) == 2L,
    institution_varies = uniqueN(high_inst_m1) == 2L), by = psu_year]
  keep_years <- year_support[field_varies & institution_varies, psu_year]
  x <- x[psu_year %in% keep_years]
  x[, exam_year := relevel(factor(psu_year), ref = names(which.max(table(psu_year))))]
  sample_flow[[s]] <- data.table(sample = s, N_before_year_support = N_initial, N_estimation = nrow(x),
    omitted_years = paste(setdiff(year_support$psu_year, keep_years), collapse = ","))
  for (outcome in outcomes) {
    message("Fitting ", s, ": ", outcome)
    fit <- glm(reformulate(c("income_quintile", "z_math", "z_verbal", "exam_year"), response = outcome),
      family = binomial("logit"), data = x, control = glm.control(epsilon = 1e-9, maxit = 60L))
    X <- model.matrix(fit); y <- fit$y; b <- coef(fit); p <- fitted(fit); N <- nrow(X); K <- ncol(X)
    stopifnot(fit$converged, fit$rank == K, N == nrow(x), all(is.finite(b)),
      max(abs(crossprod(X, y - p))) / N < 1e-8)
    H <- crossprod(X, X * as.numeric(p * (1 - p)))
    bread <- solve(H)
    V <- N / (N - K) * bread %*% crossprod(X * as.numeric(y - p)) %*% bread
    se <- sqrt(diag(V))
    stopifnot(all(is.finite(V)), all(se > 0))
    key <- paste(s, outcome, sep = "__")
    coefs[[key]] <- data.table(sample = s, outcome = outcome, term = names(b), estimate = as.numeric(b), std_error_hc1 = se)
    covariances[[key]] <- V
    # Save compact fitted-model information; the common data permit full refits.
    fitted_models[[key]] <- list(coefficients = b, formula = deparse(formula(fit)), converged = fit$converged,
      iterations = fit$iter, log_likelihood = as.numeric(logLik(fit)), N = N)
    X0 <- X; group_cols <- grep("^income_quintile", colnames(X0))
    X0[, group_cols] <- 0
    eta0 <- as.vector(X0 %*% b); p0 <- plogis(eta0); w0 <- p0 * (1 - p0)
    for (q in 1:5) {
      if (q == 1L) { delta <- 0; delta_se <- NA_real_; pq <- p0 } else {
        term <- paste0("income_quintile", q)
        pq <- plogis(eta0 + b[term]); wq <- pq * (1 - pq)
        delta <- mean(pq - p0)
        gradient <- colMeans(X0 * as.numeric(wq - w0))
        gradient[term] <- gradient[term] + mean(wq)
        delta_se <- sqrt(as.numeric(t(gradient) %*% V %*% gradient))
        stopifnot(is.finite(delta_se), delta_se > 0,
          abs((mean(plogis(eta0 + b[term] + 1e-6)) - mean(plogis(eta0 + b[term] - 1e-6))) / 2e-6 - mean(wq)) < 1e-7)
      }
      effects[[paste(key, q, sep = "__")]] <- data.table(sample = s, outcome = outcome, income_quintile = q,
        adjusted_probability = mean(pq), difference_vs_q1 = delta, std_error_hc1 = delta_se,
        lower95 = delta - qnorm(.975) * delta_se, upper95 = delta + qnorm(.975) * delta_se)
    }
    summaries[[key]] <- data.table(sample = s, outcome = outcome, N = N, mean_outcome = mean(y),
      log_likelihood = as.numeric(logLik(fit)), iterations = fit$iter)
  }
}
effects <- rbindlist(effects); summaries <- rbindlist(summaries)
stopifnot(all(summaries[, uniqueN(N) == 1L, by = sample]$V1), unname(tools::md5sum(cache)) == original_hash)
fwrite(effects, file.path(tables, "high_premium_income_quintiles.csv"))
fwrite(rbindlist(coefs), file.path(clean, "coefficients.csv")); fwrite(summaries, file.path(clean, "model_summary.csv"))
fwrite(rbindlist(sample_flow), file.path(clean, "sample_flow.csv"))
fwrite(data.table(N_cached = N_before, N_common_outcomes = nrow(d)), file.path(clean, "outcome_coverage.csv"))
saveRDS(d, file.path(clean, "analysis_data.rds"))
saveRDS(fitted_models, file.path(clean, "models.rds")); saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
fwrite(data.table(check = c("Unique outcome join", "Binary outcomes and non-enrollee zeros", "Convergence and logit score equations", "HC1 covariance", "Finite-difference derivative check", "Common samples across outcomes", "Original quintile cache unchanged"), passed = TRUE), file.path(clean, "verification.csv"))
cell <- function(q, s, o) {
  z <- effects[income_quintile == q & sample == s & outcome == o]
  if (q == 1L) return("Reference")
  sprintf('%+.2f<span>(%.2f)</span>', 100 * z$difference_vs_q1, 100 * z$std_error_hc1)
}
rows <- vapply(1:5, function(q) paste0('<tr><th>Q', q, '</th>', paste(vapply(c("all_students", "enrolled_only"), function(s)
  paste0('<td>', cell(q, s, outcomes[1]), '</td><td>', cell(q, s, outcomes[2]), '</td>'), character(1)), collapse = ""), '</tr>'), character(1))
N_notes <- paste(vapply(c("all_students", "enrolled_only"), function(s) paste0(if (s == "all_students") "All scored students: " else "HE enrollees: ",
  format(summaries[sample == s, N][1L], big.mark = ",", trim = TRUE)), character(1)), collapse = "; ")
flow <- rbindlist(sample_flow)
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>High-premium outcomes by parental-income quintile</title><style>body{max-width:1050px;margin:38px auto;padding:0 22px;font:16px/1.55 system-ui;color:#243143}h1{font-size:26px}table{border-collapse:collapse;width:100%;margin:22px 0}th,td{padding:12px;border-bottom:1px solid #dbe0e6;text-align:right}th:first-child{text-align:left}thead{background:#f3f6f9}span,.note{color:#526171;font-size:14px}td span{display:block}</style></head><body>',
  '<h1>High-premium enrollment by parental-income quintile</h1><p>Logistic models controlling for math and verbal entrance-exam scores and admission-year fixed effects. Entries are adjusted probability differences relative to Q1, in percentage points.</p>',
  '<table><thead><tr><th></th><th colspan="2">All scored students</th><th colspan="2">HE enrollees only</th></tr><tr><th>Parental-income quintile</th><th>Premium field</th><th>Premium institution</th><th>Premium field</th><th>Premium institution</th></tr></thead><tbody>', rows,
  '</tbody></table>', paste0('<p class="note">', N_notes, '.</p>'),
  '<p class="note">For each model, predict every student under each quintile while holding their observed scores and admission year fixed, then average the probability difference from Q1. Parentheses are delta-method HC1 robust SEs. These are discrete quintile contrasts, not log-odds coefficients.</p>',
  '<p class="note">Quintiles combine adjacent existing imputed baseline SIMCE parental-income deciles. Outcome flags are the unchanged master variables high_paying_field_m1 and high_inst_m1 at first observed HE enrollment. Non-enrollees retain zero in the all-student models; no monetary income imputation is used for these binary outcomes. Both scores, baseline income quintile and both outcomes must be observed.</p>',
  paste0('<p class="note">Admission-year cells with no variation in either outcome are omitted from both outcome models within each sample, because fixed-effect logits supply no finite identifying information there. Exclusions: ',
    paste0(flow$sample, ': ', flow$N_before_year_support - flow$N_estimation, ' students (years ', flow$omitted_years, ')', collapse = '; '), '.</p>'),
  '<p><a href="../../tables/high_premium_income_quintiles/high_premium_income_quintiles.csv">Full table and confidence intervals (CSV)</a></p></body></html>')
writeLines(html, file.path(reports, "high_premium_income_quintiles.html"), useBytes = TRUE)
print(rbindlist(sample_flow)); print(effects[, .(sample, outcome, income_quintile, difference_pp = 100 * difference_vs_q1, se_pp = 100 * std_error_hc1)])
