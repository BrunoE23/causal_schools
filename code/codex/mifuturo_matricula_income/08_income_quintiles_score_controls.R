# Compare projected income across baseline parental-income quintiles.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
root <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", "C:/Users/brunem/Research/causal_schools")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
cache <- file.path(root, "data/clean/projected_income_gender/score_controls/analysis_data.rds")
master <- file.path(data_root, "data/clean/univ_gr8_df.csv")
clean <- file.path(root, "data/clean/projected_income_quintiles")
tables <- file.path(root, "output/tables/projected_income_quintiles")
reports <- file.path(root, "output/reports/projected_income_quintiles")
for (p in c(clean, tables, reports)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
cache_hash <- unname(tools::md5sum(cache))
d <- as.data.table(readRDS(cache))
# Reuse the paper's existing deciles; do not redefine quintiles in this sample.
fwrite(data.table(path = master, bytes = file.info(master)$size, selected_columns = 2L,
  strategy = "Read only student ID and existing imputed baseline income decile"), file.path(clean, "source_manifest.csv"))
message("Linking the existing parental-income deciles...")
income <- fread(master, select = c("mrun", "income_decile_imputed"), na.strings = c("", "NA"))
income <- income[mrun %in% d$mrun]
stopifnot(nrow(income) == nrow(d), !anyDuplicated(income$mrun))
d <- merge(d, income, by = "mrun", all.x = TRUE, sort = FALSE)
d[, quintile_observed := income_decile_imputed %in% 1:10]
coverage <- rbind(d[, .(sample = "with_imputation", N_scored = .N, N_income_quintile = sum(quintile_observed))],
  d[enrolled == TRUE, .(sample = "enrolled_only", N_scored = .N, N_income_quintile = sum(quintile_observed))])
fwrite(coverage, file.path(clean, "coverage.csv"))
d <- d[quintile_observed == TRUE]
d[, income_quintile := factor(ceiling(income_decile_imputed / 2), levels = 1:5)]
stopifnot(!anyNA(d$income_quintile), !anyDuplicated(d$mrun))
samples <- list(with_imputation = d, enrolled_only = d[enrolled == TRUE])
specs <- c(raw = "log_program_income_full_clp_m1 ~ income_quintile",
  score_adjusted = "log_program_income_full_clp_m1 ~ income_quintile + z_math + z_verbal + exam_year")
models <- list(); covariances <- list(); coefs <- list(); summary_rows <- list(); groups <- list()
for (sample_name in names(samples)) {
  x <- samples[[sample_name]]
  # A common year as reference avoids cancellation from a very sparse earliest year.
  x[, exam_year := relevel(factor(psu_year), ref = names(which.max(table(psu_year))))]
  groups[[sample_name]] <- x[, .(N = .N, mean_log_income = mean(log_program_income_full_clp_m1)),
    by = income_quintile][, sample := sample_name]
  for (specification in names(specs)) {
    fit <- lm(as.formula(specs[[specification]]), data = x, x = TRUE, y = TRUE)
    X <- fit$x; b <- coef(fit); N <- nrow(X); K <- ncol(X)
    stopifnot(nobs(fit) == nrow(x), fit$rank == K, all(is.finite(b)))
    bread <- solve(crossprod(X))
    V <- N / (N - K) * bread %*% crossprod(X * as.numeric(residuals(fit))) %*% bread
    se <- sqrt(diag(V))
    stopifnot(all(is.finite(se) & se > 0))
    # Verify the OLS moment equations without squaring the design's condition number.
    stopifnot(max(abs(crossprod(X, residuals(fit)))) / N < 1e-9)
    key <- paste(sample_name, specification, sep = "__")
    models[[key]] <- fit; covariances[[key]] <- V
    coefs[[key]] <- data.table(sample = sample_name, specification = specification, term = names(b),
      estimate = as.numeric(b), std_error_hc1 = se, lower95 = b - qnorm(.975) * se,
      upper95 = b + qnorm(.975) * se, p_value = 2 * pnorm(-abs(b / se)))
    summary_rows[[key]] <- data.table(sample = sample_name, specification = specification, N = N,
      r_squared = summary(fit)$r.squared)
  }
}
coefs <- rbindlist(coefs); counts <- rbindlist(groups); model_summary <- rbindlist(summary_rows)
stopifnot(all(model_summary[, uniqueN(N) == 1L, by = sample]$V1), unname(tools::md5sum(cache)) == cache_hash)
saveRDS(d, file.path(clean, "analysis_data.rds"))
saveRDS(models, file.path(clean, "models.rds")); saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
fwrite(coefs, file.path(clean, "coefficients.csv")); fwrite(counts, file.path(clean, "quintile_means.csv"))
fwrite(model_summary, file.path(clean, "model_summary.csv"))
result <- coefs[specification == "score_adjusted" & term %in% paste0("income_quintile", 2:5)]
result[, `:=`(income_quintile = as.integer(sub("income_quintile", "", term)), relative_to_q1_pct = 100 * expm1(estimate))]
result <- rbind(result, data.table(sample = names(samples), specification = "score_adjusted", term = "income_quintile1",
  estimate = 0, std_error_hc1 = NA_real_, lower95 = NA_real_, upper95 = NA_real_, p_value = NA_real_,
  income_quintile = 1L, relative_to_q1_pct = 0), fill = TRUE)
result <- merge(result, counts[, .(sample, income_quintile = as.integer(as.character(income_quintile)), N)],
  by = c("sample", "income_quintile"))
setorder(result, income_quintile, sample)
fwrite(result, file.path(tables, "projected_income_quintiles_score_controls.csv"))
labels <- c(with_imputation = "Including non-enrollee imputation", enrolled_only = "HE enrollees only")
cell <- function(q, s) {
  z <- result[income_quintile == q & sample == s]
  if (q == 1L) return("Reference")
  sprintf('%.3f <span>(%.3f); %+.1f%%</span>', z$estimate, z$std_error_hc1, z$relative_to_q1_pct)
}
rows <- vapply(1:5, function(q) paste0('<tr><th>Q', q, if (q == 1L) ' (poorest)' else if (q == 5L) ' (richest)' else '',
  '</th><td>', cell(q, "with_imputation"), '</td><td>', cell(q, "enrolled_only"), '</td></tr>'), character(1))
sample_note <- paste(vapply(names(samples), function(s) paste0(labels[s], ': ', format(nrow(samples[[s]]), big.mark = ",", trim = TRUE)), character(1)), collapse = "; ")
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Projected income by parental-income quintile</title><style>body{max-width:950px;margin:38px auto;padding:0 22px;font:16px/1.55 system-ui;color:#243143}h1{font-size:26px}table{border-collapse:collapse;width:100%;margin:22px 0}th,td{padding:12px;border-bottom:1px solid #dbe0e6;text-align:right}th:first-child{text-align:left}thead{background:#f3f6f9}span,.note{color:#526171;font-size:14px}td span{display:block}</style></head><body>',
  '<h1>Projected income by parental-income quintile</h1><p>OLS differences in log projected monthly income relative to Q1, controlling linearly for entrance-exam math and verbal scores and admission-year fixed effects.</p>',
  '<table><thead><tr><th>Parental-income quintile</th><th>Including non-enrollee imputation</th><th>HE enrollees only</th></tr></thead><tbody>', rows,
  '</tbody></table>', paste0('<p class="note">', sample_note, '.</p>'),
  '<p class="note">Quintiles combine adjacent existing grade-4 SIMCE parental-income deciles (income_decile_imputed); they are not re-ranked in this selected sample. Both positive entrance-exam scores and an observed income decile are required. The broad panel covers grade-8 cohorts 2017&ndash;2020. Score units and projected-income values are unchanged from the gender comparison.</p>',
  '<p class="note">Parentheses are HC1 robust SEs; percentages are 100 &times; [exp(coefficient) &minus; 1]. Non-enrollees retain the existing CLP 553,553 assignment; enrolled students use the same MiFuturo program-income projections. These are conditional associations.</p>',
  '<p><a href="../../tables/projected_income_quintiles/projected_income_quintiles_score_controls.csv">Full table (CSV)</a></p></body></html>')
writeLines(html, file.path(reports, "projected_income_quintiles_score_controls.html"), useBytes = TRUE)
fwrite(data.table(check = c("Unique student join", "Valid existing decile-to-quintile mapping", "Full-rank models and complete samples", "OLS residual moment equations", "Common raw/adjusted samples", "Prior analysis cache unchanged"), passed = TRUE), file.path(clean, "verification.csv"))
print(coverage); print(result[, .(sample, income_quintile, estimate, std_error_hc1, relative_to_q1_pct, N)])
