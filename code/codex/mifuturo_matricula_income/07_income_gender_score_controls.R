# Add entrance-exam math/verbal controls to the two income gender comparisons.
# Run after 06_income_by_gender.R; preserve its income sample and raw results.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
base_dir <- file.path(repo_wd, "data/clean/projected_income_gender")
clean_dir <- file.path(base_dir, "score_controls")
table_dir <- file.path(repo_wd, "output/tables/projected_income_gender")
report_dir <- file.path(repo_wd, "output/reports/projected_income_gender")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
master_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
source_paths <- file.path(base_dir, c("analysis_data.rds", "means.csv", "gender_gaps.csv"))
stopifnot(all(file.exists(c(master_path, source_paths))))
preservation <- data.table(path = source_paths, md5_before = unname(tools::md5sum(source_paths)))
d <- as.data.table(readRDS(source_paths[1L]))
raw <- fread(file.path(base_dir, "gender_gaps.csv"))
checks <- data.table(check = character(), passed = logical())
check <- function(label, passed) {
  checks <<- rbind(checks, data.table(check = label, passed = isTRUE(passed)))
  if (!isTRUE(passed)) stop(label)
  message("PASS: ", label)
}
check("The original two income samples remain available", nrow(d) == 931546L && sum(d$enrolled) == 625616L && !anyDuplicated(d$mrun))
keep <- c("mrun", "psu_year", "math_max", "leng_max")
check("The master contains both entrance-exam score controls", all(keep %in% names(fread(master_path, nrows = 0L))))
fwrite(data.table(path = master_path, bytes = file.info(master_path)$size, selected_columns = length(keep),
  strategy = "Read only student ID, admission year, math and verbal scores; retain cached broad-panel IDs"),
  file.path(clean_dir, "source_manifest.csv"))
message("Reading four score-link columns from the paper master...")
s <- fread(master_path, select = keep, na.strings = c("", "NA"))
s <- s[mrun %in% d$mrun]
check("Each cached student has one master score record", nrow(s) == nrow(d) && !anyDuplicated(s$mrun))
d <- merge(d, s, by = "mrun", all.x = TRUE, sort = FALSE)
d[, scores_observed := is.finite(math_max) & math_max > 0 & is.finite(leng_max) & leng_max > 0 & is.finite(psu_year)]
coverage <- rbindlist(list(
  d[, .(sample = "with_imputation", N_original = .N, N_with_scores = sum(scores_observed))],
  d[enrolled == TRUE, .(sample = "enrolled_only", N_original = .N, N_with_scores = sum(scores_observed))]
))
coverage[, `:=`(N_without_both_scores = N_original - N_with_scores, score_coverage = N_with_scores / N_original)]
fwrite(coverage, file.path(clean_dir, "score_coverage.csv"))
fwrite(d[, .(N = .N, scores_observed = sum(scores_observed)), by = .(gender, enrolled)],
  file.path(clean_dir, "score_coverage_by_gender_enrollment.csv"))
d <- d[scores_observed == TRUE]
normalization <- d[, .(N = .N, math_mean = mean(math_max), math_sd = sd(math_max),
  verbal_mean = mean(leng_max), verbal_sd = sd(leng_max)), by = psu_year]
check("Every admission-year score normalization has positive SD", all(normalization$math_sd > 0 & normalization$verbal_sd > 0))
d[, z_math := (math_max - mean(math_max)) / sd(math_max), by = psu_year]
d[, z_verbal := (leng_max - mean(leng_max)) / sd(leng_max), by = psu_year]
check("Both score controls are standardized within admission year", all(d[, abs(mean(z_math)) < 1e-10 && abs(sd(z_math) - 1) < 1e-10 &&
  abs(mean(z_verbal)) < 1e-10 && abs(sd(z_verbal) - 1) < 1e-10, by = psu_year]$V1))
fwrite(normalization, file.path(clean_dir, "score_normalization.csv"))
samples <- list(with_imputation = d, enrolled_only = d[enrolled == TRUE])
specifications <- c(unadjusted_same_sample = "log_program_income_full_clp_m1 ~ female",
  score_adjusted = "log_program_income_full_clp_m1 ~ female + z_math + z_verbal + factor(psu_year)")
models <- list(); covariances <- list(); coefs <- list(); summaries <- list()
for (sample_name in names(samples)) for (specification in names(specifications)) {
  x <- samples[[sample_name]]
  fit <- lm(as.formula(specifications[[specification]]), data = x, x = TRUE, y = TRUE, model = TRUE)
  X <- fit$x; N <- nrow(X); K <- ncol(X); b <- coef(fit)
  check(paste(sample_name, specification, "uses the full common scored sample"), N == nrow(x) && fit$rank == K && all(is.finite(b)))
  bread <- solve(crossprod(X))
  V <- N / (N - K) * bread %*% crossprod(X * as.numeric(residuals(fit))) %*% bread
  se <- sqrt(diag(V))
  check(paste(sample_name, specification, "has finite regression-derived HC1 uncertainty"), all(is.finite(V)) && all(se > 0))
  key <- paste(sample_name, specification, sep = "__")
  models[[key]] <- fit; covariances[[key]] <- V
  coefs[[key]] <- data.table(sample = sample_name, specification = specification, term = names(b), estimate = as.numeric(b),
    std_error_hc1 = se, p_value = 2 * pnorm(-abs(b / se)), lower95 = b - qnorm(.975) * se, upper95 = b + qnorm(.975) * se)
  gap <- unname(b["female"]); gap_se <- se["female"]
  summaries[[key]] <- data.table(sample = sample_name, specification = specification, N = N,
    N_men = sum(x$female == 0L), N_women = sum(x$female == 1L), female_coefficient = gap,
    std_error_hc1 = gap_se, lower95_log = gap - qnorm(.975) * gap_se, upper95_log = gap + qnorm(.975) * gap_se,
    implied_relative_gap_pct = 100 * expm1(gap), r_squared = summary(fit)$r.squared)
}
coefs <- rbindlist(coefs); summaries <- rbindlist(summaries)
check("Each pair of regressions uses exactly the same scored students", all(summaries[, uniqueN(N) == 1L, by = sample]$V1))
saveRDS(d, file.path(clean_dir, "analysis_data.rds"))
saveRDS(models, file.path(clean_dir, "models.rds"))
saveRDS(covariances, file.path(clean_dir, "covariance_matrices.rds"))
fwrite(coefs, file.path(clean_dir, "coefficients.csv"))
fwrite(summaries, file.path(clean_dir, "model_summary.csv"))
preservation[, md5_after := unname(tools::md5sum(path))]
check("Original income cache and raw gender comparisons are unchanged", all(preservation$md5_before == preservation$md5_after))
fwrite(preservation, file.path(clean_dir, "source_preservation.csv"))
fwrite(checks, file.path(clean_dir, "verification.csv"))

tab <- merge(dcast(summaries, sample + N ~ specification, value.var = c("female_coefficient", "std_error_hc1")),
  raw[, .(sample, original_raw_gap = gap_women_minus_men_log)], by = "sample")
tab <- merge(tab, summaries[specification == "score_adjusted", .(sample, adjusted_pct = implied_relative_gap_pct)], by = "sample")
tab <- tab[match(c("with_imputation", "enrolled_only"), sample)]
fwrite(tab, file.path(table_dir, "projected_income_by_gender_score_controls.csv"))
labels <- c(with_imputation = "Including non-enrollee income assignment", enrolled_only = "HE enrollees only")
rows <- vapply(seq_len(nrow(tab)), function(i) {
  x <- tab[i]
  sprintf('<tr><th>%s</th><td>%.4f</td><td>%.4f</td><td>%.4f<span>(%.4f)</span></td><td>%.2f%%</td><td>%s</td></tr>',
    labels[x$sample], x$original_raw_gap, x$female_coefficient_unadjusted_same_sample,
    x$female_coefficient_score_adjusted, x$std_error_hc1_score_adjusted, x$adjusted_pct, format(x$N, big.mark = ",", trim = TRUE))
}, character(1))
coverage_rows <- vapply(seq_len(nrow(coverage)), function(i) sprintf('<li>%s: %s of %s students have both scores (%.1f%%).</li>',
  labels[coverage$sample[i]], format(coverage$N_with_scores[i], big.mark = ",", trim = TRUE),
  format(coverage$N_original[i], big.mark = ",", trim = TRUE), 100 * coverage$score_coverage[i]), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Projected income by gender: score controls</title><style>body{max-width:1100px;margin:38px auto;padding:0 22px;font:16px/1.55 system-ui;color:#243143}h1{font-size:27px}table{border-collapse:collapse;width:100%;margin:24px 0}th,td{padding:12px;border-bottom:1px solid #dbe0e6;text-align:right}th:first-child{text-align:left}thead th{background:#f3f6f9}.note,td span{font-size:14px;color:#526171}td span{display:block}.wrap{overflow:auto}</style></head><body>',
  '<h1>Projected income by gender, controlling for scores</h1><p>Women-minus-men coefficients in natural-log projected monthly income. The score-adjusted OLS regression controls linearly for entrance-exam math and verbal scores and includes admission-year fixed effects.</p>',
  '<div class="wrap"><table><thead><tr><th>Sample</th><th>Original raw gap</th><th>Raw gap: scored sample</th><th>Score-adjusted gap</th><th>Adjusted relative gap</th><th>Scored students</th></tr></thead><tbody>',
  rows, '</tbody></table></div>',
  '<p class="note">The raw scored-sample and adjusted regressions use exactly the same students. Parentheses show HC1 heteroskedasticity-robust SEs. Adjusted percentage gaps are 100 &times; [exp(female coefficient) &minus; 1].</p>',
  '<ul>', coverage_rows, '</ul>',
  '<p class="note">The original broad panel covers grade-8 cohorts 2017&ndash;2020, ages 12&ndash;16 with a valid attended high school. The unchanged income sample has 931,546 students with known gender, including 625,616 HE enrollees. Adding scores requires both positive observed entrance-exam components and an observed admission year; missing tests are not assigned zero scores. Scores are standardized within admission year before regression.</p>',
  '<p class="note">Income is the existing master outcome at first observed HE enrollment. Non-enrollees retain CLP 553,553; enrollees retain the same hierarchical MiFuturo projections. These are conditional program-income associations, rather than observed individual wages or causal gender effects.</p>',
  '<p><a href="../../tables/projected_income_gender/projected_income_by_gender_score_controls.csv">Comparison table (CSV)</a></p></body></html>')
writeLines(html, file.path(report_dir, "projected_income_by_gender.html"), useBytes = TRUE)
message("Finished: ", nrow(checks), " checks passed.")
print(coverage); print(summaries)
