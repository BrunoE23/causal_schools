# Add linear grade-4 SIMCE math and verbal to parents and entrance scores.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "parent_education_baseline_scores_he_enrollment"
clean <- file.path(repo, "data/clean", task)
report <- file.path(repo, "output/reports", task)
tables <- file.path(repo, "output/tables", task)
for (p in c(clean, report, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
input <- file.path(repo, "data/clean/parent_education_linear_scores_he_enrollment/analysis_data.rds")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
source <- file.path(data_root, "data/clean/univ_gr8_df.csv")
before <- file.info(source)
input_hash <- unname(tools::md5sum(input))
a <- as.data.table(readRDS(input))
stopifnot(nrow(a) == 569681L, !anyDuplicated(a$mrun))
cols <- c("mrun", "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to")
stopifnot(all(cols %in% names(fread(source, nrows = 0L))))
# Preflight: 1.23 GB source; read four columns only and immediately filter IDs.
message("Reading four linkage/baseline-score columns from the universe.")
d <- fread(source, select = cols, na.strings = c("", "NA"), showProgress = FALSE)
d <- d[mrun %in% a$mrun]
stopifnot(nrow(d) == nrow(a), !anyDuplicated(d$mrun))
d <- d[match(a$mrun, mrun)]
stopifnot(!anyNA(d$mrun), all(a$mrun == d$mrun), all(a$cohort_gr8 == d$cohort_gr8))
a[, `:=`(baseline_math = d$z_sim_mat_4to, baseline_verbal = d$z_sim_leng_4to)]
x <- a[is.finite(baseline_math) & is.finite(baseline_verbal)]
fwrite(data.table(N_prior = nrow(a), N_both_baseline = nrow(x), excluded_missing_baseline = nrow(a) - nrow(x),
  missing_baseline_math = sum(!is.finite(a$baseline_math)), missing_baseline_verbal = sum(!is.finite(a$baseline_verbal))),
  file.path(clean, "sample_flow.csv"))
forms <- list(without_baseline_same_sample = he_enrollment ~ father_years + mother_years + z_math + z_verbal,
  with_baseline = he_enrollment ~ father_years + mother_years + z_math + z_verbal + baseline_math + baseline_verbal)
models <- list(); covariances <- list(); coefficients <- list(); summaries <- list(); checks <- list()
for (model_name in names(forms)) {
  fml <- forms[[model_name]]
  fit <- feols(fml, data = x, vcov = ~most_time_RBD,
    ssc = ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min"), notes = FALSE)
  X <- model.matrix(fml, x); N <- nrow(X); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  stopifnot(nobs(fit) == N, identical(names(b), colnames(X)), qr(X)$rank == K,
    K == if (model_name == "with_baseline") 7L else 5L)
  bread <- chol2inv(chol(crossprod(X)))
  scores <- rowsum(X * as.numeric(e), x$most_time_RBD, reorder = FALSE)
  V_check <- (G / (G - 1)) * ((N - 1) / (N - K)) * bread %*% crossprod(scores) %*% bread
  normal_error <- max(abs(crossprod(X, e))) / N
  cov_error <- max(abs(V - V_check))
  stopifnot(normal_error < 1e-8, cov_error < 1e-9)
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(model = model_name, df = G - 1L, lower95 = estimate - qt(.975, G - 1L) * std_error,
    upper95 = estimate + qt(.975, G - 1L) * std_error)]
  for (v in c("estimate", "std_error", "lower95", "upper95")) ct[, (paste0(v, "_pp")) := 100 * get(v)]
  coefficients[[model_name]] <- ct
  summaries[[model_name]] <- data.table(model = model_name, N = N, n_schools = G, K = K,
    enrollment_rate = mean(x$he_enrollment), r2 = 1 - sum(e^2) / sum((x$he_enrollment - mean(x$he_enrollment))^2),
    n_admission_2026 = sum(x$psu_year == 2026L), predictions_below_zero = mean(fitted(fit) < 0),
    predictions_above_one = mean(fitted(fit) > 1))
  checks[[model_name]] <- data.table(model = model_name, normal_error, cov_error, passed = TRUE)
  models[[model_name]] <- fit; covariances[[model_name]] <- V
}
stats <- rbindlist(summaries); ct <- rbindlist(coefficients)
stopifnot(stats[model == "with_baseline", r2] >= stats[model == "without_baseline_same_sample", r2],
  identical(unname(tools::md5sum(input)), input_hash),
  file.info(source)$size == before$size, file.info(source)$mtime == before$mtime)
fwrite(ct, file.path(clean, "coefficients.csv")); fwrite(stats, file.path(clean, "model_summary.csv"))
fwrite(rbindlist(checks), file.path(clean, "verification.csv"))
fwrite(data.table(path = c(input, source), md5 = c(input_hash, NA_character_),
  bytes = file.info(c(input, source))$size, modified = as.character(file.info(c(input, source))$mtime)),
  file.path(clean, "source_manifest.csv"))
saveRDS(x, file.path(clean, "analysis_data.rds")); saveRDS(models, file.path(clean, "models.rds"))
saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
saveRDS(list(formulas = forms, score_scale = "Existing admission-year entrance normalization and SIMCE-year baseline normalization",
  sample_restriction = "Both parent measures, both entrance scores, and both grade-4 scores available; no high-math threshold",
  cluster = "most_time_RBD", other_controls = "None"), file.path(clean, "specification.rds"))
labels <- c(father_years = "Father education (+1 year)", mother_years = "Mother education (+1 year)",
  z_math = "PAES math (+1 SD)", z_verbal = "PAES verbal (+1 SD)",
  baseline_math = "Baseline SIMCE math (+1 SD)", baseline_verbal = "Baseline SIMCE verbal (+1 SD)")
rows <- vapply(names(labels), function(term_name) {
  cells <- vapply(names(forms), function(m) {
    z <- ct[model == m & term == term_name]
    if (!nrow(z)) return('<td>--</td>')
    sprintf('<td>%.3f (%.3f)</td>', z$estimate_pp, z$std_error_pp)
  }, character(1))
  paste0('<tr><th>', labels[term_name], '</th>', paste(cells, collapse = ''), '</tr>')
}, character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Parent education, entrance scores, and baseline achievement</title>',
  '<style>body{font:16px/1.5 system-ui;max-width:1000px;margin:30px auto;padding:0 24px;color:#233143}table{border-collapse:collapse;width:100%;margin:24px 0}th,td{padding:12px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}.note{font-size:14px;color:#526171}</style></head><body>',
  '<h1>HE enrollment with baseline math and verbal scores</h1><p>OLS linear probability models. Both parents, both entrance scores, and both baseline scores enter linearly; no other controls. Coefficients in percentage points, school-clustered SEs in parentheses.</p>',
  '<table><tr><th></th><th>Without baseline, same sample</th><th>Add baseline math and verbal</th></tr>', rows,
  paste0('<tr><th>R-squared</th><td>', paste(sprintf('%.4f', stats$r2), collapse = '</td><td>'), '</td></tr></table>'),
  sprintf('<p>Both columns: N = %s; school clusters = %s; enrollment rate = %.2f%%.</p>',
    format(nrow(x), big.mark = ',', trim = TRUE), format(uniqueN(x$most_time_RBD), big.mark = ',', trim = TRUE), 100 * mean(x$he_enrollment)),
  sprintf('<p class="note">Start with the previous 569,681 students with both parent measures and both entrance scores; exclude %s without both baseline scores. No high-math restriction, income controls, region controls, year effects, or nonlinear score terms. Both models use precisely the same students.</p>', format(nrow(a) - nrow(x), big.mark = ',', trim = TRUE)),
  '<p class="note">Math and verbal retain the existing PSU/PAES within-admission-year standardization from the broad panel; no restandardization occurs here. Baseline math and verbal use the existing grade-4 SIMCE-year standardization, with no imputation or restandardization. Parent years retain the existing SIMCE approximate-year coding and imputations. Inference uses attended-school CR1 covariance and t(G-1). Coefficients are conditional associations, not causal effects.</p></body></html>')
writeLines(html, file.path(report, "regression_table.html"), useBytes = TRUE)
tex_rows <- vapply(names(labels), function(term_name) {
  cells <- vapply(names(forms), function(m) {
    z <- ct[model == m & term == term_name]
    if (!nrow(z)) return('--')
    sprintf('%.3f (%.3f)', z$estimate_pp, z$std_error_pp)
  }, character(1))
  paste0(labels[term_name], ' & ', paste(cells, collapse = ' & '), ' \\\\')
}, character(1))
writeLines(c('\\begin{tabular}{lrr}', '\\hline', ' & Without baseline, same sample & Add baseline scores \\\\', '\\hline',
  tex_rows, paste0('$R^2$ & ', paste(sprintf('%.4f', stats$r2), collapse = ' & '), ' \\\\'),
  paste0('Students & ', paste(format(stats$N, big.mark = ',', trim = TRUE), collapse = ' & '), ' \\\\'),
  '\\hline', '\\end{tabular}', '% HE enrollment OLS, linear terms only, school-clustered SEs; coefficients in pp.'),
  file.path(tables, "regression_table.tex"))
print(ct[term != "(Intercept)", .(model, term, estimate_pp, std_error_pp, p_value)])
print(stats)
