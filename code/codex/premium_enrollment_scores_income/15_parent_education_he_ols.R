# Joint parent-education coefficients in the prior high-math HE-enrollment LPM.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "parent_education_he_enrollment"
clean <- file.path(repo, "data/clean", task)
report <- file.path(repo, "output/reports", task)
tables <- file.path(repo, "output/tables", task)
for (p in c(report, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
input <- file.path(clean, "analysis_data.rds")
hash <- unname(tools::md5sum(input))
a <- as.data.table(readRDS(input))
stopifnot(nrow(a) == 93423L, !anyDuplicated(a$mrun), all(a$z_math >= 1),
  all(a$region_origin_code %in% 1:16), !anyNA(a$enrolled), all(a$enrolled %in% c(FALSE, TRUE)))
a[, `:=`(he_enrollment = as.integer(enrolled),
  math2 = z_math^2, verbal2 = z_verbal^2, math3 = z_math^3, verbal3 = z_verbal^3,
  math_verbal = z_math * z_verbal, math2_verbal = z_math^2 * z_verbal,
  math_verbal2 = z_math * z_verbal^2)]
parents <- c("father_years", "mother_years")
controls <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3",
  "math2_verbal", "math_verbal2", "income_decile", "exam_year", "region_origin")
rhs <- c(parents, controls)
specs <- data.table(model = c("reported_and_imputed", "both_reported", "exclude_admission_2026"),
  label = c("Reported + existing imputations", "Both parents reported", "Exclude admission 2026"))
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
models <- list(); baselines <- list(); covariance <- list(); ids <- list()
coefficients <- list(); summaries <- list(); audits <- list(); joint_tests <- list(); equality_tests <- list()
for (i in seq_len(nrow(specs))) {
  spec <- specs[i]
  x <- if (spec$model == "both_reported") copy(a[both_reported == TRUE]) else copy(a[both_available == TRUE])
  if (spec$model == "exclude_admission_2026") x <- x[psu_year != 2026L]
  suffix <- if (spec$model == "both_reported") "_educ_years" else "_educ_years_imputed"
  x[, `:=`(father_years = get(paste0("father", suffix)), mother_years = get(paste0("mother", suffix)),
    income_decile = relevel(factor(income_decile_imputed, levels = 1:10), ref = "5"),
    exam_year = factor(psu_year), region_origin = factor(region_origin_code, levels = 1:16))]
  stopifnot(all(is.finite(x$father_years)), all(is.finite(x$mother_years)),
    all(x$income_decile_imputed %in% 1:10), all(is.finite(x$z_verbal)))
  X <- model.matrix(reformulate(rhs), x)
  N <- nrow(X); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
  stopifnot(N == nrow(x), qr(X)$rank == K, G > K)
  fit <- feols(reformulate(rhs, response = "he_enrollment"), data = x,
    vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  fit0 <- feols(reformulate(controls, response = "he_enrollment"), data = x,
    vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  stopifnot(nobs(fit) == N, nobs(fit0) == N, identical(names(b), colnames(X)))
  # Independent CR1 sandwich uses the actual regression's cluster scores.
  bread <- chol2inv(chol(crossprod(X)))
  scores <- rowsum(X * as.numeric(e), x$most_time_RBD, reorder = FALSE)
  correction <- (G / (G - 1)) * ((N - 1) / (N - K))
  V_check <- correction * bread %*% crossprod(scores) %*% bread
  normal_error <- max(abs(crossprod(X, e))) / N
  covariance_error <- max(abs(V - V_check))
  stopifnot(normal_error < 1e-8, covariance_error < 1e-9)
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(model = spec$model, df = G - 1L,
    lower95 = estimate - qt(.975, G - 1L) * std_error,
    upper95 = estimate + qt(.975, G - 1L) * std_error)]
  for (v in c("estimate", "std_error", "lower95", "upper95")) ct[, (paste0(v, "_pp")) := 100 * get(v)]
  coefficients[[i]] <- ct
  sst <- sum((x$he_enrollment - mean(x$he_enrollment))^2)
  r2 <- 1 - sum(e^2) / sst
  r20 <- 1 - sum(resid(fit0)^2) / sst
  delta <- r2 - r20
  projection_error <- abs(delta - sum((fitted(fit) - fitted(fit0))^2) / sst)
  stopifnot(delta >= -1e-12, projection_error < 1e-10)
  F_parent <- as.numeric(crossprod(b[parents], solve(V[parents, parents], b[parents]))) / 2
  joint_tests[[i]] <- data.table(model = spec$model, F = F_parent, df1 = 2L, df2 = G - 1L,
    p_value = pf(F_parent, 2L, G - 1L, lower.tail = FALSE))
  difference <- b["mother_years"] - b["father_years"]
  difference_se <- sqrt(V["mother_years", "mother_years"] + V["father_years", "father_years"] - 2 * V["mother_years", "father_years"])
  equality_tests[[i]] <- data.table(model = spec$model, contrast = "mother minus father",
    estimate_pp = 100 * difference, std_error_pp = 100 * difference_se,
    p_value = 2 * pt(-abs(difference / difference_se), G - 1L))
  summaries[[i]] <- data.table(model = spec$model, label = spec$label, N = N, n_schools = G, K = K,
    n_excluded_parent_missing = nrow(a) - sum(a$both_available), n_non_enrolled = sum(!x$enrolled),
    enrollment_rate = mean(x$he_enrollment), r2 = r2, r2_without_parents = r20,
    delta_r2_parents = delta, parent_share_explained = delta / r2,
    father_imputed = sum(x$father_educ_years_was_imputed), mother_imputed = sum(x$mother_educ_years_was_imputed),
    mean_father_years = mean(x$father_years), mean_mother_years = mean(x$mother_years),
    parent_correlation = cor(x$father_years, x$mother_years), n_admission_2026 = sum(x$psu_year == 2026L),
    n_enrolled_admission_2026 = sum(x$psu_year == 2026L & x$enrolled),
    predictions_below_zero = mean(fitted(fit) < 0), predictions_above_one = mean(fitted(fit) > 1))
  audits[[i]] <- data.table(model = spec$model, max_normal_equation_error = normal_error,
    max_covariance_error = covariance_error, nested_projection_error = projection_error, passed = TRUE)
  models[[spec$model]] <- fit; baselines[[spec$model]] <- fit0
  covariance[[spec$model]] <- V; ids[[spec$model]] <- x$mrun
}
coefs <- rbindlist(coefficients); stats <- rbindlist(summaries)
fwrite(coefs, file.path(clean, "coefficients.csv"))
fwrite(coefs[term %in% parents], file.path(clean, "parent_coefficients.csv"))
fwrite(stats, file.path(clean, "model_summary.csv"))
fwrite(rbindlist(audits), file.path(clean, "verification.csv"))
fwrite(rbindlist(joint_tests), file.path(clean, "parent_joint_tests.csv"))
fwrite(rbindlist(equality_tests), file.path(clean, "parent_equality_tests.csv"))
saveRDS(models, file.path(clean, "models.rds"))
saveRDS(baselines, file.path(clean, "models_without_parents.rds"))
saveRDS(covariance, file.path(clean, "covariance_matrices.rds"))
saveRDS(list(input = input, md5 = hash, sample_ids = ids, rhs = rhs,
  high_math = "original z_math >= 1", income_reference = 5L, cluster = "most_time_RBD",
  parent_units = "Approximate completed schooling years, SIMCE grade-4 parent questionnaire"),
  file.path(clean, "specification.rds"))
stopifnot(identical(unname(tools::md5sum(input)), hash))

format_n <- function(v) format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
rows <- character()
for (term_name in parents) {
  label <- if (term_name == "father_years") "Father education (+1 year)" else "Mother education (+1 year)"
  cells <- vapply(specs$model, function(m) {
    z <- coefs[model == m & term == term_name]
    sprintf('<td>%.3f<br><span class="note">(%.3f); 95%% CI [%.3f, %.3f]</span></td>',
      z$estimate_pp, z$std_error_pp, z$lower95_pp, z$upper95_pp)
  }, character(1))
  rows <- c(rows, paste0('<tr><th>', label, '</th>', paste(cells, collapse = ''), '</tr>'))
}
summary_rows <- c('N', 'n_schools', 'enrollment_rate', 'r2', 'r2_without_parents')
summary_labels <- c('Students', 'School clusters', 'Enrollment rate', 'R-squared', 'R-squared without parent education')
for (i in seq_along(summary_rows)) {
  v <- stats[[summary_rows[i]]]
  cells <- if (i <= 2L) format_n(v) else if (i == 3L) sprintf('%.2f%%', 100 * v) else sprintf('%.5f', v)
  rows <- c(rows, paste0('<tr><th>', summary_labels[i], '</th><td>', paste(cells, collapse = '</td><td>'), '</td></tr>'))
}
raw_rows <- vapply(seq_len(nrow(coefs)), function(i) {
  z <- coefs[i]
  sprintf('<tr><td>%s</td><td>%s</td><td>%.4f</td><td>%.4f</td><td>%.4g</td></tr>',
    z$model, z$term, z$estimate_pp, z$std_error_pp, z$p_value)
}, character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Parent education and HE enrollment</title>',
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1350px;margin:30px auto;padding:0 24px}h1{font-size:28px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:12px;border-bottom:1px solid #dce2e8;text-align:right}th:first-child,td:first-child{text-align:left}.note{font-size:13px;color:#526171}summary{cursor:pointer;font-weight:600}</style></head><body>',
  '<h1>Parent education and higher-education enrollment</h1><p>OLS linear probability models. Mother and father education enter jointly and linearly. Coefficients are <b>percentage points per additional schooling year</b>; school-clustered standard errors are in parentheses.</p>',
  paste0('<table><thead><tr><th></th><th>', paste(specs$label, collapse = '</th><th>'), '</th></tr></thead><tbody>'), rows, '</tbody></table>',
  '<p>All models condition on original standardized entrance-exam math at least one SD above its admission-year mean. Controls: the full bivariate cubic in math and verbal (nine terms), income-decile indicators (D5 omitted), admission-year indicators, and PAES-reported domicile-region indicators.</p>',
  '<p>Parent education comes from the grade-4 SIMCE parent questionnaire. Mother and father are separate variables. The existing category-to-years coding is retained: completed university = 16, master = 18, doctorate = 21; vocational and incomplete tertiary categories are approximate. The main model uses reported values plus existing baseline-context median imputations; the second column requires both reported. Remaining missing values are excluded, with coverage recorded separately.</p>',
  '<p class="note">The third column additionally excludes admission 2026, whose enrollment outcomes are all zero in this data vintage. The first two columns preserve those recorded zeros, as in the previous OLS analysis. Inference uses attended-school CR1 covariance, including all model parameters in its finite-sample correction, and t(G-1) confidence intervals. These are conditional associations, not identified causal effects. No new imputation, score normalization, or income ranking is performed.</p>',
  '<details><summary>All regression coefficients (percentage-point units)</summary><table><tr><th>Model</th><th>Term</th><th>Coefficient</th><th>Clustered SE</th><th>p-value</th></tr>', raw_rows, '</table></details></body></html>')
writeLines(html, file.path(report, "regression_table.html"), useBytes = TRUE)
tex_rows <- vapply(parents, function(term_name) {
  label <- if (term_name == "father_years") "Father education (years)" else "Mother education (years)"
  cells <- vapply(specs$model, function(m) {
    z <- coefs[model == m & term == term_name]
    sprintf('%.3f (%.3f)', z$estimate_pp, z$std_error_pp)
  }, character(1))
  paste0(label, ' & ', paste(cells, collapse = ' & '), ' \\\\')
}, character(1))
writeLines(c('\\begin{tabular}{lrrr}', '\\hline',
  ' & Reported + imputed & Both reported & Exclude 2026 \\\\', '\\hline', tex_rows,
  paste0('Students & ', paste(format_n(stats$N), collapse = ' & '), ' \\\\'),
  paste0('School clusters & ', paste(format_n(stats$n_schools), collapse = ' & '), ' \\\\'),
  paste0('$R^2$ & ', paste(sprintf('%.5f', stats$r2), collapse = ' & '), ' \\\\'),
  '\\hline', '\\end{tabular}',
  '% HE enrollment LPM, math >= 1 SD. Both parents enter jointly; estimates and clustered SEs are in pp per year.',
  '% Controls: full bivariate cubic scores, income deciles (D5 reference), admission year, and domicile region.',
  '% Existing grade-4 SIMCE education categories mapped to approximate years; observed/imputed versions as labeled.'),
  file.path(tables, "parent_coefficients.tex"))
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(coefs[term %in% parents, .(model, term, estimate_pp, std_error_pp, lower95_pp, upper95_pp, p_value)])
print(stats[, .(model, N, n_schools, enrollment_rate, r2, r2_without_parents, parent_share_explained, n_admission_2026)])
print(rbindlist(equality_tests))
