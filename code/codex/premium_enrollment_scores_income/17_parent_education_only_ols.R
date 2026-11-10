# Full-sample HE enrollment on continuous father and mother education only.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
task <- "parent_education_only_he_enrollment"
clean <- file.path(repo, "data/clean", task)
report <- file.path(repo, "output/reports", task)
tables <- file.path(repo, "output/tables", task)
for (path in c(clean, report, tables)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
sample_path <- file.path(repo, "data/clean/all_students_by_attended_va/broad_panel_analysis_data.rds")
parent_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
before <- file.info(c(sample_path, parent_path))
sample_hash <- unname(tools::md5sum(sample_path))
a <- as.data.table(readRDS(sample_path))[, .(mrun, cohort_gr8, most_time_RBD, enrolled, psu_year)]
stopifnot(nrow(a) == 931547L, !anyDuplicated(a$mrun), !anyNA(a$mrun),
  !anyNA(a$enrolled), all(a$most_time_RBD > 0))
cols <- c("mrun", "cohort_gr8", "father_educ_years", "mother_educ_years",
  "father_educ_years_imputed", "mother_educ_years_imputed")
stopifnot(all(cols %in% names(fread(parent_path, nrows = 0L))))
# The source is 1.23 GB. Read six columns only, then retain the broad-panel IDs.
message("Reading six linkage/education columns; retaining the full broad-panel sample.")
p <- fread(parent_path, select = cols, na.strings = c("", "NA"), showProgress = FALSE)
p <- p[mrun %in% a$mrun]
stopifnot(nrow(p) == nrow(a), !anyDuplicated(p$mrun))
p <- p[match(a$mrun, mrun)]
stopifnot(all(p$mrun == a$mrun), all(p$cohort_gr8 == a$cohort_gr8))
for (v in setdiff(cols, c("mrun", "cohort_gr8"))) set(a, j = v, value = p[[v]])
rm(p)
a[, `:=`(father_years = father_educ_years_imputed, mother_years = mother_educ_years_imputed,
  he_enrollment = as.integer(enrolled))]
x <- a[is.finite(father_years) & is.finite(mother_years)]
stopifnot(nrow(x) == 775268L, all(x$he_enrollment %in% 0:1),
  all(x$father_years >= 0 & x$father_years <= 21), all(x$mother_years >= 0 & x$mother_years <= 21))
for (parent in c("father", "mother")) {
  observed <- x[[paste0(parent, "_educ_years")]]
  years <- x[[paste0(parent, "_years")]]
  stopifnot(all(years[is.finite(observed)] == observed[is.finite(observed)]))
}
fml <- he_enrollment ~ father_years + mother_years
fit <- feols(fml, data = x, vcov = ~most_time_RBD,
  ssc = ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min"), notes = FALSE)
X <- model.matrix(~father_years + mother_years, x)
N <- nrow(X); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
b <- coef(fit); V <- vcov(fit); e <- resid(fit)
stopifnot(K == 3L, nobs(fit) == N, identical(names(b), colnames(X)), qr(X)$rank == K)
bread <- chol2inv(chol(crossprod(X)))
scores <- rowsum(X * as.numeric(e), x$most_time_RBD, reorder = FALSE)
V_check <- (G / (G - 1)) * ((N - 1) / (N - K)) * bread %*% crossprod(scores) %*% bread
normal_error <- max(abs(crossprod(X, e))) / N
covariance_error <- max(abs(V - V_check))
stopifnot(normal_error < 1e-8, covariance_error < 1e-9)
ct <- as.data.table(coeftable(fit), keep.rownames = "term")
setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
ct[, `:=`(lower95 = estimate - qt(.975, G - 1L) * std_error,
  upper95 = estimate + qt(.975, G - 1L) * std_error, df = G - 1L)]
for (v in c("estimate", "std_error", "lower95", "upper95")) ct[, (paste0(v, "_pp")) := 100 * get(v)]
summary <- data.table(N_broad = nrow(a), N = N, excluded_missing_parent = nrow(a) - N, n_schools = G,
  n_parameters = K, enrollment_rate = mean(x$he_enrollment),
  r2 = 1 - sum(e^2) / sum((x$he_enrollment - mean(x$he_enrollment))^2),
  n_both_reported = sum(is.finite(x$father_educ_years) & is.finite(x$mother_educ_years)),
  n_father_imputed = sum(!is.finite(x$father_educ_years)), n_mother_imputed = sum(!is.finite(x$mother_educ_years)),
  mean_father_years = mean(x$father_years), mean_mother_years = mean(x$mother_years),
  parent_correlation = cor(x$father_years, x$mother_years),
  n_admission_2026 = sum(x$psu_year == 2026L, na.rm = TRUE),
  n_enrolled_admission_2026 = sum(x$psu_year == 2026L & x$enrolled, na.rm = TRUE),
  predictions_below_zero = mean(fitted(fit) < 0), predictions_above_one = mean(fitted(fit) > 1))
parents <- c("father_years", "mother_years")
F_joint <- as.numeric(crossprod(b[parents], solve(V[parents, parents], b[parents]))) / 2
difference <- b["mother_years"] - b["father_years"]
difference_se <- sqrt(V["mother_years", "mother_years"] + V["father_years", "father_years"] - 2 * V["mother_years", "father_years"])
fwrite(data.table(test = c("Both parent coefficients zero", "Mother minus father equals zero"),
  statistic = c(F_joint, difference / difference_se), df1 = c(2L, NA_integer_), df2 = G - 1L,
  p_value = c(pf(F_joint, 2L, G - 1L, lower.tail = FALSE), 2 * pt(-abs(difference / difference_se), G - 1L))),
  file.path(clean, "joint_tests.csv"))
fwrite(ct, file.path(clean, "coefficients.csv")); fwrite(summary, file.path(clean, "model_summary.csv"))
fwrite(x[, .(N = .N, n_enrolled = sum(enrolled)), by = cohort_gr8], file.path(clean, "sample_by_cohort.csv"))
saveRDS(x[, .(mrun, cohort_gr8, most_time_RBD, he_enrollment, father_years, mother_years)], file.path(clean, "analysis_data.rds"))
saveRDS(fit, file.path(clean, "model.rds")); saveRDS(V, file.path(clean, "covariance_matrix.rds"))
saveRDS(list(formula = fml, sample = "Full broad panel with both education measures available",
  parent_variables = c("father_educ_years_imputed", "mother_educ_years_imputed"),
  controls = "None beyond the two requested parent education regressors and intercept",
  cluster = "most_time_RBD", source = c(sample_path, parent_path)), file.path(clean, "specification.rds"))
after <- file.info(c(sample_path, parent_path))
stopifnot(identical(before$size, after$size), identical(before$mtime, after$mtime),
  identical(sample_hash, unname(tools::md5sum(sample_path))))
fwrite(data.table(path = c(sample_path, parent_path), bytes = before$size, modified = as.character(before$mtime),
  md5 = c(sample_hash, NA_character_), integrity = c("checksum", "size and modification time")), file.path(clean, "source_manifest.csv"))
fwrite(data.table(check = c("Exactly intercept and two continuous parent regressors", "OLS normal equations",
  "School-clustered CR1 covariance", "Sources unchanged"),
  error = c(0, normal_error, covariance_error, 0), passed = TRUE), file.path(clean, "verification.csv"))
display <- ct[term %in% parents]
display[, label := ifelse(term == "father_years", "Father education (+1 year)", "Mother education (+1 year)")]
rows <- vapply(seq_len(nrow(display)), function(i) {
  z <- display[i]
  sprintf('<tr><th>%s</th><td>%.3f</td><td>%.3f</td><td>[%.3f, %.3f]</td></tr>',
    z$label, z$estimate_pp, z$std_error_pp, z$lower95_pp, z$upper95_pp)
}, character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>HE enrollment on parent education only</title>',
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1000px;margin:32px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:24px 0}th,td{padding:12px;border-bottom:1px solid #dce2e8;text-align:right}th:first-child{text-align:left}.note{font-size:14px;color:#526171}</style></head><body>',
  '<h1>HE enrollment on parent education only</h1><p>OLS: HE enrollment = intercept + father schooling years + mother schooling years. Both parents enter jointly and continuously. No other controls or high-math restriction.</p>',
  '<table><tr><th>Regressor</th><th>Coefficient (pp/year)</th><th>Clustered SE</th><th>95% CI</th></tr>', rows, '</table>',
  sprintf('<p>N = %s; school clusters = %s; enrollment rate = %.2f%%; R-squared = %.4f.</p>',
    format(N, big.mark = ',', trim = TRUE), format(G, big.mark = ',', trim = TRUE), 100 * summary$enrollment_rate, summary$r2),
  sprintf('<p class="note">Start with all 931,547 broad-panel students (grade-8 cohorts 2017-2020, ages 12-16, attended school observed). Retain %s with both parent measures available; exclude %s with remaining missing education. No restrictions on exam-taking, scores, income, region, or estimated school VA.</p>',
    format(N, big.mark = ',', trim = TRUE), format(nrow(a) - N, big.mark = ',', trim = TRUE)),
  sprintf('<p class="note">Education uses the existing grade-4 SIMCE category-to-approximate-years conversion and existing baseline-context imputations: %s father and %s mother values are imputed; %s students report both. No new imputation is performed. Each coefficient conditions on the other parent\'s schooling. These are associations, not causal effects.</p>',
    format(summary$n_father_imputed, big.mark = ',', trim = TRUE), format(summary$n_mother_imputed, big.mark = ',', trim = TRUE), format(summary$n_both_reported, big.mark = ',', trim = TRUE)),
  '<p class="note">SEs cluster by attended high school with CR1 finite-sample correction and t(G-1) intervals. Enrollment and admission-year records are kept as recorded in the existing data. The model includes no admission-year effects.</p></body></html>')
writeLines(html, file.path(report, "regression_table.html"), useBytes = TRUE)
tex_rows <- vapply(seq_len(nrow(display)), function(i) sprintf('%s & %.3f & (%.3f) \\\\',
  display$label[i], display$estimate_pp[i], display$std_error_pp[i]), character(1))
writeLines(c('\\begin{tabular}{lrr}', '\\hline', ' & Coefficient (pp/year) & Clustered SE \\\\', '\\hline', tex_rows,
  sprintf('Students & %s & \\\\', format(N, big.mark = ',', trim = TRUE)), sprintf('$R^2$ & %.4f & \\\\', summary$r2),
  '\\hline', '\\end{tabular}', '% OLS with intercept and both continuous parent education variables only; full broad panel.',
  '% Existing SIMCE approximate education years, reported plus imputed; school-clustered CR1 inference.'),
  file.path(tables, "regression_table.tex"))
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(ct); print(summary)
