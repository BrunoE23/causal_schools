# Compare parents + linear PAES with the full VA baseline set, for two premium outcomes.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "premium_parent_paes_va_baseline_comparison"
clean <- file.path(repo, "data/clean", task)
report <- file.path(repo, "output/reports", task)
tables <- file.path(repo, "output/tables", task)
for (p in c(clean, report, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
inputs <- file.path(repo, "data/clean", c("he_enrollment_paes_va_baseline/analysis_data.rds",
  "all_students_by_attended_va/broad_panel_analysis_data.rds", "he_enrollment_paes_va_baseline/specification.rds"))
stopifnot(all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
x <- as.data.table(readRDS(inputs[1L]))
d <- as.data.table(readRDS(inputs[2L]))[, .(mrun, cohort_gr8, most_time_RBD, enrolled, z_math, z_verbal,
  high_paying_field_m1, high_inst_m1)]
prior <- readRDS(inputs[3L])
stopifnot(nrow(x) == 559914L, !anyDuplicated(x$mrun), !anyDuplicated(d$mrun))
d <- d[match(x$mrun, mrun)]
stopifnot(!anyNA(d$mrun), identical(x$mrun, d$mrun), all(x$he_enrollment == as.integer(d$enrolled)),
  all(x$cohort_gr8 == d$cohort_gr8), all(x$most_time_RBD == d$most_time_RBD),
  identical(x$z_math, d$z_math), identical(x$z_verbal, d$z_verbal),
  all(x$father_years == x$father_educ_years_imputed), all(x$mother_years == x$mother_educ_years_imputed))
outcomes <- c("high_paying_field_m1", "high_inst_m1")
for (o in outcomes) set(x, j = o, value = d[[o]])
stopifnot(all(unlist(x[, ..outcomes]) %in% 0:1), all(unlist(x[he_enrollment == 0L, ..outcomes]) == 0L))
ssc_rule <- prior$ssc
models <- list(); coefs <- list(); stats <- list(); checks <- list()
for (outcome in outcomes) {
  full_formula <- prior$formulas$all_va_baseline
  full_formula[[2L]] <- as.name(outcome)
  simple_formula <- reformulate(c("father_years", "mother_years", "z_math", "z_verbal"), response = outcome)
  forms <- list(parents_paes = simple_formula, all_va_baseline = full_formula)
  for (model_name in names(forms)) {
    key <- paste(outcome, model_name, sep = "__")
  message("Fitting ", model_name, "; N=", nrow(x))
  fit <- feols(forms[[model_name]], data = x, vcov = ~most_time_RBD, ssc = ssc_rule,
    fixef.rm = "none", demeaned = TRUE, fixef.tol = 1e-8, notes = FALSE)
  b <- coef(fit); V <- vcov(fit, attr = TRUE); e <- resid(fit)
  X <- if (model_name == "all_va_baseline") fit$X_demeaned else model.matrix(simple_formula, x)
  G <- uniqueN(x$most_time_RBD); N <- nrow(x); K <- attr(V, "df.K")
  stopifnot(nobs(fit) == N, is.finite(K), identical(names(b), colnames(X)), !anyNA(b))
  bread <- chol2inv(chol(crossprod(X)))
  scores <- rowsum(X * as.numeric(e), x$most_time_RBD, reorder = FALSE)
  V_check <- (G / (G - 1)) * ((N - 1) / (N - K)) * bread %*% crossprod(scores) %*% bread
  normal_error <- max(abs(crossprod(X, e))) / N; cov_error <- max(abs(V - V_check))
  stopifnot(normal_error < 1e-7, cov_error < 1e-8)
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(outcome = outcome, model = model_name, estimate_pp = 100 * estimate, std_error_pp = 100 * std_error)]
  coefs[[key]] <- ct
  stats[[key]] <- data.table(outcome = outcome, model = model_name, N, n_schools = G, K,
    outcome_rate = mean(x[[outcome]]), r2 = 1 - sum(e^2) / sum((x[[outcome]] - mean(x[[outcome]]))^2))
  checks[[key]] <- data.table(outcome = outcome, model = model_name, normal_error, cov_error, passed = TRUE)
    # Retain fitted models and covariance while avoiding redundant large within matrices.
    fit$X_demeaned <- NULL; fit$y_demeaned <- NULL
    models[[key]] <- fit
  }
}
ct <- rbindlist(coefs); sums <- rbindlist(stats)
for (o in outcomes) stopifnot(sums[outcome == o & model == "all_va_baseline", r2] >= sums[outcome == o & model == "parents_paes", r2])
stopifnot(all(sums$N == 559914L), all(sums$n_schools == 3371L), identical(unname(tools::md5sum(inputs)), hashes))
fwrite(ct, file.path(clean, "coefficients.csv")); fwrite(sums, file.path(clean, "model_summary.csv"))
fwrite(rbindlist(checks), file.path(clean, "verification.csv"))
fwrite(data.table(path = inputs, md5 = hashes), file.path(clean, "source_manifest.csv"))
fwrite(x[, c(list(N = .N, n_enrolled = sum(he_enrollment)), lapply(.SD, sum)), by = psu_year, .SDcols = outcomes],
  file.path(clean, "outcomes_by_year.csv"))
saveRDS(models, file.path(clean, "models.rds"), compress = FALSE)
saveRDS(x, file.path(clean, "analysis_data.rds"))
saveRDS(list(full_formula = prior$formulas$all_va_baseline, simple_rhs = c("father_years", "mother_years", "z_math", "z_verbal"),
  outcomes = outcomes, ssc = ssc_rule, cluster = "most_time_RBD", ids = x$mrun), file.path(clean, "specification.rds"))
labels <- c(father_years = "Father education (+1 year)", mother_years = "Mother education (+1 year)",
  z_math = "PAES math (+1 SD)", z_verbal = "PAES verbal (+1 SD)")
columns <- CJ(outcome = outcomes, model = c("parents_paes", "all_va_baseline"), sorted = FALSE)
get_cell <- function(term_name, o, m) {
  mapped <- if (m == "all_va_baseline" && term_name %in% c("father_years", "mother_years"))
    sub("_years", "_educ_years_imputed", term_name) else term_name
  z <- ct[outcome == o & model == m & term == mapped]
  stopifnot(nrow(z) == 1L)
  sprintf("%.3f (%.3f)", z$estimate_pp, z$std_error_pp)
}
rows <- vapply(names(labels), function(v) {
  cells <- vapply(seq_len(nrow(columns)), function(j) get_cell(v, columns$outcome[j], columns$model[j]), character(1))
  paste0('<tr><th>', labels[v], '</th><td>', paste(cells, collapse = '</td><td>'), '</td></tr>')
}, character(1))
for (v in c("r2", "outcome_rate", "N")) {
  cells <- vapply(seq_len(nrow(columns)), function(j) {
    o <- columns$outcome[j]; m <- columns$model[j]; val <- sums[outcome == o & model == m, get(v)]
    if (v == "N") format(val, big.mark = ',', trim = TRUE) else if (v == "outcome_rate") sprintf('%.2f%%', 100 * val) else sprintf('%.4f', val)
  }, character(1))
  label <- c(r2 = 'R-squared', outcome_rate = 'Outcome mean', N = 'Students')[v]
  rows <- c(rows, paste0('<tr><th>', label, '</th><td>', paste(cells, collapse = '</td><td>'), '</td></tr>'))
}
raw <- vapply(seq_len(nrow(ct)), function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%.4f</td><td>%.4f</td></tr>',
  ct$outcome[i], ct$model[i], ct$term[i], ct$estimate_pp[i], ct$std_error_pp[i]), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Premium field and institution regressions</title>',
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1350px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}.note{font-size:14px;color:#526171}</style></head><body>',
  '<h1>Premium field and institution: parents + PAES versus full baseline controls</h1>',
  '<p>OLS linear probability models. Coefficients are percentage points; attended-high-school-clustered SEs in parentheses. All four regressions use exactly 559,914 students and 3,371 school clusters.</p>',
  '<table><tr><th></th><th colspan="2">High-premium field</th><th colspan="2">High-premium institution</th></tr><tr><th></th><th>Parents + PAES</th><th>Full baseline</th><th>Parents + PAES</th><th>Full baseline</th></tr>',
  rows, '</table>',
  '<p>Parents + PAES includes only an intercept, both continuous parent education variables, and linear standardized entrance math/verbal. Full baseline adds the complete observational-VA control set: separate cubic grade-4 SIMCE math/verbal terms, linear income decile, parent indigenous indicators, early-childhood attendance, middle-school GPA and attendance, and cohort, sex, age, home-comuna, middle-years-observed and middle-school indicators. Parent education enters once; destination high-school effects are not included.</p>',
  '<p class="note">Sample: the previous full-baseline estimation sample, including non-enrollees (zero for both outcomes), with no high-math or HE-enrollment restriction. Keep the existing high_paying_field_m1/high_inst_m1 definitions, source imputations and score normalizations. Inference uses CR1 with exact full baseline-FE degrees of freedom and t(G-1). Overall R-squared includes the explanatory contribution of the baseline fixed effects. Estimates are conditional associations.</p>',
  '<details><summary>All raw coefficients</summary><table><tr><th>Outcome</th><th>Model</th><th>Term</th><th>Coefficient (pp)</th><th>Clustered SE</th></tr>', raw, '</table></details></body></html>')
writeLines(html, file.path(report, "comparison.html"), useBytes = TRUE)
print(ct[term %in% c(names(labels), "father_educ_years_imputed", "mother_educ_years_imputed"), .(outcome, model, term, estimate_pp, std_error_pp, p_value)])
print(sums)
