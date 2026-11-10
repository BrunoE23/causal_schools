# Projected monthly income among HE enrollees: parent schooling x institution VA.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "projected_income_parent_institution_va_interactions"
clean <- file.path(repo, "data/clean", task)
report <- file.path(repo, "output/reports", task)
for (p in c(clean, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
inputs <- file.path(repo, "data/clean", c(
  "premium_parent_education_va_interactions/analysis_data.rds",
  "premium_parent_paes_va_baseline_comparison/specification.rds",
  "all_students_by_attended_va/broad_panel_analysis_data.rds",
  "premium_parent_education_va_interactions/source_manifest.csv"))
stopifnot(all(file.exists(inputs)), all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
x <- as.data.table(readRDS(inputs[1L])); prior <- readRDS(inputs[2L])
d <- as.data.table(readRDS(inputs[3L]))[, .(mrun, most_time_RBD, cohort_gr8, enrolled, z_math, z_verbal,
  program_income_full_clp_m1, log_program_income_full_clp_m1)]
stopifnot(nrow(x) == 559819L, !anyDuplicated(x$mrun), !anyDuplicated(d$mrun))
d <- d[match(x$mrun, mrun)]
stopifnot(identical(x$mrun, d$mrun), !anyNA(d$mrun), all(x$he_enrollment == as.integer(d$enrolled)),
  all(x$most_time_RBD == d$most_time_RBD), all(x$cohort_gr8 == d$cohort_gr8),
  identical(x$z_math, d$z_math), identical(x$z_verbal, d$z_verbal),
  all(is.finite(x$high_inst_m1_va)))
x[, `:=`(projected_income_clp = d$program_income_full_clp_m1,
  log_projected_income = d$log_program_income_full_clp_m1)]
x[, valid_income := is.finite(projected_income_clp) & projected_income_clp > 0 & is.finite(log_projected_income)]
coverage <- x[, .(N_previous = .N, n_enrolled = sum(he_enrollment == 1L),
  n_non_enrolled = sum(he_enrollment == 0L), n_enrolled_invalid_income = sum(he_enrollment == 1L & !valid_income),
  common_N = sum(he_enrollment == 1L & valid_income))]
fwrite(coverage, file.path(clean, "sample_coverage.csv"))
x <- x[he_enrollment == 1L & valid_income]
stopifnot(nrow(x) > 0L, max(abs(x$log_projected_income - log(x$projected_income_clp))) < 1e-10,
  all(x$father_years == x$father_educ_years_imputed), all(x$mother_years == x$mother_educ_years_imputed))
# Linear scaling keeps the level model numerically well behaved; outputs restore CLP.
x[, projected_income_100k := projected_income_clp / 100000]
outcomes <- c("log_projected_income", "projected_income_100k")
parent_means <- c(father = mean(x$father_years), mother = mean(x$mother_years))
x[, `:=`(father_c = father_years - parent_means['father'], mother_c = mother_years - parent_means['mother'])]
models <- coefs <- stats <- checks <- slopes <- scaling <- formulas <- list()
for (o in outcomes) {
  va_raw <- x$high_inst_m1_va
  display_factor <- if (o == "log_projected_income") 1 else 100000
  display_unit <- if (o == "log_projected_income") "log_points" else "CLP_per_month"
  va_mean <- mean(va_raw)
  x[, va_10pp := (va_raw - va_mean) / 0.10]
  x[, `:=`(father_va = father_c * va_10pp, mother_va = mother_c * va_10pp)]
  scaling[[o]] <- data.table(outcome = o, va_mean, va_sd_student = sd(va_raw), va_min = min(va_raw), va_max = max(va_raw),
    father_mean = parent_means['father'], mother_mean = parent_means['mother'], va_unit = 0.10)
  simple <- reformulate(c("father_c", "mother_c", "z_math", "z_verbal", "va_10pp", "father_va", "mother_va"), response = o)
  full_text <- paste(deparse(prior$full_formula, width.cutoff = 500L), collapse = " ")
  full_text <- sub("father_educ_years_imputed", "father_c", full_text, fixed = TRUE)
  full_text <- sub("mother_educ_years_imputed", "mother_c", full_text, fixed = TRUE)
  full_text <- sub("|", "+ va_10pp + father_va + mother_va |", full_text, fixed = TRUE)
  full <- as.formula(full_text); full[[2L]] <- as.name(o)
  forms <- list(parents_paes = simple, all_va_baseline = full)
  for (m in names(forms)) {
    key <- paste(o, m, sep = "__")
    message("Fitting ", key, "; N=", nrow(x))
    fit <- feols(forms[[m]], data = x, vcov = ~most_time_RBD, ssc = prior$ssc,
      fixef.rm = "none", demeaned = TRUE, fixef.tol = 1e-8, notes = FALSE)
    b <- coef(fit); V <- vcov(fit, attr = TRUE); e <- resid(fit)
    X <- if (m == "all_va_baseline") fit$X_demeaned else model.matrix(simple, x)
    G <- uniqueN(x$most_time_RBD); N <- nrow(x); K <- attr(V, "df.K")
    stopifnot(nobs(fit) == N, is.finite(K), identical(names(b), colnames(X)), !anyNA(b), all(is.finite(V)),
      all(c("father_c", "mother_c", "va_10pp", "father_va", "mother_va") %in% names(b)))
    bread <- chol2inv(chol(crossprod(X)))
    scores <- rowsum(X * as.numeric(e), x$most_time_RBD, reorder = FALSE)
    V_check <- G/(G-1) * (N-1)/(N-K) * bread %*% crossprod(scores) %*% bread
    normal_error <- max(abs(crossprod(X, e))) / N
    cov_error <- max(abs(V - V_check))
    stopifnot(normal_error < 1e-7, cov_error < 1e-8)
    ct <- as.data.table(coeftable(fit), keep.rownames = "term")
    setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
    ct[, `:=`(outcome = o, model = m, display_unit = display_unit, estimate_display = display_factor * estimate, std_error_display = display_factor * std_error)]
    coefs[[key]] <- ct
    it <- c("father_va", "mother_va")
    joint_F <- as.numeric(crossprod(b[it], solve(V[it, it], b[it]))) / 2
    stats[[key]] <- data.table(outcome = o, model = m, N, n_schools = G, K,
      mean_outcome = mean(x[[o]]) * display_factor, display_unit = display_unit, r2 = 1 - sum(e^2)/sum((x[[o]] - mean(x[[o]]))^2),
      interaction_F = joint_F, interaction_joint_p = pf(joint_F, 2, G-1, lower.tail = FALSE))
    checks[[key]] <- data.table(outcome = o, model = m, normal_error, cov_error, passed = TRUE)
    # Exact linear-combination inference for parent gradients at student-weighted VA quartiles.
    qs <- quantile(va_raw, c(.25, .50, .75), names = FALSE, type = 1)
    slopes[[key]] <- rbindlist(lapply(c("father", "mother"), function(p) rbindlist(lapply(seq_along(qs), function(j) {
      a <- setNames(rep(0, length(b)), names(b)); a[paste0(p, "_c")] <- 1
      a[paste0(p, "_va")] <- (qs[j] - va_mean)/.10
      est <- sum(a*b); se <- sqrt(as.numeric(crossprod(a, V %*% a)))
      data.table(outcome = o, model = m, parent = p, percentile = c(25L,50L,75L)[j], va = qs[j],
        display_unit = display_unit, estimate_display = display_factor*est, std_error_display = display_factor*se,
        lower_display = display_factor*(est-qt(.975,G-1)*se), upper_display = display_factor*(est+qt(.975,G-1)*se))
    }))))
    formulas[[key]] <- forms[[m]]
    fit$X_demeaned <- NULL; fit$y_demeaned <- NULL
    models[[key]] <- fit
  }
}
ct <- rbindlist(coefs); sums <- rbindlist(stats); sc <- rbindlist(scaling); sl <- rbindlist(slopes)
stopifnot(uniqueN(sums$N) == 1L, uniqueN(sums$n_schools) == 1L, identical(unname(tools::md5sum(inputs)), hashes))
for (o in outcomes) stopifnot(sums[outcome == o & model == "all_va_baseline", r2] >= sums[outcome == o & model == "parents_paes", r2])
for (nm in c("va_10pp", "father_va", "mother_va")) x[, (nm) := NULL]
for (item in c("coefficients", "model_summary", "verification", "scaling", "parent_slopes")) {
  obj <- switch(item, coefficients = ct, model_summary = sums, verification = rbindlist(checks), scaling = sc, parent_slopes = sl)
  fwrite(obj, file.path(clean, paste0(item, ".csv")))
}
fwrite(data.table(path = inputs, bytes = file.info(inputs)$size, md5 = hashes), file.path(clean, "source_manifest.csv"))
saveRDS(models, file.path(clean, "models.rds"), compress = FALSE)
saveRDS(x, file.path(clean, "analysis_data.rds"))
saveRDS(list(formulas = formulas, scaling = sc, outcomes = outcomes, cluster = "most_time_RBD", ssc = prior$ssc,
  ids = x$mrun, coverage = coverage), file.path(clean, "specification.rds"))
labels <- c(father_c = "Father education (+1 year), at mean VA", mother_c = "Mother education (+1 year), at mean VA",
  father_va = "Father education x institution VA (+1 year x +10 pp)",
  mother_va = "Mother education x institution VA (+1 year x +10 pp)",
  va_10pp = "Institution VA (+10 pp), at mean parent education", z_math = "PAES math (+1 SD)", z_verbal = "PAES verbal (+1 SD)")
cols <- CJ(outcome = outcomes, model = c("parents_paes", "all_va_baseline"), sorted = FALSE)
row_html <- function(label, cells) paste0('<tr><th>', label, '</th><td>', paste(cells, collapse = '</td><td>'), '</td></tr>')
fmt <- function(v, o) if (o == "log_projected_income") sprintf('%.4f', v) else formatC(v, format = 'f', digits = 0, big.mark = ',')
rows <- vapply(names(labels), function(t) row_html(labels[t], vapply(seq_len(nrow(cols)), function(j) {
  z <- ct[outcome == cols$outcome[j] & model == cols$model[j] & term == t]
  stopifnot(nrow(z) == 1L)
  paste0(fmt(z$estimate_display, cols$outcome[j]), ' (', fmt(z$std_error_display, cols$outcome[j]), ')')
}, character(1))), character(1))
for (t in c("r2", "interaction_joint_p", "mean_outcome", "N", "n_schools")) rows <- c(rows, row_html(t, vapply(seq_len(nrow(cols)), function(j) {
  z <- sums[outcome == cols$outcome[j] & model == cols$model[j], get(t)]
  if (t %in% c("N", "n_schools")) format(z, big.mark = ',', trim = TRUE) else if (t == "mean_outcome") fmt(z, cols$outcome[j]) else formatC(z, digits = 4, format = 'g')
}, character(1))))
raw <- vapply(seq_len(nrow(ct)), function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%.7g</td><td>%.7g</td><td>%.4g</td></tr>',
  ct$outcome[i], ct$model[i], ct$term[i], ct$display_unit[i], ct$estimate_display[i], ct$std_error_display[i], ct$p_value[i]), character(1))
slrows <- vapply(seq_len(nrow(sl)), function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%.2f</td><td>%s (%s)</td><td>[%s, %s]</td></tr>',
  sl$outcome[i], sl$model[i], sl$parent[i], sl$percentile[i], 100*sl$va[i], fmt(sl$estimate_display[i],sl$outcome[i]),
  fmt(sl$std_error_display[i],sl$outcome[i]), fmt(sl$lower_display[i],sl$outcome[i]), fmt(sl$upper_display[i],sl$outcome[i])), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Projected income: parent education x institution VA</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1400px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Projected income: parent education x premium-institution VA</h1>',
 sprintf('<p>OLS among HE enrollees only. All four models use %s students and %s attended-school clusters. Log columns are natural-log points; level columns are monthly Chilean pesos (CLP). School-clustered SEs in parentheses.</p>', format(nrow(x),big.mark=','), uniqueN(x$most_time_RBD)),
 sprintf('<p>From the preceding %s-person VA-covered sample: %s non-enrollees excluded and %s enrollees excluded for missing/nonpositive income.</p>', format(coverage$N_previous,big.mark=','),format(coverage$n_non_enrolled,big.mark=','),coverage$n_enrolled_invalid_income),
 '<p>Income is the existing projection associated with first HE enrollment (program_income_full_clp_m1), not observed personal earnings. The natural-log identity is verified. Levels use the same positive-income sample as logs; no trimming, winsorization or new imputation. Level regressions are fitted in 100,000 CLP units and reported back in CLP.</p>',
 '<p>Both outcomes use existing 2017-2020 All-sample EB premium-institution VA of the attended high school, matched by most_time_RBD. Center VA and parental education at their means among included enrollees; scale VA so one unit equals +10 pp. Include both interactions and all lower-order terms.</p>',
 '<table><tr><th></th><th colspan="2">Log projected income (log points)</th><th colspan="2">Projected income (CLP/month)</th></tr><tr><th></th><th>Parents + PAES</th><th>Full baseline</th><th>Parents + PAES</th><th>Full baseline</th></tr>', rows, '</table>',
 '<p>Parents + PAES contains both continuous parent schooling years and linear standardized entrance math/verbal. Full baseline adds separate cubic grade-4 math/verbal, linear income decile, parent indigenous and early-childhood controls, middle-school GPA/attendance, and cohort, sex, age, comuna, middle-years-observed and middle-school indicators. No attended-HS fixed effects or high-math filter. Preserve existing imputations and score normalizations.</p>',
 '<p>CR1 uses exact full baseline fixed-effect degrees of freedom; individual tests use t(G-1) and joint interactions F(2,G-1). Inference treats VA as fixed. VA is estimated using overlapping cohorts and related outcomes, without cross-fitting. These are descriptive associations conditional on HE enrollment, not causal effects or observed earnings returns.</p>',
 '<h2>Parent gradients at student-weighted institution-VA quartiles</h2><table><tr><th>Outcome</th><th>Model</th><th>Parent</th><th>Percentile</th><th>Original centered VA (pp)</th><th>Gradient per year (SE)</th><th>95% CI</th></tr>', slrows, '</table>',
 '<details><summary>All coefficients</summary><table><tr><th>Outcome</th><th>Model</th><th>Term</th><th>Unit</th><th>Estimate</th><th>SE</th><th>p-value</th></tr>', raw, '</table></details></body></html>')
writeLines(html, file.path(report, "interactions.html"), useBytes = TRUE)
print(coverage); print(sc); print(ct[term %in% names(labels), .(outcome, model, term, estimate_display, std_error_display, p_value)])
print(sums); print(sl)
