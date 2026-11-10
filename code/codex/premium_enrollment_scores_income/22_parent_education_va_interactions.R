# Interact each parent's schooling with the attended school's outcome-specific EB VA.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "premium_parent_education_va_interactions"
clean <- file.path(repo, "data/clean", task)
report <- file.path(repo, "output/reports", task)
for (p in c(clean, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
inputs <- c(file.path(repo, "data/clean/premium_parent_paes_va_baseline_comparison", c("analysis_data.rds", "specification.rds")),
  "C:/Users/brunem/Box/causal_schools/data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
stopifnot(all(file.exists(inputs)), all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
x <- as.data.table(readRDS(inputs[1L])); prior <- readRDS(inputs[2L])
outcomes <- c("high_paying_field_m1", "high_inst_m1")
stopifnot(nrow(x) == 559914L, !anyDuplicated(x$mrun))
v <- fread(inputs[3L], select = c("outcome", "school_rbd", "analysis_sample", "controlled_value_added_eb_centered_student"))
v <- v[analysis_sample == "All" & outcome %chin% outcomes]
stopifnot(!anyDuplicated(v[, .(outcome, school_rbd)]))
setnames(v, "controlled_value_added_eb_centered_student", "va")
coverage <- list()
for (o in outcomes) {
  vv <- v[outcome == o]
  z <- vv$va[match(x$most_time_RBD, vv$school_rbd)]
  set(x, j = paste0(o, "_va"), value = z)
  coverage[[o]] <- data.table(outcome = o, original_N = nrow(x), matched_N = sum(is.finite(z)))
}
va_cols <- paste0(outcomes, "_va")
keep <- Reduce(`&`, lapply(x[, ..va_cols], is.finite))
fwrite(x[!keep, .(N = .N), by = most_time_RBD], file.path(clean, "excluded_schools.csv"))
x <- x[keep]
coverage <- rbindlist(coverage); coverage[, common_N := nrow(x)]
fwrite(coverage, file.path(clean, "va_coverage.csv"))
stopifnot(all(x$father_years == x$father_educ_years_imputed), all(x$mother_years == x$mother_educ_years_imputed),
  all(unlist(x[, ..outcomes]) %in% 0:1), all(unlist(x[he_enrollment == 0L, ..outcomes]) == 0L))
parent_means <- c(father = mean(x$father_years), mother = mean(x$mother_years))
x[, `:=`(father_c = father_years - parent_means['father'], mother_c = mother_years - parent_means['mother'])]
models <- coefs <- stats <- checks <- slopes <- scaling <- formulas <- list()
for (o in outcomes) {
  va_raw <- x[[paste0(o, "_va")]]
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
    ct[, `:=`(outcome = o, model = m, estimate_pp = 100 * estimate, std_error_pp = 100 * std_error)]
    coefs[[key]] <- ct
    it <- c("father_va", "mother_va")
    joint_F <- as.numeric(crossprod(b[it], solve(V[it, it], b[it]))) / 2
    stats[[key]] <- data.table(outcome = o, model = m, N, n_schools = G, K,
      outcome_rate = mean(x[[o]]), r2 = 1 - sum(e^2)/sum((x[[o]] - mean(x[[o]]))^2),
      interaction_F = joint_F, interaction_joint_p = pf(joint_F, 2, G-1, lower.tail = FALSE))
    checks[[key]] <- data.table(outcome = o, model = m, normal_error, cov_error, passed = TRUE)
    # Exact linear-combination inference for parent gradients at student-weighted VA quartiles.
    qs <- quantile(va_raw, c(.25, .50, .75), names = FALSE, type = 1)
    slopes[[key]] <- rbindlist(lapply(c("father", "mother"), function(p) rbindlist(lapply(seq_along(qs), function(j) {
      a <- setNames(rep(0, length(b)), names(b)); a[paste0(p, "_c")] <- 1
      a[paste0(p, "_va")] <- (qs[j] - va_mean)/.10
      est <- sum(a*b); se <- sqrt(as.numeric(crossprod(a, V %*% a)))
      data.table(outcome = o, model = m, parent = p, percentile = c(25L,50L,75L)[j], va = qs[j],
        estimate_pp = 100*est, std_error_pp = 100*se,
        lower_pp = 100*(est-qt(.975,G-1)*se), upper_pp = 100*(est+qt(.975,G-1)*se))
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
  father_va = "Father education x VA (+1 year x +10 pp)", mother_va = "Mother education x VA (+1 year x +10 pp)",
  va_10pp = "School VA (+10 pp), at mean parent education", z_math = "PAES math (+1 SD)", z_verbal = "PAES verbal (+1 SD)")
cols <- CJ(outcome = outcomes, model = c("parents_paes", "all_va_baseline"), sorted = FALSE)
row_html <- function(label, cells) paste0('<tr><th>', label, '</th><td>', paste(cells, collapse = '</td><td>'), '</td></tr>')
rows <- vapply(names(labels), function(t) row_html(labels[t], vapply(seq_len(nrow(cols)), function(j) {
  z <- ct[outcome == cols$outcome[j] & model == cols$model[j] & term == t]
  stopifnot(nrow(z) == 1L); sprintf('%.3f (%.3f)', z$estimate_pp, z$std_error_pp)
}, character(1))), character(1))
for (t in c("r2", "interaction_joint_p", "N", "n_schools")) rows <- c(rows, row_html(t, vapply(seq_len(nrow(cols)), function(j) {
  z <- sums[outcome == cols$outcome[j] & model == cols$model[j], get(t)]
  if (t %in% c("N", "n_schools")) format(z, big.mark = ',', trim = TRUE) else formatC(z, digits = 4, format = 'g')
}, character(1))))
raw <- vapply(seq_len(nrow(ct)), function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%.4f</td><td>%.4f</td><td>%.4g</td></tr>',
  ct$outcome[i], ct$model[i], ct$term[i], ct$estimate_pp[i], ct$std_error_pp[i], ct$p_value[i]), character(1))
slrows <- vapply(seq_len(nrow(sl)), function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%.2f</td><td>%.3f (%.3f)</td><td>[%.3f, %.3f]</td></tr>',
  sl$outcome[i], sl$model[i], sl$parent[i], sl$percentile[i], 100*sl$va[i], sl$estimate_pp[i], sl$std_error_pp[i], sl$lower_pp[i], sl$upper_pp[i]), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Parent education and attended-school VA interactions</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1400px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Parent education x attended-school VA</h1>',
 sprintf('<p>OLS; coefficients in percentage points, attended-high-school-clustered SEs in parentheses. Common sample: %s students and %s school clusters; %s students excluded for missing VA.</p>', format(nrow(x),big.mark=','),uniqueN(x$most_time_RBD),559914L-nrow(x)),
 '<p>Use the existing 2017-2020 All-sample EB-adjusted field VA for the field outcome and institution VA for the institution outcome, matched to most_time_RBD. Center each VA at its student-weighted sample mean and divide by 0.10. Center both parent education variables at their student means. Include both parents, both interactions and the VA main effect in every model.</p>',
 '<table><tr><th></th><th colspan="2">High-premium field</th><th colspan="2">High-premium institution</th></tr><tr><th></th><th>Parents + PAES</th><th>Full baseline</th><th>Parents + PAES</th><th>Full baseline</th></tr>', rows, '</table>',
 '<p>Parents + PAES controls for both continuous parent schooling years and linear standardized entrance math/verbal. Full baseline additionally includes separate cubic grade-4 math/verbal, linear income decile, parent indigenous and early-childhood variables, middle-school GPA/attendance, and cohort, sex, age, comuna, middle-years-observed and middle-school indicators. No destination high-school fixed effects. Non-enrollees remain zero; no high-math filter. Existing imputations and score normalizations retained.</p>',
 '<p>CR1 inference counts full baseline fixed-effect degrees of freedom exactly; individual tests use t(G-1), and the joint interaction test uses F(2,G-1). Inference treats estimated VA as fixed. VA is estimated from overlapping cohorts/outcomes, without cross-fitting; results are descriptive conditional associations, not causal effects.</p>',
 '<h2>Parent gradients at student-weighted VA quartiles</h2><table><tr><th>Outcome</th><th>Model</th><th>Parent</th><th>Percentile</th><th>Original centered VA (pp)</th><th>pp per schooling year (SE)</th><th>95% CI</th></tr>', slrows, '</table>',
 '<details><summary>All coefficients</summary><table><tr><th>Outcome</th><th>Model</th><th>Term</th><th>Estimate (pp)</th><th>SE (pp)</th><th>p-value</th></tr>', raw, '</table></details></body></html>')
writeLines(html, file.path(report, "interactions.html"), useBytes = TRUE)
print(coverage); print(sc); print(ct[term %in% names(labels), .(outcome, model, term, estimate_pp, std_error_pp, p_value)])
print(sums); print(sl)
