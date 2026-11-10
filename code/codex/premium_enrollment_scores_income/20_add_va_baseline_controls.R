# HE enrollment on linear PAES scores and the complete observational-VA baseline set.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
task <- "he_enrollment_paes_va_baseline"
clean <- file.path(repo, "data/clean", task); report <- file.path(repo, "output/reports", task)
dir.create(clean, recursive = TRUE, showWarnings = FALSE); dir.create(report, recursive = TRUE, showWarnings = FALSE)
input <- file.path(repo, "data/clean/parent_education_linear_scores_he_enrollment/analysis_data.rds")
source <- file.path(data_root, "data/clean/univ_gr8_df.csv")
middle_path <- file.path(data_root, "data/clean/middle_school_controls/middle_school_controls.csv")
va_code <- file.path(repo, "code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R")
paths <- c(input, source, middle_path, va_code); before <- file.info(paths)
small_hashes <- unname(tools::md5sum(paths[c(1, 4)]))
a <- as.data.table(readRDS(input))
stopifnot(nrow(a) == 569681L, !anyDuplicated(a$mrun))
cpad <- c("father_educ_years_imputed", "mother_educ_years_imputed", "father_indigenous_imputed",
  "mother_indigenous_imputed", "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed")
cols <- c("mrun", "cohort_gr8", "GEN_ALU", "EDAD_ALU", "COD_COM_ALU", "income_decile_imputed",
  cpad, "z_sim_mat_4to", "z_sim_leng_4to")
stopifnot(all(cols %in% names(fread(source, nrows = 0L))))
# Preflighted 1.23 GB universe and 100 MB middle-school file: selected columns only.
d <- fread(source, select = cols, na.strings = c("", "NA"), showProgress = FALSE)[mrun %in% a$mrun]
stopifnot(!anyDuplicated(d$mrun), nrow(d) == nrow(a))
d <- d[match(a$mrun, mrun)]
stopifnot(all(d$mrun == a$mrun), all(d$cohort_gr8 == a$cohort_gr8),
  all(d$father_educ_years_imputed == a$father_years), all(d$mother_educ_years_imputed == a$mother_years))
for (v in setdiff(cols, c("mrun", "cohort_gr8"))) set(a, j = v, value = d[[v]])
middle_vars <- c("most_time_RBD_middle", "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean")
m <- fread(middle_path, select = c("MRUN", "cohort_gr8", middle_vars), na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(!anyDuplicated(m$MRUN))
mi <- match(a$mrun, m$MRUN)
stopifnot(all(m$cohort_gr8[mi] == a$cohort_gr8, na.rm = TRUE))
for (v in middle_vars) set(a, j = v, value = m[[v]][mi])
required <- c(cols[-1L], middle_vars, "z_math", "z_verbal", "most_time_RBD", "he_enrollment")
coverage <- data.table(variable = required, missing = vapply(a[, ..required], function(v) sum(!is.finite(v)), integer(1)))
x <- a[Reduce(`&`, lapply(a[, ..required], is.finite))]
stopifnot(nrow(x) > 0L, all(x$he_enrollment %in% 0:1))
fwrite(coverage, file.path(clean, "missing_controls.csv"))
fwrite(data.table(N_prior = nrow(a), N_complete = nrow(x), excluded = nrow(a) - nrow(x)), file.path(clean, "sample_flow.csv"))
poly <- c("z_sim_mat_4to", "I(z_sim_mat_4to^2)", "I(z_sim_mat_4to^3)",
  "z_sim_leng_4to", "I(z_sim_leng_4to^2)", "I(z_sim_leng_4to^3)")
rhs <- c("z_math", "z_verbal", "income_decile_imputed", cpad, "z_gpa_middle_mean", "z_att_middle_mean", poly)
fes <- c("cohort_gr8", "GEN_ALU", "EDAD_ALU", "COD_COM_ALU", "middle_years_observed", "most_time_RBD_middle")
full_formula <- as.formula(paste("he_enrollment ~", paste(rhs, collapse = " + "), "|", paste(fes, collapse = " + ")))
simple_formula <- he_enrollment ~ father_years + mother_years + z_math + z_verbal + z_sim_mat_4to + z_sim_leng_4to
ssc_rule <- ssc(K.adj = TRUE, K.fixef = "full", K.exact = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
forms <- list(previous_same_sample = simple_formula, all_va_baseline = full_formula)
models <- list(); coefs <- list(); stats <- list(); checks <- list(); effects <- list()
for (model_name in names(forms)) {
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
  ct[, `:=`(model = model_name, estimate_pp = 100 * estimate, std_error_pp = 100 * std_error)]
  coefs[[model_name]] <- ct
  stats[[model_name]] <- data.table(model = model_name, N, n_schools = G, K,
    enrollment_rate = mean(x$he_enrollment), r2 = 1 - sum(e^2) / sum((x$he_enrollment - mean(x$he_enrollment))^2))
  checks[[model_name]] <- data.table(model = model_name, normal_error, cov_error, passed = TRUE)
  for (v in c("z_sim_mat_4to", "z_sim_leng_4to")) {
    g <- setNames(numeric(length(b)), names(b)); g[v] <- 1
    if (model_name == "all_va_baseline") {
      # fixest can wrap an existing I() call again in coefficient labels.
      pi <- match(paste0(v, "^", 2:3), gsub("[[:space:]()]|I", "", names(b)))
      if (anyNA(pi)) stop("Unmatched polynomial names: ", paste(names(b), collapse = " | "))
      g[pi] <- c(2 * mean(x[[v]]), 3 * mean(x[[v]]^2))
    }
    est <- sum(g * b); se <- sqrt(as.numeric(crossprod(g, V %*% g)))
    effects[[paste(model_name, v)]] <- data.table(model = model_name, term = v, estimate_pp = 100 * est,
      std_error_pp = 100 * se, p_value = 2 * pt(-abs(est / se), G - 1L))
  }
  models[[model_name]] <- fit
}
ct <- rbindlist(coefs); sums <- rbindlist(stats); ames <- rbindlist(effects)
stopifnot(sums[model == "all_va_baseline", r2] >= sums[model == "previous_same_sample", r2])
fwrite(ct, file.path(clean, "coefficients.csv")); fwrite(sums, file.path(clean, "model_summary.csv"))
fwrite(ames, file.path(clean, "baseline_average_derivatives.csv")); fwrite(rbindlist(checks), file.path(clean, "verification.csv"))
saveRDS(models, file.path(clean, "models.rds"), compress = FALSE); saveRDS(x, file.path(clean, "analysis_data.rds"))
saveRDS(list(formulas = forms, baseline_fixed_effects = fes, va_reference_code = va_code,
  omitted_from_va = "Destination high-school fixed effects are the VA object, not baseline controls",
  ssc = ssc_rule, cluster = "most_time_RBD"), file.path(clean, "specification.rds"))
after <- file.info(paths)
stopifnot(identical(before$size, after$size), identical(before$mtime, after$mtime),
  identical(unname(tools::md5sum(paths[c(1, 4)])), small_hashes))
fwrite(data.table(path = paths, bytes = before$size, modified = as.character(before$mtime),
  md5 = c(small_hashes[1L], NA_character_, NA_character_, small_hashes[2L])), file.path(clean, "source_manifest.csv"))
table_rows <- vapply(seq_len(nrow(ct)), function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%.3f</td><td>%.3f</td><td>%.4g</td></tr>',
  ct$model[i], ct$term[i], ct$estimate_pp[i], ct$std_error_pp[i], ct$p_value[i]), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>HE enrollment with VA baseline controls</title>',
  '<style>body{font:16px/1.5 system-ui;max-width:1300px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%}td,th{padding:9px;border-bottom:1px solid #ddd;text-align:right}td:first-child,td:nth-child(2){text-align:left}</style></head><body>',
  '<h1>HE enrollment with the complete VA baseline control set</h1>',
  sprintf('<p>Both models: %s students, %s attended-school clusters. R2: %.5f for the previous model on this sample; %.5f with the full baseline set.</p>',
    format(nrow(x), big.mark = ',', trim = TRUE), format(uniqueN(x$most_time_RBD), big.mark = ',', trim = TRUE), sums$r2[1], sums$r2[2]),
  '<p>Linear PAES math/verbal; cubic grade-4 SIMCE math and verbal separately; linear income decile, both parent education years, parent indigenous indicators, nursery/daycare/prekindergarten/kindergarten attendance, standardized middle-school GPA and attendance; cohort, sex, age, student home-comuna, observed middle-school years, and middle-school indicators. This matches the observational VA baseline controls. Destination high-school effects are not baseline variables and are not included.</p>',
  sprintf('<p>Exclude %s of the prior 569,681 students for missing baseline controls. No high-math restriction. Existing outcome, score normalization and baseline imputations are preserved. Parent education enters once. School-clustered CR1/t inference counts all baseline fixed effects using exact fixest degrees of freedom. Both covariance checks pass.</p>', format(nrow(a) - nrow(x), big.mark = ',', trim = TRUE)),
  '<p>Coefficients and SEs below are in percentage points. Raw cubic coefficients are not marginal effects; average baseline-score derivatives are saved separately. These are conditional associations.</p>',
  '<table><tr><th>Model</th><th>Term</th><th>Coefficient</th><th>Clustered SE</th><th>p-value</th></tr>', table_rows, '</table></body></html>')
writeLines(html, file.path(report, "regression_table.html"), useBytes = TRUE)
print(sums); print(ct[term %in% c("father_years", "mother_years", cpad[1:2], "z_math", "z_verbal"), .(model, term, estimate_pp, std_error_pp, p_value)])
print(ames)
