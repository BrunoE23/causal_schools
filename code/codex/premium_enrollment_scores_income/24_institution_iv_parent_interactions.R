# Paper scalar IV extended with both parent-education interactions.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
task <- "institution_iv_parent_education_interactions"
clean <- file.path(repo, "data/clean", task); report <- file.path(repo, "output/reports", task)
for (p in c(clean, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
tag <- "va_2017_2020__sae_2018_2020"
inputs <- c(file.path(repo, "output/intermediate/empirical_bayes_school_va", tag, "scalar_iv_regression_df.csv"),
  file.path(data_root, "data/clean/univ_gr8_df.csv"),
  file.path(repo, "output/tables/empirical_bayes_school_va", tag, "main_results.csv"))
stopifnot(all(file.exists(inputs)))
before <- file.info(inputs)
hashes <- c(unname(tools::md5sum(inputs[1L])), NA_character_, unname(tools::md5sum(inputs[3L])))
cols <- c("student_id", "mrun", "cohort_gr8", "sae_proceso", "timely_sae", "any_risk", "most_time_RBD",
  "admission_exam_taker", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to",
  "high_inst_m1", "d_highinst_adj_eb", "z_highinst_adj_eb", "expected_highinst_adj_eb")
# Preflight: ~103 MB IV file and ~1.23 GB universe; read selected columns only.
stopifnot(all(cols %in% names(fread(inputs[1L], nrows = 0L))))
d <- fread(inputs[1L], select = cols, na.strings = c("", "NA"), showProgress = FALSE)
required <- c("cohort_gr8", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to",
  "high_inst_m1", "d_highinst_adj_eb", "z_highinst_adj_eb", "expected_highinst_adj_eb", "admission_exam_taker")
x <- d[complete.cases(d[, ..required]) & admission_exam_taker == 1L]
stopifnot(!anyDuplicated(x$mrun), !anyNA(x$mrun), all(x$timely_sae == 1L), all(x$any_risk == 1L),
  all(x$high_inst_m1 %in% 0:1), all(x$sae_proceso %in% 2018:2020))
base <- c("factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to", "factor(GEN_ALU)", "factor(EDAD_ALU)")
paper_rhs <- paste(c(base, "expected_highinst_adj_eb"), collapse = " + ")
paper_formula <- as.formula(paste("high_inst_m1 ~", paper_rhs, "| 0 | d_highinst_adj_eb ~ z_highinst_adj_eb"))
paper <- feols(paper_formula, x, vcov = "hetero", notes = FALSE)
paper_fs <- feols(as.formula(paste("d_highinst_adj_eb ~", paper_rhs, "+ z_highinst_adj_eb")), x, vcov = "hetero", notes = FALSE)
ref <- fread(inputs[3L])[spec == "highinst_adj_eb"]
replication <- data.table(N = nobs(paper), ref_N = ref$n_obs,
  beta = coef(paper)["fit_d_highinst_adj_eb"], ref_beta = ref$beta,
  se = se(paper)["fit_d_highinst_adj_eb"], ref_se = ref$se,
  fs_F = (coef(paper_fs)["z_highinst_adj_eb"]/se(paper_fs)["z_highinst_adj_eb"])^2, ref_fs_F = ref$fs_f)
stopifnot(replication$N == replication$ref_N, abs(replication$beta-replication$ref_beta) < 1e-8,
  abs(replication$se-replication$ref_se) < 1e-8, abs(replication$fs_F-replication$ref_fs_F) < 1e-6)
fwrite(replication, file.path(clean, "paper_replication.csv"))
message("Paper main IV reproduced: N=", nrow(x), "; reading six parent linkage columns.")
parent_cols <- c("mrun", "cohort_gr8", "father_educ_years", "mother_educ_years", "father_educ_years_imputed", "mother_educ_years_imputed")
stopifnot(all(parent_cols %in% names(fread(inputs[2L], nrows = 0L))))
p <- fread(inputs[2L], select = parent_cols, na.strings = c("", "NA"), showProgress = FALSE)
p <- p[mrun %in% x$mrun]
stopifnot(!anyDuplicated(p$mrun), nrow(p) == nrow(x))
p <- p[match(x$mrun, mrun)]
stopifnot(identical(p$mrun, x$mrun), all(p$cohort_gr8 == x$cohort_gr8))
for (v in setdiff(parent_cols, c("mrun", "cohort_gr8"))) set(x, j = v, value = p[[v]])
rm(p, d)
keep <- is.finite(x$father_educ_years_imputed) & is.finite(x$mother_educ_years_imputed)
coverage <- data.table(N_paper = nrow(x), N_parent_sample = sum(keep), excluded_missing_parent = sum(!keep))
fwrite(x[!keep, .(N = .N), by = cohort_gr8], file.path(clean, "excluded_parent_by_cohort.csv"))
x <- x[keep]
stopifnot(all(x$father_educ_years_imputed %between% c(0,21)), all(x$mother_educ_years_imputed %between% c(0,21)))
scaling <- x[, .(father_mean = mean(father_educ_years_imputed), mother_mean = mean(mother_educ_years_imputed),
  attended_va_mean = mean(d_highinst_adj_eb), va_unit = .10)]
x[, `:=`(father_c = father_educ_years_imputed - scaling$father_mean,
  mother_c = mother_educ_years_imputed - scaling$mother_mean,
  d10 = (d_highinst_adj_eb - scaling$attended_va_mean)/.10,
  z10 = (z_highinst_adj_eb - scaling$attended_va_mean)/.10,
  e10 = (expected_highinst_adj_eb - scaling$attended_va_mean)/.10)]
x[, `:=`(d_father = d10*father_c, d_mother = d10*mother_c,
  z_father = z10*father_c, z_mother = z10*mother_c,
  e_father = e10*father_c, e_mother = e10*mother_c)]
coverage[, `:=`(n_attended_schools = uniqueN(x$most_time_RBD), outcome_rate = mean(x$high_inst_m1),
  n_father_imputed = sum(!is.finite(x$father_educ_years)), n_mother_imputed = sum(!is.finite(x$mother_educ_years)))]
controls <- c(base, "father_c", "mother_c", "e10", "e_father", "e_mother")
endog <- c("d10", "d_father", "d_mother"); instr <- c("z10", "z_father", "z_mother")
rhs <- paste(controls, collapse = " + ")
fml <- as.formula(paste("high_inst_m1 ~", rhs, "| 0 |", paste(endog, collapse = " + "), "~", paste(instr, collapse = " + ")))
message("Fitting joint interaction IV; N=", nrow(x))
iv <- feols(fml, x, vcov = "hetero", notes = FALSE)
b <- coef(iv); V <- vcov(iv); e <- resid(iv)
# Independently reconstruct 2SLS and HC1 using structural, not fitted-stage, residuals.
C <- model.matrix(as.formula(paste("~", rhs)), x)
W <- cbind(C, as.matrix(x[, ..endog])); Q <- cbind(C, as.matrix(x[, ..instr]))
wnames <- sub("^fit_", "", names(b)); W <- W[, wnames, drop = FALSE]
N <- nrow(x); K <- ncol(W)
stopifnot(nobs(iv) == N, length(b) == K, ncol(Q) == K, qr(W)$rank == K, qr(Q)$rank == K)
What <- Q %*% solve(crossprod(Q), crossprod(Q, W))
stopifnot(qr(What)$rank == K)
bread <- chol2inv(chol(crossprod(What)))
b_check <- as.numeric(bread %*% crossprod(What, x$high_inst_m1))
e_check <- x$high_inst_m1 - as.numeric(W %*% b_check)
V_check <- N/(N-K) * bread %*% crossprod(What * e_check) %*% bread
verification <- data.table(coefficient_error = max(abs(b-b_check)), residual_error = max(abs(e-e_check)),
  covariance_error = max(abs(V-V_check)), moment_error = max(abs(crossprod(Q,e)))/N,
  design_rank = qr(What)$rank, K, projected_design_condition = kappa(What, exact = TRUE))
stopifnot(verification$coefficient_error < 1e-8, verification$residual_error < 1e-8,
  verification$covariance_error < 1e-8, verification$moment_error < 1e-8)
verification[, passed := TRUE]
ct <- data.table(term = names(b), estimate = as.numeric(b), std_error = sqrt(diag(V)))
ct[, `:=`(z_statistic = estimate/std_error, p_value = 2*pnorm(-abs(estimate/std_error)),
  estimate_pp = 100*estimate, std_error_pp = 100*std_error,
  lower95_pp = 100*(estimate-qnorm(.975)*std_error), upper95_pp = 100*(estimate+qnorm(.975)*std_error))]
it <- paste0("fit_", c("d_father", "d_mother"))
joint_chi2 <- as.numeric(crossprod(b[it], solve(V[it,it], b[it])))
joint <- data.table(test = "Both VA-parent interactions zero", chi2 = joint_chi2, df = 2L,
  p_value = pchisq(joint_chi2, 2L, lower.tail = FALSE))
fs_models <- list(); fs_stats <- fs_coefs <- list()
for (v in endog) {
  fs <- feols(as.formula(paste(v, "~", rhs, "+", paste(instr, collapse = " + "))), x, vcov = "hetero", notes = FALSE)
  fs0 <- feols(as.formula(paste(v, "~", rhs)), x, notes = FALSE)
  bb <- coef(fs)[instr]; VV <- vcov(fs)[instr,instr]
  stat <- as.numeric(crossprod(bb, solve(VV,bb)))
  fs_stats[[v]] <- data.table(endogenous = v, excluded_instrument_wald_F = stat/3, numerator_df = 3L,
    partial_r2 = 1-sum(resid(fs)^2)/sum(resid(fs0)^2), p_value_chi2 = pchisq(stat,3,lower.tail=FALSE))
  fs_coefs[[v]] <- data.table(endogenous = v, instrument = instr, estimate = bb, std_error = sqrt(diag(VV)))
  fs_models[[v]] <- fs
}
first <- rbindlist(fs_stats)
# VA effects at actual parental quartiles, holding the other parent's years at its mean.
effects <- rbindlist(lapply(c("father", "mother"), function(p) {
  vals <- quantile(x[[paste0(p,"_educ_years_imputed")]], c(.25,.50,.75), names = FALSE, type = 1)
  rbindlist(lapply(seq_along(vals), function(j) {
    a <- setNames(rep(0,length(b)),names(b)); a['fit_d10'] <- 1
    a[paste0('fit_d_',p)] <- vals[j]-scaling[[paste0(p,'_mean')]]
    est <- sum(a*b); se_eff <- sqrt(as.numeric(crossprod(a,V%*%a)))
    data.table(parent = p, percentile = c(25L,50L,75L)[j], education_years = vals[j],
      va_effect_pp_per_10pp = 100*est, se_pp = 100*se_eff,
      lower95_pp = 100*(est-qnorm(.975)*se_eff), upper95_pp = 100*(est+qnorm(.975)*se_eff))
  }))
}))
after <- file.info(inputs)
stopifnot(identical(before$size, after$size), identical(before$mtime,after$mtime),
  identical(hashes[c(1,3)],unname(tools::md5sum(inputs[c(1,3)]))))
for (nm in c("coverage", "scaling", "verification", "coefficients", "joint_test", "first_stages", "first_stage_coefficients", "va_effects")) {
  obj <- switch(nm, coverage=coverage, scaling=scaling, verification=verification, coefficients=ct,
    joint_test=joint, first_stages=first, first_stage_coefficients=rbindlist(fs_coefs), va_effects=effects)
  fwrite(obj,file.path(clean,paste0(nm,".csv")))
}
fwrite(data.table(path=inputs,bytes=before$size,modified=as.character(before$mtime),md5=hashes,
  integrity=c("checksum","size and modification time","checksum")),file.path(clean,"source_manifest.csv"))
saveRDS(list(iv=iv,first_stages=fs_models,paper=paper,paper_first_stage=paper_fs),file.path(clean,"models.rds"),compress=FALSE)
saveRDS(x,file.path(clean,"analysis_data.rds"))
saveRDS(list(formula=fml,controls=controls,endogenous=endog,instruments=instr,scaling=scaling,
  vcov="HC1, paper convention",p_values="asymptotic normal/chi-square, paper convention",ids=x$mrun),file.path(clean,"specification.rds"))
labels <- c(fit_d10="Attended institution VA (+10 pp), at mean parent education",
  fit_d_father="Institution VA x father education (+10 pp x +1 year)",
  fit_d_mother="Institution VA x mother education (+10 pp x +1 year)",
  father_c="Father education (+1 year), at mean attended and expected VA",
  mother_c="Mother education (+1 year), at mean attended and expected VA")
rows <- vapply(names(labels),function(t) {
  r <- ct[term==t]; stopifnot(nrow(r)==1L)
  sprintf('<tr><th>%s</th><td>%.3f</td><td>%.3f</td><td>[%.3f, %.3f]</td><td>%.4g</td></tr>',labels[t],r$estimate_pp,r$std_error_pp,r$lower95_pp,r$upper95_pp,r$p_value)
},character(1))
fsrows <- vapply(seq_len(nrow(first)),function(j) sprintf('<tr><td>%s</td><td>%.2f</td><td>%.4f</td></tr>',
  first$endogenous[j],first$excluded_instrument_wald_F[j],first$partial_r2[j]),character(1))
erows <- vapply(seq_len(nrow(effects)),function(j) sprintf('<tr><td>%s</td><td>%s</td><td>%.2f</td><td>%.3f (%.3f)</td><td>[%.3f, %.3f]</td></tr>',
  effects$parent[j],effects$percentile[j],effects$education_years[j],effects$va_effect_pp_per_10pp[j],effects$se_pp[j],effects$lower95_pp[j],effects$upper95_pp[j]),character(1))
raw <- vapply(seq_len(nrow(ct)),function(j) sprintf('<tr><td>%s</td><td>%.4f</td><td>%.4f</td><td>%.4g</td></tr>',ct$term[j],ct$estimate_pp[j],ct$std_error_pp[j],ct$p_value[j]),character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Institution IV: parent education interactions</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1200px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Premium-institution enrollment: IV interactions with parent education</h1>',
 sprintf('<p>2SLS; %s students (%s excluded from the %s-person paper sample for missing parent schooling). Outcome: high_inst_m1. Coefficients and HC1 robust SEs in percentage points; normal/chi-square inference matches the paper.</p>',format(N,big.mark=','),coverage$excluded_missing_parent,format(coverage$N_paper,big.mark=',')),
 '<table><tr><th>Regressor</th><th>Coefficient (pp)</th><th>Robust SE</th><th>95% CI</th><th>p-value</th></tr>',rows,'</table>',
 sprintf('<p>Joint test of both interactions: chi-square(2) = %.3f, p = %.4g.</p>',joint$chi2,joint$p_value),
 '<p>Endogenous regressors: attended-school institution VA and its products with centered father and mother schooling. Excluded instruments: assigned-school institution VA and its products with the same centered parent variables. Control for expected institution VA, both expected-VA x parent interactions, both parent main effects, linear grade-4 math/verbal, and cohort, sex and age indicators. VA is centered at mean attended VA and scaled by 0.10; parents are centered at their estimation-sample means. Both parents enter jointly.</p>',
 '<p>Use the existing September paper cache: 2017-2020 observational EB VA and timely 2018-2020 SAE applicants with nondegenerate assignment risk, complete paper variables and entrance-exam taking. Preserve the paper binary outcome, including zeros for non-enrollees. No PAES-score or full observational-VA-baseline controls are added to the second stage. Parent variables retain the existing baseline-context imputations; no new imputation.</p>',
 sprintf('<p>Before extension, reproduce the paper main coefficient %.6f (SE %.6f), N=%s, first-stage F=%.3f. Independently reconstruct 2SLS coefficients, structural residuals and HC1 covariance for the interaction model; all checks pass.</p>',replication$beta,replication$se,format(replication$N,big.mark=','),replication$fs_F),
 '<h2>First stages</h2><table><tr><th>Endogenous term</th><th>Joint excluded-instrument robust Wald F (3 instruments)</th><th>Partial R2</th></tr>',fsrows,'</table>',
 '<p>These are ordinary joint first-stage statistics for each endogenous regressor, not conditional weak-identification statistics for the multi-endogenous-variable system. The projected design is full rank. Estimated VA is treated as fixed; interpretation requires the paper IV assumptions to extend to the parent-interacted instruments. Parent main effects are not randomized parent-education effects. The paper exam-taking sample restriction is retained.</p>',
 '<h2>VA effect at parental schooling quartiles</h2><p>Other parent held at their mean; outcome pp per +10 pp attended VA, using full IV covariance.</p><table><tr><th>Parent varied</th><th>Percentile</th><th>Years</th><th>VA effect (SE)</th><th>95% CI</th></tr>',erows,'</table>',
 '<details><summary>All coefficients</summary><table><tr><th>Term</th><th>Estimate (pp)</th><th>SE (pp)</th><th>p-value</th></tr>',raw,'</table></details></body></html>')
writeLines(html,file.path(report,"interactions.html"),useBytes=TRUE)
print(replication); print(coverage); print(scaling); print(ct[term %in% names(labels)]); print(joint); print(first); print(effects); print(verification)
