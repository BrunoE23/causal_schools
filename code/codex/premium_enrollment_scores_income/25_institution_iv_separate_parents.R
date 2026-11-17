# Separate parent-specific IV models; only the selected parent enters each model.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "institution_iv_separate_parent_interactions"
clean <- file.path(repo,"data/clean",task); report <- file.path(repo,"output/reports",task)
for (p in c(clean,report)) dir.create(p,recursive=TRUE,showWarnings=FALSE)
previous <- file.path(repo,"data/clean/institution_iv_parent_education_interactions")
inputs <- file.path(previous,c("analysis_data.rds","specification.rds","coefficients.csv","source_manifest.csv"))
stopifnot(all(file.exists(inputs)),all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
x <- as.data.table(readRDS(inputs[1L])); prior <- readRDS(inputs[2L]); joint <- fread(inputs[3L])
stopifnot(nrow(x)==94244L,!anyDuplicated(x$mrun),identical(x$mrun,prior$ids),all(x$admission_exam_taker==1L),
  all(x$timely_sae==1L),all(x$any_risk==1L),all(x$high_inst_m1 %in% 0:1))
base <- c("factor(cohort_gr8)","z_sim_mat_4to","z_sim_leng_4to","factor(GEN_ALU)","factor(EDAD_ALU)")
coefs <- checks <- firsts <- fs_coefficients <- models <- specs <- list()
for (parent in c("father","mother")) {
  controls <- c(base,paste0(parent,"_c"),"e10",paste0("e_",parent))
  endog <- c("d10",paste0("d_",parent)); instr <- c("z10",paste0("z_",parent))
  rhs <- paste(controls,collapse=" + ")
  fml <- as.formula(paste("high_inst_m1 ~",rhs,"| 0 |",paste(endog,collapse=" + "),"~",paste(instr,collapse=" + ")))
  other <- if (parent=="father") "mother" else "father"
  stopifnot(!any(grepl(other,all.vars(fml))))
  message("Fitting ",parent," only IV; N=",nrow(x))
  iv <- feols(fml,x,vcov="hetero",notes=FALSE)
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
fs_models <- list(); fs_stats <- fs_coefs <- list()
for (v in endog) {
  fs <- feols(as.formula(paste(v, "~", rhs, "+", paste(instr, collapse = " + "))), x, vcov = "hetero", notes = FALSE)
  fs0 <- feols(as.formula(paste(v, "~", rhs)), x, notes = FALSE)
  bb <- coef(fs)[instr]; VV <- vcov(fs)[instr,instr]
  stat <- as.numeric(crossprod(bb, solve(VV,bb)))
  fs_stats[[v]] <- data.table(endogenous = v, excluded_instrument_wald_F = stat/2, numerator_df = 2L,
    partial_r2 = 1-sum(resid(fs)^2)/sum(resid(fs0)^2), p_value_chi2 = pchisq(stat,2,lower.tail=FALSE))
  fs_coefs[[v]] <- data.table(endogenous = v, instrument = instr, estimate = bb, std_error = sqrt(diag(VV)))
  fs_models[[v]] <- fs
}
first <- rbindlist(fs_stats)

  ct[, model := parent]; verification[, model := parent]; first[, model := parent]
  fc <- rbindlist(fs_coefs); fc[, model := parent]
  coefs[[parent]] <- ct; checks[[parent]] <- verification; firsts[[parent]] <- first
  fs_coefficients[[parent]] <- fc; models[[parent]] <- list(iv=iv,first_stages=fs_models)
  specs[[parent]] <- list(formula=fml,controls=controls,endogenous=endog,instruments=instr)
}
ct <- rbindlist(coefs); verification <- rbindlist(checks); first <- rbindlist(firsts)
comparison <- rbindlist(lapply(c("father","mother"),function(p) {
  t <- paste0("fit_d_",p)
  separate <- ct[model==p & term==t]; both <- joint[term==t]
  data.table(parent=p,separate_estimate_pp=separate$estimate_pp,separate_se_pp=separate$std_error_pp,
    separate_p=separate$p_value,joint_estimate_pp=both$estimate_pp,joint_se_pp=both$std_error_pp,joint_p=both$p_value)
}))
stopifnot(identical(hashes,unname(tools::md5sum(inputs))))
for (nm in c("coefficients","verification","first_stages","first_stage_coefficients","comparison")) {
  obj <- switch(nm,coefficients=ct,verification=verification,first_stages=first,
    first_stage_coefficients=rbindlist(fs_coefficients),comparison=comparison)
  fwrite(obj,file.path(clean,paste0(nm,".csv")))
}
fwrite(data.table(path=inputs,bytes=file.info(inputs)$size,md5=hashes),file.path(clean,"source_manifest.csv"))
fwrite(data.table(N=nrow(x),n_attended_schools=uniqueN(x$most_time_RBD),outcome_rate=mean(x$high_inst_m1),
  parent_correlation=cor(x$father_c,x$mother_c)),file.path(clean,"sample_summary.csv"))
saveRDS(models,file.path(clean,"models.rds"),compress=FALSE)
saveRDS(list(models=specs,scaling=prior$scaling,ids=x$mrun,sample_path=inputs[1L],
  vcov="HC1",p_values="normal/chi-square",other_parent_main_effect=FALSE),file.path(clean,"specification.rds"))
rows <- vapply(c("va","interaction","parent"),function(v) {
  cells <- vapply(c("father","mother"),function(p) {
    t <- switch(v,va="fit_d10",interaction=paste0("fit_d_",p),parent=paste0(p,"_c"))
    z <- ct[model==p & term==t]; stopifnot(nrow(z)==1L)
    sprintf('<td>%.3f (%.3f)</td>',z$estimate_pp,z$std_error_pp)
  },character(1))
  label <- c(va="Institution VA (+10 pp), at mean selected-parent schooling",
    interaction="VA x selected parent education (+10 pp x +1 year)",parent="Selected parent education (+1 year)")[v]
  paste0('<tr><th>',label,'</th>',paste(cells,collapse=''),'</tr>')
},character(1))
pvals <- vapply(c("father","mother"),function(p) sprintf('<td>%.4f</td>',comparison[parent==p,separate_p]),character(1))
rows <- c(rows,paste0('<tr><th>Interaction p-value</th>',paste(pvals,collapse=''),'</tr>'))
comp_rows <- vapply(seq_len(nrow(comparison)),function(i) {
  r <- comparison[i]
  sprintf('<tr><td>%s</td><td>%.3f (%.3f)</td><td>%.4f</td><td>%.3f (%.3f)</td><td>%.4f</td></tr>',
    r$parent,r$separate_estimate_pp,r$separate_se_pp,r$separate_p,r$joint_estimate_pp,r$joint_se_pp,r$joint_p)
},character(1))
fsrows <- vapply(seq_len(nrow(first)),function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%.2f</td><td>%.4f</td></tr>',
  first$model[i],first$endogenous[i],first$excluded_instrument_wald_F[i],first$partial_r2[i]),character(1))
raw <- vapply(seq_len(nrow(ct)),function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%.4f</td><td>%.4f</td><td>%.4g</td></tr>',
  ct$model[i],ct$term[i],ct$estimate_pp[i],ct$std_error_pp[i],ct$p_value[i]),character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Institution IV: one parent at a time</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1200px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Premium-institution IV: one parent at a time</h1>',
 '<p>Each model contains only the selected parent education main effect and VA interaction. Both use the same 94,244 students as the prior joint-parent IV. Outcome: premium-institution enrollment. Coefficients in percentage points; HC1 robust SEs in parentheses, normal/chi-square inference matching the paper.</p>',
 '<table><tr><th></th><th>Father only</th><th>Mother only</th></tr>',rows,'</table>',
 '<p>For parent P: instrument attended institution VA D and D x P with assigned VA Z and Z x P. Controls: parent P, expected VA E, E x P, linear baseline math/verbal, and cohort, sex and age indicators. The other parent main effect, VA interaction, instrument interaction and expected-VA interaction are omitted. Keep the previous common sample, parent means, VA centering and 10 pp VA unit. No new data restrictions, imputations or PAES-score controls.</p>',
 '<h2>Interaction comparison with the joint model</h2><table><tr><th>Parent</th><th>Separate (SE)</th><th>p-value</th><th>Joint (SE)</th><th>p-value</th></tr>',comp_rows,'</table>',
 '<h2>First stages</h2><table><tr><th>Model</th><th>Endogenous term</th><th>Joint excluded-instrument robust Wald F (2 instruments)</th><th>Partial R2</th></tr>',fsrows,'</table>',
 '<p>Both models pass independent 2SLS coefficient, structural residual, HC1 covariance, moment and full-rank checks. First-stage statistics are ordinary joint tests, not conditional weak-identification diagnostics. Estimated VA is treated as fixed. Interpretation requires the interacted IV assumptions and retains the paper lottery-risk/exam-taking sample restriction. Parent main effects are not causal effects of parental education.</p>',
 '<details><summary>All coefficients</summary><table><tr><th>Model</th><th>Term</th><th>Coefficient (pp)</th><th>SE (pp)</th><th>p-value</th></tr>',raw,'</table></details></body></html>')
writeLines(html,file.path(report,"interactions.html"),useBytes=TRUE)
print(ct[term %in% c("fit_d10","fit_d_father","fit_d_mother","father_c","mother_c")]); print(comparison); print(first); print(verification)
