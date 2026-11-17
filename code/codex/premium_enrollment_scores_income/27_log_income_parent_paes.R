# Add linear standardized PAES math and verbal, one parent's years at a time.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset=getwd())
task <- "log_projected_income_parent_paes"
clean <- file.path(repo,"data/clean",task); report <- file.path(repo,"output/reports",task)
for (p in c(clean,report)) dir.create(p,recursive=TRUE,showWarnings=FALSE)
input <- file.path(repo,"data/clean/projected_income_parent_institution_va_interactions/analysis_data.rds")
stopifnot(file.exists(input),file.info(input)$size < 100e6)
hash <- unname(tools::md5sum(input))
x <- as.data.table(readRDS(input))[, .(mrun,most_time_RBD,cohort_gr8,he_enrollment,
  father_years,mother_years,z_math,z_verbal,projected_income_clp,log_projected_income)]
stopifnot(nrow(x)==463033L,!anyDuplicated(x$mrun),all(x$he_enrollment==1L),
  all(x$projected_income_clp>0),max(abs(x$log_projected_income-log(x$projected_income_clp)))<1e-10,
  all(is.finite(x$father_years)),all(is.finite(x$mother_years)),all(is.finite(x$z_math)),all(is.finite(x$z_verbal)))
models <- coefs <- stats <- checks <- formulas <- list()
for (parent in c("father","mother")) {
  term <- paste0(parent,"_years")
  fml <- reformulate(c(term,"z_math","z_verbal"),response="log_projected_income")
  fit <- feols(fml,x,vcov=~most_time_RBD,
    ssc=ssc(K.adj=TRUE,G.adj=TRUE,G.df="min",t.df="min"),notes=FALSE)
  X <- model.matrix(fml,x); b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  N <- nrow(x); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
  stopifnot(K==4L,nobs(fit)==N,identical(names(b),colnames(X)))
  bread <- chol2inv(chol(crossprod(X)))
  scores <- rowsum(X*as.numeric(e),x$most_time_RBD,reorder=FALSE)
  V_check <- G/(G-1)*(N-1)/(N-K)*bread%*%crossprod(scores)%*%bread
  normal_error <- max(abs(crossprod(X,e)))/N; covariance_error <- max(abs(V-V_check))
  stopifnot(normal_error<1e-8,covariance_error<1e-9)
  ct <- as.data.table(coeftable(fit),keep.rownames="term")
  setnames(ct,names(ct)[2:5],c("estimate","std_error","t_statistic","p_value"))
  ct[, `:=`(model=parent,lower95=estimate-qt(.975,G-1)*std_error,upper95=estimate+qt(.975,G-1)*std_error)]
  coefs[[parent]] <- ct
  stats[[parent]] <- data.table(model=parent,N,n_schools=G,r2=1-sum(e^2)/sum((x$log_projected_income-mean(x$log_projected_income))^2),
    pct_geometric_mean_per_year=100*expm1(b[term]),mean_log_income=mean(x$log_projected_income))
  checks[[parent]] <- data.table(model=parent,normal_error,covariance_error,passed=TRUE)
  models[[parent]] <- fit; formulas[[parent]] <- fml
}
ct <- rbindlist(coefs); sums <- rbindlist(stats)
stopifnot(identical(hash,unname(tools::md5sum(input))),uniqueN(sums$N)==1L)
fwrite(ct,file.path(clean,"coefficients.csv")); fwrite(sums,file.path(clean,"model_summary.csv"))
fwrite(rbindlist(checks),file.path(clean,"verification.csv"))
fwrite(data.table(path=input,bytes=file.info(input)$size,md5=hash),file.path(clean,"source_manifest.csv"))
saveRDS(models,file.path(clean,"models.rds"),compress=FALSE)
saveRDS(x,file.path(clean,"analysis_data.rds"))
saveRDS(list(formulas=formulas,ids=x$mrun,cluster="most_time_RBD",source=input,
  sample="Same complete-baseline and VA-covered HE enrollees as prior projected-income regressions; no additional restrictions"),
  file.path(clean,"specification.rds"))
previous <- file.path(repo,"data/clean/log_projected_income_parent_only")
comparison_inputs <- file.path(previous,c("coefficients.csv","model_summary.csv","source_manifest.csv"))
comparison_hashes <- unname(tools::md5sum(comparison_inputs))
old <- fread(comparison_inputs[1L]); old_stats <- fread(comparison_inputs[2L]); old_manifest <- fread(comparison_inputs[3L])
stopifnot(old_manifest$path==input,old_manifest$md5==hash,all(old_stats$N==nrow(x)))
comparison <- rbindlist(lapply(c("father","mother"),function(p) {
  t <- paste0(p,"_years"); now <- ct[model==p & term==t]; before <- old[model==p & term==t]
  data.table(model=p,before_estimate=before$estimate,before_se=before$std_error,
    paes_estimate=now$estimate,paes_se=now$std_error,
    before_r2=old_stats[model==p,r2],paes_r2=sums[model==p,r2],
    coefficient_reduction_pct=100*(1-now$estimate/before$estimate))
}))
stopifnot(all(comparison$paes_r2>=comparison$before_r2),identical(comparison_hashes,unname(tools::md5sum(comparison_inputs))))
fwrite(comparison,file.path(clean,"comparison.csv"))
fwrite(rbindlist(list(data.table(path=input,bytes=file.info(input)$size,md5=hash),
  data.table(path=comparison_inputs,bytes=file.info(comparison_inputs)$size,md5=comparison_hashes))),file.path(clean,"source_manifest.csv"))
labels <- c(parent="Selected parent education (+1 year)",z_math="PAES math (+1 SD)",z_verbal="PAES verbal (+1 SD)")
rows <- vapply(names(labels),function(t) {
  cells <- vapply(c("father","mother"),function(p) {
    tt <- if(t=="parent") paste0(p,"_years") else t
    z <- ct[model==p & term==tt]; stopifnot(nrow(z)==1L)
    sprintf('<td>%.5f (%.5f)</td>',z$estimate,z$std_error)
  },character(1))
  paste0('<tr><th>',labels[t],'</th>',paste(cells,collapse=''),'</tr>')
},character(1))
rows <- c(rows,paste0('<tr><th>R2</th>',paste(vapply(c("father","mother"),function(p)
  sprintf('<td>%.4f</td>',sums[model==p,r2]),character(1)),collapse=''),'</tr>'))
comp_rows <- vapply(seq_len(nrow(comparison)),function(i) {
  r <- comparison[i]
  sprintf('<tr><th>%s</th><td>%.5f (%.5f)</td><td>%.5f (%.5f)</td><td>%.1f%%</td><td>%.4f</td><td>%.4f</td></tr>',
    r$model,r$before_estimate,r$before_se,r$paes_estimate,r$paes_se,r$coefficient_reduction_pct,r$before_r2,r$paes_r2)
},character(1))
raw <- vapply(seq_len(nrow(ct)),function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%.6f</td><td>%.6f</td><td>%s</td></tr>',
  ct$model[i],ct$term[i],ct$estimate[i],ct$std_error[i],ifelse(ct$p_value[i]<.001,"&lt;0.001",sprintf('%.4g',ct$p_value[i]))),character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Log projected income: parent education and PAES</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1200px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Log projected income: each parent plus PAES scores</h1>',
 '<p>Two separate OLS regressions: natural log of projected monthly CLP income on an intercept, selected parent schooling years, standardized entrance math and verbal. Both scores enter linearly in their existing within-admission-year SD units. The other parent, VA and further baseline controls are omitted.</p>',
 sprintf('<p>Identical sample to the parent-only regressions: %s HE enrollees and %s attended-school clusters. Coefficients in natural-log points; school-clustered CR1 SEs in parentheses and t(G-1) inference.</p>',format(nrow(x),big.mark=','),uniqueN(x$most_time_RBD)),
 '<table><tr><th></th><th>Father + PAES</th><th>Mother + PAES</th></tr>',rows,'</table>',
 '<h2>Parent coefficients before and after adding PAES</h2><table><tr><th>Parent</th><th>Parent only (SE)</th><th>With PAES (SE)</th><th>Coefficient reduction</th><th>Previous R2</th><th>New R2</th></tr>',comp_rows,'</table>',
 '<p>The sample retains the earlier complete-baseline and VA-coverage restrictions for comparability, although those variables are not included here. Preserve all income projections, parent imputations and original score normalization; no new exclusions. These are conditional associations in projected program/institution income, not realized earnings or causal parent-education returns. Coefficient attenuation is not a causal mediation decomposition.</p>',
 '<p>Checks passed: common source hash and sample size, positive income and natural-log identity, finite original scores, exactly four parameters, OLS normal equations, independent school-clustered covariance, and nested-model R2.</p>',
 '<details><summary>All coefficients</summary><table><tr><th>Model</th><th>Term</th><th>Estimate</th><th>SE</th><th>p-value</th></tr>',raw,'</table></details></body></html>')
writeLines(html,file.path(report,"regressions.html"),useBytes=TRUE)
print(ct);print(sums);print(comparison);print(rbindlist(checks))
