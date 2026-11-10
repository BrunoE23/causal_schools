# Log projected income: separate parent-by-institution-VA interactions, controlling PAES.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset=getwd())
task <- "log_income_separate_parent_paes_va"
clean <- file.path(repo,"data/clean",task); report <- file.path(repo,"output/reports",task)
for (p in c(clean,report)) dir.create(p,recursive=TRUE,showWarnings=FALSE)
input <- file.path(repo,"data/clean/projected_income_parent_institution_va_interactions/analysis_data.rds")
stopifnot(file.exists(input),file.info(input)$size < 100e6)
hash <- unname(tools::md5sum(input))
x <- as.data.table(readRDS(input))[, .(mrun,most_time_RBD,cohort_gr8,he_enrollment,
  father_years,mother_years,z_math,z_verbal,high_inst_m1_va,projected_income_clp,log_projected_income)]
stopifnot(nrow(x)==463033L,!anyDuplicated(x$mrun),all(x$he_enrollment==1L),
  all(x$projected_income_clp>0),max(abs(x$log_projected_income-log(x$projected_income_clp)))<1e-10,
  all(is.finite(x$father_years)),all(is.finite(x$mother_years)),all(is.finite(x$z_math)),all(is.finite(x$z_verbal)))
stopifnot(all(is.finite(x$high_inst_m1_va)))
scaling <- x[, .(father_mean=mean(father_years),mother_mean=mean(mother_years),va_mean=mean(high_inst_m1_va),va_unit=.10)]
x[, `:=`(father_c=father_years-scaling$father_mean,mother_c=mother_years-scaling$mother_mean,
  va10=(high_inst_m1_va-scaling$va_mean)/.10)]
x[, `:=`(father_va=father_c*va10,mother_va=mother_c*va10)]
models <- coefs <- stats <- checks <- formulas <- list()
for (parent in c("father","mother")) {
  term <- paste0(parent,"_c")
  fml <- reformulate(c(term,"z_math","z_verbal","va10",paste0(parent,"_va")),response="log_projected_income")
  other <- if(parent=="father") "mother" else "father"
  stopifnot(!any(grepl(other,all.vars(fml))))
  fit <- feols(fml,x,vcov=~most_time_RBD,
    ssc=ssc(K.adj=TRUE,G.adj=TRUE,G.df="min",t.df="min"),notes=FALSE)
  X <- model.matrix(fml,x); b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  N <- nrow(x); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
  stopifnot(K==6L,nobs(fit)==N,identical(names(b),colnames(X)))
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
    pct_geometric_mean_per_year_at_mean_va=100*expm1(b[term]),mean_log_income=mean(x$log_projected_income))
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
fwrite(scaling,file.path(clean,"scaling.csv"))
saveRDS(list(formulas=formulas,scaling=scaling,ids=x$mrun,cluster="most_time_RBD",source=input,
  sample="Same complete-baseline and VA-covered HE enrollees as prior projected-income regressions; no additional restrictions"),
  file.path(clean,"specification.rds"))
previous <- file.path(repo,"data/clean/log_projected_income_parent_paes")
comparison_inputs <- file.path(previous,c("model_summary.csv","specification.rds","source_manifest.csv"))
comparison_hashes <- unname(tools::md5sum(comparison_inputs))
old_stats <- fread(comparison_inputs[1L]); old_spec <- readRDS(comparison_inputs[2L]); old_manifest <- fread(comparison_inputs[3L])
stopifnot(identical(x$mrun,old_spec$ids),all(old_stats$N==nrow(x)),old_manifest[path==input,md5]==hash)
comparison <- merge(sums[,.(model,r2_with_va_interaction=r2)],old_stats[,.(model,r2_parent_paes=r2)],by="model")
stopifnot(all(comparison$r2_with_va_interaction>=comparison$r2_parent_paes),
  identical(comparison_hashes,unname(tools::md5sum(comparison_inputs))))
fwrite(comparison,file.path(clean,"r2_comparison.csv"))
fwrite(rbindlist(list(data.table(path=input,bytes=file.info(input)$size,md5=hash),
  data.table(path=comparison_inputs,bytes=file.info(comparison_inputs)$size,md5=comparison_hashes))),file.path(clean,"source_manifest.csv"))
# Parental gradients across student-weighted institution VA quartiles.
qs <- quantile(x$high_inst_m1_va,c(.25,.5,.75),names=FALSE,type=1)
slopes <- rbindlist(lapply(c("father","mother"),function(p) {
  b <- coef(models[[p]]); V <- vcov(models[[p]])
  rbindlist(lapply(seq_along(qs),function(j) {
    a <- setNames(rep(0,length(b)),names(b)); a[paste0(p,"_c")] <- 1
    a[paste0(p,"_va")] <- (qs[j]-scaling$va_mean)/.10
    est <- sum(a*b); se_eff <- sqrt(as.numeric(crossprod(a,V%*%a)))
    data.table(model=p,va_percentile=c(25L,50L,75L)[j],va=qs[j],estimate=est,std_error=se_eff,
      lower95=est-qt(.975,G-1)*se_eff,upper95=est+qt(.975,G-1)*se_eff)
  }))
}))
fwrite(slopes,file.path(clean,"parent_slopes.csv"))
labels <- c(parent="Selected parent education (+1 year), at mean VA",interaction="Parent education x institution VA (+1 year x +10 pp)",
  va10="Institution VA (+10 pp), at mean parent education",z_math="PAES math (+1 SD)",z_verbal="PAES verbal (+1 SD)")
rows <- vapply(names(labels),function(t) {
  cells <- vapply(c("father","mother"),function(p) {
    tt <- if(t=="parent") paste0(p,"_c") else if(t=="interaction") paste0(p,"_va") else t
    z <- ct[model==p & term==tt]; stopifnot(nrow(z)==1L)
    sprintf('<td>%.5f (%.5f)</td>',z$estimate,z$std_error)
  },character(1))
  paste0('<tr><th>',labels[t],'</th>',paste(cells,collapse=''),'</tr>')
},character(1))
pformat <- function(p) ifelse(p<.001,"&lt;0.001",sprintf('%.4f',p))
rows <- c(rows,paste0('<tr><th>Interaction p-value</th>',paste(vapply(c("father","mother"),function(p)
  paste0('<td>',pformat(ct[model==p & term==paste0(p,"_va"),p_value]),'</td>'),character(1)),collapse=''),'</tr>'),
  paste0('<tr><th>R2</th>',paste(vapply(c("father","mother"),function(p)
  sprintf('<td>%.4f</td>',sums[model==p,r2]),character(1)),collapse=''),'</tr>'))
slrows <- vapply(seq_len(nrow(slopes)),function(i) {
  r <- slopes[i]
  sprintf('<tr><td>%s</td><td>%s</td><td>%.2f</td><td>%.5f (%.5f)</td><td>[%.5f, %.5f]</td></tr>',
    r$model,r$va_percentile,100*r$va,r$estimate,r$std_error,r$lower95,r$upper95)
},character(1))
raw <- vapply(seq_len(nrow(ct)),function(i) sprintf('<tr><td>%s</td><td>%s</td><td>%.6f</td><td>%.6f</td><td>%s</td></tr>',
  ct$model[i],ct$term[i],ct$estimate[i],ct$std_error[i],pformat(ct$p_value[i])),character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Log projected income: parent x institution VA</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1200px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Log projected income: one parent x premium-institution VA, controlling PAES</h1>',
 '<p>Separate OLS regressions for father and mother: selected parent schooling, attended-school premium-institution VA, their interaction, and linear standardized PAES math/verbal. The other parent is omitted. Include an intercept and all lower-order terms; no IV or other baseline controls.</p>',
 sprintf('<p>Same %s HE enrollees and %s attended-school clusters as the previous PAES-adjusted models. Coefficients in natural-log points; school-clustered CR1 SEs in parentheses and t(G-1) inference. VA units: +10 percentage points. Parent schooling and VA are centered at their student-sample means.</p>',format(nrow(x),big.mark=','),uniqueN(x$most_time_RBD)),
 '<table><tr><th></th><th>Father model</th><th>Mother model</th></tr>',rows,'</table>',
 '<p>Use the existing 2017-2020 All-sample EB institution VA matched to attended high school (high_inst_m1_va). Preserve parent imputations, PAES within-admission-year scales and projected-income values. The common sample inherits complete-baseline and VA-coverage restrictions; no new exclusions or trimming.</p>',
 '<h2>Parent gradients across institution VA</h2><table><tr><th>Parent</th><th>VA percentile</th><th>Original centered VA (pp)</th><th>Log-income slope/year (SE)</th><th>95% CI</th></tr>',slrows,'</table>',
 '<p>These are conditional associations in projected program/institution income, not realized earnings or causal effects. Estimated VA is treated as fixed and is not cross-fitted. Checks passed: identical prior sample IDs and source hash, finite VA/scores, exactly six parameters, correct single-parent design, OLS normal equations, independently reconstructed school-clustered covariance and nested-model R2.</p>',
 '<details><summary>All coefficients</summary><table><tr><th>Model</th><th>Term</th><th>Estimate</th><th>SE</th><th>p-value</th></tr>',raw,'</table></details></body></html>')
writeLines(html,file.path(report,"regressions.html"),useBytes=TRUE)
print(ct);print(sums);print(comparison);print(slopes);print(rbindlist(checks))
