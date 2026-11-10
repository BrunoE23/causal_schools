# Bivariate log projected-income regressions, one parent's years at a time.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset=getwd())
task <- "log_projected_income_parent_only"
clean <- file.path(repo,"data/clean",task); report <- file.path(repo,"output/reports",task)
for (p in c(clean,report)) dir.create(p,recursive=TRUE,showWarnings=FALSE)
input <- file.path(repo,"data/clean/projected_income_parent_institution_va_interactions/analysis_data.rds")
stopifnot(file.exists(input),file.info(input)$size < 100e6)
hash <- unname(tools::md5sum(input))
x <- as.data.table(readRDS(input))[, .(mrun,most_time_RBD,cohort_gr8,he_enrollment,
  father_years,mother_years,projected_income_clp,log_projected_income)]
stopifnot(nrow(x)==463033L,!anyDuplicated(x$mrun),all(x$he_enrollment==1L),
  all(x$projected_income_clp>0),max(abs(x$log_projected_income-log(x$projected_income_clp)))<1e-10,
  all(is.finite(x$father_years)),all(is.finite(x$mother_years)))
models <- coefs <- stats <- checks <- formulas <- list()
for (parent in c("father","mother")) {
  term <- paste0(parent,"_years")
  fml <- reformulate(term,response="log_projected_income")
  fit <- feols(fml,x,vcov=~most_time_RBD,
    ssc=ssc(K.adj=TRUE,G.adj=TRUE,G.df="min",t.df="min"),notes=FALSE)
  X <- model.matrix(fml,x); b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  N <- nrow(x); K <- ncol(X); G <- uniqueN(x$most_time_RBD)
  stopifnot(K==2L,nobs(fit)==N,identical(names(b),colnames(X)))
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
rows <- vapply(c("father","mother"),function(p) {
  z <- ct[model==p & term==paste0(p,"_years")]; ss <- sums[model==p]
  sprintf('<tr><th>%s</th><td>%.5f</td><td>%.5f</td><td>[%.5f, %.5f]</td><td>%s</td><td>%.2f%%</td><td>%.4f</td></tr>',
    p,z$estimate,z$std_error,z$lower95,z$upper95,ifelse(z$p_value<.001,"&lt;0.001",sprintf('%.4g',z$p_value)),ss$pct_geometric_mean_per_year,ss$r2)
},character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Log projected income and parent schooling</title>',
 '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1200px;margin:30px auto;padding:0 24px}table{border-collapse:collapse;width:100%;margin:20px 0}th,td{padding:10px;border-bottom:1px solid #ddd;text-align:right}th:first-child{text-align:left}</style></head><body>',
 '<h1>Log projected income on each parent schooling measure</h1>',
 '<p>Two separate OLS regressions: natural log of projected monthly CLP income on an intercept and father years, or on an intercept and mother years. No other controls, interactions or instruments.</p>',
 sprintf('<p>Both models use the same %s HE enrollees and %s attended-school clusters as the earlier projected-income analysis. Standard errors use attended-school CR1 and t(G-1) inference.</p>',format(nrow(x),big.mark=','),uniqueN(x$most_time_RBD)),
 '<table><tr><th>Parent</th><th>Log-point coefficient/year</th><th>Clustered SE</th><th>95% CI</th><th>p-value</th><th>Geometric-mean income change/year</th><th>R2</th></tr>',rows,'</table>',
 '<p>The percentage column is 100 x (exp(coefficient)-1), referring to the model-implied geometric mean. Income is projected from the first HE program/institution, not realized individual earnings. These are unadjusted associations.</p>',
 '<p>The common sample inherits the earlier complete-PAES/full-baseline and school-VA availability restrictions, although none of those variables enters these bivariate regressions. Preserve existing parent-year imputations and income values. No new exclusions, imputation, normalization or trimming.</p>',
 '<p>Checks passed: common sample, natural-log identity, exactly two parameters per model, OLS normal equations, independent school-clustered covariance reconstruction and unchanged source hash.</p></body></html>')
writeLines(html,file.path(report,"regressions.html"),useBytes=TRUE)
print(ct); print(sums); print(rbindlist(checks))
