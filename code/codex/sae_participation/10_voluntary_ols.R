# One joint descriptive LPM, using every term in the constructed full model.
# Reuse the cached design; no new feature selection or prediction exercise.
source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
setFixest_nthreads(4L)
ols_out <- file.path(out,'voluntary_ols')
dir.create(ols_out,recursive=TRUE,showWarnings=FALSE)
write_audit <- function(z,name) fwrite(z,file.path(ols_out,paste0(name,'.csv')))
log_message('Loading cached full-model training and evaluation designs.')
old <- readRDS(file.path(out,'voluntary/models/full.rds'))
evaluation <- readRDS(file.path(out,'voluntary/evaluation_frame.rds'))
fml <- old$fml_all$linear
required <- unique(c(all.vars(fml),'origin_rbd'))
stopifnot(!is.null(old$data),all(required %in% names(old$data)),all(required %in% names(evaluation$data)))
train_n <- nrow(old$data); test_n <- nrow(evaluation$data)
stopifnot(length(intersect(old$data$origin_rbd,evaluation$data$origin_rbd))==0L)
d <- rbindlist(list(old$data[,required,with=FALSE],evaluation$data[,required,with=FALSE]))
rm(old,evaluation); invisible(gc())
stopifnot(nrow(d)==313813L,all(d$applied_sae %in% 0:1),sum(d$applied_sae)==45947L)
log_message('Estimating one joint OLS regression with school-clustered standard errors.')
fit <- feols(fml,data=d,vcov=~origin_rbd,notes=FALSE,lean=FALSE)
stopifnot(nobs(fit)==nrow(d))
ct <- as.data.table(coeftable(fit),keep.rownames='term')
setnames(ct,c('term','estimate','se_cluster_school','t','p_value'))
ci <- confint(fit)
ct[,`:=`(estimate_pp=100*estimate,se_pp=100*se_cluster_school,
  ci_low_pp=100*ci[match(term,rownames(ci)),1],ci_high_pp=100*ci[match(term,rownames(ci)),2])]
ct[,kind:=fcase(grepl('__z$',term),'Linear',grepl('__sq$',term),'Quadratic',grepl('__missing$',term),'Missingness',default='Context')]
ct[,feature:=sub('__(z|sq|missing)$','',term)]
cw <- fread('code/codex/sae_participation/parent_crosswalk.csv')
parent_names <- setNames(cw$label,paste0('parent_',cw$feature))
admin_names <- c(gpa='GPA',attendance='Attendance',z_gpa='GPA relative to peers',z_attendance='Attendance relative to peers',
  gap_math='GPA minus baseline SIMCE math (relative z-scores)',gap_language='GPA minus baseline SIMCE language (relative z-scores)')
label_feature <- function(x) {
  if(x %in% names(parent_names)) return(unname(parent_names[x]))
  if(grepl('_g[5-8]$',x)) {
    stem <- sub('_g[5-8]$','',x)
    if(stem %in% names(admin_names)) return(paste0(unname(admin_names[stem]),', grade ',sub('.*_g','',x)))
  }
  if(x=='age') return('Age')
  if(x=='same_simce_origin') return('Same grade-4 and grade-8 school')
  if(x=='parent_record') return('Parent questionnaire record linked')
  x
}
ct[,label:=vapply(feature,label_feature,character(1))]
write_audit(ct,'coefficients')
omitted <- data.table(term=fit$collin.var)
write_audit(omitted,'omitted_collinear')
res <- resid(fit); fitted <- fitted(fit); y <- d$applied_sae
r2 <- 1-sum(res^2)/sum((y-mean(y))^2)
summary <- data.table(students=nrow(d),schools=uniqueN(d$origin_rbd),applicants=sum(y),application_rate=mean(y),
  r_squared=r2,adjusted_r_squared=as.numeric(fitstat(fit,'ar2')[[1]]),estimated_coefficients=nrow(ct),
  original_train_n=train_n,original_test_n=test_n,residual_df=nrow(d)-nrow(ct))
write_audit(summary,'model_summary')
writeLines(paste(deparse(fml),collapse=' '),file.path(ols_out,'formula.txt'))

# Check OLS normal equations and reconstruct the actual clustered covariance
# from school-summed X'e, with the same fixest finite-sample correction.
# Do not replace regression-derived inference with a residual-SD proxy.
X <- model.matrix(fit,type='rhs',collin.rm=TRUE)
X <- X[,names(coef(fit)),drop=FALSE]
score_by_school <- rowsum(X*res,group=d$origin_rbd,reorder=FALSE)
xx_inv <- solve(crossprod(X))
G <- uniqueN(d$origin_rbd); N <- nrow(d); K <- ncol(X)
vcov_independent <- (G/(G-1))*((N-1)/(N-K))*xx_inv %*% crossprod(score_by_school) %*% xx_inv
se_independent <- sqrt(diag(vcov_independent))
checks <- data.table(check=c('full sample retained','binary outcome and applicant count','OLS normal equations',
  'independent school-clustered standard errors','independent R-squared','all 39 parent responses represented',
  'all grade 5-8 linear and quadratic constructs represented'),
  passed=c(nobs(fit)==313813L,all(y %in% 0:1)&&sum(y)==45947L,
    max(abs(crossprod(X,res)))/N<1e-8,
    max(abs(se_independent-ct$se_cluster_school))<1e-7,
    abs(r2-as.numeric(fitstat(fit,'r2')[[1]]))<1e-10,
    all(paste0(names(parent_names),'__z') %in% ct$term),
    all(unlist(lapply(names(admin_names),function(v) c(paste0(v,'_g',5:8,'__z'),paste0(v,'_g',5:8,'__sq')))) %in% ct$term)))
write_audit(checks,'verification')
stopifnot(all(checks$passed))
# Store the actual regression object; the design is reproducible from the two
# source caches above. Prediction deployment is not the purpose of this fit.
fit$call$data <- quote(d)
saveRDS(fit,file.path(ols_out,'model.rds'))
rm(X,score_by_school,xx_inv,vcov_independent); invisible(gc())

source('code/codex/sae_participation/11_voluntary_ols_report.R')
