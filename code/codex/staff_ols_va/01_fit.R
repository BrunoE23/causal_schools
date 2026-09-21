# Fixed 14-variable joint OLS; no selection, penalty or cross-validation.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
root <- normalizePath('.',winslash='/')
out <- file.path(root,'data/clean/staff_ols_va')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,'coefficients.csv')) && !'--overwrite' %in% commandArgs()) stop('Use --overwrite for an intentional refresh.')
prior <- file.path(root,'data/clean/staff_lasso_va')
paths <- c(predictors=file.path(prior,'school_predictors.csv'),dictionary=file.path(prior,'predictor_dictionary.csv'),
  outcomes=file.path(prior,'outcome_dictionary.csv'),
  va=file.path(root,'output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv'))
hash <- tools::md5sum(paths)
features <- c('teacher__va_students_per_staff','teacher__UG_HIGH_PREMIUM','teacher__age_under35_share',
  'counselor__staff_per1000_va_students','counselor__ROLE_SPECIFIC_QUALIFICATION',
  'leadership__primary_headcount_mean','leadership__ANY_HIGH_PREMIUM','leadership__ROLE_SPECIFIC_MAGISTER',
  'school__composition_math_mean','school__composition_math_sd','school__composition_income_decile_mean',
  'school__log_public_funding_level','school__log_va_students','school__has_tp_or_artistic')
nuisance <- unlist(lapply(c('teacher','counselor','leadership'),function(role)
  paste0(role,c('__absent_all_years','__roster_incomplete'))))
x <- fread(paths[['predictors']]); setorder(x,RBD)
dd <- fread(paths[['dictionary']])[match(features,FEATURE)]
outs <- fread(paths[['outcomes']])
va <- fread(paths[['va']],select=c('school_rbd','analysis_sample','outcome','controlled_value_added_eb_centered_student'))[
  analysis_sample=='All' & outcome %in% outs$OUTCOME]
stopifnot(!anyDuplicated(x$RBD),!anyDuplicated(va,by=c('school_rbd','outcome')),nrow(x)==3682L,
  all(c(features,nuisance) %in% names(x)),identical(dd$FEATURE,features))
coeff <- list(); audit <- list(); models <- list(); prep <- list(); samples <- list(); alias <- list()
for(o in outs$OUTCOME) {
  v <- va[outcome==o][match(x$RBD,school_rbd)]
  idx <- which(is.finite(v$controlled_value_added_eb_centered_student))
  raw <- as.matrix(x[idx,..features]); raw[!is.finite(raw)] <- NA_real_
  y <- v$controlled_value_added_eb_centered_student[idx]
  # One SD among observed values in this outcome's school sample (sample SD).
  mu <- colMeans(raw,na.rm=TRUE); sig <- apply(raw,2,sd,na.rm=TRUE)
  med <- apply(raw,2,median,na.rm=TRUE)
  stopifnot(all(is.finite(sig)),all(sig>0))
  missing <- is.na(raw)
  for(j in seq_along(features)) raw[missing[,j],j] <- med[j]
  z <- sweep(sweep(raw,2,mu,'-'),2,sig,'/')
  controls <- as.matrix(x[idx,..nuisance])
  control_missing <- 1.0*!is.finite(controls)
  colnames(control_missing) <- paste0(nuisance,'__missing')
  controls[!is.finite(controls)] <- 0
  miss <- 1.0*missing; colnames(miss) <- paste0(features,'__missing')
  full <- cbind('(Intercept)'=1,z,miss,controls,control_missing)
  # Drop only redundant nuisance columns. All 14 focal variables must be identified.
  keep <- integer(); dropped <- character()
  for(j in seq_len(ncol(full))) {
    rank <- qr(full[,c(keep,j),drop=FALSE],tol=1e-9)$rank
    if(rank>length(keep)) keep <- c(keep,j) else {
      if(j<=15L) stop('A focal coefficient is not identified: ',colnames(full)[j])
      dropped <- c(dropped,colnames(full)[j])
    }
  }
  design <- full[,keep,drop=FALSE]
  ym <- mean(y); ys <- sd(y); yy <- (y-ym)/ys
  fit <- lm.fit(design,yy)
  bread <- chol2inv(chol(crossprod(design)))
  leverage <- rowSums((design %*% bread)*design)
  stopifnot(fit$rank==ncol(design),max(leverage)<1+1e-8)
  # HC1 remains defined with singleton nuisance cells (HC3 has 0/0 at h=1).
  meat_rows <- design*as.numeric(fit$residuals)
  vcov <- (length(y)/fit$df.residual)*bread %*% crossprod(meat_rows) %*% bread
  se <- sqrt(diag(vcov))
  stopifnot(all(is.finite(se)),max(abs(crossprod(design,fit$residuals)))<1e-7)
  coeff[[o]] <- data.table(OUTCOME=o,FEATURE=colnames(design),BETA_SD=as.numeric(fit$coefficients),
    SE_HC1=se,FOCAL=colnames(design) %in% features)
  audit[[o]] <- data.table(OUTCOME=o,N=length(y),P=ncol(design),DF=fit$df.residual,
    R2=1-sum(fit$residuals^2)/sum((yy-mean(yy))^2),
    ADJ_R2=1-(sum(fit$residuals^2)/fit$df.residual)/(sum((yy-mean(yy))^2)/(length(y)-1)),
    MAX_LEVERAGE=max(leverage),N_UNIT_LEVERAGE=sum(leverage>1-1e-8),CONDITION_NUMBER=kappa(design,exact=TRUE),Y_MEAN=ym,Y_SD=ys)
  prep[[o]] <- data.table(OUTCOME=o,FEATURE=features,CENTER=mu,SCALE=sig,MEDIAN=med,N_OBSERVED=colSums(!missing))
  samples[[o]] <- data.table(OUTCOME=o,RBD=x$RBD[idx],Y=y,FITTED=ym+ys*fit$fitted.values)
  alias[[o]] <- data.table(OUTCOME=rep(o,length(dropped)),FEATURE=dropped,REASON='Constant or linearly redundant nuisance control')
  models[[o]] <- list(fit=fit,vcov_hc1=vcov,columns=colnames(design))
}
fwrite(rbindlist(coeff),file.path(out,'coefficients.csv'))
fwrite(rbindlist(audit),file.path(out,'model_summary.csv'))
fwrite(rbindlist(prep),file.path(out,'normalization.csv'))
fwrite(rbindlist(samples),file.path(out,'school_predictions.csv.gz'))
fwrite(rbindlist(alias),file.path(out,'redundant_nuisance_controls.csv'))
fwrite(dd,file.path(out,'focal_dictionary.csv'))
fwrite(outs,file.path(out,'outcome_dictionary.csv'))
saveRDS(models,file.path(out,'ols_models.rds'))
stopifnot(identical(hash,tools::md5sum(paths)))
fwrite(data.table(SOURCE=names(paths),PATH=unname(paths),MD5=unname(hash)),file.path(out,'source_manifest.csv'))
print(rbindlist(audit)[,.(OUTCOME,N,P,R2,MAX_LEVERAGE)])
cat('All 12 joint OLS models estimated; source files unchanged.\n')
