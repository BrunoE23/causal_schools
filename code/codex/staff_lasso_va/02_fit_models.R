suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
root <- normalizePath('.',winslash='/')
out <- file.path(root,'data/clean/staff_lasso_va')
.libPaths(c(file.path(out,'r_library'),.libPaths()))
stopifnot(requireNamespace('glmnet',quietly=TRUE))
glmnet::glmnet.control(fdev=0,devmax=1)
source(file.path(root,'code/codex/staff_lasso_va/lasso_helpers.R'))
if(file.exists(file.path(out,'lasso_coefficients.csv')) && !'--overwrite' %in% commandArgs())
  stop('Model outputs exist; pass --overwrite to rerun.')
manifest <- fread(file.path(out,'input_manifest.csv'))
stopifnot(identical(unname(tools::md5sum(manifest$PATH)),manifest$MD5))
xp <- fread(file.path(out,'school_predictors.csv')); setorder(xp,RBD)
dd <- fread(file.path(out,'predictor_dictionary.csv'))
folds <- fread(file.path(out,'school_folds.csv'))[match(xp$RBD,RBD)]
outs <- fread(file.path(out,'outcome_dictionary.csv'))
va <- fread(manifest[SOURCE=='va',PATH],select=c('school_rbd','analysis_sample','outcome',
  'controlled_value_added_eb_centered_student','controlled_value_added_centered_student','eb_reliability'))[
  analysis_sample=='All' & outcome %in% outs$OUTCOME]
setnames(va,c('school_rbd','outcome','controlled_value_added_eb_centered_student',
  'controlled_value_added_centered_student'),c('RBD','OUTCOME','Y','Y_RAW'))
stopifnot(!anyDuplicated(va,by=c('RBD','OUTCOME')))
lambda_joint <- exp(seq(log(1),log(.0005),length.out=70))
# A first audit found the small school-only model sometimes chose the weakest
# grid penalty. Extend that baseline to the unpenalized endpoint so the baseline
# is not disadvantaged by a truncated tuning range. Joint optima were interior.
lambda_school <- c(lambda_joint,exp(seq(log(.0005),log(.000001),length.out=21))[-1],0)
# Same preassigned school folds across outcomes/specifications. Inner outer-fit
# assignments are generated without outcomes or predictor values.
inner <- sapply(1:5,function(k) {
  set.seed(20260920L+k)
  sample(rep(1:5,length.out=nrow(xp)))
})
saveRDS(list(lambda_joint=lambda_joint,lambda_school=lambda_school,folds=folds,inner=inner,seed=20260920L,
  glmnet_version=as.character(packageVersion('glmnet')),session=capture.output(sessionInfo())),
  file.path(out,'run_settings.rds'))
coef_rows <- list(); cv_rows <- list(); pred_rows <- list(); stats_rows <- list()
selection_rows <- list(); audit_rows <- list(); model_list <- list(); design_rows <- list()
for (outcome in outs$OUTCOME) {
  ytable <- va[OUTCOME==outcome][match(xp$RBD,RBD)]
  ok <- which(is.finite(ytable$Y))
  y <- ytable$Y[ok]; ids <- xp$RBD[ok]; outer <- folds$OUTER_FOLD[ok]
  design_rows[[outcome]] <- data.table(RBD=ids,OUTCOME=outcome,Y=y,Y_RAW=ytable$Y_RAW[ok],
    EB_RELIABILITY=ytable$eb_reliability[ok],OUTER_FOLD=outer)
  for (spec in c('school_only','joint')) {
    lambda <- if(spec=='school_only') lambda_school else lambda_joint
    message(format(Sys.time(),'%H:%M:%S'),' ',outcome,' / ',spec)
    features <- dd[if(spec=='school_only') ROLE=='school' else rep(TRUE,.N),FEATURE]
    raw <- as.matrix(xp[ok,..features])
    tuning <- sl_tune(raw,y,folds$FINAL_INNER_FOLD[ok],lambda)
    cv_rows[[length(cv_rows)+1L]] <- cbind(OUTCOME=outcome,SPEC=spec,OUTER_FOLD=0L,tuning$curve)
    final <- sl_path(raw,y,lambda)
    model_list[[paste(outcome,spec,sep=':')]] <- list(model=final,tuning=tuning,ids=ids,features=features)
    for(rule in names(tuning$index)) {
      ix <- tuning$index[[rule]]; lam <- lambda[ix]
      cc <- sl_coefficients(final,lam)
      coef_rows[[length(coef_rows)+1L]] <- cbind(OUTCOME=outcome,SPEC=spec,RULE=rule,LAMBDA=lam,cc)
      kk <- sl_kkt(final,y,lam)
      if(kk>2e-4) stop('KKT convergence audit failed: ',kk)
      audit_rows[[length(audit_rows)+1L]] <- data.table(OUTCOME=outcome,SPEC=spec,RULE=rule,
        OUTER_FOLD=0L,LAMBDA=lam,LAMBDA_INDEX=ix,KKT_MAX=kk,
        N_TRAIN=length(y),N_TEST=0L,N_SELECTED=sum(cc$SELECTED),N_CONSTANT=sum(cc$TRAIN_CONSTANT))
    }
    for (k in 1:5) {
      tr <- which(outer!=k); te <- which(outer==k)
      tune <- sl_tune(raw[tr,,drop=FALSE],y[tr],inner[ok,k][tr],lambda)
      cv_rows[[length(cv_rows)+1L]] <- cbind(OUTCOME=outcome,SPEC=spec,OUTER_FOLD=k,tune$curve)
      fit <- sl_path(raw[tr,,drop=FALSE],y[tr],lambda)
      for (rule in names(tune$index)) {
        ix <- tune$index[[rule]]; lam <- lambda[ix]
        prediction <- as.numeric(sl_predict(fit,raw[te,,drop=FALSE],lam))
        pred_rows[[length(pred_rows)+1L]] <- data.table(RBD=ids[te],OUTCOME=outcome,SPEC=spec,
          RULE=rule,OUTER_FOLD=k,Y=y[te],PRED=prediction,NULL_PRED=mean(y[tr]))
        cc <- sl_coefficients(fit,lam)
        selection_rows[[length(selection_rows)+1L]] <- cbind(OUTCOME=outcome,SPEC=spec,RULE=rule,OUTER_FOLD=k,cc)
        kk <- sl_kkt(fit,y[tr],lam)
        if(kk>2e-4) stop('Outer KKT audit failed: ',kk)
        audit_rows[[length(audit_rows)+1L]] <- data.table(OUTCOME=outcome,SPEC=spec,RULE=rule,
          OUTER_FOLD=k,LAMBDA=lam,LAMBDA_INDEX=ix,KKT_MAX=kk,N_TRAIN=length(tr),N_TEST=length(te),
          N_SELECTED=sum(cc$SELECTED),N_CONSTANT=sum(cc$TRAIN_CONSTANT))
      }
    }
  }
  # Checkpoint within owned output folder; these are not source inputs.
  saveRDS(list(coefficients=coef_rows,cv=cv_rows,predictions=pred_rows,audit=audit_rows),
          file.path(out,'checkpoint.rds'))
}
co <- rbindlist(coef_rows); pred <- rbindlist(pred_rows)
stats <- pred[,.(N=.N,R2_OOF=1-sum((Y-PRED)^2)/sum((Y-mean(Y))^2),
  RMSE_OOF=sqrt(mean((Y-PRED)^2)),R2_VS_TRAIN_MEAN=1-sum((Y-PRED)^2)/sum((Y-NULL_PRED)^2),
  NULL_RMSE=sqrt(mean((Y-NULL_PRED)^2))),by=.(OUTCOME,SPEC,RULE)]
stats <- merge(stats,co[,.(N_SELECTED=sum(SELECTED),N_NONCONSTANT=sum(!TRAIN_CONSTANT),
  LAMBDA=unique(LAMBDA)),by=.(OUTCOME,SPEC,RULE)],by=c('OUTCOME','SPEC','RULE'))
selections <- rbindlist(selection_rows)
frequency <- selections[,.(N_OUTER_FOLDS=.N,SELECTION_FREQUENCY=mean(SELECTED),
  POSITIVE_FREQUENCY=mean(BETA_SD>0),NEGATIVE_FREQUENCY=mean(BETA_SD<0)),by=.(OUTCOME,SPEC,RULE,FEATURE)]
stopifnot(all(frequency$N_OUTER_FOLDS==5),!anyDuplicated(pred,by=c('RBD','OUTCOME','SPEC','RULE')))
fwrite(co,file.path(out,'lasso_coefficients.csv'))
fwrite(stats,file.path(out,'lasso_performance.csv'))
fwrite(rbindlist(cv_rows),file.path(out,'lasso_cv_curves.csv'))
fwrite(pred,file.path(out,'lasso_oof_predictions.csv.gz'))
fwrite(frequency,file.path(out,'lasso_selection_frequency.csv'))
fwrite(selections,file.path(out,'lasso_outer_coefficients.csv.gz'))
fwrite(rbindlist(audit_rows),file.path(out,'lasso_fit_audit.csv'))
fwrite(rbindlist(design_rows),file.path(out,'school_va_analysis.csv.gz'))
saveRDS(model_list,file.path(out,'lasso_final_models.rds'))
stopifnot(identical(unname(tools::md5sum(manifest$PATH)),manifest$MD5))
print(stats[RULE=='one_se',.(OUTCOME,SPEC,N,N_SELECTED,R2_OOF)])
cat('Finished nested CV and full-sample refits; all source hashes unchanged.\n')
