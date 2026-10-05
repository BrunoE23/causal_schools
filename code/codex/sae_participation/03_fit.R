source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
setFixest_nthreads(4L)
panel <- readRDS(file.path(out,'admin_panel.rds'))
parents <- readRDS(file.path(out,'parent_predictors.rds'))
dd <- merge(panel,parents[,!c('simce_year'),with=FALSE],by='mrun',all.x=TRUE)
dd <- dd[analysis_eligible==1L]
dd[is.na(parent_record),parent_record:=0L]
dd[!age %between% c(9,25),age:=NA_real_]
setorder(dd,mrun)
set.seed(20261005)
schools <- unique(dd[,.(origin_rbd)])[order(origin_rbd)]
schools[,split:=ifelse(runif(.N)<.8,'train','test')]
dd <- merge(dd,schools,by='origin_rbd',all.x=TRUE,sort=FALSE)
train <- which(dd$split=='train'); test <- which(dd$split=='test')
stopifnot(length(intersect(dd$origin_rbd[train],dd$origin_rbd[test]))==0L)
audit_write(schools,'school_split')
audit_write(dd[,.(students=.N,schools=uniqueN(origin_rbd),applicants=sum(applied_sae),application_rate=mean(applied_sae)),by=.(split,cohort_gr8,continuation_group)],'split_summary')
cw <- fread('code/codex/sae_participation/parent_crosswalk.csv')
parent_vars <- paste0('parent_',cw$feature)
admin_vars <- unlist(lapply(c('gpa','attendance','z_gpa','z_attendance'),function(v) paste0(v,'_g',5:8)))
gap_vars <- unlist(lapply(c('gap_math','gap_language'),function(v) paste0(v,'_g',5:8)))
numeric_vars <- c('age',admin_vars,gap_vars,'same_simce_origin','parent_record',parent_vars)
stopifnot(all(numeric_vars %in% names(dd)))
coverage <- rbindlist(lapply(numeric_vars,function(v) dd[,.(feature=v,students=.N,observed=sum(is.finite(get(v)))),by=cohort_gr8]))
audit_write(coverage,'predictor_coverage')
prep <- list(); value_terms <- list(); missing_terms <- list(); square_terms <- list()
for(v in numeric_vars) {
  x <- dd[[v]]; x[!is.finite(x)] <- NA_real_
  observed <- x[train][!is.na(x[train])]
  mu <- mean(observed); sig <- sd(observed); med <- median(observed)
  miss_name <- paste0(v,'__missing'); value_name <- paste0(v,'__z'); sq_name <- paste0(v,'__sq')
  miss <- as.integer(is.na(x)); set(dd,j=miss_name,value=miss)
  if(length(unique(miss[train]))>1L) missing_terms[[v]] <- miss_name
  active <- length(observed)>1L && is.finite(sig) && sig>0
  if(active) {
    x[is.na(x)] <- med
    zx <- (x-mu)/sig
    set(dd,j=value_name,value=zx)
    value_terms[[v]] <- value_name
    if(v %in% c('age',admin_vars,gap_vars)) {
      set(dd,j=sq_name,value=zx^2)
      square_terms[[v]] <- sq_name
    }
  }
  prep[[v]] <- data.table(feature=v,train_observed=length(observed),mean=mu,sd=sig,median=med,active=active)
}
audit_write(rbindlist(prep),'training_preprocessing')
for(v in c('origin_sector','gender','origin_region')) dd[is.na(get(v)),(v):=0L]
terms_for <- function(vars,squares=TRUE,missing=TRUE) unique(c(unlist(value_terms[vars]),if(squares) unlist(square_terms[vars]),if(missing) unlist(missing_terms[vars])))
context <- c('factor(cohort_gr8)*factor(origin_region)','factor(origin_sector)','factor(gender)',terms_for('age'))
baseline <- c(context,'factor(continuation_group)')
admin <- c(baseline,terms_for(admin_vars))
gaps <- c(admin,terms_for(gap_vars))
availability <- c(gaps,terms_for(c('same_simce_origin','parent_record')),unlist(missing_terms[parent_vars]))
full <- c(availability,terms_for(parent_vars,FALSE,FALSE))
admin_57 <- admin_vars[!grepl('_g8$',admin_vars)]
gap_57 <- gap_vars[!grepl('_g8$',gap_vars)]
specs <- list(context_only=context,baseline=baseline,admin_grades5_8=admin,plus_simce_gaps=gaps,
              plus_parent_availability=availability,plus_parent_answers=full,
              grades5_7_parent_answers=c(baseline,terms_for(admin_57),terms_for(gap_57),terms_for(c('same_simce_origin','parent_record')),unlist(missing_terms[parent_vars]),terms_for(parent_vars,FALSE,FALSE)))
metrics <- list(); coefficients <- list(); calibration <- list(); model_audit <- list(); predictions <- dd[test,.(mrun,origin_rbd,cohort_gr8,applied_sae,applied_regular,continuation_group,same_simce_origin)]
score_metrics <- function(y,p,reference) {
  stopifnot(length(y)==length(p),all(is.finite(p)),all(p>=0 & p<=1))
  n1 <- as.double(sum(y==1L)); n0 <- as.double(sum(y==0L))
  auc <- if(n1>0 && n0>0) (sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0) else NA_real_
  q <- pmin(pmax(p,1e-12),1-1e-12)
  data.table(n=length(y),prevalence=mean(y),predicted_mean=mean(p),auc=auc,brier=mean((y-p)^2),
             brier_r2=1-mean((y-p)^2)/mean((y-reference)^2),log_loss=-mean(y*log(q)+(1-y)*log1p(-q)))
}
reference <- mean(dd$applied_sae[train])
dir.create(file.path(out,'models'),showWarnings=FALSE)
for(name in names(specs)) {
  log_message('Fitting ',name,' (',length(specs[[name]]),' formula terms)')
  fml <- as.formula(paste('applied_sae ~',paste(unique(specs[[name]]),collapse=' + ')))
  fit <- feglm(fml,data=dd[train],family='binomial',vcov=~origin_rbd,notes=FALSE,warn=TRUE,glm.iter=100L)
  p <- as.numeric(predict(fit,newdata=dd[test],type='response'))
  stopifnot(nobs(fit)==length(train),all(is.finite(p)),isTRUE(fit$convStatus))
  predictions[,(name):=p]
  groupings <- list(overall=rep('All',length(test)),school=dd$continuation_group[test],cohort=as.character(dd$cohort_gr8[test]),same_school=as.character(dd$same_simce_origin[test]))
  for(group_type in names(groupings)) for(g in unique(na.omit(groupings[[group_type]]))) {
    ii <- which(groupings[[group_type]]==g)
    m <- score_metrics(dd$applied_sae[test][ii],p[ii],reference)
    m[,`:=`(model=name,group_type=group_type,group=g)]
    metrics[[length(metrics)+1L]] <- m
  }
  ct <- as.data.table(coeftable(fit),keep.rownames='term')
  setnames(ct,c('term','estimate','se_cluster_school','z','p_value'))
  ct[,model:=name]; coefficients[[name]] <- ct
  bins <- data.table(y=dd$applied_sae[test],p=p)
  bins[,bin:=pmin(10L,ceiling(frank(p,ties.method='average')/.N*10))]
  calibration[[name]] <- bins[,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,model:=name]
  model_audit[[name]] <- data.table(model=name,train_n=nobs(fit),test_n=length(test),coefficients=length(coef(fit)),converged=fit$convStatus,
    omitted_collinear=paste(fit$collin.var,collapse='|'),formula=paste(deparse(fml),collapse=' '))
  saveRDS(fit,file.path(out,'models',paste0(name,'.rds')))
  audit_write(rbindlist(metrics),'predictive_performance')
  log_message(name,': held-out AUC ',round(rbindlist(metrics)[model==name & group_type=='overall',auc],4))
  rm(fit); gc(verbose=FALSE)
}
audit_write(rbindlist(coefficients),'coefficients')
audit_write(rbindlist(calibration),'calibration')
audit_write(rbindlist(model_audit),'model_audit')
saveRDS(predictions,file.path(out,'heldout_predictions.rds'))
saveRDS(list(data=dd,train=train,test=test,specs=specs,parent_vars=parent_vars,reference=reference),file.path(out,'model_frame.rds'))
writeLines(capture.output(sessionInfo()),file.path(out,'session_info.txt'))
log_message('Prediction models completed.')
