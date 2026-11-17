# Refit prediction models among grade-8 origins offering grade 9.
# Original school holdout retained; this follow-up is exploratory, not a new test.
source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
suppressPackageStartupMessages(library(rpart))
setFixest_nthreads(4L)
reuse_logits <- '--reuse-logits' %in% commandArgs(trailingOnly=TRUE)
vol_out <- file.path(out,'voluntary')
dir.create(vol_out,recursive=TRUE,showWarnings=FALSE)
write_audit <- function(z,name) fwrite(z,file.path(vol_out,paste0(name,'.csv')))
checks <- list()
check <- function(name,ok) {
  checks[[length(checks)+1L]] <<- data.table(check=name,passed=isTRUE(ok))
  write_audit(rbindlist(checks),'verification')
  if(!isTRUE(ok)) stop('Failed check: ',name)
}
parent_vars <- paste0('parent_',fread('code/codex/sae_participation/parent_crosswalk.csv')$feature)
admin_vars <- unlist(lapply(c('gpa','attendance','z_gpa','z_attendance'),function(v) paste0(v,'_g',5:8)))
gap_vars <- unlist(lapply(c('gap_math','gap_language'),function(v) paste0(v,'_g',5:8)))
numeric_vars <- c('age',admin_vars,gap_vars,'same_simce_origin','parent_record',parent_vars)
context_vars <- c('cohort_gr8','origin_region','origin_sector','gender')
keep <- unique(c('mrun','origin_rbd','applied_sae',context_vars,setdiff(numeric_vars,c('parent_record',parent_vars))))
log_message('Reading cached administrative panel once, filtering immediately to voluntary margin.')
panel <- readRDS(file.path(out,'admin_panel.rds'))
dd <- panel[analysis_eligible==1L & continuation_group=='Offers grade 9',keep,with=FALSE]
rm(panel); gc(verbose=FALSE)
parents <- readRDS(file.path(out,'parent_predictors.rds'))
dd <- merge(dd,parents[,c('mrun','parent_record',parent_vars),with=FALSE],by='mrun',all.x=TRUE)
rm(parents); gc(verbose=FALSE)
dd[is.na(parent_record),parent_record:=0L]
dd[!age %between% c(9,25),age:=NA_real_]
dd <- merge(dd,fread(file.path(out,'school_split.csv')),by='origin_rbd',all.x=TRUE,sort=FALSE)
setorder(dd,mrun)
train <- which(dd$split=='train'); test <- which(dd$split=='test')
check('expected voluntary sample and unique students',nrow(dd)==313813L && !anyDuplicated(dd$mrun))
check('original school split has no overlap',length(intersect(dd$origin_rbd[train],dd$origin_rbd[test]))==0L)
check('all students assigned to original split',length(train)+length(test)==nrow(dd))
write_audit(dd[,.(students=.N,schools=uniqueN(origin_rbd),applicants=sum(applied_sae),application_rate=mean(applied_sae)),by=.(split,cohort_gr8)],'sample')

# Prepare from raw measures separately for the outer training set and tree's
# inner training set. Validation and test values never determine imputation.
prepare <- function(raw,ii) {
  z <- copy(raw); values <- misses <- squares <- list(); moments <- list()
  for(v in numeric_vars) {
    x <- z[[v]]; x[!is.finite(x)] <- NA_real_
    obs <- x[ii][!is.na(x[ii])]
    med <- if(length(obs)) median(obs) else 0
    mu <- if(length(obs)) mean(obs) else 0
    sig <- if(length(obs)>1L) sd(obs) else 0
    active <- is.finite(sig) && sig>0
    missing <- as.integer(is.na(x)); mn <- paste0(v,'__missing'); vn <- paste0(v,'__z'); sn <- paste0(v,'__sq')
    set(z,j=mn,value=missing)
    if(uniqueN(missing[ii])>1L) misses[[v]] <- mn
    if(active) {
      x[is.na(x)] <- med; x <- (x-mu)/sig; set(z,j=vn,value=x); values[[v]] <- vn
      if(v %in% c('age',admin_vars,gap_vars)) {set(z,j=sn,value=x*x); squares[[v]] <- sn}
    }
    moments[[v]] <- data.table(feature=v,observed=length(obs),median=med,mean=mu,sd=sig,active=active)
  }
  for(v in context_vars) z[is.na(get(v)),(v):=0L]
  terms_for <- function(v,sq=TRUE,miss=TRUE) unique(c(unlist(values[v]),if(sq) unlist(squares[v]),if(miss) unlist(misses[v])))
  context <- c('factor(cohort_gr8)*factor(origin_region)','factor(origin_sector)','factor(gender)',terms_for('age'))
  admin <- c(context,terms_for(admin_vars)); gaps <- c(admin,terms_for(gap_vars))
  available <- c(gaps,terms_for(c('same_simce_origin','parent_record')),unlist(misses[parent_vars]))
  specs <- list(context=context,admin=admin,gaps=gaps,availability=available,
    full=c(available,terms_for(parent_vars,FALSE,FALSE)),
    before_grade8=c(context,terms_for(admin_vars[!grepl('_g8$',admin_vars)]),terms_for(gap_vars[!grepl('_g8$',gap_vars)]),
      terms_for(c('same_simce_origin','parent_record')),unlist(misses[parent_vars]),terms_for(parent_vars,FALSE,FALSE)))
  # Training-defined numeric category indicators avoid unseen-level errors in
  # trees; a category absent from training has zero on all its indicators.
  tree_context <- character()
  for(v in context_vars) for(lev in sort(unique(z[[v]][ii]))) {
    vn <- paste0(v,'__level_',lev); set(z,j=vn,value=as.integer(z[[v]]==lev)); tree_context <- c(tree_context,vn)
  }
  tree_vars <- c(tree_context,terms_for(numeric_vars,FALSE,TRUE))
  list(data=z,specs=specs,moments=rbindlist(moments),tree_vars=tree_vars)
}
outer <- prepare(dd,train); z <- outer$data
write_audit(outer$moments,'training_preprocessing')
reference <- mean(z$applied_sae[train])
pred <- z[test,.(mrun,origin_rbd,cohort_gr8,applied_sae,same_simce_origin)]
pred[,no_application:=0]
pred[,training_rate:=reference]
pooled <- readRDS(file.path(out,'heldout_predictions.rds'))
pred[,pooled_full:=pooled$plus_parent_answers[match(mrun,pooled$mrun)]]
check('same heldout pupils as pooled comparison',all(is.finite(pred$pooled_full)) && nrow(pred)==62189L)
rm(pooled)
coefficients <- audits <- list()
dir.create(file.path(vol_out,'models'),showWarnings=FALSE)
for(name in names(outer$specs)) {
  log_message('Fitting voluntary logit: ',name)
  fml <- as.formula(paste('applied_sae ~',paste(unique(outer$specs[[name]]),collapse=' + ')))
  if(reuse_logits) {
    fit <- readRDS(file.path(vol_out,'models',paste0(name,'.rds')))
    check(paste(name,'cached formula unchanged'),identical(as.character(fit$fml_all$linear),as.character(fml)))
  } else {
    fit <- feglm(fml,data=z[train],family='binomial',vcov=~origin_rbd,notes=FALSE,glm.iter=100L,lean=TRUE,data.save=TRUE)
  }
  # This fixest version drops data.save when lean=TRUE; attach the exact
  # required training columns explicitly before prediction and serialization.
  missing_training_data <- is.null(fit$data)
  fit$data <- z[train,unique(c(all.vars(fml),'origin_rbd')),with=FALSE]
  p <- as.numeric(predict(fit,newdata=z[test],type='response'))
  check(paste(name,'converged and common training sample'),isTRUE(fit$convStatus) && nobs(fit)==length(train))
  check(paste(name,'finite probabilities'),all(is.finite(p) & p>=0 & p<=1))
  pred[,(name):=p]
  ct <- as.data.table(coeftable(fit),keep.rownames='term'); setnames(ct,c('term','estimate','se_cluster_school','z','p_value'))
  ct[,model:=name]; coefficients[[name]] <- ct
  audits[[name]] <- data.table(model=name,train_n=nobs(fit),test_n=length(test),converged=fit$convStatus,
    formula=paste(deparse(fml),collapse=' '),omitted_collinear=paste(fit$collin.var,collapse='|'))
  if(!reuse_logits || missing_training_data) saveRDS(fit,file.path(vol_out,'models',paste0(name,'.rds')))
  # The saved fit carries exact training data, avoiding factor-reference drift.
  restored <- readRDS(file.path(vol_out,'models',paste0(name,'.rds')))
  check(paste(name,'saved model reproduces small batch'),max(abs(predict(restored,newdata=z[test[1:250]],type='response')-p[1:250]))<1e-10)
  rm(fit,restored); gc(verbose=FALSE)
  saveRDS(pred,file.path(vol_out,'heldout_predictions.rds'))
}
write_audit(rbindlist(coefficients),'coefficients'); write_audit(rbindlist(audits),'model_audit')

# A limited nonlinear alternative: CART probability trees, no reweighting.
# A numeric binary response gives leaf applicant rates and squared-error splits.
# Classification-error pruning can collapse useful probability variation to the
# root when every leaf is majority nonapplicant; squared-error pruning avoids it.
# Select complexity using a fresh school split inside original training only.
set.seed(20261007)
inner_schools <- sort(unique(dd$origin_rbd[train]))
validation_schools <- sample(inner_schools,ceiling(length(inner_schools)*.2))
inner_train <- train[!dd$origin_rbd[train] %in% validation_schools]
validation <- train[dd$origin_rbd[train] %in% validation_schools]
check('inner validation school separation',length(intersect(dd$origin_rbd[inner_train],dd$origin_rbd[validation]))==0L && !any(validation %in% test))
inner <- prepare(dd,inner_train)
write_audit(inner$moments,'inner_training_preprocessing')
write_audit(dd[train,.(mrun,origin_rbd,split=ifelse(origin_rbd %in% validation_schools,'validation','inner_train'))],'inner_split')
tree_audit <- list()
for(name in c('tree_full','tree_before_grade8')) {
  log_message('Selecting voluntary ',name,' using training-school validation')
  vars <- inner$tree_vars
  if(name=='tree_before_grade8') vars <- vars[!grepl('_g8__',vars)]
  tree_formula <- reformulate(vars,response='applied_sae')
  grow <- function(formula,data) rpart(formula,data=data,method='anova',
    control=rpart.control(cp=.0001,minbucket=200L,maxdepth=8L,xval=0L,maxsurrogate=0L,usesurrogate=0L,maxcompete=0L),
    model=FALSE,x=FALSE,y=FALSE)
  fit <- grow(tree_formula,inner$data[inner_train])
  candidates <- rbindlist(lapply(c(.01,.005,.001,.0005,.0001),function(cp) {
    prune_fit <- prune(fit,cp=cp)
    p <- as.numeric(predict(prune_fit,newdata=inner$data[validation])); y <- dd$applied_sae[validation]
    data.table(model=name,cp=cp,accuracy=mean((p>=.5)==y),brier=mean((y-p)^2),leaves=sum(prune_fit$frame$var=='<leaf>'))
  }))
  setorder(candidates,-accuracy,brier,-cp)
  chosen_cp <- candidates$cp[1L]
  candidates[,selected:=cp==chosen_cp]; tree_audit[[name]] <- candidates
  vars <- outer$tree_vars
  if(name=='tree_before_grade8') vars <- vars[!grepl('_g8__',vars)]
  tree_formula <- reformulate(vars,response='applied_sae')
  fit <- prune(grow(tree_formula,z[train]),cp=chosen_cp)
  p <- as.numeric(predict(fit,newdata=z[test]))
  check(paste(name,'finite probabilities'),all(is.finite(p)&p>=0&p<=1))
  pred[,(name):=p]
  saveRDS(fit,file.path(vol_out,'models',paste0(name,'.rds')))
  importance <- if(length(fit$variable.importance)) data.table(feature=names(fit$variable.importance),importance=as.numeric(fit$variable.importance),model=name) else
    data.table(feature=character(),importance=double(),model=character())
  write_audit(importance,paste0(name,'_importance'))
  rm(fit); gc(verbose=FALSE)
}
write_audit(rbindlist(tree_audit),'tree_validation')
saveRDS(pred,file.path(vol_out,'heldout_predictions.rds'))
saveRDS(list(data=z[test],reference=reference,specs=outer$specs,tree_vars=outer$tree_vars),file.path(vol_out,'evaluation_frame.rds'))
rm(inner,dd,outer,z); gc(verbose=FALSE)

score <- function(y,p,school,id,model) {
  n1 <- sum(as.double(y)); n0 <- length(y)-n1
  auc <- (sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0)
  # Average precision uses score groups to avoid favorable ordering of ties.
  grouped <- data.table(y,p)[,.(positive=sum(y),n=.N),by=p][order(-p)]
  ap <- sum(grouped$positive/n1*cumsum(grouped$positive)/cumsum(grouped$n))
  h <- as.integer(p>=.5); tp <- sum(h==1 & y==1); fp <- sum(h==1 & y==0)
  fn <- sum(h==0 & y==1); tn <- sum(h==0 & y==0)
  delta <- as.integer(h==y)-as.integer(y==0)
  lo <- hi <- 0
  if(any(delta!=0)) {
    paired <- feols(delta~1,data=data.frame(delta,school),vcov=~school,notes=FALSE)
    ci <- confint(paired); lo <- ci[1,1]; hi <- ci[1,2]
  }
  q <- pmin(pmax(p,1e-12),1-1e-12)
  logloss <- if(any((p==0 & y==1)|(p==1 & y==0))) Inf else -mean(y*log(q)+(1-y)*log1p(-q))
  # Exact top decile; outcome-independent student ID breaks score ties.
  top <- order(-p,id)[seq_len(ceiling(length(y)*.1))]
  data.table(model=model,n=length(y),applicants=sum(y),auc=auc,average_precision=ap,brier=mean((y-p)^2),
    brier_skill_vs_training_rate=1-mean((y-p)^2)/mean((y-reference)^2),log_loss=logloss,
    predicted_mean=mean(p),accuracy=mean(h==y),errors=fp+fn,applicants_detected=tp,false_positives=fp,
    precision=if(tp+fp>0) tp/(tp+fp) else NA_real_,recall=tp/n1,net_errors_avoided=tp-fp,
    gain_pp=100*mean(delta),gain_ci_low_pp=100*lo,gain_ci_high_pp=100*hi,
    top_decile_n=length(top),top_decile_precision=mean(y[top]),top_decile_recall=sum(y[top])/sum(y),
    top_decile_lift=mean(y[top])/mean(y))
}
models <- setdiff(names(pred),c('mrun','origin_rbd','cohort_gr8','applied_sae','same_simce_origin'))
metrics <- rbindlist(lapply(models,function(m) score(pred$applied_sae,pred[[m]],pred$origin_rbd,pred$mrun,m)))
write_audit(metrics,'performance')
by_cohort <- rbindlist(lapply(models,function(m) pred[,score(applied_sae,get(m),origin_rbd,mrun,m),by=cohort_gr8]))
write_audit(by_cohort,'performance_by_cohort')
calibration <- rbindlist(lapply(setdiff(models,c('no_application','training_rate')),function(m) {
  b <- pred[,.(y=applied_sae,p=get(m),mrun)]
  b[,bin:=pmin(10L,ceiling(frankv(b,c('p','mrun'),order=c(-1L,1L),ties.method='first')/.N*10))]
  b[,.(students=.N,applicants=sum(y),observed=mean(y),predicted=mean(p)),by=bin][,model:=m]
}))
write_audit(calibration,'calibration')
for(m in models) {
  p <- pred[[m]]; y <- pred$applied_sae; h <- p>=.5
  confusion <- table(factor(y,levels=0:1),factor(as.integer(h),levels=0:1))
  row <- metrics[model==m]
  check(paste(m,'independent confusion counts'),row$applicants_detected==confusion[2,2] && row$false_positives==confusion[1,2] &&
    row$net_errors_avoided==sum(diag(confusion))-sum(y==0) && abs(row$accuracy-sum(diag(confusion))/sum(confusion))<1e-12)
  groups <- data.table(y,p)[,.(yes=sum(y),no=sum(1-y)),by=p][order(p)]
  independent_auc <- sum(groups$yes*(cumsum(as.double(groups$no))-groups$no+.5*groups$no))/(sum(y)*as.double(sum(1-y)))
  check(paste(m,'independent grouped AUC'),abs(row$auc-independent_auc)<1e-10)
}
check('sensitivity formulas exclude grade8',!grepl('_g8',audits[['before_grade8']]$formula))
check('parent increment holds availability constant',all(c('availability','full') %in% metrics$model))
writeLines(capture.output(sessionInfo()),file.path(vol_out,'session_info.txt'))
log_message('Voluntary-margin estimation and checks completed.')
print(metrics[,.(model,auc,average_precision,accuracy,applicants_detected,false_positives,net_errors_avoided,top_decile_precision)])
