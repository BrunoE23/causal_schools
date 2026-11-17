# PCA and a reusable analysis frame for the October 10 interpretation exercise.
source('code/codex/sae_participation/common.R')
new_out <- file.path(out,'interpretation_pca')
dir.create(new_out,recursive=TRUE,showWarnings=FALSE)
write_audit <- function(z,name) fwrite(z,file.path(new_out,paste0(name,'.csv')))
cw <- fread('code/codex/sae_participation/parent_crosswalk.csv')
parent_vars <- paste0('parent_',cw$feature)
stems <- c('gpa','attendance','z_gpa','z_attendance','gap_math','gap_language')
academic <- unlist(lapply(stems,function(s) paste0(s,'_g',5:8)))
keep <- c('mrun','origin_rbd','cohort_gr8','origin_region','origin_sector','gender','age','applied_sae',
  'simce_year','same_simce_origin',academic)
log_message('Reading cached panel once and immediately restricting to voluntary sample.')
panel <- readRDS(file.path(out,'admin_panel.rds'))
d <- panel[analysis_eligible==1L & continuation_group=='Offers grade 9',keep,with=FALSE]
rm(panel); invisible(gc())
parents <- readRDS(file.path(out,'parent_predictors.rds'))
d <- merge(d,parents[,c('mrun','parent_record',parent_vars),with=FALSE],by='mrun',all.x=TRUE)
rm(parents); invisible(gc())
d[is.na(parent_record),parent_record:=0L]
d[!age %between% c(9,25),age:=NA_real_]
for(v in c('origin_region','origin_sector','gender')) d[is.na(get(v)),(v):=0L]
setorder(d,mrun)
stopifnot(nrow(d)==313813L,!anyDuplicated(d$mrun),sum(d$applied_sae)==45947L)

# Preserve the original regressor units and imputation for direct comparisons.
prep <- fread(file.path(out,'voluntary/training_preprocessing.csv'))
for(v in c('age','same_simce_origin','parent_record',academic)) {
  row <- prep[feature==v]; stopifnot(nrow(row)==1L,row$active)
  x <- d[[v]]; x[!is.finite(x)] <- NA_real_
  set(d,j=paste0(v,'__missing'),value=as.integer(is.na(x)))
  x[is.na(x)] <- row$median
  x <- (x-row$mean)/row$sd
  set(d,j=paste0(v,'__z'),value=x)
  if(v %in% c('age',academic)) set(d,j=paste0(v,'__sq'),value=x*x)
}
d[,survey_year:=fifelse(is.na(simce_year),0L,as.integer(simce_year))]
d[,survey_fraction_answered:=0]
loadings <- variance <- coverage <- audits <- list(); pca_objects <- list()
for(yr in 2013:2016) {
  vars <- paste0('parent_',cw[!is.na(get(paste0('code',yr))) & get(paste0('code',yr))!='',feature])
  ii <- which(d$survey_year==yr)
  matrix_answers <- as.matrix(d[ii,vars,with=FALSE])
  responded <- rowSums(is.finite(matrix_answers))
  complete <- responded==length(vars)
  fit_rows <- ii[complete]
  stopifnot(length(fit_rows)>length(vars),all(apply(matrix_answers[complete,,drop=FALSE],2,sd)>0))
  # Correlation PCA, without imputation and without consulting application Y.
  pc <- prcomp(matrix_answers[complete,,drop=FALSE],center=TRUE,scale.=TRUE)
  # PC1 points toward favorable school ratings. Other signs are deterministic:
  # their largest-magnitude loading is positive; meanings remain year-specific.
  for(k in seq_len(ncol(pc$rotation))) {
    sgn <- if(k==1L) sign(sum(pc$rotation[,k])) else sign(pc$rotation[which.max(abs(pc$rotation[,k])),k])
    if(sgn==0) sgn <- 1
    pc$rotation[,k] <- pc$rotation[,k]*sgn; pc$x[,k] <- pc$x[,k]*sgn
  }
  check_cov <- cov(pc$x)
  audits[[as.character(yr)]] <- data.table(survey_year=yr,
    orthonormal=max(abs(crossprod(pc$rotation)-diag(ncol(pc$rotation))))<1e-10,
    diagonal_score_cov=max(abs(check_cov-diag(diag(check_cov))))<1e-8,
    variance_sum=abs(sum(pc$sdev^2)-length(vars))<1e-8,
    correct_pc1_eigenvector=max(abs(cor(matrix_answers[complete,,drop=FALSE])%*%pc$rotation[,1]-pc$sdev[1]^2*pc$rotation[,1]))<1e-8)
  for(k in 1:3) {
    nm <- paste0('survey_pc',k,'_',yr)
    set(d,j=nm,value=rep(0,nrow(d)))
    set(d,fit_rows,j=nm,value=pc$x[,k]/pc$sdev[k])
    stopifnot(abs(mean(d[[nm]][fit_rows]))<1e-10,abs(sd(d[[nm]][fit_rows])-1)<1e-10)
  }
  set(d,j=paste0('survey_complete_',yr),value=as.integer(seq_len(nrow(d)) %in% fit_rows))
  set(d,ii,j='survey_fraction_answered',value=responded/length(vars))
  variance[[as.character(yr)]] <- data.table(survey_year=yr,component=seq_along(pc$sdev),eigenvalue=pc$sdev^2,
    share=pc$sdev^2/sum(pc$sdev^2),cumulative=cumsum(pc$sdev^2)/sum(pc$sdev^2))
  loadings[[as.character(yr)]] <- rbindlist(lapply(1:3,function(k) data.table(survey_year=yr,component=k,
    feature=rownames(pc$rotation),loading=pc$rotation[,k],correlation_with_score=pc$rotation[,k]*pc$sdev[k])))
  coverage[[as.character(yr)]] <- data.table(survey_year=yr,items=length(vars),linked_students=length(ii),
    any_answer=sum(responded>0),complete_students=sum(complete),fraction_complete=mean(complete))
  pc$x <- NULL
  pca_objects[[as.character(yr)]] <- pc
  log_message('PCA ',yr,': ',length(fit_rows),' complete respondents, ',length(vars),' items; PC1 ',round(100*variance[[as.character(yr)]]$share[1],1),'%')
}
verification <- rbindlist(audits)
stopifnot(all(unlist(verification[,-'survey_year'])))
write_audit(verification,'pca_verification')
write_audit(rbindlist(variance),'pca_variance')
loading_table <- rbindlist(loadings)
loading_table[,label:=cw$label[match(sub('^parent_','',feature),cw$feature)]]
write_audit(loading_table,'pca_loadings')
write_audit(rbindlist(coverage),'pca_coverage')
write_audit(data.table(students=nrow(d),applicants=sum(d$applied_sae),schools=uniqueN(d$origin_rbd),
  complete_survey=sum(rowSums(d[,paste0('survey_complete_',2013:2016),with=FALSE])>0),
  unknown_survey_year=sum(d$survey_year==0L)),'sample')
saveRDS(pca_objects,file.path(new_out,'survey_pca_models.rds'))
saveRDS(d[,setdiff(names(d),parent_vars),with=FALSE],file.path(new_out,'analysis_frame.rds'))
writeLines(capture.output(sessionInfo()),file.path(new_out,'session_info.txt'))
log_message('PCA construction completed.')
