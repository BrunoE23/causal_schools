# Replace grade-specific slopes with compact academic-history averages.
source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
setFixest_nthreads(4L)
average_out <- file.path(out,'averaged_grades')
dir.create(average_out,recursive=TRUE,showWarnings=FALSE)
write_audit <- function(z,name) fwrite(z,file.path(average_out,paste0(name,'.csv')))
stems <- c('gpa','attendance','z_gpa','z_attendance','gap_math','gap_language')
raw_vars <- unlist(lapply(stems,function(s) paste0(s,'_g',5:8)))
context_vars <- c('mrun','origin_rbd','applied_sae','cohort_gr8','origin_region','origin_sector','gender',
  'age__z','age__sq','age__missing','same_simce_origin__z','same_simce_origin__missing','parent_record__z',
  'survey_year','survey_fraction_answered',paste0('survey_complete_',2013:2016),paste0('survey_pc1_',2013:2016))
# The source is 122 MB. Read it once, immediately retaining only needed fields.
frame <- readRDS(file.path(out,'interpretation_pca/analysis_frame.rds'))
d <- frame[,c(context_vars,raw_vars),with=FALSE]
rm(frame); invisible(gc())
school_split <- fread(file.path(out,'school_split.csv'))
training <- d$origin_rbd %in% school_split[split=='train',origin_rbd]
checks <- list(); prep <- list(); coverage <- list()
check <- function(name,ok) {
  checks[[length(checks)+1L]] <<- data.table(check=name,passed=isTRUE(ok))
  write_audit(rbindlist(checks),'verification')
  if(!isTRUE(ok)) stop('Failed: ',name)
}
check('same voluntary population',nrow(d)==313813L && !anyDuplicated(d$mrun) && sum(d$applied_sae)==45947L)
add_average <- function(name,values) {
  values[!is.finite(values)] <- NA_real_
  count <- rowSums(!is.na(values)); avg <- rowMeans(values,na.rm=TRUE); avg[count==0L] <- NA_real_
  observed <- avg[training & is.finite(avg)]
  mu <- mean(observed); sig <- sd(observed); med <- median(observed)
  stopifnot(is.finite(sig),sig>0)
  x <- avg; x[is.na(x)] <- med; x <- (x-mu)/sig
  set(d,j=name,value=avg); set(d,j=paste0(name,'__n'),value=as.integer(count))
  set(d,j=paste0(name,'__z'),value=x); set(d,j=paste0(name,'__sq'),value=x*x)
  prep[[name]] <<- data.table(feature=name,mean=mu,sd=sig,median=med,train_observed=length(observed))
  coverage[[name]] <<- data.table(observed_grades=count)[,.(students=.N),by=observed_grades][,feature:=name]
  # Independently recompute means for a fixed, outcome-free spread of rows.
  rows <- unique(round(seq(1,nrow(values),length.out=101L)))
  manual <- apply(values[rows,,drop=FALSE],1,function(x) if(all(is.na(x))) NA_real_ else sum(x,na.rm=TRUE)/sum(!is.na(x)))
  check(paste(name,'mean uses observed grades only'),isTRUE(all.equal(avg[rows],manual,tolerance=1e-12)))
}
for(period in c('58','56','78')) {
  grades <- switch(period,'58'=5:8,'56'=5:6,'78'=7:8)
  for(s in stems) add_average(paste0(s,'_avg',period),as.matrix(d[,paste0(s,'_g',grades),with=FALSE]))
}
# For the focused GPA/gap comparison, average over identical observed grades,
# so the GPA-minus-baseline identity remains exact for each pupil.
G <- as.matrix(d[,paste0('z_gpa_g',5:8),with=FALSE])
D <- as.matrix(d[,paste0('gap_math_g',5:8),with=FALSE])
observed_pair <- is.finite(G)&is.finite(D)
G[!observed_pair] <- NA_real_; D[!observed_pair] <- NA_real_
add_average('paired_gpa_avg58',G); add_average('paired_gap_avg58',D)
M <- rowMeans(G-D,na.rm=TRUE)
pair_rows <- which(d$paired_gpa_avg58__n>0L)
check('paired averages use identical grades',identical(d$paired_gpa_avg58__n,d$paired_gap_avg58__n))
check('averaged GPA gap identity',max(abs(d$paired_gpa_avg58[pair_rows]-d$paired_gap_avg58[pair_rows]-M[pair_rows]))<1e-12)
write_audit(rbindlist(prep),'preprocessing'); write_audit(rbindlist(coverage),'coverage')
rm(G,D,M,observed_pair); invisible(gc())
context <- c('factor(cohort_gr8)*factor(origin_region)','factor(origin_sector)','factor(gender)',
  'age__z','age__sq','age__missing','same_simce_origin__z','same_simce_origin__missing','parent_record__z',
  'factor(survey_year)','survey_fraction_answered',paste0('survey_complete_',2013:2016),paste0('survey_pc1_',2013:2016))
terms_for <- function(vars) unlist(lapply(vars,function(v) c(paste0(v,'__z'),paste0(v,'__sq'),paste0('factor(',v,'__n)'))))
ct_all <- summaries <- audits <- list(); models <- list()
dir.create(file.path(average_out,'models'),showWarnings=FALSE)
estimate <- function(name,vars,rows=seq_len(nrow(d))) {
  fml <- as.formula(paste('applied_sae ~',paste(c(context,terms_for(vars)),collapse=' + ')))
  log_message('Average-history OLS: ',name)
  fit <- feols(fml,data=d[rows],vcov=~origin_rbd,lean=TRUE,notes=FALSE)
  check(paste(name,'no sample loss'),nobs(fit)==length(rows))
  ct <- as.data.table(coeftable(fit),keep.rownames='term'); setnames(ct,c('term','estimate','se_cluster_school','t','p_value'))
  ci <- confint(fit)
  ct[,`:=`(model=name,estimate_pp=100*estimate,se_pp=100*se_cluster_school,
    ci_low_pp=100*ci[match(term,rownames(ci)),1],ci_high_pp=100*ci[match(term,rownames(ci)),2])]
  ct_all[[name]] <<- ct
  summaries[[name]] <<- data.table(model=name,students=nobs(fit),schools=uniqueN(d$origin_rbd[rows]),
    applicants=sum(d$applied_sae[rows]),r_squared=as.numeric(fitstat(fit,'r2')[[1]]),estimated_coefficients=length(coef(fit)))
  audits[[name]] <<- data.table(model=name,formula=paste(deparse(fml),collapse=' '),omitted_collinear=paste(fit$collin.var,collapse='|'))
  models[[name]] <<- fit; saveRDS(fit,file.path(average_out,'models',paste0(name,'.rds')))
  invisible(fit)
}
for(s in stems) estimate(paste0('one_',s),paste0(s,'_avg58'))
estimate('joint_avg58',paste0(stems,'_avg58'))
for(s in stems) estimate(paste0('periods_',s),paste0(s,'_avg',c('56','78')))
estimate('joint_two_periods',unlist(lapply(stems,function(s) paste0(s,'_avg',c('56','78')))))
estimate('paired_gpa_only','paired_gpa_avg58',pair_rows)
estimate('paired_gap_only','paired_gap_avg58',pair_rows)
fit <- estimate('paired_both',c('paired_gpa_avg58','paired_gap_avg58'),pair_rows)

# Independently reconstruct OLS clustered covariance for the compact joint model.
joint <- models[['joint_avg58']]
X <- model.matrix(joint$fml_all$linear,data=d)[,names(coef(joint)),drop=FALSE]
e <- d$applied_sae-as.numeric(X%*%coef(joint))
inverse <- solve(crossprod(X)); scores <- rowsum(X*e,d$origin_rbd,reorder=FALSE)
N <- nrow(d); K <- ncol(X); n_school <- uniqueN(d$origin_rbd)
independent <- (n_school/(n_school-1))*((N-1)/(N-K))*inverse%*%crossprod(scores)%*%inverse
check('joint average OLS normal equations',max(abs(crossprod(X,e)))/N<1e-8)
check('joint average exact clustered SEs',max(abs(sqrt(diag(independent))-sqrt(diag(vcov(joint)))))<1e-7)
check('one-at-a-time models contain one academic summary',all(vapply(stems,function(s) {
  vars <- all.vars(models[[paste0('one_',s)]]$fml_all$linear)
  all(vars[grepl('_avg',vars)] %in% paste0(s,'_avg58',c('__z','__sq','__n')))
},logical(1))))
write_audit(rbindlist(ct_all),'coefficients'); write_audit(rbindlist(summaries),'model_summary'); write_audit(rbindlist(audits),'model_audit')
saveRDS(d[,setdiff(names(d),raw_vars),with=FALSE],file.path(average_out,'analysis_frame.rds'))
writeLines(capture.output(sessionInfo()),file.path(average_out,'session_info.txt'))
log_message('Averaged-history regressions completed.')
print(rbindlist(ct_all)[model %in% c('one_z_gpa','one_gap_math','joint_avg58') & grepl('_avg58__z$',term),.(model,term,estimate_pp,se_pp)])
