# Split the exact main first-stage and pass-through specifications by origin continuation.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
if(.Platform$OS.type=='windows') invisible(Sys.setlocale('LC_CTYPE','English_United States.utf8'))
root <- normalizePath('.',winslash='/')
data_root <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD','C:/Users/brunem/Box/causal_schools')
pair <- 'va_2017_2020__sae_2018_2020'
dest <- file.path(root,'data/clean/empirical_bayes_school_va',pair,'by_continuation')
table_dir <- file.path(root,'output/tables/empirical_bayes_school_va',pair)
dir.create(file.path(dest,'models'),recursive=TRUE,showWarnings=FALSE)
# This paper snapshot includes higher-ed enrollment; the older Box copy omits it.
input <- file.path(root,'output/intermediate/empirical_bayes_school_va',pair,'scalar_iv_regression_df.csv')
admin_input <- file.path(root,'data/clean/sae_participation/admin_panel.rds')
reference <- fread(file.path(table_dir,'main_table.csv'))
specs <- data.table(
  spec=c('math_adj_eb','leng_adj_eb','exam_adj_eb','anypost_adj_eb','highered_adj_eb','highinst_adj_eb','highpay_adj_eb'),
  outcome=c('z_year_math_max','z_year_leng_max','admission_exam_taker','any_postulacion','higher_ed_enrolled_m1','high_inst_m1','high_paying_field_m1'),
  label=c('Math','Verbal','Exam taking','Fin. aid app.','Higher-ed enrollment','High-premium inst.','High-premium field'),
  require_exam=c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE))
fwrite(data.table(path=c(input,admin_input),bytes=file.info(c(input,admin_input))$size),file.path(dest,'input_manifest.csv'))
# Preflight: selected columns of the 103 MB cached regression CSV; the 160 MB
# admin RDS is read once and immediately reduced. No VA or probability rebuild.
cols <- unique(c('mrun','student_id','cohort_gr8','sae_proceso','timely_sae','any_risk','most_time_RBD',
  'GEN_ALU','EDAD_ALU','z_sim_mat_4to','z_sim_leng_4to','admission_exam_taker',specs$outcome,
  paste0('d_',specs$spec),paste0('z_',specs$spec),paste0('expected_',specs$spec)))
stopifnot(all(cols %in% names(fread(input,nrows=0))))
d <- fread(input,select=cols,na.strings=c('','NA'),showProgress=FALSE)
checks <- list()
check <- function(name,passed) {
  checks[[length(checks)+1L]] <<- data.table(check=name,passed=isTRUE(passed))
  fwrite(rbindlist(checks),file.path(dest,'verification.csv'))
  if(!isTRUE(passed)) stop('Failed: ',name)
}
check('original cached lottery population',nrow(d)==162633L && !anyDuplicated(d$mrun) &&
  all(d$timely_sae==1L & d$any_risk==1L & d$cohort_gr8 %in% 2018:2020 &
    d$cohort_gr8==d$sae_proceso & d$EDAD_ALU>=12 & d$EDAD_ALU<=16 & d$most_time_RBD>0))
admin <- readRDS(admin_input)
origin <- admin[,.(mrun,cohort_gr8,origin_rbd,continuation_group)]
rm(admin); invisible(gc())
check('unique origin lookup',!anyDuplicated(origin[,.(mrun,cohort_gr8)]))
d <- merge(d,origin,by=c('mrun','cohort_gr8'),all.x=TRUE,sort=FALSE)
check('origin lookup preserves population',nrow(d)==162633L && !anyDuplicated(d$mrun) && !anyNA(d$continuation_group))
d[,continuation_sample:=fcase(continuation_group=='Ends at 8 in SAE offerings','school_ending',
  continuation_group=='Offers grade 9','voluntary',default='unresolved_other')]
coverage <- d[,.(students=.N),by=.(continuation_sample,continuation_group)]
fwrite(coverage,file.path(dest,'classification_coverage.csv'))
fwrite(d[,.(students=.N),by=.(cohort_gr8,continuation_sample)],file.path(dest,'cohort_coverage.csv'))

results <- audits <- sample_counts <- paper_checks <- list()
independent_check <- function(reg,row,iv,fs) {
  s <- row$spec; y <- row$outcome; tr <- paste0('d_',s); z <- paste0('z_',s); ev <- paste0('expected_',s)
  cvars <- c('factor(cohort_gr8)','z_sim_mat_4to','z_sim_leng_4to','factor(GEN_ALU)','factor(EDAD_ALU)',ev)
  X <- model.matrix(as.formula(paste('~',paste(c(tr,cvars),collapse='+'))),reg)
  Z <- model.matrix(as.formula(paste('~',paste(c(z,cvars),collapse='+'))),reg)
  H <- Z %*% solve(crossprod(Z),crossprod(Z,X))
  bread <- solve(crossprod(H,X))
  b <- as.numeric(bread %*% crossprod(H,reg[[y]]))
  residual <- as.numeric(reg[[y]]-X%*%b)
  N <- nrow(X); K <- ncol(X)
  V <- (N/(N-K))*bread%*%crossprod(H*residual)%*%t(bread)
  pos <- match(tr,colnames(X)); term <- paste0('fit_',tr)
  check(paste(row$sample,s,'independent 2SLS point estimate'),abs(b[pos]-coef(iv)[[term]])<1e-8)
  check(paste(row$sample,s,'independent structural-residual HC1 SE'),abs(sqrt(V[pos,pos])-se(iv)[[term]])<1e-8)
  bf <- solve(crossprod(Z),crossprod(Z,reg[[tr]])); ef <- as.numeric(reg[[tr]]-Z%*%bf)
  inv <- solve(crossprod(Z)); vf <- (N/(N-ncol(Z)))*inv%*%crossprod(Z*ef)%*%inv
  iz <- match(z,colnames(Z))
  check(paste(row$sample,s,'independent first-stage HC1 SE'),abs(sqrt(vf[iz,iz])-se(fs)[[z]])<1e-8)
}
run_one <- function(row,sample_name) {
  s <- row$spec; y <- row$outcome; tr <- paste0('d_',s); z <- paste0('z_',s); ev <- paste0('expected_',s)
  base <- if(sample_name=='all') d else d[continuation_sample==sample_name]
  controls <- c('factor(cohort_gr8)','z_sim_mat_4to','z_sim_leng_4to','factor(GEN_ALU)','factor(EDAD_ALU)',ev)
  needed <- c(y,tr,z,ev,'cohort_gr8','z_sim_mat_4to','z_sim_leng_4to','GEN_ALU','EDAD_ALU')
  if(row$require_exam) needed <- c(needed,'admission_exam_taker')
  reg <- base[complete.cases(base[,..needed])]
  if(row$require_exam) reg <- reg[admission_exam_taker==1L]
  message(sample_name,' / ',row$label,': ',nrow(reg),' pupils')
  fml <- as.formula(paste0(y,' ~ ',paste(controls,collapse=' + '),' | 0 | ',tr,' ~ ',z))
  fs_fml <- as.formula(paste0(tr,' ~ ',paste(c(z,controls),collapse=' + ')))
  iv <- feols(fml,data=reg,vcov='hetero',notes=FALSE,lean=TRUE)
  fs <- feols(fs_fml,data=reg,vcov='hetero',notes=FALSE,lean=TRUE)
  term <- paste0('fit_',tr)
  check(paste(sample_name,s,'no extra regression sample loss'),nobs(iv)==nrow(reg) && nobs(fs)==nrow(reg))
  check(paste(sample_name,s,'identified instrument and treatment'),term %in% names(coef(iv)) && z %in% names(coef(fs)))
  b <- coef(iv)[[term]]; std <- se(iv)[[term]]; k <- coef(fs)[[z]]; ks <- se(fs)[[z]]
  result <- data.table(sample=sample_name,spec=s,label=row$label,beta=b,se=std,
    p_value_equal_zero=2*pnorm(-abs(b/std)),p_value_equal_one=2*pnorm(-abs((b-1)/std)),
    fs_beta=k,fs_se=ks,fs_p_value=2*pnorm(-abs(k/ks)),fs_f=(k/ks)^2,n_obs=nobs(iv))
  if(sample_name=='all') {
    ref <- reference[spec==s]
    vars <- c('beta','se','fs_beta','fs_se','fs_f','n_obs')
    comp <- data.table(spec=s,statistic=vars,estimated=unlist(result[,..vars]),paper=unlist(ref[,..vars]))
    comp[,passed:=abs(estimated-paper)<1e-8*pmax(1,abs(paper))]
    paper_checks[[s]] <<- comp
    fwrite(rbindlist(paper_checks),file.path(dest,'paper_replication.csv'))
    check(paste(s,'reproduces paper first and second stages'),nrow(ref)==1L && all(comp$passed))
    sample_counts[[s]] <<- reg[,.(students=.N),by=continuation_sample][,spec:=s]
  } else {
    if(s=='math_adj_eb') {row$sample <- sample_name; independent_check(reg,row,iv,fs)}
    saveRDS(list(iv=iv,first_stage=fs),file.path(dest,'models',paste0(sample_name,'_',s,'.rds')))
  }
  audits[[paste(sample_name,s)]] <<- data.table(sample=sample_name,spec=s,
    iv_formula=paste(deparse(fml),collapse=' '),first_stage_formula=paste(deparse(fs_fml),collapse=' '),
    omitted_collinear=paste(c(iv$collin.var,fs$collin.var),collapse='|'))
  result
}
# Establish exact agreement with the current paper before reporting subgroup fits.
for(sample_name in c('all','school_ending','voluntary')) for(i in seq_len(nrow(specs))) {
  key <- paste(sample_name,specs$spec[i])
  results[[key]] <- run_one(specs[i],sample_name)
}
res <- rbindlist(results); account <- rbindlist(sample_counts)
for(g in c('school_ending','voluntary')) {
  chk <- merge(res[sample==g,.(spec,n_obs)],account[continuation_sample==g,.(spec,students)],by='spec')
  check(paste(g,'is exact subset of each paper estimation sample'),nrow(chk)==7L && all(chk$n_obs==chk$students))
}
check('outcome-specific population accounting',all(account[,.(total=sum(students)),by=spec][
  res[sample=='all'],on='spec',total==i.n_obs]))
fwrite(res,file.path(dest,'results.csv'))
fwrite(account,file.path(dest,'sample_accounting.csv'))
fwrite(rbindlist(audits),file.path(dest,'model_audit.csv'))
saveRDS(d,file.path(dest,'analysis_frame.rds'))
writeLines(capture.output(sessionInfo()),file.path(dest,'session_info.txt'))
print(res[sample!='all',.(sample,label,beta,se,fs_beta,fs_se,fs_f,n_obs)])
