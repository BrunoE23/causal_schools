# Describe all Table 2 applicants, not only the selected IV estimation pupils.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if(.Platform$OS.type=='windows') invisible(Sys.setlocale('LC_CTYPE','English_United States.utf8'))
root <- normalizePath('.',winslash='/')
data_root <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD','C:/Users/brunem/Box/causal_schools')
pair <- 'va_2017_2020__sae_2018_2020'
dest <- file.path(root,'data/clean/empirical_bayes_school_va',pair,'by_continuation')
specs <- data.table(
  spec=c('math_adj_eb','leng_adj_eb','exam_adj_eb','anypost_adj_eb','highered_adj_eb','highinst_adj_eb','highpay_adj_eb'),
  outcome=c('z_year_math_max','z_year_leng_max','admission_exam_taker','any_postulacion','higher_ed_enrolled_m1','high_inst_m1','high_paying_field_m1'),
  label=c('Math','Verbal','Exam taking','Fin. aid app.','Higher-ed enrollment','High-premium inst.','High-premium field'),
  require_exam=c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE),scale=c(1,1,100,100,100,100,100))
universe_path <- file.path(data_root,'data/clean/univ_gr8_df.csv')
value_path <- file.path(data_root,'data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv')
admin_path <- file.path(root,'data/clean/sae_participation/admin_panel.rds')
fwrite(data.table(path=c(universe_path,value_path,admin_path),bytes=file.info(c(universe_path,value_path,admin_path))$size),
  file.path(dest,'descriptive_input_manifest.csv'))
# Preflight: 1.23 GB universe -> only seven roster fields, then filter immediately.
u <- fread(universe_path,select=c('mrun','student_id','cohort_gr8','sae_proceso','timely_sae','most_time_RBD','EDAD_ALU'),
  na.strings=c('','NA'),showProgress=FALSE)
u <- u[cohort_gr8 %in% 2017:2020 & !is.na(most_time_RBD) & most_time_RBD>0 &
  !is.na(EDAD_ALU) & EDAD_ALU>=12 & EDAD_ALU<=16]
setkey(u,mrun); u <- unique(u,by='mrun')
stopifnot(nrow(u)==931547L)
u <- u[cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L]
stopifnot(nrow(u)==294198L,!anyDuplicated(u$mrun),all(u$mrun==u$student_id))
admin <- readRDS(admin_path)
origin <- admin[,.(mrun,cohort_gr8,continuation_group)]
rm(admin); invisible(gc())
stopifnot(!anyDuplicated(origin[,.(mrun,cohort_gr8)]))
u <- merge(u,origin,by=c('mrun','cohort_gr8'),all.x=TRUE,sort=FALSE)
stopifnot(nrow(u)==294198L,!anyNA(u$continuation_group))
u[,sample:=fcase(continuation_group=='Ends at 8 in SAE offerings','school_ending',
  continuation_group=='Offers grade 9','voluntary',default='unresolved_other')]
values <- fread(value_path,select=c('school_rbd','analysis_sample','outcome','controlled_value_added_eb_centered_student'))
values <- values[analysis_sample=='All' & outcome %in% specs$outcome]
stopifnot(!anyDuplicated(values[,.(school_rbd,outcome)]),all(is.finite(values$controlled_value_added_eb_centered_student)))
wide <- dcast(values,school_rbd~outcome,value.var='controlled_value_added_eb_centered_student')
prob_audit <- list()
expected <- rbindlist(lapply(2018:2020,function(year) {
  message('Expected VA for all Table 2 applicants: ',year)
  p <- fread(file.path(data_root,'data/clean/DA_probs',paste0('DA_probs_',year,'.csv')),
    select=c('student_id','school_id','prob'),na.strings=c('','NA'),showProgress=FALSE)
  p <- p[student_id %in% u[sae_proceso==year,student_id]]
  p[,`:=`(school_rbd=suppressWarnings(as.numeric(sub('_.*$','',school_id))),
    unmatched=tolower(trimws(school_id))=='unmatched')]
  p <- merge(p,wide,by='school_rbd',all.x=TRUE,sort=FALSE)
  z <- p[,.(any_risk=as.integer(max(prob,na.rm=TRUE)<1),total_mass=sum(prob),
    unmatched_mass=sum(prob*unmatched)),by=student_id]
  stopifnot(max(abs(z$total_mass-1))<1e-6)
  for(i in seq_len(nrow(specs))) {
    v <- specs$outcome[i]; s <- specs$spec[i]
    q <- p[,.(expected=sum(prob*fifelse(is.na(get(v)),0,get(v))),
      covered_mass=sum(prob*as.integer(!is.na(get(v))))),by=student_id]
    setnames(q,c('expected','covered_mass'),paste0(c('expected_','mass_'),s))
    z <- merge(z,q,by='student_id',sort=FALSE)
  }
  prob_audit[[as.character(year)]] <<- data.table(year,applicants=u[sae_proceso==year,.N],
    probability_students=nrow(z),max_mass_error=max(abs(z$total_mass-1)))
  z[,sae_proceso:=year]
}))
stopifnot(!anyDuplicated(expected[,.(student_id,sae_proceso)]))
u <- merge(u,expected,by=c('student_id','sae_proceso'),all.x=TRUE,sort=FALSE)
stopifnot(nrow(u)==294198L,!anyDuplicated(u$mrun))
groups <- c('all','school_ending','voluntary','unresolved_other')
risk_rates <- rbindlist(lapply(groups,function(g) {
  x <- if(g=='all') u else u[sample==g]
  data.table(sample=g,applicants=nrow(x),probability_observed=sum(!is.na(x$any_risk)),
    applicants_at_risk=sum(x$any_risk==1L,na.rm=TRUE),
    risk_share=sum(x$any_risk==1L,na.rm=TRUE)/nrow(x),mean_unmatched_mass=mean(x$unmatched_mass,na.rm=TRUE))
}))
stopifnot(risk_rates[sample=='all',applicants_at_risk]==162633L,
  risk_rates[sample=='school_ending',applicants]==250564L,
  risk_rates[sample=='voluntary',applicants]==38868L)
fwrite(risk_rates,file.path(dest,'assignment_risk_rates.csv'))
fwrite(rbindlist(prob_audit),file.path(dest,'probability_coverage.csv'))

# Check expected VA against the exact cached main-regression inputs, outcome by outcome.
iv_data <- readRDS(file.path(dest,'analysis_frame.rds'))
ev_vars <- paste0('expected_',specs$spec)
validation <- merge(iv_data[,c('mrun','cohort_gr8',ev_vars),with=FALSE],
  u[,c('mrun','cohort_gr8',ev_vars),with=FALSE],by=c('mrun','cohort_gr8'),suffixes=c('_cached','_rebuilt'))
checks <- rbindlist(lapply(ev_vars,function(v) {
  delta <- abs(validation[[paste0(v,'_cached')]]-validation[[paste0(v,'_rebuilt')]])
  data.table(variable=v,students=nrow(validation),max_absolute_difference=max(delta),
    passed=nrow(validation)==162633L && all(is.finite(delta)) && max(delta)<1e-10)
}))
fwrite(checks,file.path(dest,'expected_va_verification.csv'))
stopifnot(all(checks$passed))
summarize_ev <- function(x,g,scope,i) {
  s <- specs$spec[i]; e <- paste0('expected_',s); mass <- paste0('mass_',s)
  data.table(sample=g,scope,spec=s,label=specs$label[i],students=nrow(x),
    observed_expected=sum(is.finite(x[[e]])),mean_expected=mean(x[[e]],na.rm=TRUE),
    sd_expected=sd(x[[e]],na.rm=TRUE),mean_covered_mass=mean(x[[mass]],na.rm=TRUE),
    display_scale=specs$scale[i],unit=if(i<=2) 'outcome SD' else 'percentage points')
}
ev_stats <- list()
regression_results <- fread(file.path(dest,'results.csv'))
for(g in c('all','school_ending','voluntary')) for(i in seq_len(nrow(specs))) {
  x <- if(g=='all') u else u[sample==g]
  ev_stats[[length(ev_stats)+1L]] <- summarize_ev(x,g,'All applicants',i)
  ev_stats[[length(ev_stats)+1L]] <- summarize_ev(x[any_risk==1L],g,'Applicants with risk',i)
  s <- specs$spec[i]; y <- specs$outcome[i]
  v <- if(g=='all') iv_data else iv_data[continuation_sample==g]
  needed <- c(y,paste0(c('d_','z_','expected_'),s),'cohort_gr8','z_sim_mat_4to','z_sim_leng_4to','GEN_ALU','EDAD_ALU')
  if(specs$require_exam[i]) needed <- c(needed,'admission_exam_taker')
  v <- v[complete.cases(v[,..needed])]
  if(specs$require_exam[i]) v <- v[admission_exam_taker==1L]
  stopifnot(nrow(v)==regression_results[sample==g & spec==s,n_obs])
  ev_stats[[length(ev_stats)+1L]] <- summarize_ev(x[mrun %in% v$mrun],g,'IV estimation sample',i)
}
ev_stats <- rbindlist(ev_stats)
ev_stats[,display_mean:=mean_expected*display_scale]
fwrite(ev_stats,file.path(dest,'expected_va_by_group.csv'))
saveRDS(u,file.path(dest,'all_applicant_expected_va.rds'))
print(risk_rates)
print(ev_stats[sample!='all' & scope=='All applicants',.(sample,label,students,display_mean,unit,mean_covered_mass)])
