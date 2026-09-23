# Compare direct unshrunk cross-outcome IV estimates with causal gains implied by
# the RSS projection matrix and the unshrunk same-outcome IV pass-through.
suppressPackageStartupMessages({library(data.table);library(fixest)})
data_wd<-Sys.getenv('CAUSAL_SCHOOLS_DATA_WD',unset='C:/Users/brunem/Box/causal_schools')
repo_wd<-Sys.getenv('CAUSAL_SCHOOLS_REPO_WD',unset=getwd())
pair_tag<-'va_2017_2020__sae_2018_2020'
reg_path<-file.path(data_wd,'data/clean/empirical_bayes_school_va',pair_tag,'scalar_iv_regression_df.csv')
va_path<-file.path(data_wd,'data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv')
prob_dir<-file.path(data_wd,'data/clean/DA_probs')
rss_path<-file.path(data_wd,'data/clean/rss_debiased_varcov/four_year_no_sae/rss_implied_gains.csv')
out_dir<-file.path(repo_wd,'output/tables/rss_debiased_varcov');dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

specs<-data.table(
  spec=c('math','verbal','exam','highinst','highpay','income','aid'),
  label=c('Math','Verbal','Exam taking','High-premium inst.','High-premium field','Log income','Fin. aid app.'),
  outcome=c('z_year_math_max','z_year_leng_max','admission_exam_taker','high_inst_m1','high_paying_field_m1','log_program_income_clp_m1','any_postulacion'),
  require_exam=c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE))

message('Reading scalar-IV sample and unshrunk school VA.')
dt<-fread(reg_path,na.strings=c('','NA'))
dt<-dt[!is.na(most_time_RBD)&most_time_RBD>0&!is.na(EDAD_ALU)&EDAD_ALU>=12&EDAD_ALU<=16]
va<-fread(va_path,select=c('outcome','school_rbd','analysis_sample','controlled_value_added_centered_student'),na.strings=c('','NA'))
va<-merge(va[analysis_sample=='All'],specs[,.(key=spec,outcome)],by='outcome',all=FALSE)
va<-va[,.(school_rbd=as.numeric(school_rbd),key,value=controlled_value_added_centered_student)]
stopifnot(!anyDuplicated(va[,.(school_rbd,key)]))
wide<-dcast(va,school_rbd~key,value.var='value')
setnames(wide,specs$spec,paste0('raw_',specs$spec))

message('Constructing expected unshrunk VA from DA probabilities.')
prob<-rbindlist(lapply(2018:2020,function(y){
  x<-fread(file.path(prob_dir,paste0('DA_probs_',y,'.csv')),select=c('student_id','school_id','prob'))
  x[,.(student_id=as.numeric(student_id),sae_proceso=as.integer(y),school_rbd=suppressWarnings(as.numeric(sub('_.*$','',school_id))),prob)]
}))
prob<-merge(prob,wide,by='school_rbd',all.x=TRUE,sort=FALSE)
expected<-unique(prob[,.(student_id,sae_proceso)])
for(k in specs$spec){
  col<-paste0('raw_',k)
  z<-prob[,.(value=sum(prob*fifelse(is.na(get(col)),0,get(col)),na.rm=TRUE)),by=.(student_id,sae_proceso)]
  setnames(z,'value',paste0('expected_raw_',k));expected<-merge(expected,z,by=c('student_id','sae_proceso'),all.x=TRUE,sort=FALSE)
}
rm(prob);gc(FALSE)
dt<-merge(dt,expected,by=c('student_id','sae_proceso'),all=FALSE,sort=FALSE)
a<-copy(wide);setnames(a,paste0('raw_',specs$spec),paste0('d_raw_',specs$spec));dt<-merge(dt,a,by.x='most_time_RBD',by.y='school_rbd',all.x=TRUE,sort=FALSE)
o<-copy(wide);setnames(o,paste0('raw_',specs$spec),paste0('z_raw_',specs$spec));dt<-merge(dt,o,by.x='rbd_treated_1R',by.y='school_rbd',all.x=TRUE,sort=FALSE)
for(k in specs$spec)dt[is.na(rbd_treated_1R)|rbd_treated_1R==0,(paste0('z_raw_',k)):=0]

run_one<-function(from,to){
  p<-specs[spec==from];q<-specs[spec==to];y<-q$outcome;d<-paste0('d_raw_',from);z<-paste0('z_raw_',from);e<-paste0('expected_raw_',from)
  controls<-c('factor(cohort_gr8)','z_sim_mat_4to','z_sim_leng_4to','factor(GEN_ALU)','factor(EDAD_ALU)',e)
  needed<-c(y,d,z,e,'cohort_gr8','z_sim_mat_4to','z_sim_leng_4to','GEN_ALU','EDAD_ALU')
  if(q$require_exam)needed<-c(needed,'admission_exam_taker')
  reg<-dt[complete.cases(dt[,..needed])];if(q$require_exam)reg<-reg[admission_exam_taker==1L]
  m<-feols(as.formula(paste0(y,' ~ ',paste(controls,collapse=' + '),' | 0 | ',d,' ~ ',z)),data=reg,vcov='hetero',notes=FALSE)
  fs<-feols(as.formula(paste0(d,' ~ ',z,' + ',paste(controls,collapse=' + '))),data=reg,vcov='hetero',notes=FALSE)
  term<-paste0('fit_',d);if(!term%chin%names(coef(m)))term<-d
  b<-unname(coef(m)[term]);s<-unname(se(m)[term]);kb<-unname(coef(fs)[z]);ks<-unname(se(fs)[z])
  data.table(from_key=from,from_label=p$label,to_key=to,to_label=q$label,direct_iv=b,direct_se=s,direct_p=2*pnorm(-abs(b/s)),kappa=kb,kappa_se=ks,first_stage_f=(kb/ks)^2,n_obs=nobs(m))
}
setFixest_nthreads(0)
res<-rbindlist(lapply(specs$spec,function(from)rbindlist(lapply(specs$spec,function(to)run_one(from,to)))))

message('Merging RSS gains and constructing predicted causal gains.')
rss<-fread(rss_path)
map<-setNames(specs$spec,specs$outcome)
rss[,`:=`(from_key=map[outcome_from],to_key=map[outcome_to])]
diag_rss<-specs[,.(from_key=spec,to_key=spec,beta=1,bootstrap_se=0,J_schools=NA_integer_)]
rss<-rbind(rss[,.(from_key,to_key,beta,bootstrap_se,J_schools)],diag_rss,fill=TRUE)
res<-merge(res,rss,by=c('from_key','to_key'),all.x=TRUE,sort=FALSE)
diag_psi<-res[from_key==to_key,.(to_key,same_outcome_psi=direct_iv,same_outcome_psi_se=direct_se)]
res<-merge(res,diag_psi,by='to_key',all.x=TRUE,sort=FALSE)
res[,predicted_causal_gain:=beta*same_outcome_psi]
res[,predicted_se_independence:=sqrt((same_outcome_psi*bootstrap_se)^2+(beta*same_outcome_psi_se)^2)]
res[,difference:=direct_iv-predicted_causal_gain]
res[,`:=`(from_order=match(from_key,specs$spec),to_order=match(to_key,specs$spec))];setorder(res,from_order,to_order);res[,c('from_order','to_order'):=NULL]
fwrite(res,file.path(out_dir,'unshrunk_cross_outcome_iv_rss_validation.csv'))
off_diagonal<-res[from_key!=to_key]
summary_stats<-off_diagonal[,.(
  n_pairs=.N,
  correlation=cor(direct_iv,predicted_causal_gain),
  sign_agreement=mean(sign(direct_iv)==sign(predicted_causal_gain)),
  mean_absolute_difference=mean(abs(difference)),
  min_first_stage_f=min(first_stage_f),
  max_first_stage_f=max(first_stage_f)
)]
fwrite(summary_stats,file.path(out_dir,'unshrunk_cross_outcome_iv_rss_validation_summary.csv'))

stars<-function(p)fifelse(p<.01,'***',fifelse(p<.05,'**',fifelse(p<.10,'*','')))
cell<-function(from,to,type){x<-res[from_key==from&to_key==to];if(type=='direct')sprintf('%.3f%s',x$direct_iv,stars(x$direct_p))else sprintf('%.3f',x$predicted_causal_gain)}
header<-paste(specs$label,collapse=' & ')
panel<-function(type){unlist(lapply(specs$spec,function(k){paste0(specs[spec==k,label],' & ',paste(vapply(specs$spec,function(j)cell(k,j,type),character(1)),collapse=' & '),' \\\\')}))}
tex<-c('\\begin{table}[!htbp]','\\centering','\\caption{Cross-outcome causal gains: direct IV and RSS-implied predictions}','\\label{tab:unshrunk-cross-iv-rss-validation}','\\resizebox{\\textwidth}{!}{%','\\begin{tabular}{lccccccc}','\\toprule',paste0('Row school VA & ',header,' \\\\'),'\\midrule','\\multicolumn{8}{l}{\\textit{Panel A. Direct unshrunk cross-outcome IV}} \\\\',panel('direct'),'\\midrule','\\multicolumn{8}{l}{\\textit{Panel B. Predicted gain: RSS implied gain $\\times$ destination-outcome pass-through}} \\\\',panel('predicted'),'\\bottomrule','\\end{tabular}}','\\par\\medskip','\\footnotesize','\\begin{minipage}{\\textwidth}','Notes: Panel A instruments attended-school unshrunk value added in each row with first-round offered-school unshrunk value added and changes the student outcome across columns. All regressions control for expected unshrunk row value added, cohort, grade-4 math and verbal scores, gender, and age. Panel B multiplies the RSS latent projection from the row VA to the column VA by the unshrunk same-outcome IV pass-through for the column outcome. Stars in Panel A refer to heteroskedasticity-robust tests against zero. This table provides point-estimate validation; formal tests of equality require joint inference across the RSS and IV estimators. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.','\\end{minipage}','\\end{table}')
writeLines(tex,file.path(out_dir,'unshrunk_cross_outcome_iv_rss_validation.tex'))
print(res[,.(from_label,to_label,direct_iv,direct_se,predicted_causal_gain,difference,n_obs)])
