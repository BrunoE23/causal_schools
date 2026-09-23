# Robustness IV tables adding SAE low-income priority status to the controls.
suppressPackageStartupMessages({library(data.table); library(fixest)})

data_wd <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD', unset='C:/Users/brunem/Box/causal_schools')
repo_wd <- Sys.getenv('CAUSAL_SCHOOLS_REPO_WD', unset=getwd())
pair_tag <- Sys.getenv('EB_IV_PAIR_TAG', unset='va_2017_2020__sae_2018_2020')
clean_dir <- file.path(data_wd,'data/clean/empirical_bayes_school_va',pair_tag)
table_dir <- file.path(repo_wd,'output/tables/empirical_bayes_school_va',pair_tag)
input_path <- Sys.getenv('EB_IV_REGRESSION_INPUT_PATH',unset=file.path(clean_dir,'scalar_iv_regression_df.csv'))

specs <- data.table(
  spec=c('math_adj_eb','leng_adj_eb','exam_adj_eb','highinst_adj_eb','highpay_adj_eb','program_income_adj_eb','anypost_adj_eb'),
  outcome=c('z_year_math_max','z_year_leng_max','admission_exam_taker','high_inst_m1','high_paying_field_m1','log_program_income_clp_m1','any_postulacion'),
  label=c('Math','Verbal','Exam taking','High-premium inst.','High-premium field','Program income','Fin. aid app.'),
  require_exam=c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE))

dt <- fread(input_path,na.strings=c('','NA'))
dt[,`:=`(mrun=as.numeric(mrun),sae_proceso=as.integer(sae_proceso))]
dt <- dt[!is.na(most_time_RBD) & most_time_RBD > 0 &
  !is.na(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16]
priority_cache_dir <- file.path(repo_wd,'data/clean/empirical_bayes_school_va',pair_tag)
dir.create(priority_cache_dir,recursive=TRUE,showWarnings=FALSE)
priority_cache <- file.path(priority_cache_dir,'prioritario_sae_2018_2020.csv')
if(file.exists(priority_cache)){
  priority <- fread(priority_cache,na.strings=c('','NA'))
}else{
  priority <- rbindlist(lapply(sort(unique(dt$sae_proceso)),function(year){
    ascii_dir <- Sys.getenv('SAE_B1_ASCII_DIR',unset='')
    files <- if(nzchar(ascii_dir)) file.path(ascii_dir,paste0('B1_',year,'.csv')) else
      Sys.glob(file.path(data_wd,'data/raw',year,paste0('SAE_',year),'B1_Postulantes_etapa_regular_*PUBL.csv'))
    files <- files[file.exists(files)]
    if(length(files)!=1L) stop('Expected one B1 applicant file for ',year,'; found ',length(files))
    x <- fread(files[[1]],select=c('mrun','cod_nivel','prioritario'),sep=';',dec=',',na.strings=c('','NA'))
    x <- x[cod_nivel==9,.(mrun=as.numeric(mrun),sae_proceso=as.integer(year),prioritario=as.integer(prioritario))]
    unique(x,by=c('mrun','sae_proceso'))
  }))
  fwrite(priority,priority_cache)
}
priority[,`:=`(mrun=as.numeric(mrun),sae_proceso=as.integer(sae_proceso),prioritario=as.integer(prioritario))]
stopifnot(!anyDuplicated(priority[,.(mrun,sae_proceso)]))
dt <- merge(dt,priority,by=c('mrun','sae_proceso'),all.x=TRUE,sort=FALSE)
coverage <- dt[,.(n=.N,n_priority_observed=sum(!is.na(prioritario)),share_priority_observed=mean(!is.na(prioritario)),share_prioritario=mean(prioritario,na.rm=TRUE))]
print(coverage)
if(coverage$share_priority_observed<.95) warning('Prioritario coverage is below 95%; robustness samples require observed status.')

run_spec <- function(row){
  s<-row$spec;y<-row$outcome;d<-paste0('d_',s);z<-paste0('z_',s);e<-paste0('expected_',s)
  controls<-c('factor(cohort_gr8)','z_sim_mat_4to','z_sim_leng_4to','factor(GEN_ALU)','factor(EDAD_ALU)','prioritario',e)
  needed<-c(y,d,z,e,'cohort_gr8','z_sim_mat_4to','z_sim_leng_4to','GEN_ALU','EDAD_ALU','prioritario')
  if(row$require_exam) needed<-c(needed,'admission_exam_taker')
  reg<-dt[complete.cases(dt[,..needed])]
  if(row$require_exam) reg<-reg[admission_exam_taker==1L]
  iv<-feols(as.formula(paste0(y,' ~ ',paste(controls,collapse=' + '),' | 0 | ',d,' ~ ',z)),data=reg,vcov='hetero',notes=FALSE)
  fs<-feols(as.formula(paste0(d,' ~ ',z,' + ',paste(controls,collapse=' + '))),data=reg,vcov='hetero',notes=FALSE)
  term<-paste0('fit_',d);if(!term%chin%names(coef(iv)))term<-d
  psi<-unname(coef(iv)[term]);psi_se<-unname(se(iv)[term]);kappa<-unname(coef(fs)[z]);kappa_se<-unname(se(fs)[z])
  data.table(spec=s,outcome_group=row$label,psi,psi_se,psi_p_value=2*pnorm(-abs(psi/psi_se)),kappa,kappa_se,kappa_p_value=2*pnorm(-abs(kappa/kappa_se)),first_stage_f=(kappa/kappa_se)^2,n_obs=nobs(iv))
}
setFixest_nthreads(0)
results<-rbindlist(lapply(seq_len(nrow(specs)),function(i)run_spec(specs[i])))
fwrite(results,file.path(table_dir,'main_results_prioritario_control.csv'))
stars<-function(p)fifelse(p<.01,'***',fifelse(p<.05,'**',fifelse(p<.10,'*','')))
header<-c(' & \\multicolumn{3}{c}{Exams} & \\multicolumn{3}{c}{Higher ed. choices} & Financial aid \\\\','\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-8}',' & Math & Verbal & Exam taking & High-premium inst. & High-premium field & Program income & Fin. aid app. \\\\')
make_table<-function(kind=c('psi','kappa')){
  kind<-match.arg(kind)
  if(kind=='psi'){est<-results$psi;std<-results$psi_se;p<-results$psi_p_value;symbol<-'\\psi^{EB}';caption<-'Main EB school-value IV estimates controlling for SAE priority status';label<-'tab:scalar-school-value-iv-main-seven-eb-prioritario';filename<-'main_table_prioritario_control.tex';extra<-character()
  }else{est<-results$kappa;std<-results$kappa_se;p<-results$kappa_p_value;symbol<-'\\kappa^{EB}';caption<-'First-stage coefficients controlling for SAE priority status';label<-'tab:scalar-school-value-iv-main-seven-eb-first-stage-prioritario';filename<-'first_stage_kappa_table_prioritario_control.tex';extra<-paste0('First-stage F & ',paste(sprintf('%.1f',results$first_stage_f),collapse=' & '),' \\\\')}
  tex<-c('\\begin{table}[!htbp]','\\centering',paste0('\\caption{',caption,'}'),paste0('\\label{',label,'}'),'\\resizebox{\\textwidth}{!}{%','\\begin{tabular}{lccccccc}','\\toprule',header,'\\midrule',paste0('$',symbol,'$ & ',paste(paste0(sprintf('%.3f',est),stars(p)),collapse=' & '),' \\\\'),paste0(' & ',paste(sprintf('(%.3f)',std),collapse=' & '),' \\\\'),extra,paste0('N & ',paste(format(results$n_obs,big.mark=',',trim=TRUE),collapse=' & '),' \\\\'),'\\bottomrule','\\end{tabular}','}','\\par\\medskip','\\footnotesize','\\begin{minipage}{\\textwidth}','Notes: This robustness specification adds the SAE low-income priority indicator, \\textit{prioritario}, to the controls used in the corresponding main specification. It also controls for the DA-probability expected value of the same EB measure, cohort, grade-4 SIMCE math and language, gender, and age. The sample is restricted to students with observed priority status. Heteroskedasticity-robust standard errors are reported in parentheses. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.','\\end{minipage}','\\end{table}')
  writeLines(tex,file.path(table_dir,filename))
}
make_table('psi');make_table('kappa');print(results)
