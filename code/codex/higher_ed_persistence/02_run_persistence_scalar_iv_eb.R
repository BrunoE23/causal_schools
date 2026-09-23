# Scalar IV persistence table using the existing observational EB school VA.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setFixest_nthreads(0)

data_root <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD', 'C:/Users/brunem/Box/causal_schools')
clean <- file.path(data_root, 'data/clean')
repo <- normalizePath('.', winslash = '/', mustWork = TRUE)
legacy_value_path <- file.path(repo, 'output/tables/empirical_bayes_school_va',
  'stata_eb_school_rbd_observational_values_for_iv.csv')
main_value_path <- file.path(clean, 'empirical_bayes_school_va', 'cohorts_2017_2020',
  'eb_school_rbd_observational_values.csv')
persistence_path <- file.path(repo, 'data/clean/higher_ed_persistence',
  'higher_ed_persistence_outcomes.csv')
program_path <- file.path(repo, 'output/tables/mifuturo_matricula_income',
  'mifuturo_person_level_income_outcomes.csv')
out_dir <- file.path(repo, 'output/tables/higher_ed_persistence')
clean_out <- file.path(repo, 'data/clean/higher_ed_persistence')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

specs <- data.table(
  spec = c('highered', 'math', 'leng', 'exam', 'highinst', 'highpay',
    'program_income', 'anypost'),
  value_outcome = c('higher_ed_enrolled_m1', 'z_year_math_max', 'z_year_leng_max',
    'admission_exam_taker', 'high_inst_m1', 'high_paying_field_m1',
    'log_program_income_clp_m1', 'any_postulacion'),
  label = c('Higher-ed enrollment', 'Math', 'Verbal', 'Admission-exam taking',
    'High-premium institution', 'High-premium field', 'Projected program income',
    'Fin. aid app.'),
  require_exam = rep(FALSE, 8)
)
focal_specs <- c('highered', 'highpay', 'highinst')
cross_spec_order <- c('highered', 'highpay', 'highinst', 'math', 'leng', 'exam',
  'program_income', 'anypost')

values_main <- fread(main_value_path, na.strings = c('', 'NA'))[
  analysis_sample == 'All' & outcome %chin% specs[spec != 'highered', value_outcome],
  .(school_rbd = as.numeric(school_rbd), outcome,
    value = controlled_value_added_eb_centered_student)]
values_highered <- fread(legacy_value_path, na.strings = c('', 'NA'))[
  analysis_sample == 'All' & outcome == 'higher_ed_enrolled_m1',
  .(school_rbd = as.numeric(school_rbd), outcome,
    value = controlled_value_added_eb_centered_student)]
values <- rbindlist(list(values_highered, values_main), use.names = TRUE)
stopifnot(!anyDuplicated(values[, .(school_rbd, outcome)]))
wide <- dcast(values, school_rbd ~ outcome, value.var = 'value')
setnames(wide, specs$value_outcome, paste0('school_value_', specs$spec))

u_cols <- c('student_id','mrun','cohort_gr8','sae_proceso','timely_sae',
  'rbd_treated_1R','most_time_RBD','GEN_ALU','EDAD_ALU','z_sim_mat_4to',
  'z_sim_leng_4to','math_max','leng_max','psu_year')
u <- fread(file.path(clean, 'univ_gr8_df.csv'), select = u_cols, na.strings = c('', 'NA'))
u[, `:=`(student_id=as.numeric(student_id),mrun=as.numeric(mrun),
  sae_proceso=as.integer(sae_proceso),rbd_treated_1R=as.numeric(rbd_treated_1R),
  most_time_RBD=as.numeric(most_time_RBD))]
u <- u[timely_sae == 1L & cohort_gr8 %in% 2018:2019]
stopifnot(!anyDuplicated(u[,.(student_id,sae_proceso)]))
u[, admission_exam_taker := as.integer(!is.na(math_max) | !is.na(leng_max))]

p <- fread(persistence_path, na.strings=c('', 'NA'))
p <- p[cohort_gr8 %in% 2018:2019]
p[, field_entry_to_y1 := fcase(
  is.na(entry_high_premium_field), NA_real_,
  entry_high_premium_field == 0, 0,
  entry_high_premium_field == 1 & is.na(high_premium_field_y1), NA_real_,
  default = entry_high_premium_field * high_premium_field_y1)]
p[, inst_entry_to_y1 := entry_high_premium_institution * high_premium_institution_y1]
p[, field_entry_through_y2 := fcase(
  followup_y2_observed == 0, NA_real_,
  is.na(entry_high_premium_field), NA_real_,
  entry_high_premium_field == 0, 0,
  entry_high_premium_field == 1 &
    (is.na(high_premium_field_y1) | is.na(high_premium_field_y2)), NA_real_,
  default = entry_high_premium_field * high_premium_field_y1 * high_premium_field_y2)]
p[, inst_entry_through_y2 := fifelse(
  followup_y2_observed == 1,
  entry_high_premium_institution * high_premium_institution_y1 *
    high_premium_institution_y2,
  NA_real_)]
p <- p[, .(mrun, entered_and_persist_y1, field_entry_to_y1, inst_entry_to_y1,
  entered_and_persist_continuous_through_y2, field_entry_through_y2,
  inst_entry_through_y2)]
u <- merge(u, p, by='mrun', all=FALSE, sort=FALSE)

prob <- rbindlist(lapply(2018:2019, function(y) {
  x <- fread(file.path(clean,'DA_probs',paste0('DA_probs_',y,'.csv')),
    select=c('student_id','school_id','prob'))
  x[, `:=`(student_id=as.numeric(student_id),sae_proceso=y,
    school_rbd=suppressWarnings(as.numeric(sub('_.*$','',school_id))))]
  x[,.(student_id,sae_proceso,school_rbd,prob)]
}))
prob <- merge(prob,wide,by='school_rbd',all.x=TRUE,sort=FALSE)
ps <- prob[,.(any_risk=as.integer(max(prob,na.rm=TRUE)<1)),by=.(student_id,sae_proceso)]
for(s in specs$spec){
  vc<-paste0('school_value_',s)
  z<-prob[,.(expected=sum(prob*fifelse(is.na(get(vc)),0,get(vc)),na.rm=TRUE)),
    by=.(student_id,sae_proceso)]
  setnames(z,'expected',paste0('expected_',s))
  ps<-merge(ps,z,by=c('student_id','sae_proceso'),all.x=TRUE,sort=FALSE)
}
rm(prob);gc(FALSE)
dt<-merge(u,ps,by=c('student_id','sae_proceso'),all=FALSE,sort=FALSE)[any_risk==1]
a<-copy(wide);setnames(a,paste0('school_value_',specs$spec),paste0('d_',specs$spec))
dt<-merge(dt,a,by.x='most_time_RBD',by.y='school_rbd',all.x=TRUE,sort=FALSE)
o<-copy(wide);setnames(o,paste0('school_value_',specs$spec),paste0('z_',specs$spec))
dt<-merge(dt,o,by.x='rbd_treated_1R',by.y='school_rbd',all.x=TRUE,sort=FALSE)
for(s in specs$spec) dt[is.na(rbd_treated_1R)|rbd_treated_1R==0,(paste0('z_',s)):=0]

outcomes <- data.table(
  spec=focal_specs,
  outcome=c('entered_and_persist_y1','field_entry_to_y1','inst_entry_to_y1')
)
outcomes_y2 <- data.table(
  spec=focal_specs,
  outcome=c('entered_and_persist_continuous_through_y2',
    'field_entry_through_y2','inst_entry_through_y2')
)

run_one <- function(r){
  sp<-r$spec;y<-r$outcome;d<-paste0('d_',sp);z<-paste0('z_',sp);e<-paste0('expected_',sp)
  meta<-specs[spec==sp]
  controls<-c('factor(cohort_gr8)','z_sim_mat_4to','z_sim_leng_4to','factor(GEN_ALU)','factor(EDAD_ALU)',e)
  needed<-c(y,d,z,e,'cohort_gr8','z_sim_mat_4to','z_sim_leng_4to','GEN_ALU','EDAD_ALU')
  reg<-dt[complete.cases(dt[,..needed])]
  if(meta$require_exam) reg<-reg[admission_exam_taker==1]
  if(uniqueN(reg$cohort_gr8) < 2L) controls <- setdiff(controls, 'factor(cohort_gr8)')
  m<-feols(as.formula(paste0(y,' ~ ',paste(controls,collapse=' + '),' | 0 | ',d,' ~ ',z)),
    data=reg,vcov='hetero',notes=FALSE)
  term<-paste0('fit_',d);if(!term%in%names(coef(m)))term<-d
  fs<-feols(as.formula(paste0(d,' ~ ',z,' + ',paste(controls,collapse=' + '))),
    data=reg,vcov='hetero',notes=FALSE)
  b<-unname(coef(m)[term]);ss<-unname(se(m)[term]);fb<-unname(coef(fs)[z]);fse<-unname(se(fs)[z])
  data.table(spec=sp,label=meta$label,outcome=y,beta=b,se=ss,
    p_value=2*pnorm(-abs(b/ss)),n_obs=nobs(m),first_stage_f=(fb/fse)^2,
    outcome_mean=mean(reg[[y]]))
}
res<-rbindlist(lapply(seq_len(nrow(outcomes)),function(i)run_one(outcomes[i])))
res_y2<-rbindlist(lapply(seq_len(nrow(outcomes_y2)),function(i)run_one(outcomes_y2[i])))
fwrite(res,file.path(clean_out,'persistence_scalar_iv_eb_results.csv'))
fwrite(res,file.path(out_dir,'persistence_scalar_iv_eb_results.csv'))
fwrite(res_y2,file.path(clean_out,'persistence_two_year_scalar_iv_eb_results.csv'))
fwrite(res_y2,file.path(out_dir,'persistence_two_year_scalar_iv_eb_results.csv'))

stars<-function(p)fifelse(p<.01,'***',fifelse(p<.05,'**',fifelse(p<.10,'*','')))
main<-res[match(focal_specs,spec)]
main[,cell:=paste0(sprintf('%.3f',beta),stars(p_value))]
theta_row<-paste0('$\\theta^{EB}$ & ',paste(main$cell,collapse=' & '),' \\\\')
se_row<-paste0(' & ',paste0('(',sprintf('%.3f',main$se),')',collapse=' & '),' \\\\')
n_row<-paste0('N & ',paste(format(main$n_obs,big.mark=','),collapse=' & '),' \\\\')
f_row<-paste0('First-stage F & ',paste(sprintf('%.1f',main$first_stage_f),collapse=' & '),' \\\\')
tex<-c('\\begin{table}[!htbp]','\\centering',
  '\\caption{School value added and persistence in higher education}',
  '\\label{tab:va-higher-ed-persistence}','\\begin{threeparttable}',
  '\\begin{tabular}{lccc}','\\toprule',
  ' & Higher-ed enrollment & High-premium field & High-premium institution \\\\',
  '\\midrule',theta_row,se_row,n_row,f_row,
  '\\bottomrule','\\end{tabular}','\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item Notes: Each column uses the EB observational school value-added measure named in its heading. Attended-school VA is instrumented with first-round offered-school VA, controlling for the DA-probability expected value of that VA, cohort, grade-4 math and verbal scores, gender, and age. The main sample contains timely SAE applicants from the 2018--2019 cohorts with nondegenerate assignment risk; admission-exam taking is not a sample restriction. Outcomes are unconditional; students who do not enter the relevant category are coded zero. Heteroskedasticity-robust standard errors are reported. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(tex,file.path(out_dir,'persistence_scalar_iv_eb_main.tex'))
main_y2<-res_y2[match(focal_specs,spec)]
main_y2[,cell:=paste0(sprintf('%.3f',beta),stars(p_value))]
theta_y2<-paste0('$\\theta^{EB}$ & ',paste(main_y2$cell,collapse=' & '),' \\\\')
se_y2<-paste0(' & ',paste0('(',sprintf('%.3f',main_y2$se),')',collapse=' & '),' \\\\')
n_y2<-paste0('N & ',paste(format(main_y2$n_obs,big.mark=','),collapse=' & '),' \\\\')
f_y2<-paste0('First-stage F & ',paste(sprintf('%.1f',main_y2$first_stage_f),collapse=' & '),' \\\\')
tex_y2<-c('\\begin{table}[!htbp]','\\centering',
  '\\caption{School value added and persistence through two years after entry}',
  '\\label{tab:va-higher-ed-persistence-two-year}','\\begin{threeparttable}',
  '\\begin{tabular}{lccc}','\\toprule',
  ' & Higher-ed enrollment & High-premium field & High-premium institution \\\\',
  '\\midrule',theta_y2,se_y2,n_y2,f_y2,'\\bottomrule','\\end{tabular}',
  '\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item Notes: Each column uses the EB observational school value-added measure named in its heading. Attended-school VA is instrumented with first-round offered-school VA, controlling for the DA-probability expected value of that VA, grade-4 math and verbal scores, gender, and age. The sample contains timely SAE applicants from the 2018 cohort with nondegenerate assignment risk; admission-exam taking is not a sample restriction. Outcomes equal one when the student enters the relevant category and remains in it in each of the following two academic years. Heteroskedasticity-robust standard errors are reported. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(tex_y2,file.path(out_dir,'persistence_two_year_scalar_iv_eb.tex'))

# Separate cross-outcome table: outcomes in columns and VA measures in rows.
cross_outcomes <- data.table(
  outcome_key = c('highered_persistence', 'highpay_persistence', 'highinst_persistence'),
  outcome = c('entered_and_persist_y1', 'field_entry_to_y1', 'inst_entry_to_y1'),
  outcome_label = c('Higher-ed enrollment', 'High-premium field', 'High-premium institution')
)
cross_grid <- CJ(spec = specs$spec, outcome_key = cross_outcomes$outcome_key, unique = TRUE)
cross_grid <- merge(cross_grid, cross_outcomes, by = 'outcome_key', sort = FALSE)
cross_results <- rbindlist(lapply(seq_len(nrow(cross_grid)), function(i) {
  ans <- run_one(cross_grid[i, .(spec, outcome)])
  ans[, `:=`(outcome_key = cross_grid$outcome_key[i],
    outcome_label = cross_grid$outcome_label[i])]
  ans
}))
cross_results[, va_label := specs$label[match(spec, specs$spec)]]
setcolorder(cross_results, c('spec','va_label','outcome_key','outcome_label','outcome',
  'beta','se','p_value','n_obs','first_stage_f','outcome_mean'))
cross_results[, `:=`(
  spec_order = match(spec, cross_spec_order),
  outcome_order = match(outcome_key, cross_outcomes$outcome_key)
)]
setorder(cross_results, spec_order, outcome_order)
cross_results[, c('spec_order', 'outcome_order') := NULL]
fwrite(cross_results, file.path(clean_out, 'persistence_second_year_cross_va_results.csv'))
fwrite(cross_results, file.path(out_dir, 'persistence_second_year_cross_va_results.csv'))

cross_results[, cell := paste0(sprintf('%.3f', beta), stars(p_value))]
cross_lines <- unlist(lapply(seq_along(cross_spec_order), function(i) {
  s <- cross_spec_order[i]
  x <- cross_results[spec == s][match(cross_outcomes$outcome_key, outcome_key)]
  lines <- c(
    paste0(x$va_label[1], ' VA & ', paste(x$cell, collapse = ' & '), ' \\\\'),
    paste0(' & ', paste0('(', sprintf('%.3f', x$se), ')', collapse = ' & '), ' \\\\')
  )
  if (i == 3L) lines <- c(lines, '\\midrule')
  lines
}))
cross_tex <- c('\\begin{table}[!htbp]','\\centering',
  '\\caption{School value added and second-academic-year persistence}',
  '\\label{tab:va-higher-ed-persistence-cross-va}','\\begin{threeparttable}',
  '\\begin{tabular}{lccc}','\\toprule',
  ' & Higher-ed enrollment & High-premium field & High-premium institution \\\\',
  '\\midrule',cross_lines,'\\bottomrule','\\end{tabular}',
  '\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item Notes: Columns report persistence outcomes requiring entry into the relevant category and continued participation in the second academic year. Rows identify the EB observational school value-added measure used as the endogenous school-quality index. In each cell, attended-school VA is instrumented with first-round offered-school VA, controlling for the DA-probability expected value of the same row VA, cohort, grade-4 math and verbal scores, gender, and age. The sample contains timely SAE applicants from the 2018--2019 cohorts with nondegenerate assignment risk; admission-exam taking is not a sample restriction. Heteroskedasticity-robust standard errors are in parentheses. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(cross_tex, file.path(out_dir, 'persistence_second_year_cross_va.tex'))
print(res)
print(res_y2)
print(cross_results)
