# Table 2 variant: retain paper definitions; add applicant continuation groups.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if(.Platform$OS.type=='windows') invisible(Sys.setlocale('LC_CTYPE','English_United States.utf8'))
root <- normalizePath('.',winslash='/')
data_root <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD','C:/Users/brunem/Box/causal_schools')
clean <- file.path(root,'data/clean/balance_table_continuation')
tables <- file.path(root,'output/tables/balance_table')
reports <- file.path(root,'output/reports/balance_table')
for(p in c(clean,tables,reports)) dir.create(p,recursive=TRUE,showWarnings=FALSE)
original <- fread(file.path(tables,'balance_table.csv'))
source_path <- file.path(data_root,'data/clean/univ_gr8_df.csv')
admin_path <- file.path(root,'data/clean/sae_participation/admin_panel.rds')
prob_paths <- file.path(data_root,'data/clean/DA_probs',paste0('DA_probs_',2018:2020,'.csv'))
fwrite(data.table(path=c(source_path,admin_path,prob_paths),
  bytes=file.info(c(source_path,admin_path,prob_paths))$size),file.path(clean,'input_manifest.csv'))
# 1.23 GB CSV: select just the Table 2 fields; no raw-data rebuild or wide probabilities.
wanted <- c('mrun','student_id','cohort_gr8','most_time_rbd','edad_alu','cod_reg_alu','gen_alu',
  'sae_proceso','timely_sae','psu_year','math_max','leng_max','cod_sies_m1',
  'income_decile_imputed','mother_educ_years_imputed','z_sim_mat_4to','z_sim_leng_4to',
  'z_gpa_middle_mean','z_att_middle_mean','high_inst_m1','high_paying_field_m1',
  'z_year_math_max','z_year_leng_max','admission_exam_taker')
header <- names(fread(source_path,nrows=0))
selected <- header[match(intersect(wanted,tolower(header)),tolower(header))]
message('Reading only Table 2 columns from the broad universe.')
d <- fread(source_path,select=selected,na.strings=c('','NA'),showProgress=FALSE)
setnames(d,tolower(names(d)))
for(v in intersect(wanted,names(d))) if(v!='cod_sies_m1') set(d,j=v,value=suppressWarnings(as.numeric(d[[v]])))
d <- d[cohort_gr8 %in% 2017:2020 & !is.na(most_time_rbd) & most_time_rbd>0 &
  !is.na(edad_alu) & edad_alu>=12 & edad_alu<=16]
setkey(d,mrun); d <- unique(d,by='mrun')
d[,`:=`(female=as.integer(gen_alu==2),in_rm=as.integer(cod_reg_alu==13))]
for(subject in c('math','leng')) {
  raw <- paste0(subject,'_max'); z <- paste0('z_year_',raw)
  if(!z %in% names(d)) d[!is.na(psu_year) & !is.na(get(raw)) & get(raw)>0,
    (z):=(get(raw)-mean(get(raw),na.rm=TRUE))/sd(get(raw),na.rm=TRUE),by=psu_year]
}
if(!'admission_exam_taker' %in% names(d)) d[,admission_exam_taker:=as.integer(
  rowSums(!is.na(.SD))>0),.SDcols=c('z_year_math_max','z_year_leng_max','math_max','leng_max')]
d[,cod_sies_m1:=trimws(as.character(cod_sies_m1))]
d[,higher_ed_enrolled_m1:=as.integer(!is.na(cod_sies_m1) & nzchar(cod_sies_m1) & cod_sies_m1!='NA')]
risk <- rbindlist(lapply(2018:2020,function(year) {
  p <- fread(file.path(data_root,'data/clean/DA_probs',paste0('DA_probs_',year,'.csv')),
    select=c('student_id','prob'),na.strings=c('','NA'),showProgress=FALSE)
  p[,student_id:=as.numeric(student_id)]
  p[,.(any_risk=as.integer(max(prob,na.rm=TRUE)<1)),by=student_id][,sae_proceso:=year]
}))
stopifnot(!anyDuplicated(risk[,.(student_id,sae_proceso)]))
d <- merge(d,risk,by=c('student_id','sae_proceso'),all.x=TRUE,sort=FALSE)
d[is.na(any_risk),any_risk:=0L]
d[,`:=`(g_broad=1L,g_sae_app=as.integer(cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L),
  g_sae_risk=as.integer(cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L & any_risk==1L))]
# Read the 160 MB cached origin classification once, then retain four columns.
admin <- readRDS(admin_path)
origin <- admin[,.(mrun,cohort_gr8,origin_rbd,continuation_group)]
rm(admin); invisible(gc())
stopifnot(!anyDuplicated(origin[,.(mrun,cohort_gr8)]))
n_before <- nrow(d)
d <- merge(d,origin,by=c('mrun','cohort_gr8'),all.x=TRUE,sort=FALSE)
stopifnot(nrow(d)==n_before,!anyDuplicated(d$mrun),!anyNA(d$continuation_group))
d[,`:=`(g_ending=as.integer(g_sae_app==1L & continuation_group=='Ends at 8 in SAE offerings'),
  g_voluntary=as.integer(g_sae_app==1L & continuation_group=='Offers grade 9'))]
groups <- c('g_broad','g_sae_app','g_sae_risk','g_ending','g_voluntary')
labels <- c('Broad panel','SAE applicants','SAE applicants with risk','SAE: school ends at grade 8','SAE: voluntary')
names(labels) <- groups
specs <- unique(original[,.(var,label)],by='var')
stats <- rbindlist(lapply(groups,function(g) {
  x <- d[get(g)==1L]
  rbindlist(lapply(seq_len(nrow(specs)),function(i) {
    v <- specs$var[i]
    data.table(group=g,var=v,label=specs$label[i],n=nrow(x),observed=sum(!is.na(x[[v]])),
      mean=mean(x[[v]],na.rm=TRUE),match=mean(!is.na(x[[v]])))
  }))
}))
# Verify all 45 preexisting Table 2 means, Ns and variable match rates exactly.
compare <- merge(stats[group %in% c('g_broad','g_sae_app','g_sae_risk')],
  original[group %in% c('g_broad','g_sae_app','g_sae_risk')],by=c('group','var'),suffixes=c('_new','_paper'))
compare[,passed:=n_new==n_paper & abs(mean_new-mean_paper)<1e-10 & abs(match_new-match_paper)<1e-10]
fwrite(compare,file.path(clean,'paper_replication.csv'))
stopifnot(nrow(compare)==45L,all(compare$passed))
counts <- data.table(group=groups,label=unname(labels),students=vapply(groups,function(g) sum(d[[g]]),numeric(1)))
counts[,percent_broad:=100*students/students[group=='g_broad']]
counts[,percent_applicants:=100*students/students[group=='g_sae_app']]
coverage <- d[g_sae_app==1L,.(students=.N),by=continuation_group]
stopifnot(sum(coverage$students)==counts[group=='g_sae_app',students],
  !any(d$g_ending==1L & d$g_voluntary==1L),all(d$g_ending<=d$g_sae_app),all(d$g_voluntary<=d$g_sae_app))
unresolved <- counts[group=='g_sae_app',students]-sum(counts[group %in% c('g_ending','g_voluntary'),students])
blocks <- list('SIMCE'=c('z_sim_mat_4to','z_sim_leng_4to','income_decile_imputed','mother_educ_years_imputed'),
  'Middle school'=c('z_gpa_middle_mean','z_att_middle_mean'))
matches <- rbindlist(lapply(groups,function(g) rbindlist(lapply(names(blocks),function(b)
  data.table(group=g,block=b,match=mean(complete.cases(d[get(g)==1L,blocks[[b]],with=FALSE])))))))
for(nm in c('stats','counts','coverage','matches')) fwrite(get(nm),file.path(clean,paste0(nm,'.csv')))

source('code/codex/balance_table/03_nonprivate_attendance.R')
source('code/codex/balance_table/02_report_continuation.R')
