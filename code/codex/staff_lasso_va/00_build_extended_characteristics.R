# Resources, pre-HS student composition and contemporaneous staff age.
# Reads only identifiers/baseline columns from the VA cache; no outcome values.
# Never modifies source data.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
root <- normalizePath('.',winslash='/')
out <- file.path(root,'data/clean/staff_lasso_va')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(all(args %in% c('--preflight','--overwrite')))
data_root <- 'C:/Users/brunem/Box/causal_schools/data/clean'
paths <- c(universe=file.path(data_root,'univ_gr8_df.csv'),
  analytic_cache=file.path(root,'output/tables/empirical_bayes_school_va/stata_va_analytic_cache.dta'),
  middle=file.path(data_root,'middle_school_controls/middle_school_controls.csv'),
  funding_year=file.path(data_root,'school_expenditure_values/school_public_funding_per_student_year.csv'),
  funding_period=file.path(data_root,'school_expenditure_values/school_public_funding_per_student_2017_2021.csv'),
  members=file.path(root,'data/clean/titulados_staff_linkage/staff_membership_person_school_year.csv.gz'),
  staff_roster=file.path(root,'data/clean/staff_quality_va/staff_school_year_roster.csv'),
  leader_roster=file.path(root,'data/clean/leadership_quality_va/leadership_school_year_roster.csv'),
  staff_manifest=file.path(root,'data/clean/leadership_quality_va/leadership_source_manifest.csv'),
  va_counts=file.path(root,'output/tables/empirical_bayes_school_va/stata_va_eb_input_exam.csv'))
sm <- fread(paths[['staff_manifest']])[AGNO %in% 2018:2024]
stopifnot(identical(sm$AGNO,2018:2024),all(file.exists(paths)))
paths <- c(paths,setNames(sm$SOURCE_PATH,paste0('birth_',sm$AGNO)))
print(data.table(SOURCE=names(paths),MB=round(file.info(paths)$size/1024^2,1)))
# The 1 GB universe is read once with selected baseline columns, then filtered.
# Annual staff files (~100 MB each) are read sequentially, retaining only DOB/ID.
for(i in seq_len(nrow(sm))) {
  h <- names(fread(sm$SOURCE_PATH[i],sep=';',nrows=0L))
  stopifnot(all(c('MRUN','DOC_FEC_NAC') %in% h))
}
if('--preflight' %in% args) quit(save='no')
if(file.exists(file.path(out,'extended_school_predictors.csv')) && !'--overwrite' %in% args)
  stop('Extension outputs exist; use --overwrite.')
hash <- tools::md5sum(paths)
va <- fread(paths[['va_counts']],select=c('school_rbd','n_students','analysis_sample','outcome'))
stopifnot(!anyDuplicated(va$school_rbd),all(va$analysis_sample=='All'),
  all(va$outcome=='admission_exam_taker'),nrow(va)==3682L,sum(va$n_students)==757999L)
setnames(va,c('school_rbd','n_students'),c('RBD','N_EXPECTED'))
setorder(va,RBD)
x <- va[,.(RBD)]; dict <- list()
add <- function(name,value,label,role='school',block='composition') {
  stopifnot(length(value)==nrow(x),!name %in% names(x))
  value <- as.numeric(value); value[!is.finite(value)] <- NA_real_
  set(x,j=name,value=value)
  dict[[length(dict)+1L]] <<- data.table(FEATURE=name,LABEL=label,ROLE=role,BLOCK=block)
}
avg <- function(v) if(any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_

message(format(Sys.time(),'%H:%M:%S'),' Reconstructing the broad VA sample from baseline columns.')
cpad <- c('father_educ_years_imputed','mother_educ_years_imputed','father_indigenous_imputed',
  'mother_indigenous_imputed','sala_cuna_imputed','jardin_imputed','prekinder_imputed','kinder_imputed')
controls <- c('cohort_gr8','GEN_ALU','EDAD_ALU','COD_COM_ALU','income_decile_imputed',cpad,
  'z_sim_mat_4to','z_sim_leng_4to','z_gpa_middle_mean','z_att_middle_mean','middle_years_observed','most_time_RBD_middle')
impute <- c('income_mid_was_imputed','father_educ_years_was_imputed','mother_educ_years_was_imputed')
ucols <- unique(c('MRUN','most_time_RBD',setdiff(controls,c('z_gpa_middle_mean','z_att_middle_mean',
  'middle_years_observed','most_time_RBD_middle')),impute))
u <- fread(paths[['universe']],select=ucols,colClasses=c(MRUN='character'))
stopifnot(!anyDuplicated(u$MRUN))
u <- u[cohort_gr8 %in% 2017:2020 & EDAD_ALU %between% c(12,16) & most_time_RBD>0]
mc <- fread(paths[['middle']],select=c('MRUN','z_gpa_middle_mean','z_att_middle_mean',
  'middle_years_observed','most_time_RBD_middle'),colClasses=c(MRUN='character'))
stopifnot(!anyDuplicated(mc$MRUN))
u <- merge(u,mc,by='MRUN',all.x=TRUE,sort=FALSE); rm(mc)
u <- u[complete.cases(u[,..controls])]
# The saved estimation cache is authoritative; current CSV counts differ by two.
# Compare current CSV reconstruction, but build from the actual estimation inputs.
csv_counts <- u[,.(N_CURRENT_CSV=.N),by=.(RBD=most_time_RBD)]
u <- as.data.table(haven::read_dta(paths[['analytic_cache']],
  col_select=tidyselect::all_of(c('MRUN','school_rbd',controls,impute))))
u[,MRUN:=as.character(MRUN)]
u <- u[cohort_gr8 %in% 2017:2020 & complete.cases(u[,..controls])]
setnames(u,'school_rbd','most_time_RBD')
setnames(u,'most_time_RBD','RBD')
counts <- merge(va[,.(RBD,N_EXPECTED)],u[,.(N_BUILT=.N),by=RBD],by='RBD',all=TRUE)
counts <- merge(counts,csv_counts,by='RBD',all=TRUE)
fwrite(counts,file.path(out,'composition_sample_reconciliation.csv'))
stopifnot(nrow(u)==757999L,all(counts$N_EXPECTED==counts$N_BUILT),!anyNA(counts))
stopifnot(all(u$GEN_ALU %in% 0:2),all(u$income_decile_imputed %in% 1:10),
  all(as.matrix(u[,..impute]) %in% 0:1))
# Save a thin, outcome-free cache for an independent aggregation check.
fwrite(u,file.path(out,'composition_student_inputs.csv.gz'),na='NA')
comp <- u[,.(N_STUDENTS=.N,N_COHORTS=uniqueN(cohort_gr8),
  math_mean=mean(z_sim_mat_4to),math_sd=sd(z_sim_mat_4to),
  language_mean=mean(z_sim_leng_4to),language_sd=sd(z_sim_leng_4to),
  income_decile_mean=mean(income_decile_imputed),low_income_share=mean(income_decile_imputed<=2),
  high_income_share=mean(income_decile_imputed>=9),
  father_education=mean(father_educ_years_imputed),mother_education=mean(mother_educ_years_imputed),
  female_share=avg(ifelse(GEN_ALU %in% 1:2,as.numeric(GEN_ALU==2),NA_real_)),
  N_KNOWN_SEX=sum(GEN_ALU %in% 1:2),grade8_age_mean=mean(EDAD_ALU),
  income_imputed_share=mean(income_mid_was_imputed),
  father_education_imputed_share=mean(father_educ_years_was_imputed),
  mother_education_imputed_share=mean(mother_educ_years_was_imputed)),by=RBD]
comp <- comp[match(x$RBD,RBD)]
labels <- c(math_mean='Mean pre-HS grade-4 math (SD units)',math_sd='Within-school SD of pre-HS grade-4 math',
  language_mean='Mean pre-HS grade-4 language (SD units)',language_sd='Within-school SD of pre-HS grade-4 language',
  income_decile_mean='Mean baseline household income decile (observed/imputed)',
  low_income_share='Baseline income bottom-two-decile share (observed/imputed)',
  high_income_share='Baseline income top-two-decile share (observed/imputed)',
  father_education='Mean father education years (observed/imputed)',
  mother_education='Mean mother education years (observed/imputed)',
  female_share='Female student share among known sex',grade8_age_mean='Mean student age in grade 8',
  income_imputed_share='Baseline household income imputed share',
  father_education_imputed_share='Father education imputed share',
  mother_education_imputed_share='Mother education imputed share')
for(nm in names(labels)) add(paste0('school__composition_',nm),comp[[nm]],labels[[nm]],
  block=if(grepl('imputed_share',nm)) 'coverage' else 'composition')
fwrite(comp,file.path(out,'school_student_composition.csv'),na='NA')
rm(u); invisible(gc())

message(format(Sys.time(),'%H:%M:%S'),' Building public-resource measures, 2017-2021.')
fy <- fread(paths[['funding_year']])[school_rbd %in% x$RBD]
fp <- fread(paths[['funding_period']])[school_rbd %in% x$RBD]
stopifnot(!anyDuplicated(fy,by=c('school_rbd','year')),all(fy$year %in% 2017:2021),
  !anyDuplicated(fp$school_rbd))
# Replicate existing observed-year aggregate before applying a stricter coverage gate.
recon <- fy[is.finite(public_funding_per_student),.(
  CHECK_LEVEL=sum(total_public_funding_components_2021_pesos,na.rm=TRUE)/sum(avg_monthly_enrollment,na.rm=TRUE)),by=school_rbd]
recon <- merge(recon,fp,by='school_rbd')
stopifnot(all(abs(recon$CHECK_LEVEL-recon$enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021)<1e-6))
fy[, VALID_FULL_YEAR:=n_months_observed==12 & n_month_rows==12 &
  is.finite(avg_monthly_enrollment) & avg_monthly_enrollment>0 &
  is.finite(total_public_funding_components_2021_pesos) & total_public_funding_components_2021_pesos>=0]
res <- fy[,.(N_RECORD_YEARS=.N,N_FULL_YEARS=sum(VALID_FULL_YEAR),
  LEVEL=if(all(VALID_FULL_YEAR) && .N==5L) sum(total_public_funding_components_2021_pesos)/sum(avg_monthly_enrollment) else NA_real_,
  CHANGE=if(all(VALID_FULL_YEAR) && .N==5L) mean(public_funding_per_student_2021_pesos[year>=2019])-
    mean(public_funding_per_student_2021_pesos[year<=2018]) else NA_real_),by=.(RBD=school_rbd)]
res <- merge(x[,.(RBD)],res,by='RBD',all.x=TRUE)
res[is.na(N_RECORD_YEARS),`:=`(N_RECORD_YEARS=0L,N_FULL_YEARS=0L)]
add('school__public_funding_level_millions',res$LEVEL/1e6,'Public funding per student, 2017-2021 (million 2021 CLP)',block='resources')
add('school__log_public_funding_level',ifelse(res$LEVEL>0,log(res$LEVEL),NA_real_),
  'Log public funding per student, 2017-2021',block='resources')
add('school__public_funding_change_millions',res$CHANGE/1e6,
  'Public funding change: 2019-2021 vs 2017-2018 (million 2021 CLP/student)',block='resources')
add('school__funding_record_year_share',res$N_RECORD_YEARS/5,'Share of 2017-2021 years with funding records',block='coverage')
add('school__funding_full_year_share',res$N_FULL_YEARS/5,'Share of 2017-2021 years with complete funding records',block='coverage')
fwrite(res,file.path(out,'school_resources.csv'),na='NA')
fwrite(fy,file.path(out,'school_resources_year_audit.csv'),na='NA')

message(format(Sys.time(),'%H:%M:%S'),' Building age from each contemporaneous staff file.')
members <- fread(paths[['members']],colClasses=c(MRUN='character'))[ROLE!='Non-HS teachers']
stopifnot(!anyDuplicated(members,by=c('MRUN','RBD','ROLE','AGNO')),all(members$AGNO %in% 2018:2024))
age_parts <- list(); birth_audit <- list()
for(i in seq_len(nrow(sm))) {
  year <- sm$AGNO[i]
  mm <- members[AGNO==year]
  b <- fread(sm$SOURCE_PATH[i],sep=';',select=c('MRUN','DOC_FEC_NAC'),colClasses='character')
  b <- unique(b[MRUN %chin% mm$MRUN])
  b[, `:=`(YOB=suppressWarnings(as.integer(substr(DOC_FEC_NAC,1,4))),
    MONTH=suppressWarnings(as.integer(substr(DOC_FEC_NAC,5,6))))]
  b[, VALID:=!is.na(DOC_FEC_NAC) & grepl('^[0-9]{6}$',DOC_FEC_NAC) & MONTH %in% 1:12 &
    DOC_FEC_NAC!='190001' & year-YOB>=18 & year-YOB<100]
  # No backfilling from 2024 survivors and no arbitrary choice between valid conflicts.
  bp <- b[,.(N_VALID_DATES=uniqueN(DOC_FEC_NAC[VALID]),
    AGE=if(uniqueN(DOC_FEC_NAC[VALID])==1L) year-YOB[VALID][1L] else NA_integer_),by=MRUN]
  aa <- merge(mm,bp,by='MRUN',all.x=TRUE)
  stopifnot(nrow(aa)==nrow(mm))
  aa[is.na(N_VALID_DATES),N_VALID_DATES:=0L]
  age_parts[[as.character(year)]] <- aa
  birth_audit[[as.character(year)]] <- data.table(AGNO=year,N_STAFF=uniqueN(mm$MRUN),
    N_VALID=sum(is.finite(bp$AGE)),N_CONFLICT=sum(bp$N_VALID_DATES>1),
    N_INVALID_OR_UNRECOGNIZED_DATE_ROWS=sum(!b$VALID),
    N_EIGHT_DIGIT_DATES=sum(grepl('^[0-9]{8}$',b$DOC_FEC_NAC)))
  message('  ',year,': ',sum(is.finite(bp$AGE)),' valid person ages; ',sum(bp$N_VALID_DATES>1),' conflicts.')
}
ap <- rbindlist(age_parts)
fwrite(ap,file.path(out,'staff_age_person_role_school_year.csv.gz'),na='NA')
fwrite(rbindlist(birth_audit),file.path(out,'staff_age_birthdate_audit.csv'))
sr <- fread(paths[['staff_roster']]); lr <- fread(paths[['leader_roster']])
annual_parts <- list(); period_parts <- list()
age_fields <- c('age_mean','age_sd','age_under35_share','age_35_49_share','age_50plus_share')
age_labels <- c('Mean age attained in staff year','Mean within-year age SD (years with 2+ valid ages)',
  'Staff younger than 35 share','Staff age 35-49 share','Staff age 50+ share')
for(role in c('teacher','counselor','leadership')) {
  label <- c(teacher='HS teachers',counselor='Orientadores',leadership='Leadership')[[role]]
  rr <- if(role=='leadership') lr[,.(RBD,AGNO,N_ROLE)] else
    sr[,.(RBD,AGNO,N_ROLE=get(if(role=='teacher') 'N_TEACHER' else 'N_COUNSELOR'))]
  aa <- merge(CJ(RBD=x$RBD,AGNO=2018:2024),rr,by=c('RBD','AGNO'),all.x=TRUE)
  aa <- merge(aa,ap[ROLE==label,.(N_MEMBERS=.N,N_VALID_AGE=sum(is.finite(AGE)),
    N_AGE_CONFLICT=sum(N_VALID_DATES>1),age_mean=avg(AGE),age_sd=sd(AGE,na.rm=TRUE),
    age_under35_share=avg(as.numeric(AGE<35)),age_35_49_share=avg(as.numeric(AGE>=35 & AGE<50)),
    age_50plus_share=avg(as.numeric(AGE>=50))),by=.(RBD,AGNO)],by=c('RBD','AGNO'),all.x=TRUE)
  aa[N_ROLE==0 & is.na(N_MEMBERS),`:=`(N_MEMBERS=0L,N_VALID_AGE=0L,N_AGE_CONFLICT=0L)]
  stopifnot(all(aa[!is.na(N_ROLE),N_ROLE==N_MEMBERS]))
  aa[, age_valid_share:=fifelse(N_ROLE>0,N_VALID_AGE/N_ROLE,NA_real_)]
  # Annual valid-age means are averaged equally across active years, matching credentials.
  pp <- aa[,c(list(N_KNOWN=sum(!is.na(N_ROLE)),N_ACTIVE=sum(N_ROLE>0,na.rm=TRUE),
    N_AGE_YEARS=sum(is.finite(age_mean))),lapply(.SD,avg)),by=RBD,.SDcols=c(age_fields,'age_valid_share')]
  pp[N_KNOWN<7 | N_ACTIVE==0,(c(age_fields,'age_valid_share')):=NA_real_]
  pp[,age_year_coverage:=fifelse(N_KNOWN==7 & N_ACTIVE>0,N_AGE_YEARS/N_ACTIVE,NA_real_)]
  # Means/shares need at least one valid age in every active year; never silently omit a year.
  pp[N_AGE_YEARS<N_ACTIVE,(age_fields):=NA_real_]
  pp <- pp[match(x$RBD,RBD)]
  for(j in seq_along(age_fields)) {
    nm <- age_fields[j]
    # Middle age share retained in reusable data but omitted from regression as complement.
    if(nm!='age_35_49_share') add(paste0(role,'__',nm),pp[[nm]],age_labels[j],role,'age')
  }
  add(paste0(role,'__age_valid_share'),pp$age_valid_share,'Valid birthdate share, active-year average',role,'coverage')
  add(paste0(role,'__age_year_coverage'),pp$age_year_coverage,'Share of active staff years with any valid age',role,'coverage')
  aa[,ROLE:=role]; pp[,ROLE:=role]
  annual_parts[[role]] <- aa; period_parts[[role]] <- pp
}
fwrite(rbindlist(annual_parts),file.path(out,'staff_age_school_year.csv'),na='NA')
fwrite(rbindlist(period_parts),file.path(out,'staff_age_school_period.csv'),na='NA')
dd <- rbindlist(dict)
dd[,N_OBSERVED:=vapply(FEATURE,function(nm) sum(is.finite(x[[nm]])),integer(1))]
dd[,N_UNIQUE_OBSERVED:=vapply(FEATURE,function(nm) uniqueN(x[[nm]][is.finite(x[[nm]])]),integer(1))]
stopifnot(nrow(dd)==37L,!anyDuplicated(dd$FEATURE),!anyDuplicated(x$RBD),
  identical(hash,tools::md5sum(paths)))
fwrite(x,file.path(out,'extended_school_predictors.csv'),na='NA')
fwrite(dd,file.path(out,'extended_predictor_dictionary.csv'))
fwrite(data.table(SOURCE=names(paths),PATH=unname(paths),MD5=unname(hash)),file.path(out,'extended_source_manifest.csv'))
print(dd[,.(N_FEATURES=.N,MIN_OBSERVED=min(N_OBSERVED),MAX_OBSERVED=max(N_OBSERVED)),by=.(ROLE,BLOCK)])
cat('Extension complete: exact VA counts, unique rosters, contemporaneous ages and unchanged sources.\n')
