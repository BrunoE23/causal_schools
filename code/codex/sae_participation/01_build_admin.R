source('code/codex/sae_participation/common.R')
if(file.exists(file.path(out,'admin_panel.rds')) && !'--overwrite' %in% commandArgs()) stop('Output exists; use --overwrite to rebuild.')

# Keep all baseline student records, including students with no questionnaire or score.
simce <- rbindlist(lapply(2013:2016,function(year) {
  log_message('Baseline SIMCE ',year)
  p <- one_file(file.path(simce_folder(year),'Archivos TXT (Planos)'),'alu.*csv$')
  z <- read_cols(p,c('agno','mrun','idalumno','rbd','ptje_mate4b_alu','ptje_lect4b_alu'),'|')
  setnames(z,c('agno','rbd','ptje_mate4b_alu','ptje_lect4b_alu'),c('simce_year','simce_rbd','simce_math','simce_language'))
  for (v in c('simce_math','simce_language')) {
    set(z,j=v,value=suppressWarnings(as.numeric(z[[v]])))
    z[get(v)<=0,(v):=NA_real_]
    set(z,j=paste0(v,'_national_z'),value=safe_z(z[[v]]))
  }
  z
}),fill=TRUE)
simce <- simce[!is.na(mrun) & mrun>0]
simce[, score_n:=rowSums(!is.na(.SD)),.SDcols=c('simce_math','simce_language')]
simce[, score_avg:=rowMeans(.SD,na.rm=TRUE),.SDcols=c('simce_math','simce_language')]
setorder(simce,mrun,simce_year,-score_n,-score_avg,idalumno)
simce <- unique(simce,by='mrun')
simce[,c('score_n','score_avg'):=NULL]
saveRDS(simce,file.path(out,'baseline_simce.rds'))

univ <- read_cols(file.path(data_root,'data/clean/universe.csv'),c('MRUN','cohort_gr8'))
univ <- unique(univ[cohort_gr8 %in% 2017:2020 & mrun>0],by=c('mrun','cohort_gr8'))
stopifnot(!anyDuplicated(univ$mrun))
audits <- list(); records <- list(); peer_audits <- list()
for(year in 2013:2020) {
  log_message('Reading selected tracking columns, ',year)
  cache <- file.path(out,paste0('tracking_features_',year,'.rds'))
  if(file.exists(cache) && !'--overwrite' %in% commandArgs()) {
    records[[as.character(year)]] <- readRDS(cache)
    next
  }
  p <- one_file(file.path(data_root,'data/raw/student_tracking',year,paste0('Rendimiento-',year)),'csv$')
  z <- read_cols(p,c('AGNO','MRUN','RBD','COD_ENSE','COD_GRADO','PROM_GRAL','ASISTENCIA','SIT_FIN','SIT_FIN_R','COD_REG_RBD','COD_DEPE','GEN_ALU','EDAD_ALU'),';',',')
  raw_n <- nrow(z)
  z <- unique(z[cod_ense==110 & cod_grado %in% 5:8 & !is.na(sit_fin) & mrun>0 & rbd>0])
  for(v in c('prom_gral','asistencia')) set(z,j=v,value=suppressWarnings(as.numeric(z[[v]])))
  z[, gpa:=fifelse(prom_gral>=1 & prom_gral<=7,prom_gral,NA_real_)]
  # Preserve literal zero attendance: the source dictionary defines a percentage,
  # not a missing-value code. Zero is also retained in the data-quality audit.
  z[, attendance:=fifelse(asistencia>=0 & asistencia<=100,asistencia,NA_real_)]
  z[, `:=`(valid_att=as.integer(!is.na(attendance)&attendance>0),passed=as.integer(sit_fin=='P'))]
  z[, school_records_year:=uniqueN(rbd),by=mrun]
  setorderv(z,c('mrun','rbd','cod_grado','valid_att','passed','attendance'),c(1,1,1,-1,-1,-1),na.last=TRUE)
  z <- unique(z,by=c('mrun','rbd','cod_grado'))
  z[simce,on='mrun',`:=`(simce_year=i.simce_year,baseline_math=i.simce_math_national_z,baseline_language=i.simce_language_national_z)]
  z[is.na(simce_year)|simce_year>=year,`:=`(baseline_math=NA_real_,baseline_language=NA_real_)]
  # Full school-grade-year rosters, before selecting the target student universe.
  z[, `:=`(z_gpa=safe_z(gpa),z_attendance=safe_z(attendance),
           z_baseline_math=safe_z(baseline_math),z_baseline_language=safe_z(baseline_language),
           peer_n=.N,peer_simce_n=sum(!is.na(baseline_math))),by=.(rbd,cod_grado)]
  z[, `:=`(gap_math=z_gpa-z_baseline_math,gap_language=z_gpa-z_baseline_language)]
  peer_audits[[as.character(year)]] <- z[,.(year=year,school_grade_cells=uniqueN(paste(rbd,cod_grado)),
    student_school_records=.N,gpa_nonmissing=sum(!is.na(gpa)),attendance_zero=sum(attendance==0,na.rm=TRUE),
    attendance_nonmissing=sum(!is.na(attendance)),baseline_math_nonmissing=sum(!is.na(baseline_math)))]
  z <- merge(z,univ,by='mrun',all=FALSE)
  z <- z[agno<=cohort_gr8]
  z[, canonical_year:=FALSE]
  setorderv(z,c('mrun','valid_att','passed','attendance','rbd','cod_grado'),c(1,-1,-1,-1,1,-1),na.last=TRUE)
  z[!duplicated(mrun),canonical_year:=TRUE]
  z[, c('valid_att','passed','prom_gral','asistencia','baseline_math','baseline_language','simce_year'):=NULL]
  saveRDS(z,cache)
  records[[as.character(year)]] <- z
  audits[[as.character(year)]] <- data.table(year,raw_rows=raw_n,target_school_records=nrow(z),students=uniqueN(z$mrun),multi_school_students=uniqueN(z[school_records_year>1]$mrun))
  rm(z); gc(verbose=FALSE)
}
long <- rbindlist(records,fill=TRUE)
saveRDS(long,file.path(out,'all_student_school_records.rds'))
if(length(audits)) audit_write(rbindlist(audits),'tracking_audit')
if(length(peer_audits)) audit_write(rbindlist(peer_audits),'peer_roster_audit')

# The year-end grade-8 origin is explicit; no post-grade-8 school information enters.
origin <- long[agno==cohort_gr8 & cod_grado==8]
setorderv(origin,c('mrun','canonical_year','attendance','rbd'),c(1,-1,-1,1),na.last=TRUE)
origin <- unique(origin,by='mrun')
origin <- origin[,.(mrun,cohort_gr8,origin_rbd=rbd,origin_region=cod_reg_rbd,origin_sector=cod_depe,gender=gen_alu,age=edad_alu,
                    origin_multischool=as.integer(school_records_year>1))]
panel <- merge(univ,origin,by=c('mrun','cohort_gr8'),all.x=TRUE)
hist <- long[canonical_year==TRUE]
setorder(hist,mrun,cod_grado,-agno)
hist <- unique(hist,by=c('mrun','cod_grado'))
hist[, repeat_grade:=as.integer(agno < cohort_gr8-(8-cod_grado))]
values <- c('gpa','attendance','z_gpa','z_attendance','gap_math','gap_language','peer_n','peer_simce_n','school_records_year','rbd','agno')
wide <- dcast(hist,mrun~cod_grado,value.var=values,sep='_g')
panel <- merge(panel,wide,by='mrun',all.x=TRUE)
panel <- merge(panel,simce[,.(mrun,simce_year,simce_rbd,simce_math_national_z,simce_language_national_z)],by='mrun',all.x=TRUE)
panel[, same_simce_origin:=fifelse(!is.na(simce_rbd)&!is.na(origin_rbd),as.integer(simce_rbd==origin_rbd),NA_integer_)]

offers <- list(); apps <- list(); coverage <- list(); app_audit <- list()
for(year in 2017:2020) {
  log_message('SAE applications and offerings ',year)
  folder <- file.path(data_root,'data/raw',year,paste0('SAE_',year))
  directory <- read_cols(one_file(file.path(data_root,'data/raw/school_directory',year),'csv$'),c('RBD','COD_REG_RBD','COD_DEPE'),';')
  stopifnot(!anyDuplicated(directory$rbd))
  of <- read_cols(one_file(folder,'^A1_.*csv$'),c('rbd','cod_nivel','cupos_totales'),';')
  schools <- of[,.(max_offered_grade=max(cod_nivel,na.rm=TRUE),offers_grade9=as.integer(any(cod_nivel==9)),offers_grade8=as.integer(any(cod_nivel==8))),by=rbd]
  # Early rollout lists only entry grades in some regions. For a school where
  # the target student attends grade 8, no offered secondary grade implies no
  # secondary continuation in SAE; requiring a listed grade-8 row would fail.
  schools[, `:=`(cohort_gr8=year,ends8_sae=as.integer(max_offered_grade<=8))]
  offers[[as.character(year)]] <- schools
  served <- unique(directory[rbd %in% of[cod_nivel==9]$rbd]$cod_reg_rbd)
  # Directory is dated April; year-end tracking uses Nuble's new September-2018
  # region code 16. Those communes were in covered region 8 at directory date.
  if(year==2018L && 8L %in% served) served <- union(served,16L)
  coverage[[as.character(year)]] <- data.table(cohort_gr8=year,origin_region=served,sae_region_covered=1L)
  for(stage in c('regular','complementary')) {
    prefix <- if(stage=='regular') '^C1_' else '^C2_'
    a <- read_cols(one_file(folder,paste0(prefix,'.*csv$')),c('mrun','cod_nivel','agregada_por_continuidad'),';')
    a <- a[cod_nivel==9 & mrun>0]
    app_audit[[paste(year,stage)]] <- a[,.(year,stage,grade9_rows=.N,students_any_row=uniqueN(mrun),students_explicit_choice=uniqueN(mrun[agregada_por_continuidad==0]))]
    a <- unique(a[agregada_por_continuidad==0,.(mrun)])
    a[, `:=`(cohort_gr8=year,stage=stage)]
    apps[[paste(year,stage)]] <- a
  }
}
of <- rbindlist(offers); cov <- rbindlist(coverage); a <- rbindlist(apps)
audit_write(of,'school_offerings'); audit_write(cov,'region_year_coverage'); audit_write(rbindlist(app_audit),'application_audit')
a <- a[,.(applied_sae=1L,applied_regular=as.integer(any(stage=='regular')),applied_complementary=as.integer(any(stage=='complementary'))),by=.(mrun,cohort_gr8)]
panel <- merge(panel,a,by=c('mrun','cohort_gr8'),all.x=TRUE)
for(v in c('applied_sae','applied_regular','applied_complementary')) set(panel,which(is.na(panel[[v]])),v,0L)
panel <- merge(panel,of,by.x=c('origin_rbd','cohort_gr8'),by.y=c('rbd','cohort_gr8'),all.x=TRUE)
panel <- merge(panel,cov,by=c('cohort_gr8','origin_region'),all.x=TRUE)
panel[is.na(sae_region_covered),sae_region_covered:=0L]
panel[, analysis_eligible:=as.integer(sae_region_covered==1L & !is.na(origin_rbd))]
panel[, continuation_group:=fcase(ends8_sae==1L,'Ends at 8 in SAE offerings',offers_grade9==1L,'Offers grade 9',!is.na(ends8_sae),'Other SAE offering',default='Not resolved in SAE offerings')]
setorder(panel,mrun)
stopifnot(nrow(panel)==nrow(univ),!anyDuplicated(panel$mrun),all(panel$applied_regular<=panel$applied_sae))
audit_write(panel[,.(students=.N,applicants=sum(applied_sae),regular=sum(applied_regular),application_rate=mean(applied_sae)),by=.(cohort_gr8,analysis_eligible,continuation_group)],'sample_summary')
audit_write(panel[,.(students=.N,applicants=sum(applied_sae)),by=.(cohort_gr8,origin_sector,continuation_group)],'school_type_coverage')
saveRDS(panel,file.path(out,'admin_panel.rds'))
log_message('Administrative panel saved: ',nrow(panel),' students; eligible ',sum(panel$analysis_eligible))
