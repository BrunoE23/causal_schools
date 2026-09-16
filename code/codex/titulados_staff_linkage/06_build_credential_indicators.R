suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE))
task <- dirname(normalizePath(script,winslash="/"))
root <- normalizePath(file.path(task,"../../.."),winslash="/")
source(file.path(task,"linkage_helpers.R"))
source(file.path(task,"credential_helpers.R"))
setDTthreads(4L)
base <- file.path(root,"data/clean/titulados_staff_linkage")
out <- file.path(base,"credentials")
inputs <- c(awards=file.path(base,"staff_linked_awards.rds"),
  people=file.path(base,"staff_person_role_year.csv.gz"),
  effects=file.path(root,"output/tables/mifuturo_matricula_income/mifuturo_income_fe_institution_effects.csv"),
  overrides=file.path(task,"credential_subject_overrides.csv"),
  subject_rules=file.path(task,"credential_helpers.R"))
stopifnot(all(file.exists(inputs)))
before <- tools::md5sum(inputs)
if (dir.exists(out) && length(list.files(out)) && !"--overwrite" %in% commandArgs())
  stop("Credential outputs already exist; pass --overwrite to rebuild only these derived outputs.")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
people <- fread(inputs[["people"]],colClasses=c(MRUN="character",STAFF_BIRTH_YM="character",STAFF_TITLE_YEARS="character"))
# Cached linked awards are 12.7 MB on disk. Select only needed columns immediately.
a <- readRDS(inputs[["awards"]])[,.(RECORD_ID,MRUN,INSTITUTION_ID,NOMB_INST,
  REPORT_YEAR,AWARD_YEAR,ASOF_YEAR,ASOF_BASIS,LEVEL,NIVEL_CARRERA_1,
  NOMB_CARRERA,NOMBRE_TITULO,NOMBRE_GRADO,AREA_CONOCIMIENTO)]
stopifnot(!anyDuplicated(a$RECORD_ID),!anyNA(a$ASOF_YEAR))
a <- prepare_credential_awards(a,fread(inputs[["effects"]]))
a <- apply_subject_overrides(a,fread(inputs[["overrides"]]))
mapcols <- c("NOMB_CARRERA","NOMBRE_TITULO","NOMBRE_GRADO","AREA_CONOCIMIENTO",
  "TEACHER_RELEVANT","ORIENTADOR_RELEVANT","LEADERSHIP_RELEVANT",
  "ORIENTADOR_PROGRAM_ONLY","LEADERSHIP_PROGRAM_ONLY",
  "ORIENTADOR_MENTION_CONFLICT","LEADERSHIP_MENTION_CONFLICT",
  "TEACHER_RULE","ORIENTADOR_RULE","LEADERSHIP_RULE","SUBJECT_OVERRIDE_REASON")
subjects <- a[,.(N_AWARD_RECORDS=.N,N_PEOPLE=uniqueN(MRUN)),by=mapcols]
setorder(subjects,-N_AWARD_RECORDS,NOMB_CARRERA)
institutions <- a[,.(N_AWARD_RECORDS=.N,N_PEOPLE=uniqueN(MRUN)),
  by=.(INSTITUTION_ID,NOMB_INST,FE_INSTITUTION_KEY,INSTITUTION_PREMIUM,FE_COVERED,HIGH_PREMIUM)]
fwrite(subjects,file.path(out,"credential_subject_mapping.csv"),na="NA")
fwrite(institutions,file.path(out,"credential_institution_mapping.csv"),na="NA")
if ("--review-only" %in% commandArgs()) {
  print(institutions[HIGH_PREMIUM==1,.(INSTITUTION_ID,NOMB_INST,INSTITUTION_PREMIUM)])
  print(institutions[FE_COVERED==0][order(-N_AWARD_RECORDS)][1:15])
  quit(status=0)
}
panel <- build_credential_panel(people,a)
stopifnot(nrow(panel)==nrow(people),!anyDuplicated(panel,by=c("MRUN","ROLE","AGNO")))
for (f in c(credential_names,credential_aux)) stopifnot(all(panel[[f]] %in% 0:1))
stopifnot(all(panel$MAGISTER_HIGH_PREMIUM<=panel$ANY_MAGISTER),
  all(panel$ANY_MAGISTER<=panel$ANY_POST_UG),
  all(panel$ROLE_SPECIFIC_MAGISTER<=panel$ANY_MAGISTER),
  all(panel$ROLE_SPECIFIC_MAGISTER<=panel$ROLE_SPECIFIC_QUALIFICATION),
  all(panel$UG_HIGH_PREMIUM<=panel$ANY_HIGH_PREMIUM),
  all(panel$POST_UG_HIGH_PREMIUM<=panel$ANY_HIGH_PREMIUM))

# Independent direct set membership at every role/year, not the first-year method.
checks <- list(); k <- 0L
for (y in sort(unique(panel$AGNO))) {
  eligible <- a[ASOF_YEAR<=y]
  for (r in unique(panel[AGNO==y,ROLE])) {
    x <- panel[AGNO==y & ROLE==r]
    rf <- if (r %chin% c("HS teachers","Non-HS teachers")) "TEACHER_RELEVANT" else
      if (r=="Orientadores") "ORIENTADOR_RELEVANT" else "LEADERSHIP_RELEVANT"
    for (f in c(credential_names,credential_aux)) {
      ids <- if(f=="ROLE_SPECIFIC_QUALIFICATION") eligible[get(rf)==1L,MRUN] else
        if(f=="ROLE_SPECIFIC_MAGISTER") eligible[get(rf)==1L & ANY_MAGISTER==1L,MRUN] else
          eligible[get(f)==1L,MRUN]
      expected <- as.integer(x$MRUN %chin% ids)
      stopifnot(identical(x[[f]],expected))
      k <- k+1L
      checks[[k]] <- data.table(ROLE=r,AGNO=y,METRIC=f,N_CHECKED=nrow(x),N_ONE=sum(expected),PASS=TRUE)
    }
  }
}
verification <- rbindlist(checks)
# Existing coverage is an independent saved control for population and chronology.
prior <- fread(file.path(base,"staff_match_person_role_year.csv.gz"),
  select=c("MRUN","ROLE","AGNO","MATCH_ANY_BY_STAFF_YEAR","MATCH_UNDERGRAD_BY_STAFF_YEAR"),
  colClasses=c(MRUN="character"))
v <- merge(panel[,.(MRUN,ROLE,AGNO,MATCH_ANY_BY_STAFF_YEAR,MATCH_UNDERGRAD_BY_STAFF_YEAR)],
  prior,by=c("MRUN","ROLE","AGNO"),suffixes=c("_new","_prior"))
stopifnot(nrow(v)==nrow(panel),
  all(v$MATCH_ANY_BY_STAFF_YEAR_new==v$MATCH_ANY_BY_STAFF_YEAR_prior),
  all(v$MATCH_UNDERGRAD_BY_STAFF_YEAR_new==v$MATCH_UNDERGRAD_BY_STAFF_YEAR_prior))
summary <- rbindlist(lapply(c(credential_names,credential_aux),function(f)
  panel[,.(METRIC=f,N_PEOPLE=.N,N_ONE=sum(get(f)),SHARE=mean(get(f))),by=.(ROLE,AGNO)]))
fwrite(panel,file.path(out,"staff_credentials_person_role_year.csv.gz"),na="NA")
fwrite(panel[AGNO==2024],file.path(out,"staff_credentials_2024.csv.gz"),na="NA")
fwrite(summary,file.path(out,"staff_credentials_summary.csv"),na="NA")
fwrite(a,file.path(out,"credential_award_evidence.csv.gz"),na="NA")
fwrite(verification,file.path(out,"credential_verification.csv"))
definitions <- c("Observed undergraduate at institution with centered MiFuturo FE > 0.1",
  "Observed non-undergraduate award: diplomado, postitulo, magister, doctorate or specialty",
  "Observed non-undergraduate award at high-premium institution",
  "Any observed award at high-premium institution",
  "Observed award whose subject matches current staff role under saved subject mapping",
  "Observed award classified Magister in NIVEL_CARRERA_1; doctorate alone excluded",
  "Same observed award is Magister and from a high-premium institution",
  "Same observed award is Magister and matches current role-specific subject rule")
fwrite(data.table(VARIABLE=credential_names,DEFINITION=definitions,
  TIMING="ASOF_YEAR <= staff AGNO; requires reporting and valid award year",
  ZERO_MEANS="No qualifying award observed, not verified lifetime absence"),
  file.path(out,"credential_dictionary.csv"))
stopifnot(identical(before,tools::md5sum(inputs)))
fwrite(data.table(INPUT=names(inputs),PATH=unname(inputs),MD5=unname(before)),
  file.path(out,"credential_input_manifest.csv"))
message("Verified ",sum(verification$N_CHECKED)," binary cells across ",nrow(panel),
  " person-role-years; ",uniqueN(panel$MRUN)," distinct people.")
print(dcast(summary[AGNO==2024 & METRIC %chin% credential_names],ROLE+N_PEOPLE~METRIC,value.var="N_ONE"))
