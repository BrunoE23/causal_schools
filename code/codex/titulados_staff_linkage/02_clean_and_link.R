suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task <- dirname(normalizePath(script, winslash = "/")); root <- normalizePath(file.path(task,"../../.."), winslash="/")
source(file.path(task,"linkage_helpers.R"))
source(file.path(root,"code/codex/docentes_educacion/staff_cleaning_helpers.R"))
setDTthreads(4L)
args <- commandArgs(trailingOnly=TRUE); stopifnot(all(args %chin% "--overwrite"))
out <- file.path(root,"data/clean/titulados_staff_linkage")
if (dir.exists(out) && length(list.files(out)) && !"--overwrite" %chin% args) stop("Outputs exist; use --overwrite explicitly.")
dir.create(file.path(out,"annual"),recursive=TRUE,showWarnings=FALSE)
rawroot <- "C:/Users/brunem/Box/causal_schools/data/raw/titulados"
files <- list.files(rawroot,pattern="[.]csv$",recursive=TRUE,full.names=TRUE)
manifest <- data.table(PATH=files,BYTES=file.info(files)$size,MTIME=as.character(file.info(files)$mtime))
manifest[, YEAR:=as.integer(sub(".*_Superior_([0-9]{4})_WEB.csv$","\\1",PATH))]
manifest[, FOLDER_YEAR:=as.integer(sub(".*Titulados-Ed-Superior-([0-9]{4})/.*","\\1",PATH))]
manifest[, INCLUDED:=YEAR==FOLDER_YEAR]
manifest[, MD5:=unname(tools::md5sum(PATH))]
stopifnot(identical(sort(manifest[INCLUDED==TRUE,YEAR]),2007:2025))
for (i in which(!manifest$INCLUDED)) stopifnot(manifest$MD5[i] == manifest[INCLUDED==TRUE & YEAR==manifest$YEAR[i],MD5])
fwrite(manifest,file.path(out,"graduation_source_manifest.csv"),na="NA")
protected <- c(file.path(root,"data/clean/staff_quality_va/staff_person_school_year_features.rds"),
  file.path(root,"data/clean/leadership_quality_va/leadership_person_school_year.rds"),
  file.path(root,"data/clean/staff_quality_va/school_staff_indices_and_measures.csv"),
  file.path(root,"data/clean/leadership_quality_va/leadership_indices_and_measures.csv"))
preserved <- data.table(PATH=protected,MD5=unname(tools::md5sum(protected)))
staff <- readRDS(protected[1]); leaders <- readRDS(protected[2])
membership <- rbindlist(list(
  unique(staff[TEACHER_HS_ANY==1L,.(MRUN,RBD,AGNO)])[,ROLE:="HS teachers"],
  unique(staff[ORIENTADOR_ANY==1L,.(MRUN,RBD,AGNO)])[,ROLE:="Orientadores"],
  unique(leaders[,.(MRUN,RBD,AGNO)])[,ROLE:="Leadership"]
))
membership[,MRUN:=t_id(MRUN)]; stopifnot(!anyNA(membership$MRUN),all(membership$AGNO %in% 2018:2024))
rm(staff,leaders); invisible(gc())
staff_sources <- fread(file.path(root,"data/clean/staff_quality_va/staff_source_manifest.csv"))[AGNO>=2018 & AGNO<=2024]
staff_sources[,`:=`(CHECK_BYTES=file.info(SOURCE_PATH)$size,CHECK_MTIME=as.character(file.info(SOURCE_PATH)$mtime))]
schools <- fread(file.path(root,"data/clean/staff_quality_va/staff_school_context.csv"))$RBD
profiles <- list()
for (i in seq_len(nrow(staff_sources))) {
  yr <- staff_sources$AGNO[i]; message(format(Sys.time(),"%H:%M:%S")," Staff profile ",yr)
  cols <- c("MRUN","RBD","DOC_FEC_NAC","DOC_GENERO","ANO_TITULACION_1","ANO_TITULACION_2","TIT_ID_1","TIT_ID_2")
  if (yr==2024L) cols <- c(cols,"ID_IFP","ID_IFS","COD_ENS_1","COD_ENS_2")
  d <- fread(staff_sources$SOURCE_PATH[i],sep=";",select=cols,colClasses="character")
  d[,`:=`(MRUN=t_id(MRUN),RBD=t_int(RBD))]
  if (yr==2024L) {
    for (f in c("ID_IFP","ID_IFS","COD_ENS_1","COD_ENS_2")) set(d,j=f,value=t_int(d[[f]]))
    teachers <- d[(ID_IFP==1L | ID_IFS==1L) & !is.na(MRUN)]
    teachers[,HAS_HS:=COD_ENS_1 %in% staff_hs_teaching_codes | COD_ENS_2 %in% staff_hs_teaching_codes]
    teachers[,UNKNOWN:=!COD_ENS_1 %in% as.integer(names(staff_teaching_levels)) | !COD_ENS_2 %in% as.integer(names(staff_teaching_levels))]
    teachers[,OTHER:=!HAS_HS & !UNKNOWN & (COD_ENS_1>0L | COD_ENS_2>0L)]
    classify <- teachers[,.(HS=any(HAS_HS),UNKNOWN=any(UNKNOWN),OTHER_VA=any(OTHER & RBD %in% schools)),by=MRUN]
    ids <- classify[!HS & !UNKNOWN & OTHER_VA,MRUN]
    extra <- unique(teachers[MRUN %chin% ids & RBD %in% schools & OTHER,.(MRUN,RBD)])[,`:=`(AGNO=2024L,ROLE="Non-HS teachers")]
    membership <- rbind(membership,extra)
    rm(teachers,classify,extra)
  }
  d <- d[MRUN %chin% membership[AGNO==yr,MRUN]]
  d[,`:=`(BIRTH_YM=t_birth(DOC_FEC_NAC),SEX=fifelse(t_int(DOC_GENERO) %in% 1:2,t_int(DOC_GENERO),NA_integer_))]
  for (k in 1:2) {
    year <- t_int(d[[paste0("ANO_TITULACION_",k)]])
    tit <- t_int(d[[paste0("TIT_ID_",k)]])
    set(d,j=paste0("TITLE_YEAR_",k),value=fifelse(tit %in% 1:2 & year>1900L & year<=yr,year,NA_integer_))
  }
  profile <- d[, {
    dob <- unique(BIRTH_YM[!is.na(BIRTH_YM)]); sex <- unique(SEX[!is.na(SEX)])
    years <- sort(unique(c(TITLE_YEAR_1,TITLE_YEAR_2))); years <- years[!is.na(years)]
    .(STAFF_BIRTH_YM=if(length(dob)==1L) dob else NA_character_, N_STAFF_BIRTH_VALUES=length(dob),
      STAFF_SEX=if(length(sex)==1L) sex else NA_integer_,N_STAFF_SEX_VALUES=length(sex),
      STAFF_TITLE_YEARS=if(length(years)) paste(years,collapse=",") else NA_character_,
      STAFF_TITLE_YEAR_KNOWN=length(years)>0L,
      STAFF_ANY_TITLE_2007PLUS=if(length(years)) any(years>=2007L) else NA,
      STAFF_ALL_TITLES_PRE2007=if(length(years)) all(years<2007L) else NA)
  },by=MRUN]
  profile[,AGNO:=yr]; profiles[[i]] <- profile
  rm(d,profile); invisible(gc())
}
membership <- unique(membership); setorder(membership,ROLE,AGNO,MRUN,RBD)
people <- merge(unique(membership[,.(MRUN,ROLE,AGNO)]),rbindlist(profiles),by=c("MRUN","AGNO"),all.x=TRUE)
people[,AGE:=AGNO-t_int(substr(STAFF_BIRTH_YM,1,4))]
people[AGE<18 | AGE>100,AGE:=NA_integer_]
stopifnot(!anyDuplicated(people,by=c("MRUN","ROLE","AGNO")))
fwrite(membership,file.path(out,"staff_membership_person_school_year.csv.gz"),na="NA")
fwrite(people,file.path(out,"staff_person_role_year.csv.gz"),na="NA")
fwrite(staff_sources,file.path(out,"staff_profile_source_manifest.csv"),na="NA")
ids <- unique(people$MRUN); rm(profiles,membership); invisible(gc())
matches <- list(); audits <- list(); clean_manifest <- list()
for (year in 2007:2025) {
  path <- manifest[INCLUDED==TRUE & YEAR==year,PATH]
  message(format(Sys.time(),"%H:%M:%S")," Cleaning graduation cohort ",year)
  raw <- fread(path,sep=";",encoding="UTF-8",colClasses="character",strip.white=TRUE)
  cleaned <- t_clean_awards(raw,year,basename(path)); rm(raw)
  d <- cleaned$data
  stopifnot(cleaned$audit$N_INPUT==t_expected[as.character(year)],cleaned$audit$N_UNMAPPED_LEVEL==0)
  annual_path <- file.path(out,"annual",paste0("titulados_",year,".rds"))
  saveRDS(d,annual_path,compress="gzip")
  matches[[as.character(year)]] <- d[!is.na(MRUN) & MRUN %chin% ids]
  audits[[as.character(year)]] <- cleaned$audit
  clean_manifest[[as.character(year)]] <- data.table(YEAR=year,PATH=annual_path,ROWS=nrow(d),MD5=unname(tools::md5sum(annual_path)))
  message("  ",nrow(d)," clean records; ",nrow(matches[[as.character(year)]])," linked award records.")
  rm(d,cleaned); invisible(gc())
}
matched <- rbindlist(matches,use.names=TRUE,fill=FALSE)
stopifnot(!anyDuplicated(matched$RECORD_ID))
saveRDS(matched,file.path(out,"staff_linked_awards.rds"),compress="gzip")
thin <- c("RECORD_ID","REPORT_YEAR","SOURCE_ROW","MRUN","BIRTH_YM","SEX","AWARD_DATE","AWARD_YEAR","ASOF_YEAR","ASOF_BASIS",
  "LEVEL","IS_UNDERGRAD","IS_POSTGRAD","IS_POSTITULO","IS_EDUCATION","IS_EDUCATION_UNDERGRAD",
  "INSTITUTION_ID","NOMB_INST","NOMB_CARRERA","NOMBRE_TITULO","NOMBRE_GRADO","INSTITUTION_NAMED","PROGRAM_NAMED")
fwrite(matched[,..thin],file.path(out,"staff_linked_awards.csv.gz"),na="NA")
fwrite(rbindlist(audits),file.path(out,"graduation_cleaning_audit.csv"),na="NA")
fwrite(rbindlist(clean_manifest),file.path(out,"graduation_clean_manifest.csv"),na="NA")
fwrite(preserved,file.path(out,"preserved_staff_sources.csv"),na="NA")
stopifnot(identical(unname(tools::md5sum(manifest$PATH)),manifest$MD5),
  identical(unname(tools::md5sum(preserved$PATH)),preserved$MD5),
  identical(as.numeric(file.info(staff_sources$SOURCE_PATH)$size),as.numeric(staff_sources$CHECK_BYTES)),
  identical(as.character(file.info(staff_sources$SOURCE_PATH)$mtime),staff_sources$CHECK_MTIME))
print(rbindlist(audits))
message("Cleaned every cohort; exact-MRUN links saved without changing raw files or prior staff datasets.")
