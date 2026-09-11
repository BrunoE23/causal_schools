suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE))
task <- dirname(normalizePath(script,winslash="/")); root <- normalizePath(file.path(task,"../../.."),winslash="/")
source(file.path(task,"linkage_helpers.R")); setDTthreads(4L)
out <- file.path(root,"data/clean/titulados_staff_linkage")
manifest <- fread(file.path(out,"graduation_clean_manifest.csv"))
audit <- fread(file.path(out,"graduation_cleaning_audit.csv"))
checks <- list(); schema <- NULL
for (i in seq_len(nrow(manifest))) {
  d <- readRDS(manifest$PATH[i]); yr <- manifest$YEAR[i]
  stopifnot(nrow(d)==manifest$ROWS[i],all(d$REPORT_YEAR==yr),all(t_int(d$CAT_PERIODO)==yr),
    !anyDuplicated(d$SOURCE_ROW),!anyDuplicated(d$RECORD_ID),all(d$SOURCE_ROW>=1L & d$SOURCE_ROW<=audit[YEAR==yr,N_INPUT]),
    identical(d$MRUN,t_id(d$MRUN_RAW)),sum(is.na(d$MRUN))==audit[YEAR==yr,N_INVALID_MRUN],
    identical(d$AWARD_DATE,t_date(d$FECHA_OBTENCION_TITULO)),
    all(d$ASOF_YEAR>=d$REPORT_YEAR),all(d$ASOF_YEAR>=d$AWARD_YEAR,na.rm=TRUE))
  original <- setdiff(names(d),c("SOURCE_ROW","MRUN","REPORT_YEAR","SOURCE_FILE","RECORD_ID","BIRTH_YM","SEX",
    "AWARD_DATE","AWARD_YEAR","LEVEL","IS_UNDERGRAD","IS_POSTGRAD","IS_POSTITULO","IS_EDUCATION","IS_EDUCATION_UNDERGRAD",
    "AWARD_YEAR_DIFFERS_FROM_REPORT","DATE_MISSING_OR_INVALID","ASOF_YEAR","ASOF_BASIS","INSTITUTION_ID",
    "INSTITUTION_NAMED","PROGRAM_NAMED","EDUCATION_FIELD_NAMED"))
  stopifnot(length(original)==40L,!anyDuplicated(d,by=original),
    nrow(d)+audit[YEAR==yr,N_EXACT_DUPLICATES_REMOVED]==audit[YEAR==yr,N_INPUT])
  checks[[i]] <- d[,.(YEAR=yr,N_CLEAN=.N,N_AWARD_BEFORE_REPORT=sum(AWARD_YEAR<REPORT_YEAR),
    N_AWARD_AFTER_REPORT=sum(AWARD_YEAR>REPORT_YEAR),MIN_AWARD_YEAR=min(AWARD_YEAR),MAX_AWARD_YEAR=max(AWARD_YEAR),
    N_INVALID_INSTITUTION_ID=sum(is.na(INSTITUTION_ID)),N_NO_INSTITUTION_NAME=sum(!INSTITUTION_NAMED),
    N_NO_PROGRAM_NAME=sum(!PROGRAM_NAMED))]
  if (is.null(schema)) schema <- data.table(FIELD=names(d),STORAGE_TYPE=vapply(d,function(x) paste(class(x),collapse="/"),""),
    SOURCE_OR_DERIVED=ifelse(names(d) %in% original,"Source field after CSV parsing","Derived analytical/provenance field"))
  rm(d); invisible(gc())
}
result <- rbindlist(checks)
fwrite(result,file.path(out,"clean_file_verification.csv"),na="NA")
fwrite(schema,file.path(out,"clean_field_dictionary.csv"),na="NA")
print(result)
message("All 19 clean files verified: full schema, source-row support, no exact duplicates and parsed fields.")
