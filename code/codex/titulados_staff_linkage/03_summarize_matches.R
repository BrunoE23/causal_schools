suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE))
task <- dirname(normalizePath(script,winslash="/")); root <- normalizePath(file.path(task,"../../.."),winslash="/")
source(file.path(task,"linkage_helpers.R")); setDTthreads(4L)
out <- file.path(root,"data/clean/titulados_staff_linkage")
people <- fread(file.path(out,"staff_person_role_year.csv.gz"),colClasses=c(MRUN="character",STAFF_BIRTH_YM="character",STAFF_TITLE_YEARS="character"))
awards <- fread(file.path(out,"staff_linked_awards.csv.gz"),colClasses=c(MRUN="character",BIRTH_YM="character"))
first <- t_flags_by_year(awards)
panel <- merge(people,first,by="MRUN",all.x=TRUE)
panel[, MATCH_ANY_2007_2025:=!is.na(FIRST_ANY_YEAR)]
map <- c(MATCH_ANY_BY_STAFF_YEAR="FIRST_ANY_YEAR",MATCH_UNDERGRAD_BY_STAFF_YEAR="FIRST_UNDERGRAD_YEAR",
  MATCH_EDUCATION_UNDERGRAD_BY_STAFF_YEAR="FIRST_EDUCATION_UNDERGRAD_YEAR",
  MATCH_POSTGRAD_BY_STAFF_YEAR="FIRST_POSTGRAD_YEAR",MATCH_POSTITULO_BY_STAFF_YEAR="FIRST_POSTITULO_YEAR",
  MATCH_NAMED_INSTITUTION_BY_STAFF_YEAR="FIRST_NAMED_INSTITUTION_YEAR")
for (f in names(map)) set(panel,j=f,value=!is.na(panel[[map[[f]]]]) & panel[[map[[f]]]]<=panel$AGNO)
panel[,AGE_BAND:=as.character(cut(AGE,breaks=c(17,29,39,42,49,59,69,100),
  labels=c("18-29","30-39","40-42","43-49","50-59","60-69","70+")))]
panel[is.na(AGE_BAND),AGE_BAND:="Unknown"]
panel[,TITLE_COHORT:=fcase(STAFF_ANY_TITLE_2007PLUS %in% TRUE,"At least one reported title in 2007+",
  STAFF_ALL_TITLES_PRE2007 %in% TRUE,"All reported title years before 2007",default="No valid reported title year")]
flags <- c("MATCH_ANY_2007_2025",names(map))
summarize <- function(d,by) {
  rbindlist(lapply(flags,function(f) d[,.(METRIC=f,N_PEOPLE=.N,N_MATCHED=sum(get(f)),SHARE_MATCHED=mean(get(f))),by=by]))
}
annual <- summarize(panel,c("ROLE","AGNO"))
current <- panel[AGNO==2024L]
byage <- summarize(current,c("ROLE","AGE_BAND"))
bytitle <- summarize(current,c("ROLE","TITLE_COHORT"))
# Unique ever-members, not a sum of repeated person-years. As-of = last year
# actually observed in that role, not automatically 2024 for departed staff.
setorder(panel,ROLE,MRUN,AGNO)
last <- panel[, .SD[.N],by=.(ROLE,MRUN)]
ever <- summarize(last,"ROLE")
ever[,SCOPE:="Unique people observed in role during 2018-2024; non-HS only 2024"]
# Rich pair-level DOB/sex and reported-title-year validation on the 2024 cross-section.
check <- merge(current[,.(MRUN,ROLE,AGNO,STAFF_BIRTH_YM,STAFF_SEX,STAFF_TITLE_YEARS)],awards,by="MRUN",allow.cartesian=TRUE)
check <- check[ASOF_YEAR<=AGNO]
check[,`:=`(DOB_COMPARABLE=!is.na(STAFF_BIRTH_YM)&!is.na(BIRTH_YM),
  SEX_COMPARABLE=!is.na(STAFF_SEX)&!is.na(SEX))]
check[,`:=`(DOB_AGREE=DOB_COMPARABLE & STAFF_BIRTH_YM==BIRTH_YM,
  SEX_AGREE=SEX_COMPARABLE & STAFF_SEX==SEX)]
check[,REPORTED_YEAR_MATCH:=mapply(function(y,ys) !is.na(y) & !is.na(ys) &
  as.character(y) %in% strsplit(ifelse(is.na(ys),"",ys),",",fixed=TRUE)[[1]], AWARD_YEAR,STAFF_TITLE_YEARS)]
validation <- check[,.(N_AWARDS_BY2024=.N,N_DOB_COMPARABLE=sum(DOB_COMPARABLE),N_DOB_AGREE=sum(DOB_AGREE),
  N_DOB_DISAGREE=sum(DOB_COMPARABLE & !DOB_AGREE),N_SEX_COMPARABLE=sum(SEX_COMPARABLE),N_SEX_AGREE=sum(SEX_AGREE),
  ANY_UNDERGRAD_TITLE_YEAR_MATCH=any(IS_UNDERGRAD & REPORTED_YEAR_MATCH),
  ANY_MATCH_WITH_BIRTH_AND_SEX=any(DOB_AGREE & SEX_AGREE),
  ANY_REPORT_YEAR_ONLY=any(ASOF_BASIS=="report_year_only")),by=.(ROLE,MRUN)]
quality <- validation[,.(N_MATCHED_PEOPLE=.N,N_DOB_COMPARABLE_PEOPLE=sum(N_DOB_COMPARABLE>0),
  N_ANY_DOB_AGREE=sum(N_DOB_AGREE>0),N_ANY_DOB_DISAGREE=sum(N_DOB_DISAGREE>0),
  N_SEX_COMPARABLE_PEOPLE=sum(N_SEX_COMPARABLE>0),N_ANY_SEX_AGREE=sum(N_SEX_AGREE>0),
  N_ALL_DOB_COMPARISONS_AGREE=sum(N_DOB_COMPARABLE>0 & N_DOB_DISAGREE==0),
  N_BOTH_BIRTH_AND_SEX_AGREE=sum(ANY_MATCH_WITH_BIRTH_AND_SEX),
  N_UNDERGRAD_TITLE_YEAR_MATCH=sum(ANY_UNDERGRAD_TITLE_YEAR_MATCH),N_ANY_REPORT_YEAR_ONLY=sum(ANY_REPORT_YEAR_ONLY)),by=ROLE]
quality[,SHARE_DOB_AGREE_WHEN_COMPARABLE:=N_ANY_DOB_AGREE/N_DOB_COMPARABLE_PEOPLE]
quality[,SHARE_SEX_AGREE_WHEN_COMPARABLE:=N_ANY_SEX_AGREE/N_SEX_COMPARABLE_PEOPLE]
school_members <- fread(file.path(out,"staff_membership_person_school_year.csv.gz"),colClasses=c(MRUN="character"))
school_members <- merge(school_members,panel[,c("MRUN","ROLE","AGNO",flags),with=FALSE],by=c("MRUN","ROLE","AGNO"),all.x=TRUE)
school <- school_members[,c(list(N_STAFF=.N),lapply(.SD,mean)),by=.(ROLE,RBD,AGNO),.SDcols=flags]
institution <- unique(check[IS_UNDERGRAD==TRUE,.(ROLE,MRUN,INSTITUTION_ID,NOMB_INST)])[,.(N_PEOPLE=uniqueN(MRUN)),by=.(ROLE,INSTITUTION_ID,NOMB_INST)]
setorder(institution,ROLE,-N_PEOPLE)
level <- check[,.(N_AWARDS=.N,N_PEOPLE=uniqueN(MRUN)),by=.(ROLE,LEVEL)]
dictionary <- data.table(METRIC=flags,DEFINITION=c("At least one exact-MRUN award record anywhere in the 2007-2025 files, including later awards",
  "Any award recorded by staff year; require report year and valid award year <= staff year; missing award date uses report year",
  "An undergraduate award by staff year; not necessarily the original or first lifetime degree",
  "An undergraduate award in the broad Education field by staff year; not necessarily a teaching license",
  "A postgraduate award by staff year", "A postitulo award by staff year (separate from postgraduate)",
  "Any award with an institution name by staff year"))
exports <- list(staff_match_person_role_year=panel,staff_match_annual_summary=annual,staff_match_2024_summary=annual[AGNO==2024],
  staff_match_2024_by_age=byage,staff_match_2024_by_reported_title_year=bytitle,staff_match_unique_period_people=ever,
  staff_match_2024_identity_validation=validation,staff_match_2024_identity_summary=quality,
  staff_match_school_year=school,staff_match_2024_institutions=institution,staff_match_2024_award_levels=level,match_metric_dictionary=dictionary)
for (name in names(exports)) fwrite(exports[[name]],file.path(out,paste0(name,if(name %chin% c("staff_match_person_role_year","staff_match_2024_identity_validation","staff_match_school_year")) ".csv.gz" else ".csv")),na="NA")
stopifnot(all(panel[MATCH_ANY_BY_STAFF_YEAR==TRUE,MATCH_ANY_2007_2025]),
  all(panel[MATCH_EDUCATION_UNDERGRAD_BY_STAFF_YEAR==TRUE,MATCH_UNDERGRAD_BY_STAFF_YEAR]),
  all(panel[MATCH_UNDERGRAD_BY_STAFF_YEAR==TRUE,MATCH_ANY_BY_STAFF_YEAR]))
print(dcast(annual[AGNO==2024 & METRIC %chin% flags[1:4]],ROLE+N_PEOPLE~METRIC,value.var="SHARE_MATCHED"))
print(quality)
message("Saved exact-ID coverage, age/title-cohort breakdowns and match-quality checks.")
