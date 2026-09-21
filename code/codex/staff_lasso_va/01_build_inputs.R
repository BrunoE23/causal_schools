# Assemble a school-level predictor matrix without reading any VA outcome values.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
root <- normalizePath('.', winslash='/')
out <- file.path(root,'data/clean/staff_lasso_va')
dir.create(out, recursive=TRUE, showWarnings=FALSE)
if (file.exists(file.path(out,'school_predictors.csv')) && !'--overwrite' %in% commandArgs())
  stop('Inputs exist; pass --overwrite to replace this task\'s derived inputs.')
paths <- c(
  context='data/clean/staff_quality_va/staff_school_context.csv',
  staff='data/clean/staff_quality_va/school_staff_indices_and_measures.csv',
  leaders='data/clean/leadership_quality_va/leadership_indices_and_measures.csv',
  staff_dictionary='data/clean/staff_quality_va/staff_analysis_metric_dictionary.csv',
  leader_dictionary='data/clean/leadership_quality_va/leadership_analysis_metric_dictionary.csv',
  members='data/clean/titulados_staff_linkage/staff_membership_person_school_year.csv.gz',
  credentials='data/clean/titulados_staff_linkage/credentials/staff_credentials_person_role_year.csv.gz',
  staff_roster='data/clean/staff_quality_va/staff_school_year_roster.csv',
  leader_roster='data/clean/leadership_quality_va/leadership_school_year_roster.csv',
  counselor_period='data/clean/titulados_staff_linkage/orientador_credentials_va/orientador_credentials_school_period.csv',
  outcomes='data/clean/staff_quality_va/staff_va_outcome_dictionary.csv',
  va='output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv')
paths <- setNames(file.path(root,paths),names(paths))
hash <- tools::md5sum(paths)
source(file.path(root,'code/codex/titulados_staff_linkage/credential_helpers.R'))
metrics <- credential_names
cn <- c('UG degree at high-premium institution','Any non-UG qualification',
  'Non-UG qualification at high-premium institution','Any degree at high-premium institution',
  'Role-specific qualification','Any magister','Magister at high-premium institution','Role-specific magister')
context <- fread(paths[['context']]); setorder(context,RBD)
stopifnot(!anyDuplicated(context$RBD),all(context$N_VA_STUDENTS>0))
x <- context[,.(RBD)]
dict <- list(); excluded <- list(); di <- 0L
add <- function(key,value,label,role,block) {
  stopifnot(!key %in% names(x),length(value)==nrow(x))
  set(x,j=key,value=as.numeric(value))
  di <<- di+1L
  dict[[di]] <<- data.table(FEATURE=key,LABEL=label,ROLE=role,BLOCK=block)
}
add('school__log_va_students',log(context$N_VA_STUDENTS),'Log VA-sample student count','school','context')
add('school__log_va_students_squared',log(context$N_VA_STUDENTS)^2,'Squared log VA-sample student count','school','context')
for (nm in c('RURAL_RBD','HAS_TP_OR_ARTISTIC','HAS_BASIC')) {
  v <- as.numeric(context[[nm]])
  stopifnot(all(v[!is.na(v)] %in% 0:1))
  add(paste0('school__',tolower(nm)),v,
      c(RURAL_RBD='Rural school',HAS_TP_OR_ARTISTIC='Technical-professional/artistic offering',HAS_BASIC='Basic-education offering')[[nm]],'school','context')
}
# Fixed administrative-code categories, not ordered numerical predictors.
# Reference categories: municipal corporation (dependency 1), Metropolitana (region 13).
for (spec in list(list(field='COD_DEPE',codes=1:6,ref=1L,prefix='dependency'),
                 list(field='COD_REG_RBD',codes=1:16,ref=13L,prefix='region'))) {
  v <- context[[spec$field]]; known <- v %in% spec$codes
  for (code in setdiff(spec$codes,spec$ref))
    add(paste0('school__',spec$prefix,'_',code),as.integer(known & v==code),
        paste(spec$prefix,code),'school','context')
  add(paste0('school__',spec$prefix,'_unknown'),as.integer(!known),
      paste(spec$prefix,'unknown'),'school','context')
}
staff <- fread(paths[['staff']]); leaders <- fread(paths[['leaders']])
sdict <- fread(paths[['staff_dictionary']]); ldict <- fread(paths[['leader_dictionary']])
m <- fread(paths[['members']],colClasses=c(MRUN='character'))[ROLE!='Non-HS teachers']
ff <- c(metrics,'MATCH_ANY_BY_STAFF_YEAR','MATCH_UNDERGRAD_BY_STAFF_YEAR','ANY_UNCOVERED_INSTITUTION')
f <- fread(paths[['credentials']],select=c('MRUN','ROLE','AGNO',ff),colClasses=c(MRUN='character'))[ROLE!='Non-HS teachers']
stopifnot(!anyDuplicated(m,by=c('MRUN','RBD','ROLE','AGNO')),
          !anyDuplicated(f,by=c('MRUN','ROLE','AGNO')))
d <- merge(m,f,by=c('MRUN','ROLE','AGNO'),all.x=TRUE)
stopifnot(nrow(d)==nrow(m),!anyNA(d[,..ff]))
roster <- fread(paths[['staff_roster']]); lr <- fread(paths[['leader_roster']])
periods <- list()
for (role in c('teacher','counselor','leadership')) {
  role_label <- c(teacher='HS teachers',counselor='Orientadores',leadership='Leadership')[[role]]
  w <- if(role=='leadership') copy(leaders) else copy(staff[ROLE==role])
  stopifnot(!anyDuplicated(w$RBD))
  w <- w[match(x$RBD,w$RBD)]
  dd <- if(role=='leadership') copy(ldict) else copy(sdict[ROLE==role])
  dd[,INCLUDE:=ANALYZE & !BLOCK %in% c('index','history_sensitivity') & METRIC!='secondary_only_share']
  excluded[[role]] <- dd[INCLUDE==FALSE,.(ROLE=role,METRIC,BLOCK,
    REASON=fifelse(BLOCK=='index','Composite duplicates candidate components',
      fifelse(BLOCK=='history_sensitivity','Alternative history window, not an additional characteristic',
      fifelse(METRIC=='secondary_only_share','Complement of primary_role_share','Existing audit/applicability exclusion'))))]
  for (j in which(dd$INCLUDE)) {
    nm <- dd$METRIC[j]
    add(paste0(role,'__',nm),w[[nm]],dd$LABEL[j],role,dd$BLOCK[j])
  }
  absent <- ifelse(w$N_ROLE_YEARS_KNOWN==7,as.integer(w$N_ACTIVE_YEARS==0),NA_integer_)
  add(paste0(role,'__absent_all_years'),absent,'Role absent throughout 2018-2024',role,'availability')
  add(paste0(role,'__roster_incomplete'),as.integer(is.na(w$N_ROLE_YEARS_KNOWN) | w$N_ROLE_YEARS_KNOWN<7),
      'Incomplete seven-year role-count coverage',role,'coverage')
  counts <- d[ROLE==role_label,c(list(N_MEMBERS=.N),lapply(.SD,sum)),by=.(RBD,AGNO),.SDcols=ff]
  rr <- if(role=='leadership') lr[,.(RBD,AGNO,N_ROLE,N_IDENTIFIED)] else
    roster[,.(RBD,AGNO,N_ROLE=get(if(role=='teacher') 'N_TEACHER' else 'N_COUNSELOR'),
      N_IDENTIFIED=get(if(role=='teacher') 'N_TEACHER_IDENTIFIED' else 'N_COUNSELOR_IDENTIFIED'))]
  annual <- merge(CJ(RBD=x$RBD,AGNO=2018:2024),rr,by=c('RBD','AGNO'),all.x=TRUE)
  annual <- merge(annual,counts,by=c('RBD','AGNO'),all.x=TRUE)
  fields <- c('N_MEMBERS',ff)
  annual[is.na(N_MEMBERS) & N_IDENTIFIED==0,(fields):=as.list(rep(0L,length(fields)))]
  stopifnot(all(annual[!is.na(N_IDENTIFIED),N_MEMBERS==N_IDENTIFIED]),
            all(annual[!is.na(N_ROLE),N_MEMBERS==N_ROLE]))
  for (nm in ff) annual[,(nm):=fifelse(N_ROLE>0 & !is.na(N_ROLE),get(nm)/N_ROLE,NA_real_)]
  pp <- annual[,c(list(N_KNOWN=sum(!is.na(N_ROLE)),N_ACTIVE=sum(N_ROLE>0,na.rm=TRUE)),
    lapply(.SD,function(v) if(all(is.na(v))) NA_real_ else mean(v,na.rm=TRUE))),by=RBD,.SDcols=ff]
  pp[N_KNOWN<7 | N_ACTIVE==0,(ff):=NA_real_]
  pp <- pp[match(x$RBD,pp$RBD)]
  pp[,ROLE:=role]
  periods[[role]] <- pp
  for (j in seq_along(metrics)) add(paste0(role,'__',metrics[j]),pp[[metrics[j]]],cn[j],role,'linked_credentials')
  for (nm in setdiff(ff,metrics)) add(paste0(role,'__',nm),pp[[nm]],
    c(MATCH_ANY_BY_STAFF_YEAR='Any as-of qualification database match',
      MATCH_UNDERGRAD_BY_STAFF_YEAR='As-of undergraduate database match',
      ANY_UNCOVERED_INSTITUTION='Observed degree with institution premium unavailable')[[nm]],role,'coverage')
}
# Check the generic aggregation against the previous verified counselor output.
old <- fread(paths[['counselor_period']])
new <- melt(periods[['counselor']],id.vars='RBD',measure.vars=metrics,variable.name='METRIC',value.name='NEW')
check <- merge(old[,.(RBD,METRIC,RATE)],new,by=c('RBD','METRIC'))
stopifnot(nrow(check)==nrow(old),identical(is.na(check$RATE),is.na(check$NEW)),
          max(abs(check$RATE-check$NEW),na.rm=TRUE)<1e-12)
dictionary <- rbindlist(dict)
dictionary[,N_OBSERVED:=vapply(FEATURE,function(nm) sum(is.finite(x[[nm]])),integer(1))]
dictionary[,N_UNIQUE_OBSERVED:=vapply(FEATURE,function(nm) uniqueN(x[[nm]][is.finite(x[[nm]])]),integer(1))]
stopifnot(!anyDuplicated(dictionary$FEATURE),nrow(x)==3682,!anyDuplicated(x$RBD))
set.seed(20260920L)
folds <- data.table(RBD=x$RBD,OUTER_FOLD=sample(rep(1:5,length.out=nrow(x))),
  FINAL_INNER_FOLD=sample(rep(1:5,length.out=nrow(x))))
fwrite(x,file.path(out,'school_predictors.csv'),na='NA')
fwrite(dictionary,file.path(out,'predictor_dictionary.csv'))
fwrite(rbindlist(excluded),file.path(out,'excluded_candidates.csv'))
fwrite(rbindlist(periods),file.path(out,'school_credential_period.csv'),na='NA')
fwrite(folds,file.path(out,'school_folds.csv'))
fwrite(fread(paths[['outcomes']]),file.path(out,'outcome_dictionary.csv'))
stopifnot(identical(hash,tools::md5sum(paths)))
fwrite(data.table(SOURCE=names(paths),PATH=unname(paths),MD5=unname(hash)),file.path(out,'input_manifest.csv'))
print(dictionary[,.(N_FEATURES=.N),by=ROLE])
cat('Verified: unique joins, roster denominators, unchanged sources and previous counselor credential shares.\n')
