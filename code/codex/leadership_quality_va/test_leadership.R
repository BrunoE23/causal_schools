suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE))
task <- dirname(normalizePath(script, winslash="/"))
source(file.path(task, "../docentes_educacion/staff_cleaning_helpers.R"))
source(file.path(task, "../staff_quality_va/staff_quality_helpers.R"))
source(file.path(task, "../staff_quality_va/staff_association_helpers.R"))
source(file.path(task, "leadership_helpers.R"))
setDTthreads(2L); checks <- 0L
check <- function(value, label) {if (!isTRUE(value)) stop("FAILED: ", label); checks <<- checks+1L; message("PASS: ", label)}
check(identical(l_role_flag(c(3L,4L,10L,15L,2L,7L,8L,16L,NA_integer_)), c(1L,1L,1L,1L,0L,0L,0L,0L,NA_integer_)), "Exactly the four requested leadership codes qualify")
raw <- CJ(AGNO=2013:2024, MRUN=as.character(101:106))
raw[, `:=`(RBD=1L, PERSONAS=1L, ID_IFP=4L, ID_IFS=0L)]
for (f in setdiff(staff_raw_fields(), names(raw))) set(raw,j=f,value=if(f %in% c("NOMBRE_SLEP","DOC_FEC_NAC")) NA_character_ else NA_integer_)
raw[, `:=`(COD_ENS_1=0L,COD_ENS_2=0L,NIVEL1=0L,NIVEL2=0L,SUBSECTOR1=0L,
  TIT_ID_1=1L,TIP_TIT_ID_1=14L,ESP_ID_1=142L,TIP_INSTI_ID_1=1L,ANO_TITULACION_1=2005L,DURACION_CARRERA_1=10,
  TIT_ID_2=3L,TIP_TIT_ID_2=30L,ESP_ID_2=300L,TIP_INSTI_ID_2=0L,ANO_SERVICIO_EE=0,ANO_SERVICIO_SISTEMA=0)]
for (f in grep("^MEN_",names(raw),value=TRUE)) set(raw,j=f,value=0L)
raw[MRUN=="101" & AGNO<2018,ID_IFP:=15L]
raw[MRUN=="102",`:=`(ID_IFP=1L,ID_IFS=10L)]
raw[MRUN=="103",`:=`(ID_IFP=7L,RBD=2L)]
raw[MRUN=="104" & AGNO>=2015 & AGNO<2020,ID_IFP:=16L]
raw[MRUN=="104" & AGNO>=2020,ID_IFP:=15L]
raw[MRUN=="106",ID_IFP:=3L]
raw[MRUN=="106" & AGNO>=2019,ID_IFP:=10L]
folder <- tempfile("leadership-fixtures-"); dir.create(folder)
cleaned <- rbindlist(lapply(2013:2024,function(year){f<-file.path(folder,paste0(year,".csv"));fwrite(raw[AGNO==year],f,sep=";");staff_clean_annual(f)$data}))
current <- cleaned[AGNO>=2018]
duplicate <- copy(current[MRUN=="105" & AGNO==2018]);duplicate[,PERSONAS:=0L]
prepared <- l_prepare(rbind(current,duplicate),c(1L,2L,3L))
check(!anyDuplicated(prepared$people,by=c("MRUN","RBD","AGNO")),"Duplicate appointments never duplicate leadership people")
check(all(prepared$people$TEACHER_HS_ANY==0L),"Leadership does not require an HS teaching assignment")
check(all(prepared$people[MRUN=="102",LEADER_ANY]==1L),"Secondary leadership retained")
check(all(prepared$people[MRUN=="102",LEADER_PRIMARY]==0L),"Secondary leadership not relabeled primary")
check(!"103" %in% prepared$people$MRUN,"Technical-pedagogical staff excluded")
check(!any(prepared$people$MRUN=="104" & prepared$people$AGNO<2020),"Current encargado code 16 excluded")
histcols<-c("MRUN","RBD","AGNO","PERSONAS","ID_IFP","ID_IFS","VALID_PERSON_ID","VALID_SCHOOL_ID")
h<-l_histories(cleaned[,..histcols],unique(prepared$people$MRUN));people<-l_attach(prepared$people,h)
check(people[MRUN=="101" & AGNO==2020,LEAD_PRIMARY_PRIOR_YEARS_OBSERVED]==7L,"Promotion retains cumulative leadership history")
check(people[MRUN=="101" & AGNO==2020,LEAD_ANY_CONSECUTIVE_YEARS_TO_DATE]==8L,"Within-leadership promotion preserves spell")
check(people[MRUN=="101" & AGNO==2020,LEADERSHIP_STATUS_CHANGED_PRIOR_RATE]==0,"Promotion is not leadership entry/exit")
check(people[MRUN=="101" & AGNO==2020,MAIN_FUNCTION_CHANGED_PRIOR_RATE]>0,"Detailed role change retained separately")
check(all(people[MRUN=="102",LEAD_PRIMARY_PRIOR_YEARS_OBSERVED]==0),"Secondary-only history not counted as primary experience")
check(people[MRUN=="101" & AGNO==2020,PRIOR_LEADERSHIP_YEARS_2015]==5,"2015 sensitivity excludes two earlier years")
check(people[MRUN=="101" & AGNO==2020,SCHOOL_LEADERSHIP_SPELL_2015]==6,"2015 sensitivity truncates school spell too")
check(is.na(h$py[MRUN=="104" & AGNO==2015,MAIN_FUNCTION_CHANGED]),"Historical code break excluded from transition denominator")
check(people[MRUN=="104" & AGNO==2020,PRE2015_CODE4_PRIOR]==1,"Ambiguous earlier director code flagged")
truncated<-l_histories(cleaned[AGNO<=2020,..histcols],unique(prepared$people$MRUN))
check(isTRUE(all.equal(h$py[AGNO<=2020],truncated$py)),"No future observations alter earlier person histories")
check(isTRUE(all.equal(h$sy[AGNO<=2020],truncated$sy)),"No future observations alter earlier school histories")
check(all(people$YEARS_AT_SCHOOL==0),"Reported tenure preserved, not replaced by spells")
counts<-l_aggregate(people,prepared$roster,c(1L,2L,3L))
check(all(counts$roster[RBD==2,N_ROLE]==0),"Observed nonleadership school is known zero")
check(all(is.na(counts$roster[RBD==3,N_ROLE])),"Absent roster stays unknown")
check(all(is.na(counts$period[RBD %in% 2:3,VALUE])),"No leaders is not zero leader characteristics")
altered<-copy(people);altered[AGNO==2018,UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED:=NA_real_]
gated<-l_aggregate(altered,prepared$roster,c(1L,2L,3L))
check(is.na(gated$annual[RBD==1 & AGNO==2018 & METRIC=="university_share",VALUE]),"Unknown credentials trigger annual coverage gate")
check(gated$period[RBD==1 & METRIC=="university_share",VALUE]==1,"Six valid active years pass period gate")
unknown<-copy(current[AGNO==2018]);unknown[MRUN=="103",ID_IFS:=NA_integer_]
u<-l_prepare(unknown,c(1L,2L));check(is.na(u$roster[RBD==2,N_ROLE]),"Unknown secondary function cannot become no leaders")
set.seed(921); fake<-data.table(RBD=1:180,N_ACTIVE_YEARS=7L,prior_role_years=rnorm(180),school_role_spell_years=rnorm(180),university_share=runif(180),teaching_title_share=runif(180),VA=rnorm(180))
components<-c("prior_role_years","school_role_spell_years","university_share","teaching_title_share")
fit<-q_fit_indices(copy(fake),"leadership",components)
fake[,VA:=100*VA];other<-q_fit_indices(copy(fake),"leadership",components)
check(identical(fit$wide$balanced_index,other$wide$balanced_index),"Leadership weights do not depend on VA")
check(identical(fit$parameters$COMPONENT,components),"Explicit leadership title component, not HS teaching title")
check(abs(sd(fit$wide$balanced_index)-1)<1e-12,"Leadership index normalized to SD one")
message("All ",checks," leadership tests passed.")
