# School-level titulados linkage coverage, not a staff-quality measure.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
script <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE))
task <- dirname(normalizePath(script,winslash="/"))
root <- normalizePath(file.path(task,"../../.."),winslash="/")
source(file.path(root,"code/codex/staff_quality_va/staff_association_helpers.R"))
setDTthreads(4L)
base <- file.path(root,"data/clean/titulados_staff_linkage")
out <- file.path(base,"school_coverage")
fig <- file.path(root,"output/figures/titulados_school_coverage")
if(dir.exists(out) && length(list.files(out)) && !"--overwrite" %in% commandArgs())
  stop("School coverage outputs exist; pass --overwrite to rebuild these outputs only.")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create(fig,recursive=TRUE,showWarnings=FALSE)
paths <- c(members=file.path(base,"staff_membership_person_school_year.csv.gz"),
  flags=file.path(base,"staff_match_person_role_year.csv.gz"),
  teacher_roster=file.path(root,"data/clean/staff_quality_va/staff_school_year_roster.csv"),
  leader_roster=file.path(root,"data/clean/leadership_quality_va/leadership_school_year_roster.csv"),
  context=file.path(root,"data/clean/staff_quality_va/staff_school_context.csv"),
  outcomes=file.path(root,"data/clean/staff_quality_va/staff_va_outcome_dictionary.csv"),
  va=file.path(root,"output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv"))
hashes <- tools::md5sum(paths)
roles <- c("HS teachers","Orientadores","Leadership")
context <- fread(paths[["context"]])
stopifnot(nrow(context)==3682L,!anyDuplicated(context$RBD))
members <- fread(paths[["members"]],colClasses=c(MRUN="character"))[ROLE %chin% roles]
flags <- fread(paths[["flags"]],select=c("MRUN","ROLE","AGNO","MATCH_ANY_2007_2025",
  "FIRST_UNDERGRAD_YEAR","MATCH_ANY_BY_STAFF_YEAR","MATCH_UNDERGRAD_BY_STAFF_YEAR"),
  colClasses=c(MRUN="character"))[ROLE %chin% roles]
metrics <- c("ANY_DB","UG_DB","ANY_ASOF","UG_ASOF")
flags[,`:=`(ANY_DB=as.integer(MATCH_ANY_2007_2025),UG_DB=as.integer(!is.na(FIRST_UNDERGRAD_YEAR)),
  ANY_ASOF=as.integer(MATCH_ANY_BY_STAFF_YEAR),UG_ASOF=as.integer(MATCH_UNDERGRAD_BY_STAFF_YEAR))]
stopifnot(!anyDuplicated(members,by=c("MRUN","ROLE","AGNO","RBD")),
  !anyDuplicated(flags,by=c("MRUN","ROLE","AGNO")),all(members$AGNO %in% 2018:2024))
joined <- merge(members,flags[,c("MRUN","ROLE","AGNO",metrics),with=FALSE],
  by=c("MRUN","ROLE","AGNO"),all.x=TRUE)
stopifnot(nrow(joined)==nrow(members),!anyNA(joined[,..metrics]))
counts <- joined[,c(list(N_MEMBERS=.N),lapply(.SD,sum)),by=.(RBD,ROLE,AGNO),.SDcols=metrics]
r <- fread(paths[["teacher_roster"]]); l <- fread(paths[["leader_roster"]])
roster <- rbindlist(list(
  r[,.(RBD,AGNO,ROLE="HS teachers",N_ROLE=N_TEACHER,N_IDENTIFIED=N_TEACHER_IDENTIFIED)],
  r[,.(RBD,AGNO,ROLE="Orientadores",N_ROLE=N_COUNSELOR,N_IDENTIFIED=N_COUNSELOR_IDENTIFIED)],
  l[,.(RBD,AGNO,ROLE="Leadership",N_ROLE,N_IDENTIFIED)]))
stopifnot(!anyDuplicated(roster,by=c("RBD","ROLE","AGNO")))
annual_wide <- merge(CJ(RBD=context$RBD,ROLE=roles,AGNO=2018:2024),roster,
  by=c("RBD","ROLE","AGNO"),all.x=TRUE)
annual_wide <- merge(annual_wide,counts,by=c("RBD","ROLE","AGNO"),all.x=TRUE)
# Missing membership rows are zero only when the original roster confirms none.
annual_wide[is.na(N_MEMBERS) & N_IDENTIFIED==0,c("N_MEMBERS",metrics):=as.list(rep(0L,5))]
stopifnot(all(annual_wide[!is.na(N_IDENTIFIED),!is.na(N_MEMBERS) & N_MEMBERS==N_IDENTIFIED]),
  all(annual_wide[!is.na(N_ROLE),N_ROLE==N_MEMBERS]))
annual <- melt(annual_wide,id.vars=c("RBD","ROLE","AGNO","N_ROLE","N_IDENTIFIED","N_MEMBERS"),
  measure.vars=metrics,variable.name="METRIC",value.name="N_MATCHED",variable.factor=FALSE)
annual[,RATE:=fifelse(!is.na(N_ROLE) & N_ROLE>0,N_MATCHED/N_ROLE,NA_real_)]
annual[,STATUS:=fcase(is.na(N_ROLE),"unknown_role_count",N_ROLE==0,"role_absent",default="observed")]
stopifnot(all(annual[!is.na(RATE),RATE>=0 & RATE<=1]))
safe_mean <- function(x) if(all(is.na(x))) NA_real_ else mean(x,na.rm=TRUE)
period <- annual[,.(N_KNOWN_YEARS=sum(!is.na(N_ROLE)),N_ACTIVE_YEARS=sum(N_ROLE>0,na.rm=TRUE),
  N_RATE_YEARS=sum(!is.na(RATE)),N_STAFF_PERSON_YEARS=sum(N_ROLE,na.rm=TRUE),
  N_MATCHED_PERSON_YEARS=sum(N_MATCHED[!is.na(N_ROLE)],na.rm=TRUE),
  RATE_OBSERVED_YEARS=safe_mean(RATE)),by=.(RBD,ROLE,METRIC)]
period[,RATE:=fifelse(N_KNOWN_YEARS==7L & N_ACTIVE_YEARS>0,RATE_OBSERVED_YEARS,NA_real_)]
period[,RATE_POOLED_PERSON_YEARS:=fifelse(N_KNOWN_YEARS==7L & N_STAFF_PERSON_YEARS>0,
  N_MATCHED_PERSON_YEARS/N_STAFF_PERSON_YEARS,NA_real_)]
period[,STATUS:=fcase(N_KNOWN_YEARS<7L,"incomplete_role_count_coverage",
  N_ACTIVE_YEARS==0,"role_never_observed",default="observed")]
# Alternative for the literal fraction of distinct staff ever at each school.
# Only use time-invariant full-database flags for this turnover sensitivity.
distinct <- unique(joined[,.(RBD,ROLE,MRUN,ANY_DB,UG_DB)])
unique_rates <- distinct[,.(N_UNIQUE_STAFF=.N,ANY_DB_UNIQUE=mean(ANY_DB),UG_DB_UNIQUE=mean(UG_DB)),by=.(RBD,ROLE)]
period <- merge(period,unique_rates,by=c("RBD","ROLE"),all.x=TRUE)
period[,RATE_UNIQUE_PEOPLE:=fcase(METRIC=="ANY_DB" & STATUS=="observed",ANY_DB_UNIQUE,
  METRIC=="UG_DB" & STATUS=="observed",UG_DB_UNIQUE,default=NA_real_)]
period[,c("ANY_DB_UNIQUE","UG_DB_UNIQUE"):=NULL]
period <- merge(period,context[,.(RBD,N_VA_STUDENTS,NOM_RBD)],by="RBD",all.x=TRUE)
wide <- dcast(period,RBD+ROLE+NOM_RBD+N_VA_STUDENTS+N_KNOWN_YEARS+N_ACTIVE_YEARS+N_UNIQUE_STAFF+STATUS~METRIC,value.var="RATE")
stopifnot(nrow(wide)==3682L*3L,!anyDuplicated(wide,by=c("RBD","ROLE")))
distribution <- period[!is.na(RATE),.(N_SCHOOLS=.N,MEAN=mean(RATE),SD=sd(RATE),
  P10=as.numeric(quantile(RATE,.1)),P25=as.numeric(quantile(RATE,.25)),MEDIAN=median(RATE),
  P75=as.numeric(quantile(RATE,.75)),P90=as.numeric(quantile(RATE,.9)),
  MIN=min(RATE),MAX=max(RATE),N_ZERO=sum(RATE==0),N_ONE=sum(RATE==1)),by=.(ROLE,METRIC)]
availability <- period[METRIC=="ANY_DB",.(N_SCHOOLS=.N),by=.(ROLE,STATUS)]
outcomes <- fread(paths[["outcomes"]])
va <- fread(paths[["va"]],select=c("school_rbd","analysis_sample","outcome",
  "controlled_value_added_centered_student","controlled_value_added_eb_centered_student"))[
    analysis_sample=="All" & outcome %chin% outcomes$OUTCOME]
setnames(va,c("school_rbd","outcome","controlled_value_added_centered_student",
  "controlled_value_added_eb_centered_student"),c("RBD","OUTCOME","Y_RAW","Y"))
stopifnot(!anyDuplicated(va,by=c("RBD","OUTCOME")),setequal(va$OUTCOME,outcomes$OUTCOME))
analysis <- merge(period,va[,.(RBD,OUTCOME,Y,Y_RAW)],by="RBD",all.x=TRUE,allow.cartesian=TRUE)
correlations <- analysis[,q_correlations(data.table(X=RATE,Y,Y_RAW,N_VA_STUDENTS),adjusted=FALSE),
  by=.(ROLE,METRIC,OUTCOME)]
correlations[,PEARSON_Q_BH:=p.adjust(PEARSON_P,"BH"),by=.(ROLE,METRIC)]
correlations <- merge(correlations,outcomes,by="OUTCOME",all.x=TRUE)
robustness <- rbindlist(lapply(c("pooled_person_years","unique_people","three_active_years"),function(spec) {
  d <- copy(analysis[METRIC=="ANY_DB"])
  d[,X:=switch(spec,pooled_person_years=RATE_POOLED_PERSON_YEARS,
    unique_people=RATE_UNIQUE_PEOPLE,three_active_years=fifelse(N_ACTIVE_YEARS>=3,RATE,NA_real_))]
  d[,cbind(SPEC=spec,q_correlations(.SD,adjusted=FALSE)),by=.(ROLE,OUTCOME),
    .SDcols=c("X","Y","Y_RAW","N_VA_STUDENTS")]
}))
fwrite(annual,file.path(out,"school_staff_match_annual.csv.gz"),na="NA")
fwrite(period,file.path(out,"school_staff_match_period_long.csv"),na="NA")
fwrite(wide,file.path(out,"school_staff_match_rates.csv"),na="NA")
fwrite(distribution,file.path(out,"school_staff_match_distribution.csv"),na="NA")
fwrite(availability,file.path(out,"school_staff_match_availability.csv"))
fwrite(correlations,file.path(out,"school_staff_match_va_correlations.csv"),na="NA")
fwrite(robustness,file.path(out,"school_staff_match_va_robustness.csv"),na="NA")
fwrite(analysis,file.path(out,"school_staff_match_va_analysis.csv.gz"),na="NA")
fwrite(data.table(METRIC=metrics,DEFINITION=c("Any exact-ID award anywhere in 2007-2025 DB",
  "Any undergraduate award anywhere in 2007-2025 DB","Any award reported/obtained by staff year",
  "Any undergraduate award reported/obtained by staff year")),file.path(out,"school_staff_match_dictionary.csv"))
stopifnot(identical(hashes,tools::md5sum(paths)))
fwrite(data.table(SOURCE=names(paths),PATH=unname(paths),MD5=unname(hashes)),file.path(out,"school_staff_match_input_manifest.csv"))

# Percent of schools in fixed five-percentage-point coverage bins, within role.
h <- period[METRIC=="ANY_DB" & is.finite(RATE),.(ROLE,RATE)]
h[,BIN:=pmin(19L,floor(RATE*20))]
h <- h[,.(N=.N),by=.(ROLE,BIN)]
h[,PCT:=100*N/sum(N),by=ROLE]
h[,ROLE:=factor(ROLE,levels=roles)]
means <- distribution[METRIC=="ANY_DB"]
means[,ROLE:=factor(ROLE,levels=roles)]
labels <- setNames(paste0(as.character(means$ROLE),"\n",format(means$N_SCHOOLS,big.mark=","),
  " schools; mean ",sprintf("%.1f%%",100*means$MEAN)),as.character(means$ROLE))
p <- ggplot(h,aes(2.5+5*BIN,PCT,fill=ROLE)) + geom_col(width=4.7,show.legend=FALSE) +
  geom_vline(data=means,aes(xintercept=100*MEAN),linetype="dashed",color="#aa4d32",linewidth=.65) +
  facet_wrap(~ROLE,nrow=1,labeller=as_labeller(labels)) +
  scale_fill_manual(values=c("#276b91","#287c70","#80648f")) +
  scale_x_continuous(breaks=seq(0,100,25),limits=c(0,100),expand=expansion(mult=c(.01,.01))) +
  labs(title="How much of each school's staff appears in titulados?",
    subtitle="Any qualification in the 2007-2025 database; mean annual coverage over 2018-2024",
    x="Staff matched (%)",y="Schools (%)",caption="Dashed line: mean across schools. Each school has equal weight. Absent roles are not zero coverage.") +
  theme_minimal(base_size=12) + theme(panel.grid.minor=element_blank(),strip.text=element_text(face="bold"),
    panel.spacing=grid::unit(1.6,"lines"),
    plot.title=element_text(face="bold"),plot.margin=margin(12,14,10,10))
ggsave(file.path(fig,"school_staff_match_distribution.png"),p,width=11,height=4.4,dpi=200,bg="white")
hm <- correlations[METRIC=="ANY_DB"]
hm[,ROLE:=factor(ROLE,levels=roles)]
hm[,LABEL:=factor(LABEL,levels=rev(outcomes$LABEL))]
hm[,CELL:=sprintf("%.2f\n(n=%s)",fifelse(abs(PEARSON_R)<.005,0,PEARSON_R),format(N,big.mark=",",trim=TRUE))]
lim <- max(.4,ceiling(max(abs(hm$PEARSON_R),na.rm=TRUE)*10)/10)
p <- ggplot(hm,aes(ROLE,LABEL,fill=PEARSON_R)) + geom_tile(color="white",linewidth=.8) +
  geom_text(aes(label=CELL,color=abs(PEARSON_R)>.6*lim),size=3.4,lineheight=.9) +
  scale_color_manual(values=c("FALSE"="#203244","TRUE"="white"),guide="none") +
  scale_fill_gradient2(low="#a64838",mid="#fafaf7",high="#247d78",midpoint=0,limits=c(-lim,lim),name="Pearson r") +
  labs(title="School staff matching rates and value added",subtitle="Any titulados match; unweighted correlations with saved EB VA",
    x=NULL,y=NULL,caption="Each cell uses schools observed on both measures. Descriptive associations, not causal effects.") +
  theme_minimal(base_size=12) + theme(panel.grid=element_blank(),legend.position="bottom",
    plot.title=element_text(face="bold"),axis.text=element_text(color="#203244"),plot.margin=margin(12,14,10,10))
ggsave(file.path(fig,"school_staff_match_va_correlations.png"),p,width=8.2,height=8.7,dpi=200,bg="white")
print(distribution[METRIC=="ANY_DB",.(ROLE,N_SCHOOLS,MEAN,P10,MEDIAN,P90,N_ZERO,N_ONE)])
print(correlations[METRIC=="ANY_DB" & OUTCOME %chin% c("z_year_math_max","z_year_leng_max","high_paying_field_m1","high_inst_m1","log_program_income_full_clp_m1"),
  .(ROLE,LABEL,N,PEARSON_R,SPEARMAN_R,STUDENT_WEIGHTED_R)])
message("School coverage, distributions and VA correlations saved; original sources unchanged.")
