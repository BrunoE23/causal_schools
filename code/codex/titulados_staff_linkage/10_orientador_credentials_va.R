# Eight observed counselor credential shares versus non-score school VA.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
script <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE))
task <- dirname(normalizePath(script,winslash="/"))
root <- normalizePath(file.path(task,"../../.."),winslash="/")
source(file.path(task,"credential_helpers.R"))
source(file.path(root,"code/codex/staff_quality_va/staff_association_helpers.R"))
setDTthreads(4L)
base <- file.path(root,"data/clean/titulados_staff_linkage")
out <- file.path(base,"orientador_credentials_va")
fig <- file.path(root,"output/figures/orientador_credentials_va")
if(dir.exists(out) && length(list.files(out)) && !"--overwrite" %in% commandArgs())
  stop("Outputs exist; pass --overwrite to replace only these derived outputs.")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dir.create(fig,recursive=TRUE,showWarnings=FALSE)
paths <- c(members=file.path(base,"staff_membership_person_school_year.csv.gz"),
  credentials=file.path(base,"credentials/staff_credentials_person_role_year.csv.gz"),
  roster=file.path(root,"data/clean/staff_quality_va/staff_school_year_roster.csv"),
  context=file.path(root,"data/clean/staff_quality_va/staff_school_context.csv"),
  outcomes=file.path(root,"data/clean/staff_quality_va/staff_va_outcome_dictionary.csv"),
  va=file.path(root,"output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv"))
before <- tools::md5sum(paths)
metrics <- credential_names
labels <- c("UG at high-premium institution","Any non-UG qualification","Non-UG at high-premium institution",
  "Any award at high-premium institution","Orientation-related qualification","Any magister",
  "Magister at high-premium institution","Orientation-related magister")
dictionary <- data.table(METRIC=metrics,METRIC_LABEL=labels,
  MATCHED_DENOMINATOR=c("UG matched",rep("Any award matched",7)))
context <- fread(paths[["context"]])
m <- fread(paths[["members"]],colClasses=c(MRUN="character"))[ROLE=="Orientadores"]
f <- fread(paths[["credentials"]],select=c("MRUN","ROLE","AGNO",metrics,
  "MATCH_ANY_BY_STAFF_YEAR","MATCH_UNDERGRAD_BY_STAFF_YEAR","ANY_UNCOVERED_INSTITUTION"),
  colClasses=c(MRUN="character"))[ROLE=="Orientadores"]
stopifnot(!anyDuplicated(m,by=c("MRUN","RBD","AGNO")),!anyDuplicated(f,by=c("MRUN","AGNO")))
d <- merge(m,f,by=c("MRUN","ROLE","AGNO"),all.x=TRUE)
stopifnot(nrow(d)==nrow(m),!anyNA(d[,..metrics]))
counts <- d[,c(list(N_MEMBERS=.N,N_MATCH_ANY=sum(MATCH_ANY_BY_STAFF_YEAR),
  N_MATCH_UG=sum(MATCH_UNDERGRAD_BY_STAFF_YEAR),N_UNCOVERED_INST=sum(ANY_UNCOVERED_INSTITUTION)),
  lapply(.SD,sum)),by=.(RBD,AGNO),.SDcols=metrics]
roster <- fread(paths[["roster"]])[,.(RBD,AGNO,N_ROLE=N_COUNSELOR,N_IDENTIFIED=N_COUNSELOR_IDENTIFIED)]
annual <- merge(CJ(RBD=context$RBD,AGNO=2018:2024),roster,by=c("RBD","AGNO"),all.x=TRUE)
annual <- merge(annual,counts,by=c("RBD","AGNO"),all.x=TRUE)
cc <- c("N_MEMBERS","N_MATCH_ANY","N_MATCH_UG","N_UNCOVERED_INST",metrics)
annual[is.na(N_MEMBERS) & N_IDENTIFIED==0,(cc):=as.list(rep(0L,length(cc)))]
stopifnot(all(annual[!is.na(N_IDENTIFIED),!is.na(N_MEMBERS) & N_MEMBERS==N_IDENTIFIED]),
  all(annual[!is.na(N_ROLE),N_ROLE==N_MEMBERS]))
annual <- melt(annual,id.vars=setdiff(names(annual),metrics),measure.vars=metrics,
  variable.name="METRIC",value.name="N_QUALIFIED",variable.factor=FALSE)
annual[,MATCHED_DENOM:=fifelse(METRIC=="UG_HIGH_PREMIUM",N_MATCH_UG,N_MATCH_ANY)]
annual[,`:=`(RATE=fifelse(N_ROLE>0 & !is.na(N_ROLE),N_QUALIFIED/N_ROLE,NA_real_),
  RATE_MATCHED=fifelse(N_ROLE>0 & !is.na(N_ROLE) & MATCHED_DENOM>0,N_QUALIFIED/MATCHED_DENOM,NA_real_),
  ANY_MATCH_RATE=fifelse(N_ROLE>0 & !is.na(N_ROLE),N_MATCH_ANY/N_ROLE,NA_real_),
  UG_MATCH_RATE=fifelse(N_ROLE>0 & !is.na(N_ROLE),N_MATCH_UG/N_ROLE,NA_real_))]
stopifnot(all(annual[!is.na(RATE),RATE>=0 & RATE<=1]),
  all(annual[!is.na(RATE_MATCHED),RATE_MATCHED>=0 & RATE_MATCHED<=1]))
avg <- function(x) if(all(is.na(x))) NA_real_ else mean(x,na.rm=TRUE)
period <- annual[,.(N_KNOWN_YEARS=sum(!is.na(N_ROLE)),N_ACTIVE_YEARS=sum(N_ROLE>0,na.rm=TRUE),
  N_MATCHED_YEARS=sum(!is.na(RATE_MATCHED)),RATE=avg(RATE),RATE_MATCHED=avg(RATE_MATCHED),
  ANY_MATCH_RATE=avg(ANY_MATCH_RATE),UG_MATCH_RATE=avg(UG_MATCH_RATE)),by=.(RBD,METRIC)]
period[N_KNOWN_YEARS<7 | N_ACTIVE_YEARS==0,c("RATE","RATE_MATCHED","ANY_MATCH_RATE","UG_MATCH_RATE"):=NA_real_]
# Matched-only sensitivity needs a nonempty matched denominator in >=80% of
# active role years. It is conditional on a selected observed-credential sample.
period[N_MATCHED_YEARS<ceiling(.8*N_ACTIVE_YEARS),RATE_MATCHED:=NA_real_]
period <- merge(period,context[,.(RBD,NOM_RBD,N_VA_STUDENTS)],by="RBD",all.x=TRUE)
outcomes <- fread(paths[["outcomes"]])[FAMILY!="scores"]
stopifnot(nrow(outcomes)==10L)
va <- fread(paths[["va"]],select=c("school_rbd","analysis_sample","outcome",
  "controlled_value_added_centered_student","controlled_value_added_eb_centered_student"))[
    analysis_sample=="All" & outcome %chin% outcomes$OUTCOME]
setnames(va,c("school_rbd","outcome","controlled_value_added_centered_student",
  "controlled_value_added_eb_centered_student"),c("RBD","OUTCOME","Y_RAW","Y"))
stopifnot(!anyDuplicated(va,by=c("RBD","OUTCOME")))
analysis <- merge(period,va[,.(RBD,OUTCOME,Y,Y_RAW)],by="RBD",all.x=TRUE,allow.cartesian=TRUE)
results <- rbindlist(lapply(c("all_orientadores","matched_only","three_active_years"),function(spec) {
  x <- copy(analysis)
  x[,X:=switch(spec,all_orientadores=RATE,matched_only=RATE_MATCHED,
    three_active_years=fifelse(N_ACTIVE_YEARS>=3,RATE,NA_real_))]
  x[,cbind(SPEC=spec,q_correlations(.SD,adjusted=FALSE)),by=.(METRIC,OUTCOME),
    .SDcols=c("X","Y","Y_RAW","N_VA_STUDENTS")]
}))
results[,PEARSON_Q_BH:=p.adjust(PEARSON_P,"BH"),by=SPEC]
partial <- analysis[,{
  x <- .SD[is.finite(RATE) & is.finite(Y) & is.finite(ANY_MATCH_RATE) & is.finite(UG_MATCH_RATE)]
  # A descriptive partial correlation, not a causal adjusted coefficient.
  rx <- residuals(lm(RATE ~ ANY_MATCH_RATE + UG_MATCH_RATE,data=x))
  ry <- residuals(lm(Y ~ ANY_MATCH_RATE + UG_MATCH_RATE,data=x))
  .(N_PARTIAL=nrow(x),COVERAGE_PARTIAL_R=if(sd(rx)>1e-12 & sd(ry)>1e-12) cor(rx,ry) else NA_real_)
},by=.(METRIC,OUTCOME)]
results <- merge(results,partial,by=c("METRIC","OUTCOME"),all.x=TRUE)
results[SPEC!="all_orientadores",c("N_PARTIAL","COVERAGE_PARTIAL_R"):=NA_real_]
results <- merge(merge(results,dictionary,by="METRIC"),outcomes,by="OUTCOME")
summary <- period[is.finite(RATE),.(N_SCHOOLS=.N,MEAN_SHARE=mean(RATE),MEDIAN_SHARE=median(RATE),
  N_POSITIVE=sum(RATE>0),N_MATCHED_SCHOOLS=sum(!is.na(RATE_MATCHED)),MEAN_MATCHED_SHARE=avg(RATE_MATCHED)),by=METRIC]
summary <- merge(summary,dictionary,by="METRIC")
fwrite(annual,file.path(out,"orientador_credentials_annual.csv.gz"),na="NA")
fwrite(period,file.path(out,"orientador_credentials_school_period.csv"),na="NA")
fwrite(analysis,file.path(out,"orientador_credentials_va_analysis.csv.gz"),na="NA")
fwrite(results,file.path(out,"orientador_credentials_va_correlations.csv"),na="NA")
fwrite(summary,file.path(out,"orientador_credentials_distribution.csv"),na="NA")
fwrite(dictionary,file.path(out,"orientador_credentials_dictionary.csv"))
stopifnot(identical(before,tools::md5sum(paths)))
fwrite(data.table(SOURCE=names(paths),PATH=unname(paths),MD5=unname(before)),file.path(out,"orientador_credentials_input_manifest.csv"))
hm <- results[SPEC=="all_orientadores"]
hm[,ROW:=factor(METRIC,levels=rev(metrics),labels=rev(labels))]
plotlabels <- c("Exam\ntaking","Higher-ed\nenrollment","STEM","High-premium\nfield","High-premium\ninstitution",
  "Projected\nincome","Income:\nfield only","Income:\ninst. only","Program\naccreditation","Institution\naccreditation")
hm[,COL:=factor(OUTCOME,levels=outcomes$OUTCOME,labels=plotlabels)]
hm[,CELL:=paste0(sprintf("%.2f",fifelse(abs(PEARSON_R)<.005,0,PEARSON_R)),fifelse(PEARSON_Q_BH<.05,"*",""))]
lim <- max(.3,ceiling(max(abs(hm$PEARSON_R))*10)/10)
p <- ggplot(hm,aes(COL,ROW,fill=PEARSON_R)) + geom_tile(color="white",linewidth=.7) +
  geom_text(aes(label=CELL,color=abs(PEARSON_R)>.6*lim),size=3.6) +
  scale_color_manual(values=c("FALSE"="#253444","TRUE"="white"),guide="none") +
  scale_fill_gradient2(low="#ac4d3a",mid="#fbfaf6",high="#247c79",midpoint=0,limits=c(-lim,lim),
    breaks=c(-lim,0,lim),name="Pearson r",guide=guide_colourbar(barwidth=grid::unit(2.4,"in"))) +
  labs(title="Orientador credentials and non-achievement school VA",
    subtitle="2018-2024 school credential shares; equally weighted schools and saved EB VA",
    x=NULL,y=NULL,caption=paste0("* BH-adjusted p < 0.05 across all 80 main tests. N = ",min(hm$N),"-",max(hm$N),
      " schools per cell.\nShares include all orientadores; zero means no qualifying credential observed. Correlations are not causal.")) +
  theme_minimal(base_size=12) + theme(panel.grid=element_blank(),legend.position="bottom",
    axis.text=element_text(color="#253444"),axis.text.x=element_text(size=10),
    plot.title=element_text(face="bold"),plot.margin=margin(12,14,10,10))
ggsave(file.path(fig,"orientador_credentials_nonachievement_va.png"),p,width=13,height=7,dpi=200,bg="white")
print(results[SPEC=="all_orientadores" & OUTCOME=="log_program_income_full_clp_m1",
  .(METRIC,N,PEARSON_R,PEARSON_Q_BH,SPEARMAN_R,COVERAGE_PARTIAL_R)])
message("Computed all eight credential associations, matched-only and active-year sensitivities.")
