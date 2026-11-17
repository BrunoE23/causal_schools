# Descriptive pre/post SAE enrollment trajectories by fixed retrospective VA.
args <- commandArgs(trailingOnly=TRUE)
# Separate short workers bound memory on the shared Windows research machine.
if (!length(args)) {
  for (mode in c("z_year_math_max","admission_exam_taker","high_paying_field_m1","high_inst_m1","report")) {
    status <- system2(file.path(R.home("bin"),"Rscript.exe"),c("code/codex/secondary_sae_spots/05_analyze_enrollment_trends.R",mode))
    stopifnot(status==0L)
  }
  quit(save="no")
}
mode <- args[1]
suppressPackageStartupMessages({library(data.table); library(fixest)})
options(datatable.auto.index=FALSE,datatable.use.index=FALSE)
setDTthreads(4L)
setFixest_nthreads(2L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash="/")
task <- file.path(root,"data/clean/secondary_sae_spots")
out <- file.path(task,"enrollment_trends")
fig <- file.path(root,"output/figures/secondary_sae_spots")
report <- file.path(root,"output/reports/secondary_sae_spots")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD","C:/Users/brunem/Box/causal_schools")
va_path <- file.path(data_root,"data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
inputs <- c(file.path(out,"school_year_enrollment.csv"),file.path(task,"school_year_spots.csv"),va_path)
stopifnot(all(file.info(inputs)$size<1e8))
en <- fread(inputs[1]); cap <- fread(inputs[2])
en[,source_region:=fifelse(source_region==16L,8L,source_region)]
cap[,region:=fifelse(region==16L,8L,region)]
va <- fread(va_path,select=c("outcome","school_rbd","analysis_sample","controlled_value_added_eb_centered_student","eb_reliability"))[analysis_sample=="All"]
setnames(va,c("school_rbd","controlled_value_added_eb_centered_student"),c("rbd","va"))
specs <- data.table(outcome=c("z_year_math_max","admission_exam_taker","high_paying_field_m1","high_inst_m1"),
  label=c("Math achievement","Exam taking","High-premium field","High-premium institution"),ordering=1:4)
va <- merge(va,specs,by="outcome")
stopifnot(!anyDuplicated(va[,.(rbd,outcome)]), !anyDuplicated(en[,.(rbd,year)]))

# Regional first entry cohort, not the preceding application/process year.
timing <- data.table(region=1:15)
timing[,entry_year:=fcase(region==12L,2017L,region %in% c(1L,4L,6L,10L),2018L,region==13L,2020L,default=2019L)]
inferred <- cap[capacity>0,.(first_process=min(process_year)),by=region]
timing <- merge(timing,inferred,by="region")
stopifnot(nrow(timing)==15L,all(timing$entry_year==timing$first_process+1L))
school <- cap[capacity>0][order(process_year),.(region=last(region),school_name=last(school_name),listed_2021=any(process_year==2021L)),by=rbd]
school <- merge(school,timing[,.(region,entry_year)],by="region")
panel <- merge(CJ(rbd=school$rbd,year=2010:2025),school,by="rbd")
panel <- merge(panel,en[,.(rbd,year,secondary_students,grade9_students,source_region)],by=c("rbd","year"),all.x=TRUE)
stopifnot(all(panel[!is.na(source_region),source_region==region]))
panel[,reported_secondary:=!is.na(secondary_students)]
for (v in c("secondary_students","grade9_students")) set(panel,which(is.na(panel[[v]])),v,0L)
panel[,event_time:=year-entry_year]
base <- panel[event_time %in% -3:-1,.(baseline_secondary=mean(secondary_students),baseline_grade9=mean(grade9_students),
  positive_pre_secondary=all(secondary_students>0),positive_pre_grade9=all(grade9_students>0)),by=rbd]
pre5 <- panel[event_time %in% -5:-1,.(positive_pre5_secondary=all(secondary_students>0),positive_pre5_grade9=all(grade9_students>0)),by=rbd]
base <- merge(base,pre5,by="rbd")
panel <- merge(panel,base,by="rbd")
cohort <- merge(school,base,by="rbd")[positive_pre_secondary==TRUE]
matched <- merge(cohort,va,by="rbd")
matched <- matched[is.finite(va)]
matched[,`:=`(va_threshold=as.numeric(quantile(va,.75,type=7)),z_va=(va-mean(va))/sd(va)),by=outcome]
matched[,high_va:=as.integer(va>=va_threshold)]
stopifnot(!anyDuplicated(matched[,.(rbd,outcome)]))
sample_audit <- matched[,.(ever_SAE_schools=nrow(school),pre_incumbents=nrow(cohort),matched_schools=.N,
  high_VA_schools=sum(high_va),other_schools=sum(1L-high_va),grade9_eligible=sum(positive_pre_grade9),
  threshold=first(va_threshold),median_baseline_students=median(baseline_secondary)),by=.(outcome,label,ordering)]
dat <- merge(panel[,.(rbd,year,event_time,secondary_students,grade9_students,reported_secondary)],matched,by="rbd",allow.cartesian=TRUE)
dat[,`:=`(secondary_index=100*secondary_students/baseline_secondary,grade9_index=100*grade9_students/baseline_grade9)]
dat[,region_year:=interaction(region,year,drop=TRUE)]
stopifnot(nrow(dat)==16L*nrow(matched),!anyDuplicated(dat[,.(rbd,outcome,year)]))
fwrite(timing,file.path(out,"regional_entry_timing.csv"))
fwrite(sample_audit,file.path(out,"trend_sample_audit.csv"))
fwrite(cohort,file.path(out,"incumbent_school_cohort.csv"))
fwrite(dat,file.path(out,"enrollment_va_panel.csv"))
fwrite(data.table(path=inputs,bytes=file.info(inputs)$size,md5=unname(tools::md5sum(inputs))),file.path(out,"trend_input_manifest.csv"))
rm(en,cap,va,panel,base,pre5,matched,school,cohort)
invisible(gc())

metrics <- data.table(metric=c("secondary","grade9"),metric_label=c("All secondary grades","First-year secondary"))
variations <- c("main","exclude_2020_2021","baseline_100plus","baseline_enrollment_weighted","continuous_VA","listed_2021","established_five_pre_years")
results <- events <- list(); k<-0L; ek<-0L
dir.create(file.path(out,"models"),showWarnings=FALSE)
for (o in specs[outcome==mode,outcome]) for (m in metrics$metric) for (s in variations) {
  message(o," / ",m," / ",s)
  d <- copy(dat[outcome==o & event_time %between% c(-5L,5L)])
  if(m=="grade9") d<-d[positive_pre_grade9==TRUE]
  d[,`:=`(y=get(paste0(m,"_index")),baseline=get(paste0("baseline_",m)))]
  if(s=="exclude_2020_2021") d<-d[!year %in% c(2020L,2021L)]
  if(s=="baseline_100plus") d<-d[baseline_secondary>=100]
  if(s=="listed_2021") d<-d[listed_2021==TRUE]
  if(s=="established_five_pre_years") d<-d[get(paste0("positive_pre5_",m))==TRUE]
  d[,quality:=if(s=="continuous_VA") z_va else high_va]
  d[,`:=`(q_pre=quality*event_time,q_jump=quality*as.integer(event_time>=0),q_change=quality*pmax(event_time,0L))]
  d[,weight:=if(s=="baseline_enrollment_weighted") baseline else 1]
  fit <- feols(y~q_pre+q_jump+q_change|rbd+region_year,data=d,weights=~weight,vcov=~region,notes=FALSE,lean=TRUE,mem.clean=TRUE)
  b<-coef(fit); V<-vcov(fit); df<-uniqueN(d$region)-1L; critical<-qt(.975,df)
  contrasts <- list(pre=c(1,0,0),post=c(1,0,1),slope_change=c(0,0,1),level_jump=c(0,1,0))
  stopifnot(identical(names(b),c("q_pre","q_jump","q_change")),all(is.finite(b)),nobs(fit)==nrow(d))
  for (term in names(contrasts)) {
    a<-contrasts[[term]]; est<-sum(a*b); se<-sqrt(as.numeric(t(a)%*%V%*%a)); k<-k+1L
    results[[k]]<-data.table(outcome=o,label=specs[outcome==o,label],metric=m,specification=s,contrast=term,
      estimate=est,se=se,lo=est-critical*se,hi=est+critical*se,p=2*pt(-abs(est/se),df),df=df,
      n_schools=uniqueN(d$rbd),n_school_years=nrow(d),n_high=uniqueN(d[high_va==1L,rbd]),regions=uniqueN(d$region))
  }
  saveRDS(fit,file.path(out,"models",paste0(paste(o,m,s,sep="__"),".rds")))
  if (s=="main") {
    ef<-feols(y~i(event_time,high_va,ref=-1)|rbd+region_year,data=d,vcov=~region,notes=FALSE,lean=TRUE,mem.clean=TRUE)
    eb<-coef(ef); es<-sqrt(diag(vcov(ef))); ek<-ek+1L
    events[[ek]]<-rbind(data.table(outcome=o,metric=m,event_time=as.integer(sub(".*::(-?[0-9]+):high_va","\\1",names(eb))),
      estimate=eb,se=es,lo=eb-critical*es,hi=eb+critical*es),
      data.table(outcome=o,metric=m,event_time=-1L,estimate=0,se=0,lo=0,hi=0))
    saveRDS(ef,file.path(out,"models",paste0(paste(o,m,"event_study",sep="__"),".rds")))
    rm(ef)
  }
  rm(fit,d); invisible(gc())
}
if (mode!="report") {
  stopifnot(length(results)==56L,length(events)==2L)
  fwrite(rbindlist(results),file.path(out,paste0("regression_part_",mode,".csv")))
  fwrite(rbindlist(events),file.path(out,paste0("event_part_",mode,".csv")))
  quit(save="no")
}
res<-rbindlist(lapply(specs$outcome,function(o)fread(file.path(out,paste0("regression_part_",o,".csv")))))
res[,p_bh_four_VA:=p.adjust(p,"BH"),by=.(metric,specification,contrast)]
fwrite(res,file.path(out,"segmented_trend_results.csv"))
ev<-rbindlist(lapply(specs$outcome,function(o)fread(file.path(out,paste0("event_part_",o,".csv")))))[order(outcome,metric,event_time)]
fwrite(ev,file.path(out,"event_study_results.csv"))

group_means<-rbindlist(lapply(metrics$metric,function(m) {
  d<-dat[event_time %between% c(-5L,5L)]
  if(m=="grade9") d<-d[positive_pre_grade9==TRUE]
  d[,.(mean_index=mean(get(paste0(m,"_index"))),enrollment_weighted_index=100*sum(get(paste0(m,"_students")))/sum(get(paste0("baseline_",m))),
    students=sum(get(paste0(m,"_students"))),n_schools=.N,zero_enrollment=sum(get(paste0(m,"_students"))==0)),by=.(outcome,label,high_va,event_time)][,metric:=m]
}))
fwrite(group_means,file.path(out,"event_time_group_means.csv"))
calendar<-dat[,.(mean_index=mean(secondary_index),students=sum(secondary_students),n_schools=.N),by=.(outcome,label,high_va,year)]
fwrite(calendar,file.path(out,"calendar_year_group_means.csv"))

# Use a fixed cohort in every event year; do not remove exits or zero enrollment.
cols<-c("#697C8B","#007E87")
for (m in metrics$metric) {
  png(file.path(fig,paste0("enrollment_trends_",m,"_by_va.png")),width=1900,height=1400,res=170)
  par(mfrow=c(2,2),mar=c(4.5,4.8,3.3,1),oma=c(7,0,2.8,0),las=1)
  for(o in specs$outcome) {
    d<-group_means[outcome==o & metric==m]
    plot(NA,xlim=c(-5,5),ylim=range(group_means[metric==m,mean_index]),xlab="Years from first SAE entry cohort",ylab="Enrollment index (pre-SAE mean = 100)",main=specs[outcome==o,label],xaxt="n")
    axis(1,at=-5:5);abline(v=0,lty=2,col="#B35B36");abline(h=100,col="#dce2e7")
    for(g in 0:1) { z<-d[high_va==g][order(event_time)]; lines(z$event_time,z$mean_index,col=cols[g+1],lwd=2.3);points(z$event_time,z$mean_index,pch=16,col=cols[g+1],cex=.65) }
    legend("topleft",c(sprintf("Top VA quartile (N=%s)",d[high_va==1,n_schools][1]),sprintf("Remaining schools (N=%s)",d[high_va==0,n_schools][1])),col=rev(cols),lwd=2,bty="n",cex=.85)
  }
  mtext(paste0(metrics[metric==m,metric_label],": enrollment before and after SAE"),outer=TRUE,side=3,line=.8,font=2,cex=1.15)
  mtext("Equal-school means; fixed cohort and VA groups. Each school's mean enrollment in years -3 to -1 = 100.\nSAE entry: 2017 Magallanes; 2018 four regions; 2019 remaining regions; 2020 Metropolitana.\nVA is retrospective observational EB VA from 2017-2020 grade-8 cohorts. These trajectories are descriptive.",outer=TRUE,side=1,line=3.2,cex=.8)
  dev.off()
}
png(file.path(fig,"enrollment_relative_gap_event_study.png"),width=1900,height=1400,res=170)
par(mfrow=c(2,2),mar=c(4.5,4.8,3,1),oma=c(7,0,2,0),las=1)
for(o in specs$outcome) {
  d<-ev[outcome==o & metric=="secondary"]
  plot(d$event_time,d$estimate,type="b",pch=16,col=cols[2],lwd=2,ylim=range(ev[metric=="secondary",c(lo,hi)]),
    xlab="Years from first SAE entry cohort",ylab="High-minus-other gap, relative to year -1",main=specs[outcome==o,label],xaxt="n")
  axis(1,at=-5:5);abline(h=0,col="#9da9b2",lty=2);abline(v=0,col="#B35B36",lty=2)
  z<-d[event_time!=-1L]
  arrows(z$event_time,z$lo,z$event_time,z$hi,angle=90,code=3,length=.04,col=cols[2])
}
mtext("Adjusted secondary-enrollment gap by VA",outer=TRUE,side=3,line=.5,font=2,cex=1.15)
mtext("School and region-by-calendar-year fixed effects; equal school weights; year -1 is the reference.\n95% confidence intervals cluster by 15 regions (t critical value, 14 df). Outcome: school enrollment index.\nPre-existing differences in trends and retrospective VA classification prevent a causal SAE interpretation.",outer=TRUE,side=1,line=3.2,cex=.8)
dev.off()

esc<-function(x) {x<-gsub("&","&amp;",as.character(x),fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
html_table<-function(x) paste0("<div class='table'><table><thead><tr>",paste0("<th>",esc(names(x)),"</th>",collapse=""),"</tr></thead><tbody>",paste(apply(as.data.frame(x),1,function(r)paste0("<tr>",paste0("<td>",esc(r),"</td>",collapse=""),"</tr>")),collapse=""),"</tbody></table></div>")
display<-function(m,s="main") {
  d<-res[metric==m & specification==s & contrast!="level_jump"]
  d[,value:=sprintf("%+.2f [%+.2f, %+.2f]",estimate,lo,hi)]
  z<-dcast(d,label+n_schools~contrast,value.var="value")
  z<-merge(z,specs[,.(label,ordering)],by="label")[order(ordering)][,!"ordering"]
  setcolorder(z,c("label","n_schools","pre","post","slope_change"))
  setnames(z,c("VA measure","Schools","Pre-SAE slope gap","Post-SAE slope gap","Change in slope gap"))
  z
}
nat<-fread(file.path(out,"enrollment_source_audit.csv"))[,.(year,secondary_students,grade9_students,secondary_schools)]
html<-paste0("<!doctype html><html><head><meta charset='utf-8'><title>Enrollment trends before and after SAE by VA</title><style>body{font:16px/1.55 system-ui;background:#f3f6f8;color:#183244;margin:0}main{max-width:1200px;margin:auto;background:white;padding:36px}h1{font-size:32px}h2{margin-top:32px}img{width:100%}table{border-collapse:collapse;width:100%;font-size:14px}td,th{padding:9px;border-bottom:1px solid #d9e3e9;text-align:right}td:first-child,th:first-child{text-align:left}th{background:#edf3f6}.table{overflow-x:auto}.note{background:#edf4f7;padding:16px}a{color:#006d77}</style></head><body><main>",
  "<h1>Did enrollment trends change at SAE for high-VA schools?</h1><p>Annual observed secondary enrollment, 2010-2025. The analysis aligns schools to their region's first SAE entry cohort and compares the top quarter of schools by fixed VA with the remaining schools.</p>",
  "<p class='note'><b>For math and exam-taking VA, the growth advantage predates SAE and weakens afterward.</b> The adjusted high-minus-other growth gap falls from +1.85 to +0.28 index points per year for math, and from +3.11 to -0.52 for exam taking. The slope changes are -1.57 (95% CI -2.40 to -0.74) and -3.63 (-4.69 to -2.57), respectively. Both groups' enrollment grows after rollout in the raw normalized series; it is their relative growth advantage that shrinks. These findings survive the pandemic exclusion, enrollment weighting, continuous-VA measure, and five-pre-year incumbent restriction. Field VA gives weaker, specification-sensitive evidence; institution VA shows no clear trend break.</p>",
  "<p class='note'>These are actual students, not capacity or advertised vacancies. The primary outcome is each school's secondary enrollment divided by its own mean in the three years before SAE, times 100. High VA uses the existing 2017-2020 cohort empirical-Bayes observational estimates, measured retrospectively. This is a descriptive trend-break check, not a causal estimate of SAE.</p>",
  "<h2>Secondary enrollment</h2><img src='../../figures/secondary_sae_spots/enrollment_trends_secondary_by_va.png'>",
  "<h2>Does the relative trend change?</h2><p>Estimates below are the high-VA-minus-other difference in annual enrollment-index growth, in percentage points of each school's pre-SAE enrollment per year. The pre and post slopes come from a segmented linear model with a separate rollout-year level shift. Brackets show 95% confidence intervals, clustered by region (15 clusters; t with 14 degrees of freedom).</p>",
  html_table(display("secondary")),
  "<p>Model: enrollment index = school fixed effect + region-by-calendar-year fixed effect + HighVA x event time + HighVA x Post + HighVA x max(event time,0). Event window -5 through +5. A positive slope change means the high-VA group's relative growth becomes more positive. It does not necessarily mean that group's enrollment rises in absolute terms. The post slope is the sum of the pre slope and slope change; its SE uses the full regression covariance matrix.</p>",
  "<img src='../../figures/secondary_sae_spots/enrollment_relative_gap_event_study.png'>",
  "<h2>First-year secondary enrollment</h2><p>Grade 9 is the immediate admissions margin. The VA group assignment is unchanged; schools must also have positive grade-9 enrollment in each of the three baseline years.</p>",
  "<img src='../../figures/secondary_sae_spots/enrollment_trends_grade9_by_va.png'>",html_table(display("grade9")),
  "<h2>Sensitivity: omit calendar years 2020 and 2021</h2>",html_table(display("secondary","exclude_2020_2021")),
  "<h2>Sensitivity: weight schools by baseline enrollment</h2><p>This gives more influence to larger schools rather than treating a small school's percentage change equally with a large school's.</p>",html_table(display("secondary","baseline_enrollment_weighted")),
  "<h2>Sensitivity: at least 100 baseline secondary students</h2>",html_table(display("secondary","baseline_100plus")),
  "<h2>Sensitivity: positive enrollment in all five pre-SAE years</h2><p>Removes schools opening within the pre-trend window; VA group assignments and the three-year normalization remain fixed.</p>",html_table(display("secondary","established_five_pre_years")),
  "<h2>Sensitivity: continuous VA</h2><p>Here the slopes compare schools one school-level VA standard deviation apart rather than top-quartile versus the rest.</p>",html_table(display("secondary","continuous_VA")),
  "<h2>Sample and timing</h2>",html_table(sample_audit[,.(label,ever_SAE_schools,pre_incumbents,matched_schools,high_VA_schools,grade9_eligible)]),
  "<p>The universe is RBDs with positive declared secondary capacity in any SAE process from 2016 to 2025. Incumbents must have positive regular youth secondary enrollment in each of the three years preceding their region's first SAE entry year. The cohort and VA groups stay fixed. Missing later regular-secondary records enter as zero reported enrollment; this is not a legal-closure classification. Schools that opened after the baseline are excluded. This eventual-SAE population does not represent every historical school or every pre-SAE closure.</p>",
  "<p>Entry dates are 2017 Magallanes; 2018 Tarapaca, Coquimbo, O'Higgins and Los Lagos; 2019 the remaining non-Metropolitana regions; 2020 Metropolitana. Nuble is combined with historical Biobio. These dates refer to entry in grade 9; full coverage of other grades followed. Process/application year is one year earlier.</p>",
  "<p>High VA is at or above the 75th percentile among matched incumbents, separately by outcome, with school weights. VA is adjusted observational EB VA, not school-specific lottery IV estimates. The cohorts and later outcomes used to measure VA overlap SAE, so this grouping cannot establish what administrators knew before SAE or identify a policy effect. Regional demographic/calendar shocks are absorbed by region-year effects, but shocks that differ by VA may remain. Fifteen regional clusters provide limited precision. Four-VA Benjamini-Hochberg adjusted p-values are supplied in the result CSV.</p>",
  "<h2>National enrollment context, including schools outside the fixed SAE sample</h2>",html_table(nat),
  "<h2>Sources and reproducibility</h2><p><a href='https://datosabiertos.mineduc.cl/resumen-de-matricula-por-curso/'>MINEDUC classroom enrollment archives</a>: April 30 SIGE enrollment snapshots, 2010-2025. Sum N_ALU across unique classroom records in functioning schools and regular youth-secondary teaching codes 310, 410, 510, 610, 710, 810 and 910. Adult/special education is excluded. Combined classrooms use their grade-specific enrollment counts. Before 2015 the release covers functioning schools; from 2015 require ESTADO_ESTAB=1.</p>",
  "<p><a href='../../../data/clean/secondary_sae_spots/enrollment_trends/segmented_trend_results.csv'>All regression results and sensitivities</a> | <a href='../../../data/clean/secondary_sae_spots/enrollment_trends/event_time_group_means.csv'>Plot data</a> | <a href='../../../data/clean/secondary_sae_spots/enrollment_trends/enrollment_va_panel.csv'>School-year analysis panel</a> | <a href='../../../data/clean/secondary_sae_spots/enrollment_trends/enrollment_source_audit.csv'>Source audit</a> | <a href='../../../code/codex/secondary_sae_spots/README.md'>Code and definitions</a></p>",
  "<p>Generated ",Sys.Date(),". Additional saved sensitivity restricts to schools with positive 2021 SAE capacity, closer to the earlier capacity-growth exercise. All source hashes, regional timing, match counts, event-study estimates and fitted regression objects are saved.</p></main></body></html>")
writeLines(html,file.path(report,"enrollment_trends_before_after_sae_va.html"),useBytes=TRUE)
print(sample_audit)
print(res[metric=="secondary" & specification=="main" & contrast!="level_jump",.(label,contrast,estimate,se,lo,hi,p,p_bh_four_VA,n_schools)])
