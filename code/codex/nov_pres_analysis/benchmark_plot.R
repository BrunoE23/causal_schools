# Descriptive cross-study magnitude benchmarks; not comparable policy effects.
library(data.table)
d <- fread("output/tables/results_section/primary_seven_va_distribution.csv")
r <- fread("output/tables/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/main_results.csv")
math <- d[outcome_key=="math",p90-p10]*r[spec=="math_adj_eb",beta]
field <- 100*d[outcome_key=="highpay",p90-p10]*r[spec=="highpay_adj_eb",beta]
b <- data.table(
  panel=rep(c("Math achievement","STEM-related choices"),each=3),
  label=c("This paper: P90-P10 school VA","Personalized instruction: Mindspark",
    "High-dosage tutoring: Saga (pooled)","This paper: P90-P10 school VA",
    "Female scientist classroom visits","Information + female-imagery brochure"),
  detail=c("Chile | implied gain across high schools",
    "India, grades 6-9 | 4.5 months | ITT",
    "Chicago, grades 9-10 | one school year | TOT",
    "Chile | HIGH-PREMIUM FIELD, not STEM",
    "France, grade-12 girls | male-dominated STEM | ITT",
    "US college women | technology major, year 2 | exploratory ITT"),
  value=c(math,.37,.282,field,3.4,2.2),
  unit=rep(c("SD","pp"),each=3),
  source=c("September paper EB VA gap x pass-through","Muralidharan, Singh & Ganimian (2019), Table 2",
    "Guryan et al. (2023), Table 5","September paper EB VA gap x pass-through",
    "Breda et al. (2023), Section 4.3","Brodnax (2025), Table 6 and discussion"),
  url=c("", "https://alejandroganimian.com/files/20190216_Disrupting_education.pdf",
    "https://educationlab.uchicago.edu/wp-content/uploads/sites/3/2023/10/UChicago-Education-Lab-Not-Too-Late-Paper_03.23.pdf", "",
    "https://www.parisschoolofeconomics.com/grenet-julien/wp/Breda_Grenet_Monnet_Van_Effenterre_2023.pdf",
    "https://journals.sagepub.com/doi/10.3102/01623737241228278"))
b <- rbind(b, data.table(
  panel=c("Math achievement","STEM-related choices"),
  label=c("Fryer (2014): school-practices bundle","Canaan & Mouganie: female science advisor"),
  detail=c("Houston elementary | 2012/13 pooled | ITT",
    "Lebanon, college women | STEM enrollment | random assignment"),
  value=c(.135,5.4),unit=c("SD","pp"),
  source=c("Fryer (2014), Table III, column 3",
    "Canaan & Mouganie (2023; December 2020 manuscript), Table 5, column 2"),
  url=c("https://fryer.scholars.harvard.edu/sites/g/files/omnuum5986/files/fryer/files/2014_injecting_charter_school_best_practices_into_traditional_public_schools.pdf",
    "https://swlb1.aeaweb.org/conference/2021/preliminary/paper/BkAyA2Tn")))
b[label=="Personalized instruction: Mindspark",label:="Muralidharan et al. (2019): Mindspark"]
b[label=="High-dosage tutoring: Saga (pooled)",label:="Guryan et al. (2023): Saga tutoring"]
b[label=="Female scientist classroom visits",label:="Breda et al. (2023): scientist visits"]
b[label=="Information + female-imagery brochure",label:="Brodnax (2025): information brochure"]
setorder(b,panel,-value)
dir.create("data/clean/nov_pres_analysis",recursive=TRUE,showWarnings=FALSE)
fwrite(b,"data/clean/nov_pres_analysis/intervention_benchmarks.csv")
png("output/figures/nov_pres_analysis/intervention_benchmarks.png",width=2800,height=1750,res=180)
par(oma=c(6.1,1,5.5,1),mar=c(3.8,2,2.2,2),mfrow=c(1,2),family="sans",fg="#23383C")
for(j in 1:2) {
  rows <- b[((j-1)*4+1):(j*4)]
  xmax <- c(.7,12.5)[j]
  plot(NA,xlim=c(0,xmax),ylim=c(.5,4.65),axes=FALSE,xlab="",ylab="",xaxs="i")
  ticks <- if(j==1) seq(0,.6,.2) else seq(0,12,3)
  abline(v=ticks,col="#E8EBEB",lwd=1)
  axis(1,at=ticks,labels=ticks,col="#BCC5C5",col.axis="#23383C",cex.axis=1.1)
  mtext(if(j==1) "Math achievement (standard deviations)" else "Enrollment / major choice (percentage points)",side=1,line=2.5,cex=1.05)
  title(main=c("A. Math achievement","B. STEM-related choices")[j],adj=0,cex.main=1.35)
  for(i in 1:4) {
    y <- 5-i
    col <- if(i==1) "#21666B" else "#82979B"
    text(0,y+.39,rows$label[i],adj=c(0,.5),font=2,cex=1.05)
    text(0,y+.17,rows$detail[i],adj=c(0,.5),cex=.79,col="#526468")
    rect(0,y-.12,rows$value[i],y+.015,col=col,border=NA)
    text(rows$value[i]+xmax*.025,y-.05,
      if(j==1) sprintf("%.3f SD",rows$value[i]) else sprintf("%.1f pp",rows$value[i]),
      adj=c(0,.5),font=2,cex=1.13,col=col)
  }
}
mtext("School-effect magnitudes in context",outer=TRUE,side=3,line=3.0,cex=1.8,font=2,adj=.02)
mtext("P90-P10 implied gains compared with three distinct papers per outcome",outer=TRUE,side=3,line=1.3,cex=1.05,adj=.02)
mtext("Scale benchmarks only: populations, outcomes, exposure lengths and estimands differ. Point estimates; no confidence intervals shown.",outer=TRUE,side=1,line=1.4,adj=.02,cex=.86)
mtext("High-premium fields include law and medicine. STEM/technology benchmarks are narrower; technology major gains need not raise total STEM enrollment.",outer=TRUE,side=1,line=2.7,adj=.02,cex=.82)
mtext("Canaan & Mouganie: 2023 publication, estimate verified in Dec. 2020 manuscript. Brodnax: exploratory female-imagery treatment arm.",outer=TRUE,side=1,line=4.0,adj=.02,cex=.82)
dev.off()
