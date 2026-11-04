# September-sample EB heterogeneity and paper cross-outcome slide exports.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setFixest_nthreads(2)
tag <- "va_2017_2020__sae_2018_2020"
input <- file.path("output/intermediate/empirical_bayes_school_va", tag, "scalar_iv_regression_df.csv")
out <- "data/clean/nov_pres_analysis"
fig <- "output/figures/nov_pres_analysis"
tab <- "output/tables/nov_pres_analysis"
for (p in c(out, fig, tab)) dir.create(p, recursive=TRUE, showWarnings=FALSE)
specs <- data.table(outcome_key=c("math", "leng", "highinst", "highpay"),
  label=c("Math", "Verbal", "High-premium institution", "High-premium field"),
  y=c("z_year_math_max", "z_year_leng_max", "high_inst_m1", "high_paying_field_m1"))
specs[, spec := paste0(outcome_key, "_adj_eb")]
base <- c("cohort_gr8", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to")
cols <- c(base, "admission_exam_taker", specs$y,
  paste0("d_", specs$spec), paste0("z_", specs$spec), paste0("expected_", specs$spec))
dt <- fread(input, select=cols, na.strings=c("", "NA"))
# Same rank-based quintile definition as the legacy heterogeneity scripts:
# define once in the joint regression dataframe, before outcome restrictions.
dt[, quintile := NA_integer_]
dt[!is.na(z_sim_mat_4to), quintile := pmin(5L,
  as.integer(ceiling(5 * frank(z_sim_mat_4to, ties.method="average") / .N)))]
run <- function(dat, s, split="All", group="All") {
  d <- paste0("d_", s$spec); z <- paste0("z_", s$spec)
  e <- paste0("expected_", s$spec)
  required <- c(base, s$y, d, z, e, "admission_exam_taker")
  reg <- dat[complete.cases(dat[, ..required]) & admission_exam_taker == 1L]
  controls <- c("factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to",
    "factor(EDAD_ALU)", e)
  if (uniqueN(reg$GEN_ALU) > 1L) controls <- c(controls, "factor(GEN_ALU)")
  rhs <- paste(controls, collapse=" + ")
  iv <- feols(as.formula(paste(s$y, "~", rhs, "| 0 |", d, "~", z)),
    reg, vcov="hetero", notes=FALSE)
  fs <- feols(as.formula(paste(d, "~", rhs, "+", z)), reg, vcov="hetero", notes=FALSE)
  b <- unname(coef(iv)[paste0("fit_", d)]); se_b <- unname(se(iv)[paste0("fit_", d)])
  data.table(spec=s$spec, label=s$label, split=split, group=group,
    beta=b, se=se_b, p_value=2*pnorm(-abs(b/se_b)), n_obs=nobs(iv),
    fs_beta=unname(coef(fs)[z]), fs_se=unname(se(fs)[z]),
    fs_f=unname((coef(fs)[z]/se(fs)[z])^2))
}
all <- rbindlist(lapply(1:4, function(i) run(dt, specs[i])))
ref <- fread(file.path("output/tables/empirical_bayes_school_va", tag, "main_results.csv"))
audit <- merge(all, ref[, .(spec, ref_beta=beta, ref_se=se, ref_n=n_obs, ref_f=fs_f)], by="spec")
stopifnot(all(abs(audit$beta-audit$ref_beta)<1e-8), all(abs(audit$se-audit$ref_se)<1e-8),
  all(audit$n_obs==audit$ref_n), all(abs(audit$fs_f-audit$ref_f)<1e-6))
fwrite(audit, file.path(out, "main_replication.csv"))
message("All four main estimates, SEs, Ns and first-stage F statistics reproduced.")
results <- rbindlist(lapply(1:4, function(i) rbindlist(c(
  lapply(1:5, function(q) run(dt[quintile==q], specs[i], "Quintile", paste0("Q",q))),
  lapply(1:2, function(g) run(dt[GEN_ALU==g], specs[i], "Gender", c("Boys","Girls")[g]))))))
counts <- merge(results[, .(subgroup_n=sum(n_obs)), by=.(spec, split)],
  all[, .(spec, main_n=n_obs)], by="spec")
stopifnot(all(counts$subgroup_n==counts$main_n), all(is.finite(results$se)),
  all(results$se>0), all(results$fs_f>0))
fwrite(counts,file.path(out,"subgroup_sample_audit.csv"))
fwrite(results, file.path(out, "heterogeneity.csv"))
# Disjoint subgroup estimates, same HC1 inference as the main specifications.
contrasts <- rbindlist(lapply(1:4, function(i) {
  r <- results[spec==specs$spec[i]]
  pairs <- list(c("Q1","Q3"),c("Q2","Q3"),c("Q4","Q3"),c("Q5","Q3"),c("Girls","Boys"))
  rbindlist(lapply(pairs, function(pair) {
    a <- r[group==pair[1]]; b <- r[group==pair[2]]
    difference <- a$beta-b$beta; se_diff <- sqrt(a$se^2+b$se^2)
    data.table(spec=specs$spec[i], contrast=paste(pair,collapse=" - "),
      difference=difference, se=se_diff, p_value=2*pnorm(-abs(difference/se_diff)))
  }))
}))
fwrite(contrasts, file.path(out,"contrasts.csv"))
for (split_key in c("Quintile", "Gender")) {
  png(file.path(fig,paste0(tolower(split_key),".png")), width=1800,height=1000,res=160)
  par(mfrow=c(2,2),mar=c(3.3,4.3,2.0,1.0),mgp=c(2.4,.7,0),cex=.95)
  for(i in 1:4) {
    r <- results[spec==specs$spec[i] & split==split_key]
    x <- seq_len(nrow(r)); lo <- r$beta-1.96*r$se; hi <- r$beta+1.96*r$se
    plot(x,r$beta,ylim=range(c(lo,hi,0,1)),xlim=c(.7,length(x)+.3),
      xaxt="n",xlab="",ylab="EB pass-through",main=specs$label[i],
      pch=19,col="#21666B",bty="l")
    abline(h=c(0,1),lty=c(1,2),col=c("grey80","grey65"))
    arrows(x,lo,x,hi,angle=90,code=3,length=.035,col="#21666B",lwd=1.6)
    points(x,r$beta,pch=19,col="#21666B",cex=1.1)
    axis(1,at=x,labels=r$group)
  }
  dev.off()
}
# Use the exact direct-IV entries in the September paper (UNSHRUNK VA).
cross <- fread("output/tables/rss_debiased_varcov/unshrunk_cross_outcome_iv_rss_validation.csv")
keys <- c("math","verbal","highinst","highpay")
print(unique(cross[, .(from_key, from_label)]))
stopifnot(all(keys %in% cross$from_key), all(keys %in% cross$to_key))
cross <- cross[from_key %in% keys & to_key %in% keys]
stopifnot(nrow(cross)==16L)
fwrite(cross, file.path(out,"cross_outcome_paper.csv"))
stars <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.1,"*","")))
lines <- c("\\begin{tabular}{lcccc}","\\toprule",
  paste0("& \\multicolumn{4}{c}{\\textbf{Student outcome (dependent variable)}} ",strrep("\\",2)),
  "\\cmidrule(lr){2-5}",
  paste0("\\shortstack[l]{School VA measure\\\\(instrumented treatment)} & Math & Verbal & \\shortstack{High-premium\\\\institution} & \\shortstack{High-premium\\\\field} ", strrep("\\",2)),"\\midrule")
for(i in 1:4) {
  r <- cross[from_key==keys[i]][match(keys,to_key)]
  lines <- c(lines,paste0(specs$label[i]," & ",paste(sprintf("%.3f%s",r$direct_iv,stars(r$direct_p)),collapse=" & ")," \\\\"),
    paste0(" & ",paste(sprintf("(%.3f)",r$direct_se),collapse=" & ")," \\\\"))
}
writeLines(c(lines,"\\bottomrule","\\end{tabular}"),file.path(tab,"cross_outcome.tex"))
small_keys <- c("math", "highpay")
small_lines <- c("\\begin{tabular}{lcc}", "\\toprule",
  paste0("& \\multicolumn{2}{c}{\\textbf{Student outcome (dependent variable)}} ",strrep("\\",2)),
  "\\cmidrule(lr){2-3}",
  paste0("\\shortstack[l]{School VA measure\\\\(instrumented treatment)} & Math score & High-premium field ",strrep("\\",2)),"\\midrule")
for (k in small_keys) {
  r <- cross[from_key==k][match(small_keys,to_key)]
  label <- if (k=="math") "Math VA" else "High-premium-field VA"
  small_lines <- c(small_lines,
    paste0(label," & ",paste(sprintf("%.3f%s",r$direct_iv,stars(r$direct_p)),collapse=" & ")," ",strrep("\\",2)),
    paste0(" & ",paste(sprintf("(%.3f)",r$direct_se),collapse=" & ")," ",strrep("\\",2)))
}
writeLines(c(small_lines,"\\bottomrule","\\end{tabular}"),file.path(tab,"cross_outcome_2x2.tex"))
writeLines(capture.output(sessionInfo()),file.path(out,"session_info.txt"))
print(results)
print(contrasts)
