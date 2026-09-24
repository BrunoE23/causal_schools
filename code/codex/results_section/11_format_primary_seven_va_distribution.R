# Format the seven-outcome value-added distribution table for the paper.
# The input is produced by the seven-outcome distribution construction and
# already contains the EB posterior distribution and RSS latent-effect SD.
suppressPackageStartupMessages(library(data.table))
repo_wd <- Sys.getenv('CAUSAL_SCHOOLS_REPO_WD',unset=getwd())
out_dir <- file.path(repo_wd,'output/tables/results_section')
input_path <- file.path(out_dir,'primary_seven_va_distribution.csv')
output_path <- file.path(out_dir,'primary_seven_va_distribution.tex')
x <- fread(input_path)
order_keys <- c('math','language','exam','highinst','highpay','program_income_full','postulacion')
x <- x[match(order_keys,outcome_key)]
labels <- c('Math achievement','Verbal achievement','Admission exam taken','High-premium institution','High-premium field','Log projected income','Financial aid application')
fmt <- function(z) sprintf('%.3f',z)
fmt_n <- function(z) format(z,big.mark=',',trim=TRUE,scientific=FALSE)
weighted_sd <- function(value, weight) {
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  value <- value[keep]
  weight <- weight[keep]
  mu <- sum(weight * value) / sum(weight)
  sqrt(sum(weight * (value - mu)^2) / sum(weight))
}
weighted_quantile <- function(value, weight, probs) {
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  value <- value[keep]
  weight <- weight[keep]
  ord <- order(value)
  value <- value[ord]
  cumulative_weight <- cumsum(weight[ord]) / sum(weight)
  vapply(probs, function(p) value[which(cumulative_weight >= p)[1L]], numeric(1))
}

# Use the unified pooled VA/EB file for all seven outcomes so the VA and RSS
# calculations share the same outcome-specific sample rule and student weights.
data_wd <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD',unset='C:/Users/brunem/Box/causal_schools')
eb_path <- file.path(data_wd,'data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv')
eb <- fread(eb_path,select=c(
  'outcome','analysis_sample','controlled_value_added_centered_student',
  'controlled_value_added_eb_centered_student','n_students_regression'
))[analysis_sample=='All']
outcome_map <- c(
  math='z_year_math_max', language='z_year_leng_max', exam='admission_exam_taker',
  highinst='high_inst_m1', highpay='high_paying_field_m1',
  program_income_full='log_program_income_clp_m1', postulacion='any_postulacion'
)
for (i in seq_len(nrow(x))) {
  z <- eb[outcome==outcome_map[[x$outcome_key[i]]]]
  w <- z$n_students_regression
  q <- weighted_quantile(z$controlled_value_added_eb_centered_student,w,c(.10,.25,.50,.75,.90))
  x[i,`:=`(
    eb_va_sd=weighted_sd(z$controlled_value_added_eb_centered_student,w),
    p10=q[1],p25=q[2],p50=q[3],p75=q[4],p90=q[5],
    n_schools=nrow(z),eb_source='r_lfe_unified',
    unshrunk_va_sd=weighted_sd(z$controlled_value_added_centered_student,w)
  )]
}
fwrite(x,input_path)

rows <- unlist(lapply(seq_len(nrow(x)),function(i){
  line <- paste0(labels[i],' & ',fmt_n(x$n_students[i]),' & ',fmt(x$sample_mean[i]),' & ',fmt(x$sample_sd[i]),
    ' & ',fmt(x$p10[i]),' & ',fmt(x$p50[i]),' & ',fmt(x$p90[i]),' & ',fmt(x$eb_va_sd[i]),
    ' & ',fmt(x$unshrunk_va_sd[i]),' & ',fmt(x$debiased_sd[i]),' \\\\')
  if(i %in% c(2L,3L)) c(line,'\\addlinespace[2pt]') else line
}))
tex <- c(
  '\\begin{table}[!htbp]','\\centering','\\begin{threeparttable}',
  '\\caption{Distribution of student outcomes and school value added}',
  '\\label{tab:school-va-distribution}',
  '\\footnotesize','\\setlength{\\tabcolsep}{1.5pt}',
  '\\begin{tabular}{lccc cccc cc}','\\toprule',
  ' & \\multicolumn{3}{c}{Student outcome} & \\multicolumn{4}{c}{EB posterior school VA} & \\multicolumn{1}{c}{Unshrunk VA} & \\multicolumn{1}{c}{RSS latent VA} \\\\',
  '\\cmidrule(lr){2-4} \\cmidrule(lr){5-8} \\cmidrule(lr){9-9} \\cmidrule(lr){10-10}',
  'Outcome & $N$ & Mean & SD & P10 & P50 & P90 & SD & SD & SD \\\\',
  '\\midrule',rows,'\\bottomrule','\\end{tabular}',
  '\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item Notes: The student-outcome columns report moments for the outcome-specific samples. The EB block reports the student-weighted distribution of EB-shrunken, student-centered school value added; schools are weighted by the number of students in the corresponding pooled value-added regression. Unshrunk VA is the student-weighted dispersion of the school fixed-effect estimates supplied to the EB shrinkage step. The RSS column reports the standard deviation of latent school effects estimated from cross-cohort products and uses the same outcome-specific sample rule and student-count weights. All VA columns use the unified R-based pooled VA pipeline. Math and verbal are measured in admission-test standard deviations, admission-exam taking, high-premium institution, high-premium field, and financial-aid application are binary, and projected income is measured in logs.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(tex,output_path)
message('Wrote: ',output_path)
