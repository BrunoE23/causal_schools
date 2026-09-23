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
rows <- unlist(lapply(seq_len(nrow(x)),function(i){
  line <- paste0(labels[i],' & ',fmt(x$sample_mean[i]),' & ',fmt(x$sample_sd[i]),' & ',fmt_n(x$n_students[i]),
    ' & ',fmt(x$eb_va_sd[i]),' & ',fmt(x$p10[i]),' & ',fmt(x$p50[i]),' & ',fmt(x$p90[i]),
    ' & ',fmt(x$debiased_sd[i]),' [',fmt(x$debiased_sd_se[i]),'] \\\\')
  if(i %in% c(2L,3L)) c(line,'\\addlinespace[2pt]') else line
}))
tex <- c(
  '\\begin{table}[!htbp]','\\centering','\\begin{threeparttable}',
  '\\caption{Distribution of student outcomes and school value added}',
  '\\label{tab:school-va-distribution}',
  '\\footnotesize','\\setlength{\\tabcolsep}{3pt}',
  '\\begin{tabular}{lccc cccc c}','\\toprule',
  ' & \\multicolumn{3}{c}{Student outcome} & \\multicolumn{4}{c}{EB posterior school VA} & \\multicolumn{1}{c}{RSS latent VA} \\\\',
  '\\cmidrule(lr){2-4} \\cmidrule(lr){5-8} \\cmidrule(lr){9-9}',
  'Outcome & Mean & SD & $N$ & SD & P10 & P50 & P90 & SD [bootstrap SE] \\\\',
  '\\midrule',rows,'\\bottomrule','\\end{tabular}',
  '\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item Notes: The student-outcome columns report moments for the outcome-specific samples. The EB block reports the student-weighted distribution of EB-shrunken, student-centered school value added; schools are weighted by the number of students in the corresponding value-added regression. The RSS column reports the standard deviation of latent school effects estimated from cross-cohort products, with a school-bootstrap standard error in brackets. The EB SD is the dispersion of posterior school estimates after shrinkage, whereas the RSS SD estimates latent across-school dispersion and therefore need not equal the EB SD. Financial-aid application uses the R-based EB pipeline employed by the main IV analysis; the other rows use the corresponding Stata EB outputs. Math and verbal are measured in admission-test standard deviations, admission-exam taking, high-premium institution, high-premium field, and financial-aid application are binary, and projected income is measured in logs.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(tex,output_path)
message('Wrote: ',output_path)
