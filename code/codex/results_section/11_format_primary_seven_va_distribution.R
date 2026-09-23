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

# The unshrunk estimates are the school fixed effects supplied to the EB step.
eb_dir <- file.path(repo_wd,'output/tables/empirical_bayes_school_va')
stata_keys <- c('math','language','exam','highinst','highpay','program_income_full')
unshrunk_sd <- setNames(numeric(length(order_keys)),order_keys)
for(key in stata_keys) {
  z <- fread(file.path(eb_dir,paste0('stata_eb_school_values_',key,'.csv')),
             select=c('va_centered','n_students'))
  unshrunk_sd[key] <- weighted_sd(z$va_centered,z$n_students)
}
data_wd <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD',unset='C:/Users/brunem/Box/causal_schools')
aid_path <- file.path(data_wd,'data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv')
aid <- fread(aid_path,select=c('outcome','controlled_value_added_centered_student','n_students_regression'))
aid <- aid[outcome=='any_postulacion']
unshrunk_sd['postulacion'] <- weighted_sd(aid$controlled_value_added_centered_student,aid$n_students_regression)
x[,unshrunk_va_sd:=unshrunk_sd[outcome_key]]
fwrite(x,input_path)

rows <- unlist(lapply(seq_len(nrow(x)),function(i){
  line <- paste0(labels[i],' & ',fmt(x$sample_mean[i]),' & ',fmt(x$sample_sd[i]),' & ',fmt_n(x$n_students[i]),
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
  'Outcome & Mean & SD & $N$ & P10 & P50 & P90 & SD & SD & SD \\\\',
  '\\midrule',rows,'\\bottomrule','\\end{tabular}',
  '\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item Notes: The student-outcome columns report moments for the outcome-specific samples. The EB block reports the student-weighted distribution of EB-shrunken, student-centered school value added; schools are weighted by the number of students in the corresponding value-added regression. Unshrunk VA is the student-weighted dispersion of the school fixed-effect estimates supplied to the EB shrinkage step. The RSS column reports the standard deviation of latent school effects estimated from cross-cohort products. RSS uses its cross-cohort estimation sample and school weighting, which differ from the EB calculation; the relative magnitudes of EB and RSS therefore need not follow a fixed ordering. Financial-aid application uses the R-based EB pipeline employed by the main IV analysis; the other rows use the corresponding Stata EB outputs. Math and verbal are measured in admission-test standard deviations, admission-exam taking, high-premium institution, high-premium field, and financial-aid application are binary, and projected income is measured in logs.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(tex,output_path)
message('Wrote: ',output_path)
