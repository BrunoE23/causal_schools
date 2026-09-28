# Format the primary-outcome value-added distribution table for the paper.
# The input is produced by the primary-outcome distribution construction and
# already contains the EB posterior distribution and RSS latent-effect SD.
suppressPackageStartupMessages(library(data.table))
repo_wd <- Sys.getenv('CAUSAL_SCHOOLS_REPO_WD',unset=getwd())
out_dir <- file.path(repo_wd,'output/tables/results_section')
input_path <- file.path(out_dir,'primary_seven_va_distribution.csv')
output_path <- file.path(out_dir,'primary_seven_va_distribution.tex')
x <- fread(input_path)
order_keys <- c('math','language','exam','postulacion','enrolled','highinst','highpay')
x <- x[match(order_keys,outcome_key)]
labels <- c('Math achievement','Verbal achievement','Admission exam taken','Financial aid application','Higher-ed enrollment','High-premium institution','High-premium field')
fmt <- function(z) sprintf('%.3f',z)
fmt_mean <- function(z, binary=FALSE) if(binary) sprintf('%.1f',100*z) else sprintf('%.3f',z)
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

# Use the unified pooled VA/EB file for all primary outcomes so the VA and RSS
# calculations share the same outcome-specific sample rule and student weights.
data_wd <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD',unset='C:/Users/brunem/Box/causal_schools')
eb_path <- file.path(data_wd,'data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv')
eb <- fread(eb_path,select=c(
  'outcome','analysis_sample','controlled_value_added_centered_student',
  'controlled_value_added_eb_centered_student','n_students_regression'
))[analysis_sample=='All']
outcome_map <- c(
  math='z_year_math_max', language='z_year_leng_max', exam='admission_exam_taker',
  enrolled='higher_ed_enrolled_m1',
  highinst='high_inst_m1', highpay='high_paying_field_m1',
  postulacion='any_postulacion'
)

# Refresh the RSS diagonal from the current seven-outcome estimator output.
# This also fills higher-ed enrollment, which was added after the original
# distribution CSV was constructed.
rss_path <- file.path(
  data_wd, 'data/clean/rss_debiased_varcov/four_year_no_sae/rss_varcov_matrix.csv'
)
rss <- fread(rss_path)[type == 'variance' & outcome_1 == outcome_2]
rss[, `:=`(
  debiased_sd = sqrt(pmax(estimate, 0)),
  debiased_sd_se = bootstrap_se / (2 * sqrt(pmax(estimate, 0)))
)]
for (i in seq_len(nrow(x))) {
  z <- rss[outcome_1 == outcome_map[[x$outcome_key[i]]]]
  if (nrow(z) == 1L) {
    x[i, `:=`(
      debiased_sd = z$debiased_sd,
      debiased_sd_se = z$debiased_sd_se
    )]
  }
}
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

binary <- x$outcome_key %in% c('exam','enrolled','highinst','highpay','postulacion')
units <- ifelse(binary,'pp','SD')
rows <- unlist(lapply(seq_len(nrow(x)),function(i){
  scale <- if(binary[i]) 100 else 1
  line <- paste0(labels[i],' & ',units[i],' & ',fmt_n(x$n_students[i]),' & ',
    fmt_mean(x$sample_mean[i],binary[i]),' & ',fmt_mean(x$sample_sd[i],binary[i]),
    ' & ',fmt(scale*x$eb_va_sd[i]),' & ',fmt(scale*x$unshrunk_va_sd[i]),
    ' & ',fmt(scale*x$debiased_sd[i]),' & ',fmt(scale*(x$p90[i]-x$p10[i])),' \\\\')
  if(i %in% c(2L,5L)) c(line,'\\addlinespace[2pt]') else line
}))
tex <- c(
  '\\begin{table}[!htbp]','\\centering','\\begin{threeparttable}',
  '\\caption{Scale of student outcomes and dispersion of school value added}',
  '\\label{tab:school-va-distribution}',
  '\\scriptsize','\\setlength{\\tabcolsep}{2pt}',
  '\\begin{tabular}{llrcc cccc}','\\toprule',
  ' & & & \\multicolumn{2}{c}{Student outcome} & \\multicolumn{4}{c}{School VA dispersion} \\\\',
  '\\cmidrule(lr){4-5} \\cmidrule(lr){6-9}',
  'Outcome & Units & $N$ & Mean & SD & EB SD & Unshrunk SD & RSS SD & EB 90--10 \\\\',
  '\\midrule',rows,'\\bottomrule','\\end{tabular}',
  '\\begin{tablenotes}[flushleft]','\\footnotesize',
  '\\item \\textit{Notes:} Outcomes use the units in column 2. Math, verbal, high-premium-institution, and field measures are estimated among exam takers; non-enrollees are coded zero for the latter two. EB columns report student-weighted dispersion in shrunken, centered school VA; unshrunk VA is its fixed-effect input. RSS reports latent dispersion from cross-cohort products using the same samples and weights.',
  '\\end{tablenotes}','\\end{threeparttable}','\\end{table}')
writeLines(tex,output_path)
message('Wrote: ',output_path)
