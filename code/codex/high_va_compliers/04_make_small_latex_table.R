# Small paper-facing table: projected-income VA complier composition.
suppressPackageStartupMessages(library(data.table))
input <- 'data/clean/high_va_compliers/all_cutoffs_minus_sae_population.csv'
output <- 'output/tables/high_va_compliers/program_income_complier_ses_by_cutoff.tex'
x <- fread(input)
keep <- c('z_sim_mat_4to', 'z_sim_leng_4to',
          'income_decile_imputed', 'father_educ_years_imputed',
          'mother_educ_years_imputed')
labels <- c(z_sim_mat_4to = 'Baseline math score',
            z_sim_leng_4to = 'Baseline verbal score',
            income_decile_imputed = 'Income decile',
            father_educ_years_imputed = "Father's education (years)",
            mother_educ_years_imputed = "Mother's education (years)")
x <- x[va == 'program_income_full' & characteristic %chin% keep]
stopifnot(nrow(x) == 15L, setequal(x$cutoff_percentile, c(25L, 50L, 75L)),
          all(is.finite(x$difference)), all(is.finite(x$difference_se)))
x[, `:=`(characteristic_order = match(characteristic, keep),
         cutoff_order = match(cutoff_percentile, c(25L, 50L, 75L)))]
setorder(x, characteristic_order, cutoff_order)
stars <- function(p) fifelse(p < .01, '***', fifelse(p < .05, '**', fifelse(p < .10, '*', '')))
x[, p_value := 2 * pnorm(-abs(difference / difference_se))]
rows <- unlist(lapply(keep, function(v) {
  z <- x[characteristic == v][match(c(25L, 50L, 75L), cutoff_percentile)]
  c(paste0(labels[[v]], ' & ', sprintf('%.3f', z$population_mean[1]), ' & ',
           paste0(sprintf('%+.3f', z$difference), stars(z$p_value), collapse = ' & '), ' \\\\'),
    paste0(' &  & ', paste0('(', sprintf('%.3f', z$difference_se), ')', collapse = ' & '), ' \\\\'))
}))
tex <- c(
  '\\begin{table}[!htbp]',
  '\\centering',
  '\\caption{Baseline characteristics of projected-income value-added compliers}',
  '\\label{tab:program-income-va-complier-ses}',
  '\\begin{threeparttable}',
  '\\small',
  '\\setlength{\\tabcolsep}{4pt}',
  '\\begin{tabular}{lcccc}',
  '\\toprule',
  ' & SAE mean & Above P25 & Above median & Above P75 \\\\',
  '\\midrule', rows,
  '\\bottomrule',
  '\\end{tabular}',
  '\\begin{tablenotes}[flushleft]',
  '\\footnotesize',
  '\\item Notes: Each threshold column reports the estimated mean baseline characteristic among students induced by their first-round offer to attend a school above the stated projected-income value-added threshold, minus the corresponding mean among all timely SAE applicants in the 2018--2020 cohorts. The SAE mean is common across thresholds. Parentheses contain heteroskedasticity-robust standard errors that account for estimation of the complier and population means and their covariance. High value added is defined using equally weighted school quantiles of EB-shrunken value added. The complier population and estimation sample may differ across thresholds. Differences describe baseline composition and are not treatment effects. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{tablenotes}',
  '\\end{threeparttable}',
  '\\end{table}'
)
writeLines(tex, output)
cat(paste(tex, collapse = '\n'), '\n')
