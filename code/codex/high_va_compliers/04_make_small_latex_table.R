# Small paper-facing tables: cutoff-specific VA complier composition.
suppressPackageStartupMessages(library(data.table))
input <- 'data/clean/high_va_compliers/all_cutoffs_minus_sae_population.csv'
all_results <- fread(input)
specs <- data.table(
  va = c('program_income_full', 'math', 'highpay'),
  descriptor = c('projected-income', 'math', 'high-premium-field'),
  filename = c('program_income_complier_ses_by_cutoff.tex',
               'math_complier_ses_by_cutoff.tex',
               'high_premium_field_complier_ses_by_cutoff.tex'),
  label = c('tab:program-income-va-complier-ses',
            'tab:math-va-complier-ses',
            'tab:high-premium-field-va-complier-ses')
)
keep <- c('z_sim_mat_4to', 'z_sim_leng_4to',
          'income_decile_imputed', 'father_educ_years_imputed',
          'mother_educ_years_imputed')
labels <- c(z_sim_mat_4to = 'Baseline math score',
            z_sim_leng_4to = 'Baseline verbal score',
            income_decile_imputed = 'Income decile',
            father_educ_years_imputed = "Father's education (years)",
            mother_educ_years_imputed = "Mother's education (years)")
stars <- function(p) fifelse(p < .01, '***', fifelse(p < .05, '**', fifelse(p < .10, '*', '')))
for (i in seq_len(nrow(specs))) {
  spec <- specs[i]
  x <- all_results[va == spec$va & characteristic %chin% keep]
  stopifnot(nrow(x) == 15L, setequal(x$cutoff_percentile, c(25L, 50L, 75L)),
            all(is.finite(x$difference)), all(is.finite(x$difference_se)))
  x[, `:=`(characteristic_order = match(characteristic, keep),
           cutoff_order = match(cutoff_percentile, c(25L, 50L, 75L)),
           p_value = 2 * pnorm(-abs(difference / difference_se)))]
  setorder(x, characteristic_order, cutoff_order)
  rows <- unlist(lapply(keep, function(v) {
    z <- x[characteristic == v][match(c(25L, 50L, 75L), cutoff_percentile)]
    c(paste0(labels[[v]], ' & ', sprintf('%.3f', z$population_mean[1]), ' & ',
             paste0(sprintf('%+.3f', z$difference), stars(z$p_value), collapse = ' & '), ' \\\\'),
      paste0(' &  & ', paste0('(', sprintf('%.3f', z$difference_se), ')', collapse = ' & '), ' \\\\'))
  }))
  tex <- c(
    '\\begin{table}[!htbp]',
    '\\centering',
    paste0('\\caption{Baseline characteristics of ', spec$descriptor, ' value-added compliers}'),
    paste0('\\label{', spec$label, '}'),
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
    paste0('\\item Notes: Each threshold column reports the estimated mean baseline characteristic among students induced by their first-round offer to attend a school above the stated ', spec$descriptor, ' value-added threshold, minus the corresponding mean among all timely SAE applicants in the 2018--2020 cohorts. The SAE mean is common across thresholds. Parentheses contain heteroskedasticity-robust standard errors that account for estimation of the complier and population means and their covariance. High value added is defined using equally weighted school quantiles of EB-shrunken value added. The complier population and estimation sample may differ across thresholds. Differences describe baseline composition and are not treatment effects. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.'),
    '\\end{tablenotes}',
    '\\end{threeparttable}',
    '\\end{table}'
  )
  output <- file.path('output/tables/high_va_compliers', spec$filename)
  writeLines(tex, output)
  message('Wrote: ', output)
}
