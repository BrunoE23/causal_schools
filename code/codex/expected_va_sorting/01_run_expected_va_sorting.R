# Descriptive sorting into expected EB school VA. Run from repository root.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(min(4L, getDTthreads())); setFixest_nthreads(0)

roots <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'),
  'C:/Users/brunem/Box/causal_schools', 'C:/Users/brunem/Dropbox/causal_schools')
roots <- roots[nzchar(roots) & file.exists(file.path(roots, 'data/clean/univ_gr8_df.csv'))]
if (!length(roots)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the data root.')
clean <- file.path(roots[1], 'data/clean')
value_dir <- 'output/tables/empirical_bayes_school_va'
out_clean <- 'data/clean/expected_va_sorting'
out_table <- 'output/tables/expected_va_sorting'
dir.create(out_clean, recursive = TRUE, showWarnings = FALSE)
dir.create(out_table, recursive = TRUE, showWarnings = FALSE)

specs <- data.table(
  label = c('Math', 'Verbal', 'High-premium institution',
    'High-premium field', 'Projected income'),
  file = paste0('stata_eb_school_values_',
    c('math', 'language', 'highinst', 'highpay', 'program_income_full'), '.csv')
)
specs[, key := c('math', 'language', 'highinst', 'highpay', 'program_income_full')]
values <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  x <- fread(file.path(value_dir, specs$file[i]),
    select = c('school_rbd', 'analysis_sample', 'va_eb_centered'))
  x <- x[analysis_sample == 'All']
  stopifnot(nrow(x) > 0L, !anyDuplicated(x$school_rbd), all(is.finite(x$va_eb_centered)))
  x[, .(school_rbd = as.numeric(school_rbd), key = specs$key[i], value = va_eb_centered)]
}))
wide <- dcast(values, school_rbd ~ key, value.var = 'value')
setnames(wide, specs$key, paste0('va_', specs$key))

cols <- c('student_id', 'sae_proceso', 'timely_sae', 'GEN_ALU',
  'z_sim_mat_4to', 'z_sim_leng_4to', 'income_decile_imputed',
  'father_educ_years_imputed', 'mother_educ_years_imputed')
message('Reading selected baseline columns.')
u <- fread(file.path(clean, 'univ_gr8_df.csv'), select = cols, na.strings = c('', 'NA'))
u <- u[timely_sae == 1L & sae_proceso %in% 2018:2020 & !is.na(student_id)]
u[, `:=`(student_id = as.numeric(student_id), sae_proceso = as.integer(sae_proceso),
  female = fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2), NA_real_))]
stopifnot(!anyDuplicated(u[, .(student_id, sae_proceso)]))

message('Constructing application-specific expected EB VA.')
prob <- rbindlist(lapply(2018:2020, function(y) {
  p <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
    select = c('student_id', 'school_id', 'prob'))
  p[, `:=`(student_id = as.numeric(student_id), sae_proceso = y,
    unmatched = tolower(trimws(school_id)) == 'unmatched',
    school_rbd = suppressWarnings(as.numeric(sub('_.*$', '', school_id))))]
  p[student_id %in% u[sae_proceso == y, student_id],
    .(student_id, sae_proceso, school_rbd, prob, unmatched)]
}))
prob <- merge(prob, wide, by = 'school_rbd', all.x = TRUE, sort = FALSE)
summary <- prob[, .(total_mass = sum(prob), unmatched_mass = sum(prob * unmatched)),
  by = .(student_id, sae_proceso)]
for (k in specs$key) {
  col <- paste0('va_', k)
  s <- prob[, .(expected = sum(prob * fifelse(unmatched, 0, get(col)), na.rm = TRUE),
    unknown_mass = sum(prob * as.integer(!unmatched & is.na(get(col))))),
    by = .(student_id, sae_proceso)]
  setnames(s, c('expected', 'unknown_mass'), c(paste0('expected_', k), paste0('unknown_', k)))
  summary <- merge(summary, s, by = c('student_id', 'sae_proceso'), sort = FALSE)
}
rm(prob); gc(verbose = FALSE)
dt <- merge(u, summary, by = c('student_id', 'sae_proceso'), all = FALSE, sort = FALSE)

covars <- c('z_sim_mat_4to', 'z_sim_leng_4to', 'income_decile_imputed',
  'father_educ_years_imputed', 'mother_educ_years_imputed', 'female')
expected <- paste0('expected_', specs$key)
unknown <- paste0('unknown_', specs$key)
dt[, max_unknown_mass := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = unknown]
common <- dt[abs(total_mass - 1) < 1e-6 & max_unknown_mass < 1e-12 &
  complete.cases(dt[, c(covars, expected), with = FALSE])]
stopifnot(nrow(common) > 0L)

zscore <- function(x) as.numeric((x - mean(x)) / sd(x))
continuous <- covars[covars != 'female']
for (v in continuous) common[, (paste0('z_', v)) := zscore(get(v))]
for (v in expected) common[, (paste0('z_', v)) := zscore(get(v))]
xvars <- c(paste0('z_', continuous), 'female')
xlabels <- c('Baseline math score', 'Baseline verbal score', 'Income decile',
  "Father's education", "Mother's education", 'Female')

results <- list(); fit_stats <- list()
for (i in seq_len(nrow(specs))) {
  y <- paste0('z_expected_', specs$key[i])
  full <- feols(as.formula(paste(y, '~', paste(xvars, collapse = ' + '), '| sae_proceso')),
    data = common, vcov = 'hetero', notes = FALSE)
  tab <- coeftable(full)
  results[[i]] <- data.table(outcome = specs$label[i],
    covariate = xvars, covariate_label = xlabels,
    beta = tab[xvars, 1], se = tab[xvars, 2], p_value = tab[xvars, 4])
  results[[i]][, key := specs$key[i]]
  rss_full <- sum(resid(full)^2)
  reduced_achievement <- feols(as.formula(paste(y, '~', paste(setdiff(xvars,
    c('z_z_sim_mat_4to', 'z_z_sim_leng_4to')), collapse = ' + '), '| sae_proceso')),
    data = common, notes = FALSE)
  reduced_ses <- feols(as.formula(paste(y, '~', paste(setdiff(xvars,
    c('z_income_decile_imputed', 'z_father_educ_years_imputed',
      'z_mother_educ_years_imputed')), collapse = ' + '), '| sae_proceso')),
    data = common, notes = FALSE)
  fit_stats[[i]] <- data.table(outcome = specs$label[i],
    n = nobs(full), r2 = unname(r2(full, 'r2')), adjusted_r2 = unname(r2(full, 'ar2')),
    partial_r2_achievement = (sum(resid(reduced_achievement)^2) - rss_full) /
      sum(resid(reduced_achievement)^2),
    partial_r2_ses = (sum(resid(reduced_ses)^2) - rss_full) / sum(resid(reduced_ses)^2),
    expected_va_sd_raw = sd(common[[paste0('expected_', specs$key[i])]]))
  fit_stats[[i]][, key := specs$key[i]]
}
results <- rbindlist(results); fit_stats <- rbindlist(fit_stats)
stopifnot(nrow(results) == 30L, all(is.finite(results$beta)), all(results$se > 0),
  uniqueN(fit_stats$n) == 1L)
fwrite(results, file.path(out_clean, 'expected_va_sorting_coefficients.csv'))
fwrite(fit_stats, file.path(out_clean, 'expected_va_sorting_fit.csv'))
fwrite(data.table(starting_timely_sae = nrow(u), with_probabilities = nrow(dt),
  common_complete_sample = nrow(common)), file.path(out_clean, 'sample_diagnostics.csv'))
fwrite(results, file.path(out_table, 'expected_va_sorting_coefficients.csv'))
fwrite(fit_stats, file.path(out_table, 'expected_va_sorting_fit.csv'))

star <- function(p) fifelse(p < .01, '***', fifelse(p < .05, '**', fifelse(p < .10, '*', '')))
rows <- unlist(lapply(seq_along(xvars), function(j) {
  z <- results[covariate == xvars[j]][match(specs$key, key)]
  c(paste0(xlabels[j], ' & ', paste0(sprintf('%.3f', z$beta), star(z$p_value), collapse = ' & '), ' \\\\'),
    paste0(' & ', paste0('(', sprintf('%.3f', z$se), ')', collapse = ' & '), ' \\\\'))
}))
fit_stats <- fit_stats[match(specs$key, key)]
tex <- c('\\begin{table}[!htbp]', '\\centering',
  '\\caption{Baseline predictors of expected school value added}',
  '\\label{tab:expected-va-sorting}', '\\resizebox{\\textwidth}{!}{%',
  '\\begin{tabular}{lccccc}', '\\toprule',
  paste0(' & ', paste(specs$label, collapse = ' & '), ' \\\\'), '\\midrule', rows,
  '\\midrule',
  paste0('$R^2$ & ', paste(sprintf('%.3f', fit_stats$r2), collapse = ' & '), ' \\\\'),
  paste0('Adjusted $R^2$ & ', paste(sprintf('%.3f', fit_stats$adjusted_r2), collapse = ' & '), ' \\\\'),
  paste0('Partial $R^2$: achievement & ', paste(sprintf('%.3f', fit_stats$partial_r2_achievement), collapse = ' & '), ' \\\\'),
  paste0('Partial $R^2$: SES & ', paste(sprintf('%.3f', fit_stats$partial_r2_ses), collapse = ' & '), ' \\\\'),
  paste0('N & ', paste(format(fit_stats$n, big.mark = ','), collapse = ' & '), ' \\\\'),
  '\\bottomrule', '\\end{tabular}', '}', '\\par\\medskip', '\\footnotesize',
  '\\begin{minipage}{\\textwidth}',
  'Notes: The dependent variable in each column is the standardized application-specific expected EB-shrunken school value added, $E_i[V^m]=\\sum_j p_{ij}V_j^m$. Continuous baseline covariates are standardized on the common estimation sample; the female coefficient is the difference relative to male applicants in outcome SD units. All specifications include assignment-cohort fixed effects. The sample contains timely 2018--2020 SAE applicants with complete baseline covariates, complete probability mass, and VA coverage for every positive-probability school option across all five dimensions. Simulated unmatched assignments receive centered VA zero. Heteroskedasticity-robust standard errors are in parentheses. The achievement block contains baseline math and verbal scores; the SES block contains income decile and both parental-education measures. These are descriptive sorting relationships, not causal effects. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{minipage}', '\\end{table}')
writeLines(tex, file.path(out_table, 'expected_va_sorting.tex'))
print(fit_stats)
message('Common sample N = ', nrow(common))
