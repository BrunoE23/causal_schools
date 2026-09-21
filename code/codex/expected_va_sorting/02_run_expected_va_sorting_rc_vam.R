# Descriptive sorting into expected unregularized RC-VAM. Run from repo root.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(min(4L, getDTthreads())); setFixest_nthreads(0)
roots <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'), 'C:/Users/brunem/Box/causal_schools',
  'C:/Users/brunem/Dropbox/causal_schools')
roots <- roots[nzchar(roots) & file.exists(file.path(roots, 'data/clean/univ_gr8_df.csv'))]
if (!length(roots)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the data root.')
clean <- file.path(roots[1], 'data/clean')
value_path <- Sys.getenv('RC_VAM_VALUES_PATH',
  'C:/Users/brunem/Box/causal_schools/data/clean/rc_vam_school_values/test1/rc_vam_school_values_r_noreg.csv')
out_clean <- 'data/clean/expected_va_sorting/rc_vam_test1'
out_table <- 'output/tables/expected_va_sorting/rc_vam_test1'
dir.create(out_clean, recursive = TRUE, showWarnings = FALSE)
dir.create(out_table, recursive = TRUE, showWarnings = FALSE)

specs <- data.table(label = c('Math', 'Verbal', 'Projected income'),
  value_outcome = c('z_year_math_max', 'z_year_leng_max', 'log_program_income_clp_m1'))
specs[, key := c('math', 'verbal', 'program_income')]
v <- fread(value_path, select = c('outcome', 'school_rbd', 'analysis_sample',
  'controlled_value_added_centered_student'))
stopifnot(uniqueN(v$analysis_sample) == 1L,
  unique(v$analysis_sample) == 'SAE_timely_RC_VAM_r')
v <- v[outcome %chin% specs$value_outcome,
  .(school_rbd = as.numeric(school_rbd), outcome,
    value = controlled_value_added_centered_student)]
stopifnot(nrow(v) > 0, !anyDuplicated(v[, .(outcome, school_rbd)]), all(is.finite(v$value)))
wide <- dcast(v, school_rbd ~ outcome, value.var = 'value')
setnames(wide, specs$value_outcome, paste0('va_', specs$key))

covars <- c('z_sim_mat_4to', 'z_sim_leng_4to', 'income_decile_imputed',
  'father_educ_years_imputed', 'mother_educ_years_imputed', 'female')
cols <- c('student_id', 'sae_proceso', 'timely_sae', 'GEN_ALU', covars[covars != 'female'])
message('Reading selected baseline columns.')
u <- fread(file.path(clean, 'univ_gr8_df.csv'), select = cols, na.strings = c('', 'NA'))
u <- u[timely_sae == 1L & sae_proceso %in% 2018:2020 & !is.na(student_id)]
u[, `:=`(student_id = as.numeric(student_id), sae_proceso = as.integer(sae_proceso),
  female = fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2), NA_real_))]
stopifnot(!anyDuplicated(u[, .(student_id, sae_proceso)]))

message('Constructing expected RC-VAM from assignment probabilities.')
p <- rbindlist(lapply(2018:2020, function(y) {
  x <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
    select = c('student_id', 'school_id', 'prob'))
  x[, `:=`(student_id = as.numeric(student_id), sae_proceso = y,
    unmatched = tolower(trimws(school_id)) == 'unmatched',
    school_rbd = suppressWarnings(as.numeric(sub('_.*$', '', school_id))))]
  x[student_id %in% u[sae_proceso == y, student_id],
    .(student_id, sae_proceso, school_rbd, prob, unmatched)]
}))
p <- merge(p, wide, by = 'school_rbd', all.x = TRUE, sort = FALSE)
s <- p[, .(total_mass = sum(prob), unmatched_mass = sum(prob * unmatched)),
  by = .(student_id, sae_proceso)]
for (k in specs$key) {
  col <- paste0('va_', k)
  a <- p[, .(expected = sum(prob * fifelse(unmatched, 0, get(col)), na.rm = TRUE),
    unknown_mass = sum(prob * as.integer(!unmatched & is.na(get(col))))),
    by = .(student_id, sae_proceso)]
  setnames(a, c('expected', 'unknown_mass'), c(paste0('expected_', k), paste0('unknown_', k)))
  s <- merge(s, a, by = c('student_id', 'sae_proceso'), sort = FALSE)
}
rm(p); gc(verbose = FALSE)
dt <- merge(u, s, by = c('student_id', 'sae_proceso'), all = FALSE, sort = FALSE)
expected <- paste0('expected_', specs$key); unknown <- paste0('unknown_', specs$key)
dt[, max_unknown_mass := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = unknown]
common <- dt[abs(total_mass - 1) < 1e-6 & max_unknown_mass < 1e-12 &
  complete.cases(dt[, c(covars, expected), with = FALSE])]
stopifnot(nrow(common) > 0L)

zscore <- function(x) as.numeric((x - mean(x)) / sd(x))
continuous <- covars[covars != 'female']
for (x in continuous) common[, (paste0('z_', x)) := zscore(get(x))]
for (y in expected) common[, (paste0('z_', y)) := zscore(get(y))]
xvars <- c(paste0('z_', continuous), 'female')
xlabels <- c('Baseline math score', 'Baseline verbal score', 'Income decile',
  "Father's education", "Mother's education", 'Female')
coefs <- list(); fits <- list()
for (i in seq_len(nrow(specs))) {
  y <- paste0('z_expected_', specs$key[i])
  full <- feols(as.formula(paste(y, '~', paste(xvars, collapse = ' + '), '| sae_proceso')),
    data = common, vcov = 'hetero', notes = FALSE)
  tab <- coeftable(full)
  coefs[[i]] <- data.table(outcome = specs$label[i], covariate = xvars,
    covariate_label = xlabels, beta = tab[xvars, 1], se = tab[xvars, 2],
    p_value = tab[xvars, 4])
  coefs[[i]][, key := specs$key[i]]
  rss <- sum(resid(full)^2)
  no_ach <- feols(as.formula(paste(y, '~', paste(setdiff(xvars,
    c('z_z_sim_mat_4to', 'z_z_sim_leng_4to')), collapse = ' + '), '| sae_proceso')),
    data = common, notes = FALSE)
  no_ses <- feols(as.formula(paste(y, '~', paste(setdiff(xvars,
    c('z_income_decile_imputed', 'z_father_educ_years_imputed',
      'z_mother_educ_years_imputed')), collapse = ' + '), '| sae_proceso')),
    data = common, notes = FALSE)
  fits[[i]] <- data.table(outcome = specs$label[i], n = nobs(full),
    r2 = unname(r2(full, 'r2')), adjusted_r2 = unname(r2(full, 'ar2')),
    partial_r2_achievement = (sum(resid(no_ach)^2)-rss)/sum(resid(no_ach)^2),
    partial_r2_ses = (sum(resid(no_ses)^2)-rss)/sum(resid(no_ses)^2),
    expected_va_sd_raw = sd(common[[paste0('expected_', specs$key[i])]]))
  fits[[i]][, key := specs$key[i]]
}
coefs <- rbindlist(coefs); fits <- rbindlist(fits)
stopifnot(nrow(coefs) == 18L, all(is.finite(coefs$beta)), all(coefs$se > 0), uniqueN(fits$n) == 1L)
fwrite(coefs, file.path(out_clean, 'expected_rc_vam_sorting_coefficients.csv'))
fwrite(fits, file.path(out_clean, 'expected_rc_vam_sorting_fit.csv'))
fwrite(coefs, file.path(out_table, 'expected_rc_vam_sorting_coefficients.csv'))
fwrite(fits, file.path(out_table, 'expected_rc_vam_sorting_fit.csv'))
fwrite(data.table(starting_timely_sae = nrow(u), with_probabilities = nrow(dt),
  common_complete_sample = nrow(common)), file.path(out_clean, 'sample_diagnostics.csv'))

star <- function(p) fifelse(p < .01, '***', fifelse(p < .05, '**', fifelse(p < .10, '*', '')))
rows <- unlist(lapply(seq_along(xvars), function(j) {
  z <- coefs[covariate == xvars[j]][match(specs$key, key)]
  c(paste0(xlabels[j], ' & ', paste0(sprintf('%.3f', z$beta), star(z$p_value), collapse = ' & '), ' \\\\'),
    paste0(' & ', paste0('(', sprintf('%.3f', z$se), ')', collapse = ' & '), ' \\\\'))
}))
fits <- fits[match(specs$key, key)]
tex <- c('\\begin{table}[!htbp]', '\\centering',
  '\\caption{Baseline predictors of expected risk-controlled school value added}',
  '\\label{tab:expected-rc-vam-sorting}', '\\begin{threeparttable}',
  '\\begin{tabular}{lccc}', '\\toprule',
  paste0(' & ', paste(specs$label, collapse = ' & '), ' \\\\'), '\\midrule', rows,
  '\\midrule', paste0('$R^2$ & ', paste(sprintf('%.3f', fits$r2), collapse = ' & '), ' \\\\'),
  paste0('Adjusted $R^2$ & ', paste(sprintf('%.3f', fits$adjusted_r2), collapse = ' & '), ' \\\\'),
  paste0('Partial $R^2$: achievement & ', paste(sprintf('%.3f', fits$partial_r2_achievement), collapse = ' & '), ' \\\\'),
  paste0('Partial $R^2$: SES & ', paste(sprintf('%.3f', fits$partial_r2_ses), collapse = ' & '), ' \\\\'),
  paste0('N & ', paste(format(fits$n, big.mark = ','), collapse = ' & '), ' \\\\'),
  '\\bottomrule', '\\end{tabular}', '\\begin{tablenotes}[flushleft]', '\\footnotesize',
  '\\item Notes: The dependent variable in each column is standardized application-specific expected unregularized risk-controlled value added, $E_i[V^m]=\\sum_jp_{ij}V_j^m$. Continuous baseline covariates are standardized on the common estimation sample; female is a binary indicator. All specifications include assignment-cohort fixed effects. The sample contains timely 2018--2020 SAE applicants with complete covariates, probability mass one, and full school-value coverage across all three dimensions; simulated unmatched assignments receive centered VA zero. Heteroskedasticity-robust standard errors are in parentheses. These are descriptive sorting relationships, not causal effects. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{tablenotes}', '\\end{threeparttable}', '\\end{table}')
writeLines(tex, file.path(out_table, 'expected_rc_vam_sorting.tex'))
print(fits); message('Common RC-VAM sample N = ', nrow(common))
