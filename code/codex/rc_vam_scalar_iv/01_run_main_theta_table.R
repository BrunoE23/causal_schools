# Main theta/pass-through table using the supplied unregularized RC-VAM values.
# Run from the repository root.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(min(4L, getDTthreads()))
setFixest_nthreads(0)

find_data_root <- function() {
  candidates <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'),
    'C:/Users/brunem/Box/causal_schools',
    'C:/Users/brunem/Dropbox/causal_schools')
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(file.path(candidates, 'data/clean/univ_gr8_df.csv'))]
  if (!length(hit)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the project data root.')
  hit[1]
}
z_within_group <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
stars <- function(p) fifelse(p < .01, '***', fifelse(p < .05, '**', fifelse(p < .10, '*', '')))

data_root <- find_data_root()
clean <- file.path(data_root, 'data/clean')
value_path <- Sys.getenv('RC_VAM_VALUES_PATH',
  'C:/Users/brunem/Box/causal_schools/data/clean/rc_vam_school_values/test1/rc_vam_school_values_r_noreg.csv')
income_path <- 'output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv'
out_clean <- 'data/clean/rc_vam_scalar_iv/test1'
out_table <- 'output/tables/rc_vam_scalar_iv/test1'
dir.create(out_clean, recursive = TRUE, showWarnings = FALSE)
dir.create(out_table, recursive = TRUE, showWarnings = FALSE)

specs <- data.table(
  spec = c('math', 'verbal', 'program_income'),
  outcome = c('z_year_math_max', 'z_year_leng_max', 'log_program_income_clp_m1'),
  value_outcome = c('z_year_math_max', 'z_year_leng_max', 'log_program_income_clp_m1'),
  label = c('Math', 'Verbal', 'Log projected income'),
  require_exam = c(TRUE, TRUE, FALSE)
)
message('Reading RC-VAM school values: ', value_path)
values <- fread(value_path, na.strings = c('', 'NA'))
required_value_cols <- c('outcome', 'school_rbd', 'controlled_value_added_centered_student',
  'controlled_value_added_se', 'analysis_sample')
stopifnot(all(required_value_cols %in% names(values)),
  uniqueN(values$analysis_sample) == 1L,
  unique(values$analysis_sample) == 'SAE_timely_RC_VAM_r')
values <- values[outcome %chin% specs$value_outcome,
  .(school_rbd = as.numeric(school_rbd), outcome,
    value = controlled_value_added_centered_student,
    value_se = controlled_value_added_se,
    n_students_regression)]
stopifnot(nrow(values) > 0L, !anyDuplicated(values[, .(outcome, school_rbd)]),
  all(is.finite(values$value)), all(is.finite(values$value_se)))
wide <- dcast(values, school_rbd ~ outcome, value.var = 'value')
setnames(wide, specs$value_outcome, paste0('school_value_', specs$spec))

universe_cols <- c('student_id', 'mrun', 'cohort_gr8', 'sae_proceso', 'timely_sae',
  'rbd_treated_1R', 'most_time_RBD', 'GEN_ALU', 'EDAD_ALU',
  'z_sim_mat_4to', 'z_sim_leng_4to', 'math_max', 'leng_max', 'psu_year')
message('Reading selected universe columns.')
u <- fread(file.path(clean, 'univ_gr8_df.csv'), select = universe_cols, na.strings = c('', 'NA'))
u[, `:=`(student_id = as.numeric(student_id), mrun = as.numeric(mrun),
  sae_proceso = as.integer(sae_proceso), rbd_treated_1R = as.numeric(rbd_treated_1R),
  most_time_RBD = as.numeric(most_time_RBD))]
u[!is.na(psu_year) & !is.na(math_max) & math_max > 0,
  z_year_math_max := z_within_group(math_max), by = psu_year]
u[!is.na(psu_year) & !is.na(leng_max) & leng_max > 0,
  z_year_leng_max := z_within_group(leng_max), by = psu_year]
u <- u[timely_sae == 1L & sae_proceso %in% 2018:2020]
stopifnot(!anyDuplicated(u[, .(student_id, sae_proceso)]))

income <- fread(income_path, select = c('mrun', 'log_program_income_clp_m1'),
  na.strings = c('', 'NA'))
income[, mrun := as.numeric(mrun)]
stopifnot(!anyDuplicated(income$mrun))
u <- merge(u, income, by = 'mrun', all.x = TRUE, sort = FALSE)
u[, admission_exam_taker := as.integer(!is.na(z_year_math_max) | !is.na(z_year_leng_max))]

message('Computing expected RC-VAM values from DA probabilities.')
prob <- rbindlist(lapply(2018:2020, function(y) {
  p <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
    select = c('student_id', 'school_id', 'prob'))
  p[, `:=`(student_id = as.numeric(student_id), sae_proceso = y,
    is_unmatched = tolower(trimws(school_id)) == 'unmatched',
    school_rbd = suppressWarnings(as.numeric(sub('_.*$', '', school_id))))]
  p[student_id %in% u[sae_proceso == y, student_id],
    .(student_id, sae_proceso, school_rbd, prob, is_unmatched)]
}))
prob <- merge(prob, wide, by = 'school_rbd', all.x = TRUE, sort = FALSE)
summary <- prob[, .(any_risk = as.integer(max(prob, na.rm = TRUE) < 1),
  total_probability_mass = sum(prob, na.rm = TRUE),
  unmatched_probability_mass = sum(prob * as.integer(is_unmatched), na.rm = TRUE)),
  by = .(student_id, sae_proceso)]
for (s in specs$spec) {
  value_col <- paste0('school_value_', s)
  tmp <- prob[, .(expected = sum(prob * fifelse(is.na(get(value_col)), 0, get(value_col)), na.rm = TRUE),
    mass_with_value = sum(prob * as.integer(!is.na(get(value_col))), na.rm = TRUE)),
    by = .(student_id, sae_proceso)]
  setnames(tmp, c('expected', 'mass_with_value'), c(paste0('expected_', s), paste0('mass_with_value_', s)))
  summary <- merge(summary, tmp, by = c('student_id', 'sae_proceso'), all.x = TRUE, sort = FALSE)
}
rm(prob); gc(verbose = FALSE)
dt <- merge(u, summary, by = c('student_id', 'sae_proceso'), all = FALSE, sort = FALSE)
dt <- dt[any_risk == 1L]

attended <- copy(wide)
setnames(attended, paste0('school_value_', specs$spec), paste0('d_', specs$spec))
dt <- merge(dt, attended, by.x = 'most_time_RBD', by.y = 'school_rbd', all.x = TRUE, sort = FALSE)
offered <- copy(wide)
setnames(offered, paste0('school_value_', specs$spec), paste0('z_', specs$spec))
dt <- merge(dt, offered, by.x = 'rbd_treated_1R', by.y = 'school_rbd', all.x = TRUE, sort = FALSE)
for (s in specs$spec) dt[is.na(rbd_treated_1R) | rbd_treated_1R == 0, (paste0('z_', s)) := 0]

run_spec <- function(row) {
  y <- row$outcome; s <- row$spec
  d <- paste0('d_', s); z <- paste0('z_', s); expected <- paste0('expected_', s)
  mass <- paste0('mass_with_value_', s)
  controls <- c('factor(cohort_gr8)', 'z_sim_mat_4to', 'z_sim_leng_4to',
    'factor(GEN_ALU)', 'factor(EDAD_ALU)', expected)
  needed <- c(y, d, z, controls[2:3], 'cohort_gr8', 'GEN_ALU', 'EDAD_ALU', expected, mass)
  reg <- dt[complete.cases(dt[, ..needed])]
  if (isTRUE(row$require_exam)) reg <- reg[admission_exam_taker == 1L]
  formula <- as.formula(paste0(y, ' ~ ', paste(controls, collapse = ' + '), ' | 0 | ', d, ' ~ ', z))
  model <- feols(formula, data = reg, vcov = 'hetero', notes = FALSE)
  term <- paste0('fit_', d)
  if (!term %in% names(coef(model))) term <- d
  fs_formula <- as.formula(paste0(d, ' ~ ', z, ' + ', paste(controls, collapse = ' + ')))
  fs <- feols(fs_formula, data = reg, vcov = 'hetero', notes = FALSE)
  beta <- unname(coef(model)[term]); std_error <- unname(se(model)[term])
  fs_beta <- unname(coef(fs)[z]); fs_se <- unname(se(fs)[z])
  data.table(spec = s, outcome = y, label = row$label, theta = beta, se = std_error,
    p_value = 2*pnorm(-abs(beta/std_error)), n_obs = nobs(model),
    first_stage_beta = fs_beta, first_stage_se = fs_se, first_stage_f = (fs_beta/fs_se)^2,
    expected_va_beta = unname(coef(model)[expected]), expected_va_se = unname(se(model)[expected]),
    mean_probability_mass_with_value = mean(reg[[mass]]),
    share_full_probability_mass_with_value = mean(abs(reg[[mass]] - 1) < 1e-6),
    mean_probability_mass_accounted = mean(reg[[mass]] + reg$unmatched_probability_mass),
    share_full_probability_mass_accounted = mean(abs(reg[[mass]] + reg$unmatched_probability_mass - 1) < 1e-6))
}
results <- rbindlist(lapply(seq_len(nrow(specs)), function(i) run_spec(specs[i])))
stopifnot(nrow(results) == 3L, all(is.finite(results$theta)), all(results$se > 0),
  all(results$first_stage_f > 0))
fwrite(results, file.path(out_clean, 'main_theta_results.csv'))
fwrite(results[, .(spec, outcome, label, theta, se, p_value, n_obs,
  first_stage_beta, first_stage_se, first_stage_f)],
  file.path(out_table, 'main_theta_math_verbal_program_income.csv'))
diagnostics <- merge(specs[, .(spec, value_outcome)],
  values[, .(n_schools = .N, min_school_n = min(n_students_regression),
    median_school_n = median(n_students_regression), max_school_n = max(n_students_regression)),
    by = outcome], by.x = 'value_outcome', by.y = 'outcome')
diagnostics <- merge(diagnostics, results[, .(spec, n_obs, first_stage_beta, first_stage_se, first_stage_f,
  mean_probability_mass_with_value, share_full_probability_mass_with_value)], by = 'spec')
diagnostics <- merge(diagnostics,
  results[, .(spec, mean_probability_mass_accounted, share_full_probability_mass_accounted)],
  by = 'spec')
fwrite(diagnostics, file.path(out_clean, 'main_theta_diagnostics.csv'))

results[, cell := paste0(sprintf('%.3f', theta), stars(p_value))]
theta_row <- paste0('$\\theta^{RC}$ & ', paste(results$cell, collapse = ' & '), ' \\\\')
se_row <- paste0(' & ', paste0('(', sprintf('%.3f', results$se), ')', collapse = ' & '), ' \\\\')
n_row <- paste0('N & ', paste(format(results$n_obs, big.mark = ','), collapse = ' & '), ' \\\\')
f_row <- paste0('First-stage F & ', paste(sprintf('%.1f', results$first_stage_f), collapse = ' & '), ' \\\\')
tex <- c('\\begin{table}[!htbp]', '\\centering',
  '\\caption{Pass-through estimates using unregularized risk-controlled value added}',
  '\\label{tab:rc-vam-main-theta-test1}',
  '\\begin{threeparttable}', '\\begin{tabular}{lccc}', '\\toprule',
  paste0(' & ', paste(results$label, collapse = ' & '), ' \\\\'), '\\midrule',
  theta_row, se_row, n_row, f_row, '\\bottomrule', '\\end{tabular}',
  '\\begin{tablenotes}[flushleft]', '\\footnotesize',
  '\\item Notes: Each column reports the scalar IV pass-through from attended-school risk-controlled value added to the corresponding student outcome. Attended-school RC-VAM is instrumented with first-round offered-school RC-VAM. Specifications control for the DA-probability expected value of the same RC-VAM measure, cohort, grade-4 SIMCE math and verbal scores, gender, and age. The sample contains timely SAE applicants from the 2018--2020 assignment cohorts with nondegenerate assignment risk and complete regression variables. The RC-VAM inputs are the supplied unregularized test1 estimates using 2,656 assignment-probability terms. Heteroskedasticity-robust standard errors are in parentheses. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.',
  '\\end{tablenotes}', '\\end{threeparttable}', '\\end{table}')
writeLines(tex, file.path(out_table, 'main_theta_math_verbal_program_income.tex'))
print(results[, .(label, theta, se, p_value, n_obs, first_stage_f)])
message('Wrote separate RC-VAM outputs under ', out_table)
