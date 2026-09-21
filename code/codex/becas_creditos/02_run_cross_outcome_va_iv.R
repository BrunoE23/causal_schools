###############################################################################
# Cross-outcome lottery IV: math, high-premium-field, postulacion, gratuidad
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
income_decile_max <- suppressWarnings(as.numeric(Sys.getenv("INCOME_DECILE_MAX", unset = "Inf")))
if (is.na(income_decile_max)) stop("INCOME_DECILE_MAX must be numeric.")
income_subset_var <- "income_decile"
analysis_suffix <- if (is.finite(income_decile_max)) {
  paste0("_simce4_income_decile_le", income_decile_max)
} else {
  ""
}
clean_dir <- file.path(data_wd, "data", "clean")
output_dir <- file.path(clean_dir, "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
middle_controls_path <- file.path(clean_dir, "middle_school_controls", "middle_school_controls.csv")
program_outcomes_path <- file.path(repo_wd, "output", "tables", "mifuturo_matricula_income", "mifuturo_person_level_income_outcomes.csv")
school_values_path <- file.path(clean_dir, "school_rbd_observational_values", "school_rbd_observational_values.csv")
benefits_outcomes_path <- file.path(output_dir, "becas_creditos_outcomes.csv")
benefits_va_path <- file.path(output_dir, "becas_creditos_school_va.csv")
prob_dir <- file.path(clean_dir, "DA_probs")

dimensions <- data.table(
  dimension_key = c("math", "highpay_field", "any_postulacion", "grat_asignacion"),
  label = c("Math", "High-premium field", "Any postulacion", "Gratuidad assignment"),
  outcome = c("z_year_math_max", "high_paying_field_m1", "any_postulacion", "grat_asignacion")
)
control_vars <- c(
  "cohort_gr8", "GEN_ALU", "EDAD_ALU", "COD_COM_ALU", "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "z_gpa_middle_mean", "z_att_middle_mean", "middle_years_observed",
  "z_sim_mat_4to", "z_sim_leng_4to"
)
va_controls <- c(
  "factor(cohort_gr8)", "factor(GEN_ALU)", "factor(EDAD_ALU)", "factor(COD_COM_ALU)",
  "income_decile_imputed", "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "z_gpa_middle_mean", "z_att_middle_mean", "factor(middle_years_observed)",
  "z_sim_mat_4to", "I(z_sim_mat_4to^2)", "I(z_sim_mat_4to^3)",
  "z_sim_leng_4to", "I(z_sim_leng_4to^2)", "I(z_sim_leng_4to^3)"
)

key_from_numeric <- function(x) as.character(as.numeric(x))
extract_term <- function(model, term) {
  candidates <- names(coef(model))
  if (term %in% candidates) return(term)
  fitted_term <- paste0("fit_", term)
  if (fitted_term %in% candidates) return(fitted_term)
  stop("Missing coefficient: ", term, call. = FALSE)
}
z_within_group <- function(x) {
  sigma <- sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / sigma
}

message("Reading grade-8 universe and outcome files.")
universe_columns <- c(
  "MRUN", "student_id", "cohort_gr8", "sae_proceso", "timely_sae", "rbd_treated_1R",
  "most_time_RBD", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to",
  "math_max", "psu_year", "COD_COM_ALU", "income_decile", "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed", "father_indigenous_imputed",
  "mother_indigenous_imputed", "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed"
)
universe <- fread(universe_path, select = universe_columns, na.strings = c("", "NA"))
universe[, `:=`(
  mrun_key = key_from_numeric(MRUN),
  student_id = as.numeric(student_id),
  sae_proceso = as.integer(sae_proceso),
  most_time_RBD = as.numeric(most_time_RBD),
  rbd_treated_1R = as.numeric(rbd_treated_1R)
)]
universe[!is.na(psu_year) & !is.na(math_max) & math_max > 0,
         z_year_math_max := z_within_group(math_max), by = psu_year]

middle_controls <- fread(
  middle_controls_path,
  select = c("MRUN", "most_time_RBD_middle", "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean"),
  na.strings = c("", "NA")
)
middle_controls[, `:=`(mrun_key = key_from_numeric(MRUN), MRUN = NULL)]
universe <- merge(universe, middle_controls, by = "mrun_key", all.x = TRUE, sort = FALSE)
universe[, most_time_RBD_middle := as.numeric(most_time_RBD_middle)]

highpay <- fread(program_outcomes_path, select = c("MRUN", "high_paying_field_m1"), na.strings = c("", "NA"))
highpay[, `:=`(mrun_key = key_from_numeric(MRUN), MRUN = NULL)]
if (anyDuplicated(highpay$mrun_key)) stop("High-premium-field file is not unique by MRUN.")
universe <- merge(universe, highpay, by = "mrun_key", all.x = TRUE, sort = FALSE)

benefits <- fread(benefits_outcomes_path)
benefits[, mrun_key := key_from_numeric(mrun_key)]
universe <- merge(universe, benefits[, .(mrun_key, any_postulacion, grat_asignacion)], by = "mrun_key", all.x = TRUE, sort = FALSE)

analytic <- universe[
  !is.na(most_time_RBD) & most_time_RBD > 0 &
    !is.na(most_time_RBD_middle) & !is.na(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16
]
if (is.finite(income_decile_max)) {
  analytic <- analytic[!is.na(get(income_subset_var)) & get(income_subset_var) <= income_decile_max]
}

message("Estimating all four school value-added measures within the selected population.")
estimate_school_va <- function(outcome_name, va_name) {
  needed <- c(outcome_name, control_vars, "most_time_RBD", "most_time_RBD_middle")
  model_data <- analytic[complete.cases(analytic[, ..needed])]
  model <- feols(
    as.formula(paste0(
      outcome_name, " ~ ", paste(va_controls, collapse = " + "),
      " | most_time_RBD + most_time_RBD_middle"
    )),
    data = model_data,
    notes = FALSE
  )
  effects <- fixef(model)[["most_time_RBD"]]
  values <- data.table(school_rbd = as.numeric(names(effects)), value = as.numeric(effects))
  counts <- model_data[, .(n_students_va = .N), by = .(school_rbd = most_time_RBD)]
  values <- merge(values, counts, by = "school_rbd", all.x = TRUE)
  values[, value := value - weighted.mean(value, n_students_va)]
  setnames(values, "value", va_name)
  values[, c("school_rbd", va_name), with = FALSE]
}

va_specs <- data.table(
  outcome = dimensions$outcome,
  va_name = c("math_va", "highpay_field_va", "any_postulacion_va", "grat_asignacion_va")
)
school_va <- Reduce(
  function(x, y) merge(x, y, by = "school_rbd", all = TRUE),
  lapply(seq_len(nrow(va_specs)), function(i) estimate_school_va(va_specs$outcome[i], va_specs$va_name[i]))
)
fwrite(school_va, file.path(output_dir, paste0("cross_outcome", analysis_suffix, "_school_va.csv")))

message("Computing expected school values from lottery probabilities.")
probabilities <- rbindlist(lapply(2018:2021, function(year) {
  data <- fread(file.path(prob_dir, paste0("DA_probs_", year, ".csv")), select = c("student_id", "school_id", "prob"))
  data[, `:=`(
    student_id = as.numeric(student_id),
    sae_proceso = as.integer(year),
    rbd_prob = suppressWarnings(as.numeric(sub("_.*$", "", school_id)))
  )]
  data[, .(student_id, sae_proceso, rbd_prob, prob)]
}), use.names = TRUE)
va_columns <- c("math_va", "highpay_field_va", "any_postulacion_va", "grat_asignacion_va")
probabilities <- merge(probabilities, school_va, by.x = "rbd_prob", by.y = "school_rbd", all.x = TRUE, sort = FALSE)
prob_summary <- probabilities[, .(
  any_risk = as.integer(max(prob, na.rm = TRUE) < 1),
  total_probability_mass = sum(prob, na.rm = TRUE)
), by = .(student_id, sae_proceso)]
for (value_name in va_columns) {
  expected <- probabilities[, .(
    expected_value = sum(prob * fifelse(is.na(get(value_name)), 0, get(value_name)), na.rm = TRUE)
  ), by = .(student_id, sae_proceso)]
  setnames(expected, "expected_value", paste0("expected_", value_name))
  prob_summary <- merge(prob_summary, expected, by = c("student_id", "sae_proceso"), all.x = TRUE, sort = FALSE)
}

iv_data <- merge(universe, prob_summary, by = c("student_id", "sae_proceso"), all = FALSE, sort = FALSE)
iv_data <- iv_data[timely_sae == 1L & any_risk == 1L]
if (is.finite(income_decile_max)) {
  iv_data <- iv_data[!is.na(get(income_subset_var)) & get(income_subset_var) <= income_decile_max]
}
attended_values <- copy(school_va)
setnames(attended_values, va_columns, paste0("d_", va_columns))
iv_data <- merge(iv_data, attended_values, by.x = "most_time_RBD", by.y = "school_rbd", all.x = TRUE, sort = FALSE)
offered_values <- copy(school_va)
setnames(offered_values, va_columns, paste0("z_", va_columns))
iv_data <- merge(iv_data, offered_values, by.x = "rbd_treated_1R", by.y = "school_rbd", all.x = TRUE, sort = FALSE)

message("Running the 4 by 4 cross-outcome lottery IV matrix.")
setFixest_nthreads(0)
results <- rbindlist(lapply(seq_len(nrow(dimensions)), function(outcome_i) {
  outcome_row <- dimensions[outcome_i]
  rbindlist(lapply(seq_len(nrow(dimensions)), function(treatment_i) {
    treatment_row <- dimensions[treatment_i]
    d <- paste0("d_", treatment_row$dimension_key, "_va")
    z <- paste0("z_", treatment_row$dimension_key, "_va")
    expected <- paste0("expected_", treatment_row$dimension_key, "_va")
    y <- outcome_row$outcome
    needed <- c(y, d, z, expected, "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU")
    model_data <- iv_data[complete.cases(iv_data[, ..needed])]
    controls <- c("factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to", "factor(GEN_ALU)", "factor(EDAD_ALU)", expected)
    rhs <- paste(controls, collapse = " + ")
    iv_model <- feols(as.formula(paste0(y, " ~ ", rhs, " | 0 | ", d, " ~ ", z)), data = model_data, vcov = "hetero", notes = FALSE)
    fs_model <- feols(as.formula(paste0(d, " ~ ", z, " + ", rhs)), data = model_data, vcov = "hetero", notes = FALSE)
    iv_term <- extract_term(iv_model, d)
    fs_term <- extract_term(fs_model, z)
    beta <- coef(iv_model)[[iv_term]]
    beta_se <- se(iv_model)[[iv_term]]
    fs_beta <- coef(fs_model)[[fs_term]]
    fs_se <- se(fs_model)[[fs_term]]
    data.table(
      outcome = outcome_row$dimension_key,
      outcome_label = outcome_row$label,
      treatment_va = treatment_row$dimension_key,
      treatment_label = treatment_row$label,
      outcome_mean = mean(model_data[[y]]),
      beta = beta,
      se = beta_se,
      p_value = 2 * pnorm(-abs(beta / beta_se)),
      n_obs = nobs(iv_model),
      treatment_sd = sd(model_data[[d]]),
      effect_per_treatment_sd = beta * sd(model_data[[d]]),
      first_stage = fs_beta,
      first_stage_f = (fs_beta / fs_se)^2
    )
  }), use.names = TRUE)
}), use.names = TRUE)

fwrite(results, file.path(output_dir, paste0("cross_outcome", analysis_suffix, "_lottery_iv.csv")))
fwrite(results, file.path(table_dir, paste0("cross_outcome", analysis_suffix, "_lottery_iv.csv")))
coefficient_matrix <- dcast(results, outcome_label ~ treatment_label, value.var = "beta")
fwrite(coefficient_matrix, file.path(output_dir, paste0("cross_outcome", analysis_suffix, "_lottery_iv_matrix.csv")))
fwrite(coefficient_matrix, file.path(table_dir, paste0("cross_outcome", analysis_suffix, "_lottery_iv_matrix.csv")))
print(results)
