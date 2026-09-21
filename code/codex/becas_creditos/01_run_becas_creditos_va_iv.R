###############################################################################
# Benefits and credit outcomes: school value added and lottery IV
#
# Outcomes are defined from any observed record in the 2020--2025 releases:
#   any_postulacion: any FUAS application
#   any_asignacion: any benefit/credit assignment
#   grat_asignacion: any assignment with BENEFICIO_BECA_FSCU == GRATUIDAD
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(fixest)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean_dir <- file.path(data_wd, "data", "clean")
prob_dir <- file.path(clean_dir, "DA_probs")
middle_controls_path <- file.path(clean_dir, "middle_school_controls", "middle_school_controls.csv")
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
postulaciones_path <- file.path(clean_dir, "postulaciones.dta")
asignaciones_path <- file.path(clean_dir, "asignaciones.dta")
output_dir <- file.path(clean_dir, "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

outcomes <- c("any_postulacion", "any_asignacion", "grat_asignacion")
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

key_from_numeric <- function(x) {
  as.character(as.numeric(x))
}

extract_term <- function(model, term) {
  available <- names(coef(model))
  if (term %in% available) return(term)
  fitted_term <- paste0("fit_", term)
  if (fitted_term %in% available) return(fitted_term)
  stop("Missing coefficient: ", term, call. = FALSE)
}

message("Reading grade-8 universe and controls.")
universe_columns <- c(
  "MRUN", "student_id", "cohort_gr8", "sae_proceso", "timely_sae",
  "rbd_treated_1R", "most_time_RBD", "GEN_ALU", "EDAD_ALU",
  "z_sim_mat_4to", "z_sim_leng_4to", "COD_COM_ALU", "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed"
)
universe <- fread(universe_path, select = universe_columns, na.strings = c("", "NA"))
universe[, mrun_key := key_from_numeric(MRUN)]
universe[, `:=`(
  student_id = as.numeric(student_id),
  sae_proceso = as.integer(sae_proceso),
  most_time_RBD = as.numeric(most_time_RBD),
  rbd_treated_1R = as.numeric(rbd_treated_1R)
)]

middle_controls <- fread(
  middle_controls_path,
  select = c("MRUN", "most_time_RBD_middle", "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean"),
  na.strings = c("", "NA")
)
middle_controls[, mrun_key := key_from_numeric(MRUN)]
middle_controls[, MRUN := NULL]
universe <- merge(universe, middle_controls, by = "mrun_key", all.x = TRUE, sort = FALSE)
universe[, most_time_RBD_middle := as.numeric(most_time_RBD_middle)]

message("Constructing benefits outcomes from all 2020--2025 records.")
post_ids <- unique(key_from_numeric(read_dta(postulaciones_path, col_select = "mrun")$mrun))
assignment_source <- as.data.table(read_dta(
  asignaciones_path,
  col_select = c("mrun", "beneficio_beca_fscu")
))
assignment_source[, mrun_key := key_from_numeric(mrun)]
assignment_ids <- unique(assignment_source$mrun_key)
gratuidad_ids <- unique(assignment_source[
  toupper(trimws(beneficio_beca_fscu)) == "GRATUIDAD", mrun_key
])

outcome_lookup <- data.table(mrun_key = unique(universe$mrun_key))
outcome_lookup[, `:=`(
  any_postulacion = as.integer(mrun_key %chin% post_ids),
  any_asignacion = as.integer(mrun_key %chin% assignment_ids),
  grat_asignacion = as.integer(mrun_key %chin% gratuidad_ids)
)]
fwrite(outcome_lookup, file.path(output_dir, "becas_creditos_outcomes.csv"))

universe <- merge(universe, outcome_lookup, by = "mrun_key", all.x = TRUE, sort = FALSE)

analytic <- universe[
  !is.na(most_time_RBD) & most_time_RBD > 0 &
    !is.na(most_time_RBD_middle) &
    !is.na(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16
]

message("Estimating adjusted school value added for benefits outcomes.")
setFixest_nthreads(0)
va_results <- rbindlist(lapply(outcomes, function(outcome_name) {
  needed <- c(outcome_name, control_vars, "most_time_RBD", "most_time_RBD_middle")
  model_data <- analytic[complete.cases(analytic[, ..needed])]
  model_formula <- as.formula(paste0(
    outcome_name, " ~ ", paste(va_controls, collapse = " + "),
    " | most_time_RBD + most_time_RBD_middle"
  ))
  model <- feols(model_formula, data = model_data, notes = FALSE)
  effects <- fixef(model)[["most_time_RBD"]]
  school_values <- data.table(
    school_rbd = as.numeric(names(effects)),
    controlled_value_added = as.numeric(effects)
  )
  counts <- model_data[, .(n_students_regression = .N), by = .(school_rbd = most_time_RBD)]
  school_values <- merge(school_values, counts, by = "school_rbd", all.x = TRUE)
  school_values[, controlled_value_added_centered_student :=
    controlled_value_added - weighted.mean(controlled_value_added, n_students_regression)
  ]
  raw_values <- model_data[, .(
    raw_mean = mean(get(outcome_name)),
    n_students_outcome = .N
  ), by = .(school_rbd = most_time_RBD)]
  school_values <- merge(school_values, raw_values, by = "school_rbd", all.x = TRUE)
  school_values[, `:=`(
    outcome = outcome_name,
    n_students_regression_total = nobs(model),
    outcome_mean_regression = mean(model_data[[outcome_name]])
  )]
  school_values
}), use.names = TRUE, fill = TRUE)
setcolorder(va_results, c(
  "school_rbd", "outcome", "raw_mean", "controlled_value_added",
  "controlled_value_added_centered_student", "n_students_outcome",
  "n_students_regression", "n_students_regression_total", "outcome_mean_regression"
))
fwrite(va_results, file.path(output_dir, "becas_creditos_school_va.csv"))

message("Computing expected school values from lottery probabilities.")
value_wide <- dcast(
  va_results,
  school_rbd ~ outcome,
  value.var = "controlled_value_added_centered_student"
)
setnames(value_wide, outcomes, paste0("school_value_", outcomes))
probabilities <- rbindlist(lapply(2018:2021, function(year) {
  source <- fread(file.path(prob_dir, paste0("DA_probs_", year, ".csv")),
                  select = c("student_id", "school_id", "prob"))
  source[, `:=`(
    student_id = as.numeric(student_id),
    sae_proceso = as.integer(year),
    rbd_prob = as.numeric(sub("_.*$", "", school_id))
  )]
  source[, .(student_id, sae_proceso, rbd_prob, prob)]
}), use.names = TRUE)
probabilities <- merge(probabilities, value_wide, by.x = "rbd_prob", by.y = "school_rbd", all.x = TRUE, sort = FALSE)
prob_summary <- probabilities[, .(
  any_risk = as.integer(max(prob, na.rm = TRUE) < 1),
  total_probability_mass = sum(prob, na.rm = TRUE)
), by = .(student_id, sae_proceso)]
for (outcome_name in outcomes) {
  value_col <- paste0("school_value_", outcome_name)
  expected <- probabilities[, .(
    expected_value = sum(prob * fifelse(is.na(get(value_col)), 0, get(value_col)), na.rm = TRUE)
  ), by = .(student_id, sae_proceso)]
  setnames(expected, "expected_value", paste0("expected_", outcome_name))
  prob_summary <- merge(prob_summary, expected, by = c("student_id", "sae_proceso"), all.x = TRUE, sort = FALSE)
}

iv_data <- merge(universe, prob_summary, by = c("student_id", "sae_proceso"), all = FALSE, sort = FALSE)
iv_data <- iv_data[timely_sae == 1L & any_risk == 1L]
attended_values <- copy(value_wide)
setnames(attended_values, paste0("school_value_", outcomes), paste0("d_", outcomes))
iv_data <- merge(iv_data, attended_values, by.x = "most_time_RBD", by.y = "school_rbd", all.x = TRUE, sort = FALSE)
offered_values <- copy(value_wide)
setnames(offered_values, paste0("school_value_", outcomes), paste0("z_", outcomes))
iv_data <- merge(iv_data, offered_values, by.x = "rbd_treated_1R", by.y = "school_rbd", all.x = TRUE, sort = FALSE)

message("Running expected-value lottery IV for benefits outcomes.")
iv_results <- rbindlist(lapply(outcomes, function(outcome_name) {
  d <- paste0("d_", outcome_name)
  z <- paste0("z_", outcome_name)
  expected <- paste0("expected_", outcome_name)
  needed <- c(outcome_name, d, z, expected, "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU")
  model_data <- iv_data[complete.cases(iv_data[, ..needed])]
  controls <- c(
    "factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to",
    "factor(GEN_ALU)", "factor(EDAD_ALU)", expected
  )
  control_rhs <- paste(controls, collapse = " + ")
  iv_model <- feols(
    as.formula(paste0(outcome_name, " ~ ", control_rhs, " | 0 | ", d, " ~ ", z)),
    data = model_data,
    vcov = "hetero",
    notes = FALSE
  )
  fs_model <- feols(
    as.formula(paste0(d, " ~ ", z, " + ", control_rhs)),
    data = model_data,
    vcov = "hetero",
    notes = FALSE
  )
  iv_term <- extract_term(iv_model, d)
  fs_term <- extract_term(fs_model, z)
  beta <- coef(iv_model)[[iv_term]]
  beta_se <- se(iv_model)[[iv_term]]
  first_stage <- coef(fs_model)[[fs_term]]
  first_stage_se <- se(fs_model)[[fs_term]]
  data.table(
    outcome = outcome_name,
    outcome_mean = mean(model_data[[outcome_name]]),
    beta = beta,
    se = beta_se,
    p_value = 2 * pnorm(-abs(beta / beta_se)),
    n_obs = nobs(iv_model),
    first_stage = first_stage,
    first_stage_se = first_stage_se,
    first_stage_f = (first_stage / first_stage_se)^2
  )
}), use.names = TRUE)
fwrite(iv_results, file.path(output_dir, "becas_creditos_lottery_iv_results.csv"))
fwrite(iv_results, file.path(table_dir, "becas_creditos_lottery_iv_results.csv"))

universe_means <- universe[, lapply(.SD, mean), .SDcols = outcomes]
universe_means[, sample := "grade8_universe"]
iv_means <- iv_data[, lapply(.SD, mean), .SDcols = outcomes]
iv_means[, sample := "lottery_iv_sample"]
summary <- rbindlist(list(universe_means, iv_means), fill = TRUE)
fwrite(summary, file.path(output_dir, "becas_creditos_outcome_means.csv"))

print(iv_results)
message("Wrote outcomes, school VA, IV results, and outcome means to: ", output_dir)
