suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(fixest)
  library(lfe)
  library(purrr)
  library(readr)
  library(tidyr)
})

parse_env_list <- function(var) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(trimws(value))) {
    return(character())
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

# ------------------------- Configuration -------------------------

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)
repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = getwd()
)

input_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
middle_school_controls_path <- file.path(
  data_wd,
  "data/clean/middle_school_controls/middle_school_controls.csv"
)
program_income_path <- Sys.getenv(
  "PROGRAM_INCOME_OUTCOMES_PATH",
  unset = file.path(
    repo_wd,
    "output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv"
  )
)
output_dir <- file.path(data_wd, "data/clean/school_rbd_observational_values")
output_path <- Sys.getenv(
  "SCHOOL_RBD_VALUES_OUTPUT_PATH",
  unset = file.path(output_dir, "school_rbd_observational_values.csv")
)
score_diagnostics_path <- Sys.getenv(
  "SCHOOL_RBD_SCORE_DIAGNOSTICS_OUTPUT_PATH",
  unset = file.path(output_dir, "score_scale_diagnostics_by_year.csv")
)
configured_outcome_filter <- parse_env_list("SCHOOL_VA_OUTCOMES")

school_var <- "most_time_RBD"
school_definition <- "most_time_RBD"
gender_var <- "GEN_ALU"
male_gender_code <- 1
female_gender_code <- 2
age_var <- "EDAD_ALU"
min_age <- 12
max_age <- 16
# EDAD_ALU is interpreted here as age in grade 8.

score_year_var <- "psu_year"
raw_score_outcomes <- c(
  "math_max",
  "leng_max",
  "leng_math_total",
  "hist_max",
  "scien_max",
  "math2_max"
)

# PSU-style scores in 2021-2022 are on the old 850-point frame, while later
# PAES-style scores use a 1000-point frame. The scale1000_* outcomes multiply
# old-frame scores by 1000 / 850 and leave 2023+ scores unchanged. For
# leng_math_total, this is equivalent to rescaling both components before
# summing, so the total is on an implied 2000-point two-test frame.
old_score_scale_years <- c(2021, 2022)
old_to_new_scale_factor <- 1000 / 850

field_vars <- c(
  m1 = "field_reclassified_m1",
  ml = "field_reclassified_ml"
)

field_indicator_stems <- c(
  "science",
  "social",
  "business",
  "law",
  "teaching",
  "humarts",
  "eng",
  "medicine",
  "health"
)

# Existing binary field indicators in univ_gr8_df.csv. The script treats
# missing values as non-enrollment when constructing enrollment outcomes.
field_indicator_vars <- as.vector(outer(
  paste0("f_", field_indicator_stems),
  c("_m1", "_ml"),
  paste0
))

# STEM enrollment definition for the existing binary specs. Edit this list if
# the STEM definition changes.
stem_indicator_vars <- list(
  m1 = c("f_science_m1", "f_eng_m1"),
  ml = c("f_science_ml", "f_eng_ml")
)

# Controlled VA is the expensive step. Keep this list narrow while iterating;
# raw means can still be computed for the broader configured outcome set.
controlled_value_added_outcomes <- c(
  "z_year_math_max",
  "z_year_leng_max",
  "z_year_leng_math_total",
  "stem_enrollment_m1",
  "log_program_income_clp_m1",
  "program_certified_years_m1",
  "inst_certified_years_m1"
)

# Main individual-level control set for controlled observational value-added.
# COD_COM_ALU is the student's comuna, not the school's comuna.
# income_decile_imputed is the observed SIMCE parent-survey income decile when
# available, otherwise a baseline-context median imputation documented in
# setup_md_codex/project_memory/decisions/.
baseline_score_vars <- c("z_sim_mat_4to", "z_sim_leng_4to")
baseline_score_poly_terms <- unlist(lapply(baseline_score_vars, function(var) {
  c(var, paste0("I(", var, "^", 2:3, ")"))
}), use.names = FALSE)
baseline_cpad_control_vars <- c(
  "father_educ_years_imputed",
  "mother_educ_years_imputed",
  "father_indigenous_imputed",
  "mother_indigenous_imputed",
  "sala_cuna_imputed",
  "jardin_imputed",
  "prekinder_imputed",
  "kinder_imputed"
)

middle_school_control_vars <- c(
  "z_gpa_middle_mean",
  "z_att_middle_mean"
)

middle_school_fixed_effect_vars <- c("most_time_RBD_middle")

control_terms <- c(
  "factor(cohort_gr8)",
  "factor(GEN_ALU)",
  "factor(EDAD_ALU)",
  "factor(COD_COM_ALU)",
  "income_decile_imputed",
  baseline_cpad_control_vars,
  middle_school_control_vars,
  "factor(middle_years_observed)",
  baseline_score_poly_terms
)

control_vars <- c(
  "cohort_gr8",
  "GEN_ALU",
  "EDAD_ALU",
  "COD_COM_ALU",
  "income_decile_imputed",
  baseline_cpad_control_vars,
  middle_school_control_vars,
  "middle_years_observed",
  "z_sim_mat_4to",
  "z_sim_leng_4to"
)

fixed_effect_vars <- c("school_rbd", middle_school_fixed_effect_vars)
school_value_added_se_vcov <- "iid"
school_value_added_se_bootstrap_reps <- as.integer(Sys.getenv(
  "SCHOOL_VA_SE_BOOTSTRAP_REPS",
  unset = "100"
))
if (is.na(school_value_added_se_bootstrap_reps) || school_value_added_se_bootstrap_reps <= 0) {
  stop("SCHOOL_VA_SE_BOOTSTRAP_REPS must be a positive integer.", call. = FALSE)
}
school_value_added_se_estimable_function <- Sys.getenv(
  "SCHOOL_VA_SE_EF",
  unset = "school_centered_student"
)

# Keep gender-specific and gender-gap VA on hold unless explicitly requested.
gender_gap_base_outcomes <- character()
gender_gap_control_terms <- control_terms[control_terms != "factor(GEN_ALU)"]
gender_gap_control_vars <- setdiff(control_vars, gender_var)
gender_gap_interacted_control_terms <- setdiff(gender_gap_control_terms, "factor(COD_COM_ALU)")
gender_gap_prefix <- "gender_gap"

low_count_threshold <- 20L

# ------------------------- Helpers -------------------------

stop_if_missing <- function(data, vars, label) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      label,
      " missing required columns: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

read_program_income_outcomes <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Program income outcome file does not exist: ",
      path,
      ". Run code/codex/mifuturo_matricula_income/03_construct_person_level_income_outcomes.R first.",
      call. = FALSE
    )
  }

  keep_cols <- c(
    "MRUN",
    "program_income_clp_m1",
    "log_program_income_clp_m1",
    "program_income_source_m1",
    "program_income_missing_m1"
  )
  header <- names(fread(path, nrows = 0, showProgress = FALSE))
  missing_cols <- setdiff(keep_cols, header)
  if (length(missing_cols) > 0) {
    stop(
      "Program income outcome file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- fread(path, select = keep_cols, na.strings = c("", "NA"), showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(MRUN = as.character(MRUN))

  if (anyDuplicated(out$MRUN) > 0) {
    stop("Program income outcome file is not unique at MRUN level.", call. = FALSE)
  }

  out
}

estimate_school_regression_se <- function(regression_data,
                                          outcome,
                                          controls,
                                          fixed_effect_vars,
                                          regression_counts,
                                          vcov_type = "iid",
                                          bootstrap_reps = school_value_added_se_bootstrap_reps,
                                          estimable_function = school_value_added_se_estimable_function) {
  if (vcov_type != "iid") {
    return(tibble(
      school_rbd = regression_counts$school_rbd,
      controlled_value_added_se = NA_real_,
      controlled_value_added_se_method = paste0(
        "lfe_getfe_school_rbd_",
        vcov_type,
        "_unsupported"
      )
    ))
  }

  se_formula <- as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(controls, collapse = " + "),
      " | ",
      paste(fixed_effect_vars, collapse = " + ")
    )
  )

  se_model <- tryCatch(
    {
      fit <- felm(se_formula, data = as.data.frame(regression_data))
      fit$call$formula <- se_formula
      fit
    },
    error = function(e) e
  )
  if (inherits(se_model, "error")) {
    return(tibble(
      school_rbd = regression_counts$school_rbd,
      controlled_value_added_se = NA_real_,
      controlled_value_added_se_method = "lfe_getfe_school_rbd_iid_failed"
    ))
  }

  if (estimable_function == "school_centered_student") {
    alpha_template <- getfe(se_model, se = FALSE, ef = "ref")
    school_rows <- which(alpha_template$fe == "school_rbd")
    school_ids <- as.character(alpha_template$idx[school_rows])
    weight_lookup <- regression_counts$n_students_regression
    names(weight_lookup) <- as.character(regression_counts$school_rbd)
    school_weights <- as.numeric(weight_lookup[school_ids])
    school_weights[is.na(school_weights)] <- 0
    if (sum(school_weights) <= 0) {
      school_weights <- rep(1, length(school_rows))
    }
    school_weights <- school_weights / sum(school_weights)

    school_centered_student_ef <- function(v, addnames) {
      school_values <- v[school_rows]
      out <- school_values - sum(school_weights * school_values)
      if (addnames) {
        names(out) <- paste0("school_rbd.", school_ids)
      }
      out
    }

    school_se_raw <- getfe(
      se_model,
      se = TRUE,
      ef = school_centered_student_ef,
      bN = bootstrap_reps
    )
    school_se <- tibble(
      term = rownames(school_se_raw),
      controlled_value_added_se = school_se_raw$se
    ) %>%
      transmute(
        school_rbd = suppressWarnings(as.numeric(sub("^school_rbd\\.", "", term))),
        controlled_value_added_se
      )
    se_method <- paste0(
      "lfe_getfe_school_rbd_centered_student_iid_bN",
      bootstrap_reps
    )
  } else {
    school_se <- getfe(
      se_model,
      se = TRUE,
      ef = estimable_function,
      bN = bootstrap_reps
    ) %>%
      filter(fe == "school_rbd") %>%
      transmute(
        school_rbd = suppressWarnings(as.numeric(as.character(idx))),
        controlled_value_added_se = se
      )
    se_method <- paste0(
      "lfe_getfe_school_rbd_iid_ef_",
      estimable_function,
      "_bN",
      bootstrap_reps
    )
  }

  regression_counts %>%
    select(school_rbd) %>%
    left_join(school_se, by = "school_rbd") %>%
    mutate(
      controlled_value_added_se = if_else(
        is.nan(controlled_value_added_se),
        NA_real_,
        controlled_value_added_se
      ),
      controlled_value_added_se_method = se_method
    )
}

standardize_gender_indicator <- function(x,
                                         male_code = male_gender_code,
                                         female_code = female_gender_code) {
  case_when(
    x == female_code ~ 1L,
    x == male_code ~ 0L,
    TRUE ~ NA_integer_
  )
}

score_summary <- function(data, outcome, value_var, score_scale, year_var) {
  data %>%
    filter(!is.na(.data[[value_var]])) %>%
    group_by(.data[[year_var]]) %>%
    summarise(
      outcome = outcome,
      score_scale = score_scale,
      n_students = n(),
      mean = mean(.data[[value_var]], na.rm = TRUE),
      sd = sd(.data[[value_var]], na.rm = TRUE),
      min = min(.data[[value_var]], na.rm = TRUE),
      p01 = quantile(.data[[value_var]], 0.01, na.rm = TRUE),
      p05 = quantile(.data[[value_var]], 0.05, na.rm = TRUE),
      p25 = quantile(.data[[value_var]], 0.25, na.rm = TRUE),
      median = median(.data[[value_var]], na.rm = TRUE),
      p75 = quantile(.data[[value_var]], 0.75, na.rm = TRUE),
      p95 = quantile(.data[[value_var]], 0.95, na.rm = TRUE),
      p99 = quantile(.data[[value_var]], 0.99, na.rm = TRUE),
      max = max(.data[[value_var]], na.rm = TRUE),
      share_gt_850 = mean(.data[[value_var]] > 850, na.rm = TRUE),
      share_gt_1000 = mean(.data[[value_var]] > 1000, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(score_year = all_of(year_var))
}

z_within_group <- function(x) {
  x_sd <- sd(x, na.rm = TRUE)
  if (is.na(x_sd) || x_sd == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / x_sd
}

prepare_score_outcomes <- function(data,
                                   raw_outcomes,
                                   year_var,
                                   old_scale_years,
                                   old_to_new_factor) {
  stop_if_missing(data, year_var, "Score outcome construction")
  present_outcomes <- intersect(raw_outcomes, names(data))

  data %>%
    mutate(
      across(
        all_of(present_outcomes),
        ~ ifelse(is.na(.x) | .x <= 0, NA_real_, as.numeric(.x))
      )
    ) -> data

  outcome_specs <- tibble(
    outcome = character(),
    outcome_family = character()
  )
  diagnostics <- tibble()

  for (outcome in present_outcomes) {
    scale1000_outcome <- paste0("scale1000_", outcome)
    z_year_outcome <- paste0("z_year_", outcome)

    data[[scale1000_outcome]] <- ifelse(
      !is.na(data[[year_var]]) & data[[year_var]] %in% old_scale_years,
      data[[outcome]] * old_to_new_factor,
      data[[outcome]]
    )

    data <- data %>%
      group_by(.data[[year_var]]) %>%
      mutate("{z_year_outcome}" := z_within_group(.data[[outcome]])) %>%
      ungroup()

    outcome_specs <- bind_rows(
      outcome_specs,
      tibble(
        outcome = c(outcome, scale1000_outcome, z_year_outcome),
        outcome_family = c("score_raw", "score_scale1000", "score_z_year")
      )
    )

    diagnostics <- bind_rows(
      diagnostics,
      score_summary(data, outcome, outcome, "raw", year_var),
      score_summary(data, outcome, scale1000_outcome, "scale1000", year_var),
      score_summary(data, outcome, z_year_outcome, "z_year", year_var)
    )
  }

  list(data = data, outcome_specs = outcome_specs, diagnostics = diagnostics)
}

add_field_outcomes <- function(data,
                               field_vars,
                               field_indicator_vars,
                               stem_indicator_vars) {
  stop_if_missing(
    data,
    c(unname(field_vars), field_indicator_vars, unlist(stem_indicator_vars)),
    "Field outcome construction"
  )

  data <- data %>%
    mutate(
      across(
        all_of(field_indicator_vars),
        ~ ifelse(is.na(.x), 0, as.numeric(.x))
      )
    )

  outcome_specs <- tibble(
    outcome = character(),
    outcome_family = character()
  )

  for (suffix in names(field_vars)) {
    field_var <- unname(field_vars[[suffix]])
    enrolled_outcome <- paste0("enrolled_", suffix)
    stem_outcome <- paste0("stem_enrollment_", suffix)

    data[[enrolled_outcome]] <- as.integer(!is.na(data[[field_var]]))
    data[[stem_outcome]] <- as.integer(
      rowSums(data[stem_indicator_vars[[suffix]]], na.rm = TRUE) > 0
    )

    outcome_specs <- bind_rows(
      outcome_specs,
      tibble(
        outcome = c(enrolled_outcome, stem_outcome),
        outcome_family = "enrollment"
      )
    )
  }

  outcome_specs <- bind_rows(
    outcome_specs,
    tibble(
      outcome = field_indicator_vars,
      outcome_family = "enrollment_field"
    )
  )

  list(data = data, outcome_specs = outcome_specs)
}

add_accreditation_outcomes <- function(data) {
  accreditation_vars <- c(
    "ACREDITADA_CARR_m1",
    "ACREDITADA_INST_m1",
    "ACRE_INST_ANIO_m1",
    "program_certified_years_m1",
    "institution_accredited_m1"
  )
  stop_if_missing(data, accreditation_vars, "Accreditation outcome construction")

  data <- data %>%
    mutate(
      observed_matricula_m1 = !is.na(ACREDITADA_CARR_m1) |
        !is.na(ACREDITADA_INST_m1) |
        !is.na(ACRE_INST_ANIO_m1),
      program_certified_years_m1 = case_when(
        is.na(program_certified_years_m1) & !observed_matricula_m1 ~ 0,
        TRUE ~ as.numeric(program_certified_years_m1)
      ),
      inst_certified_years_m1 = case_when(
        institution_accredited_m1 == 1L ~ as.numeric(ACRE_INST_ANIO_m1),
        institution_accredited_m1 == 0L ~ 0,
        is.na(institution_accredited_m1) & !observed_matricula_m1 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-observed_matricula_m1)

  list(
    data = data,
    outcome_specs = tibble(
      outcome = c("program_certified_years_m1", "inst_certified_years_m1"),
      outcome_family = "accreditation"
    )
  )
}

estimate_school_value_added <- function(data,
                                        outcome,
                                        controls,
                                        control_vars,
                                        fixed_effect_vars = "school_rbd",
                                        school_se_vcov = school_value_added_se_vcov,
                                        analysis_sample = "All") {
  regression_data <- data %>%
    select(all_of(fixed_effect_vars), all_of(outcome), all_of(control_vars)) %>%
    filter(
      !is.na(school_rbd),
      !is.na(.data[[outcome]])
    ) %>%
    drop_na(all_of(c(control_vars, fixed_effect_vars)))

  if (
    nrow(regression_data) == 0 ||
      n_distinct(regression_data$school_rbd) < 2
  ) {
    return(tibble(
      school_rbd = numeric(),
      analysis_sample = analysis_sample,
      outcome = outcome,
      controlled_value_added = numeric(),
      controlled_value_added_se = numeric(),
      controlled_value_added_resid_sd = numeric(),
      controlled_value_added_se_method = character(),
      controlled_value_added_centered = numeric(),
      controlled_adjusted_mean = numeric(),
      n_students_regression = integer(),
      n_students_regression_total = integer()
    ))
  }

  model_formula <- as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(controls, collapse = " + "),
      " | ",
      paste(fixed_effect_vars, collapse = " + ")
    )
  )

  model <- feols(model_formula, data = regression_data, notes = FALSE)
  school_fe <- fixef(model)[["school_rbd"]]
  model_obs <- obs(model)
  regression_used_data <- regression_data[model_obs, , drop = FALSE]
  regression_used_data$.model_resid <- as.numeric(residuals(model))

  regression_counts <- regression_used_data %>%
    group_by(school_rbd) %>%
    summarise(
      n_students_regression = n(),
      controlled_value_added_resid_sd = sd(.model_resid, na.rm = TRUE),
      .groups = "drop"
    )
  model_df <- df.residual(model)
  if (is.null(model_df) || is.na(model_df) || model_df <= 0) {
    model_df <- nrow(regression_used_data)
  }
  model_resid_sd <- sqrt(sum(regression_used_data$.model_resid^2, na.rm = TRUE) / model_df)
  school_regression_se <- estimate_school_regression_se(
    regression_data = regression_used_data,
    outcome = outcome,
    controls = controls,
    fixed_effect_vars = fixed_effect_vars,
    regression_counts = regression_counts,
    vcov_type = school_se_vcov
  )

  fe_values <- tibble(
    school_rbd = as.numeric(names(school_fe)),
    analysis_sample = analysis_sample,
    outcome = outcome,
    controlled_value_added = as.numeric(school_fe),
    n_students_regression_total = nobs(model)
  ) %>%
    left_join(regression_counts, by = "school_rbd") %>%
    left_join(school_regression_se, by = "school_rbd") %>%
    mutate(
      controlled_value_added_resid_sd = if_else(
        is.na(controlled_value_added_resid_sd),
        model_resid_sd,
        controlled_value_added_resid_sd
      )
    )

  fe_weighted_mean <- weighted.mean(
    fe_values$controlled_value_added,
    fe_values$n_students_regression,
    na.rm = TRUE
  )
  fe_school_mean <- mean(fe_values$controlled_value_added, na.rm = TRUE)
  outcome_mean <- mean(regression_used_data[[outcome]], na.rm = TRUE)
  raw_outcome_mean <- mean(data[[outcome]], na.rm = TRUE)

  fe_values %>%
    mutate(
      controlled_value_added_centered = controlled_value_added - fe_school_mean,
      controlled_adjusted_mean = raw_outcome_mean + controlled_value_added_centered,
      controlled_value_added_centered_student = controlled_value_added - fe_weighted_mean,
      controlled_adjusted_mean_student = outcome_mean + controlled_value_added_centered_student
    )
}

compute_raw_gender_gap <- function(data,
                                   outcome,
                                   female_var = "female_indicator",
                                   outcome_name = paste(gender_gap_prefix, outcome, sep = "__")) {
  data %>%
    filter(!is.na(.data[[female_var]])) %>%
    group_by(school_rbd) %>%
    summarise(
      girls_mean = mean_or_na(ifelse(.data[[female_var]] == 1L, .data[[outcome]], NA_real_)),
      boys_mean = mean_or_na(ifelse(.data[[female_var]] == 0L, .data[[outcome]], NA_real_)),
      n_girls_outcome = sum(.data[[female_var]] == 1L & !is.na(.data[[outcome]])),
      n_boys_outcome = sum(.data[[female_var]] == 0L & !is.na(.data[[outcome]])),
      .groups = "drop"
    ) %>%
    transmute(
      school_rbd,
      outcome = outcome_name,
      raw_mean = girls_mean - boys_mean,
      n_students_outcome = n_girls_outcome + n_boys_outcome,
      n_girls_outcome,
      n_boys_outcome
    ) %>%
    group_by(outcome) %>%
    mutate(
      raw_mean_centered = raw_mean - mean(raw_mean, na.rm = TRUE),
      raw_mean_centered_student = raw_mean - weighted.mean(raw_mean, n_students_outcome, na.rm = TRUE)
    ) %>%
    ungroup()
}

estimate_gender_gap_value_added <- function(data,
                                            outcome,
                                            controls,
                                            interacted_controls,
                                            control_vars,
                                            female_var = "female_indicator",
                                            outcome_name = paste(gender_gap_prefix, outcome, sep = "__")) {
  regression_data <- data %>%
    select(school_rbd, all_of(outcome), all_of(control_vars), all_of(female_var)) %>%
    filter(
      !is.na(school_rbd),
      !is.na(.data[[outcome]]),
      !is.na(.data[[female_var]])
    ) %>%
    drop_na(all_of(control_vars)) %>%
    mutate(
      school_gender_fe = paste0(
        school_rbd,
        "__",
        ifelse(.data[[female_var]] == 1L, "girl", "boy")
      )
    )

  if (
    nrow(regression_data) == 0 ||
      n_distinct(regression_data$school_gender_fe) < 2
  ) {
    return(tibble(
      school_rbd = numeric(),
      outcome = outcome_name,
      controlled_value_added = numeric(),
      controlled_value_added_centered = numeric(),
      controlled_adjusted_mean = numeric(),
      controlled_value_added_centered_student = numeric(),
      controlled_adjusted_mean_student = numeric(),
      n_students_regression = integer(),
      n_students_regression_total = integer(),
      n_girls_regression = integer(),
      n_boys_regression = integer()
    ))
  }

  controls_rhs <- paste(controls, collapse = " + ")
  interacted_rhs <- paste(interacted_controls, collapse = " + ")
  model_formula <- as.formula(
    paste0(
      outcome,
      " ~ ",
      controls_rhs,
      if (nzchar(interacted_rhs)) {
        paste0(
          " + ",
          female_var,
          ":(",
          interacted_rhs,
          ")"
        )
      } else {
        ""
      },
      " | school_gender_fe"
    )
  )

  model <- feols(model_formula, data = regression_data, notes = FALSE)
  school_gender_effects <- fixef(model)[["school_gender_fe"]]

  regression_counts <- regression_data %>%
    group_by(school_rbd) %>%
    summarise(
      n_girls_regression = sum(.data[[female_var]] == 1L),
      n_boys_regression = sum(.data[[female_var]] == 0L),
      .groups = "drop"
    ) %>%
    mutate(n_students_regression = n_girls_regression + n_boys_regression)

  fe_values <- tibble(
    school_gender_fe = names(school_gender_effects),
    school_gender_effect = as.numeric(school_gender_effects)
  ) %>%
    separate(
      school_gender_fe,
      into = c("school_rbd", "gender_group"),
      sep = "__",
      remove = FALSE
    ) %>%
    mutate(school_rbd = as.numeric(school_rbd)) %>%
    select(-school_gender_fe) %>%
    pivot_wider(
      names_from = gender_group,
      values_from = school_gender_effect,
      values_fn = mean
    ) %>%
    mutate(
      outcome = outcome_name,
      controlled_value_added = .data[["girl"]] - .data[["boy"]],
      n_students_regression_total = nobs(model)
    ) %>%
    left_join(regression_counts, by = "school_rbd") %>%
    select(
      school_rbd,
      outcome,
      controlled_value_added,
      n_students_regression,
      n_students_regression_total,
      n_girls_regression,
      n_boys_regression
    )

  gap_weighted_mean <- weighted.mean(
    fe_values$controlled_value_added,
    fe_values$n_students_regression,
    na.rm = TRUE
  )
  gap_school_mean <- mean(fe_values$controlled_value_added, na.rm = TRUE)

  fe_values %>%
    mutate(
      controlled_value_added_centered = controlled_value_added - gap_school_mean,
      controlled_adjusted_mean = controlled_value_added,
      controlled_value_added_centered_student = controlled_value_added - gap_weighted_mean,
      controlled_adjusted_mean_student = controlled_value_added
    )
}

# ------------------------- Load and prepare data -------------------------

if (!file.exists(input_path)) {
  stop("Input file does not exist: ", input_path, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

input_cols <- unique(c(
  "MRUN",
  school_var,
  gender_var,
  age_var,
  score_year_var,
  raw_score_outcomes,
  unname(field_vars),
  field_indicator_vars,
  "ACREDITADA_CARR_m1",
  "ACREDITADA_INST_m1",
  "ACRE_INST_ANIO_m1",
  "program_certified_years_m1",
  "institution_accredited_m1",
  setdiff(control_vars, c(middle_school_control_vars, "middle_years_observed"))
))
available_cols <- names(fread(input_path, nrows = 0, showProgress = FALSE))
input_cols <- intersect(input_cols, available_cols)

df <- fread(
  input_path,
  select = input_cols,
  na.strings = c("", "NA"),
  showProgress = TRUE
) %>%
  as_tibble()

program_income_outcomes <- read_program_income_outcomes(program_income_path)
df <- df %>%
  mutate(MRUN = as.character(MRUN)) %>%
  left_join(program_income_outcomes, by = "MRUN")

if (!file.exists(middle_school_controls_path)) {
  stop(
    "Middle-school controls file does not exist: ",
    middle_school_controls_path,
    call. = FALSE
  )
}

middle_school_controls <- read_csv(
  middle_school_controls_path,
  show_col_types = FALSE
) %>%
  mutate(MRUN = as.character(MRUN)) %>%
  select(
    MRUN,
    most_time_RBD_middle,
    middle_years_observed,
    z_gpa_middle_mean,
    z_att_middle_mean
  )

df <- df %>%
  mutate(MRUN = as.character(MRUN)) %>%
  left_join(middle_school_controls, by = "MRUN")

stop_if_missing(
  df,
  c(school_var, control_vars, middle_school_fixed_effect_vars, age_var),
  "Main input"
)

student_id_var <- if ("mrun" %in% names(df)) {
  "mrun"
} else if ("MRUN" %in% names(df)) {
  "MRUN"
} else {
  NA_character_
}

if (!is.na(student_id_var) && anyDuplicated(df[[student_id_var]]) > 0) {
  stop(
    "Input file is not unique at the student level for ",
    student_id_var,
    ". This task expects one row per student.",
    call. = FALSE
  )
}

analytic <- df %>%
  mutate(school_rbd = as.numeric(.data[[school_var]])) %>%
  filter(
    !is.na(school_rbd),
    school_rbd > 0,
    !is.na(.data[[age_var]]),
    .data[[age_var]] >= min_age,
    .data[[age_var]] <= max_age
  )

analytic <- analytic %>%
  mutate(female_indicator = standardize_gender_indicator(.data[[gender_var]]))

score_result <- prepare_score_outcomes(
  analytic,
  raw_score_outcomes,
  score_year_var,
  old_score_scale_years,
  old_to_new_scale_factor
)
analytic <- score_result$data
write_csv(score_result$diagnostics, score_diagnostics_path)

analytic <- analytic %>%
  mutate(admission_exam_taker = !is.na(math_max) | !is.na(leng_max)) %>%
  filter(admission_exam_taker)

field_result <- add_field_outcomes(
  analytic,
  field_vars,
  field_indicator_vars,
  stem_indicator_vars
)
analytic <- field_result$data

accreditation_result <- add_accreditation_outcomes(analytic)
analytic <- accreditation_result$data

program_income_specs <- tibble(
  outcome = intersect(
    c("program_income_clp_m1", "log_program_income_clp_m1"),
    names(analytic)
  ),
  outcome_family = "program_income"
)

outcome_specs <- bind_rows(
  score_result$outcome_specs,
  field_result$outcome_specs,
  accreditation_result$outcome_specs,
  program_income_specs
) %>%
  distinct(outcome, .keep_all = TRUE)

if (length(configured_outcome_filter) > 0) {
  missing_filtered_outcomes <- setdiff(configured_outcome_filter, outcome_specs$outcome)
  if (length(missing_filtered_outcomes) > 0) {
    stop(
      "SCHOOL_VA_OUTCOMES includes outcomes not found in the analytic data: ",
      paste(missing_filtered_outcomes, collapse = ", "),
      call. = FALSE
    )
  }
  outcome_specs <- outcome_specs %>%
    filter(outcome %in% configured_outcome_filter)
}

if (nrow(outcome_specs) == 0) {
  stop("No configured outcomes were found in the input data.", call. = FALSE)
}

school_counts <- analytic %>%
  count(school_rbd, name = "n_students_school")

# ------------------------- Raw school means -------------------------

analysis_samples <- tibble(
  analysis_sample = "All",
  gender_code = NA_real_
)

sample_data <- function(data, gender_code) {
  if (is.na(gender_code)) {
    return(data)
  }
  data %>% filter(.data[[gender_var]] == gender_code)
}

sample_control_terms <- function(analysis_sample) {
  if (analysis_sample == "All") {
    return(control_terms)
  }
  setdiff(control_terms, "factor(GEN_ALU)")
}

sample_control_vars <- function(analysis_sample) {
  if (analysis_sample == "All") {
    return(control_vars)
  }
  setdiff(control_vars, gender_var)
}

school_counts_by_sample <- pmap_dfr(
  analysis_samples,
  function(analysis_sample, gender_code) {
    sample_data(analytic, gender_code) %>%
      count(school_rbd, name = "n_students_school") %>%
      mutate(analysis_sample = analysis_sample)
  }
)

raw_values <- pmap_dfr(analysis_samples, function(analysis_sample, gender_code) {
  sample_analytic <- sample_data(analytic, gender_code)

  map_dfr(outcome_specs$outcome, function(outcome_name) {
    sample_analytic %>%
      group_by(school_rbd) %>%
      summarise(
        raw_mean = mean_or_na(.data[[outcome_name]]),
        n_students_outcome = sum(!is.na(.data[[outcome_name]])),
        .groups = "drop"
      ) %>%
      mutate(
        analysis_sample = analysis_sample,
        outcome = outcome_name
      )
  })
}) %>%
  group_by(analysis_sample, outcome) %>%
  mutate(
    raw_mean_centered = raw_mean - mean(raw_mean, na.rm = TRUE),
    raw_mean_centered_student = raw_mean - weighted.mean(raw_mean, n_students_outcome, na.rm = TRUE)
  ) %>%
  ungroup()

# ------------------------- Controlled school value-added -------------------------

controlled_outcomes <- intersect(controlled_value_added_outcomes, outcome_specs$outcome)

if (length(controlled_outcomes) == 0) {
  stop("No configured controlled VA outcomes were found.", call. = FALSE)
}

controlled_values <- map_dfr(controlled_outcomes, function(outcome_name) {
  pmap_dfr(analysis_samples, function(analysis_sample, gender_code) {
    estimate_school_value_added(
      data = sample_data(analytic, gender_code),
      outcome = outcome_name,
      controls = sample_control_terms(analysis_sample),
      control_vars = sample_control_vars(analysis_sample),
      fixed_effect_vars = fixed_effect_vars,
      analysis_sample = analysis_sample
    )
  })
})

# ------------------------- Gender-gap school values -------------------------

gender_gap_outcomes <- intersect(gender_gap_base_outcomes, names(analytic))

if (length(gender_gap_outcomes) > 0) {
  gender_gap_specs <- tibble(
    outcome = paste(gender_gap_prefix, gender_gap_outcomes, sep = "__"),
    outcome_family = "gender_gap",
    base_outcome = gender_gap_outcomes
  )

  gender_gap_raw_values <- map_dfr(gender_gap_outcomes, function(outcome_name) {
    compute_raw_gender_gap(
      data = analytic,
      outcome = outcome_name
    )
  })

  gender_gap_controlled_values <- map_dfr(gender_gap_outcomes, function(outcome_name) {
    estimate_gender_gap_value_added(
      data = analytic,
      outcome = outcome_name,
      controls = gender_gap_control_terms,
      interacted_controls = gender_gap_interacted_control_terms,
      control_vars = gender_gap_control_vars
    )
  })

  gender_gap_final_values <- gender_gap_raw_values %>%
    left_join(gender_gap_controlled_values, by = c("school_rbd", "outcome")) %>%
    left_join(gender_gap_specs, by = "outcome") %>%
    left_join(school_counts, by = "school_rbd") %>%
    mutate(
      analysis_sample = "All",
      control_set = paste(gender_gap_control_terms, collapse = " + "),
      fixed_effect_set = "school_gender_fe",
      school_definition = school_definition,
      low_outcome_count = n_students_outcome < low_count_threshold,
      low_gender_count = pmin(n_girls_outcome, n_boys_outcome) < low_count_threshold,
      missing_controlled_value = is.na(controlled_value_added)
    )
} else {
  gender_gap_final_values <- tibble()
}

# ------------------------- Final output -------------------------

base_final_values <- raw_values %>%
  left_join(controlled_values, by = c("school_rbd", "analysis_sample", "outcome")) %>%
  left_join(outcome_specs, by = "outcome") %>%
  left_join(school_counts_by_sample, by = c("school_rbd", "analysis_sample")) %>%
  mutate(
    control_set = case_when(
      analysis_sample == "All" ~ paste(control_terms, collapse = " + "),
      TRUE ~ paste(setdiff(control_terms, "factor(GEN_ALU)"), collapse = " + ")
    ),
    fixed_effect_set = paste(fixed_effect_vars, collapse = " + "),
    school_definition = school_definition,
    base_outcome = outcome,
    n_girls_outcome = NA_integer_,
    n_boys_outcome = NA_integer_,
    n_girls_regression = NA_integer_,
    n_boys_regression = NA_integer_,
    low_outcome_count = n_students_outcome < low_count_threshold,
    low_gender_count = NA,
    missing_controlled_value = is.na(controlled_value_added)
  )

final_values <- bind_rows(base_final_values, gender_gap_final_values) %>%
  select(
    school_rbd,
    analysis_sample,
    outcome,
    outcome_family,
    base_outcome,
    raw_mean,
    raw_mean_centered,
    raw_mean_centered_student,
    controlled_value_added,
    controlled_value_added_se,
    controlled_value_added_resid_sd,
    controlled_value_added_se_method,
    controlled_value_added_centered,
    controlled_adjusted_mean,
    controlled_value_added_centered_student,
    controlled_adjusted_mean_student,
    n_students_school,
    n_students_outcome,
    n_students_regression,
    n_students_regression_total,
    n_girls_outcome,
    n_boys_outcome,
    n_girls_regression,
    n_boys_regression,
    control_set,
    fixed_effect_set,
    school_definition,
    low_outcome_count,
    low_gender_count,
    missing_controlled_value
  ) %>%
  arrange(outcome_family, outcome, analysis_sample, school_rbd)

if (anyDuplicated(final_values[c("school_rbd", "analysis_sample", "outcome")]) > 0) {
  stop("Final output has duplicate school_rbd-analysis_sample-outcome rows.", call. = FALSE)
}

write_csv(final_values, output_path)

message("Wrote: ", output_path)
message("Rows: ", nrow(final_values))
message("Outcomes: ", n_distinct(final_values$outcome))
message("Schools: ", n_distinct(final_values$school_rbd))
message("Analysis samples: ", paste(unique(final_values$analysis_sample), collapse = ", "))
