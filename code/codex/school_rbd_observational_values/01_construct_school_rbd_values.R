suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(purrr)
  library(readr)
  library(tidyr)
})

# ------------------------- Configuration -------------------------

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

input_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
output_dir <- file.path(data_wd, "data/clean/school_rbd_observational_values")
output_path <- file.path(output_dir, "school_rbd_observational_values.csv")
score_diagnostics_path <- file.path(output_dir, "score_scale_diagnostics_by_year.csv")

school_var <- "most_time_RBD"
school_definition <- "most_time_RBD"
gender_var <- "GEN_ALU"
male_gender_code <- 1
female_gender_code <- 2

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

# Main individual-level control set for controlled observational value-added.
# COD_COM_ALU is the student's comuna, not the school's comuna.
# income_decile is intentionally excluded for now because it is currently messy.
control_terms <- c(
  "factor(cohort_gr8)",
  "factor(GEN_ALU)",
  "factor(COD_COM_ALU)",
  "z_sim_mat_4to",
  "z_sim_leng_4to"
)

control_vars <- c(
  "cohort_gr8",
  "GEN_ALU",
  "COD_COM_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to"
)

gender_gap_base_outcomes <- c("z_year_math_max", "stem_enrollment_m1")
gender_gap_control_terms <- control_terms[control_terms != "factor(GEN_ALU)"]
gender_gap_control_vars <- setdiff(control_vars, gender_var)
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

estimate_school_value_added <- function(data, outcome, controls, control_vars) {
  regression_data <- data %>%
    select(school_rbd, all_of(outcome), all_of(control_vars)) %>%
    filter(
      !is.na(school_rbd),
      !is.na(.data[[outcome]])
    ) %>%
    drop_na(all_of(control_vars))

  if (
    nrow(regression_data) == 0 ||
      n_distinct(regression_data$school_rbd) < 2
  ) {
    return(tibble(
      school_rbd = numeric(),
      outcome = outcome,
      controlled_value_added = numeric(),
      controlled_value_added_centered = numeric(),
      controlled_adjusted_mean = numeric(),
      n_students_regression = integer(),
      n_students_regression_total = integer()
    ))
  }

  model_formula <- as.formula(
    paste0(outcome, " ~ ", paste(controls, collapse = " + "), " | school_rbd")
  )

  model <- feols(model_formula, data = regression_data, notes = FALSE)
  school_fe <- fixef(model)$school_rbd

  regression_counts <- regression_data %>%
    count(school_rbd, name = "n_students_regression")

  fe_values <- tibble(
    school_rbd = as.numeric(names(school_fe)),
    outcome = outcome,
    controlled_value_added = as.numeric(school_fe),
    n_students_regression_total = nobs(model)
  ) %>%
    left_join(regression_counts, by = "school_rbd")

  fe_weighted_mean <- weighted.mean(
    fe_values$controlled_value_added,
    fe_values$n_students_regression,
    na.rm = TRUE
  )
  fe_school_mean <- mean(fe_values$controlled_value_added, na.rm = TRUE)
  outcome_mean <- mean(regression_data[[outcome]], na.rm = TRUE)
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
  model_formula <- as.formula(
    paste0(
      outcome,
      " ~ ",
      controls_rhs,
      " + ",
      female_var,
      ":(",
      controls_rhs,
      ") | school_gender_fe"
    )
  )

  model <- feols(model_formula, data = regression_data, notes = FALSE)
  school_gender_fe <- fixef(model)$school_gender_fe

  regression_counts <- regression_data %>%
    group_by(school_rbd) %>%
    summarise(
      n_girls_regression = sum(.data[[female_var]] == 1L),
      n_boys_regression = sum(.data[[female_var]] == 0L),
      .groups = "drop"
    ) %>%
    mutate(n_students_regression = n_girls_regression + n_boys_regression)

  fe_values <- tibble(
    school_gender_fe = names(school_gender_fe),
    school_gender_effect = as.numeric(school_gender_fe)
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
      values_from = school_gender_effect
    ) %>%
    rename(
      girls_effect = girl,
      boys_effect = boy
    ) %>%
    mutate(
      outcome = outcome_name,
      controlled_value_added = girls_effect - boys_effect,
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

df <- read_csv(input_path, show_col_types = FALSE)

stop_if_missing(df, c(school_var, control_vars), "Main input")

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
  filter(!is.na(school_rbd), school_rbd > 0)

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

field_result <- add_field_outcomes(
  analytic,
  field_vars,
  field_indicator_vars,
  stem_indicator_vars
)
analytic <- field_result$data

outcome_specs <- bind_rows(score_result$outcome_specs, field_result$outcome_specs) %>%
  distinct(outcome, .keep_all = TRUE)

if (nrow(outcome_specs) == 0) {
  stop("No configured outcomes were found in the input data.", call. = FALSE)
}

school_counts <- analytic %>%
  count(school_rbd, name = "n_students_school")

# ------------------------- Raw school means -------------------------

raw_values <- map_dfr(outcome_specs$outcome, function(outcome_name) {
  analytic %>%
    group_by(school_rbd) %>%
    summarise(
      raw_mean = mean_or_na(.data[[outcome_name]]),
      n_students_outcome = sum(!is.na(.data[[outcome_name]])),
      .groups = "drop"
    ) %>%
    mutate(outcome = outcome_name)
}) %>%
  group_by(outcome) %>%
  mutate(
    raw_mean_centered = raw_mean - mean(raw_mean, na.rm = TRUE),
    raw_mean_centered_student = raw_mean - weighted.mean(raw_mean, n_students_outcome, na.rm = TRUE)
  ) %>%
  ungroup()

# ------------------------- Controlled school value-added -------------------------

controlled_values <- map_dfr(outcome_specs$outcome, function(outcome_name) {
  estimate_school_value_added(
    data = analytic,
    outcome = outcome_name,
    controls = control_terms,
    control_vars = control_vars
  )
})

# ------------------------- Gender-gap school values -------------------------

gender_gap_outcomes <- intersect(gender_gap_base_outcomes, names(analytic))

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
    control_vars = gender_gap_control_vars
  )
})

# ------------------------- Final output -------------------------

base_final_values <- raw_values %>%
  left_join(controlled_values, by = c("school_rbd", "outcome")) %>%
  left_join(outcome_specs, by = "outcome") %>%
  left_join(school_counts, by = "school_rbd") %>%
  mutate(
    control_set = paste(control_terms, collapse = " + "),
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

gender_gap_final_values <- gender_gap_raw_values %>%
  left_join(gender_gap_controlled_values, by = c("school_rbd", "outcome")) %>%
  left_join(gender_gap_specs, by = "outcome") %>%
  left_join(school_counts, by = "school_rbd") %>%
  mutate(
    control_set = paste(gender_gap_control_terms, collapse = " + "),
    school_definition = school_definition,
    low_outcome_count = n_students_outcome < low_count_threshold,
    low_gender_count = pmin(n_girls_outcome, n_boys_outcome) < low_count_threshold,
    missing_controlled_value = is.na(controlled_value_added)
  )

final_values <- bind_rows(base_final_values, gender_gap_final_values) %>%
  select(
    school_rbd,
    outcome,
    outcome_family,
    base_outcome,
    raw_mean,
    raw_mean_centered,
    raw_mean_centered_student,
    controlled_value_added,
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
    school_definition,
    low_outcome_count,
    low_gender_count,
    missing_controlled_value
  ) %>%
  arrange(outcome_family, outcome, school_rbd)

if (anyDuplicated(final_values[c("school_rbd", "outcome")]) > 0) {
  stop("Final output has duplicate school_rbd-outcome rows.", call. = FALSE)
}

write_csv(final_values, output_path)

message("Wrote: ", output_path)
message("Rows: ", nrow(final_values))
message("Outcomes: ", n_distinct(final_values$outcome))
message("Schools: ", n_distinct(final_values$school_rbd))
