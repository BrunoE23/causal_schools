suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# Construct RSS-style orthogonal school value-added metrics without modifying the
# existing school_rbd_observational_values or scalar-IV workflows.
#
# Main object:
#   math_va_s = adjusted school VA for z_year_math_max
#   stem_va_orth_math_s = adjusted STEM-enrollment VA residualized on math VA
#
# The projection coefficient uses cross-cohort products of school-cohort residual
# means, following the EB-free variance/covariance logic in Rose, Schellenberg,
# and Shem-Tov (2025).

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
middle_school_controls_path <- file.path(
  clean_dir,
  "middle_school_controls",
  "middle_school_controls.csv"
)
output_dir <- file.path(clean_dir, "orthogonal_school_va")

school_values_path <- file.path(
  clean_dir,
  "school_rbd_observational_values",
  "school_rbd_observational_values.csv"
)

school_va_output <- file.path(output_dir, "school_orthogonal_va_exam_takers.csv")
school_cohort_output <- file.path(output_dir, "school_cohort_residual_means_exam_takers.csv")
covariance_inputs_output <- file.path(output_dir, "orthogonal_va_covariance_inputs_exam_takers.csv")
diagnostics_output <- file.path(output_dir, "orthogonal_va_diagnostics_exam_takers.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

z_within_group <- function(x) {
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sigma
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  stats::cor(x[ok], y[ok])
}

stop_if_missing <- function(data_names, required, label) {
  missing_vars <- setdiff(required, data_names)
  if (length(missing_vars) > 0) {
    stop(
      label,
      " missing required columns: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
}

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

load_existing_pooled_va <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  needed_cols <- c(
    "school_rbd",
    "analysis_sample",
    "outcome",
    "controlled_value_added",
    "controlled_value_added_centered",
    "controlled_value_added_centered_student",
    "n_students_regression",
    "n_students_regression_total"
  )
  existing <- fread(
    path,
    select = needed_cols,
    na.strings = c("", "NA")
  )
  stop_if_missing(names(existing), needed_cols, "Existing school VA")
  existing <- existing[
    analysis_sample == "All" &
      outcome %chin% c("z_year_math_max", "stem_enrollment_m1")
  ]

  if (nrow(existing) == 0) {
    return(NULL)
  }

  existing
}

universe_cols <- c(
  "MRUN",
  "cohort_gr8",
  "GEN_ALU",
  "EDAD_ALU",
  "COD_COM_ALU",
  "most_time_RBD",
  "psu_year",
  "math_max",
  "leng_max",
  "f_science_m1",
  "f_eng_m1",
  "income_decile_imputed",
  baseline_cpad_control_vars,
  "z_sim_mat_4to",
  "z_sim_leng_4to"
)
middle_cols <- c(
  "MRUN",
  "cohort_gr8",
  "most_time_RBD_middle",
  "middle_years_observed",
  "z_gpa_middle_mean",
  "z_att_middle_mean"
)

message("Reading selected universe columns: ", universe_path)
universe <- fread(universe_path, select = universe_cols, na.strings = c("", "NA"))
stop_if_missing(names(universe), universe_cols, "Universe")

message("Reading selected middle-school controls: ", middle_school_controls_path)
middle_controls <- fread(
  middle_school_controls_path,
  select = middle_cols,
  na.strings = c("", "NA")
)
stop_if_missing(names(middle_controls), middle_cols, "Middle-school controls")

message("Merging controls and constructing outcomes.")
dt <- merge(
  universe,
  middle_controls,
  by = c("MRUN", "cohort_gr8"),
  all.x = TRUE,
  sort = FALSE
)
rm(universe, middle_controls)
gc(verbose = FALSE)

dt[, `:=`(
  school_rbd = as.numeric(most_time_RBD),
  cohort_gr8 = as.integer(cohort_gr8),
  GEN_ALU = as.integer(GEN_ALU),
  EDAD_ALU = as.integer(EDAD_ALU),
  COD_COM_ALU = as.integer(COD_COM_ALU),
  most_time_RBD_middle = as.numeric(most_time_RBD_middle),
  middle_years_observed = as.integer(middle_years_observed)
)]

dt[
  !is.na(psu_year) & !is.na(math_max) & math_max > 0,
  z_year_math_max := z_within_group(math_max),
  by = psu_year
]
dt[, admission_exam_taker := (
  !is.na(math_max) & math_max > 0
) | (
  !is.na(leng_max) & leng_max > 0
)]
dt <- dt[admission_exam_taker == TRUE]
dt[, stem_enrollment_m1 := as.integer(
  fifelse(is.na(f_science_m1), 0, as.numeric(f_science_m1)) == 1 |
    fifelse(is.na(f_eng_m1), 0, as.numeric(f_eng_m1)) == 1
)]

estimate_outcome_components <- function(data, outcome, outcome_label) {
  needed_vars <- unique(c(fixed_effect_vars, outcome, control_vars))
  regression_data <- data[
    !is.na(school_rbd) &
      !is.na(get(outcome)) &
      complete.cases(data[, ..needed_vars])
  ]

  if (nrow(regression_data) == 0 || uniqueN(regression_data$school_rbd) < 2) {
    stop("No usable regression data for outcome: ", outcome, call. = FALSE)
  }

  model_formula <- as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(control_terms, collapse = " + "),
      " | ",
      paste(fixed_effect_vars, collapse = " + ")
    )
  )

  message("Estimating adjusted school VA for ", outcome, ".")
  model <- feols(model_formula, data = regression_data, notes = FALSE)
  school_fe <- fixef(model)[["school_rbd"]]

  school_counts <- regression_data[
    ,
    .(
      n_students_regression = .N,
      n_cohorts_regression = uniqueN(cohort_gr8)
    ),
    by = school_rbd
  ]

  pooled_va <- data.table(
    school_rbd = as.numeric(names(school_fe)),
    outcome = outcome,
    outcome_label = outcome_label,
    controlled_value_added = as.numeric(school_fe),
    n_students_regression_total = nobs(model)
  )
  pooled_va <- merge(pooled_va, school_counts, by = "school_rbd", all.x = TRUE)

  school_mean <- mean(pooled_va$controlled_value_added, na.rm = TRUE)
  student_mean <- weighted.mean(
    pooled_va$controlled_value_added,
    pooled_va$n_students_regression,
    na.rm = TRUE
  )
  pooled_va[, `:=`(
    controlled_value_added_centered_school =
      controlled_value_added - school_mean,
    controlled_value_added_centered_student =
      controlled_value_added - student_mean
  )]

  regression_data[, school_fe_component :=
    as.numeric(school_fe[as.character(school_rbd)])]
  regression_data[, school_component_residual :=
    stats::residuals(model) + school_fe_component]

  school_cohort_means <- regression_data[
    ,
    .(
      ybar_school_component = mean(school_component_residual, na.rm = TRUE),
      n_students = .N
    ),
    by = .(school_rbd, cohort_gr8)
  ]
  school_cohort_means[, `:=`(
    outcome = outcome,
    outcome_label = outcome_label
  )]

  list(
    pooled_va = pooled_va,
    school_cohort_means = school_cohort_means,
    n_obs = nobs(model),
    n_schools = uniqueN(regression_data$school_rbd),
    n_school_cohorts = nrow(school_cohort_means)
  )
}

math_components <- estimate_outcome_components(
  dt,
  "z_year_math_max",
  "Math"
)
stem_components <- estimate_outcome_components(
  dt,
  "stem_enrollment_m1",
  "STEM enrollment"
)

rm(dt)
gc(verbose = FALSE)

make_single_outcome_stats <- function(school_cohort_means, prefix) {
  out <- school_cohort_means[
    ,
    .(
      T = .N,
      mean_ybar = mean(ybar_school_component, na.rm = TRUE),
      sum_ybar = sum(ybar_school_component, na.rm = TRUE),
      sum_ybar2 = sum(ybar_school_component^2, na.rm = TRUE),
      n_students_school_cohort_total = sum(n_students, na.rm = TRUE)
    ),
    by = school_rbd
  ]
  out[, signal2 := fifelse(
    T >= 2,
    (sum_ybar^2 - sum_ybar2) / (T * (T - 1)),
    NA_real_
  )]

  setnames(
    out,
    c("T", "mean_ybar", "sum_ybar", "sum_ybar2", "signal2", "n_students_school_cohort_total"),
    paste0(
      prefix,
      c("_T", "_mean_ybar", "_sum_ybar", "_sum_ybar2", "_signal2", "_n_students_school_cohort_total")
    )
  )
  out
}

estimate_covariance_objects <- function(math_means, stem_means) {
  math_stats <- make_single_outcome_stats(math_means, "math")
  stem_stats <- make_single_outcome_stats(stem_means, "stem")

  same_cohort <- merge(
    math_means[
      ,
      .(
        school_rbd,
        cohort_gr8,
        math_ybar = ybar_school_component
      )
    ],
    stem_means[
      ,
      .(
        school_rbd,
        cohort_gr8,
        stem_ybar = ybar_school_component
      )
    ],
    by = c("school_rbd", "cohort_gr8"),
    all = FALSE,
    sort = FALSE
  )
  same_stats <- same_cohort[
    ,
    .(
      same_cohort_product_sum = sum(math_ybar * stem_ybar, na.rm = TRUE),
      n_same_cohorts = .N
    ),
    by = school_rbd
  ]

  stats <- merge(math_stats, stem_stats, by = "school_rbd", all = FALSE)
  stats <- merge(stats, same_stats, by = "school_rbd", all.x = TRUE)
  stats[is.na(same_cohort_product_sum), same_cohort_product_sum := 0]
  stats[is.na(n_same_cohorts), n_same_cohorts := 0L]
  stats[, n_cross_outcome_cross_cohort_pairs :=
    math_T * stem_T - n_same_cohorts]
  stats[, stem_math_signal := fifelse(
    n_cross_outcome_cross_cohort_pairs > 0,
    (stem_sum_ybar * math_sum_ybar - same_cohort_product_sum) /
      n_cross_outcome_cross_cohort_pairs,
    NA_real_
  )]

  beta_stats <- stats[
    !is.na(math_signal2) &
      !is.na(stem_signal2) &
      !is.na(stem_math_signal) &
      math_T >= 2 &
      stem_T >= 2
  ]
  J <- nrow(beta_stats)
  if (J < 2) {
    stop("Fewer than two schools have usable cross-cohort math/STEM signals.")
  }

  variance_from_signals <- function(signal, mean_ybar) {
    ((J - 1) / J) * mean(signal, na.rm = TRUE) -
      ((sum(mean_ybar, na.rm = TRUE)^2 -
          sum(mean_ybar^2, na.rm = TRUE)) / J^2)
  }

  covariance_from_signals <- function(signal_xy, mean_x, mean_y) {
    ((J - 1) / J) * mean(signal_xy, na.rm = TRUE) -
      ((sum(mean_x, na.rm = TRUE) * sum(mean_y, na.rm = TRUE) -
          sum(mean_x * mean_y, na.rm = TRUE)) / J^2)
  }

  paper_var_math <- variance_from_signals(
    beta_stats$math_signal2,
    beta_stats$math_mean_ybar
  )
  paper_var_stem <- variance_from_signals(
    beta_stats$stem_signal2,
    beta_stats$stem_mean_ybar
  )
  paper_cov_stem_math <- covariance_from_signals(
    beta_stats$stem_math_signal,
    beta_stats$stem_mean_ybar,
    beta_stats$math_mean_ybar
  )

  beta_stem_on_math <- if (is.finite(paper_var_math) && paper_var_math > 0) {
    paper_cov_stem_math / paper_var_math
  } else {
    NA_real_
  }
  beta_math_on_stem <- if (is.finite(paper_var_stem) && paper_var_stem > 0) {
    paper_cov_stem_math / paper_var_stem
  } else {
    NA_real_
  }
  paper_corr_stem_math <- if (
    is.finite(paper_var_math) &&
      is.finite(paper_var_stem) &&
      paper_var_math > 0 &&
      paper_var_stem > 0
  ) {
    paper_cov_stem_math / sqrt(paper_var_math * paper_var_stem)
  } else {
    NA_real_
  }

  diagnostics <- data.table(
    metric = c(
      "n_schools_covariance_beta",
      "paper_var_math",
      "paper_var_stem",
      "paper_cov_stem_math",
      "paper_corr_stem_math",
      "beta_stem_on_math",
      "beta_math_on_stem",
      "paper_cov_stem_orth_math",
      "paper_var_stem_orth_math"
    ),
    value = c(
      J,
      paper_var_math,
      paper_var_stem,
      paper_cov_stem_math,
      paper_corr_stem_math,
      beta_stem_on_math,
      beta_math_on_stem,
      paper_cov_stem_math - beta_stem_on_math * paper_var_math,
      paper_var_stem - paper_cov_stem_math^2 / paper_var_math
    )
  )

  list(
    stats = stats,
    beta_school_set = beta_stats$school_rbd,
    diagnostics = diagnostics,
    beta_stem_on_math = beta_stem_on_math,
    beta_math_on_stem = beta_math_on_stem
  )
}

covariance_objects <- estimate_covariance_objects(
  math_components$school_cohort_means,
  stem_components$school_cohort_means
)

message("Reading existing pooled adjusted VA values for final school scores.")
existing_pooled_va <- load_existing_pooled_va(school_values_path)

if (is.null(existing_pooled_va)) {
  message(
    "Existing school VA file unavailable; using pooled FEs from this script ",
    "for final school scores."
  )
  pooled_va_source <- "orthogonal_school_va_script"
  math_va <- copy(math_components$pooled_va)
  stem_va <- copy(stem_components$pooled_va)
} else {
  pooled_va_source <- "school_rbd_observational_values"
  math_va <- existing_pooled_va[outcome == "z_year_math_max"]
  stem_va <- existing_pooled_va[outcome == "stem_enrollment_m1"]
  for (pooled_dt in list(math_va, stem_va)) {
    pooled_dt[, n_cohorts_regression := NA_integer_]
    setnames(
      pooled_dt,
      c("controlled_value_added_centered"),
      c("controlled_value_added_centered_school")
    )
  }
}

math_keep <- c(
  "school_rbd",
  "controlled_value_added",
  "controlled_value_added_centered_school",
  "controlled_value_added_centered_student",
  "n_students_regression",
  "n_cohorts_regression",
  "n_students_regression_total"
)
math_va <- math_va[, ..math_keep]
setnames(
  math_va,
  setdiff(names(math_va), "school_rbd"),
  paste0("math_", setdiff(names(math_va), "school_rbd"))
)

stem_va <- stem_va[, ..math_keep]
setnames(
  stem_va,
  setdiff(names(stem_va), "school_rbd"),
  paste0("stem_", setdiff(names(stem_va), "school_rbd"))
)

school_va <- merge(math_va, stem_va, by = "school_rbd", all = FALSE)
school_va[, pooled_va_source := pooled_va_source]
school_va[, used_in_cross_cohort_covariance :=
  school_rbd %in% covariance_objects$beta_school_set]
school_va[, beta_stem_on_math := covariance_objects$beta_stem_on_math]
school_va[, beta_math_on_stem := covariance_objects$beta_math_on_stem]

school_va[, stem_va_orth_math_school_centered :=
  stem_controlled_value_added_centered_school -
    beta_stem_on_math * math_controlled_value_added_centered_school]
school_va[, math_va_orth_stem_school_centered :=
  math_controlled_value_added_centered_school -
    beta_math_on_stem * stem_controlled_value_added_centered_school]
school_va[, stem_va_orth_math_student_centered :=
  stem_controlled_value_added_centered_student -
    beta_stem_on_math * math_controlled_value_added_centered_student]
school_va[, math_va_orth_stem_student_centered :=
  math_controlled_value_added_centered_student -
    beta_math_on_stem * stem_controlled_value_added_centered_student]

school_va[, stem_va_orth_math_school_centered :=
  stem_va_orth_math_school_centered -
    mean(stem_va_orth_math_school_centered, na.rm = TRUE)]
school_va[, math_va_orth_stem_school_centered :=
  math_va_orth_stem_school_centered -
    mean(math_va_orth_stem_school_centered, na.rm = TRUE)]
school_va[, stem_va_orth_math_student_centered :=
  stem_va_orth_math_student_centered -
    weighted.mean(
      stem_va_orth_math_student_centered,
      stem_n_students_regression,
      na.rm = TRUE
    )]
school_va[, math_va_orth_stem_student_centered :=
  math_va_orth_stem_student_centered -
    weighted.mean(
      math_va_orth_stem_student_centered,
      math_n_students_regression,
      na.rm = TRUE
    )]

school_cohort_means_long <- rbindlist(
  list(
    math_components$school_cohort_means,
    stem_components$school_cohort_means
  ),
  use.names = TRUE
)

diagnostics <- rbindlist(
  list(
    data.table(
      metric = c(
        "n_obs_math_regression",
        "n_obs_stem_regression",
        "n_schools_math_regression",
        "n_schools_stem_regression",
        "n_school_cohorts_math",
        "n_school_cohorts_stem",
        "n_schools_pooled_both",
        "naive_school_corr_math_stem_before",
        "naive_school_corr_math_stem_orth_after",
        "share_pooled_schools_used_in_cross_cohort_covariance"
      ),
      value = c(
        math_components$n_obs,
        stem_components$n_obs,
        math_components$n_schools,
        stem_components$n_schools,
        math_components$n_school_cohorts,
        stem_components$n_school_cohorts,
        nrow(school_va),
        safe_cor(
          school_va$math_controlled_value_added_centered_school,
          school_va$stem_controlled_value_added_centered_school
        ),
        safe_cor(
          school_va$math_controlled_value_added_centered_school,
          school_va$stem_va_orth_math_school_centered
        ),
        mean(school_va$used_in_cross_cohort_covariance)
      )
    ),
    covariance_objects$diagnostics
  ),
  use.names = TRUE
)

message("Writing school orthogonal VA: ", school_va_output)
fwrite(school_va, school_va_output)

message("Writing school-cohort residual means: ", school_cohort_output)
fwrite(school_cohort_means_long, school_cohort_output)

message("Writing covariance inputs: ", covariance_inputs_output)
fwrite(covariance_objects$stats, covariance_inputs_output)

message("Writing diagnostics: ", diagnostics_output)
fwrite(diagnostics, diagnostics_output)

message("Done.")
