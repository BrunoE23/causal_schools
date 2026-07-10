###############################################################################
# Empirical-Bayes shrinkage for observational school value added
#
# This script starts from the All-sample controlled school value-added estimates
# produced by:
#
#   code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R
#
# That constructor must first be rerun so the school-value file includes
# controlled_value_added_se. The SE is a regression-derived uncertainty input
# for the student-weighted centered school effect used in EB shrinkage.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

weighted_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  x <- x[ok]
  w <- w[ok]
  mu <- stats::weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

weighted_sd <- function(x, w) {
  sqrt(weighted_var(x, w))
}

parse_env_list <- function(var) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(trimws(value))) {
    return(character())
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
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
values_dir <- file.path(clean_dir, "school_rbd_observational_values")
output_dir <- file.path(clean_dir, "empirical_bayes_school_va")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

school_values_path <- Sys.getenv(
  "SCHOOL_RBD_VALUES_INPUT_PATH",
  unset = file.path(values_dir, "school_rbd_observational_values.csv")
)
eb_values_path <- Sys.getenv(
  "EB_SCHOOL_VALUES_OUTPUT_PATH",
  unset = file.path(output_dir, "eb_school_rbd_observational_values.csv")
)
diagnostics_path <- Sys.getenv(
  "EB_SCHOOL_DIAGNOSTICS_OUTPUT_PATH",
  unset = file.path(output_dir, "eb_school_rbd_observational_values_diagnostics.csv")
)
dir.create(dirname(eb_values_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(diagnostics_path), recursive = TRUE, showWarnings = FALSE)

target_outcomes <- c(
  "z_year_math_max",
  "z_year_leng_max",
  "stem_enrollment_m1",
  "log_program_income_clp_m1",
  "program_certified_years_m1",
  "inst_certified_years_m1"
)

configured_outcome_filter <- parse_env_list("EB_SCHOOL_VA_OUTCOMES")
if (length(configured_outcome_filter) > 0) {
  missing_filtered_outcomes <- setdiff(configured_outcome_filter, target_outcomes)
  if (length(missing_filtered_outcomes) > 0) {
    stop(
      "EB_SCHOOL_VA_OUTCOMES includes outcomes not configured for EB shrinkage: ",
      paste(missing_filtered_outcomes, collapse = ", "),
      call. = FALSE
    )
  }
  target_outcomes <- target_outcomes[target_outcomes %in% configured_outcome_filter]
}

required_cols <- c(
  "school_rbd",
  "analysis_sample",
  "outcome",
  "controlled_value_added_centered_student",
  "controlled_value_added_se",
  "controlled_value_added_resid_sd",
  "controlled_value_added_se_method",
  "n_students_regression",
  "n_students_regression_total"
)

message("Reading school value-added estimates: ", school_values_path)
values <- fread(school_values_path, na.strings = c("", "NA"))
missing_cols <- setdiff(required_cols, names(values))
if (length(missing_cols) > 0) {
  stop(
    "School value file is missing columns required for EB shrinkage: ",
    paste(missing_cols, collapse = ", "),
    ". Rerun code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R first.",
    call. = FALSE
  )
}

values <- values[
  analysis_sample == "All" &
    outcome %chin% target_outcomes &
    !is.na(controlled_value_added_centered_student) &
    !is.na(controlled_value_added_se) &
    controlled_value_added_se >= 0 &
    !is.na(n_students_regression) &
    n_students_regression > 0
]

if (nrow(values) == 0) {
  stop("No eligible All-sample controlled VA rows found for EB shrinkage.", call. = FALSE)
}

setDT(values)
values[, school_rbd := as.numeric(school_rbd)]
values[, eb_noise_variance := controlled_value_added_se^2]

eb_values <- values[, {
  x <- controlled_value_added_centered_student
  se2 <- eb_noise_variance
  w <- n_students_regression

  center_student <- stats::weighted.mean(x, w, na.rm = TRUE)
  center_school <- mean(x, na.rm = TRUE)
  variance_observed_student <- weighted_var(x, w)
  variance_observed_school <- stats::var(x, na.rm = TRUE)
  mean_noise_variance_student <- stats::weighted.mean(se2, w, na.rm = TRUE)
  mean_noise_variance_school <- mean(se2, na.rm = TRUE)
  tau2 <- max(variance_observed_student - mean_noise_variance_student, 0)

  reliability <- if (tau2 == 0) {
    rep(0, length(x))
  } else {
    tau2 / (tau2 + se2)
  }

  eb_uncentered <- center_student + reliability * (x - center_student)
  eb_centered_student <- eb_uncentered -
    stats::weighted.mean(eb_uncentered, w, na.rm = TRUE)
  eb_centered_school <- eb_uncentered - mean(eb_uncentered, na.rm = TRUE)

  .(
    school_rbd = school_rbd,
    analysis_sample = analysis_sample,
    controlled_value_added_centered_student = x,
    controlled_value_added_se = controlled_value_added_se,
    controlled_value_added_resid_sd = controlled_value_added_resid_sd,
    controlled_value_added_se_method = controlled_value_added_se_method,
    n_students_regression = n_students_regression,
    n_students_regression_total = n_students_regression_total,
    eb_tau2 = tau2,
    eb_tau = sqrt(tau2),
    eb_reliability = reliability,
    controlled_value_added_eb = eb_uncentered,
    controlled_value_added_eb_centered = eb_centered_school,
    controlled_value_added_eb_centered_student = eb_centered_student,
    eb_center_student = center_student,
    eb_center_school = center_school,
    eb_variance_observed_student = variance_observed_student,
    eb_variance_observed_school = variance_observed_school,
    eb_mean_noise_variance_student = mean_noise_variance_student,
    eb_mean_noise_variance_school = mean_noise_variance_school,
    eb_method = "normal_normal_weighted_by_n_students_regression"
  )
}, by = outcome]

diagnostics <- eb_values[, .(
  n_schools = .N,
  n_students_regression = sum(n_students_regression, na.rm = TRUE),
  tau2 = eb_tau2[1L],
  tau = eb_tau[1L],
  observed_sd_student_weighted = sqrt(eb_variance_observed_student[1L]),
  eb_sd_student_weighted = weighted_sd(controlled_value_added_eb_centered_student, n_students_regression),
  observed_sd_school_unweighted = sqrt(eb_variance_observed_school[1L]),
  eb_sd_school_unweighted = stats::sd(controlled_value_added_eb_centered, na.rm = TRUE),
  mean_noise_sd_student_weighted = sqrt(eb_mean_noise_variance_student[1L]),
  mean_reliability_student_weighted = stats::weighted.mean(eb_reliability, n_students_regression, na.rm = TRUE),
  mean_reliability_school_unweighted = mean(eb_reliability, na.rm = TRUE),
  p10_reliability = as.numeric(stats::quantile(eb_reliability, 0.10, na.rm = TRUE)),
  p50_reliability = as.numeric(stats::quantile(eb_reliability, 0.50, na.rm = TRUE)),
  p90_reliability = as.numeric(stats::quantile(eb_reliability, 0.90, na.rm = TRUE))
), by = outcome]

message("Writing EB school values: ", eb_values_path)
fwrite(eb_values, eb_values_path)

message("Writing EB diagnostics: ", diagnostics_path)
fwrite(diagnostics, diagnostics_path)

print(diagnostics)
