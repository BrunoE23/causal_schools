###############################################################################
# Empirical-Bayes shrinkage for RC VAM school value added.
#
# Mirrors the EB shrinkage logic in
#   code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R
# (the same normal-normal shrinkage formula, weighted by
# n_students_regression), applied instead to the RC VAM coefficients produced
# by 01_construct_rc_vam_school_values.R.
#
# This lets you compare, outcome by outcome:
#   - the current broad-panel observational VA -> EB values
#   - the RC VAM (lottery-risk-sample, propensity-score-controlled) -> EB values
# on the same shrinkage methodology, differing only in what's on the
# right-hand side of the first-stage regression and what sample it's
# estimated on.
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
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  mu <- stats::weighted.mean(x, w)
  sum(w * (x - mu)^2) / sum(w)
}
weighted_sd <- function(x, w) sqrt(weighted_var(x, w))

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
rc_vam_dir <- file.path(clean_dir, "rc_vam_school_values")

rc_vam_values_path <- Sys.getenv(
  "RC_VAM_SCHOOL_VALUES_INPUT_PATH",
  # Default points at the R noreg pipeline's actual output filename --
  # 01_construct_rc_vam_school_values_noreg.R is the only active producer
  # of RC VAM school values (the ridge/lasso .do scripts are archived, see
  # code/claude/rc_vam/archive/). Override via the env var above if you
  # ever need to point this at a ridge/lasso run instead.
  unset = file.path(rc_vam_dir, "rc_vam_school_values_r_noreg.csv")
)
eb_values_path <- Sys.getenv(
  "EB_RC_VAM_SCHOOL_VALUES_OUTPUT_PATH",
  unset = file.path(rc_vam_dir, "eb_rc_vam_school_values.csv")
)
diagnostics_path <- Sys.getenv(
  "EB_RC_VAM_DIAGNOSTICS_OUTPUT_PATH",
  unset = file.path(rc_vam_dir, "eb_rc_vam_school_values_diagnostics.csv")
)
dir.create(dirname(eb_values_path), recursive = TRUE, showWarnings = FALSE)

required_cols <- c(
  "school_rbd", "outcome", "controlled_value_added_centered_student",
  "controlled_value_added_se", "n_students_regression", "n_students_regression_total"
)

message("Reading RC VAM school-value estimates: ", rc_vam_values_path)
if (!file.exists(rc_vam_values_path)) {
  stop(
    "RC VAM values file does not exist: ", rc_vam_values_path,
    ". Run code/codex/rc_vam/01_construct_rc_vam_school_values.R first.",
    call. = FALSE
  )
}
values <- fread(rc_vam_values_path, na.strings = c("", "NA"))
missing_cols <- setdiff(required_cols, names(values))
if (length(missing_cols) > 0) {
  stop(
    "RC VAM values file is missing columns required for EB shrinkage: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

values <- values[
  !is.na(controlled_value_added_centered_student) &
    !is.na(controlled_value_added_se) &
    controlled_value_added_se >= 0 &
    !is.na(n_students_regression) &
    n_students_regression > 0
]

if (nrow(values) == 0) {
  stop("No eligible RC VAM rows found for EB shrinkage.", call. = FALSE)
}

setDT(values)
values[, school_rbd := as.numeric(school_rbd)]
values[, eb_noise_variance := controlled_value_added_se^2]

eb_values <- values[, {
  x <- controlled_value_added_centered_student
  se2 <- eb_noise_variance
  w <- n_students_regression

  center_student <- stats::weighted.mean(x, w, na.rm = TRUE)
  variance_observed_student <- weighted_var(x, w)
  mean_noise_variance_student <- stats::weighted.mean(se2, w, na.rm = TRUE)
  tau2 <- max(variance_observed_student - mean_noise_variance_student, 0)

  reliability <- if (tau2 == 0) rep(0, length(x)) else tau2 / (tau2 + se2)

  eb_uncentered <- center_student + reliability * (x - center_student)
  eb_centered_student <- eb_uncentered - stats::weighted.mean(eb_uncentered, w, na.rm = TRUE)

  .(
    school_rbd = school_rbd,
    controlled_value_added_centered_student = x,
    controlled_value_added_se = controlled_value_added_se,
    n_students_regression = n_students_regression,
    n_students_regression_total = n_students_regression_total,
    eb_tau2 = tau2,
    eb_reliability = reliability,
    rc_vam_eb_centered_student = eb_centered_student,
    eb_method = "normal_normal_weighted_by_n_students_regression"
  )
}, by = outcome]

diagnostics <- eb_values[, .(
  n_schools = .N,
  n_students_regression = sum(n_students_regression, na.rm = TRUE),
  tau2 = eb_tau2[1L],
  observed_sd_student_weighted = sqrt(weighted_var(controlled_value_added_centered_student, n_students_regression)),
  eb_sd_student_weighted = weighted_sd(rc_vam_eb_centered_student, n_students_regression),
  mean_reliability_student_weighted = stats::weighted.mean(eb_reliability, n_students_regression, na.rm = TRUE),
  p10_reliability = as.numeric(stats::quantile(eb_reliability, 0.10, na.rm = TRUE)),
  p50_reliability = as.numeric(stats::quantile(eb_reliability, 0.50, na.rm = TRUE)),
  p90_reliability = as.numeric(stats::quantile(eb_reliability, 0.90, na.rm = TRUE))
), by = outcome]

message("Writing EB RC VAM values: ", eb_values_path)
fwrite(eb_values, eb_values_path)
message("Writing EB RC VAM diagnostics: ", diagnostics_path)
fwrite(diagnostics, diagnostics_path)

print(diagnostics)
