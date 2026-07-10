###############################################################################
# Combine one-outcome Stata VA+EB outputs and prepare the EB-IV input file.
#
# The one-outcome Stata runner writes one set of files per outcome key:
#   stata_eb_school_values_<key>.csv
#   stata_eb_diagnostics_<key>.csv
#   stata_va_eb_timing_<key>.csv
#
# This script appends those files and creates the column names expected by
# 02_run_expected_va_scalar_iv_eb.R.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

parse_env_list <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(trimws(value))) {
    return(default)
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
outcome_keys <- parse_env_list(
  "STATA_EB_OUTCOME_KEYS",
  c(
    "math",
    "language",
    "exam",
    "enrolled",
    "stem",
    "program_income_area",
    "program_income_institution",
    "program_income_full",
    "progcert",
    "instcert"
  )
)

required_files <- data.table(
  outcome_key = outcome_keys,
  eb_path = file.path(table_dir, paste0("stata_eb_school_values_", outcome_keys, ".csv")),
  diag_path = file.path(table_dir, paste0("stata_eb_diagnostics_", outcome_keys, ".csv")),
  timing_path = file.path(table_dir, paste0("stata_va_eb_timing_", outcome_keys, ".csv"))
)

missing_paths <- unlist(required_files[, .(eb_path, diag_path, timing_path)], use.names = FALSE)
missing_paths <- missing_paths[!file.exists(missing_paths)]
if (length(missing_paths) > 0) {
  stop(
    "Missing one-outcome Stata VA+EB outputs:\n",
    paste(missing_paths, collapse = "\n"),
    call. = FALSE
  )
}

eb <- rbindlist(lapply(required_files$eb_path, fread), use.names = TRUE, fill = TRUE)
diagnostics <- rbindlist(lapply(required_files$diag_path, fread), use.names = TRUE, fill = TRUE)
timing <- rbindlist(lapply(required_files$timing_path, fread), use.names = TRUE, fill = TRUE)

all_eb_path <- file.path(table_dir, "stata_eb_school_values_all_outcomes.csv")
all_diag_path <- file.path(table_dir, "stata_eb_diagnostics_all_outcomes.csv")
all_timing_path <- file.path(table_dir, "stata_va_eb_timing_all_outcomes.csv")
iv_path <- file.path(table_dir, "stata_eb_school_rbd_observational_values_for_iv.csv")

fwrite(eb, all_eb_path)
fwrite(diagnostics, all_diag_path)
fwrite(timing, all_timing_path)

iv <- copy(eb)
iv[, `:=`(
  n_students_regression = as.integer(n_students),
  controlled_value_added = as.numeric(va_raw),
  controlled_value_added_centered_student = as.numeric(va_centered),
  controlled_value_added_se = as.numeric(va_se),
  controlled_value_added_resid_sd = as.numeric(va_resid_sd),
  controlled_value_added_se_method = va_se_method,
  controlled_value_added_eb = as.numeric(va_eb),
  controlled_value_added_eb_centered_student = as.numeric(va_eb_centered)
)]

iv_cols <- c(
  "school_rbd",
  "analysis_sample",
  "outcome",
  "n_students_regression",
  "n_total",
  "controlled_value_added",
  "controlled_value_added_centered_student",
  "controlled_value_added_se",
  "controlled_value_added_resid_sd",
  "controlled_value_added_se_method",
  "eb_tau2",
  "eb_tau",
  "eb_reliability",
  "eb_method",
  "controlled_value_added_eb",
  "controlled_value_added_eb_centered_student"
)
fwrite(iv[, ..iv_cols], iv_path)

message("Wrote: ", all_eb_path)
message("Wrote: ", all_diag_path)
message("Wrote: ", all_timing_path)
message("Wrote: ", iv_path)
print(timing[, .(outcome_key, outcome, va_seconds, eb_seconds, total_seconds, n_obs, n_schools)])
