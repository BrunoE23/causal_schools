###############################################################################
# Scalar school-value IV with expected-VA assignment-risk control
#
# CURRENT DEFAULT PATH:
# Use this script for main scalar-IV estimates and heterogeneous-effects tables.
# It avoids the legacy high-dimensional prob_* / iszero_* controls by carrying
# one scalar expected-value risk control for each school-value metric.
#
# This replaces the high-dimensional prob_* / iszero_* controls with one
# scalar risk control for each value metric:
#
#   expected_VA_i = sum_s p_is * VA_s
#
# where p_is comes from the long DA probability files and VA_s comes from the
# All-sample school value-added output.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
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

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
prob_dir <- file.path(clean_dir, "DA_probs")
values_dir <- file.path(clean_dir, "school_rbd_observational_values")
scalar_dir <- file.path(clean_dir, "scalar_school_value_iv")
table_dir <- file.path(repo_wd, "output", "tables", "scalar_school_value_iv")

dir.create(scalar_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
school_values_path <- file.path(values_dir, "school_rbd_observational_values.csv")

regression_csv <- file.path(
  scalar_dir,
  "scalar_school_value_iv_expected_va.csv"
)
results_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_results_expected_va.csv"
)
table_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_main_results_expected_va.csv"
)
table_tex <- file.path(
  table_dir,
  "scalar_school_value_iv_main_results_expected_va.tex"
)
diagnostics_csv <- file.path(
  scalar_dir,
  "scalar_school_value_iv_expected_va_diagnostics.csv"
)

value_specs <- data.table::data.table(
  spec = c(
    "math_unadj", "math_adj",
    "leng_unadj", "leng_adj",
    "stem_unadj", "stem_adj"
  ),
  outcome = c(
    "z_year_math_max", "z_year_math_max",
    "z_year_leng_max", "z_year_leng_max",
    "stem_enrollment_m1", "stem_enrollment_m1"
  ),
  value_outcome = c(
    "z_year_math_max", "z_year_math_max",
    "z_year_leng_max", "z_year_leng_max",
    "stem_enrollment_m1", "stem_enrollment_m1"
  ),
  value_column = c(
    "raw_mean_centered_student",
    "controlled_value_added_centered_student",
    "raw_mean_centered_student",
    "controlled_value_added_centered_student",
    "raw_mean_centered_student",
    "controlled_value_added_centered_student"
  ),
  outcome_group = c("Math", "Math", "Language", "Language", "STEM", "STEM"),
  adjustment = c("Unadjusted", "Adjusted", "Unadjusted", "Adjusted", "Unadjusted", "Adjusted"),
  column_id = c("math_unadj", "math_adj", "leng_unadj", "leng_adj", "stem_unadj", "stem_adj")
)

z_within_group <- function(x) {
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sigma
}

extract_coef <- function(model, term) {
  coefs <- coef(model)
  if (term %in% names(coefs)) {
    return(term)
  }
  fit_term <- paste0("fit_", term)
  if (fit_term %in% names(coefs)) {
    return(fit_term)
  }
  hit <- grep(term, names(coefs), fixed = TRUE, value = TRUE)
  if (length(hit) == 1) {
    return(hit)
  }
  stop("Could not find coefficient for term: ", term, call. = FALSE)
}

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

format_p_value <- function(x) {
  fifelse(
    is.na(x),
    "",
    fifelse(x < 0.001, "$<0.001$", sprintf("%.3f", x))
  )
}

message("Reading All-sample school values: ", school_values_path)
school_values_raw <- fread(school_values_path)
school_values_raw <- school_values_raw[
  analysis_sample == "All" &
    outcome %chin% value_specs$value_outcome,
  .(school_rbd, outcome, raw_mean_centered_student, controlled_value_added_centered_student)
]
school_values_raw[, school_rbd := as.numeric(school_rbd)]

school_values_long <- rbindlist(lapply(seq_len(nrow(value_specs)), function(i) {
  spec_row <- value_specs[i]
  school_values_raw[
    outcome == spec_row$value_outcome,
    .(
      school_rbd,
      spec = spec_row$spec,
      value = get(spec_row$value_column)
    )
  ]
}), use.names = TRUE)

school_values_wide <- dcast(
  school_values_long,
  school_rbd ~ spec,
  value.var = "value"
)
setnames(
  school_values_wide,
  setdiff(names(school_values_wide), "school_rbd"),
  paste0("school_value_", setdiff(names(school_values_wide), "school_rbd"))
)

message("Reading broad grade-8 universe columns: ", universe_path)
universe_cols <- c(
  "student_id", "mrun", "cohort_gr8", "sae_proceso", "timely_sae",
  "rbd_treated_1R", "most_time_RBD", "GEN_ALU", "EDAD_ALU",
  "z_sim_mat_4to", "z_sim_leng_4to", "math_max", "leng_max", "psu_year",
  "f_science_m1", "f_eng_m1",
  "simce_year_8b",
  "ptje_mate8b_alu",
  "simce4_math_decile",
  "simce8_math_decile",
  "simce8_math_quintile",
  "simce8_vs_4to_math_decile_change",
  "simce8_math_decile_movement",
  "simce8_math_improved_gt1_decile",
  "simce8_math_within1_decile",
  "simce8_math_worsened_gt1_decile"
)
universe <- fread(universe_path, select = universe_cols, na.strings = c("", "NA"))
universe[, `:=`(
  student_id = as.numeric(student_id),
  sae_proceso = as.integer(sae_proceso),
  rbd_treated_1R = as.numeric(rbd_treated_1R),
  most_time_RBD = as.numeric(most_time_RBD)
)]

universe[
  !is.na(psu_year) & !is.na(math_max) & math_max > 0,
  z_year_math_max := z_within_group(math_max),
  by = psu_year
]
universe[
  !is.na(psu_year) & !is.na(leng_max) & leng_max > 0,
  z_year_leng_max := z_within_group(leng_max),
  by = psu_year
]
universe[, stem_enrollment_m1 := as.integer(
  fifelse(is.na(f_science_m1), 0, as.numeric(f_science_m1)) == 1 |
    fifelse(is.na(f_eng_m1), 0, as.numeric(f_eng_m1)) == 1
)]

message("Reading long DA probability files and computing expected VA.")
prob_list <- lapply(2018:2021, function(year) {
  path <- file.path(prob_dir, paste0("DA_probs_", year, ".csv"))
  dt <- fread(path, select = c("student_id", "school_id", "prob"))
  dt[, sae_proceso := as.integer(year)]
  dt[, rbd_prob := suppressWarnings(as.numeric(sub("_.*$", "", school_id)))]
  dt[, .(student_id = as.numeric(student_id), sae_proceso, rbd_prob, prob)]
})

prob_long <- rbindlist(prob_list, use.names = TRUE)
prob_long <- merge(
  prob_long,
  school_values_wide,
  by.x = "rbd_prob",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

value_names <- value_specs$spec

prob_summary <- prob_long[, .(
  any_risk = as.integer(max(prob, na.rm = TRUE) < 1),
  total_probability_mass = sum(prob, na.rm = TRUE),
  n_positive_probability_options = sum(prob > 0, na.rm = TRUE)
), by = .(student_id, sae_proceso)]

for (v in value_names) {
  value_col <- paste0("school_value_", v)
  expected_col <- paste0("expected_", v)
  mass_col <- paste0("mass_with_value_", v)

  tmp <- prob_long[, .(
    expected_value = sum(prob * fifelse(is.na(get(value_col)), 0, get(value_col)), na.rm = TRUE),
    mass_with_value = sum(prob * as.integer(!is.na(get(value_col))), na.rm = TRUE)
  ), by = .(student_id, sae_proceso)]

  setnames(tmp, c("expected_value", "mass_with_value"), c(expected_col, mass_col))
  prob_summary <- merge(
    prob_summary,
    tmp,
    by = c("student_id", "sae_proceso"),
    all.x = TRUE,
    sort = FALSE
  )
}

rm(prob_long)
gc(verbose = FALSE)

message("Merging estimation sample and scalar school values.")
estimation_df <- merge(
  universe,
  prob_summary,
  by = c("student_id", "sae_proceso"),
  all = FALSE,
  sort = FALSE
)
estimation_df <- estimation_df[timely_sae == 1 & any_risk == 1]

attended_values <- copy(school_values_wide)
setnames(
  attended_values,
  paste0("school_value_", value_names),
  paste0("attended_", value_names)
)
estimation_df <- merge(
  estimation_df,
  attended_values,
  by.x = "most_time_RBD",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

offered_values <- copy(school_values_wide)
setnames(
  offered_values,
  paste0("school_value_", value_names),
  paste0("offered_", value_names)
)
estimation_df <- merge(
  estimation_df,
  offered_values,
  by.x = "rbd_treated_1R",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

for (v in value_names) {
  d_col <- paste0("d_", v)
  z_col <- paste0("z_", v)
  attended_col <- paste0("attended_", v)
  offered_col <- paste0("offered_", v)

  estimation_df[, (d_col) := get(attended_col)]
  estimation_df[, (z_col) := fifelse(
    is.na(rbd_treated_1R) | rbd_treated_1R == 0,
    0,
    get(offered_col)
  )]
}

keep_cols <- c(
  "student_id", "mrun", "cohort_gr8", "sae_proceso", "timely_sae",
  "rbd_treated_1R", "most_time_RBD", "any_risk", "total_probability_mass",
  "n_positive_probability_options", "GEN_ALU", "EDAD_ALU",
  "z_sim_mat_4to", "z_sim_leng_4to", "math_max", "leng_max", "psu_year",
  "z_year_math_max", "z_year_leng_max", "stem_enrollment_m1",
  "simce_year_8b",
  "ptje_mate8b_alu",
  "simce4_math_decile",
  "simce8_math_decile",
  "simce8_math_quintile",
  "simce8_vs_4to_math_decile_change",
  "simce8_math_decile_movement",
  "simce8_math_improved_gt1_decile",
  "simce8_math_within1_decile",
  "simce8_math_worsened_gt1_decile",
  paste0("expected_", value_names),
  paste0("mass_with_value_", value_names),
  paste0("d_", value_names),
  paste0("z_", value_names)
)
estimation_df <- estimation_df[, ..keep_cols]

message("Writing regression dataframe: ", regression_csv)
fwrite(estimation_df, regression_csv)

run_iv_spec <- function(dt, spec_row) {
  spec <- spec_row$spec
  y <- spec_row$outcome
  d <- paste0("d_", spec)
  z <- paste0("z_", spec)
  expected <- paste0("expected_", spec)

  controls <- c(
    "factor(cohort_gr8)",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "factor(GEN_ALU)",
    "factor(EDAD_ALU)",
    expected
  )

  needed <- c(y, d, z, expected, "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU")
  reg_dt <- dt[complete.cases(dt[, ..needed])]

  if (nrow(reg_dt) == 0 || uniqueN(reg_dt[[z]]) < 2 || uniqueN(reg_dt[[d]]) < 2) {
    return(data.table(
      spec = spec,
      group = "all",
      outcome = y,
      treatment = d,
      instrument = z,
      risk_control = expected,
      beta = NA_real_,
      se = NA_real_,
      zstat = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(reg_dt),
      fs_beta = NA_real_,
      fs_se = NA_real_,
      fs_f = NA_real_,
      rc = 1L
    ))
  }

  control_rhs <- paste(controls, collapse = " + ")
  iv_formula <- as.formula(paste0(y, " ~ ", control_rhs, " | 0 | ", d, " ~ ", z))
  fs_formula <- as.formula(paste0(d, " ~ ", z, " + ", control_rhs))

  iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  coef_term <- extract_coef(iv_model, d)
  beta <- coef(iv_model)[[coef_term]]
  se <- se(iv_model)[[coef_term]]
  zstat <- beta / se
  p_value <- 2 * stats::pnorm(-abs(zstat))

  fs_model <- feols(fs_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  fs_beta <- coef(fs_model)[[z]]
  fs_se <- se(fs_model)[[z]]
  fs_f <- (fs_beta / fs_se)^2

  data.table(
    spec = spec,
    group = "all",
    outcome = y,
    treatment = d,
    instrument = z,
    risk_control = expected,
    beta = beta,
    se = se,
    zstat = zstat,
    p_value = p_value,
    n_obs = nobs(iv_model),
    fs_beta = fs_beta,
    fs_se = fs_se,
    fs_f = fs_f,
    rc = 0L
  )
}

message("Running expected-VA scalar IV specs.")
setFixest_nthreads(0)
results <- rbindlist(lapply(seq_len(nrow(value_specs)), function(i) {
  message("  spec: ", value_specs$spec[i])
  run_iv_spec(estimation_df, value_specs[i])
}), use.names = TRUE)

message("Writing results: ", results_csv)
fwrite(results, results_csv)

table_out <- merge(
  value_specs,
  results,
  by = "spec",
  all.x = TRUE,
  sort = FALSE
)
fwrite(
  table_out[, .(
    spec, outcome_group, adjustment, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f
  )],
  table_csv
)

display_rows <- rbindlist(list(
  table_out[, .(stat = "$\\theta$", column_id, value = format_estimate(beta))],
  table_out[, .(stat = "SE", column_id, value = format_estimate(se))],
  table_out[, .(stat = "p-value", column_id, value = format_p_value(p_value))],
  table_out[, .(stat = "First-stage coef.", column_id, value = format_estimate(fs_beta))],
  table_out[, .(stat = "First-stage SE", column_id, value = format_estimate(fs_se))],
  table_out[, .(stat = "First-stage F", column_id, value = format_estimate(fs_f))],
  table_out[, .(stat = "N", column_id, value = format(n_obs, big.mark = ",", scientific = FALSE, trim = TRUE))]
), use.names = TRUE)

display_wide <- dcast(display_rows, stat ~ column_id, value.var = "value")
stat_order <- c("$\\theta$", "SE", "p-value", "First-stage coef.", "First-stage SE", "First-stage F", "N")
display_wide[, stat := factor(stat, levels = stat_order)]
setorder(display_wide, stat)
display_wide[, stat := as.character(stat)]

row_values <- function(row) {
  c(
    row[["stat"]],
    row[["math_unadj"]],
    row[["math_adj"]],
    row[["leng_unadj"]],
    row[["leng_adj"]],
    row[["stem_unadj"]],
    row[["stem_adj"]]
  )
}

latex_rows <- unlist(lapply(seq_len(nrow(display_wide)), function(i) {
  row <- paste(row_values(display_wide[i]), collapse = " & ")
  if (display_wide$stat[i] == "p-value") {
    return(c(paste0(row, " \\\\"), "\\midrule"))
  }
  paste0(row, " \\\\")
}), use.names = FALSE)

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Scalar school-value IV estimates with expected-VA risk control}",
  "\\label{tab:scalar_school_value_iv_expected_va}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Math VA} & \\multicolumn{2}{c}{Language VA} & \\multicolumn{2}{c}{STEM VA} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  " & Unadjusted & Adjusted & Unadjusted & Adjusted & Unadjusted & Adjusted \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(latex_table, table_tex)

diagnostics <- data.table(
  measure = c(
    "universe_rows",
    "probability_student_process_rows",
    "estimation_rows_timely_at_risk",
    "mean_total_probability_mass",
    paste0("mean_mass_with_value_", value_names)
  ),
  value = c(
    nrow(universe),
    nrow(prob_summary),
    nrow(estimation_df),
    mean(estimation_df$total_probability_mass, na.rm = TRUE),
    sapply(paste0("mass_with_value_", value_names), function(col) {
      mean(estimation_df[[col]], na.rm = TRUE)
    })
  )
)
fwrite(diagnostics, diagnostics_csv)

message("Wrote: ", regression_csv)
message("Wrote: ", results_csv)
message("Wrote: ", table_csv)
message("Wrote: ", table_tex)
message("Wrote: ", diagnostics_csv)
print(table_out[, .(spec, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f)])
