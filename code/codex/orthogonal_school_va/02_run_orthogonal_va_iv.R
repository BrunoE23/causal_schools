suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# Two-dimensional IV using the orthogonal school VA metrics.
#
# Endogenous regressors:
#   1. attended math VA
#   2. attended STEM VA orthogonal to math VA
#
# Instruments:
#   1. offered math VA
#   2. offered STEM VA orthogonal to math VA
#
# Risk controls:
#   1. expected math VA under the DA probability portfolio
#   2. expected orthogonal STEM VA under the DA probability portfolio
#
# Outcomes:
#   1. grade-12 math score, year-standardized
#   2. first-enrollment STEM indicator

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
orthogonal_dir <- file.path(clean_dir, "orthogonal_school_va")
scalar_iv_dir <- file.path(clean_dir, "scalar_school_value_iv")
table_dir <- file.path(repo_wd, "output", "tables", "orthogonal_school_va")

dir.create(orthogonal_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

orthogonal_va_path <- file.path(orthogonal_dir, "school_orthogonal_va_exam_takers.csv")
base_regression_path <- file.path(
  scalar_iv_dir,
  "scalar_school_value_iv_expected_va.csv"
)
regression_output <- file.path(
  orthogonal_dir,
  "orthogonal_va_iv_regression_df_exam_takers.csv"
)
results_output <- file.path(
  table_dir,
  "orthogonal_va_iv_results.csv"
)
table_csv <- file.path(
  table_dir,
  "orthogonal_va_iv_2x2.csv"
)
table_tex <- file.path(
  table_dir,
  "orthogonal_va_iv_2x2.tex"
)

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

format_estimate_se <- function(beta, se) {
  ifelse(
    is.na(beta) | is.na(se),
    "",
    paste0(sprintf("%.3f", beta), " (", sprintf("%.3f", se), ")")
  )
}

format_se <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
}

message("Reading orthogonal school VA metrics: ", orthogonal_va_path)
school_values <- fread(
  orthogonal_va_path,
  select = c(
    "school_rbd",
    "math_controlled_value_added_centered_student",
    "stem_va_orth_math_student_centered"
  ),
  na.strings = c("", "NA")
)
stop_if_missing(
  names(school_values),
  c(
    "school_rbd",
    "math_controlled_value_added_centered_student",
    "stem_va_orth_math_student_centered"
  ),
  "Orthogonal VA"
)
setnames(
  school_values,
  c(
    "math_controlled_value_added_centered_student",
    "stem_va_orth_math_student_centered"
  ),
  c("school_value_math_va", "school_value_stem_orth_va")
)
school_values[, school_rbd := as.numeric(school_rbd)]

message("Reading base expected-VA regression dataframe: ", base_regression_path)
base_cols <- c(
  "student_id",
  "mrun",
  "cohort_gr8",
  "sae_proceso",
  "timely_sae",
  "rbd_treated_1R",
  "most_time_RBD",
  "any_risk",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "z_year_math_max",
  "z_year_leng_max",
  "stem_enrollment_m1"
)
estimation_df <- fread(
  base_regression_path,
  select = base_cols,
  na.strings = c("", "NA")
)
stop_if_missing(names(estimation_df), base_cols, "Base regression dataframe")
estimation_df <- estimation_df[timely_sae == 1 & any_risk == 1]
estimation_df <- estimation_df[!is.na(z_year_math_max) | !is.na(z_year_leng_max)]
estimation_df[, `:=`(
  student_id = as.numeric(student_id),
  sae_proceso = as.integer(sae_proceso),
  rbd_treated_1R = as.numeric(rbd_treated_1R),
  most_time_RBD = as.numeric(most_time_RBD)
)]

message("Reading DA probability files and computing expected orthogonal VA.")
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
  school_values,
  by.x = "rbd_prob",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

prob_summary <- prob_long[, .(
  expected_math_va = sum(
    prob * fifelse(is.na(school_value_math_va), 0, school_value_math_va),
    na.rm = TRUE
  ),
  expected_stem_orth_va = sum(
    prob * fifelse(is.na(school_value_stem_orth_va), 0, school_value_stem_orth_va),
    na.rm = TRUE
  ),
  mass_with_value_math_va = sum(
    prob * as.integer(!is.na(school_value_math_va)),
    na.rm = TRUE
  ),
  mass_with_value_stem_orth_va = sum(
    prob * as.integer(!is.na(school_value_stem_orth_va)),
    na.rm = TRUE
  )
), by = .(student_id, sae_proceso)]

rm(prob_long)
gc(verbose = FALSE)

message("Merging attended, offered, and expected orthogonal VA metrics.")
estimation_df <- merge(
  estimation_df,
  prob_summary,
  by = c("student_id", "sae_proceso"),
  all.x = TRUE,
  sort = FALSE
)

attended_values <- copy(school_values)
setnames(
  attended_values,
  c("school_value_math_va", "school_value_stem_orth_va"),
  c("d_math_va", "d_stem_orth_va")
)
estimation_df <- merge(
  estimation_df,
  attended_values,
  by.x = "most_time_RBD",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

offered_values <- copy(school_values)
setnames(
  offered_values,
  c("school_value_math_va", "school_value_stem_orth_va"),
  c("offered_math_va", "offered_stem_orth_va")
)
estimation_df <- merge(
  estimation_df,
  offered_values,
  by.x = "rbd_treated_1R",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

estimation_df[, z_math_va := fifelse(
  is.na(rbd_treated_1R) | rbd_treated_1R == 0,
  0,
  offered_math_va
)]
estimation_df[, z_stem_orth_va := fifelse(
  is.na(rbd_treated_1R) | rbd_treated_1R == 0,
  0,
  offered_stem_orth_va
)]
estimation_df[, c("offered_math_va", "offered_stem_orth_va") := NULL]

message("Writing regression dataframe: ", regression_output)
fwrite(estimation_df, regression_output)

outcome_specs <- data.table(
  outcome_id = c("math_score", "stem_enrollment"),
  outcome = c("z_year_math_max", "stem_enrollment_m1"),
  outcome_label = c("Math score", "STEM enrollment")
)

regressor_specs <- data.table(
  regressor = c("d_math_va", "d_stem_orth_va"),
  regressor_label = c("Math VA", "STEM VA orthogonal to math")
)

run_two_va_iv <- function(dt, spec_row) {
  y <- spec_row$outcome
  controls <- c(
    "factor(cohort_gr8)",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "factor(GEN_ALU)",
    "factor(EDAD_ALU)",
    "expected_math_va",
    "expected_stem_orth_va"
  )
  endog <- c("d_math_va", "d_stem_orth_va")
  instruments <- c("z_math_va", "z_stem_orth_va")
  needed <- c(y, controls[-c(1, 4, 5)], "cohort_gr8", "GEN_ALU", "EDAD_ALU", endog, instruments)
  reg_dt <- dt[complete.cases(dt[, ..needed])]

  if (
    nrow(reg_dt) == 0 ||
      any(vapply(c(endog, instruments), function(v) uniqueN(reg_dt[[v]]) < 2, logical(1)))
  ) {
    return(data.table(
      outcome_id = spec_row$outcome_id,
      outcome = y,
      outcome_label = spec_row$outcome_label,
      regressor = endog,
      beta = NA_real_,
      se = NA_real_,
      zstat = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(reg_dt),
      rc = 1L
    ))
  }

  iv_formula <- as.formula(paste0(
    y,
    " ~ ",
    paste(controls, collapse = " + "),
    " | 0 | ",
    paste(endog, collapse = " + "),
    " ~ ",
    paste(instruments, collapse = " + ")
  ))

  iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)

  out <- rbindlist(lapply(endog, function(term) {
    coef_term <- extract_coef(iv_model, term)
    beta <- coef(iv_model)[[coef_term]]
    se <- se(iv_model)[[coef_term]]
    zstat <- beta / se
    data.table(
      outcome_id = spec_row$outcome_id,
      outcome = y,
      outcome_label = spec_row$outcome_label,
      regressor = term,
      beta = beta,
      se = se,
      zstat = zstat,
      p_value = 2 * stats::pnorm(-abs(zstat)),
      n_obs = nobs(iv_model),
      rc = 0L
    )
  }), use.names = TRUE)

  out
}

message("Running two-VA IV regressions.")
results <- rbindlist(lapply(seq_len(nrow(outcome_specs)), function(i) {
  message("  outcome: ", outcome_specs$outcome_label[i])
  run_two_va_iv(estimation_df, outcome_specs[i])
}), use.names = TRUE)
results <- merge(results, regressor_specs, by = "regressor", all.x = TRUE, sort = FALSE)
setcolorder(
  results,
  c(
    "outcome_id",
    "outcome",
    "outcome_label",
    "regressor",
    "regressor_label",
    "beta",
    "se",
    "zstat",
    "p_value",
    "n_obs",
    "rc"
  )
)

message("Writing full results: ", results_output)
fwrite(results, results_output)

table_long <- copy(results)
table_long[, cell := format_estimate_se(beta, se)]
table_wide <- dcast(
  table_long,
  regressor_label ~ outcome_label,
  value.var = "cell"
)
table_wide[, regressor_order := match(
  regressor_label,
  c("Math VA", "STEM VA orthogonal to math")
)]
setorder(table_wide, regressor_order)
table_wide[, regressor_order := NULL]
fwrite(table_wide, table_csv)

latex_rows <- c()
for (reg_label in c("Math VA", "STEM VA orthogonal to math")) {
  row_dt <- table_long[regressor_label == reg_label]
  row_dt <- row_dt[match(c("Math score", "STEM enrollment"), outcome_label)]
  latex_rows <- c(
    latex_rows,
    paste0(
      reg_label,
      " & ",
      paste(format_estimate(row_dt$beta), collapse = " & "),
      " \\\\"
    ),
    paste0(
      " & ",
      paste(format_se(row_dt$se), collapse = " & "),
      " \\\\"
    )
  )
}

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Two-dimensional orthogonal school-value IV estimates}",
  "\\label{tab:orthogonal_school_va_iv_2x2}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & Math score & STEM enrollment \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(latex_table, table_tex)

message("Writing 2x2 table CSV: ", table_csv)
message("Writing 2x2 table TeX: ", table_tex)
message("Done.")
