###############################################################################
# Scalar school-value IV with EB-shrunken expected-VA risk controls
#
# This script repeats the current expected-VA scalar IV design after replacing
# the stage-1 observational school value-added estimates with EB-shrunken
# versions produced by:
#
#   code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R
#
# Outputs are written separately from the current main specification.
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

format_se <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
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
eb_dir <- file.path(clean_dir, "empirical_bayes_school_va")
table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")

dir.create(eb_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
school_values_path <- Sys.getenv(
  "EB_SCHOOL_VALUES_INPUT_PATH",
  unset = file.path(eb_dir, "eb_school_rbd_observational_values.csv")
)
program_income_path <- Sys.getenv(
  "PROGRAM_INCOME_OUTCOMES_PATH",
  unset = file.path(
    repo_wd,
    "output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv"
  )
)

regression_csv <- Sys.getenv(
  "EB_IV_REGRESSION_OUTPUT_PATH",
  unset = file.path(eb_dir, "scalar_school_value_iv_expected_va_eb.csv")
)
results_csv <- Sys.getenv(
  "EB_IV_RESULTS_OUTPUT_PATH",
  unset = file.path(table_dir, "scalar_school_value_iv_results_expected_va_eb.csv")
)
main_table_csv <- Sys.getenv(
  "EB_IV_MAIN_TABLE_CSV",
  unset = file.path(table_dir, "scalar_school_value_iv_main_four_expected_va_eb.csv")
)
main_table_tex <- Sys.getenv(
  "EB_IV_MAIN_TABLE_TEX",
  unset = file.path(table_dir, "scalar_school_value_iv_main_four_expected_va_eb.tex")
)
accreditation_table_csv <- Sys.getenv(
  "EB_IV_ACCREDITATION_TABLE_CSV",
  unset = file.path(table_dir, "scalar_school_value_iv_accreditation_expected_va_eb.csv")
)
accreditation_table_tex <- Sys.getenv(
  "EB_IV_ACCREDITATION_TABLE_TEX",
  unset = file.path(table_dir, "scalar_school_value_iv_accreditation_expected_va_eb.tex")
)
program_income_table_csv <- Sys.getenv(
  "EB_IV_PROGRAM_INCOME_TABLE_CSV",
  unset = file.path(table_dir, "scalar_school_value_iv_program_income_expected_va_eb.csv")
)
program_income_table_tex <- Sys.getenv(
  "EB_IV_PROGRAM_INCOME_TABLE_TEX",
  unset = file.path(table_dir, "scalar_school_value_iv_program_income_expected_va_eb.tex")
)
diagnostics_csv <- Sys.getenv(
  "EB_IV_DIAGNOSTICS_OUTPUT_PATH",
  unset = file.path(eb_dir, "scalar_school_value_iv_expected_va_eb_diagnostics.csv")
)

value_specs <- data.table(
  spec = c(
    "math_adj_eb",
    "leng_adj_eb",
    "stem_adj_eb",
    "program_income_adj_eb",
    "progcert_adj_eb",
    "instcert_adj_eb"
  ),
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "stem_enrollment_m1",
    "log_program_income_clp_m1",
    "program_certified_years_m1",
    "inst_certified_years_m1"
  ),
  value_outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "stem_enrollment_m1",
    "log_program_income_clp_m1",
    "program_certified_years_m1",
    "inst_certified_years_m1"
  ),
  value_column = "controlled_value_added_eb_centered_student",
  outcome_group = c(
    "Math",
    "Language",
    "STEM enrollment",
    "Program income",
    "Program-certified years",
    "Institutional quality"
  )
)

configured_spec_filter <- parse_env_list("EB_IV_VALUE_SPECS")
if (length(configured_spec_filter) > 0) {
  missing_filtered_specs <- setdiff(configured_spec_filter, value_specs$spec)
  if (length(missing_filtered_specs) > 0) {
    stop(
      "EB_IV_VALUE_SPECS includes specs not configured for EB IV: ",
      paste(missing_filtered_specs, collapse = ", "),
      call. = FALSE
    )
  }
  value_specs <- value_specs[spec %chin% configured_spec_filter]
}
if (nrow(value_specs) == 0) {
  stop("No EB IV value specs configured after applying EB_IV_VALUE_SPECS.", call. = FALSE)
}

main_specs <- c("math_adj_eb", "leng_adj_eb", "stem_adj_eb", "instcert_adj_eb")
accreditation_specs <- c("progcert_adj_eb", "instcert_adj_eb")
program_income_specs <- c("program_income_adj_eb")

message("Reading EB-shrunken school values: ", school_values_path)
school_values_raw <- fread(school_values_path, na.strings = c("", "NA"))
missing_value_cols <- setdiff(
  c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"),
  names(school_values_raw)
)
if (length(missing_value_cols) > 0) {
  stop(
    "EB school value file is missing required columns: ",
    paste(missing_value_cols, collapse = ", "),
    ". Run 01_construct_eb_school_values.R first.",
    call. = FALSE
  )
}

school_values_raw <- school_values_raw[
  analysis_sample == "All" &
    outcome %chin% value_specs$value_outcome,
  .(school_rbd, outcome, controlled_value_added_eb_centered_student)
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
missing_value_specs <- setdiff(value_specs$spec, unique(school_values_long$spec))
if (length(missing_value_specs) > 0) {
  stop(
    "EB school value file has no usable school values for specs: ",
    paste(missing_value_specs, collapse = ", "),
    call. = FALSE
  )
}

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
  "ACREDITADA_CARR_m1",
  "ACREDITADA_INST_m1",
  "ACRE_INST_ANIO_m1",
  "program_certified_years_m1",
  "institution_accredited_m1"
)
universe <- fread(universe_path, select = universe_cols, na.strings = c("", "NA"))
universe[, `:=`(
  student_id = as.numeric(student_id),
  mrun = as.numeric(mrun),
  sae_proceso = as.integer(sae_proceso),
  rbd_treated_1R = as.numeric(rbd_treated_1R),
  most_time_RBD = as.numeric(most_time_RBD)
)]

message("Reading program_income outcome columns: ", program_income_path)
if (!file.exists(program_income_path)) {
  stop(
    "Program income outcome file does not exist: ",
    program_income_path,
    ". Run code/codex/mifuturo_matricula_income/03_construct_person_level_income_outcomes.R first.",
    call. = FALSE
  )
}
program_income <- fread(
  program_income_path,
  select = c(
    "mrun",
    "program_income_clp_m1",
    "log_program_income_clp_m1",
    "program_income_source_m1",
    "program_income_missing_m1"
  ),
  na.strings = c("", "NA")
)
program_income[, mrun := as.numeric(mrun)]
if (anyDuplicated(program_income$mrun) > 0) {
  stop("Program income outcome file is not unique at mrun level.", call. = FALSE)
}
universe <- merge(
  universe,
  program_income,
  by = "mrun",
  all.x = TRUE,
  sort = FALSE
)

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
universe[, observed_matricula_m1 := !is.na(ACREDITADA_CARR_m1) |
  !is.na(ACREDITADA_INST_m1) |
  !is.na(ACRE_INST_ANIO_m1)]
universe[
  is.na(program_certified_years_m1) & !observed_matricula_m1,
  program_certified_years_m1 := 0
]
universe[, inst_certified_years_m1 := fcase(
  institution_accredited_m1 == 1L, as.numeric(ACRE_INST_ANIO_m1),
  institution_accredited_m1 == 0L, 0,
  is.na(institution_accredited_m1) & !observed_matricula_m1, 0,
  default = NA_real_
)]

message("Reading long DA probability files and computing expected EB VA.")
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

message("Merging estimation sample and EB scalar school values.")
estimation_df <- merge(
  universe,
  prob_summary,
  by = c("student_id", "sae_proceso"),
  all = FALSE,
  sort = FALSE
)
estimation_df <- estimation_df[timely_sae == 1 & any_risk == 1]
estimation_df[, admission_exam_taker := as.integer(
  !is.na(z_year_math_max) | !is.na(z_year_leng_max)
)]
estimation_df <- estimation_df[admission_exam_taker == 1L]

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
  "n_positive_probability_options", "admission_exam_taker", "GEN_ALU", "EDAD_ALU",
  "z_sim_mat_4to", "z_sim_leng_4to",
  "z_year_math_max", "z_year_leng_max", "stem_enrollment_m1",
  "program_income_clp_m1", "log_program_income_clp_m1",
  "program_income_source_m1", "program_income_missing_m1",
  "program_certified_years_m1", "inst_certified_years_m1",
  paste0("expected_", value_names),
  paste0("mass_with_value_", value_names),
  paste0("d_", value_names),
  paste0("z_", value_names)
)
estimation_df <- estimation_df[, ..keep_cols]

message("Writing EB regression dataframe: ", regression_csv)
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

message("Running EB expected-VA scalar IV specs.")
setFixest_nthreads(0)
results <- rbindlist(lapply(seq_len(nrow(value_specs)), function(i) {
  message("  spec: ", value_specs$spec[i])
  run_iv_spec(estimation_df, value_specs[i])
}), use.names = TRUE)

table_out <- merge(
  value_specs,
  results,
  by = "spec",
  all.x = TRUE,
  sort = FALSE
)

message("Writing EB IV results: ", results_csv)
fwrite(results, results_csv)

make_two_row_table <- function(dt, specs, caption, label, path) {
  table_dt <- copy(dt[spec %chin% specs])
  table_dt[, spec_order := match(spec, specs)]
  setorder(table_dt, spec_order)

  theta_row <- paste0(
    "$\\theta^{EB}$ & ",
    paste(format_estimate(table_dt$beta), collapse = " & "),
    " \\\\"
  )
  se_row <- paste0(
    "SE & ",
    paste(format_se(table_dt$se), collapse = " & "),
    " \\\\"
  )
  n_row <- paste0(
    "Observations & ",
    paste(format(round(table_dt$n_obs), big.mark = ","), collapse = " & "),
    " \\\\"
  )
  header <- paste(table_dt$outcome_group, collapse = " & ")
  align <- paste(rep("c", nrow(table_dt)), collapse = "")

  latex <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{l", align, "}"),
    "\\toprule",
    paste0(" & ", header, " \\\\"),
    "\\midrule",
    theta_row,
    se_row,
    n_row,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(latex, path)
}

write_spec_table <- function(table_out, specs, csv_path, tex_path, caption, label) {
  out <- table_out[spec %chin% specs]
  if (nrow(out) == 0) {
    return(character())
  }
  fwrite(
    out[, .(spec, outcome_group, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f)],
    csv_path
  )
  make_two_row_table(out, specs, caption, label, tex_path)
  c(csv_path, tex_path)
}

written_paths <- c(regression_csv, results_csv)
written_paths <- c(
  written_paths,
  write_spec_table(
    table_out,
    main_specs,
    main_table_csv,
    main_table_tex,
    "Main scalar school-value IV estimates using EB-shrunken value added",
    "tab:scalar_school_value_iv_main_four_expected_va_eb"
  )
)
written_paths <- c(
  written_paths,
  write_spec_table(
    table_out,
    accreditation_specs,
    accreditation_table_csv,
    accreditation_table_tex,
    "Accreditation-years scalar school-value IV estimates using EB-shrunken value added",
    "tab:scalar_school_value_iv_accreditation_expected_va_eb"
  )
)
written_paths <- c(
  written_paths,
  write_spec_table(
    table_out,
    program_income_specs,
    program_income_table_csv,
    program_income_table_tex,
    "Program-income scalar school-value IV estimate using EB-shrunken value added",
    "tab:scalar_school_value_iv_program_income_expected_va_eb"
  )
)

diagnostics <- data.table(
  measure = c(
    "universe_rows",
    "probability_student_process_rows",
    "estimation_rows_exam_takers_timely_at_risk",
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
written_paths <- c(written_paths, diagnostics_csv)

for (path in written_paths) {
  message("Wrote: ", path)
}
print(table_out[, .(spec, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f)])
