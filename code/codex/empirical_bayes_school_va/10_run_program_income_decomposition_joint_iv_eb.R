###############################################################################
# Joint IV for orthogonal components of program_income_full EB VA
#
# This runs one IV regression with the requested decomposition components as
# endogenous variables:
#
#   log(program_income_full) ~ attended components
#
# instrumented with their offered-school components. The residual component from
# the school-level decomposition is deliberately not included.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

find_existing_file <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.", call. = FALSE)
  }
  candidates[[1]]
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

parse_component_order <- function(value) {
  if (!nzchar(trimws(value))) {
    value <- "math,area,institution"
  }
  order <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  valid <- c("exam", "math", "area", "institution")
  if (length(order) == 0 || anyDuplicated(order) || any(!order %chin% valid)) {
    stop(
      "PROGRAM_INCOME_DECOMP_JOINT_COMPONENTS must contain unique values from: ",
      paste(valid, collapse = ", "),
      call. = FALSE
    )
  }
  order
}

repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
out_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

regression_df_path <- find_existing_file(
  "PROGRAM_INCOME_DECOMP_JOINT_REGRESSION_DF",
  file.path(out_dir, "stata_scalar_school_value_iv_program_income_decomposition_math_area_institution_expected_va_eb_regression_df.csv"),
  "program-income decomposition regression dataframe"
)
results_path <- Sys.getenv(
  "PROGRAM_INCOME_DECOMP_JOINT_RESULTS_PATH",
  unset = file.path(out_dir, "stata_scalar_school_value_iv_program_income_decomposition_math_area_institution_joint_no_residual.csv")
)
first_stage_path <- Sys.getenv(
  "PROGRAM_INCOME_DECOMP_JOINT_FIRST_STAGE_PATH",
  unset = file.path(out_dir, "stata_scalar_school_value_iv_program_income_decomposition_math_area_institution_joint_no_residual_first_stage.csv")
)

all_component_specs <- c(
  exam = "program_income_full_exam_component_adj_eb",
  math = "program_income_full_math_component_adj_eb",
  area = "program_income_full_area_component_adj_eb",
  institution = "program_income_full_institution_component_adj_eb"
)
component_order <- parse_component_order(Sys.getenv(
  "PROGRAM_INCOME_DECOMP_JOINT_COMPONENTS",
  unset = "math,area,institution"
))
component_specs <- all_component_specs[component_order]

y <- "log_program_income_full_clp_m1"
d_cols <- paste0("d_", component_specs)
z_cols <- paste0("z_", component_specs)
expected_cols <- paste0("expected_", component_specs)
names(d_cols) <- names(component_specs)
names(z_cols) <- names(component_specs)
names(expected_cols) <- names(component_specs)
controls <- c(
  "factor(cohort_gr8)",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "factor(GEN_ALU)",
  "factor(EDAD_ALU)",
  expected_cols
)
needed_cols <- unname(c(
  y,
  d_cols,
  z_cols,
  expected_cols,
  "admission_exam_taker",
  "cohort_gr8",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "GEN_ALU",
  "EDAD_ALU"
))

message("Reading decomposition regression dataframe: ", regression_df_path)
dt <- fread(regression_df_path, select = needed_cols, na.strings = c("", "NA"))
reg_dt <- dt[complete.cases(dt[, ..needed_cols]) & admission_exam_taker == 1L]
if (nrow(reg_dt) == 0) {
  stop("No complete observations for joint decomposition IV.", call. = FALSE)
}

iv_formula <- as.formula(paste0(
  y,
  " ~ ",
  paste(controls, collapse = " + "),
  " | 0 | ",
  paste(d_cols, collapse = " + "),
  " ~ ",
  paste(z_cols, collapse = " + ")
))

message("Running joint IV without residual component.")
setFixest_nthreads(0)
iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)

results <- rbindlist(lapply(names(component_specs), function(component_name) {
  d_col <- d_cols[[component_name]]
  coef_term <- extract_coef(iv_model, d_col)
  beta <- coef(iv_model)[[coef_term]]
  se <- se(iv_model)[[coef_term]]
  zstat <- beta / se
  data.table(
    component = component_name,
    spec = component_specs[[component_name]],
    outcome = y,
    treatment = d_col,
    instruments = paste(z_cols, collapse = " + "),
    beta = beta,
    se = se,
    zstat = zstat,
    p_value = 2 * stats::pnorm(-abs(zstat)),
    n_obs = nobs(iv_model)
  )
}), use.names = TRUE)

first_stage <- rbindlist(lapply(names(component_specs), function(component_name) {
  d_col <- d_cols[[component_name]]
  fs_formula <- as.formula(paste0(d_col, " ~ ", paste(c(z_cols, controls), collapse = " + ")))
  fs_model <- feols(fs_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  rbindlist(lapply(z_cols, function(z_col) {
    coef_term <- extract_coef(fs_model, z_col)
    beta <- coef(fs_model)[[coef_term]]
    se <- se(fs_model)[[coef_term]]
    data.table(
      endogenous_component = component_name,
      endogenous = d_col,
      instrument = z_col,
      beta = beta,
      se = se,
      tstat = beta / se,
      n_obs = nobs(fs_model)
    )
  }))
}), use.names = TRUE)

message("Writing joint IV results: ", results_path)
fwrite(results, results_path)
message("Writing first stages: ", first_stage_path)
fwrite(first_stage, first_stage_path)

print(results)
