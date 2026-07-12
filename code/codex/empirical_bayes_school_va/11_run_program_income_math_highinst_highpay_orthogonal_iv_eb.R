###############################################################################
# Joint IV for math, high-premium-institution, and high-premium-field EB VA
#
# Sequential orthogonalization:
#   1. math VA
#   2. high-premium-institution VA residualized on math VA
#   3. high-premium-field VA residualized on math and high-premium-institution VA
#
# Outcome:
#   log(program_income_full)
#
# The residual from any school-level projection is not included. These are
# orthogonalized school-value dimensions used directly as IV treatments.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

find_existing_dir <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, ".", call. = FALSE)
  }
  candidates[[1]]
}

find_existing_file <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, ".", call. = FALSE)
  }
  candidates[[1]]
}

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  sum(w[ok] * x[ok]) / sum(w[ok])
}

weighted_center <- function(x, w) {
  x - weighted_mean(x, w)
}

weighted_projection <- function(x, z, w) {
  if (is.null(z) || ncol(z) == 0) {
    return(rep(0, length(x)))
  }
  sw <- sqrt(w)
  fit <- lm.wfit(x = z * sw, y = x * sw, w = rep(1, length(x)))
  as.vector(z %*% fit$coefficients)
}

weighted_residualize <- function(x, z, w) {
  x - weighted_projection(x, z, w)
}

weighted_var <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  sum(w[ok] * x[ok]^2) / sum(w[ok])
}

weighted_cor <- function(x, y, w) {
  ok <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
  x <- x[ok]
  y <- y[ok]
  w <- w[ok]
  x <- x - sum(w * x) / sum(w)
  y <- y - sum(w * y) / sum(w)
  denom <- sqrt(sum(w * x^2) / sum(w) * sum(w * y^2) / sum(w))
  if (is.na(denom) || denom == 0) {
    return(NA_real_)
  }
  sum(w * x * y) / sum(w) / denom
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

significance_stars <- function(p_value) {
  fifelse(
    is.na(p_value),
    "",
    fifelse(
      p_value < 0.01,
      "***",
      fifelse(p_value < 0.05, "**", fifelse(p_value < 0.10, "*", ""))
    )
  )
}

format_se <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
}

repo_wd <- find_existing_dir(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(getwd(), "C:/Users/brunem/Research/causal_schools"),
  "repository root"
)
data_wd <- find_existing_dir(
  "CAUSAL_SCHOOLS_DATA_WD",
  c("C:/Users/brunem/Dropbox/causal_schools", "C:/Users/xd-br/Dropbox/causal_schools"),
  "data root"
)

clean_dir <- file.path(data_wd, "data", "clean")
prob_dir <- file.path(clean_dir, "DA_probs")
out_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

school_values_path <- find_existing_file(
  "ORTH_MATH_HIGHINST_HIGHPAY_SCHOOL_VALUES",
  file.path(out_dir, "stata_eb_school_rbd_observational_values_for_iv.csv"),
  "school EB VA input"
)
base_regression_path <- find_existing_file(
  "ORTH_MATH_HIGHINST_HIGHPAY_BASE_REGRESSION_DF",
  file.path(out_dir, "stata_scalar_school_value_iv_main_five_expected_va_eb_regression_df.csv"),
  "main-five EB IV regression dataframe"
)

school_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_SCHOOL_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_school_values.csv")
)
summary_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_SUMMARY_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_summary.csv")
)
correlation_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_CORRELATION_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_correlations.csv")
)
regression_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_REGRESSION_DF_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_regression_df.csv")
)
results_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_RESULTS_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_joint_iv.csv")
)
first_stage_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_FIRST_STAGE_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_joint_iv_first_stage.csv")
)
table_output <- Sys.getenv(
  "ORTH_MATH_HIGHINST_HIGHPAY_TABLE_OUTPUT",
  unset = file.path(out_dir, "program_income_full_orthogonal_math_highinst_highpay_joint_iv.tex")
)

outcomes <- c(
  math = "z_year_math_max",
  highinst = "high_inst_m1",
  highpay = "high_paying_field_m1",
  income = "log_program_income_full_clp_m1"
)

message("Reading school EB VA: ", school_values_path)
school_long <- fread(
  school_values_path,
  select = c(
    "school_rbd",
    "analysis_sample",
    "outcome",
    "n_students_regression",
    "controlled_value_added_eb_centered_student"
  ),
  na.strings = c("", "NA")
)[analysis_sample == "All" & outcome %chin% unname(outcomes)]

missing_outcomes <- setdiff(unname(outcomes), unique(school_long$outcome))
if (length(missing_outcomes) > 0) {
  stop("Missing EB VA outcomes: ", paste(missing_outcomes, collapse = ", "), call. = FALSE)
}

school_wide <- dcast(
  school_long,
  school_rbd ~ outcome,
  value.var = c("controlled_value_added_eb_centered_student", "n_students_regression")
)
setnames(
  school_wide,
  paste0("controlled_value_added_eb_centered_student_", unname(outcomes)),
  paste0(names(outcomes), "_va")
)
setnames(
  school_wide,
  paste0("n_students_regression_", unname(outcomes)),
  paste0(names(outcomes), "_n")
)
school_wide[, school_rbd := as.numeric(school_rbd)]

needed_school_cols <- c("math_va", "highinst_va", "highpay_va", "income_n")
work <- school_wide[complete.cases(school_wide[, ..needed_school_cols]) & income_n > 0]
if (nrow(work) == 0) {
  stop("No schools have complete math/high-inst/high-pay inputs.", call. = FALSE)
}

w <- work$income_n
math_centered <- weighted_center(work$math_va, w)
highinst_centered <- weighted_center(work$highinst_va, w)
highpay_centered <- weighted_center(work$highpay_va, w)

math_orth <- math_centered
highinst_orth <- weighted_residualize(highinst_centered, cbind(math_orth), w)
highpay_orth <- weighted_residualize(highpay_centered, cbind(math_orth, highinst_orth), w)

component_values <- data.table(
  school_rbd = work$school_rbd,
  math_highinst_highpay_math_orth_va = math_orth,
  math_highinst_highpay_highinst_orth_va = highinst_orth,
  math_highinst_highpay_highpay_orth_va = highpay_orth
)

school_components <- merge(
  school_wide,
  component_values,
  by = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

component_cols <- names(component_values)[names(component_values) != "school_rbd"]
component_labels <- c(
  math_highinst_highpay_math_orth_va = "Math VA",
  math_highinst_highpay_highinst_orth_va = "High-premium-institution VA orthogonal to math",
  math_highinst_highpay_highpay_orth_va = "High-premium-field VA orthogonal to math and high-premium institution"
)

summary_dt <- data.table(
  component = unname(component_labels),
  component_col = component_cols,
  orthogonalized_against = c(
    "weighted mean only",
    "Math VA",
    "Math VA + high-premium-institution VA orthogonal to math"
  ),
  weighted_sd = sapply(component_cols, function(col) sqrt(weighted_var(component_values[[col]], w))),
  weighted_correlation_with_income_va = sapply(component_cols, function(col) {
    weighted_cor(component_values[[col]], work$income_va, w)
  }),
  n_schools_common = nrow(work),
  n_schools_input = nrow(school_wide)
)
summary_dt[, decomposition_order := "math_highinst_highpay"]

cor_vars <- c("math_va", "highinst_va", "highpay_va", "income_va", component_cols)
correlation_dt <- rbindlist(lapply(cor_vars, function(var1) {
  rbindlist(lapply(cor_vars, function(var2) {
    x <- if (var1 %chin% names(work)) work[[var1]] else component_values[[var1]]
    y <- if (var2 %chin% names(work)) work[[var2]] else component_values[[var2]]
    data.table(
      variable_1 = var1,
      variable_2 = var2,
      weighted_correlation = weighted_cor(x, y, w)
    )
  }))
}))

message("Writing school components: ", school_output)
fwrite(school_components, school_output)
message("Writing component summary: ", summary_output)
fwrite(summary_dt, summary_output)
message("Writing component correlations: ", correlation_output)
fwrite(correlation_dt, correlation_output)

message("Reading base regression dataframe: ", base_regression_path)
base_cols <- c(
  "student_id",
  "mrun",
  "cohort_gr8",
  "sae_proceso",
  "timely_sae",
  "rbd_treated_1R",
  "most_time_RBD",
  "any_risk",
  "admission_exam_taker",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "log_program_income_full_clp_m1"
)
estimation_df <- fread(base_regression_path, select = base_cols, na.strings = c("", "NA"))
estimation_df <- estimation_df[timely_sae == 1L & any_risk == 1L]
estimation_df[, `:=`(
  student_id = as.numeric(student_id),
  sae_proceso = as.integer(sae_proceso),
  rbd_treated_1R = as.numeric(rbd_treated_1R),
  most_time_RBD = as.numeric(most_time_RBD)
)]

component_school_values <- component_values
setnames(
  component_school_values,
  component_cols,
  c("school_value_math_orth", "school_value_highinst_orth", "school_value_highpay_orth")
)

message("Reading DA probabilities and computing expected orthogonal VA.")
prob_list <- lapply(2018:2021, function(year) {
  path <- file.path(prob_dir, paste0("DA_probs_", year, ".csv"))
  if (!file.exists(path)) {
    stop("Missing DA probability file: ", path, call. = FALSE)
  }
  dt <- fread(path, select = c("student_id", "school_id", "prob"))
  dt[, sae_proceso := as.integer(year)]
  dt[, rbd_prob := suppressWarnings(as.numeric(sub("_.*$", "", school_id)))]
  dt[, .(student_id = as.numeric(student_id), sae_proceso, rbd_prob, prob)]
})
prob_long <- rbindlist(prob_list, use.names = TRUE)
prob_long <- merge(
  prob_long,
  component_school_values,
  by.x = "rbd_prob",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)
prob_summary <- prob_long[, .(
  expected_math_orth_va = sum(prob * fifelse(is.na(school_value_math_orth), 0, school_value_math_orth), na.rm = TRUE),
  expected_highinst_orth_va = sum(prob * fifelse(is.na(school_value_highinst_orth), 0, school_value_highinst_orth), na.rm = TRUE),
  expected_highpay_orth_va = sum(prob * fifelse(is.na(school_value_highpay_orth), 0, school_value_highpay_orth), na.rm = TRUE),
  mass_with_value_math_orth_va = sum(prob * as.integer(!is.na(school_value_math_orth)), na.rm = TRUE),
  mass_with_value_highinst_orth_va = sum(prob * as.integer(!is.na(school_value_highinst_orth)), na.rm = TRUE),
  mass_with_value_highpay_orth_va = sum(prob * as.integer(!is.na(school_value_highpay_orth)), na.rm = TRUE)
), by = .(student_id, sae_proceso)]
rm(prob_long)
gc(verbose = FALSE)

estimation_df <- merge(
  estimation_df,
  prob_summary,
  by = c("student_id", "sae_proceso"),
  all.x = TRUE,
  sort = FALSE
)

attended_values <- copy(component_school_values)
setnames(
  attended_values,
  c("school_value_math_orth", "school_value_highinst_orth", "school_value_highpay_orth"),
  c("d_math_orth_va", "d_highinst_orth_va", "d_highpay_orth_va")
)
estimation_df <- merge(
  estimation_df,
  attended_values,
  by.x = "most_time_RBD",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

offered_values <- copy(component_school_values)
setnames(
  offered_values,
  c("school_value_math_orth", "school_value_highinst_orth", "school_value_highpay_orth"),
  c("offered_math_orth_va", "offered_highinst_orth_va", "offered_highpay_orth_va")
)
estimation_df <- merge(
  estimation_df,
  offered_values,
  by.x = "rbd_treated_1R",
  by.y = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

estimation_df[, z_math_orth_va := fifelse(
  is.na(rbd_treated_1R) | rbd_treated_1R == 0,
  0,
  offered_math_orth_va
)]
estimation_df[, z_highinst_orth_va := fifelse(
  is.na(rbd_treated_1R) | rbd_treated_1R == 0,
  0,
  offered_highinst_orth_va
)]
estimation_df[, z_highpay_orth_va := fifelse(
  is.na(rbd_treated_1R) | rbd_treated_1R == 0,
  0,
  offered_highpay_orth_va
)]
estimation_df[, c("offered_math_orth_va", "offered_highinst_orth_va", "offered_highpay_orth_va") := NULL]

message("Writing regression dataframe: ", regression_output)
fwrite(estimation_df, regression_output)

y <- "log_program_income_full_clp_m1"
endog <- c("d_math_orth_va", "d_highinst_orth_va", "d_highpay_orth_va")
instruments <- c("z_math_orth_va", "z_highinst_orth_va", "z_highpay_orth_va")
expected <- c("expected_math_orth_va", "expected_highinst_orth_va", "expected_highpay_orth_va")
controls <- c(
  "factor(cohort_gr8)",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "factor(GEN_ALU)",
  "factor(EDAD_ALU)",
  expected
)
needed <- c(
  y,
  endog,
  instruments,
  expected,
  "cohort_gr8",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "GEN_ALU",
  "EDAD_ALU",
  "admission_exam_taker"
)
reg_dt <- estimation_df[complete.cases(estimation_df[, ..needed]) & admission_exam_taker == 1L]
if (nrow(reg_dt) == 0) {
  stop("No complete observations for the joint orthogonal IV.", call. = FALSE)
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

message("Running joint IV.")
setFixest_nthreads(0)
iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)

treatment_labels <- c(
  d_math_orth_va = "Math VA",
  d_highinst_orth_va = "High-premium-institution VA net of math",
  d_highpay_orth_va = "High-premium-field VA net of first two"
)

results <- rbindlist(lapply(endog, function(term) {
  coef_term <- extract_coef(iv_model, term)
  beta <- coef(iv_model)[[coef_term]]
  se <- se(iv_model)[[coef_term]]
  data.table(
    outcome = y,
    treatment = term,
    treatment_label = treatment_labels[[term]],
    instruments = paste(instruments, collapse = " + "),
    beta = beta,
    se = se,
    zstat = beta / se,
    p_value = 2 * stats::pnorm(-abs(beta / se)),
    n_obs = nobs(iv_model)
  )
}), use.names = TRUE)

first_stage <- rbindlist(lapply(endog, function(d_col) {
  fs_formula <- as.formula(paste0(d_col, " ~ ", paste(c(instruments, controls), collapse = " + ")))
  fs_model <- feols(fs_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  rbindlist(lapply(instruments, function(z_col) {
    coef_term <- extract_coef(fs_model, z_col)
    beta <- coef(fs_model)[[coef_term]]
    se <- se(fs_model)[[coef_term]]
    data.table(
      endogenous = d_col,
      endogenous_label = treatment_labels[[d_col]],
      instrument = z_col,
      beta = beta,
      se = se,
      tstat = beta / se,
      n_obs = nobs(fs_model)
    )
  }))
}), use.names = TRUE)

message("Writing joint IV results: ", results_output)
fwrite(results, results_output)
message("Writing first stages: ", first_stage_output)
fwrite(first_stage, first_stage_output)

latex_rows <- c()
for (term in endog) {
  row <- results[treatment == term]
  latex_rows <- c(
    latex_rows,
    paste0(
      row$treatment_label,
      " & ",
      format_estimate(row$beta),
      significance_stars(row$p_value),
      " \\\\"
    ),
    paste0(" & ", format_se(row$se), " \\\\")
  )
}
latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Ordered IV horse race for log projected income}",
  "\\label{tab:program-income-orthogonal-math-highinst-highpay}",
  "\\begin{tabular}{lc}",
  "\\toprule",
  " & Log proj. income \\\\",
  "\\midrule",
  latex_rows,
  "\\midrule",
  paste0("N & ", format(results$n_obs[1], big.mark = ","), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "Notes: The dependent variable is log projected program income. ",
    "The table reports an ordered horse-race IV. Math VA enters first; ",
    "high-premium-institution VA is measured net of math VA; high-premium-field VA is ",
    "measured net of the first two dimensions. This ordering is used to aid interpretation ",
    "and does not make the coefficients structural returns to achievement, institutions, ",
    "or fields. Attended-school values are instrumented with the corresponding first-round ",
    "offered-school values, and the regression controls for the DA-probability expected ",
    "value of each dimension, cohort, grade-4 SIMCE math and language, gender, and age. ",
    "Heteroskedasticity-robust standard errors are reported in parentheses. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(latex_table, table_output)

message("Writing TeX table: ", table_output)
print(results)
