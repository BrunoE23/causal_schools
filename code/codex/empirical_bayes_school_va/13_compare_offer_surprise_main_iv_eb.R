suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
input_csv <- file.path(
  root_dir,
  "output/tables/empirical_bayes_school_va",
  "stata_scalar_school_value_iv_main_five_expected_va_eb_regression_df.csv"
)
output_dir <- file.path(root_dir, "output/tables/empirical_bayes_school_va")
comparison_csv <- file.path(
  output_dir, "main_five_level_vs_attendance_offer_surprise_iv_eb.csv"
)
surprise_tex <- file.path(
  output_dir, "main_five_attendance_offer_surprise_iv_eb.tex"
)

if (!file.exists(input_csv)) stop("Missing regression dataframe: ", input_csv)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(input_csv)

specs <- data.table(
  outcome_key = c("math", "verbal", "institution", "field", "income"),
  outcome_label = c(
    "Math", "Verbal", "High-premium inst.",
    "High-premium field", "Projected income"
  ),
  y = c(
    "z_year_math_max",
    "z_year_leng_max",
    "high_inst_m1",
    "high_paying_field_m1",
    "log_program_income_full_clp_m1"
  ),
  treatment = c(
    "d_math_adj_eb",
    "d_leng_adj_eb",
    "d_highinst_adj_eb",
    "d_highpay_adj_eb",
    "d_program_income_full_adj_eb"
  ),
  offered = c(
    "z_math_adj_eb",
    "z_leng_adj_eb",
    "z_highinst_adj_eb",
    "z_highpay_adj_eb",
    "z_program_income_full_adj_eb"
  ),
  expected = c(
    "expected_math_adj_eb",
    "expected_leng_adj_eb",
    "expected_highinst_adj_eb",
    "expected_highpay_adj_eb",
    "expected_program_income_full_adj_eb"
  )
)

extract_iv_term <- function(model, treatment) {
  term <- paste0("fit_", treatment)
  if (!term %chin% names(coef(model))) {
    stop("Missing IV coefficient: ", term, call. = FALSE)
  }
  term
}

estimate_spec <- function(spec) {
  controls <- c(
    "factor(cohort_gr8)",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "factor(GEN_ALU)",
    "factor(EDAD_ALU)",
    spec$expected
  )
  needed <- c(
    spec$y, spec$treatment, spec$offered, spec$expected,
    "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to",
    "GEN_ALU", "EDAD_ALU", "admission_exam_taker"
  )
  reg_dt <- copy(dt[
    admission_exam_taker == 1L & complete.cases(dt[, ..needed])
  ])
  reg_dt[, offer_surprise := get(spec$offered) - get(spec$expected)]
  reg_dt[, attended_surprise := get(spec$treatment) - get(spec$expected)]

  control_rhs <- paste(controls, collapse = " + ")
  original_formula <- as.formula(paste0(
    spec$y, " ~ ", control_rhs,
    " | 0 | ", spec$treatment, " ~ ", spec$offered
  ))
  surprise_formula <- as.formula(paste0(
    spec$y, " ~ ", control_rhs,
    " | 0 | attended_surprise ~ offer_surprise"
  ))

  original_model <- feols(
    original_formula, data = reg_dt, vcov = "hetero", notes = FALSE
  )
  surprise_model <- feols(
    surprise_formula, data = reg_dt, vcov = "hetero", notes = FALSE
  )
  original_term <- extract_iv_term(original_model, spec$treatment)
  surprise_term <- extract_iv_term(surprise_model, "attended_surprise")

  original_beta <- coef(original_model)[[original_term]]
  surprise_beta <- coef(surprise_model)[[surprise_term]]
  original_se <- se(original_model)[[original_term]]
  surprise_se <- se(surprise_model)[[surprise_term]]

  original_fs_formula <- as.formula(paste0(
    spec$treatment, " ~ ", spec$offered, " + ", control_rhs
  ))
  surprise_fs_formula <- as.formula(paste0(
    "attended_surprise ~ offer_surprise + ", control_rhs
  ))
  original_fs <- feols(
    original_fs_formula, data = reg_dt, vcov = "hetero", notes = FALSE
  )
  surprise_fs <- feols(
    surprise_fs_formula, data = reg_dt, vcov = "hetero", notes = FALSE
  )

  original_fs_beta <- coef(original_fs)[[spec$offered]]
  original_fs_se <- se(original_fs)[[spec$offered]]
  surprise_fs_beta <- coef(surprise_fs)[["offer_surprise"]]
  surprise_fs_se <- se(surprise_fs)[["offer_surprise"]]

  data.table(
    outcome_key = spec$outcome_key,
    outcome_label = spec$outcome_label,
    treatment = spec$treatment,
    surprise_treatment = paste0(spec$treatment, " - ", spec$expected),
    original_instrument = spec$offered,
    surprise_instrument = paste0(spec$offered, " - ", spec$expected),
    risk_control = spec$expected,
    original_beta = original_beta,
    surprise_beta = surprise_beta,
    beta_difference = surprise_beta - original_beta,
    original_se = original_se,
    surprise_se = surprise_se,
    se_difference = surprise_se - original_se,
    original_p_value = 2 * pnorm(-abs(original_beta / original_se)),
    surprise_p_value = 2 * pnorm(-abs(surprise_beta / surprise_se)),
    original_first_stage_beta = original_fs_beta,
    original_first_stage_se = original_fs_se,
    surprise_first_stage_beta = surprise_fs_beta,
    surprise_first_stage_se = surprise_fs_se,
    original_first_stage_f = (original_fs_beta / original_fs_se)^2,
    surprise_first_stage_f = (surprise_fs_beta / surprise_fs_se)^2,
    max_abs_first_stage_fitted_difference = max(
      abs(fitted(original_fs) - reg_dt[[spec$expected]] - fitted(surprise_fs)),
      na.rm = TRUE
    ),
    n_obs = nobs(surprise_model)
  )
}

setFixest_nthreads(0)
results <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  estimate_spec(specs[i])
}))
fwrite(results, comparison_csv)

stars <- function(p) {
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

estimate_row <- paste0(
  "$\\varphi^{EB}$ & ",
  paste(vapply(seq_len(nrow(results)), function(i) {
    sprintf("%.3f%s", results$surprise_beta[i], stars(results$surprise_p_value[i]))
  }, character(1)), collapse = " & "),
  " \\\\"
)
se_row <- paste0(
  " & ",
  paste(sprintf("(%.3f)", results$surprise_se), collapse = " & "),
  " \\\\"
)
n_row <- paste0(
  "N & ",
  paste(format(results$n_obs, big.mark = ",", scientific = FALSE), collapse = " & "),
  " \\\\"
)
first_stage_row <- paste0(
  "$O-E$ & ",
  paste(vapply(seq_len(nrow(results)), function(i) {
    p_value <- 2 * pnorm(-abs(
      results$surprise_first_stage_beta[i] /
        results$surprise_first_stage_se[i]
    ))
    sprintf(
      "%.3f%s", results$surprise_first_stage_beta[i], stars(p_value)
    )
  }, character(1)), collapse = " & "),
  " \\\\"
)
first_stage_se_row <- paste0(
  " & ",
  paste(sprintf("(%.3f)", results$surprise_first_stage_se), collapse = " & "),
  " \\\\"
)
first_stage_f_row <- paste0(
  "First-stage $F$ & ",
  paste(sprintf("%.1f", results$surprise_first_stage_f), collapse = " & "),
  " \\\\"
)

tex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  paste0(
    "\\caption{Main scalar school-value IV estimates using deviations from ",
    "expected value added}"
  ),
  "\\label{tab:scalar_school_value_iv_main_five_surprise_eb}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Exams} & \\multicolumn{3}{c}{Higher ed. choices} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-6}",
  paste0(" & ", paste(results$outcome_label, collapse = " & "), " \\\\"),
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel A. First stage: attended-school deviation }$A-E$} \\\\",
  first_stage_row,
  first_stage_se_row,
  first_stage_f_row,
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel B. Second stage: student outcome}} \\\\",
  estimate_row,
  se_row,
  n_row,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "Notes: Each column reports a scalar IV estimate of the pass-through from ",
    "attended-school EB value added relative to its DA-probability expected ",
    "value, $A-E$, to the corresponding student outcome. $A-E$ is instrumented ",
    "with first-round offered-school EB value added relative to the same expected ",
    "value, $O-E$. All specifications continue to control for $E$, cohort, ",
    "grade-4 SIMCE math and language, gender, and age. Heteroskedasticity-robust ",
    "standard errors are reported in parentheses. The first-stage $F$ statistic ",
    "tests the excluded instrument $O-E$. $^{*}p<0.10$, ",
    "$^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(tex_lines, surprise_tex)

message("Wrote: ", comparison_csv)
message("Wrote: ", surprise_tex)
print(results)
