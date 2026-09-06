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
distribution_csv <- file.path(
  root_dir,
  "output/tables/results_section/primary_five_va_distribution.csv"
)
output_dir <- file.path(root_dir, "output/tables/empirical_bayes_school_va")
output_csv <- file.path(output_dir, "math_field_cross_outcome_iv_eb.csv")
output_tex <- file.path(output_dir, "math_field_cross_outcome_iv_eb.tex")
output_field_income_tex <- file.path(
  output_dir,
  "field_va_log_projected_income_cross_iv_eb.tex"
)
output_institution_income_tex <- file.path(
  output_dir,
  "institution_va_log_projected_income_cross_iv_eb.tex"
)
output_all_income_csv <- file.path(
  output_dir,
  "all_va_log_projected_income_cross_iv_eb.csv"
)
output_all_income_tex <- file.path(
  output_dir,
  "all_va_log_projected_income_cross_iv_eb.tex"
)
output_all_math_csv <- file.path(
  output_dir,
  "all_va_math_score_cross_iv_eb.csv"
)
output_all_math_tex <- file.path(
  output_dir,
  "all_va_math_score_cross_iv_eb.tex"
)
output_all_field_csv <- file.path(
  output_dir,
  "all_va_high_premium_field_cross_iv_eb.csv"
)
output_all_field_tex <- file.path(
  output_dir,
  "all_va_high_premium_field_cross_iv_eb.tex"
)

if (!file.exists(input_csv)) stop("Missing regression dataframe: ", input_csv)
if (!file.exists(distribution_csv)) stop("Missing VA distribution table: ", distribution_csv)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(input_csv)
va_distribution <- fread(distribution_csv)

gaps <- va_distribution[
  outcome_key %chin% c(
    "math", "language", "highpay", "highinst", "program_income_full"
  ),
  .(
  predictor = fcase(
    outcome_key == "math", "math",
    outcome_key == "language", "verbal",
    outcome_key == "highpay", "field",
    outcome_key == "highinst", "institution",
    outcome_key == "program_income_full", "income"
  ),
  p90_p10 = p90 - p10
  )
]

specs <- data.table(
  predictor = c(
    "math", "math", "field", "field", "field", "institution",
    "math", "verbal", "income",
    "verbal", "institution", "income",
    "verbal", "institution", "income"
  ),
  predictor_label = c(
    "Math VA", "Math VA",
    "High-premium-field VA", "High-premium-field VA",
    "High-premium-field VA",
    "High-premium-institution VA",
    "Math VA", "Verbal VA", "Projected-income VA",
    "Verbal VA", "High-premium-institution VA", "Projected-income VA",
    "Verbal VA", "High-premium-institution VA", "Projected-income VA"
  ),
  outcome = c(
    "math", "field", "math", "field", "income", "income",
    "income", "income", "income",
    "math", "math", "math",
    "field", "field", "field"
  ),
  outcome_label = c(
    "Math score", "High-premium field",
    "Math score", "High-premium field",
    "Log projected income", "Log projected income",
    "Log projected income", "Log projected income", "Log projected income",
    "Math score", "Math score", "Math score",
    "High-premium field", "High-premium field", "High-premium field"
  ),
  y = c(
    "z_year_math_max", "high_paying_field_m1",
    "z_year_math_max", "high_paying_field_m1",
    "log_program_income_full_clp_m1",
    "log_program_income_full_clp_m1",
    "log_program_income_full_clp_m1",
    "log_program_income_full_clp_m1",
    "log_program_income_full_clp_m1",
    "z_year_math_max",
    "z_year_math_max",
    "z_year_math_max",
    "high_paying_field_m1",
    "high_paying_field_m1",
    "high_paying_field_m1"
  ),
  treatment = c(
    "d_math_adj_eb", "d_math_adj_eb",
    "d_highpay_adj_eb", "d_highpay_adj_eb",
    "d_highpay_adj_eb",
    "d_highinst_adj_eb",
    "d_math_adj_eb",
    "d_leng_adj_eb",
    "d_program_income_full_adj_eb",
    "d_leng_adj_eb",
    "d_highinst_adj_eb",
    "d_program_income_full_adj_eb",
    "d_leng_adj_eb",
    "d_highinst_adj_eb",
    "d_program_income_full_adj_eb"
  ),
  instrument = c(
    "z_math_adj_eb", "z_math_adj_eb",
    "z_highpay_adj_eb", "z_highpay_adj_eb",
    "z_highpay_adj_eb",
    "z_highinst_adj_eb",
    "z_math_adj_eb",
    "z_leng_adj_eb",
    "z_program_income_full_adj_eb",
    "z_leng_adj_eb",
    "z_highinst_adj_eb",
    "z_program_income_full_adj_eb",
    "z_leng_adj_eb",
    "z_highinst_adj_eb",
    "z_program_income_full_adj_eb"
  ),
  expected = c(
    "expected_math_adj_eb", "expected_math_adj_eb",
    "expected_highpay_adj_eb", "expected_highpay_adj_eb",
    "expected_highpay_adj_eb",
    "expected_highinst_adj_eb",
    "expected_math_adj_eb",
    "expected_leng_adj_eb",
    "expected_program_income_full_adj_eb",
    "expected_leng_adj_eb",
    "expected_highinst_adj_eb",
    "expected_program_income_full_adj_eb",
    "expected_leng_adj_eb",
    "expected_highinst_adj_eb",
    "expected_program_income_full_adj_eb"
  )
)
specs <- gaps[specs, on = "predictor"]

controls_for <- function(expected) {
  c(
    "factor(cohort_gr8)",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "factor(GEN_ALU)",
    "factor(EDAD_ALU)",
    expected
  )
}

estimate_spec <- function(spec) {
  needed <- c(
    spec$y, spec$treatment, spec$instrument, spec$expected,
    "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to",
    "GEN_ALU", "EDAD_ALU", "admission_exam_taker"
  )
  reg_dt <- dt[
    admission_exam_taker == 1L &
      complete.cases(dt[, ..needed])
  ]

  control_rhs <- paste(controls_for(spec$expected), collapse = " + ")
  iv_formula <- as.formula(paste0(
    spec$y, " ~ ", control_rhs,
    " | 0 | ", spec$treatment, " ~ ", spec$instrument
  ))
  first_stage_formula <- as.formula(paste0(
    spec$treatment, " ~ ", spec$instrument, " + ", control_rhs
  ))

  iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  iv_term <- paste0("fit_", spec$treatment)
  beta <- coef(iv_model)[[iv_term]]
  std_error <- se(iv_model)[[iv_term]]
  z_stat <- beta / std_error
  p_value <- 2 * pnorm(-abs(z_stat))

  first_stage <- feols(
    first_stage_formula,
    data = reg_dt,
    vcov = "hetero",
    notes = FALSE
  )
  fs_beta <- coef(first_stage)[[spec$instrument]]
  fs_se <- se(first_stage)[[spec$instrument]]

  data.table(
    predictor = spec$predictor,
    predictor_label = spec$predictor_label,
    outcome = spec$outcome,
    outcome_label = spec$outcome_label,
    treatment = spec$treatment,
    instrument = spec$instrument,
    risk_control = spec$expected,
    beta = beta,
    se = std_error,
    z_stat = z_stat,
    p_value = p_value,
    n_obs = nobs(iv_model),
    first_stage_f = (fs_beta / fs_se)^2,
    p90_p10_predictor = spec$p90_p10,
    implied_p90_p10_effect = beta * spec$p90_p10
  )
}

results <- rbindlist(lapply(seq_len(nrow(specs)), function(i) estimate_spec(specs[i])))
fwrite(results, output_csv)

stars <- function(p) {
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

cell <- function(predictor, outcome) {
  predictor_key <- predictor
  outcome_key <- outcome
  row <- results[
    predictor == predictor_key & outcome == outcome_key
  ]
  list(
    estimate = sprintf("%.3f%s", row$beta, stars(row$p_value)),
    std_error = sprintf("(%.3f)", row$se)
  )
}

math_math <- cell("math", "math")
math_field <- cell("math", "field")
field_math <- cell("field", "math")
field_field <- cell("field", "field")

tex_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Math score} & \\multicolumn{2}{c}{High-premium field} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "School value-added index & Estimate & SE & Estimate & SE \\\\",
  "\\midrule",
  sprintf(
    "Math VA & %s & %s & %s & %s \\\\",
    math_math$estimate, math_math$std_error,
    math_field$estimate, math_field$std_error
  ),
  sprintf(
    "High-premium-field VA & %s & %s & %s & %s \\\\",
    field_math$estimate, field_math$std_error,
    field_field$estimate, field_field$std_error
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(tex_lines, output_tex)

field_income <- results[predictor == "field" & outcome == "income"]
field_income_tex_lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "School value-added index & Estimate & SE & $P_{90}-P_{10}$ effect & Exact percent & N \\\\",
  "\\midrule",
  sprintf(
    "High-premium-field VA & %.3f%s & (%.3f) & %.3f log points & %.1f\\%% & %s \\\\",
    field_income$beta,
    stars(field_income$p_value),
    field_income$se,
    field_income$implied_p90_p10_effect,
    100 * expm1(field_income$implied_p90_p10_effect),
    format(field_income$n_obs, big.mark = ",", scientific = FALSE)
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(field_income_tex_lines, output_field_income_tex)

institution_income <- results[predictor == "institution" & outcome == "income"]
institution_income_tex_lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "School value-added index & Estimate & SE & $P_{90}-P_{10}$ effect & Exact percent & N \\\\",
  "\\midrule",
  sprintf(
    "High-premium-institution VA & %.3f%s & (%.3f) & %.3f log points & %.1f\\%% & %s \\\\",
    institution_income$beta,
    stars(institution_income$p_value),
    institution_income$se,
    institution_income$implied_p90_p10_effect,
    100 * expm1(institution_income$implied_p90_p10_effect),
    format(institution_income$n_obs, big.mark = ",", scientific = FALSE)
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(institution_income_tex_lines, output_institution_income_tex)

all_income <- results[outcome == "income"]
all_income[, predictor_order := match(
  predictor,
  c("math", "verbal", "institution", "field", "income")
)]
setorder(all_income, predictor_order)
all_income[, `:=`(
  implied_exact_percent = 100 * expm1(implied_p90_p10_effect),
  p90_p10_display = fcase(
    predictor == "math", sprintf("%.3f SD", p90_p10_predictor),
    predictor == "verbal", sprintf("%.3f SD", p90_p10_predictor),
    predictor == "institution", sprintf("%.1f pp", 100 * p90_p10_predictor),
    predictor == "field", sprintf("%.1f pp", 100 * p90_p10_predictor),
    predictor == "income", sprintf("%.3f log points", p90_p10_predictor)
  )
)]
fwrite(all_income[, predictor_order := NULL], output_all_income_csv)

all_income_rows <- vapply(seq_len(nrow(all_income)), function(i) {
  row <- all_income[i]
  sprintf(
    "%s & %.3f%s & (%.3f) & %s & %.3f & %.1f\\%% & %s \\\\",
    row$predictor_label,
    row$beta,
    stars(row$p_value),
    row$se,
    row$p90_p10_display,
    row$implied_p90_p10_effect,
    row$implied_exact_percent,
    format(row$n_obs, big.mark = ",", scientific = FALSE)
  )
}, character(1))

all_income_tex_lines <- c(
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "VA measure & Estimate & SE & $P_{90}-P_{10}$ VA gap & Implied log gain & Exact percent & N \\\\",
  "\\midrule",
  all_income_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(all_income_tex_lines, output_all_income_tex)

all_math <- results[outcome == "math"]
all_math[, predictor_order := match(
  predictor,
  c("math", "verbal", "institution", "field", "income")
)]
setorder(all_math, predictor_order)
all_math[, p90_p10_display := fcase(
  predictor == "math", sprintf("%.3f SD", p90_p10_predictor),
  predictor == "verbal", sprintf("%.3f SD", p90_p10_predictor),
  predictor == "institution", sprintf("%.1f pp", 100 * p90_p10_predictor),
  predictor == "field", sprintf("%.1f pp", 100 * p90_p10_predictor),
  predictor == "income", sprintf("%.3f log points", p90_p10_predictor)
)]
fwrite(all_math[, predictor_order := NULL], output_all_math_csv)

all_math_rows <- vapply(seq_len(nrow(all_math)), function(i) {
  row <- all_math[i]
  sprintf(
    "%s & %.3f%s & (%.3f) & %s & %.3f SD & %s \\\\",
    row$predictor_label,
    row$beta,
    stars(row$p_value),
    row$se,
    row$p90_p10_display,
    row$implied_p90_p10_effect,
    format(row$n_obs, big.mark = ",", scientific = FALSE)
  )
}, character(1))

all_math_tex_lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "VA measure & Estimate & SE & $P_{90}-P_{10}$ VA gap & Implied math gain & N \\\\",
  "\\midrule",
  all_math_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(all_math_tex_lines, output_all_math_tex)

all_field <- results[outcome == "field"]
all_field[, predictor_order := match(
  predictor,
  c("math", "verbal", "institution", "field", "income")
)]
setorder(all_field, predictor_order)
all_field[, `:=`(
  implied_p90_p10_pp = 100 * implied_p90_p10_effect,
  p90_p10_display = fcase(
    predictor == "math", sprintf("%.3f SD", p90_p10_predictor),
    predictor == "verbal", sprintf("%.3f SD", p90_p10_predictor),
    predictor == "institution", sprintf("%.1f pp", 100 * p90_p10_predictor),
    predictor == "field", sprintf("%.1f pp", 100 * p90_p10_predictor),
    predictor == "income", sprintf("%.3f log points", p90_p10_predictor)
  )
)]
fwrite(all_field[, predictor_order := NULL], output_all_field_csv)

all_field_rows <- vapply(seq_len(nrow(all_field)), function(i) {
  row <- all_field[i]
  sprintf(
    "%s & %.3f%s & (%.3f) & %s & %.1f pp & %s \\\\",
    row$predictor_label,
    row$beta,
    stars(row$p_value),
    row$se,
    row$p90_p10_display,
    row$implied_p90_p10_pp,
    format(row$n_obs, big.mark = ",", scientific = FALSE)
  )
}, character(1))

all_field_tex_lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "VA measure & Estimate & SE & $P_{90}-P_{10}$ VA gap & Implied field gain & N \\\\",
  "\\midrule",
  all_field_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(all_field_tex_lines, output_all_field_tex)

message("Wrote: ", output_csv)
message("Wrote: ", output_tex)
message("Wrote: ", output_field_income_tex)
message("Wrote: ", output_institution_income_tex)
message("Wrote: ", output_all_income_csv)
message("Wrote: ", output_all_income_tex)
message("Wrote: ", output_all_math_csv)
message("Wrote: ", output_all_math_tex)
message("Wrote: ", output_all_field_csv)
message("Wrote: ", output_all_field_tex)
