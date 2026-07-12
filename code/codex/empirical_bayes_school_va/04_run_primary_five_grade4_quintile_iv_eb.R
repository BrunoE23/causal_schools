###############################################################################
# Primary five EB scalar-IV estimates by grade-4 math quintile
#
# Uses the joint regression dataframe written by 02_run_expected_va_scalar_iv_eb.R
# and constructs the paper-facing Figure 1 and Table 4.
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
    stop("Could not find ", label, ". Set ", env_var, ".", call. = FALSE)
  }
  candidates[[1]]
}

make_rank_quintile <- function(x) {
  out <- rep(NA_integer_, length(x))
  observed <- !is.na(x)
  n_observed <- sum(observed)
  if (n_observed == 0L) {
    return(out)
  }
  ranks <- frank(x[observed], ties.method = "average")
  out[observed] <- pmin(5L, as.integer(ceiling(5 * ranks / n_observed)))
  out
}

extract_coef <- function(model, term) {
  model_terms <- names(coef(model))
  if (term %in% model_terms) {
    return(term)
  }
  fitted_term <- paste0("fit_", term)
  if (fitted_term %in% model_terms) {
    return(fitted_term)
  }
  match <- grep(term, model_terms, fixed = TRUE, value = TRUE)
  if (length(match) == 1L) {
    return(match)
  }
  stop("Could not find coefficient for ", term, ".", call. = FALSE)
}

format_estimate <- function(x) {
  x[!is.na(x) & abs(x) < 0.0005] <- 0
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

format_se <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
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

repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(getwd(), "C:/Users/brunem/Research/causal_schools"),
  "repository root"
)
eb_table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
results_table_dir <- file.path(repo_wd, "output", "tables", "results_section")
results_figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
dir.create(results_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_figure_dir, recursive = TRUE, showWarnings = FALSE)

regression_path <- Sys.getenv(
  "EB_PRIMARY_FIVE_REGRESSION_DF",
  unset = file.path(
    eb_table_dir,
    "stata_scalar_school_value_iv_main_five_expected_va_eb_regression_df.csv"
  )
)
results_path <- Sys.getenv(
  "EB_PRIMARY_FIVE_QUINTILE_RESULTS",
  unset = file.path(
    results_table_dir,
    "simce4_math_quintile_primary_five_eb.csv"
  )
)
contrasts_path <- Sys.getenv(
  "EB_PRIMARY_FIVE_QUINTILE_CONTRASTS",
  unset = file.path(
    results_table_dir,
    "simce4_math_quintile_primary_five_eb_contrasts_vs_q3.csv"
  )
)
table_path <- Sys.getenv(
  "EB_PRIMARY_FIVE_QUINTILE_TABLE",
  unset = file.path(
    results_table_dir,
    "simce4_math_quintile_primary_five_eb_contrasts_vs_q3.tex"
  )
)
figure_path <- Sys.getenv(
  "EB_PRIMARY_FIVE_QUINTILE_FIGURE",
  unset = file.path(
    results_figure_dir,
    "simce4_math_quintile_primary_five_eb.png"
  )
)

specs <- data.table(
  spec = c(
    "math_adj_eb",
    "leng_adj_eb",
    "highinst_adj_eb",
    "highpay_adj_eb",
    "program_income_full_adj_eb"
  ),
  outcome = c(
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
  instrument = c(
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
  ),
  label = c(
    "Math",
    "Verbal",
    "High-premium inst.",
    "High-premium field",
    "Log proj. income"
  )
)

required_cols <- unique(c(
  "cohort_gr8",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  specs$outcome,
  specs$treatment,
  specs$instrument,
  specs$expected
))
if (!file.exists(regression_path)) {
  stop("Missing EB primary-five regression dataframe: ", regression_path, call. = FALSE)
}
message("Reading EB primary-five regression dataframe: ", regression_path)
dt <- fread(regression_path, select = required_cols, na.strings = c("", "NA"))
dt[, simce4_math_quintile := make_rank_quintile(z_sim_mat_4to)]
dt <- dt[!is.na(simce4_math_quintile)]

run_iv <- function(sample_dt, spec_row, quintile) {
  needed <- c(
    spec_row$outcome,
    spec_row$treatment,
    spec_row$instrument,
    spec_row$expected,
    "cohort_gr8",
    "GEN_ALU",
    "EDAD_ALU",
    "z_sim_mat_4to",
    "z_sim_leng_4to"
  )
  reg_dt <- sample_dt[complete.cases(sample_dt[, ..needed])]
  if (
    nrow(reg_dt) == 0L ||
      uniqueN(reg_dt[[spec_row$treatment]]) < 2L ||
      uniqueN(reg_dt[[spec_row$instrument]]) < 2L
  ) {
    return(data.table(
      spec = spec_row$spec,
      label = spec_row$label,
      quintile = quintile,
      beta = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      n_obs = nrow(reg_dt),
      fs_f = NA_real_
    ))
  }

  controls <- paste(
    c(
      "factor(cohort_gr8)",
      "z_sim_mat_4to",
      "z_sim_leng_4to",
      "factor(GEN_ALU)",
      "factor(EDAD_ALU)",
      spec_row$expected
    ),
    collapse = " + "
  )
  iv_formula <- as.formula(paste0(
    spec_row$outcome, " ~ ", controls,
    " | 0 | ", spec_row$treatment, " ~ ", spec_row$instrument
  ))
  first_stage_formula <- as.formula(paste0(
    spec_row$treatment, " ~ ", spec_row$instrument, " + ", controls
  ))
  iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  coefficient <- extract_coef(iv_model, spec_row$treatment)
  beta <- coef(iv_model)[[coefficient]]
  se_beta <- se(iv_model)[[coefficient]]

  first_stage <- feols(
    first_stage_formula,
    data = reg_dt,
    vcov = "hetero",
    notes = FALSE
  )
  fs_beta <- coef(first_stage)[[spec_row$instrument]]
  fs_se <- se(first_stage)[[spec_row$instrument]]

  data.table(
    spec = spec_row$spec,
    label = spec_row$label,
    quintile = quintile,
    beta = beta,
    se = se_beta,
    p_value = 2 * pnorm(-abs(beta / se_beta)),
    ci_low = beta - 1.96 * se_beta,
    ci_high = beta + 1.96 * se_beta,
    n_obs = nobs(iv_model),
    fs_f = (fs_beta / fs_se)^2
  )
}

message("Estimating EB scalar IV by grade-4 math quintile.")
setFixest_nthreads(0)
results <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  rbindlist(lapply(1:5, function(q) {
    run_iv(dt[simce4_math_quintile == q], specs[i], q)
  }))
}))
results[, spec_order := match(spec, specs$spec)]
setorder(results, spec_order, quintile)
fwrite(results, results_path)

q3 <- results[
  quintile == 3L,
  .(spec, beta_q3 = beta, se_q3 = se)
]
contrasts <- merge(
  results[quintile != 3L],
  q3,
  by = "spec",
  all.x = TRUE,
  sort = FALSE
)
contrasts[, c("difference", "difference_se") := list(
  beta - beta_q3,
  sqrt(se^2 + se_q3^2)
)]
contrasts[, c("z_stat", "p_value_difference", "spec_order") := list(
  difference / difference_se,
  2 * pnorm(-abs(difference / difference_se)),
  match(spec, specs$spec)
)]
setorder(contrasts, quintile, spec_order)
fwrite(contrasts, contrasts_path)

contrast_rows <- unlist(lapply(c(1L, 2L, 4L, 5L), function(q) {
  row <- contrasts[quintile == q]
  setorder(row, spec_order)
  c(
    paste0(
      "$Q", q, "-Q3$ & ",
      paste0(
        format_estimate(row$difference),
        significance_stars(row$p_value_difference),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(" & ", paste(format_se(row$difference_se), collapse = " & "), " \\\\")
  )
}), use.names = FALSE)

table_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Differences in EB pass-through relative to the middle quintile}",
  "\\label{tab:simce4-math-quintile-primary-five-eb-contrasts-q3}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Exams} & \\multicolumn{3}{c}{Higher ed. choices} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-6}",
  "Comparison & Math & Verbal & High-premium inst. & High-premium field & Log proj. income \\\\",
  "\\midrule",
  contrast_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "Notes: Each entry reports $\\widehat{\\theta}^{EB}_{Q_q} - ",
    "\\widehat{\\theta}^{EB}_{Q3}$ for the indicated baseline grade-4 math ",
    "achievement quintile. Heteroskedasticity-robust standard errors are ",
    "reported in parentheses. Because quintile samples are disjoint, the ",
    "contrast variance is the sum of the two coefficient variances. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(table_lines, table_path)

message("Drawing EB quintile heterogeneity figure.")
colors <- c("#1B4D89", "#B6422E", "#1B7F5A", "#7A4EA3", "#A15C1B")
point_shapes <- c(16, 17, 15, 18, 8)
offsets <- c(-0.24, -0.12, 0, 0.12, 0.24)
png(figure_path, width = 2000, height = 1300, res = 200)
par(mar = c(5.2, 5.2, 1.5, 1.2), family = "serif")
plot(
  NA,
  xlim = c(0.5, 5.5),
  ylim = range(c(results$ci_low, results$ci_high), na.rm = TRUE),
  xaxt = "n",
  xlab = "Baseline grade-4 math achievement quintile",
  ylab = expression(paste("Estimated EB pass-through ", theta^{EB})),
  bty = "l"
)
axis(1, at = 1:5, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"))
abline(h = 1, col = "grey70", lty = 2)
for (i in seq_len(nrow(specs))) {
  row <- results[spec == specs$spec[i]][order(quintile)]
  x <- row$quintile + offsets[i]
  arrows(
    x0 = x,
    y0 = row$ci_low,
    x1 = x,
    y1 = row$ci_high,
    angle = 90,
    code = 3,
    length = 0.035,
    col = colors[i],
    lwd = 1.2
  )
  points(
    x,
    row$beta,
    pch = point_shapes[i],
    col = colors[i],
    bg = colors[i],
    cex = 1.15
  )
}
legend(
  "topleft",
  legend = specs$label,
  col = colors,
  pch = point_shapes,
  bty = "n",
  cex = 0.85
)
dev.off()

message("Wrote: ", results_path)
message("Wrote: ", contrasts_path)
message("Wrote: ", table_path)
message("Wrote: ", figure_path)
