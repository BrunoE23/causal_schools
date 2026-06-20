suppressPackageStartupMessages({
  library(data.table)
})

# Paper-facing results objects for writing/may_draft/may_draft.tex.
# Uses existing cleaned outputs; does not rebuild VA or IV inputs.

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
scalar_dir <- file.path(clean_dir, "scalar_school_value_iv")
orthogonal_dir <- file.path(clean_dir, "orthogonal_school_va")

table_dir <- file.path(repo_wd, "output", "tables", "results_section")
figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

scalar_results_path <- file.path(
  repo_wd,
  "output",
  "tables",
  "scalar_school_value_iv",
  "scalar_school_value_iv_results_expected_va.csv"
)
expected_va_path <- file.path(
  scalar_dir,
  "scalar_school_value_iv_expected_va.csv"
)
orthogonal_va_path <- file.path(
  orthogonal_dir,
  "school_orthogonal_va_exam_takers.csv"
)
school_values_path <- file.path(
  clean_dir,
  "school_rbd_observational_values",
  "school_rbd_observational_values.csv"
)
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
middle_school_controls_path <- file.path(
  clean_dir,
  "middle_school_controls",
  "middle_school_controls.csv"
)

main_table_csv <- file.path(table_dir, "main_four_va_metrics.csv")
main_table_tex <- file.path(table_dir, "main_four_va_metrics.tex")
va_distribution_csv <- file.path(table_dir, "school_va_distribution.csv")
va_distribution_tex <- file.path(table_dir, "school_va_distribution.tex")
outcome_sample_stats_csv <- file.path(table_dir, "outcome_sample_stats.csv")
heterogeneity_csv <- file.path(table_dir, "simce4_math_quintile_four_metrics.csv")
heterogeneity_contrasts_csv <- file.path(
  table_dir,
  "simce4_math_quintile_contrasts_vs_q3.csv"
)
heterogeneity_contrasts_tex <- file.path(
  table_dir,
  "simce4_math_quintile_contrasts_vs_q3.tex"
)
heterogeneity_plot <- file.path(
  figure_dir,
  "simce4_math_quintile_four_metrics.png"
)
math_language_correlation_plot <- file.path(
  figure_dir,
  "math_language_va_correlation.png"
)
math_stem_correlation_plot <- file.path(
  figure_dir,
  "math_stem_va_correlation.png"
)
correlation_summary_csv <- file.path(
  table_dir,
  "school_va_correlations.csv"
)

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

specs <- data.table(
  spec = c("math_adj", "leng_adj", "stem_adj", "instcert_adj"),
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "stem_enrollment_m1",
    "inst_certified_years_m1"
  ),
  expected = c(
    "expected_math_adj",
    "expected_leng_adj",
    "expected_stem_adj",
    "expected_instcert_adj"
  ),
  treatment = c(
    "d_math_adj",
    "d_leng_adj",
    "d_stem_adj",
    "d_instcert_adj"
  ),
  instrument = c(
    "z_math_adj",
    "z_leng_adj",
    "z_stem_adj",
    "z_instcert_adj"
  ),
  label = c(
    "Math",
    "Language",
    "STEM enrollment",
    "Institutional quality"
  ),
  plot_label = c(
    "Math VA",
    "Language VA",
    "STEM VA",
    "Inst. quality VA"
  ),
  family = c("Achievement", "Achievement", "Higher education", "Higher education")
)

message("Reading paper-facing school VA values.")
paper_school_values <- fread(
  school_values_path,
  select = c(
    "school_rbd",
    "analysis_sample",
    "outcome",
    "controlled_value_added_centered_student",
    "controlled_adjusted_mean_student",
    "n_students_regression",
    "n_students_regression_total"
  ),
  na.strings = c("", "NA")
)[
  analysis_sample == "All" & outcome %chin% specs$outcome
]
paper_school_values <- merge(
  paper_school_values,
  specs[, .(spec, outcome, label, family)],
  by = "outcome",
  all.x = TRUE,
  sort = FALSE
)

if (paper_school_values[, anyDuplicated(paste(school_rbd, outcome))]) {
  stop("Paper-facing school VA input has duplicate school-outcome rows.")
}

paper_school_values[, sample_outcome_mean :=
  controlled_adjusted_mean_student - controlled_value_added_centered_student]
sample_mean_ranges <- paper_school_values[
  is.finite(sample_outcome_mean),
  .(
    sample_mean_min = min(sample_outcome_mean),
    sample_mean_max = max(sample_outcome_mean)
  ),
  by = outcome
]
if (sample_mean_ranges[, any(abs(sample_mean_max - sample_mean_min) > 1e-10)]) {
  stop("Recovered outcome sample mean is not constant within outcome.")
}

z_within_group <- function(x) {
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sigma
}

message("Computing underlying outcome moments in the VA regression samples.")
baseline_cpad_control_vars <- c(
  "father_educ_years_imputed",
  "mother_educ_years_imputed",
  "father_indigenous_imputed",
  "mother_indigenous_imputed",
  "sala_cuna_imputed",
  "jardin_imputed",
  "prekinder_imputed",
  "kinder_imputed"
)
universe_control_vars <- c(
  "cohort_gr8",
  "GEN_ALU",
  "EDAD_ALU",
  "COD_COM_ALU",
  "income_decile_imputed",
  baseline_cpad_control_vars,
  "z_gpa_middle_mean",
  "z_att_middle_mean",
  "middle_years_observed",
  "z_sim_mat_4to",
  "z_sim_leng_4to"
)
universe_cols <- unique(c(
  "MRUN",
  "most_time_RBD",
  "psu_year",
  "math_max",
  "leng_max",
  "field_reclassified_m1",
  "f_science_m1",
  "f_eng_m1",
  "ACREDITADA_CARR_m1",
  "ACREDITADA_INST_m1",
  "ACRE_INST_ANIO_m1",
  "program_certified_years_m1",
  "institution_accredited_m1",
  setdiff(
    universe_control_vars,
    c(
      "z_gpa_middle_mean",
      "z_att_middle_mean",
      "middle_years_observed"
    )
  )
))

universe_sample <- fread(
  universe_path,
  select = universe_cols,
  na.strings = c("", "NA"),
  showProgress = FALSE
)
universe_sample[, school_rbd := as.numeric(most_time_RBD)]
universe_sample <- universe_sample[
  !is.na(school_rbd) & school_rbd > 0 &
    !is.na(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16
]

for (score in c("math_max", "leng_max")) {
  universe_sample[is.na(get(score)) | get(score) <= 0, (score) := NA_real_]
  z_score <- paste0("z_year_", score)
  universe_sample[, (z_score) := z_within_group(get(score)), by = psu_year]
}

universe_sample <- universe_sample[!is.na(math_max) | !is.na(leng_max)]
universe_sample[, stem_enrollment_m1 := as.integer(
  fifelse(is.na(f_science_m1), 0, as.numeric(f_science_m1)) == 1 |
    fifelse(is.na(f_eng_m1), 0, as.numeric(f_eng_m1)) == 1
)]
universe_sample[, observed_matricula_m1 :=
  !is.na(ACREDITADA_CARR_m1) |
    !is.na(ACREDITADA_INST_m1) |
    !is.na(ACRE_INST_ANIO_m1)]
universe_sample[, inst_certified_years_m1 := fcase(
  institution_accredited_m1 == 1L,
  as.numeric(ACRE_INST_ANIO_m1),
  institution_accredited_m1 == 0L,
  0,
  is.na(institution_accredited_m1) & !observed_matricula_m1,
  0,
  default = NA_real_
)]

middle_school_controls <- fread(
  middle_school_controls_path,
  select = c(
    "MRUN",
    "most_time_RBD_middle",
    "middle_years_observed",
    "z_gpa_middle_mean",
    "z_att_middle_mean"
  ),
  na.strings = c("", "NA"),
  showProgress = FALSE
)
universe_sample[, MRUN := as.character(MRUN)]
middle_school_controls[, MRUN := as.character(MRUN)]
universe_sample <- merge(
  universe_sample,
  middle_school_controls,
  by = "MRUN",
  all.x = TRUE,
  sort = FALSE
)

outcome_sample_stats <- rbindlist(lapply(specs$outcome, function(outcome_name) {
  required <- c(
    outcome_name,
    universe_control_vars,
    "school_rbd",
    "most_time_RBD_middle"
  )
  regression_sample <- universe_sample[complete.cases(universe_sample[, ..required])]
  data.table(
    outcome = outcome_name,
    sample_mean = mean(regression_sample[[outcome_name]]),
    sample_sd = stats::sd(regression_sample[[outcome_name]]),
    n_students = nrow(regression_sample)
  )
}))

va_sample_check_rows <- paper_school_values[
  is.finite(sample_outcome_mean) & is.finite(n_students_regression_total)
]
va_sample_checks <- va_sample_check_rows[, .(
  va_sample_mean = stats::median(sample_outcome_mean),
  va_sample_mean_range = max(sample_outcome_mean) - min(sample_outcome_mean),
  va_n_students = unique(n_students_regression_total)[[1]],
  n_distinct_va_samples = uniqueN(n_students_regression_total)
), by = outcome]
if (va_sample_checks[, any(
  va_sample_mean_range > 1e-10 | n_distinct_va_samples != 1L
)]) {
  stop("VA output contains inconsistent sample checks within an outcome.")
}
outcome_sample_stats <- merge(
  outcome_sample_stats,
  va_sample_checks,
  by = "outcome",
  all.x = TRUE,
  sort = FALSE
)
if (outcome_sample_stats[, any(
  abs(sample_mean - va_sample_mean) > 1e-10 |
    n_students != va_n_students
)]) {
  stop("Reconstructed outcome sample does not match the VA regression sample.")
}
fwrite(outcome_sample_stats, outcome_sample_stats_csv)

weighted_quantile <- function(x, weights, probs) {
  valid <- is.finite(x) & is.finite(weights) & weights > 0
  x <- x[valid]
  weights <- weights[valid]
  ord <- order(x)
  x <- x[ord]
  weights <- weights[ord]
  cumulative_weight <- cumsum(weights) / sum(weights)
  vapply(probs, function(p) x[which(cumulative_weight >= p)[1]], numeric(1))
}

weighted_sd <- function(x, weights) {
  valid <- is.finite(x) & is.finite(weights) & weights > 0
  x <- x[valid]
  weights <- weights[valid]
  weighted_mean <- sum(weights * x) / sum(weights)
  sqrt(sum(weights * (x - weighted_mean)^2) / sum(weights))
}

message("Writing main four-metric table.")
results <- fread(scalar_results_path, na.strings = c("", "NA"))
main <- merge(
  specs[, .(spec, label, family)],
  results[, .(spec, beta, se, p_value, n_obs)],
  by = "spec",
  all.x = TRUE,
  sort = FALSE
)
main[, spec := factor(spec, levels = specs$spec)]
setorder(main, spec)
fwrite(main, main_table_csv)

theta_row <- paste(format_estimate(main$beta), collapse = " & ")
se_row <- paste(format_se(main$se), collapse = " & ")
n_row <- paste(formatC(main$n_obs, format = "d", big.mark = ","), collapse = " & ")

main_tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Main scalar school-value IV estimates}",
  "\\label{tab:main-four-va-results}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Achievement} & \\multicolumn{2}{c}{Higher education} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  paste0(
    " & ",
    paste(main$label, collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  paste0("$\\theta$ & ", theta_row, " \\\\"),
  paste0("SE & ", se_row, " \\\\"),
  paste0("Observations & ", n_row, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(main_tex, main_table_tex)

message("Writing school VA distribution table.")
va_distribution <- paper_school_values[, {
  quantiles <- weighted_quantile(
    controlled_value_added_centered_student,
    n_students_regression,
    probs = c(0.10, 0.25, 0.50, 0.75, 0.90)
  )
  valid <- is.finite(controlled_value_added_centered_student) &
    is.finite(n_students_regression) &
  n_students_regression > 0

  .(
    p10 = quantiles[[1]],
    p25 = quantiles[[2]],
    p50 = quantiles[[3]],
    p75 = quantiles[[4]],
    p90 = quantiles[[5]],
    p90_p10 = quantiles[[5]] - quantiles[[1]],
    va_sd = weighted_sd(
      controlled_value_added_centered_student,
      n_students_regression
    ),
    n_schools = sum(valid),
    n_students = sum(n_students_regression[valid])
  )
}, by = .(spec, outcome, label, family)]
va_distribution <- merge(
  va_distribution,
  outcome_sample_stats[, .(outcome, sample_mean, sample_sd)],
  by = "outcome",
  all.x = TRUE,
  sort = FALSE
)
va_distribution[, spec := factor(spec, levels = specs$spec)]
setorder(va_distribution, spec)
va_distribution[, spec := as.character(spec)]
fwrite(va_distribution, va_distribution_csv)

distribution_latex_rows <- vapply(seq_len(nrow(specs)), function(i) {
  values <- unlist(va_distribution[
    spec == specs$spec[[i]],
    .(sample_mean, sample_sd, va_sd, p10, p25, p50, p75, p90)
  ], use.names = FALSE)
  paste0(
    specs$label[[i]],
    " & ",
    paste(format_estimate(values), collapse = " & "),
    " \\\\"
  )
}, character(1))

va_distribution_latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Distribution of observational school value added}",
  "\\label{tab:school-va-distribution}",
  "\\small",
  "\\setlength{\\tabcolsep}{3.5pt}",
  "\\begin{tabular}{lcc|cccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c|}{Outcome distribution} & \\multicolumn{6}{c}{Value-added distribution} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-9}",
  "Outcome & \\shortstack{Sample\\\\Mean} & \\shortstack{Sample\\\\SD} & VA SD & P10 & P25 & P50 & P75 & P90 \\\\",
  "\\midrule",
  distribution_latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item Notes: The table reports the student-weighted distribution of ",
    "All-sample adjusted school value added. Schools are weighted by the ",
    "number of students used to estimate the corresponding value-added ",
    "measure. Sample Mean and Sample SD refer to the underlying outcome ",
    "in the outcome-specific value-added regression sample; the remaining ",
    "columns describe the value-added distribution. Math and language are ",
    "measured in admission-test standard ",
    "deviations, STEM enrollment in probability units, and institutional ",
    "quality in accreditation years."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
writeLines(va_distribution_latex, va_distribution_tex)

make_rank_group <- function(x, n_groups = 5L) {
  out <- rep(NA_integer_, length(x))
  nonmissing <- !is.na(x)
  n <- sum(nonmissing)
  if (n == 0) {
    return(out)
  }
  ranks <- frank(x[nonmissing], ties.method = "average")
  out[nonmissing] <- pmin(n_groups, as.integer(ceiling(n_groups * ranks / n)))
  out
}

residualize <- function(x, controls) {
  as.numeric(qr.resid(qr(controls), x))
}

build_controls <- function(dt) {
  terms <- c("z_sim_mat_4to", "z_sim_leng_4to")

  if (uniqueN(dt$cohort_gr8) > 1) {
    terms <- c(terms, "factor(cohort_gr8)")
  }
  if (uniqueN(dt$GEN_ALU) > 1) {
    terms <- c(terms, "factor(GEN_ALU)")
  }
  if (uniqueN(dt$EDAD_ALU) > 1) {
    terms <- c(terms, "factor(EDAD_ALU)")
  }

  stats::model.matrix(
    stats::as.formula(paste("~", paste(terms, collapse = " + "))),
    data = dt
  )
}

empty_result <- function(spec_row, group_label, n_obs = 0L) {
  data.table(
    spec = spec_row$spec,
    label = spec_row$label,
    plot_label = spec_row$plot_label,
    family = spec_row$family,
    group = group_label,
    beta = NA_real_,
    se = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    n_obs = n_obs
  )
}

run_scalar_iv <- function(dt, spec_row, group_label) {
  needed <- c(
    spec_row$outcome,
    spec_row$treatment,
    spec_row$instrument,
    spec_row$expected,
    "cohort_gr8",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "GEN_ALU",
    "EDAD_ALU"
  )
  reg_dt <- dt[complete.cases(dt[, ..needed])]

  if (
    nrow(reg_dt) == 0 ||
      uniqueN(reg_dt[[spec_row$instrument]]) < 2 ||
      uniqueN(reg_dt[[spec_row$treatment]]) < 2
  ) {
    return(empty_result(spec_row, group_label, nrow(reg_dt)))
  }

  controls <- cbind(build_controls(reg_dt), expected = reg_dt[[spec_row$expected]])
  y_resid <- residualize(reg_dt[[spec_row$outcome]], controls)
  d_resid <- residualize(reg_dt[[spec_row$treatment]], controls)
  z_resid <- residualize(reg_dt[[spec_row$instrument]], controls)

  denom <- sum(z_resid * d_resid)
  if (!is.finite(denom) || abs(denom) < .Machine$double.eps) {
    return(empty_result(spec_row, group_label, nrow(reg_dt)))
  }

  beta <- sum(z_resid * y_resid) / denom
  u <- y_resid - beta * d_resid
  n <- length(u)
  k <- ncol(controls) + 1L
  small_sample_adj <- n / (n - k)
  se <- sqrt(small_sample_adj * sum((z_resid^2) * (u^2)) / denom^2)

  data.table(
    spec = spec_row$spec,
    label = spec_row$label,
    plot_label = spec_row$plot_label,
    family = spec_row$family,
    group = group_label,
    beta = beta,
    se = se,
    ci_low = beta - 1.96 * se,
    ci_high = beta + 1.96 * se,
    n_obs = n
  )
}

message("Computing grade-4 math-quintile heterogeneity.")
needed_cols <- unique(c(
  "student_id",
  "cohort_gr8",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  specs$outcome,
  specs$expected,
  specs$treatment,
  specs$instrument
))
dt <- fread(expected_va_path, select = needed_cols, na.strings = c("", "NA"))
dt[, simce4_math_quintile := make_rank_group(z_sim_mat_4to, n_groups = 5L)]
dt <- dt[!is.na(simce4_math_quintile)]

heterogeneity <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  rbindlist(lapply(1:5, function(q) {
    run_scalar_iv(
      dt[simce4_math_quintile == q],
      specs[i],
      paste0("Q", q)
    )
  }), use.names = TRUE)
}), use.names = TRUE)
heterogeneity[, quintile := as.integer(sub("Q", "", group))]
heterogeneity[, group_label := factor(
  group,
  levels = paste0("Q", 1:5),
  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")
)]
heterogeneity[, plot_label := factor(plot_label, levels = specs$plot_label)]
fwrite(heterogeneity, heterogeneity_csv)

q3_reference <- heterogeneity[
  quintile == 3L,
  .(spec, beta_q3 = beta, se_q3 = se)
]
heterogeneity_contrasts <- merge(
  heterogeneity[quintile != 3L],
  q3_reference,
  by = "spec",
  all.x = TRUE,
  sort = FALSE
)
heterogeneity_contrasts[, `:=`(
  comparison = paste0("Q", quintile, " - Q3"),
  difference = beta - beta_q3,
  difference_se = sqrt(se^2 + se_q3^2)
)]
heterogeneity_contrasts[, `:=`(
  z_stat = difference / difference_se,
  p_value = 2 * stats::pnorm(abs(difference / difference_se), lower.tail = FALSE),
  ci_low = difference - 1.96 * difference_se,
  ci_high = difference + 1.96 * difference_se,
  spec_order = match(spec, specs$spec)
)]
setorder(heterogeneity_contrasts, quintile, spec_order)
fwrite(heterogeneity_contrasts, heterogeneity_contrasts_csv)

contrast_latex_rows <- unlist(lapply(c(1L, 2L, 4L, 5L), function(q) {
  comparison <- heterogeneity_contrasts[quintile == q][order(spec_order)]
  estimates <- paste0(
    format_estimate(comparison$difference),
    significance_stars(comparison$p_value)
  )
  c(
    paste0("$Q", q, "-Q3$ & ", paste(estimates, collapse = " & "), " \\\\"),
    paste0(" & ", paste(format_se(comparison$difference_se), collapse = " & "), " \\\\")
  )
}), use.names = FALSE)

heterogeneity_contrasts_latex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Differences in pass-through relative to the middle quintile}",
  "\\label{tab:simce4-math-quintile-contrasts-q3}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Comparison & Math & Language & STEM enrollment & Institutional quality \\\\",
  "\\midrule",
  contrast_latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item Notes: Each entry reports $\\widehat{\\theta}_{Q_q} - ",
    "\\widehat{\\theta}_{Q3}$ for the indicated baseline grade-4 math ",
    "achievement quintile. Heteroskedasticity-robust standard errors are ",
    "reported in parentheses. Because quintile samples are disjoint, the ",
    "contrast variance is the sum of the two coefficient variances. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
writeLines(heterogeneity_contrasts_latex, heterogeneity_contrasts_tex)

message("Drawing heterogeneity plot.")
png(heterogeneity_plot, width = 1800, height = 1200, res = 200)
par(mar = c(5.2, 5.2, 1.5, 1.2), family = "serif")
plot(
  NA,
  xlim = c(0.55, 5.45),
  ylim = range(c(heterogeneity$ci_low, heterogeneity$ci_high), na.rm = TRUE),
  xaxt = "n",
  xlab = "Baseline grade-4 math achievement quintile",
  ylab = expression(paste("Estimated pass-through ", theta)),
  bty = "l"
)
axis(1, at = 1:5, labels = levels(heterogeneity$group_label))
abline(h = 1, col = "grey70", lty = 2)
cols <- c("#1B4D89", "#B6422E", "#1B7F5A", "#7A4EA3")
pchs <- c(16, 17, 15, 18)
offsets <- c(-0.18, -0.06, 0.06, 0.18)
for (i in seq_along(specs$plot_label)) {
  lab <- specs$plot_label[i]
  sub <- heterogeneity[plot_label == lab][order(quintile)]
  x <- sub$quintile + offsets[i]
  arrows(
    x0 = x,
    y0 = sub$ci_low,
    x1 = x,
    y1 = sub$ci_high,
    angle = 90,
    code = 3,
    length = 0.035,
    col = cols[i],
    lwd = 1.2
  )
  points(x, sub$beta, pch = pchs[i], col = cols[i], bg = cols[i], cex = 1.15)
}
legend(
  "topleft",
  legend = specs$plot_label,
  col = cols,
  pch = pchs,
  bty = "n",
  cex = 0.85
)
dev.off()

message("Reading paper-facing school VA for correlation plots.")
school_va <- fread(
  orthogonal_va_path,
  select = c(
    "school_rbd",
    "math_controlled_value_added_centered_student",
    "stem_controlled_value_added_centered_student"
  ),
  na.strings = c("", "NA")
)

language_va <- paper_school_values[
  outcome == "z_year_leng_max",
  .(
    school_rbd,
    language_controlled_value_added_centered_student =
      controlled_value_added_centered_student
  )
]

if (anyDuplicated(language_va$school_rbd)) {
  stop("Language VA input has duplicate school_rbd rows.")
}

school_va <- merge(
  school_va,
  language_va,
  by = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

draw_correlation_plot <- function(
    dt,
    y_col,
    y_label,
    output_path,
    point_color = "#1B4D89",
    line_color = "#B6422E") {
  x_col <- "math_controlled_value_added_centered_student"
  plot_dt <- dt[complete.cases(dt[, c(x_col, y_col), with = FALSE])]
  correlation <- stats::cor(plot_dt[[x_col]], plot_dt[[y_col]])
  fit <- stats::lm(plot_dt[[y_col]] ~ plot_dt[[x_col]])

  png(output_path, width = 1500, height = 1200, res = 200)
  par(mar = c(5.1, 5.2, 1.2, 1.1), family = "serif")
  plot(
    plot_dt[[x_col]],
    plot_dt[[y_col]],
    pch = 16,
    col = grDevices::adjustcolor(point_color, alpha.f = 0.35),
    xlab = "Math value added",
    ylab = y_label,
    bty = "l"
  )
  abline(fit, col = line_color, lwd = 2)
  legend(
    "topleft",
    legend = paste0("Correlation = ", sprintf("%.3f", correlation)),
    bty = "n"
  )
  dev.off()

  data.table(
    comparison = paste0("math_vs_", sub("_controlled.*$", "", y_col)),
    correlation = correlation,
    n_schools = nrow(plot_dt),
    figure = basename(output_path)
  )
}

message("Drawing math/verbal VA correlation plot.")
math_language_summary <- draw_correlation_plot(
  school_va,
  y_col = "language_controlled_value_added_centered_student",
  y_label = "Verbal value added",
  output_path = math_language_correlation_plot,
  point_color = "#1B4D89"
)

message("Drawing math/STEM VA correlation plot.")
math_stem_summary <- draw_correlation_plot(
  school_va,
  y_col = "stem_controlled_value_added_centered_student",
  y_label = "STEM enrollment value added",
  output_path = math_stem_correlation_plot,
  point_color = "#1B7F5A"
)

correlation_summary <- rbindlist(list(
  math_language_summary,
  math_stem_summary
))
fwrite(correlation_summary, correlation_summary_csv)

message("Wrote: ", main_table_tex)
message("Wrote: ", va_distribution_tex)
message("Wrote: ", heterogeneity_contrasts_tex)
message("Wrote: ", heterogeneity_plot)
message("Wrote: ", math_language_correlation_plot)
message("Wrote: ", math_stem_correlation_plot)
message("Wrote: ", correlation_summary_csv)
