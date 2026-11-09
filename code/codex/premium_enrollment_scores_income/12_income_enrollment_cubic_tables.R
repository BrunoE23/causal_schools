# Two outcome columns with explicit, different units and the same income-share definition.
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--high-math"))
high_math_only <- "--high-math" %in% args
task <- if (high_math_only) "high_math_income_enrollment_cubic_income_contribution" else
  "income_enrollment_cubic_income_contribution"
clean <- file.path(repo, "data/clean", task)
reports <- file.path(repo, "output/reports", task); tables <- file.path(repo, "output/tables", task)
for (p in c(reports, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
outcomes <- c("log_projected_income", "he_enrollment")
titles <- c("Log projected income: HE enrollees",
            if (high_math_only) "HE enrollment: high-math students" else "HE enrollment: all scored students")
s <- fread(file.path(clean, "income_contribution.csv"))[match(outcomes, outcome)]
b <- fread(file.path(clean, "coefficients.csv"))
ame <- fread(file.path(clean, "score_average_derivatives.csv"))
tests <- fread(file.path(clean, "income_joint_tests.csv"))[match(outcomes, outcome)]
coverage <- fread(file.path(clean, "sample_coverage.csv"))
if (high_math_only) {
  selection <- fread(file.path(clean, "sample_selection.csv"))
  stopifnot(isTRUE(selection$high_math_only), selection$min_z_math >= 1,
            selection$N_retained == coverage$N_base)
}
stopifnot(nrow(s) == 2L, identical(s$outcome, outcomes), s$N[1L] == coverage$n_enrolled,
          s$N[2L] == coverage$N_base, coverage$n_enrolled_missing_income == 0L,
          all(is.finite(s$income_share_explained)), all(s$income_share_explained >= 0 & s$income_share_explained <= 1),
          all(abs(s$income_share_explained * s$r2_full - s$delta_r2_income) < 1e-12))
terms <- c("ame_math", "ame_verbal", paste0("income_decile", 1:10))
term_labels <- c("Math: average derivative (per SD)", "Verbal: average derivative (per SD)",
                 "Income decile 1", paste0("Income decile ", 2:10, " vs. 1"))
raw_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3", "math2_verbal", "math_verbal2")
fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_p <- function(x) ifelse(x < .001, "<0.001", sprintf("%.3f", x))
row_html <- function(label, values, cls = "") paste0('<tr class="', cls, '"><th scope="row">', label,
  '</th>', paste0('<td>', values, '</td>', collapse = ''), '</tr>')
header_html <- function(first, headers) paste0('<table><thead><tr><th>', first, '</th>',
  paste0('<th>', headers, '</th>', collapse = ''), '</tr></thead><tbody>')
effect_cell <- function(key, o, html = TRUE) {
  if (key == "income_decile1") return("Reference")
  z <- if (key %in% c("ame_math", "ame_verbal")) ame[term == key & outcome == o] else b[term == key & outcome == o]
  stopifnot(nrow(z) == 1L, is.finite(z$estimate), is.finite(z$std_error))
  multiplier <- if (o == "he_enrollment") 100 else 1
  digits <- if (o == "he_enrollment") 2L else 3L
  est <- multiplier * z$estimate; se <- multiplier * z$std_error
  if (abs(est) < .5 * 10^(-digits)) est <- 0
  if (html) sprintf('%.*f<span class="se">(%.*f)</span>', digits, est, digits, se)
  else sprintf('%.*f (%.*f)', digits, est, digits, se)
}
stats <- list("Coefficient units" = c("Log points", "Percentage points"),
  "Outcome mean" = c(sprintf("%.3f log CLP/month", s$mean_outcome[1L]), sprintf("%.2f%% enrolled", 100 * s$mean_outcome[2L])),
  "Full R-squared" = sprintf("%.4f", s$r2_full),
  "R-squared without family income" = sprintf("%.4f", s$r2_without_income),
  "Family income gain in R-squared" = sprintf("%.5f", s$delta_r2_income),
  "Family income share of explained variation (%)" = sprintf("%.2f", 100 * s$income_share_explained),
  "Family income indicators: joint clustered p-value" = fmt_p(tests$p_value),
  "Observations" = fmt_n(s$N), "School clusters" = fmt_n(s$n_schools),
  "Admission-year indicators" = rep("Yes", 2))
rows <- vapply(seq_along(terms), function(j) row_html(term_labels[j],
  vapply(outcomes, function(o) effect_cell(terms[j], o), character(1)), if (j == 3L) "block" else ""), character(1))
stat_rows <- vapply(seq_along(stats), function(j) row_html(names(stats)[j], stats[[j]],
  if (j == 1L) "block" else ""), character(1))
notes <- c(
  "Projected-income coefficients and SEs are in natural-log points; HE-enrollment coefficients and SEs are in percentage points. Parentheses report standard errors clustered by attended high school, using the CR1 finite-sample correction and t(G-1) inference. Income decile 1 is the reference. These are conditional descriptive associations.",
  "Both full models include M, V, M^2, V^2, MV, M^3, V^3, M^2V and MV^2; nine baseline family-income-decile indicators; and admission-year indicators. Math and verbal retain their original within-admission-year broad-panel standardization. Neither scores nor income deciles are redefined among enrollees. Score rows are average local derivatives of the full cubic polynomial per SD, not finite one-SD changes.",
  paste0("The enrollment regression uses ", fmt_n(coverage$N_base),
    if (high_math_only) " students with original z_math >= 1 from the preceding complete-score/income OLS sample, including " else
      " students from the preceding complete-score/income OLS sample, including ",
    fmt_n(coverage$n_non_enrolled), " non-enrollees. The projected-income regression uses its ", fmt_n(coverage$n_enrolled),
    " HE enrollees. Every enrollee has valid projected income, so there is no additional exclusion for missing outcome data. Full and reduced models use exactly the same observations within each outcome; the two outcomes intentionally use different samples."),
  "Log projected income is the existing log_program_income_full_clp_m1: natural log of projected monthly CLP income associated with first HE enrollment. It is a projected program/institution outcome, not observed individual earnings. Any income assigned to non-enrollees is excluded from this regression. The log outcome is verified against the existing positive income level.",
  "Family income share = (R-squared with family-income indicators - R-squared without them) / R-squared with them. The reduced model retains the entire score polynomial and admission-year indicators. This measures incremental explanatory power conditional on scores and year; it is not a causal share and does not allocate their shared explanatory power. All R-squared measures are ordinary unadjusted in-sample values. Family income's partial R-squared, using 1 minus reduced-model R-squared as denominator, is saved separately.",
  paste0("The ", fmt_n(coverage$n_admission_2026), " admission-2026 students have no recorded HE enrollment and remain in the enrollment OLS. They are absent from the enrollee-only income regression. Factor levels and cluster counts are rebuilt for each sample. ",
    if (high_math_only) "Both outcomes require the original z_math >= 1 cutoff, defined before filtering. No school-VA-availability restriction is imposed." else
      "Neither outcome is subject to a high-score cutoff or school-VA-availability condition."),
  "Validation covers unique-ID linkage, exact shared covariates, income/enrollment coverage, the natural-log identity, full and reduced normal equations, independently reconstructed clustered covariance matrices, nested-model projection identities, exact sample membership, and unchanged source checksums. Enrollment LPM fitted values are not clipped.")
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
 '<title>HE enrollment and projected income: flexible-score OLS</title>',
 '<style>body{max-width:1050px;margin:36px auto;padding:0 24px;font:16px/1.5 system-ui;color:#233143}h1{font-size:28px}h2{font-size:23px;margin-top:32px}.intro{color:#526171}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums;margin:22px 0}th,td{padding:9px 12px;text-align:right;border-bottom:1px solid #e7ebef}th:first-child{text-align:left}thead{background:#eef3f7}tbody th{font-weight:500}.se{display:block;color:#64748b;font-size:13px}.block>*{border-top:2px solid #a8b5c2}.note{font-size:13px;color:#526171;margin:10px 0}details{margin-top:22px}summary{cursor:pointer;font-weight:600}</style></head><body>',
 if (high_math_only) '<h1>HE enrollment and projected income: math at least +1 SD</h1>' else
   '<h1>HE enrollment and projected income</h1>',
 '<p class="intro">OLS with full cubic math/verbal controls, family-income-decile indicators, and admission-year effects.</p>',
 paste0('<p><strong>Different units:</strong> log points for projected income among enrollees; percentage points for HE enrollment in the ',
   if (high_math_only) 'high-math sample.</p>' else 'full scored sample.</p>'),
 header_html("Regressor / statistic", titles), rows, stat_rows, '</tbody></table>',
 '<details><summary>Raw coefficients on the polynomial score basis</summary>', header_html("Score term", titles),
 vapply(raw_terms, function(key) row_html(key, vapply(outcomes, function(o) effect_cell(key, o), character(1))), character(1)),
 '</tbody></table><p class="note">Raw polynomial coefficients are not constant one-SD effects. Intercepts, admission-year coefficients, confidence intervals, and covariance matrices are saved in the clean-data task folder.</p></details>',
 '<h2>Specification and interpretation</h2>', paste0('<p class="note">', notes, '</p>'), '</body></html>')
writeLines(html, file.path(reports, "regression_table.html"), useBytes = TRUE)
tex <- c("\\begin{table}[!htbp]\\centering",
 if (high_math_only) "\\caption{HE enrollment and projected income: math at least one SD above the mean}" else
   "\\caption{HE enrollment and projected income with cubic-score controls}",
 if (high_math_only) "\\label{tab:high-math-income-enrollment-cubic-income}" else "\\label{tab:income-enrollment-cubic-income}",
 "\\small", "\\begin{tabular}{lrr}\\hline\\hline",
 paste0(" & \\shortstack{Log projected income\\\\HE enrollees} & \\shortstack{HE enrollment\\\\",
   if (high_math_only) "High-math students" else "All scored students", "} \\\\ \\hline"))
for (j in seq_along(terms)) tex <- c(tex, paste0(term_labels[j], " & ",
  paste(vapply(outcomes, function(o) effect_cell(terms[j], o, FALSE), character(1)), collapse = " & "), " \\\\"))
tex <- c(tex, "\\hline", vapply(seq_along(stats), function(j) paste0(
  gsub("%", "\\%", names(stats)[j], fixed = TRUE), " & ",
  paste(gsub("%", "\\%", gsub("<", "$<$", stats[[j]], fixed = TRUE), fixed = TRUE), collapse = " & "), " \\\\"), character(1)),
 "\\hline\\hline\\end{tabular}", "\\begin{minipage}{0.98\\textwidth}\\footnotesize",
 paste0("\\textit{Notes:} Projected-income effects are in natural-log points; enrollment effects are in percentage points. School-clustered CR1 SEs are in parentheses. Score rows are average local derivatives per original score SD. Both models contain the full total-degree-three math/verbal polynomial, nine income-decile indicators, and admission-year indicators. Decile 1 is the reference. Income share is $(R^2_{full}-R^2_{without\\ family\\ income})/R^2_{full}$, as a percentage. It is an incremental descriptive contribution conditional on scores/year. Projected income uses only HE enrollees, all of whom have valid income. ",
   if (high_math_only) "Both samples require original $z_{math} \\geq 1$, without restandardizing scores or re-ranking income. " else
     "Enrollment uses the full prior scored sample. ",
   "The ", fmt_n(coverage$n_admission_2026), " admission-2026 students remain only in the enrollment model."),
 "\\end{minipage}", "\\end{table}")
writeLines(tex, file.path(tables, "regression_table.tex"), useBytes = TRUE)
message("Saved two-outcome HTML and LaTeX tables with explicit log-point / percentage-point units.")
