# Report cubic OLS models and income's incremental share of explained variation.
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% "--high-math-main"))
high_math_main <- "--high-math-main" %in% args
task <- if (high_math_main) "high_math_main_cubic_income_contribution" else "premium_cubic_income_contribution"
share_digits <- if (high_math_main) 2L else 1L
clean <- file.path(repo, "data/clean", task)
reports <- file.path(repo, "output/reports", task); tables <- file.path(repo, "output/tables", task)
for (p in c(reports, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
s <- fread(file.path(clean, "income_contribution.csv"))
b <- fread(file.path(clean, "coefficients.csv"))
ame <- fread(file.path(clean, "score_average_derivatives.csv"))
tests <- fread(file.path(clean, "income_joint_tests.csv"))
stopifnot(nrow(s) == if (high_math_main) 4L else 8L,
          all(s$N == if (high_math_main) 93423L else 565040L),
          all(s$n_schools == if (high_math_main) 2735L else 3372L),
          all(is.finite(s$income_share_explained)), all(s$income_share_explained >= 0 & s$income_share_explained <= 1),
          all(abs(s$income_share_explained * s$r2_full - s$delta_r2_income) < 1e-12))
groups <- list(main = c("high_inst_m1", "high_paying_field_m1", "high_either", "high_both"),
               fields = c("science", "law", "engineering", "medicine_plus"))
if (high_math_main) {
  groups <- groups["main"]
  selection <- fread(file.path(clean, "sample_selection.csv"))
  stopifnot(selection$min_z_math >= 1, isTRUE(selection$exact_prior_high_math_ids),
            identical(s$outcome, groups$main))
}
titles <- list(main = c("Institution", "Field", "Either", "Both"),
               fields = c("Science", "Law", "Engineering", "Medicine+"))
group_titles <- c(main = "Main premium-enrollment outcomes", fields = "Four premium-field groups")
terms <- c("ame_math", "ame_verbal", paste0("income_decile", 1:10))
term_labels <- c("Math: average derivative (per SD)", "Verbal: average derivative (per SD)",
                 "Income decile 1", paste0("Income decile ", 2:10, " vs. 1"))
raw_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3", "math2_verbal", "math_verbal2")
fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
pp <- function(x) ifelse(abs(100 * x) < .005, 0, 100 * x)
fmt_p <- function(x) ifelse(x < .001, "<0.001", sprintf("%.3f", x))
row_html <- function(label, values, cls = "") paste0('<tr class="', cls, '"><th scope="row">', label,
  '</th>', paste0('<td>', values, '</td>', collapse = ''), '</tr>')
header_html <- function(first, headers) paste0('<table><thead><tr><th>', first, '</th>',
  paste0('<th>', headers, '</th>', collapse = ''), '</tr></thead><tbody>')
effect_cell <- function(key, o, html = TRUE) {
  if (key == "income_decile1") return("Reference")
  z <- if (key %in% c("ame_math", "ame_verbal")) ame[term == key & outcome == o] else b[term == key & outcome == o]
  stopifnot(nrow(z) == 1L, is.finite(z$estimate), is.finite(z$std_error))
  if (html) sprintf('%.2f<span class="se">(%.2f)</span>', pp(z$estimate), 100 * z$std_error)
  else sprintf('%.2f (%.2f)', pp(z$estimate), 100 * z$std_error)
}
summary_values <- function(j) c(sprintf("%.4f", s$r2_without_income[j]), sprintf("%.4f", s$r2_full[j]),
                               sprintf("%.4f", s$delta_r2_income[j]), sprintf("%.*f%%", share_digits, 100 * s$income_share_explained[j]))
contribution_html <- c(header_html("Outcome", c("R-squared without income", "Full R-squared",
  "Income gain in R-squared", "Income share of explained variation")),
  vapply(seq_len(nrow(s)), function(j) row_html(s$outcome_label[j], summary_values(j),
    if (j == 5L) "block" else ""), character(1)), '</tbody></table>')
regression_html <- list()
for (group in names(groups)) {
  os <- groups[[group]]; ss <- s[match(os, outcome)]; tt <- tests[match(os, outcome)]
  rows <- vapply(seq_along(terms), function(j) row_html(term_labels[j],
    vapply(os, function(o) effect_cell(terms[j], o), character(1)), if (j == 3L) "block" else ""), character(1))
  stats <- list("Outcome mean (%)" = sprintf("%.2f", 100 * ss$mean_outcome),
    "Full R-squared" = sprintf("%.4f", ss$r2_full),
    "R-squared without income" = sprintf("%.4f", ss$r2_without_income),
    "Income share of explained variation (%)" = sprintf("%.*f", share_digits, 100 * ss$income_share_explained),
    "Income indicators: joint clustered p-value" = fmt_p(tt$p_value),
    "Observations" = fmt_n(ss$N), "School clusters" = fmt_n(ss$n_schools),
    "Admission-year indicators" = rep("Yes", 4))
  stat_rows <- vapply(seq_along(stats), function(j) row_html(names(stats)[j], stats[[j]],
    if (j == 1L) "block" else ""), character(1))
  regression_html[[group]] <- c(paste0('<h2>', group_titles[group], '</h2>'),
    header_html("Regressor / statistic", titles[[group]]), rows, stat_rows, '</tbody></table>',
    '<details><summary>Raw coefficients on the polynomial score basis</summary>',
    header_html("Score term", titles[[group]]), vapply(raw_terms, function(key) row_html(key,
      vapply(os, function(o) effect_cell(key, o), character(1))), character(1)), '</tbody></table>',
    '<p class="note">Raw basis coefficients are in percentage points but are not constant one-SD effects. Intercepts, admission-year coefficients, confidence intervals, and covariance matrices are saved with the analysis.</p></details>')
  tex <- c("\\begin{table}[!htbp]\\centering", paste0("\\caption{", group_titles[group],
    if (high_math_main) ": cubic-score OLS among high-math students}" else ": cubic-score OLS}"),
    paste0("\\label{tab:", if (high_math_main) "high-math-" else "", "premium-cubic-income-", group, "}"),
    "\\small", "\\begin{tabular}{lrrrr}\\hline\\hline",
    paste0(" & ", paste(titles[[group]], collapse = " & "), " \\\\ \\hline"))
  for (j in seq_along(terms)) tex <- c(tex, paste0(term_labels[j], " & ",
    paste(vapply(os, function(o) effect_cell(terms[j], o, FALSE), character(1)), collapse = " & "), " \\\\"))
  tex <- c(tex, "\\hline", vapply(seq_along(stats), function(j) paste0(
    gsub("%", "\\%", names(stats)[j], fixed = TRUE), " & ",
    paste(gsub("<", "$<$", stats[[j]], fixed = TRUE), collapse = " & "), " \\\\"), character(1)),
    "\\hline\\hline\\end{tabular}", "\\begin{minipage}{0.98\\textwidth}\\footnotesize",
    paste0("\\textit{Notes:} Income-decile coefficients and score average derivatives are in percentage points; school-clustered CR1 SEs are in parentheses. Score controls include $M,V,M^2,V^2,MV,M^3,V^3,M^2V,MV^2$. Income decile 1 is the reference. Admission-year effects are included in both full and reduced models. Income share is $(R^2_{full}-R^2_{without\\ income})/R^2_{full}$, expressed as a percentage; it measures incremental explanatory power after scores and year, not a causal contribution. ",
      if (high_math_main) "All models require original $z_{math} \\geq 1$, without restandardizing scores or re-ranking income. " else "The original OLS sample is retained. ",
      "The sample includes ", fmt_n(ss$n_non_enrollees[1L]), " non-enrollees and ", fmt_n(ss$n_admission_2026[1L]), " admission-2026 records. ",
      if (high_math_main) "Either is the inclusive union; all four outcomes are zero for non-enrollees." else
        "Engineering includes manufacturing/construction; Medicine+ retains the six-program health-field definition."),
    "\\end{minipage}", "\\end{table}")
  writeLines(tex, file.path(tables, paste0("regression_", group, ".tex")), useBytes = TRUE)
}
notes <- c(
  "The full model contains M, V, M^2, V^2, MV, M^3, V^3, M^2V and MV^2, nine income-decile indicators (D1 reference), and admission-year indicators. Scores retain their original within-admission-year standardization; income deciles retain the existing imputed baseline-income classification. The reduced model drops only the income indicators, retaining the identical students and all score/year controls.",
  "Income share = (full R-squared - R-squared without income) / full R-squared. This is income's incremental contribution when added after scores and year, as a fraction of the variation explained by the full model (including year effects). It does not allocate explanatory power shared by income and scores. It is not a causal share. All R-squared values are ordinary unadjusted, in-sample measures.",
  "Income's partial R-squared instead divides the R-squared gain by 1 minus reduced-model R-squared. It describes the fraction of previously unexplained variation removed by adding income, and is saved separately in income_contribution.csv. It is not the percentage displayed as income share.",
  "Income rows below are OLS coefficients relative to decile 1. Score rows are average partial derivatives of the full cubic polynomial per SD, not finite one-SD changes. Coefficients and derivatives are multiplied by 100, with actual attended-school-clustered CR1 standard errors in parentheses. Inference uses t(G-1); the nine income coefficients are tested jointly using a clustered Wald F test.",
  paste0("The common sample has ", fmt_n(s$N[1L]), " students in ", fmt_n(s$n_schools[1L]),
    " schools, including ", fmt_n(s$n_non_enrollees[1L]), " HE non-enrollees. ",
    if (high_math_main) "The sample requires original z_math >= 1 and exactly matches the preceding high-math HE-enrollment regression. Scores are not restandardized and income deciles are not re-ranked. " else
      "There is no high-score restriction. ",
    "There is no HE-enrollment or school-VA-availability condition. The ", fmt_n(s$n_admission_2026[1L]),
    " admission-2026 records have zero recorded premium outcomes and remain in OLS; this is not evidence about future enrollment."),
  paste0("Main outcomes are enrollment in a premium institution, a premium field, either (inclusive union), and both. ",
    if (high_math_main) "" else "The four field groups are mutually exclusive and sum to the premium-field flag. Engineering includes manufacturing and construction. Medicine+ contains medicine, pharmacy, nursing, midwifery, medical technology, and dentistry. ",
    "All outcomes are zero for non-enrollees. R-squared contributions and standard errors do not add across these outcomes."),
  paste0("Checks verify common samples, normal equations, full and reduced clustered covariance matrices, the nested-model projection identity, ",
    if (high_math_main) "and the coefficient identity either = institution + field - both in both specifications. " else
      "both outcome coefficient identities, and exact reproduction of the earlier cubic field-group OLS coefficients and SEs. ",
    "LPM fitted values are not clipped; fractions outside [0,1] are retained in the analysis diagnostics."))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
 '<title>Flexible-score OLS: income contribution</title>',
 '<style>body{max-width:1200px;margin:36px auto;padding:0 24px;font:16px/1.5 system-ui;color:#233143}h1{font-size:28px}h2{font-size:23px;margin-top:40px}.intro{color:#526171}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums;margin:22px 0}th,td{padding:9px 12px;text-align:right;border-bottom:1px solid #e7ebef}th:first-child{text-align:left}thead{background:#eef3f7}tbody th{font-weight:500}.se{display:block;color:#64748b;font-size:13px}.block>*{border-top:2px solid #a8b5c2}.note{font-size:13px;color:#526171;margin:10px 0}details{margin-top:22px}summary{cursor:pointer;font-weight:600}</style></head><body>',
 if (high_math_main) '<h1>Premium enrollment among students with math at least +1 SD</h1>' else
   '<h1>Flexible-score OLS: how much does income add?</h1>',
 paste0('<p class="intro">', if (high_math_main) 'Four' else 'Eight',
   ' linear probability models. Full cubic math/verbal controls, income-decile indicators, and admission-year effects.</p>'),
 '<p><strong>Income share = (R-squared with income - R-squared without income) / R-squared with income.</strong> Both models use the same sample and score/year controls.</p>',
 contribution_html, unlist(regression_html, use.names = FALSE), '<h2>Specification and interpretation</h2>',
 paste0('<p class="note">', notes, '</p>'), '</body></html>')
writeLines(html, file.path(reports, "regression_table.html"), useBytes = TRUE)
tex_summary <- c("\\begin{table}[!htbp]\\centering",
 if (high_math_main) "\\caption{Income's incremental contribution among high-math students}" else
   "\\caption{Income's incremental contribution to cubic-score OLS fit}",
 if (high_math_main) "\\label{tab:high-math-main-cubic-income-contribution}" else "\\label{tab:premium-cubic-income-contribution}",
 "\\small", "\\begin{tabular}{lrrrr}\\hline\\hline",
 "Outcome & $R^2$ without income & Full $R^2$ & Income $\\Delta R^2$ & Income share (\\%) \\\\ \\hline",
 vapply(seq_len(nrow(s)), function(j) paste0(s$outcome_label[j], " & ",
   paste(c(sprintf("%.4f", c(s$r2_without_income[j], s$r2_full[j], s$delta_r2_income[j])),
           sprintf("%.*f", share_digits, 100 * s$income_share_explained[j])), collapse = " & "), " \\\\"), character(1)),
 "\\hline\\hline\\end{tabular}", "\\begin{minipage}{0.98\\textwidth}\\footnotesize",
 paste0("\\textit{Notes:} Income share is the increase in ordinary unadjusted $R^2$ from adding nine income-decile indicators, divided by full-model $R^2$. Both models retain all nine total-degree-three math/verbal terms and admission-year indicators, using the same ", fmt_n(s$N[1L]), " students. ",
   if (high_math_main) "Selection requires original $z_{math} \\geq 1$. " else "",
   "This is the unique incremental contribution after score/year controls; shared explanatory variation is not allocated to income. The ", fmt_n(s$n_admission_2026[1L]), " admission-2026 records remain. All fit measures are in-sample and descriptive."),
 "\\end{minipage}", "\\end{table}")
writeLines(tex_summary, file.path(tables, "income_contribution.tex"), useBytes = TRUE)
message("Saved contribution summary, ", length(groups), " regression table(s), and HTML report.")
