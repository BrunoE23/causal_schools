# Present four field-component LPMs with a full bivariate cubic score polynomial.
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/premium_field_groups_cubic_scores_income")
tables <- file.path(repo, "output/tables/premium_field_groups_cubic_scores_income")
reports <- file.path(repo, "output/reports/premium_field_groups_cubic_scores_income")
for (p in c(tables, reports)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
b <- fread(file.path(clean, "coefficients.csv"))
s <- fread(file.path(clean, "model_summary.csv"))
flow <- fread(file.path(clean, "sample_flow.csv"))
outcomes <- c("science", "law", "engineering", "medicine_plus")
titles <- c("Science", "Law", "Engineering / Manufacturing / Construction", "Medicine+")
terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3",
           "math2_verbal", "math_verbal2", paste0("income_decile", 2:10))
labels <- c("Math (M)", "Verbal (V)", "M^2", "V^2", "M x V", "M^3", "V^3", "M^2 x V", "M x V^2",
            paste0("Income decile ", 2:10, " vs. 1"))
tex_labels <- c("Math ($M$)", "Verbal ($V$)", "$M^2$", "$V^2$", "$M V$", "$M^3$", "$V^3$",
                "$M^2 V$", "$M V^2$", paste0("Income decile ", 2:10, " vs. 1"))
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
display_pp <- function(x) ifelse(abs(100 * x) < .005, 0, 100 * x)
cell <- function(term_key, outcome_key, html = FALSE) {
  z <- b[term == term_key & outcome == outcome_key]
  stopifnot(nrow(z) == 1L)
  if (html) sprintf('%.2f<span class="se">(%.2f)</span>', display_pp(z$estimate), 100 * z$std_error)
  else sprintf('%.2f (%.2f)', display_pp(z$estimate), 100 * z$std_error)
}
row_html <- function(label, values, cls = "") paste0('<tr class="', cls, '"><th scope="row">', label,
  '</th>', paste0('<td>', values, '</td>', collapse = ''), '</tr>')
rows <- vapply(seq_along(terms), function(j) row_html(labels[j],
  vapply(outcomes, function(o) cell(terms[j], o, TRUE), character(1)),
  if (j == 10L) "block" else ""), character(1))
stat_values <- list(
  "Outcome mean (%)" = sprintf("%.2f", 100 * s$mean_outcome),
  "Observations" = fmt_n(s$N),
  "Attended-school clusters" = fmt_n(s$n_schools),
  "R-squared" = sprintf("%.3f", s$r_squared),
  "Previous linear-score R-squared" = sprintf("%.3f", s$previous_r_squared),
  "Admission-year effects" = rep("Yes", 4))
stats <- vapply(seq_along(stat_values), function(j) row_html(names(stat_values)[j], stat_values[[j]],
  if (j == 1L) "block" else ""), character(1))
notes <- c(
  "Entries are OLS coefficients multiplied by 100, in percentage points. Parentheses report standard errors clustered by attended high school (CR1 finite-sample correction; t inference with number of schools minus one degrees of freedom).",
  "M and V are the original admission-year standardized math and verbal scores. Include M, V, M^2, V^2, MV, M^3, V^3, M^2V and MV^2: the full polynomial of total degree at most three. Individual polynomial coefficients are not constant +1 SD effects. Income deciles enter as indicators; decile 1 is the reference. Admission-year effects remain included. No score-income interactions or score decile indicators are used.",
  "All four regressions use the same complete sample from grade-8 cohorts 2017-2020, ages 12-16, with attended high school observed. Both scores and the existing imputed grade-4 family-income decile must be observed. No high-math threshold, HE-enrollment condition, or school-VA-availability restriction is imposed.",
  "Each outcome indicates first-HE enrollment in the specified group. Science, Law, and Engineering/Manufacturing/Construction use the existing field flags. Medicine+ includes medicine, pharmacy, nursing, midwifery, medical technology, and dentistry, preserving the original premium-field positive list. Students outside a group, including non-enrollees, are zero for its outcome.",
  "The four groups are mutually exclusive and sum exactly to the premium-field indicator. Their coefficients sum to the aggregate premium-field OLS coefficient vector using this same cubic design. This is a different design from the earlier linear-score model. Standard errors are estimated separately and are not additive. These are conditional descriptive associations.",
  paste0("The ", fmt_n(s$n_admission_2026[1L]), " students with admission year 2026 have zero for all four recorded premium outcomes. They remain in these OLS models, with an admission-year effect; unlike logit, OLS does not require outcome variation within every year. Retaining this year is a sample choice, not evidence that all these students will never enroll."))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
 '<title>Premium-field groups: cubic score controls and family income</title>',
 '<style>body{max-width:1100px;margin:36px auto;padding:0 24px;font:16px/1.5 system-ui;color:#233143}h1{font-size:27px;margin-bottom:6px}.intro{color:#526171;margin-top:0}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums;margin:24px 0}th,td{padding:9px 12px;text-align:right;border-bottom:1px solid #e7ebef}th:first-child{text-align:left}thead{background:#eef3f7}tbody th{font-weight:500}.se{display:block;color:#64748b;font-size:13px}.block>*{border-top:2px solid #a8b5c2}.note{font-size:13px;color:#526171;margin:9px 0}details{margin-top:18px}summary{cursor:pointer;font-weight:600}a{color:#185b84}</style></head><body>',
 '<h1>Premium-field groups with cubic score controls</h1>',
 '<p class="intro">Four OLS linear probability models. Full cubic math/verbal polynomial, including interactions. Coefficients and standard errors multiplied by 100.</p>',
 '<table><thead><tr><th>Regressor</th>', paste0('<th>', titles, '</th>', collapse = ''), '</tr></thead><tbody>',
 rows, stats, '</tbody></table>', paste0('<p class="note">', notes, '</p>'),
 '<details><summary>Sample flow and LPM prediction diagnostics</summary>',
 '<table><thead><tr><th>Stage</th><th>Students</th></tr></thead><tbody>',
 vapply(seq_len(nrow(flow)), function(j) row_html(flow$stage[j], fmt_n(flow$N[j])), character(1)),
 '</tbody></table>',
 '<p class="note">Linear probability models can produce fitted values outside [0,1]. The table above reports OLS coefficients; fitted probabilities are not clipped.</p>',
 '<table><thead><tr><th>Outcome</th><th>Fitted below 0 (%)</th><th>Fitted above 1 (%)</th></tr></thead><tbody>',
 vapply(seq_len(nrow(s)), function(j) row_html(s$outcome_label[j],
  sprintf('%.2f', 100 * c(s$share_predictions_below_zero[j], s$share_predictions_above_one[j]))), character(1)),
 '</tbody></table></details></body></html>')
writeLines(html, file.path(reports, "regression_table.html"), useBytes = TRUE)

tex <- c("% Four OLS linear probability models; coefficients and SEs are percentage points.",
  "\\begin{table}[!htbp]\\centering", "\\caption{Premium-field groups with cubic entrance-exam score controls}",
  "\\label{tab:premium-field-groups-cubic-scores-income}", "\\small", "\\begin{tabular}{lrrrr}\\hline\\hline",
  " & Science & Law & Engineering & Medicine+ \\\\ \\hline")
for (j in seq_along(terms)) {
  z <- b[match(paste(terms[j], outcomes), paste(term, outcome))]
  tex <- c(tex, paste0(tex_labels[j], " & ", paste(sprintf("%.2f", display_pp(z$estimate)), collapse = " & "), " \\\\"),
    paste0(" & ", paste(sprintf("(%.2f)", 100 * z$std_error), collapse = " & "), " \\\\"))
}
tex <- c(tex, "\\hline",
  vapply(seq_along(stat_values), function(j) paste0(gsub("%", "\\%", names(stat_values)[j], fixed = TRUE),
    " & ", paste(stat_values[[j]], collapse = " & "), " \\\\"), character(1)),
  "\\hline\\hline\\end{tabular}", "\\begin{minipage}{0.98\\textwidth}\\footnotesize",
  "\\textit{Notes:} OLS coefficients and school-clustered standard errors multiplied by 100. Math ($M$) and verbal ($V$) retain their original admission-year SD units. The full score polynomial has total degree at most three. Polynomial coefficients are not constant one-SD effects. Income decile 1 is omitted; admission-year effects are included. The sample and outcomes are unchanged. Engineering includes manufacturing and construction; Medicine+ includes medicine, pharmacy, nursing, midwifery, medical technology and dentistry. Students outside each group, including non-enrollees, are zero. The 5,105 admission-2026 observations remain.",
  "\\end{minipage}", "\\end{table}")
writeLines(tex, file.path(tables, "regression_table.tex"), useBytes = TRUE)
message("Saved HTML and LaTeX regression tables.")
