# Probability-scale effects and explicitly comparable fit measures for four logits.
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
task <- "premium_field_groups_logit_cubic_scores_income"
clean <- file.path(repo, "data/clean", task)
reports <- file.path(repo, "output/reports", task); tables <- file.path(repo, "output/tables", task)
for (p in c(reports, tables)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
e <- fread(file.path(clean, "probability_effects.csv")); s <- fread(file.path(clean, "model_summary.csv"))
b <- fread(file.path(clean, "coefficients_log_odds.csv")); counts <- fread(file.path(clean, "sample_counts.csv"))
stopifnot(nrow(s) == 4L, is.numeric(s$auc), all(is.finite(s$auc)), all(s$auc >= 0 & s$auc <= 1),
          all(is.finite(e[type != "reference", estimate])), all(is.finite(e[type != "reference", std_error])))
outcomes <- c("science", "law", "engineering", "medicine_plus")
titles <- c("Science", "Law", "Engineering", "Medicine+")
effects <- c("ame_math", "ame_verbal", paste0("income_decile", 1:10))
labels <- c("Math: average marginal effect (per SD)", "Verbal: average marginal effect (per SD)",
            "Income decile 1", paste0("Income decile ", 2:10, " vs. 1"))
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
pp <- function(x) ifelse(abs(100 * x) < .005, 0, 100 * x)
row_html <- function(label, values, cls = "") paste0('<tr class="', cls, '"><th scope="row">', label,
  '</th>', paste0('<td>', values, '</td>', collapse = ''), '</tr>')
effect_cell <- function(key, o, html = TRUE) {
  z <- e[effect == key & outcome == o]
  stopifnot(nrow(z) == 1L)
  if (key == "income_decile1") return("Reference")
  if (html) sprintf('%.2f<span class="se">(%.2f)</span>', pp(z$estimate), 100 * z$std_error)
  else sprintf('%.2f (%.2f)', pp(z$estimate), 100 * z$std_error)
}
rows <- vapply(seq_along(effects), function(j) row_html(labels[j],
  vapply(outcomes, function(o) effect_cell(effects[j], o), character(1)), if (j == 3L) "block" else ""), character(1))
stat_values <- list("Outcome mean (%)" = sprintf("%.2f", 100 * s$mean_outcome),
  "Observations" = fmt_n(s$N), "School clusters" = fmt_n(s$n_schools),
  "McFadden pseudo-R-squared" = sprintf("%.4f", s$mcfadden_r2),
  "Logit squared-error R-squared" = sprintf("%.4f", s$logit_brier_r2),
  "OLS R-squared (same sample)" = sprintf("%.4f", s$ols_r2_same_sample),
  "Tjur R-squared" = sprintf("%.4f", s$tjur_r2),
  "AUC (in-sample)" = sprintf("%.3f", s$auc), "Admission-year effects" = rep("Yes", 4))
stat_rows <- vapply(seq_along(stat_values), function(j) row_html(names(stat_values)[j], stat_values[[j]],
  if (j == 1L) "block" else ""), character(1))
notes <- c(
  "Entries are probability effects in percentage points, with attended-high-school-clustered delta-method standard errors in parentheses. Income rows are average discrete probability differences: predict every student at the indicated decile and at decile 1, holding that student's math, verbal and admission year fixed, then average the difference.",
  "Score rows are average partial derivatives of the fitted probability with respect to the standardized score. They account for every square, cube, interaction and the logistic link. They are local slopes averaged over the observed sample, not finite one-SD changes or raw log-odds coefficients.",
  "All models contain the full total-degree-three score polynomial: M, V, M^2, V^2, MV, M^3, V^3, M^2V, MV^2; income-decile indicators (D1 reference); and admission-year indicators. Original within-admission-year score standardization is unchanged.",
  paste0("The original OLS sample has ", fmt_n(counts$original_n), " students. All ", fmt_n(counts$excluded_n),
    " admission-", counts$excluded_years, " records have zero events in every field and are omitted from all logits because the corresponding year effect has no finite maximum-likelihood estimate. The displayed OLS comparison is refit on the same ", fmt_n(counts$N), " students as logit. Non-enrollees remain zero outcomes; no high-score or HE-enrollment restriction is imposed."),
  "McFadden pseudo-R-squared is 1 minus full-model log likelihood divided by intercept-only log likelihood. It is not a variance-explained measure. Logit squared-error R-squared is 1 - mean((Y-p)^2)/mean((Y-mean(Y))^2); this is directly comparable to the displayed OLS R-squared on the same observations. Tjur R-squared is mean(p|Y=1)-mean(p|Y=0). AUC measures discrimination (0.5 corresponds to chance ranking). These are in-sample metrics, not predictive validation on new students.",
  "The four binary logits separately model enrollment in each field versus all other outcomes. Engineering includes manufacturing and construction. Medicine+ includes medicine, pharmacy, nursing, midwifery, medical technology and dentistry. Students outside a group, including non-enrollees, are zero. Outcome groups remain mutually exclusive; separate nonlinear models do not impose additive coefficients or probabilities.",
  "The covariance uses clustered logit score sums and the inverse observed-information matrix at the converged estimates, with CR1 correction G/(G-1)*(N-1)/(N-K). Confidence intervals use a normal approximation. The analytic gradients are checked against finite differences.")
raw_terms <- c("z_math", "z_verbal", "math2", "verbal2", "math_verbal", "math3", "verbal3", "math2_verbal", "math_verbal2", paste0("income_decile", 2:10))
raw_rows <- vapply(raw_terms, function(t) row_html(t, vapply(outcomes, function(o) {
  z <- b[term == t & outcome == o]; sprintf('%.3f<span class="se">(%.3f)</span>', z$estimate, z$std_error)
}, character(1))), character(1))
header <- paste0('<table><thead><tr><th>Regressor / statistic</th>', paste0('<th>', titles, '</th>', collapse = ''), '</tr></thead><tbody>')
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
 '<title>Logit: premium-field groups with cubic score controls</title>',
 '<style>body{max-width:1180px;margin:36px auto;padding:0 24px;font:16px/1.5 system-ui;color:#233143}h1{font-size:27px}.intro{color:#526171}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums;margin:22px 0}th,td{padding:9px 12px;text-align:right;border-bottom:1px solid #e7ebef}th:first-child{text-align:left}thead{background:#eef3f7}tbody th{font-weight:500}.se{display:block;color:#64748b;font-size:13px}.block>*{border-top:2px solid #a8b5c2}.note{font-size:13px;color:#526171;margin:10px 0}details{margin-top:22px}summary{cursor:pointer;font-weight:600}</style></head><body>',
 '<h1>Premium-field enrollment: logit with cubic score controls</h1>',
 '<p class="intro">Probability effects in percentage points, clustered standard errors, and a common-sample comparison with OLS.</p>',
 header, rows, stat_rows, '</tbody></table>', paste0('<p class="note">', notes, '</p>'),
 '<details><summary>Raw log-odds coefficients and clustered standard errors</summary>', header, raw_rows,
 '</tbody></table><p class="note">These coefficients are in log-odds units, not percentage points. All year coefficients and confidence intervals are retained in the analysis CSV.</p></details></body></html>')
writeLines(html, file.path(reports, "regression_table.html"), useBytes = TRUE)
tex <- c("\\begin{table}[!htbp]\\centering", "\\caption{Premium-field enrollment: cubic-score logit probability effects}",
 "\\label{tab:premium-field-logit-cubic}", "\\small", "\\begin{tabular}{lrrrr}\\hline\\hline",
 " & Science & Law & Engineering & Medicine+ \\\\ \\hline")
for (j in seq_along(effects)) {
  tex <- c(tex, paste0(labels[j], " & ", paste(vapply(outcomes, function(o) effect_cell(effects[j], o, FALSE), character(1)), collapse = " & "), " \\\\"))
}
tex <- c(tex, "\\hline", vapply(seq_along(stat_values), function(j) paste0(gsub("%", "\\%", names(stat_values)[j], fixed = TRUE),
  " & ", paste(stat_values[[j]], collapse = " & "), " \\\\"), character(1)),
 "\\hline\\hline\\end{tabular}", "\\begin{minipage}{0.98\\textwidth}\\footnotesize",
 "\\textit{Notes:} Probability effects and school-clustered delta-method SEs (parentheses), both in percentage points. Income rows average discrete contrasts relative to decile 1, holding observed scores/year fixed. Score rows are average partial derivatives, incorporating the full cubic score polynomial and interactions. Admission-year effects are included. The 5,105 admission-2026 observations with no field events are omitted; OLS is refit on the identical logit sample. McFadden pseudo-$R^2$ is likelihood-based and not directly comparable to OLS $R^2$. The squared-error $R^2$ uses the same Brier-loss denominator as OLS. All fit metrics are in-sample. Engineering includes manufacturing/construction; Medicine+ uses the original six-program health-field list.",
 "\\end{minipage}", "\\end{table}")
writeLines(tex, file.path(tables, "regression_table.tex"), useBytes = TRUE)
message("Saved logit probability-effect tables and fit comparisons.")
