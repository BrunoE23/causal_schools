# Render saved priority-by-continuation models; this script fits no regressions.
source("code/codex/school_preference_va/common.R")
group_dir <- file.path(clean_dir, "by_priority_continuation")
read_group <- function(name) fread(file.path(group_dir, name))
coef_dt <- read_group("coefficients.csv")
samples <- read_group("model_summary.csv")
conditions <- read_group("model_diagnostics.csv")
flow <- read_group("sample_flow.csv")
definitions <- read_group("group_definitions.csv")
verification <- read_group("verification.csv")
manifest <- read_group("source_manifest.csv")
normalization <- read_group("va_normalization.csv")
type_coverage <- fread(file.path(clean_dir, "school_type_coverage.csv"))

groups <- c("ending_nonpriority", "ending_priority", "voluntary_nonpriority", "voluntary_priority")
headers <- c("School ends: non-prioritario", "School ends: prioritario",
  "Voluntary: non-prioritario", "Voluntary: prioritario")
names(headers) <- groups
terms <- c(model_static_terms, "distance")
n_coefficients <- length(terms)
check("Subgroup specification contains four VAs, two school-type indicators and distance",
  identical(controls, c("mixed_hc_tp", "tp_only")) &&
  identical(model_static_terms, c(va_terms, controls)) && n_coefficients == 7L)
check("Four requested groups have definitions", !anyDuplicated(definitions$group) &&
  all(groups %in% definitions$group))
check("All eight subgroup models are available", nrow(samples) == 8L &&
  !anyDuplicated(samples[, .(group, model)]) && setequal(samples$group, groups) &&
  setequal(samples$model, names(distance_vars)))
check("All subgroup coefficients are available", nrow(coef_dt) == 8L * n_coefficients &&
  !anyDuplicated(coef_dt[, .(group, model, term)]))
check("Subgroup coefficients and standard errors finite", all(is.finite(coef_dt$estimate)) &&
  all(is.finite(coef_dt$std_error) & coef_dt$std_error > 0))
check("Reported conditional odds ratios match coefficients", all(is.finite(coef_dt$conditional_odds_ratio)) &&
  max(abs(coef_dt$conditional_odds_ratio - exp(coef_dt$estimate))) < 1e-10)
for (g in groups) {
  for (m in names(distance_vars)) check(paste(g, m, "requested regressors retained"),
    setequal(coef_dt[group == g & model == m, term], c(model_static_terms, distance_vars[[m]])))
  check(paste(g, "models use identical sample counts"), all(vapply(
    c("applicants", "rankings", "applicant_school_pairs", "schools", "informative_rank_decisions"),
    function(v) uniqueN(samples[group == g][[v]]) == 1L, logical(1))))
}
check("All model verification checks passed", nrow(verification) > 0L && all(verification$passed))
check("Each VA retains the pooled reference normalization", all(va_terms %in% normalization$standardized_variable) &&
  all(normalization$n_reference_schools == 2501L) && all(normalization$reference_sd > 0))

f4 <- function(x) formatC(x, digits = 4L, format = "f", big.mark = ",")
pct <- function(x) paste0(formatC(100 * x, digits = 1L, format = "f"), "%")
coef_cell <- function(b, se) paste0(f4(b), " (", f4(se), ")")
pretty_numeric <- function(x) {
  x <- copy(x)
  for (v in names(x)) if (is.numeric(x[[v]])) {
    a <- x[[v]]
    x[, (v) := if (all(is.na(a) | a == round(a))) fmt_n(a) else f4(a)]
  }
  x
}

# Keep numeric estimates and standard errors beside formatted cells for reuse.
tables <- lapply(names(distance_vars), function(m) {
  x <- data.table(model = m, row_type = "coefficient", regressor = c(specs$label,
    unname(control_labels[controls]), "log(1 + distance in km)"))
  stat <- data.table(model = m, row_type = "sample_statistic",
    regressor = c("Applicants", "Student-school pairs", "Destination schools", "Log likelihood"))
  for (g in groups) {
    a <- coef_dt[group == g & model == m][match(c(model_static_terms, distance_vars[[m]]), term)]
    x[, (paste0(g, "_estimate")) := a$estimate]
    x[, (paste0(g, "_std_error")) := a$std_error]
    x[, (g) := coef_cell(a$estimate, a$std_error)]
    s <- samples[group == g & model == m]
    vals <- vapply(c("applicants", "applicant_school_pairs", "schools", "log_likelihood"),
      function(v) as.numeric(s[[v]]), numeric(1))
    stat[, (paste0(g, "_estimate")) := vals]
    stat[, (paste0(g, "_std_error")) := NA_real_]
    stat[, (g) := c(fmt_n(vals[1:3]), f4(vals[4]))]
  }
  rbindlist(list(x, stat), use.names = TRUE)
})
names(tables) <- names(distance_vars)
regression <- rbindlist(tables)
fwrite(regression, file.path(table_dir, "by_priority_continuation.csv"))

# Fragments use the manuscript's existing booktabs table conventions.
bs <- intToUtf8(92L)
tex_escape <- function(x) {
  for (z in c("&", "%", "_", "#")) x <- gsub(z, paste0(bs, z), x, fixed = TRUE)
  x
}
tex_headers <- paste0(bs, "shortstack{", c("School ends", "School ends", "Voluntary", "Voluntary"),
  bs, bs, c("Non-prioritario", "Prioritario", "Non-prioritario", "Prioritario"), "}")
for (m in names(tables)) {
  x <- tables[[m]]
  rows <- vapply(seq_len(nrow(x)), function(i) paste0(
    if (i == n_coefficients + 1L) paste0(bs, "midrule\n") else "", tex_escape(x$regressor[i]), " & ",
    paste(unlist(x[i, ..groups], use.names = FALSE), collapse = " & "), " ", bs, bs), character(1))
  tex <- c(paste0(bs, "begin{table}[!htbp]"), paste0(bs, "centering"),
    paste0(bs, "caption{School-ranking preferences by priority and continuation status: ",
      if (m == "main") "published student coordinates" else "home-comuna centroids", "}"),
    paste0(bs, "label{tab:school-preference-groups-", m, "}"),
    paste0(bs, "begin{tabular}{lcccc}"), paste0(bs, "toprule"),
    paste0(" & ", paste(tex_headers, collapse = " & "), " ", bs, bs),
    paste0(bs, "midrule"), rows, paste0(bs, "bottomrule"), paste0(bs, "end{tabular}"),
    paste0(bs, "par", bs, "smallskip"),
    paste0(bs, "begin{minipage}{0.98", bs, "linewidth}", bs, "footnotesize"),
    paste0(bs, "textit{Notes:} Separate joint rank-ordered logits of complete submitted school lists. ",
      "Student-clustered standard errors are in parentheses. All four VA measures, mixed HC/TP and TP-only school indicators, ",
      "and distance enter jointly. The omitted non-TP category includes HC-only, artistic-only, and other schools. ",
      "VA retains the pooled model's equal-school normalization across 2,501 destination schools; it is not restandardized by group. ",
      "School ends denotes an origin school without an SAE offering above grade 8; voluntary denotes an origin offering grade 9. ",
      "These are application categories and do not require an eventual school move. Unresolved origins are excluded. ",
      "School-type indicators use regular offerings in the 2024 Directorio without an active-school restriction and remain binary. ",
      "Distance is straight-line kilometers ",
      "and enters as $", bs, "log(1+d)$. The two distance specifications use identical samples within each group. ",
      "Any missing regressor excludes the entire ranking; singleton lists provide no ordering information. ",
      "Inference conditions on the existing EB VAs. Coefficients describe conditional ordering associations. ",
      "Cross-group coefficient comparisons can also reflect differences in the unobserved utility-error scale."),
    paste0(bs, "end{minipage}"), paste0(bs, "end{table}"))
  writeLines(tex, file.path(table_dir, paste0("by_priority_continuation_", m, ".tex")))
}

# Prefer explicitly pooled rows, otherwise total the annual rows exactly once.
pooled_flow <- flow[tolower(as.character(sae_proceso)) %in% c("all", "pooled")]
if (!nrow(pooled_flow)) {
  numeric_cols <- c("original_applicants", "pooled_eligible", "excluded_singleton",
    "excluded_missing_regressor", "excluded_unclassified")
  pooled_flow <- flow[, lapply(.SD, sum), by = .(group, label), .SDcols = numeric_cols]
  pooled_flow[, sae_proceso := "All"]
}
pooled_flow <- pooled_flow[order(match(group, c(groups, "unresolved_other")))]
check("Pooled subgroup flow has one row per group", !anyDuplicated(pooled_flow$group) &&
  all(groups %in% pooled_flow$group))
for (g in groups) check(paste(g, "flow equals fitted sample"),
  pooled_flow[group == g, pooled_eligible] == samples[group == g & model == "main", applicants])
check("Sample-loss categories account for every original applicant", all(pooled_flow$original_applicants ==
  pooled_flow$pooled_eligible + pooled_flow$excluded_singleton +
  pooled_flow$excluded_missing_regressor + pooled_flow$excluded_unclassified))
flow_display <- pooled_flow[, .(Group = label, Applicants = fmt_n(original_applicants),
  Estimated = fmt_n(pooled_eligible), `Retained share` = pct(pooled_eligible / original_applicants),
  `Singleton list` = fmt_n(excluded_singleton), `Missing regressor` = fmt_n(excluded_missing_regressor),
  Unclassified = fmt_n(excluded_unclassified))]
annual_flow <- flow[!tolower(as.character(sae_proceso)) %in% c("all", "pooled")]
annual_flow <- annual_flow[order(sae_proceso, match(group, c(groups, "unresolved_other")))]
annual_display <- annual_flow[, .(Year = sae_proceso, Group = label,
  Applicants = fmt_n(original_applicants), Estimated = fmt_n(pooled_eligible),
  `Singleton list` = fmt_n(excluded_singleton), `Missing regressor` = fmt_n(excluded_missing_regressor),
  Unclassified = fmt_n(excluded_unclassified))]

# Describe signs, relative ordering and sensitivity without treating group logits as a common scale.
interpretation <- vapply(groups, function(g) {
  a <- coef_dt[group == g & model == "main" & term %in% va_terms]
  a[, label := specs$label[match(term, specs$standardized)]]
  top <- a[estimate > 0][order(-estimate)]
  positive <- if (nrow(top)) paste0("The largest positive VA point estimate is ", top$label[1L],
    " (", f4(top$estimate[1L]), ", SE ", f4(top$std_error[1L]), "), corresponding to ",
    f4(exp(top$estimate[1L])), " times the conditional odds per pooled school SD.") else
    "None of the four VA point estimates is positive."
  negatives <- a[estimate < 0]
  negative <- if (nrow(negatives)) paste0(" Negative conditional point estimates are ",
    paste0(negatives$label, " (", f4(negatives$estimate), ")", collapse = ", "), ".") else ""
  uncertain <- a[p_value > .05]
  uncertainty <- if (nrow(uncertain)) paste0(" Not statistically distinguished from zero at the 5% level: ",
    paste0(uncertain$label, " (", f4(uncertain$estimate), ", SE ", f4(uncertain$std_error), ")",
      collapse = "; "), ".") else ""
  type <- coef_dt[group == g & model == "main" & term %in% controls][match(controls, term)]
  type_text <- paste0(" Relative to the omitted non-TP category, ", paste0(
    unname(control_labels[type$term]), " has coefficient ", f4(type$estimate), " (SE ",
    f4(type$std_error), "), or ", f4(exp(type$estimate)), " times the conditional odds",
    collapse = "; "), ".")
  paste0(headers[[g]], ": ", positive, negative, uncertainty, type_text)
}, character(1))
sign_patterns <- vapply(va_terms, function(v) {
  a <- coef_dt[model == "main" & term == v][match(groups, group)]
  if (!(length(unique(sign(a$estimate[1:2]))) == 1L &&
      length(unique(sign(a$estimate[3:4]))) == 1L &&
      sign(a$estimate[1L]) != sign(a$estimate[3L]))) return("")
  paste0(specs$label[match(v, specs$standardized)], " has ",
    if (a$estimate[1L] < 0) "negative" else "positive", " main point estimates in both school-ending groups (",
    f4(a$estimate[1L]), " for non-prioritario and ", f4(a$estimate[2L]), " for prioritario) and ",
    if (a$estimate[3L] < 0) "negative" else "positive", " estimates in both voluntary groups (",
    f4(a$estimate[3L]), " and ", f4(a$estimate[4L]), ", respectively).")
}, character(1))
sign_patterns <- sign_patterns[nzchar(sign_patterns)]
all_positive <- coef_dt[model == "main" & term %in% va_terms,
  .(positive_in_every_group = all(estimate > 0)), by = term][positive_in_every_group == TRUE, term]
if (length(all_positive)) sign_patterns <- c(sign_patterns, paste0(
  paste(specs$label[match(all_positive, specs$standardized)], collapse = ", "),
  if (length(all_positive) == 1L) " has" else " have", " positive point estimates in all four main fits."))
sensitivity <- rbindlist(lapply(groups, function(g) {
  a <- coef_dt[group == g & model == "main" & term %in% va_terms][match(va_terms, term)]
  b <- coef_dt[group == g & model == "municipality" & term %in% va_terms][match(va_terms, term)]
  changes <- specs$label[sign(a$estimate) != sign(b$estimate)]
  data.table(Group = headers[[g]], `Largest absolute VA coefficient change` = f4(max(abs(a$estimate - b$estimate))),
    `VA sign changes` = if (length(changes)) paste(changes, collapse = "; ") else "None",
    `Main distance coefficient` = f4(coef_dt[group == g & model == "main" & term == distance_vars[["main"]], estimate]),
    `Comuna distance coefficient` = f4(coef_dt[group == g & model == "municipality" & term == distance_vars[["municipality"]], estimate]))
}))
main_static <- coef_dt[model == "main" & term %in% model_static_terms, .(group, term, main_estimate = estimate)]
comuna_static <- coef_dt[model == "municipality" & term %in% model_static_terms, .(group, term, comuna_estimate = estimate)]
static_comparison <- merge(main_static, comuna_static, by = c("group", "term"))
va_comparison <- static_comparison[term %in% va_terms]
type_comparison <- static_comparison[term %in% controls]
static_sign_text <- if (all(sign(va_comparison$main_estimate) == sign(va_comparison$comuna_estimate)))
  "All four VA signs persist in every group when using home-comuna centers." else
  "Some VA signs change when using home-comuna centers; the complete estimates are reported above."
type_sign_text <- if (all(sign(type_comparison$main_estimate) == sign(type_comparison$comuna_estimate)))
  " Both school-type indicator signs also persist in every group." else
  " Some school-type indicator signs change with home-comuna centers."
weak_distance <- coef_dt[model == "municipality" & term == distance_vars[["municipality"]] & p_value > .05]
weak_distance_text <- if (nrow(weak_distance)) paste0(
  " Comuna-distance estimates are not statistically distinguished from zero at the 5% level for ",
  paste0(headers[weak_distance$group], " (", f4(weak_distance$estimate), ", SE ",
    f4(weak_distance$std_error), ")", collapse = "; "),
  ".", if (any(weak_distance$estimate > 0))
    " Positive point estimates in these groups do not establish a preference for greater travel distance." else "") else ""
normalization_display <- normalization[, .(Dimension = label, Mean = f4(reference_mean),
  SD = f4(reference_sd), `Reference schools` = fmt_n(n_reference_schools))]
type_coverage_display <- type_coverage[, .(Population = population, `School type` = school_type,
  Schools = fmt_n(schools), `Student-school pairs` = fmt_n(applicant_school_pairs),
  `Rankings listing type` = fmt_n(rankings_listing_type), `Share of schools` = pct(share_schools),
  `Share of pairs` = pct(share_pairs))]
definitions_display <- definitions[group %in% groups][match(groups, group), .(Group = label, Definition = definition)]
diagnostics_text <- paste0("All ", fmt_n(nrow(verification)), " upstream checks passed. All ",
  fmt_n(nrow(samples)), " fits retain all ", n_coefficients,
  " coefficients; within each group, both distance specifications retain ",
  "identical applicants and complete school lists. Model diagnostics below document convergence, within-ranking ",
  "identification and likelihood-information conditioning.")
if ("iterations" %in% names(samples)) diagnostics_text <- paste0(diagnostics_text,
  " Iteration counts range from ", min(samples$iterations), " to ", max(samples$iterations), ".")
if ("information_condition" %in% names(conditions)) diagnostics_text <- paste0(diagnostics_text,
  " The information condition number ranges from ", f4(min(conditions$information_condition)),
  " to ", f4(max(conditions$information_condition)), ".")

table_display <- function(m) {
  a <- copy(tables[[m]][, c("regressor", groups), with = FALSE])
  setnames(a, c("Regressor", unname(headers)))
  html_table(a)
}
clean_link <- function(name, label) paste0('<a download href="../../../data/clean/', task,
  '/by_priority_continuation/', name, '">', esc_html(label), '</a>')
download_html <- paste0('<ul><li><a download href="../../tables/', task,
  '/by_priority_continuation.csv">Combined regression table (CSV)</a></li><li><a download href="../../tables/',
  task, '/by_priority_continuation_main.tex">Main table (LaTeX fragment)</a>; <a download href="../../tables/',
  task, '/by_priority_continuation_municipality.tex">Comuna table (LaTeX fragment)</a></li><li>',
  clean_link("coefficients.csv", "Numeric coefficients and confidence intervals"), '</li><li>',
  clean_link("source_manifest.csv", "Source manifest"), '; ', clean_link("sample_flow.csv", "Annual sample flow"),
  '; ', clean_link("va_normalization.csv", "Fixed pooled VA normalization"), '</li></ul>')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>School preferences by priority and continuation status</title>',
  '<style>body{font:16px/1.55 system-ui,sans-serif;color:#172536;background:#f6f8fa;margin:0}',
  'main{max-width:1400px;margin:30px auto;padding:30px;background:white;border:1px solid #dce3e9;border-radius:10px}',
  'h1{font-size:30px;line-height:1.2}h2{font-size:23px;margin-top:36px}.muted,.notes{color:#556373}',
  '.notes{font-size:14px}.table{overflow:auto;margin:18px 0}table{border-collapse:collapse;width:100%;font-size:14px}',
  'th,td{padding:10px 12px;text-align:right;border-bottom:1px solid #e3e8ed}th:first-child,td:first-child{text-align:left}',
  'thead th{background:#edf2f6;white-space:nowrap}a{color:#175d99}li{margin:6px 0}details{margin:18px 0}',
  'summary{cursor:pointer;font-weight:600}.regression tbody tr:nth-child(', n_coefficients + 1L,
  ') td{border-top:2px solid #7a8794}</style></head><body><main>',
  '<h1>School preferences by prioritario and continuation status</h1>',
  '<p class="muted">Separate joint rank-ordered logits for four groups of timely grade-9 SAE applicants, 2018-2020. ',
  'Each model includes math, exam-taking, high-premium-field and high-premium-institution VA, ',
  'mixed HC/TP and TP-only school indicators, and distance.</p>',
  '<h2>Main: published student coordinates</h2><div class="regression">', table_display("main"), '</div>',
  '<p class="notes">Coefficients with student-clustered standard errors in parentheses. VA units are fixed pooled ',
  'school SDs; school-type indicators are binary; distance enters as log(1 + kilometers).</p>',
  '<h2>Robustness: home-comuna centroids</h2><div class="regression">', table_display("municipality"), '</div>',
  '<p class="notes">Every student location is replaced by the home-comuna center; actual school-campus coordinates ',
  'are retained. These models use the same applicants, complete rankings and VA normalization as the main models within each group.</p>',
  '<h2>Conditional preference patterns</h2>', paste0('<p>', esc_html(sign_patterns), '</p>', collapse = ''),
  paste0('<p>', esc_html(interpretation), '</p>', collapse = ''),
  '<p>These coefficients describe ordering associations among schools remaining in a submitted portfolio, holding the other ',
  'six regressors fixed. They do not identify causal responses to effectiveness or explain which schools enter a portfolio. ',
  'Separate logits normalize unobserved utility errors separately: differences in coefficients across groups may reflect ',
  'different error scales as well as different preferences. No formal cross-group equality tests are reported.</p>',
  '<h2>Sensitivity to municipality-based locations</h2>', html_table(sensitivity),
  '<p>', esc_html(static_sign_text), esc_html(type_sign_text), esc_html(weak_distance_text), '</p>',
  '<p>Sign changes and coefficient movements above describe sensitivity to the location definition. Both versions use ',
  'straight-line Haversine kilometers and the same log transformation. The main version retains privacy-displaced published ',
  'student points, including municipality-imputed coordinates. The robustness version replaces every home point with its ',
  'grade-8 residence-comuna center, matched by numeric comuna code.</p>',
  '<h2>Groups and sample coverage</h2>', html_table(definitions_display),
  '<p>School-ending and voluntary status refer to the origin school\'s continuation offering at application, not to a ',
  'realized move. School-ending origins have no SAE offering above grade 8; voluntary origins offer grade 9. ',
  'Unresolved or other origin classifications are excluded. Priority status is the applicant\'s prioritario flag. ',
  'Each group starts with the existing paper roster; no assignment-risk restriction is imposed.</p>', html_table(flow_display),
  '<p>Singleton lists provide no ordering information. If any school in an informative list lacks a required regressor, ',
  'the entire ranking is excluded; retained lists are never shortened. Both distance specifications use identical complete rankings within each group.</p>',
  if (nrow(annual_display)) paste0('<details><summary>Sample flow by application year</summary>', html_table(annual_display), '</details>') else '',
  '<h2>Fixed VA normalization and school types</h2>', html_table(normalization_display),
  '<p>The existing pooled 2017-2020 All-sample EB VAs are unchanged. Every group uses the same mean and sample SD ',
  'computed with equal weight across the 2,501 unique destination schools in the pooled estimation sample. ',
  'There is no within-group restandardization. VA-estimation uncertainty is not propagated.</p>',
  '<p>Mixed HC/TP and TP-only are separate unstandardized binary school-offering indicators from the 2024 Directorio. ',
  'Mixed schools offer both regular HC and TP; TP-only schools offer regular TP without regular HC. ',
  'Both can offer lower grades. The omitted non-TP category includes HC-only schools, artistic-only schools and schools ',
  'with other or no regular secondary offerings recorded in 2024. Classification uses fixed 2024 offerings without restricting ',
  'the model sample to schools active that year. The variables describe school offerings rather than the applicant\'s eventual track. ',
  'Their coefficients are conditional associations relative to the non-TP category.</p>', html_table(type_coverage_display),
  '<h2>Diagnostics and verification</h2><p>', esc_html(diagnostics_text), '</p>', html_table(pretty_numeric(conditions)),
  '<details><summary>Fit sample sizes and optimizer summaries</summary>', html_table(pretty_numeric(samples)), '</details>',
  '<details><summary>Upstream validation checks</summary>', html_table(verification), '</details>',
  '<p>The submitted-set Plackett-Luce likelihood is evaluated through the exact stratified Cox representation with unique ',
  'ranks. Each student\'s submitted schools form the choice set; the last singleton factor is one. No intercept, school fixed ',
  'effects, peer controls, outside option or unlisted alternatives enter these models.</p>',
  '<h2>Reusable outputs</h2>', download_html,
  '<details><summary>Input source manifest</summary>', html_table(pretty_numeric(manifest)), '</details>',
  '<p class="notes">Report generated from saved results by code/codex/school_preference_va/06_report_groups.R. ',
  'No additional regressions are estimated during reporting. The manuscript is unchanged.</p></main></body></html>')
writeLines(html, file.path(report_dir, "by_priority_continuation.html"), useBytes = TRUE)
fwrite(checks, file.path(group_dir, "report_verification.csv"))
message("Saved subgroup preference report: ", file.path(report_dir, "by_priority_continuation.html"))
