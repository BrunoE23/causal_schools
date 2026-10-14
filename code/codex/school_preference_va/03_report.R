# Render already estimated joint models; no additional regressions are fitted.
source("code/codex/school_preference_va/common.R")
read_clean <- function(name) fread(file.path(clean_dir, name))
coef_dt <- read_clean("coefficients.csv")
samples <- read_clean("model_summary.csv")
flow <- read_clean("sample_flow.csv")
coverage <- read_clean("va_coverage.csv")
tp_coverage <- read_clean("tp_coverage.csv")
normalization <- read_clean("va_normalization.csv")
geo <- read_clean("geography_coverage.csv")
quality <- read_clean("geography_quality.csv")
correlations <- read_clean("va_correlations.csv")
vif <- read_clean("va_vif.csv")
conditions <- read_clean("model_diagnostics.csv")
comparison <- read_clean("validation_comparison.csv")
validation_conditions <- read_clean("validation_condition_diagnostics.csv")
audit <- read_clean("ranking_audit.csv")
geo_notes <- readLines(file.path(clean_dir, "geography_notes.txt"), warn = FALSE)

n_coefficients <- length(model_static_terms) + 1L
check("Report contains exactly two joint models", setequal(unique(coef_dt$model), names(distance_vars)) &&
  nrow(coef_dt) == length(distance_vars) * n_coefficients)
for (name in names(distance_vars)) {
  check(paste(name, "report has all requested coefficients"),
    setequal(coef_dt[model == name, term], c(model_static_terms, distance_vars[[name]])))
  check(paste(name, "report has one sample summary"), nrow(samples[model == name]) == 1L)
}
check("Reported standard errors finite and positive", all(is.finite(coef_dt$std_error) & coef_dt$std_error > 0))
check("Both reported models have identical sample sizes", all(vapply(
  c("applicants", "rankings", "applicant_school_pairs", "schools", "informative_rank_decisions"),
  function(v) uniqueN(samples[[v]]) == 1L, logical(1))))

f4 <- function(x) formatC(x, digits = 4L, format = "f", big.mark = ",")
pct <- function(x) paste0(formatC(100 * x, digits = 2L, format = "f"), "%")
coef_cell <- function(b, se) paste0(f4(b), " (", f4(se), ")")
regression <- data.table(row_type = "coefficient",
  regressor = c(specs$label, unname(control_labels[controls]), "log(1 + distance in km)"))
for (name in names(distance_vars)) {
  terms <- c(model_static_terms, distance_vars[[name]])
  x <- coef_dt[model == name][match(terms, term)]
  regression[, (paste0(name, "_estimate")) := x$estimate]
  regression[, (paste0(name, "_std_error")) := x$std_error]
  regression[, (name) := coef_cell(x$estimate, x$std_error)]
}
statistics <- data.table(row_type = "sample_statistic",
  regressor = c("Student rankings", "Student-school pairs", "Destination schools", "Log likelihood"))
for (name in names(distance_vars)) {
  s <- samples[model == name]
  values <- vapply(c("rankings", "applicant_school_pairs", "schools", "log_likelihood"),
    function(v) as.numeric(s[[v]]), numeric(1))
  statistics[, (paste0(name, "_estimate")) := values]
  statistics[, (paste0(name, "_std_error")) := NA_real_]
  statistics[, (name) := c(fmt_n(values[1:3]), f4(values[4]))]
}
regression <- rbindlist(list(regression, statistics), use.names = TRUE)
fwrite(regression, file.path(table_dir, "joint_rank_ordered_logit.csv"))

# A table fragment for the existing manuscript toolchain, not a standalone file.
bs <- intToUtf8(92L)
tex_escape <- function(x) {
  for (z in c("&", "%", "_", "#")) x <- gsub(z, paste0(bs, z), x, fixed = TRUE)
  x
}
tex_rows <- vapply(seq_len(nrow(regression)), function(i) paste0(
  if (i == n_coefficients + 1L) paste0(bs, "midrule\n") else "", tex_escape(regression$regressor[i]), " & ",
  regression$main[i], " & ", regression$municipality[i], " ", bs, bs), character(1))
tex <- c(paste0(bs, "begin{table}[!htbp]"), paste0(bs, "centering"),
  paste0(bs, "caption{Submitted school rankings and multidimensional value added}"),
  paste0(bs, "label{tab:joint-rank-ordered-logit-va}"), paste0(bs, "begin{tabular}{lcc}"),
  paste0(bs, "toprule"), paste0(" & Published coordinates & Comuna centers ", bs, bs),
  paste0(bs, "midrule"), tex_rows, paste0(bs, "bottomrule"), paste0(bs, "end{tabular}"),
  paste0(bs, "par", bs, "smallskip"), paste0(bs, "begin{minipage}{0.98", bs, "linewidth}", bs, "footnotesize"),
  paste0(bs, "textit{Notes:} Joint rank-ordered logit conditional on each student's submitted schools. ",
    "Four selected VA measures, a Tecnico Profesional (TP) school indicator, and distance enter simultaneously. Coefficients are followed by ",
    "student-clustered standard errors in parentheses. VA is standardized with equal weight per unique destination ",
    "school in the common pooled estimation sample. TP is a raw 0/1 school indicator, not standardized. ",
    "It uses the paper's 2024 directory classification, includes mixed schools offering TP, and excludes artistic-only schools. ",
    "Straight-line distance in kilometers enters as $", bs, "log(1+d)$. ",
    "The first column retains all published student locations, including municipality imputations; the second replaces ",
    "the student point with the home-comuna center. Both retain the same school-campus coordinates and complete rankings. ",
    "Inference conditions on the estimated EB indices; VA-estimation uncertainty is not propagated. ",
    "No school fixed effects or unlisted alternatives enter."),
  paste0(bs, "end{minipage}"), paste0(bs, "end{table}"))
writeLines(tex, file.path(table_dir, "joint_rank_ordered_logit.tex"))

# Interpret normalized joint coefficients; no extra model or preference claim.
main <- coef_dt[model == "main" & term %in% va_terms]
main[, label := specs$label[match(term, specs$standardized)]]
positive <- main[estimate > 0][order(-estimate)]
interpretation <- if (nrow(positive)) {
  top <- positive[1L]
  paste0("The largest positive conditional VA coefficient is for ", top$label, ": ",
    f4(top$estimate), " (SE ", f4(top$std_error), "). A one-school-SD difference corresponds to ",
    f4(exp(top$estimate)), " times the relative odds of selecting that school next among the schools ",
    "remaining in a submitted ranking, holding other VA dimensions, school TP status and distance fixed.")
} else "None of the selected VA dimensions has a positive conditional point estimate in the main model."
other_positive <- positive[-1L]
if (nrow(other_positive)) interpretation <- paste0(interpretation,
  " Other positive point estimates are ", paste0(other_positive$label, " (", f4(other_positive$estimate), ")", collapse = ", "), ".")
weak <- main[p_value > .05, label]
if (length(weak)) interpretation <- paste0(interpretation, " The ", paste(weak, collapse = ", "),
  " coefficient is not statistically distinguished from zero at the 5% level.")
negative <- main[estimate < 0, label]
negative_text <- if (length(negative)) paste0("Negative conditional point estimates occur for ",
  paste(negative, collapse = ", "), ". Their uncertainty is given by the student-clustered standard errors.") else
  "All selected conditional VA point estimates are nonnegative."
near_cutoff <- main[estimate < 0 & p_value >= .04 & p_value <= .06]
if (nrow(near_cutoff)) negative_text <- paste0(negative_text, " The ",
  paste0(near_cutoff$label, " result (p = ", f4(near_cutoff$p_value), ")", collapse = ", "),
  " is close to the 5% significance cutoff.")
alt <- coef_dt[model == "municipality" & term %in% va_terms][match(main$term, term)]
changed <- main$label[sign(main$estimate) != sign(alt$estimate)]
sensitivity <- if (length(changed)) paste0("Using home-comuna centers changes coefficient signs for ",
  paste(changed, collapse = ", "), ".") else
  "Using home-comuna centers leaves all four VA coefficient signs unchanged."
sensitivity <- paste0(sensitivity, " The largest absolute change in a standardized VA coefficient is ",
  f4(max(abs(main$estimate - alt$estimate))), ".")
distance_b <- coef_dt[model == "main" & term == distance_vars[["main"]], estimate]
distance_comuna_b <- coef_dt[model == "municipality" & term == distance_vars[["municipality"]], estimate]
distance_text <- paste0("The distance coefficient is ", f4(distance_b), " with published student coordinates and ",
  f4(distance_comuna_b), " with home-comuna centers. In the main model greater distance is associated with ",
  if (distance_b < 0) "lower" else if (distance_b > 0) "higher" else "unchanged",
  " relative ranking utility, conditional on the four VA measures and TP status.")
tp_main <- coef_dt[model == "main" & term == "tp_school"]
tp_alternative <- coef_dt[model == "municipality" & term == "tp_school"]
tp_text <- paste0("The TP-school coefficient is ", f4(tp_main$estimate), " (SE ", f4(tp_main$std_error),
  ") in the main model and ", f4(tp_alternative$estimate), " with home-comuna centers. ",
  "The main estimate corresponds to ", f4(exp(tp_main$estimate)),
  " times the conditional relative odds for a TP school versus a school without the TP flag, ",
  "holding the four VA measures and distance fixed. This school-type comparison is an association, not a causal TP effect.")

all_flow <- flow[sae_proceso == "All"][order(stage_order)]
stage_n <- function(stage_name) all_flow[stage == stage_name, rankings]
n_all <- stage_n("all_submitted")
n_informative <- stage_n("two_or_more_schools")
n_selected <- stage_n("all_selected_va")
n_tp <- stage_n("all_selected_va_and_tp")
n_final <- stage_n("shared_main_and_comuna_distance")
coverage_text <- paste0("The paper roster contains ", fmt_n(n_all), " submitted student rankings. ",
  fmt_n(n_all - n_informative), " contain only one distinct school and provide no ranking information. ",
  "Requiring all four selected VAs for every submitted school removes ", fmt_n(n_informative - n_selected), " of the ",
  fmt_n(n_informative), " informative lists (", pct((n_informative - n_selected) / n_informative),
  "). Requiring a known TP classification removes ", fmt_n(n_selected - n_tp), " additional informative lists. ",
  "The common final sample contains ", fmt_n(n_final), " rankings. Any missing selected VA, TP classification or required distance ",
  "excludes the entire list; unavailable alternatives never silently disappear from retained lists.")
renamed <- function(x, labels) { setnames(x, labels); x }
coverage_display <- renamed(coverage[sae_proceso == "All", .(dimension, fmt_n(covered_school_pairs),
  pct(covered_school_pairs / school_pairs), fmt_n(covered_schools), fmt_n(complete_rankings), pct(complete_ranking_share))],
  c("Dimension", "Covered student-school pairs", "Pair coverage", "Covered schools", "Complete submitted lists", "Complete-list coverage"))
flow_display <- renamed(flow[, .(sae_proceso, stage, fmt_n(rankings), fmt_n(school_pairs), fmt_n(unique_schools), pct(share_of_roster))],
  c("Year", "Stage", "Rankings", "Student-school pairs", "Schools", "Roster share"))
normalization_display <- renamed(normalization[, .(label, f4(reference_mean), f4(reference_sd), fmt_n(n_reference_schools))],
  c("Dimension", "Reference mean", "Reference SD", "Reference schools"))
tp_display <- renamed(tp_coverage[sae_proceso == "All", .(population, fmt_n(rankings), pct(complete_tp_ranking_share),
  fmt_n(classified_school_pairs), fmt_n(missing_school_pairs), fmt_n(tp_school_pairs),
  fmt_n(classified_schools), fmt_n(tp_schools))], c("Population", "Rankings", "Complete TP lists",
  "Classified student-school pairs", "Unclassified pairs", "TP pairs", "Classified schools", "TP schools"))
tp_final <- tp_coverage[sae_proceso == "All" & population == "Final estimation rankings"]
tp_sample_text <- paste0("In the final estimation sample, ", fmt_n(tp_final$tp_schools), " of ",
  fmt_n(tp_final$unique_schools), " destination schools (", pct(tp_final$tp_schools / tp_final$unique_schools),
  ") offer TP. They account for ", fmt_n(tp_final$tp_school_pairs), " of ", fmt_n(tp_final$school_pairs),
  " submitted student-school pairs (", pct(tp_final$tp_school_pairs / tp_final$school_pairs), ").")
geo_display <- renamed(geo[, .(sae_proceso, fmt_n(program_rows), fmt_n(applicant_years),
  pct(student_coordinates_valid / program_rows), pct(school_coordinates_valid / program_rows),
  pct(home_comuna_centers_matched / program_rows), f4(main_distance_median_km), f4(main_distance_p99_km), f4(main_distance_max_km))],
  c("Year", "Program choices", "Applicants", "Student-coordinate coverage", "School-coordinate coverage",
    "Comuna-center coverage", "Main distance median (km)", "Main distance p99 (km)", "Main distance maximum (km)"))
quality_display <- renamed(quality[, .(sae_proceso, georef_quality, quality_label, fmt_n(applicant_years), pct(share))],
  c("Year", "Quality", "Definition", "Applicants", "Share"))
imputed <- quality[sae_proceso == "Pooled" & georef_quality == 4L]
geography_text <- paste0("Published student and contemporaneous school-program coordinates cover the submitted lists. ",
  "Student locations are privacy-displaced and sometimes imputed at municipality level: ", fmt_n(imputed$applicant_years),
  " applicants (", pct(imputed$share), ") have that quality code and remain in the main model. The second column ",
  "substitutes a home-comuna center for every student, retaining the same actual school-campus point. ",
  "No external geocoding is used. Coverage below counts program choices before repeated-school aggregation.")

pretty_numeric <- function(x) {
  x <- copy(x)
  for (v in names(x)) if (is.numeric(x[[v]])) {
    a <- x[[v]]
    x[, (v) := if (all(is.na(a) | a == round(a))) fmt_n(a) else f4(a)]
  }
  x
}
comparison_display <- pretty_numeric(comparison)
for (v in grep("difference$", names(comparison), value = TRUE)) {
  set(comparison_display, j = v, value = formatC(comparison[[v]], digits = 3L, format = "e"))
}
upstream_checks <- rbindlist(lapply(c("data_verification.csv", "estimation_verification.csv", "verification.csv"), function(name) {
  x <- read_clean(name); x[, source := name]; x
}), use.names = TRUE, fill = TRUE)
check("All upstream data and model checks passed", all(upstream_checks$passed))
save_checks("report_verification.csv")
all_checks <- rbindlist(list(upstream_checks, copy(checks)[, source := "report_verification.csv"]), use.names = TRUE, fill = TRUE)
setcolorder(all_checks, c("source", "check", "passed", "detail"))
regression_display <- renamed(regression[, .(regressor, main, municipality)],
  c("Regressor", "Published student coordinates", "Home-comuna centers"))
cor_display <- copy(correlations)
cor_display[, term := specs$label[match(term, specs$standardized)]]
for (v in intersect(names(cor_display), va_terms)) {
  set(cor_display, j = v, value = f4(cor_display[[v]]))
  setnames(cor_display, v, specs$label[match(v, specs$standardized)])
}
vif_display <- copy(vif)
vif_display[, term := specs$label[match(term, specs$standardized)]]
clean_link <- function(file, label) paste0('<a download href="../../../data/clean/', task, '/', file, '">', esc_html(label), '</a>')
downloads <- paste0("<ul><li>", clean_link("student_school_rankings.rds", "Full student-school rankings (RDS)"),
  "</li><li>", clean_link("program_rankings.rds", "Original program rankings (RDS)"),
  "</li><li>", clean_link("estimation_data.rds", "Common estimation dataset (RDS)"),
  "</li><li>", clean_link("ranking_roster.csv", "Ranking roster and inclusion flags (CSV)"),
  "</li><li>", clean_link("models/main.rds", "Main fitted model (RDS)"), "; ",
  clean_link("models/municipality.rds", "Comuna-distance fitted model (RDS)"),
  "</li><li>", clean_link("coefficients.csv", "Numeric coefficients and confidence intervals (CSV)"),
  '</li><li><a download href="../../tables/', task, '/joint_rank_ordered_logit.csv">Regression table (CSV)</a>; ',
  '<a download href="../../tables/', task, '/joint_rank_ordered_logit.tex">LaTeX table fragment</a>',
  "</li><li>", clean_link("source_manifest.csv", "Input source manifest (CSV)"), "</li></ul>")

html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1"><title>School rankings and value added</title>',
  '<style>body{font:16px/1.55 system-ui,sans-serif;color:#172536;background:#f6f8fa;margin:0}',
  'main{max-width:1240px;margin:30px auto;padding:30px;background:white;border:1px solid #dce3e9;border-radius:10px}',
  'h1{font-size:30px;line-height:1.2}h2{font-size:23px;margin-top:36px}p{max-width:1100px}.muted{color:#556373}',
  '.table{overflow:auto;margin:18px 0}table{border-collapse:collapse;width:100%;font-size:14px}',
  'th,td{padding:10px 12px;text-align:right;border-bottom:1px solid #e3e8ed}th:first-child,td:first-child{text-align:left}',
  'thead th{background:#edf2f6;white-space:nowrap}a{color:#175d99}li{margin:6px 0}details{margin:18px 0}',
  'summary{cursor:pointer;font-weight:600}.notes{font-size:14px;color:#465465}',
  '.regression tbody tr:nth-child(', n_coefficients + 1L, ') td{border-top:2px solid #7a8794}</style></head><body><main>',
  '<h1>Do families rank more effective schools higher?</h1>',
  '<p class="muted">Joint rank-ordered logit of submitted SAE school rankings, 2018-2020. Four existing ',
  'Empirical Bayes VA measures, a TP school indicator and distance enter together.</p><div class="regression">',
  html_table(regression_display), '</div><p class="notes">Coefficients with student-clustered standard errors in parentheses, ',
  'shown to four decimals. Both columns use identical students, schools and VA normalization. The main model retains ',
  'every published student-coordinate quality code, including municipality imputations. Both distances enter as log(1 + kilometers).</p>',
  '<h2>Interpretation</h2><p>', esc_html(interpretation), '</p><p>', esc_html(negative_text), ' ',
  esc_html(distance_text), '</p><p>', esc_html(sensitivity), '</p><p>', esc_html(tp_text), '</p>',
  '<p>These are conditional associations with submitted ordering. They do not establish that families observe or causally ',
  'respond to VA; unmeasured school attributes may account for the associations. The model does not explain which schools enter a portfolio.</p>',
  '<h2>Sample and missing data</h2><p>', esc_html(coverage_text), '</p>', html_table(coverage_display),
  '<details><summary>Sample flow by application year</summary>', html_table(flow_display), '</details>',
  '<details><summary>Submitted program and school aggregation audit</summary>', html_table(pretty_numeric(audit)), '</details>',
  '<p>The existing paper roster covers timely grade-9 applicants with and without assignment risk. Auto-appended continuity ',
  'choices are excluded; genuine priority applications remain. Each RBD is represented by its first submitted program and ',
  'that program campus. Distinct-school ranks are renumbered. Original programs and repeated/interleaved-school flags remain available.</p>',
  '<h2>VA normalization and outcome definitions</h2><p>Each pooled 2017-2020 EB index is centered and divided by its sample SD ',
  '(N-1 denominator) across unique destination schools in the final common sample. Each school has equal normalization weight, ',
  'independently of application frequency or list length. Both models and every application year use the same indices and moments. ',
  'No VA is re-estimated or imputed.</p>', html_table(normalization_display),
  '<p>The TP regressor is the paper\'s 2024 directory-based school indicator. It includes mixed schools offering TP ',
  'and excludes artistic-only schools. It describes the school\'s offering, not the individual student\'s curricular track. ',
  'Its values remain zero and one; only the four VA indices are standardized.</p>',
  '<p>', esc_html(tp_sample_text), '</p>', html_table(tp_display),
  '<p>The existing source populations differ: math VA uses the observed-score sample; high-premium-institution and ',
  'high-premium-field VAs use exam takers; exam-taking VA uses the broad complete-control sample. These definitions ',
  'are preserved. Raw math VA uses within-exam-year outcome SDs; the three binary-outcome VAs use probability units. ',
  'Inference treats EB estimates as fixed and does not propagate VA-estimation uncertainty.</p>',
  '<h2>Geographic coverage</h2><p>', esc_html(geography_text), '</p>', html_table(geo_display),
  '<details><summary>Published georeferencing quality by year</summary>', html_table(quality_display), '</details>',
  '<details><summary>Geography sources, privacy displacement and limitations</summary><ul>',
  paste0('<li>', esc_html(geo_notes), '</li>', collapse = ''), '</ul></details>',
  '<h2>Collinearity and numerical diagnostics</h2><p>Correlations below give each destination school equal weight. ',
  'VA variance-inflation factors use the four-dimensional school correlation matrix. Identification and information checks ',
  'also use the within-ranking likelihood design.</p>', html_table(cor_display), html_table(pretty_numeric(vif_display)),
  html_table(pretty_numeric(conditions)),
  '<details><summary>Independent conditional-logit validation</summary>', html_table(comparison_display),
  html_table(pretty_numeric(validation_conditions)), '</details>',
  '<details><summary>All ', fmt_n(nrow(all_checks)), ' data and model checks passed</summary>', html_table(all_checks), '</details>',
  '<h2>Likelihood and inference</h2><p>The Plackett-Luce likelihood multiplies each ranked-school choice probability ',
  'over schools remaining in that submitted list. The final singleton factor is one. A stratified Cox representation with ',
  'unique event ranks evaluates this exact likelihood. Expanded conditional logit independently verifies coefficients, ',
  'likelihood and student-cluster covariance on complete validation rankings. There is no intercept, school fixed effect, ',
  'outside option or unlisted alternative. The four VA measures, TP indicator and distance are the complete specification. ',
  'The only sensitivity fit replaces student points with home-comuna centers.</p>',
  '<h2>Reusable outputs</h2>', downloads,
  '<p class="notes">Run scripts in code/codex/school_preference_va from the repository root in this order: ',
  '01_build_rankings.R, 02_fit_rank_logit.R, 04_verify.R, 03_report.R. Original source data and the manuscript are unchanged.</p>',
  '</main></body></html>')
writeLines(html, file.path(report_dir, "joint_rank_ordered_logit.html"), useBytes = TRUE)
message("Saved joint regression table and report: ", file.path(report_dir, "joint_rank_ordered_logit.html"))
