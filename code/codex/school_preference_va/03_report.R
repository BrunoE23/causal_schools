# Render already estimated joint models; no additional regressions are fitted.
source("code/codex/school_preference_va/common.R")
read_clean <- function(name) fread(file.path(clean_dir, name))
coef_dt <- read_clean("coefficients.csv")
slopes <- read_clean("type_specific_va_slopes.csv")
samples <- read_clean("model_summary.csv")
flow <- read_clean("sample_flow.csv")
coverage <- read_clean("va_coverage.csv")
school_types <- read_clean("school_type_coverage.csv")
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
check("Reported specification includes every VA by school-type interaction",
  identical(model_static_terms, c(va_terms, controls, interaction_terms)) && length(va_terms) == 4L &&
    identical(controls, c("mixed_hc_tp", "tp_only")) && length(interaction_terms) == 8L)
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
  regressor = c(unname(model_static_labels[model_static_terms]), "log(1 + distance in km)"))
check("All interaction-table row labels available", !anyNA(regression$regressor))
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
  paste0(bs, "label{tab:joint-rank-ordered-logit-va}"), paste0(bs, "small"),
  paste0(bs, "begin{tabular}{p{0.49", bs, "linewidth}cc}"),
  paste0(bs, "toprule"), paste0(" & Published coordinates & Comuna centers ", bs, bs),
  paste0(bs, "midrule"), tex_rows, paste0(bs, "bottomrule"), paste0(bs, "end{tabular}"),
  paste0(bs, "par", bs, "smallskip"), paste0(bs, "begin{minipage}{0.98", bs, "linewidth}", bs, "footnotesize"),
  paste0(bs, "textit{Notes:} Joint rank-ordered logit conditional on each student's submitted schools. ",
    "Four selected VA measures, mixed HC/TP and TP-only school indicators, all eight VA-by-type interactions, and distance enter simultaneously. Coefficients are followed by ",
    "student-clustered standard errors in parentheses. VA is standardized with equal weight per unique destination ",
    "school in the common pooled estimation sample. The two school-type indicators remain unstandardized 0/1 variables; ",
    "both are relative to omitted non-TP schools. Base VA coefficients are slopes for non-TP schools. ",
    "Each type-specific VA slope adds its type interaction to the base VA coefficient. ",
    "School-type intercept coefficients compare types only when all four standardized VAs equal zero; type premiums otherwise depend on VA. ",
    "Straight-line distance in kilometers enters as $", bs, "log(1+d)$. ",
    "The first column retains all published student locations, including municipality imputations; the second replaces ",
    "the student point with the home-comuna center. Both retain the same school-campus coordinates and complete rankings. ",
    "Inference conditions on the estimated EB indices; VA-estimation uncertainty is not propagated. ",
    "No school fixed effects or unlisted alternatives enter."),
  paste0(bs, "end{minipage}"), paste0(bs, "end{table}"))
writeLines(tex, file.path(table_dir, "joint_rank_ordered_logit.tex"))

# Display exact total slopes, including base-interaction covariance in the SE.
type_order <- c("Non-TP", "Mixed HC/TP", "TP only")
check("Total VA slopes cover three school types", setequal(unique(slopes$school_type), type_order))
check("Total VA slopes cover both distance models", setequal(unique(slopes$model), names(distance_vars)))
check("Exactly twelve total VA slopes per model", nrow(slopes) == 24L &&
  !anyDuplicated(slopes[, .(model, school_type, outcome)]))
check("Total-slope estimates and exact contrast SEs are finite", all(is.finite(slopes$estimate)) &&
  all(is.finite(slopes$std_error) & slopes$std_error > 0))
total_table <- data.table(school_type = rep(type_order, each = nrow(specs)),
  outcome = rep(specs$outcome, times = length(type_order)))
total_table[, label := specs$label[match(outcome, specs$outcome)]]
total_keys <- paste(total_table$school_type, total_table$outcome, sep = "::")
for (name in names(distance_vars)) {
  z <- slopes[model == name]
  z <- z[match(total_keys, paste(school_type, outcome, sep = "::"))]
  check(paste(name, "all requested school-type slopes available"), !anyNA(z$estimate))
  total_table[, (paste0(name, "_estimate")) := z$estimate]
  total_table[, (paste0(name, "_std_error")) := z$std_error]
  total_table[, (name) := coef_cell(z$estimate, z$std_error)]
}
fwrite(total_table, file.path(table_dir, "type_specific_va_slopes.csv"))
total_display <- total_table[, .(school_type, label, main, municipality)]
setnames(total_display, c("School type", "VA dimension", "Published student coordinates", "Home-comuna centers"))
total_tex_rows <- vapply(seq_len(nrow(total_table)), function(i) paste0(
  if (i > 1L && (i - 1L) %% nrow(specs) == 0L) paste0(bs, "midrule\n") else "",
  tex_escape(total_table$school_type[i]), " & ", tex_escape(total_table$label[i]), " & ",
  total_table$main[i], " & ", total_table$municipality[i], " ", bs, bs), character(1))
writeLines(c(paste0(bs, "begin{table}[!htbp]"), paste0(bs, "centering"), paste0(bs, "small"),
  paste0(bs, "caption{School-type-specific total VA slopes in submitted rankings}"),
  paste0(bs, "label{tab:school-type-total-va-slopes}"), paste0(bs, "begin{tabular}{llcc}"),
  paste0(bs, "toprule"), paste0("School type & VA dimension & Published coordinates & Comuna centers ", bs, bs),
  paste0(bs, "midrule"), total_tex_rows, paste0(bs, "bottomrule"), paste0(bs, "end{tabular}"),
  paste0(bs, "par", bs, "smallskip"), paste0(bs, "begin{minipage}{0.98", bs, "linewidth}", bs, "footnotesize"),
  paste0("Each cell reports a total VA slope with its student-clustered standard error. Non-TP slopes equal the ",
    "base VA coefficient. Mixed HC/TP and TP-only slopes equal the base plus the corresponding interaction. ",
    "SEs use the exact covariance of the coefficient contrast. All VA indices use the same pooled school normalization."),
  paste0(bs, "end{minipage}"), paste0(bs, "end{table}")),
  file.path(table_dir, "type_specific_va_slopes.tex"))

# Describe the within-type total slopes rather than interpreting base effects as pooled slopes.
main_slopes <- slopes[model == "main"]
interpretation <- paste(vapply(type_order, function(type_name) {
  x <- main_slopes[school_type == type_name]
  x[, display_label := specs$label[match(outcome, specs$outcome)]]
  positive <- x[estimate > 0][order(-estimate)]
  lead <- if (nrow(positive)) paste0("The largest positive point estimate is ", positive$display_label[1L], " at ",
    f4(positive$estimate[1L]), " (SE ", f4(positive$std_error[1L]), ").") else
    "None of the four total VA slope point estimates is positive."
  negative <- x[estimate < 0, display_label]
  neg <- if (length(negative)) paste0(" Negative total point estimates occur for ", paste(negative, collapse = ", "), ".") else ""
  weak <- x[p_value > .05, display_label]
  uncertain <- if (length(weak)) paste0(" Total slopes not distinguished from zero at 5% are ",
    paste(weak, collapse = ", "), ".") else ""
  paste0(type_name, ": ", lead, neg, uncertain)
}, character(1)), collapse = " ")
negative_text <- paste0("The four base VA coefficients in the full regression table apply only to non-TP schools. ",
  "For mixed HC/TP and TP-only schools, add the matching interaction coefficient to obtain the total slope shown above. ",
  "A positive or negative interaction describes a slope difference from non-TP schools, not the sign of that type's total slope.")
alt_slopes <- slopes[model == "municipality"]
alt_slopes <- alt_slopes[match(paste(main_slopes$school_type, main_slopes$outcome),
  paste(school_type, outcome))]
changed <- paste(main_slopes$school_type, main_slopes$label)[sign(main_slopes$estimate) != sign(alt_slopes$estimate)]
sensitivity <- if (length(changed)) paste0("Using home-comuna centers changes total-slope signs for ",
  paste(changed, collapse = ", "), ".") else
  "Using home-comuna centers leaves the signs of all twelve school-type-specific total VA slopes unchanged."
sensitivity <- paste0(sensitivity, " The largest absolute change in a total VA slope is ",
  f4(max(abs(main_slopes$estimate - alt_slopes$estimate))), ".")
distance_b <- coef_dt[model == "main" & term == distance_vars[["main"]], estimate]
distance_comuna_b <- coef_dt[model == "municipality" & term == distance_vars[["municipality"]], estimate]
distance_text <- paste0("The distance coefficient is ", f4(distance_b), " with published student coordinates and ",
  f4(distance_comuna_b), " with home-comuna centers. In the main model greater distance is associated with ",
  if (distance_b < 0) "lower" else if (distance_b > 0) "higher" else "unchanged",
  " relative ranking utility, conditional on the four VA measures, both school-type indicators and all eight interactions.")
type_text <- paste(vapply(controls, function(term_name) {
  main_type <- coef_dt[model == "main" & term == term_name]
  alternative_type <- coef_dt[model == "municipality" & term == term_name]
  paste0(control_labels[[term_name]], " has coefficient ", f4(main_type$estimate), " (SE ",
    f4(main_type$std_error), ") with published coordinates and ", f4(alternative_type$estimate),
    " with home-comuna centers. At all four standardized VAs equal to zero, the main coefficient corresponds to ",
    f4(exp(main_type$estimate)), " times the conditional relative odds compared with omitted non-TP schools. ",
    "For other VA values the type premium also includes all four type-by-VA interaction products.")
}, character(1)), collapse = " ")

all_flow <- flow[sae_proceso == "All"][order(stage_order)]
stage_n <- function(stage_name) all_flow[stage == stage_name, rankings]
n_all <- stage_n("all_submitted")
n_informative <- stage_n("two_or_more_schools")
n_selected <- stage_n("all_selected_va")
n_final <- stage_n("shared_main_and_comuna_distance")
coverage_text <- paste0("The paper roster contains ", fmt_n(n_all), " submitted student rankings. ",
  fmt_n(n_all - n_informative), " contain only one distinct school and provide no ranking information. ",
  "Requiring all four selected VAs for every submitted school removes ", fmt_n(n_informative - n_selected), " of the ",
  fmt_n(n_informative), " informative lists (", pct((n_informative - n_selected) / n_informative),
  "). The common final sample contains ", fmt_n(n_final), " rankings. Any missing selected VA, required school-type indicator or distance ",
  "excludes the entire list; unavailable alternatives never silently disappear from retained lists.")
renamed <- function(x, labels) { setnames(x, labels); x }
coverage_display <- renamed(coverage[sae_proceso == "All", .(dimension, fmt_n(covered_school_pairs),
  pct(covered_school_pairs / school_pairs), fmt_n(covered_schools), fmt_n(complete_rankings), pct(complete_ranking_share))],
  c("Dimension", "Covered student-school pairs", "Pair coverage", "Covered schools", "Complete submitted lists", "Complete-list coverage"))
flow_display <- renamed(flow[, .(sae_proceso, stage, fmt_n(rankings), fmt_n(school_pairs), fmt_n(unique_schools), pct(share_of_roster))],
  c("Year", "Stage", "Rankings", "Student-school pairs", "Schools", "Roster share"))
normalization_display <- renamed(normalization[, .(label, f4(reference_mean), f4(reference_sd), fmt_n(n_reference_schools))],
  c("Dimension", "Reference mean", "Reference SD", "Reference schools"))
type_reference <- school_types[population == "Pooled estimation" & !school_type %in% c("TP only", "Mixed HC/TP")]
type_reference_text <- paste0("The omitted category contains ", fmt_n(sum(type_reference$schools)), " non-TP schools: ",
  paste0(fmt_n(type_reference$schools), " ", type_reference$school_type, collapse = "; "), ".")
school_type_display <- renamed(school_types[population == "Pooled estimation", .(school_type,
  fmt_n(schools), pct(share_schools), fmt_n(applicant_school_pairs), pct(share_pairs))],
  c("School type", "Schools", "School share", "Student-school pairs", "Pair share"))
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
  '</li><li><a download href="../../tables/', task, '/type_specific_va_slopes.csv">Total VA slopes by school type (CSV)</a>; ',
  '<a download href="../../tables/', task, '/type_specific_va_slopes.tex">Total-slope LaTeX table</a>; ',
  clean_link("type_specific_va_slopes.csv", "Exact slope contrasts and confidence intervals (CSV)"),
  "</li><li>", clean_link("school_type_classification.csv", "School-type classification (CSV)"), "; ",
  clean_link("school_type_coverage.csv", "School-type coverage (CSV)"),
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
  'Empirical Bayes VA measures, mixed HC/TP and TP-only indicators, all eight VA-by-type interactions, and distance enter together.</p>',
  '<h2>Full joint regression</h2><div class="regression">',
  html_table(regression_display), '</div><p class="notes">Coefficients with student-clustered standard errors in parentheses, ',
  'shown to four decimals. Both columns use identical students, schools and VA normalization. The main model retains ',
  'every published student-coordinate quality code, including municipality imputations. Both distances enter as log(1 + kilometers).</p>',
  '<h2>Total VA slopes within school type</h2>', html_table(total_display),
  '<p class="notes">These twelve slopes come from the same joint model, not separate school-type regressions. ',
  'Non-TP slopes are the base coefficients. Each other total slope adds the corresponding interaction. ',
  'Student-clustered SEs use the exact contrast covariance, including base-interaction covariance.</p>',
  '<h2>Interpretation</h2><p>', esc_html(interpretation), '</p><p>', esc_html(negative_text), ' ',
  esc_html(distance_text), '</p><p>', esc_html(sensitivity), '</p><p>', esc_html(type_text), '</p>',
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
  '<p>The two school-type indicators split Tecnico Profesional offerings into mixed HC/TP schools and TP-only schools. ',
  'They are mutually exclusive 0/1 school indicators, not standardized VA measures. Both coefficients are relative to ',
  'schools with neither indicator, the omitted non-TP category; this reference is not described as exclusively HC. ',
  'Every VA index is interacted with both type indicators using its standardized value. Base VA coefficients apply ',
  'to the non-TP reference type. Type intercepts apply at all four standardized VAs equal to zero, the pooled reference ',
  'means; they are not constant type premiums when the interaction terms are nonzero. ',
  'Mixed means both regular HC teaching (ENS 310) and regular TP teaching (ENS 410, 510, 610, 710 or 810). TP-only means ',
  'at least one of those TP codes without ENS 310; these schools can still offer lower grades. Classification uses the ',
  'fixed 2024 school directory and describes school offerings rather than individual student tracks. No 2024 operating-status ',
  'filter is imposed on the historical application sample.</p><p>', esc_html(type_reference_text), '</p>',
  html_table(school_type_display),
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
  'outside option or unlisted alternative. The four VA measures, two school-type indicators, all eight VA-by-type interactions ',
  'and distance are the complete specification. ',
  'The only sensitivity fit replaces student points with home-comuna centers.</p>',
  '<h2>Reusable outputs</h2>', downloads,
  '<p class="notes">Run scripts in code/codex/school_preference_va from the repository root in this order: ',
  '01_build_rankings.R, 02_fit_rank_logit.R, 04_verify.R, 03_report.R. Original source data and the manuscript are unchanged.</p>',
  '</main></body></html>')
writeLines(html, file.path(report_dir, "joint_rank_ordered_logit.html"), useBytes = TRUE)
message("Saved joint regression table and report: ", file.path(report_dir, "joint_rank_ordered_logit.html"))
