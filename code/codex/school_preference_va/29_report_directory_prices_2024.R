# Report saved rank-logit estimates with 2024 Directorio fee bands; no model fitting here.
source("code/codex/school_preference_va/directory_prices_common.R")

read_clean <- function(name) fread(file.path(clean_dir, name))
co <- read_clean("coefficients.csv")
sl <- read_clean("type_specific_slopes.csv")
ms <- read_clean("model_summary.csv")
flow <- read_clean("sample_flow.csv")
bands <- read_clean("fee_band_counts.csv")
support <- read_clean("fee_support.csv")
diagnostics <- read_clean("model_diagnostics.csv")
joint_tests <- read_clean("fee_joint_tests.csv")
build_checks <- read_clean("build_verification.csv")
fit_checks <- read_clean("fit_verification.csv")
independent_checks <- read_clean("verification.csv")

check("Twenty unique saved model results", nrow(ms) == 20L &&
  !anyDuplicated(ms[, .(group, variant, model)]))
check("Saved build, fit and independent verification checks pass", all(build_checks$passed) &&
  all(fit_checks$passed) && all(independent_checks$passed))
check("Coefficient and total-slope keys are unique", !anyDuplicated(co[, .(group, variant, model, term)]) &&
  !anyDuplicated(sl[, .(group, variant, model, school_type, term)]))
check("Saved estimates and clustered standard errors are finite", all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0) && all(is.finite(sl$estimate)) &&
  all(is.finite(sl$std_error) & sl$std_error > 0))
check("Displayed coefficient and total-slope intervals use the saved clustered standard errors",
  max(abs(co$lower95 - (co$estimate - qnorm(.975) * co$std_error))) < 1e-8 &&
  max(abs(co$upper95 - (co$estimate + qnorm(.975) * co$std_error))) < 1e-8 &&
  max(abs(sl$lower95 - (sl$estimate - qnorm(.975) * sl$std_error))) < 1e-8 &&
  max(abs(sl$upper95 - (sl$estimate + qnorm(.975) * sl$std_error))) < 1e-8 &&
  max(abs(co$conditional_odds_ratio - exp(co$estimate))) < 1e-8)
check("Ten paid bands and one shared unknown-fee indicator have distinct terms", nrow(fee_specs) == 11L &&
  !anyDuplicated(fee_specs$term) && !any(fee_specs$code == 1L) &&
  fee_specs[code == 0L, identical(term, "fee_information_unknown")])
check("Joint robust fee tests cover every group and distance", nrow(joint_tests) == 10L &&
  !anyDuplicated(joint_tests[, .(group, model)]) && all(is.finite(joint_tests$statistic)) &&
  all(is.finite(joint_tests$p_value)) && all(joint_tests$df > 0L))

f4 <- function(x) ifelse(is.finite(x), formatC(x, digits = 4L, format = "f", big.mark = ","), "")
f2 <- function(x) ifelse(is.finite(x), formatC(x, digits = 2L, format = "f", big.mark = ","), "")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
pct <- function(x) paste0(formatC(100 * x, digits = 2L, format = "f"), "%")
cell <- function(b, s) ifelse(is.finite(b) & is.finite(s), paste0(f4(b), " (", f4(s), ")"), "Not estimable")
ci_cell <- function(lo, hi) ifelse(is.finite(lo) & is.finite(hi), paste0("[", f4(lo), ", ", f4(hi), "]"), "")
p_cell <- function(x) ifelse(!is.finite(x), "", ifelse(x < .001, "<0.001", formatC(x, digits = 3L, format = "f")))
field_labels <- c(PAGO_MATRICULA = "Enrollment fee", PAGO_MENSUAL = "Monthly fee", BOTH = "Both fee fields")
field_label <- function(x) {
  result <- unname(field_labels[x])
  result[is.na(result)] <- x[is.na(result)]
  result
}

# Export every model term on a common grid. An unsupported fee band stays visible.
all_terms <- unique(c(metric_terms, controls, metric_interactions$term,
  unname(distance_vars), fee_specs$term))
term_labels <- c(metric_labels, setNames(paste0(field_label(fee_specs$field), ": ", fee_specs$label), fee_specs$term),
  setNames(paste0("log(1 + distance km): ", unname(model_labels)), unname(distance_vars)))
grid <- rbindlist(lapply(names(group_labels), function(gr) rbindlist(lapply(names(distance_vars), function(md) {
  terms <- all_terms[!all_terms %in% unname(distance_vars[setdiff(names(distance_vars), md)])]
  data.table(group = gr, model = md, term = terms)
}))))
baseline <- co[variant == "matched_baseline", .(group, model, term,
  baseline_estimate = estimate, baseline_std_error = std_error, baseline_p_value = p_value,
  baseline_lower95 = lower95, baseline_upper95 = upper95)]
extended <- co[variant == "extended", .(group, model, term,
  extended_estimate = estimate, extended_std_error = std_error, extended_p_value = p_value,
  extended_lower95 = lower95, extended_upper95 = upper95,
  extended_conditional_odds_ratio = conditional_odds_ratio)]
regression <- merge(grid, baseline, by = c("group", "model", "term"), all.x = TRUE, sort = FALSE)
regression <- merge(regression, extended, by = c("group", "model", "term"), all.x = TRUE, sort = FALSE)
regression[, `:=`(group_label = unname(group_labels[group]), attribute = unname(term_labels[term]),
  baseline_status = fifelse(term %in% fee_specs$term, "Not included", fifelse(is.finite(baseline_estimate), "Estimated", "Not estimable")),
  extended_status = fifelse(is.finite(extended_estimate), "Estimated", "Not estimable in this sample"))]
regression[is.na(attribute), attribute := term]
setorder(regression, group, model)
fwrite(regression, file.path(table_dir, "joint_regression_before_after_fees.csv"))
check("Regression table covers every saved estimate exactly once", nrow(regression[is.finite(baseline_estimate)]) ==
  nrow(baseline) && nrow(regression[is.finite(extended_estimate)]) == nrow(extended))

# Total attribute slopes include both relevant school-type interactions and their covariance.
before <- sl[variant == "matched_baseline", .(group, model, school_type, term, outcome, label,
  baseline_estimate = estimate, baseline_std_error = std_error, baseline_p_value = p_value,
  baseline_lower95 = lower95, baseline_upper95 = upper95)]
after <- sl[variant == "extended", .(group, model, school_type, term,
  extended_estimate = estimate, extended_std_error = std_error, extended_p_value = p_value,
  extended_lower95 = lower95, extended_upper95 = upper95)]
slope_changes <- merge(before, after, by = c("group", "model", "school_type", "term"), all = TRUE, sort = FALSE)
slope_changes[, `:=`(change = extended_estimate - baseline_estimate, group_label = unname(group_labels[group]))]
check("All seven attribute slopes are matched across specifications", nrow(slope_changes) == 5L * 2L * 3L * 7L &&
  all(is.finite(slope_changes$baseline_estimate)) && all(is.finite(slope_changes$extended_estimate)))
fwrite(slope_changes, file.path(table_dir, "attribute_slopes_before_after_fees.csv"))

# Join price estimates to the full band catalog rather than dropping absent categories.
fees <- rbindlist(lapply(names(group_labels), function(gr) rbindlist(lapply(names(distance_vars), function(md) {
  out <- copy(fee_specs)
  out[, `:=`(group = gr, model = md)]
  out
}))))
fees <- merge(fees, co[variant == "extended", .(group, model, term, estimate, std_error, p_value,
  lower95, upper95, conditional_odds_ratio)], by = c("group", "model", "term"), all.x = TRUE, sort = FALSE)
fees[, `:=`(group_label = unname(group_labels[group]), field_label = field_label(field),
  estimation_status = fifelse(is.finite(estimate), "Estimated", "Not estimable in this sample"),
  odds_ratio_lower95 = exp(lower95), odds_ratio_upper95 = exp(upper95))]
setorderv(fees, c("group", "model", "field", "code"))
fees <- merge(fees, support[, .(group, term, identification_status = status)],
  by = c("group", "term"), all.x = TRUE, sort = FALSE)
check("All fee bands remain in every group and distance table", nrow(fees) == 5L * 2L * nrow(fee_specs) &&
  !anyDuplicated(fees[, .(group, model, term)]))
fwrite(fees, file.path(table_dir, "fee_coefficients_by_group.csv"))
fwrite(fees[group == "pooled"], file.path(table_dir, "pooled_fee_coefficients.csv"))

sample_comparison <- merge(ms[variant == "matched_baseline", .(group, model,
  baseline_applicants = applicants, baseline_pairs = applicant_school_pairs,
  baseline_log_likelihood = log_likelihood)], ms[variant == "extended", .(group, model,
  applicants, rankings, applicant_school_pairs, schools, informative_rank_decisions,
  log_likelihood, null_log_likelihood, iterations, seconds)], by = c("group", "model"))
sample_comparison[, log_likelihood_gain := log_likelihood - baseline_log_likelihood]
check("Before/after models use identical applicants and pairs", all(sample_comparison$applicants ==
  sample_comparison$baseline_applicants) && all(sample_comparison$applicant_school_pairs == sample_comparison$baseline_pairs))
check("Adding fee bands weakly improves the fitted likelihood", all(sample_comparison$log_likelihood_gain >= -1e-6))
fwrite(sample_comparison, file.path(table_dir, "sample_and_fit_comparison.csv"))

flow[, retained_share := price_model_applicants / income_model_applicants]
check("Sample-flow totals agree with saved pooled and subgroup fits", nrow(flow) == 5L &&
  all(flow$price_model_applicants[match(ms[variant == "extended" & model == "main", group], flow$group)] ==
    ms[variant == "extended" & model == "main", applicants]))
flow_display <- flow[, .(Group = label, `Previous income-model applicants` = fmt_n(income_model_applicants),
  `Whole lists excluded` = fmt_n(excluded_missing_directory), `Price-model applicants` = fmt_n(price_model_applicants),
  `Share retained` = pct(retained_share))]
fwrite(flow, file.path(table_dir, "sample_retention.csv"))

bands[, `:=`(field_label = field_label(field), share_schools = n_schools / sum(n_schools)), by = field]
band_display <- bands[, .(Fee = field_label, Band = label, `Destination schools` = fmt_n(n_schools),
  `Applicant-school pairs` = fmt_n(n_pairs), `Rankings with this band` = fmt_n(n_rankings))]
fwrite(bands, file.path(table_dir, "fee_category_coverage.csv"))
fwrite(support, file.path(table_dir, "fee_identification_support.csv"))
fwrite(diagnostics, file.path(table_dir, "fit_diagnostics.csv"))
fwrite(joint_tests, file.path(table_dir, "fee_joint_tests.csv"))

table_html <- function(x) {
  heads <- paste0("<th>", esc_html(names(x)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(x)), function(i) paste0("<tr>", paste0("<td>",
    vapply(x[i], function(z) esc_html(as.character(z)), character(1)), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<div class='table-wrap'><table><thead><tr>", heads, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}
fee_display <- function(gr, md) {
  fees[group == gr & model == md, .(Fee = field_label, Band = label,
    `Utility coefficient (SE)` = cell(estimate, std_error), `95% CI` = ci_cell(lower95, upper95),
    `Conditional odds ratio` = f4(conditional_odds_ratio),
    `Odds-ratio 95% CI` = ci_cell(odds_ratio_lower95, odds_ratio_upper95), `p value` = p_cell(p_value),
    Support = identification_status)]
}
slope_display <- function(gr, md) {
  x <- copy(slope_changes[group == gr & model == md])
  x[, `:=`(type_order = match(school_type, c("Non-TP", "Mixed HC/TP", "TP only")),
    metric_order = match(term, metric_specs$standardized))]
  setorder(x, type_order, metric_order)
  x[, .(`School type` = school_type, Attribute = label,
    `Before adding fees` = cell(baseline_estimate, baseline_std_error),
    `With fee bands` = cell(extended_estimate, extended_std_error), `Coefficient change` = f4(change))]
}
regression_display <- function(gr, md) {
  x <- copy(regression[group == gr & model == md])
  x[, sort_order := match(term, all_terms)]
  setorder(x, sort_order)
  x[, .(Attribute = attribute,
    `Before adding fees` = fifelse(baseline_status == "Not included", "Not included", cell(baseline_estimate, baseline_std_error)),
    `With fee bands` = cell(extended_estimate, extended_std_error),
    `With-fees 95% CI` = ci_cell(extended_lower95, extended_upper95))]
}
fit_display <- sample_comparison[, .(Group = unname(group_labels[group]), Distance = unname(model_labels[model]),
  Applicants = fmt_n(applicants), `Applicant-school pairs` = fmt_n(applicant_school_pairs),
  Schools = fmt_n(schools), `Log likelihood before fees` = f2(baseline_log_likelihood),
  `Log likelihood with fees` = f2(log_likelihood), Gain = f2(log_likelihood_gain), Iterations = iterations)]
joint_display <- joint_tests[, .(Group = unname(group_labels[group]), Distance = unname(model_labels[model]),
  `Robust Wald statistic` = f2(statistic), `Degrees of freedom` = df, `p value` = p_cell(p_value))]

# State the largest coefficient movements, with no causal or willingness-to-pay interpretation.
primary_slopes <- copy(slope_changes[group == "pooled" & model == "main" & term %in% specs$standardized])
primary_slopes <- primary_slopes[order(-abs(change))]
largest_movements <- head(primary_slopes, 3L)
movement_text <- paste0(paste0(largest_movements$school_type, ", ", largest_movements$label,
  ": ", f4(largest_movements$baseline_estimate), " before fees and ", f4(largest_movements$extended_estimate),
  " with fees (change ", f4(largest_movements$change), ")"), collapse = "; ")
pooled_n <- ms[group == "pooled" & variant == "extended" & model == "main"]
fee_main <- fees[group == "pooled" & model == "main" & is.finite(estimate) & code != 0L]
fee_summary <- if (nrow(fee_main)) paste0("The estimated paid-band utility coefficients range from ",
  f4(min(fee_main$estimate)), " to ", f4(max(fee_main$estimate)),
  " relative to a free school in the corresponding fee field.") else
  "No paid fee band is estimable in the pooled model; see the support table."
validation_summary <- data.table(Stage = c("Construction", "Estimation", "Independent verification", "Report checks"),
  `Checks passed` = c(nrow(build_checks), nrow(fit_checks), nrow(independent_checks), nrow(checks) + 1L))
group_sections <- paste(vapply(setdiff(names(group_labels), "pooled"), function(gr) paste0(
  "<details><summary>", esc_html(group_labels[[gr]]), "</summary><h3>Published student coordinates</h3>",
  table_html(fee_display(gr, "main")), "<h3>Home-comuna centroids</h3>", table_html(fee_display(gr, "municipality")),
  "<h3>Attribute slopes before and after fees: published coordinates</h3>", table_html(slope_display(gr, "main")), "</details>"), character(1)), collapse = "")

html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Directory prices and school preferences</title><style>',
  'body{font:16px/1.55 system-ui,sans-serif;color:#172536;background:#fff;margin:30px}main{max-width:1320px;margin:auto}',
  'h1{font-size:30px;margin-bottom:4px}h2{margin-top:34px;font-size:23px}h3{font-size:18px}.subtitle{color:#50627a;margin-top:0}',
  '.note{background:#f0f5fb;border-left:4px solid #3a689f;padding:14px 18px;margin:22px 0}',
  '.warning{background:#fff6df;border-left-color:#c18a15}.table-wrap{overflow:auto;margin:14px 0 25px}',
  'table{border-collapse:collapse;width:100%;font-size:14px}td,th{padding:8px 10px;border-bottom:1px solid #d9e0e8;text-align:right;vertical-align:top}',
  'th{background:#f3f6fa;font-weight:600}td:first-child,th:first-child{text-align:left}details{border:1px solid #d9e0e8;padding:12px;margin:18px 0;border-radius:6px}',
  'summary{font-weight:600;cursor:pointer}a{color:#245d9b}.links{font-size:14px}footer{font-size:13px;color:#607087;margin-top:32px}</style></head><body><main>',
  '<h1>Do families value school effectiveness?</h1><p class="subtitle">Adding enrollment and monthly fee bands from the 2024 school directory</p>',
  '<p class="note"><b>Same applicants before and after adding fees.</b> The pooled model uses ', fmt_n(pooled_n$applicants),
  ' applicants, ', fmt_n(pooled_n$applicant_school_pairs), ' applicant-school pairs and ', fmt_n(pooled_n$schools),
  ' destination schools. The largest absolute change among the pooled VA slopes is ', f4(max(abs(primary_slopes$change))),
  ' utility units per school SD. ', esc_html(fee_summary), ' The largest movements in the four VA slopes are: ', esc_html(movement_text), '.</p>',
  '<p class="note warning"><b>Interpretation:</b> These are conditional ranking associations within each submitted school list. The directory is a 2024 snapshot, after the 2018-2020 application decisions. Its fee bands describe later school prices; these estimates do not identify a causal price effect or the prices families faced when applying.</p>',
  '<h2>Price associations in the pooled model</h2><p>Each paid-band row compares that fee band with <b>free</b> in the same field, holding the other fee field, all seven school attributes, type indicators, type interactions and distance constant. The same schools lack information for both fee fields, so one shared unknown-fee indicator captures their association and compares them with free in both fields. Unknown prices are never coded as free. Parentheses contain student-clustered standard errors.</p>',
  '<h3>Published student coordinates</h3>', table_html(fee_display("pooled", "main")),
  '<h3>Home-comuna distance robustness</h3>', table_html(fee_display("pooled", "municipality")),
  '<p>The conditional odds ratio is exp(coefficient): the multiplicative change in the odds of selecting an alternative earlier among the remaining listed schools. Fee effects are common across school types within each model. A band with no identifying variation remains in these tables as "Not estimable"; it is not assigned a zero effect.</p>',
  '<h2>How adding fees changes preference associations</h2><p>These are total slopes for each school type, including the relevant interaction. Each attribute keeps its existing destination-school normalization, so a coefficient compares a one-school-SD increase before versus after adding the two fee fields. Differences describe sensitivity to fee controls; they can also reflect changes in the logit utility scale.</p>',
  table_html(slope_display("pooled", "main")),
  '<details><summary>Pooled attribute slopes using home-comuna centroids</summary>', table_html(slope_display("pooled", "municipality")), '</details>',
  '<p>All seven attributes enter jointly: math VA, exam-taking VA, high-premium-field VA, high-premium-institution VA, PAES M1 mean, parent education years and the existing grade-4-SIMCE parent-income decile. Each has interactions with Mixed HC/TP and TP-only; Non-TP is the reference school type.</p>',
  '<h2>Fee associations by applicant group</h2><p>Separate models cross prioritario status with school-ending versus voluntary applications. Each distance version uses the same applicants and complete submitted lists. Utility scales can differ across separately estimated groups, so coefficient magnitudes need care when comparing groups.</p>',
  group_sections,
  '<h2>Sample retention and price coverage</h2>', table_html(flow_display),
  '<p>A missing directory match excludes the entire submitted ranking. A matched school with unknown fee codes is retained with the shared unknown-fee indicator. Single-school lists remain excluded because they provide no ordering information. The preceding income-model restrictions are retained.</p>',
  table_html(band_display),
  '<p>School and ranking counts in different bands may overlap across the two fee fields. In particular, a ranking can include several price bands, so band-specific ranking counts need not sum to the estimation sample.</p>',
  '<h2>Model fit</h2>', table_html(fit_display), table_html(joint_display),
  '<p>The joint tests use the student-clustered covariance matrix and test all estimable fee-band indicators, including the shared unknown-fee indicator, against zero.</p>',
  '<p>Estimation uses the exact stratified Cox representation of the Plackett-Luce likelihood, one stratum per complete submitted ranking and clustering by student. Each fee field contributes five paid-band indicators with free as reference, plus one shared unknown-fee indicator. Bands without within-ranking variation are not estimated. No arbitrary peso midpoint or linear price schedule is imposed, and these categorical fee coefficients do not yield a continuous willingness-to-pay measure.</p>',
  '<details><summary>All pooled regression coefficients, before and after adding fees</summary>', table_html(regression_display("pooled", "main")), '</details>',
  '<details><summary>Fee identification support</summary>', table_html(support), '</details>',
  '<details><summary>Fit diagnostics</summary>', table_html(diagnostics), '</details>',
  '<h2>Validation and saved tables</h2>', table_html(validation_summary),
  '<p class="links"><a href="../../tables/school_preference_va/directory_prices_2024/joint_regression_before_after_fees.csv">All regression coefficients</a> &middot; ',
  '<a href="../../tables/school_preference_va/directory_prices_2024/fee_coefficients_by_group.csv">Fee coefficients by group</a> &middot; ',
  '<a href="../../tables/school_preference_va/directory_prices_2024/attribute_slopes_before_after_fees.csv">All attribute slopes before and after fees</a> &middot; ',
  '<a href="../../tables/school_preference_va/directory_prices_2024/sample_and_fit_comparison.csv">Sample and fit comparison</a></p>',
  '<footer>Reproducible report: 29_report_directory_prices_2024.R. Tables are constructed from saved estimates and student-clustered covariance contrasts; this report script does not fit regressions.</footer>',
  '</main></body></html>')
report_path <- file.path(report_dir, "with_directory_prices_2024.html")
writeLines(html, report_path, useBytes = TRUE)
check("Report contains complete price, slope and sample sections", file.exists(report_path) &&
  all(vapply(c("Price associations in the pooled model", "How adding fees changes preference associations",
    "Fee associations by applicant group", "Sample retention and price coverage", "Model fit"),
    function(needle) grepl(needle, html, fixed = TRUE), logical(1))))
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "report_session_info.txt"))
fwrite(checks, file.path(clean_dir, "report_verification.csv"))
message("Saved directory-price report; all ", nrow(checks), " report checks passed.")
