# Report saved PAES-M1 extension and matched-sample baseline fits; estimate no new models.
source("code/codex/school_preference_va/paes_common.R")
read_clean <- function(name) fread(file.path(clean_dir, name))
coefficients <- read_clean("coefficients.csv")
slopes <- read_clean("type_specific_slopes.csv")
samples <- read_clean("model_summary.csv")
diagnostics <- read_clean("model_diagnostics.csv")
flow <- read_clean("sample_flow.csv")
correlations <- read_clean("metric_correlations.csv")
m1_normalization <- read_clean("paes_m1_normalization.csv")
va_normalization <- read_clean("va_normalization.csv")
school_scores <- read_clean("school_paes_m1.csv")
score_audit <- read_clean("raw_score_audit.csv")
sources <- read_clean("source_manifest.csv")
verification <- read_clean("verification.csv")

groups <- names(group_labels)
subgroups <- setdiff(groups, "pooled")
school_types <- c("Non-TP", "Mixed HC/TP", "TP only")
models <- names(distance_vars)
variants <- names(variant_terms)
check("Twenty requested fits are available", nrow(samples) == 20L &&
  !anyDuplicated(samples[, .(group, variant, model)]) && setequal(samples$group, groups) &&
  setequal(samples$variant, variants) && setequal(samples$model, models))
check("Extended specification retains five metrics, two types, ten interactions and distance",
  length(extended_static_terms) == 17L && length(metric_terms) == 5L &&
  nrow(metric_interactions) == 10L && length(base_static_terms) == 14L)
check("No duplicate coefficient records", !anyDuplicated(coefficients[, .(group, variant, model, term)]))
check("No duplicate total-slope records", !anyDuplicated(slopes[, .(group, variant, model, school_type, term)]))
check("Coefficient estimates and standard errors finite", all(is.finite(coefficients$estimate)) &&
  all(is.finite(coefficients$std_error) & coefficients$std_error > 0))
check("Total-slope estimates and standard errors finite", all(is.finite(slopes$estimate)) &&
  all(is.finite(slopes$std_error) & slopes$std_error > 0))
for (g in groups) for (v in variants) for (m in models) {
  wanted <- c(variant_terms[[v]], distance_vars[[m]])
  a <- coefficients[group == g & variant == v & model == m]
  check(paste(g, v, m, "all requested coefficients present"), nrow(a) == length(wanted) && setequal(a$term, wanted))
  active_metrics <- metric_specs[standardized %in% wanted]
  for (st in school_types) {
    b <- slopes[group == g & variant == v & model == m & school_type == st]
    check(paste(g, v, m, st, "all total metric slopes present"),
      nrow(b) == nrow(active_metrics) && setequal(b$term, active_metrics$standardized))
    expected <- a[match(active_metrics$standardized, term), estimate]
    if (st != "Non-TP") {
      target_control <- if (st == "Mixed HC/TP") "mixed_hc_tp" else "tp_only"
      additional <- metric_interactions[control == target_control][match(active_metrics$standardized, metric), term]
      expected <- expected + a[match(additional, term), estimate]
    }
    check(paste(g, v, m, st, "total slopes equal baseline plus interaction"),
      max(abs(b[match(active_metrics$standardized, term), estimate] - expected)) < 1e-10)
  }
}
for (g in groups) check(paste(g, "all four fits use identical sample counts"), all(vapply(
  c("applicants", "rankings", "applicant_school_pairs", "schools", "informative_rank_decisions"),
  function(v) uniqueN(samples[group == g][[v]]) == 1L, logical(1))))
check("All upstream verification checks pass", nrow(verification) > 0L && all(verification$passed))
check("Original four-VA normalization retained", all(va_normalization$n_reference_schools == 2501L) &&
  all(va_normalization$reference_sd > 0))
check("M1 normalization has one positive reference SD", nrow(m1_normalization) == 1L &&
  is.finite(m1_normalization$reference_sd) && m1_normalization$reference_sd > 0)

f4 <- function(x) formatC(x, digits = 4L, format = "f", big.mark = ",")
f2 <- function(x) formatC(x, digits = 2L, format = "f", big.mark = ",")
pct <- function(x) paste0(formatC(100 * x, digits = 2L, format = "f"), "%")
coef_cell <- function(b, se) paste0(f4(b), " (", f4(se), ")")
pretty_numeric <- function(x) {
  x <- copy(x)
  for (v in names(x)) if (is.numeric(x[[v]])) {
    a <- x[[v]]
    x[, (v) := if (all(is.na(a) | a == round(a))) fmt_n(a) else f4(a)]
  }
  x
}
panel_index <- rbindlist(lapply(school_types, function(st) data.table(school_type = st,
  term = metric_terms, regressor = paste(st, metric_specs$label, sep = ": "))))

# Panels retain numeric estimates/SEs and formatted display cells.
make_panel <- function(kind, target_groups, target_models) {
  if (kind == "slopes") {
    x <- copy(panel_index)
    x[, row_type := "total_slope"]
  } else {
    x <- data.table(term = c(extended_static_terms, "distance"),
      regressor = c(unname(metric_labels), "log(1 + distance in km)"), row_type = "coefficient")
  }
  column_labels <- character()
  for (g in target_groups) for (m in target_models) {
    column <- if (length(target_groups) == 1L) m else g
    column_labels[column] <- if (length(target_groups) == 1L) model_labels[[m]] else group_labels[[g]]
    if (kind == "slopes") {
      a <- slopes[group == g & variant == "extended" & model == m]
      a <- a[match(paste(x$school_type, x$term), paste(a$school_type, a$term))]
    } else {
      a <- coefficients[group == g & variant == "extended" & model == m]
      a <- a[match(c(extended_static_terms, distance_vars[[m]]), term)]
    }
    x[, (paste0(column, "_estimate")) := a$estimate]
    x[, (paste0(column, "_std_error")) := a$std_error]
    x[, (column) := coef_cell(a$estimate, a$std_error)]
  }
  if (kind == "raw") {
    stat <- data.table(term = NA_character_, row_type = "sample_statistic",
      regressor = c("Applicants", "Student-school pairs", "Destination schools", "Log likelihood"))
    for (g in target_groups) for (m in target_models) {
      column <- if (length(target_groups) == 1L) m else g
      s <- samples[group == g & variant == "extended" & model == m]
      values <- vapply(c("applicants", "applicant_school_pairs", "schools", "log_likelihood"),
        function(v) as.numeric(s[[v]]), numeric(1))
      stat[, (paste0(column, "_estimate")) := values]
      stat[, (paste0(column, "_std_error")) := NA_real_]
      stat[, (column) := c(fmt_n(values[1:3]), f4(values[4]))]
    }
    x <- rbindlist(list(x, stat), use.names = TRUE)
  }
  list(data = x, labels = column_labels)
}
panels <- list(pooled_total_slopes = make_panel("slopes", "pooled", models),
  pooled_raw_coefficients = make_panel("raw", "pooled", models))
for (m in models) {
  panels[[paste0("group_total_slopes_", m)]] <- make_panel("slopes", subgroups, m)
  panels[[paste0("group_raw_coefficients_", m)]] <- make_panel("raw", subgroups, m)
}
for (name in names(panels)) fwrite(panels[[name]]$data, file.path(table_dir, paste0(name, ".csv")))

# A before/after change is descriptive; the two fit estimates are statistically dependent.
baseline <- slopes[variant == "matched_baseline", .(group, model, school_type, term, label,
  baseline_estimate = estimate, baseline_std_error = std_error)]
added <- slopes[variant == "extended" & term %in% base_va_terms,
  .(group, model, school_type, term, extended_estimate = estimate, extended_std_error = std_error)]
changes <- merge(baseline, added, by = c("group", "model", "school_type", "term"), all = TRUE)
changes[, change := extended_estimate - baseline_estimate]
check("Matched comparison contains every VA/type/group/distance combination", nrow(changes) == 120L &&
  all(is.finite(changes$change)))
setorderv(changes, c("group", "model", "school_type", "term"))
fwrite(changes, file.path(table_dir, "matched_baseline_changes.csv"))
comparison_display <- function(g, m) {
  a <- changes[group == g & model == m]
  a <- a[order(match(school_type, school_types), match(term, base_va_terms))]
  a[, .(`School type and VA` = paste(school_type, label, sep = ": "),
    `Four VAs, matched sample` = coef_cell(baseline_estimate, baseline_std_error),
    `Add PAES M1 and interactions` = coef_cell(extended_estimate, extended_std_error),
    `Change in total slope` = f4(change))]
}

# LaTeX fragments follow the existing width-constrained manuscript convention.
bs <- intToUtf8(92L)
tex_escape <- function(x) {
  for (z in c("&", "%", "_", "#")) x <- gsub(z, paste0(bs, z), x, fixed = TRUE)
  x
}
write_tex <- function(display, name, caption, notes, break_rows = integer()) {
  rows <- vapply(seq_len(nrow(display)), function(i) paste0(
    if (i %in% break_rows) paste0(bs, "midrule\n") else "",
    paste(tex_escape(as.character(display[i])), collapse = " & "), " ", bs, bs), character(1))
  tex <- c(paste0(bs, "begin{table}[!htbp]"), paste0(bs, "centering"),
    paste0(bs, "caption{", tex_escape(caption), "}"), paste0(bs, "label{tab:paes-m1-", gsub("_", "-", name), "}"),
    paste0(bs, "small"), paste0(bs, "setlength{", bs, "tabcolsep}{4pt}"),
    paste0(bs, "resizebox{", bs, "textwidth}{!}{%"),
    paste0(bs, "begin{tabular}{l", paste(rep("c", ncol(display) - 1L), collapse = ""), "}"),
    paste0(bs, "toprule"), paste0(paste(tex_escape(names(display)), collapse = " & "), " ", bs, bs),
    paste0(bs, "midrule"), rows, paste0(bs, "bottomrule"), paste0(bs, "end{tabular}"), "}",
    paste0(bs, "par", bs, "smallskip"), paste0(bs, "begin{minipage}{0.98", bs, "linewidth}", bs, "footnotesize"),
    paste0(bs, "textit{Notes:} ", notes), paste0(bs, "end{minipage}"), paste0(bs, "end{table}"))
  writeLines(tex, file.path(table_dir, paste0(name, ".tex")))
}
panel_display <- function(name) {
  p <- panels[[name]]
  a <- copy(p$data[, c("regressor", names(p$labels)), with = FALSE])
  setnames(a, c("Regressor or total slope", unname(p$labels)))
  a
}
table_notes <- paste0("Joint submitted-set rank-ordered logits with four VAs, PAES M1 school mean, two school-type indicators, ",
  "ten type-by-metric interactions and log(1 + distance in km). Parentheses report student-clustered standard errors. ",
  "Total slopes add baseline coefficients and relevant interactions; their standard errors include covariance terms. ",
  "The original four-VA normalization is fixed across 2,501 pooled destination schools; M1 is standardized across unique ",
  "schools in the common PAES-complete sample. Both distance versions and matched-baseline fits retain the same complete ",
  "rankings within each applicant group. Non-TP includes HC-only, artistic-only and other schools. Type-indicator coefficients ",
  "apply only at all five standardized metrics equal to zero. PAES process 2025 records 2024 graduates' regular late-2024 ",
  "M1 sitting, after the 2018-2020 applications; the score mean is measured among test takers. No 2024-process fallback is used. ",
  "Associations are descriptive; separate group logits can differ in utility-error scale.")
for (name in names(panels)) write_tex(panel_display(name), name,
  paste0("School ranking preferences with PAES M1: ", gsub("_", " ", name)), table_notes,
  if (grepl("total_slopes", name)) c(6L, 11L) else 19L)
write_tex(comparison_display("pooled", "main"), "pooled_matched_baseline_comparison",
  "Total VA slopes before and after adding PAES M1, identical applicants and rankings",
  paste0("Both fits use published student coordinates and identical complete rankings. Each cell is a total VA slope with ",
    "student-clustered SE. Change equals extended minus matched-baseline point estimate. No SE or test is assigned to this ",
    "between-fit change. Both models retain the original four-VA normalization; the extended model adds standardized PAES M1 ",
    "and its two school-type interactions. The 2025-process score mean is measured after the 2018-2020 rankings."), c(5L, 9L))

# Pooled and subgroup sample-loss accounting, without altering any submitted lists.
pooled_flow <- flow[tolower(as.character(sae_proceso)) %in% c("all", "pooled")]
if (!nrow(pooled_flow)) {
  pooled_flow <- flow[, lapply(.SD, sum), by = .(group, label),
    .SDcols = c("original_applicants", "baseline_eligible", "excluded_missing_m1", "estimation_applicants")]
  pooled_flow[, sae_proceso := "All"]
}
pooled_flow <- pooled_flow[order(match(group, groups))]
check("Pooled coverage has all five group definitions", nrow(pooled_flow) == length(groups) &&
  !anyDuplicated(pooled_flow$group) && setequal(pooled_flow$group, groups))
check("Complete-ranking M1 loss accounts for the matched sample", all(pooled_flow$baseline_eligible -
  pooled_flow$excluded_missing_m1 == pooled_flow$estimation_applicants))
for (g in groups) check(paste(g, "coverage agrees with estimation"),
  pooled_flow[group == g, estimation_applicants] == samples[group == g & variant == "extended" & model == "main", applicants])
coverage_display <- pooled_flow[, .(Group = label, `Paper roster` = fmt_n(original_applicants),
  `Eligible before M1` = fmt_n(baseline_eligible), `Whole lists missing M1` = fmt_n(excluded_missing_m1),
  `Matched estimation sample` = fmt_n(estimation_applicants),
  `Share of baseline retained` = pct(estimation_applicants / baseline_eligible))]
annual_flow <- flow[!tolower(as.character(sae_proceso)) %in% c("all", "pooled")]
annual_flow <- annual_flow[order(sae_proceso, match(group, groups))]
source_years <- school_scores[, .(Schools = uniqueN(rbd), `M1 test takers` = sum(n_m1_takers, na.rm = TRUE),
  `Unweighted mean of school means` = mean(paes_m1_mean, na.rm = TRUE),
  `Minimum takers per school` = min(n_m1_takers), `Median takers per school` = as.numeric(median(n_m1_takers)),
  `Maximum takers per school` = max(n_m1_takers), `Schools with fewer than 5 takers` = sum(n_m1_takers < 5L),
  `Share with fewer than 10 takers` = pct(mean(n_m1_takers < 10L))), by = .(paes_process, graduation_year)]

# Describe M1 and VA associations within type; do not treat type intercepts as constant premiums.
pooled_m1 <- slopes[group == "pooled" & variant == "extended" & model == "main" & term == "z_paes_m1"]
pooled_m1 <- pooled_m1[match(school_types, school_type)]
m1_interpretation <- paste0("The main-model PAES M1 total slope is ",
  paste0(pooled_m1$school_type, ": ", f4(pooled_m1$estimate), " (SE ", f4(pooled_m1$std_error), ")", collapse = "; "),
  ". Each is the conditional utility change per one-school-SD increase in the score mean within that school type. ",
  "One M1 school SD is ", f2(m1_normalization$reference_sd), " score points in the matched school sample.")
uncertain_m1 <- pooled_m1[p_value > .05, school_type]
if (length(uncertain_m1)) m1_interpretation <- paste0(m1_interpretation, " The slope for ",
  paste(uncertain_m1, collapse = ", "), " is not statistically distinguished from zero at the 5% level.")
pooled_changes <- changes[group == "pooled" & model == "main"]
largest_change <- pooled_changes[order(-abs(change))][1L]
change_interpretation <- paste0("On the identical applicant sample, the largest absolute movement in a total VA slope is ",
  largest_change$school_type, " ", largest_change$label, ": ", f4(largest_change$baseline_estimate), " to ",
  f4(largest_change$extended_estimate), " (change ", f4(largest_change$change), "). The comparison below separates ",
  "the added score covariate from sample-composition changes.")
math_changes <- pooled_changes[term == "z_va_math"][match(school_types, school_type)]
math_interpretation <- paste0("The math VA total slopes move from ",
  paste0(math_changes$school_type, " ", f4(math_changes$baseline_estimate), " to ",
    f4(math_changes$extended_estimate), collapse = "; "),
  ". The extended math VA coefficients are conditional associations at fixed observed M1 school mean and the other metrics. ",
  "Observed mean achievement and VA represent different, correlated school attributes.")
main_m1 <- slopes[variant == "extended" & model == "main" & term == "z_paes_m1",
  .(group, school_type, main_estimate = estimate, main_std_error = std_error)]
comuna_m1 <- slopes[variant == "extended" & model == "municipality" & term == "z_paes_m1",
  .(group, school_type, comuna_estimate = estimate, comuna_std_error = std_error)]
m1_sensitivity <- merge(main_m1, comuna_m1, by = c("group", "school_type"))
setorder(m1_sensitivity, group, school_type)
m1_sensitivity_display <- m1_sensitivity[, .(Group = group_labels[group], `School type` = school_type,
  `Published coordinates` = coef_cell(main_estimate, main_std_error),
  `Home-comuna centroids` = coef_cell(comuna_estimate, comuna_std_error),
  `Point-estimate change` = f4(comuna_estimate - main_estimate))]
normalization_display <- rbindlist(list(va_normalization[, .(Metric = label, Mean = reference_mean,
  SD = reference_sd, `Reference schools` = n_reference_schools)],
  m1_normalization[, .(Metric = "PAES M1 school mean", Mean = reference_mean, SD = reference_sd,
    `Reference schools` = n_reference_schools)]))
all_count <- samples[group == "pooled" & variant == "extended" & model == "main"]
geography_note <- paste0("The pooled extension retains ", fmt_n(all_count$applicants), " applicants and ",
  fmt_n(all_count$applicant_school_pairs), " student-school pairs at ", fmt_n(all_count$schools),
  " destination schools. Published student points and home-comuna centers use the same actual school-campus coordinates. ",
  "Both distance measures enter as log(1 + straight-line Haversine kilometers). Published points retain the original ",
  "privacy displacement and georeferencing-quality mix. No school or student coordinates are reconstructed here.")

clean_link <- function(name, label) paste0('<a download href="../../../data/clean/', task,
  '/paes_m1/', name, '">', esc_html(label), '</a>')
table_link <- function(name, label) paste0('<a download href="../../tables/', task, '/paes_m1/', name,
  '">', esc_html(label), '</a>')
downloads <- paste0('<ul><li>', table_link("pooled_total_slopes.csv", "Pooled total slopes (CSV)"), '; ',
  table_link("pooled_total_slopes.tex", "LaTeX table"), '</li><li>',
  table_link("matched_baseline_changes.csv", "Matched-baseline changes: all groups and distances (CSV)"), '; ',
  table_link("pooled_matched_baseline_comparison.tex", "Pooled comparison (LaTeX)"), '</li><li>',
  table_link("group_total_slopes_main.tex", "Four-group main slope panel (LaTeX)"), '; ',
  table_link("group_total_slopes_municipality.tex", "Four-group comuna slope panel (LaTeX)"), '</li><li>',
  clean_link("coefficients.csv", "All 20 fits: coefficients and clustered SEs"), '; ',
  clean_link("type_specific_slopes.csv", "All total slopes and confidence intervals"), '</li><li>',
  clean_link("sample_flow.csv", "Annual sample accounting"), '; ', clean_link("source_manifest.csv", "Source manifest"), '; ',
  clean_link("paes_m1_normalization.csv", "Score normalization"), '</li></ul>')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1"><title>School preferences with PAES M1</title>',
  '<style>body{font:16px/1.55 system-ui,sans-serif;color:#172536;background:#f6f8fa;margin:0}',
  'main{max-width:1450px;margin:30px auto;padding:30px;background:white;border:1px solid #dce3e9;border-radius:10px}',
  'h1{font-size:30px;line-height:1.2}h2{font-size:23px;margin-top:36px}.notes,.muted{color:#556373}.notes{font-size:14px}',
  '.temporal{border-left:4px solid #9c6f21;background:#fff8e9;padding:12px 16px}.table{overflow:auto;margin:18px 0}',
  'table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:9px 11px;text-align:right;border-bottom:1px solid #e3e8ed}',
  'th:first-child,td:first-child{text-align:left}thead th{background:#edf2f6;white-space:nowrap}',
  'a{color:#175d99}li{margin:6px 0}details{margin:18px 0}summary{cursor:pointer;font-weight:600}',
  '.slopes tbody tr:nth-child(6) td,.slopes tbody tr:nth-child(11) td{border-top:2px solid #7a8794}</style></head><body><main>',
  '<h1>School ranking preferences with average PAES M1 achievement</h1>',
  '<p class="muted">Four VA dimensions plus school-average PAES M1, with separate slopes for non-TP, mixed HC/TP ',
  'and TP-only schools. Each joint rank-ordered logit includes five metrics, two type indicators, ten interactions and distance.</p>',
  '<p class="temporal">PAES process 2025 measures the regular late-2024 M1 sitting among 2024 graduates. These scores ',
  'postdate the 2018-2020 SAE rankings. The score mean is a retrospective school characteristic, not information observed ',
  'by families when they applied. It covers M1 test takers rather than all students; MATE1_MAX and a 2024-process fallback are not used.</p>',
  '<h2>Pooled total slopes by school type</h2><div class="slopes">', html_table(panel_display("pooled_total_slopes")), '</div>',
  '<p class="notes">Each cell is a total slope and student-clustered SE. Non-TP is the baseline; other types add the relevant ',
  'interaction, including its covariance in the SE. A one-school-SD change is measured with fixed normalization. ',
  'The original four VA scales are unchanged; the M1 scale is defined on the common PAES-complete school sample.</p>',
  '<p>', esc_html(m1_interpretation), '</p>',
  '<p>Total slopes describe associations with ordering among schools remaining in submitted lists, holding the other metrics ',
  'and distance fixed within school type. They do not identify causal responses or explain consideration sets. The type-indicator ',
  'intercepts compare types only when all five standardized metrics equal zero; type differences vary with the metric vector.</p>',
  '<details><summary>Full 18-coefficient pooled specification and sample sizes</summary>',
  html_table(panel_display("pooled_raw_coefficients")), '</details>',
  '<h2>What changes when M1 is added on the same sample?</h2><p>', esc_html(change_interpretation), '</p>',
  '<p>', esc_html(math_interpretation), '</p>',
  html_table(comparison_display("pooled", "main")),
  '<p class="notes">Both columns use published coordinates and identical complete rankings. The matched baseline includes ',
  'the four VAs, type indicators and eight VA interactions. The extended model adds the M1 mean and its two interactions. ',
  'The final column is a point-estimate difference; no between-fit SE or formal test is reported.</p>',
  '<details><summary>Same comparison using home-comuna centroids</summary>', html_table(comparison_display("pooled", "municipality")), '</details>',
  '<h2>Four applicant groups: total slopes</h2><h3>Published student coordinates</h3><div class="slopes">',
  html_table(panel_display("group_total_slopes_main")), '</div><h3>Home-comuna centroids</h3><div class="slopes">',
  html_table(panel_display("group_total_slopes_municipality")), '</div>',
  '<p class="notes">Separate models use the same VA and M1 normalizations. Differences across applicant-group coefficients ',
  'can reflect different unobserved utility-error scales as well as preferences. No formal cross-group equality tests are reported. ',
  'School-ending and voluntary status refer to the origin school\'s continuation offering, not a realized move. ',
  'The pooled sample can include applicants with unresolved origin classifications, excluded from the four defined groups.</p>',
  '<details><summary>Full 18-coefficient specifications by applicant group</summary><h3>Published coordinates</h3>',
  html_table(panel_display("group_raw_coefficients_main")), '<h3>Comuna centroids</h3>',
  html_table(panel_display("group_raw_coefficients_municipality")), '</details>',
  '<details><summary>Matched-baseline changes within each applicant group</summary>',
  paste(vapply(subgroups, function(g) paste0('<h3>', esc_html(group_labels[[g]]), '</h3>',
    html_table(comparison_display(g, "main"))), character(1)), collapse = ''), '</details>',
  '<h2>Geography sensitivity of the M1 slope</h2>', html_table(m1_sensitivity_display),
  '<p>', esc_html(geography_note), '</p>',
  '<h2>Coverage and complete-ranking restrictions</h2>', html_table(coverage_display),
  '<p>Construction begins with the existing complete-ranking estimation data. If any submitted school lacks the selected ',
  'M1 mean, the entire ranking is excluded from both the extended model and its matched baseline. No list is shortened. ',
  'Earlier exclusions for singleton lists and missing VA remain in the eligibility counts. Applicants with and without ',
  'assignment risk remain eligible. The previous preference models and reports are preserved.</p>',
  if (nrow(annual_flow)) paste0('<details><summary>Coverage by application year</summary>', html_table(pretty_numeric(annual_flow)), '</details>') else '',
  '<h2>Score source and normalization</h2>', html_table(pretty_numeric(source_years)),
  '<p>The regular M1 school mean uses the constructed public score source and the selected 2025 process. ',
  'A school without that source mean remains missing; the analysis substitutes neither a previous process nor the ',
  'maximum-across-sittings M1 measure. School means average scores among the recorded M1 takers, not all graduates. ',
  'The source-coverage table includes every constructed school mean before SAE matching and shows both processes for auditing; ',
  'the 2024-process means do not enter either preference model. ',
  'There is no minimum-taker threshold or score imputation. The school-level taker counts show that small-school means ',
  'can be based on few test takers.</p>',
  html_table(pretty_numeric(normalization_display)),
  '<p>Each school has equal weight in the M1 normalization regardless of applications or test-taker count. The original EB ',
  'VAs and their 2,501-school reference moments are reused. Inference conditions on the constructed VA indices and score ',
  'means; their first-stage or school-mean sampling uncertainty is not propagated. School types use the fixed 2024 ',
  'Directorio: mixed offers HC and TP; TP-only offers TP without HC. The non-TP reference includes HC-only, artistic-only ',
  'and other/no regular-secondary schools. Lower grades are allowed and no active-2024-school restriction is imposed.</p>',
  '<details><summary>Raw-score source audit</summary>', html_table(pretty_numeric(score_audit)), '</details>',
  '<h2>Diagnostics and validation</h2><p>All ', fmt_n(nrow(verification)), ' upstream checks passed. The exact submitted-set ',
  'Plackett-Luce likelihood is evaluated with the stratified Cox representation and student-clustered covariance. The ',
  'matched baseline has 15 coefficients; the extension has 18. No new regressions are estimated by this report.</p>',
  html_table(pretty_numeric(diagnostics)),
  '<details><summary>Fit sample sizes and optimizer summaries</summary>', html_table(pretty_numeric(samples)), '</details>',
  '<details><summary>Metric correlations and collinearity diagnostics</summary>', html_table(pretty_numeric(correlations)), '</details>',
  '<details><summary>Independent validation checks</summary>', html_table(verification), '</details>',
  '<h2>Reusable outputs</h2>', downloads,
  '<details><summary>Source manifest</summary>', html_table(pretty_numeric(sources)), '</details>',
  '<p class="notes">Generated by code/codex/school_preference_va/15_report_paes_m1.R from saved fits. ',
  'All output is separate from the earlier four-VA preference analysis. The manuscript is unchanged.</p></main></body></html>')
writeLines(html, file.path(report_dir, "with_paes_m1.html"), useBytes = TRUE)
fwrite(checks, file.path(clean_dir, "report_verification.csv"))
message("Saved PAES-M1 preference report: ", file.path(report_dir, "with_paes_m1.html"))
