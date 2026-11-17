# Render the saved parent-education models; fit no regressions here.
source("code/codex/school_preference_va/parent_education_common.R")
read_clean <- function(n) fread(file.path(clean_dir, n))
co <- read_clean("coefficients.csv")
sl <- read_clean("type_specific_slopes.csv")
ms <- read_clean("model_summary.csv")
flow <- read_clean("sample_flow.csv")
norm <- read_clean("parent_education_normalization.csv")
schools <- read_clean("school_parent_education_2022.csv")
correlations <- read_clean("metric_correlations.csv")
build_checks <- read_clean("build_verification.csv")
fit_checks <- read_clean("fit_verification.csv")
check("Twenty models are available", nrow(ms) == 20L &&
  !anyDuplicated(ms[, .(group, variant, model)]))
check("All construction and fit checks passed", nrow(build_checks) >= 1L && all(build_checks$passed) &&
  nrow(fit_checks) >= 1L && all(fit_checks$passed))
check("Coefficient and total-slope outputs contain no duplicates",
  !anyDuplicated(co[, .(group, variant, model, term)]) &&
  !anyDuplicated(sl[, .(group, variant, model, school_type, term)]))
check("All reported estimates and clustered SEs are finite", all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0) && all(is.finite(sl$estimate)) &&
  all(is.finite(sl$std_error) & sl$std_error > 0))

f4 <- function(x) formatC(x, digits = 4L, format = "f", big.mark = ",")
f2 <- function(x) formatC(x, digits = 2L, format = "f", big.mark = ",")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
pct <- function(x) paste0(formatC(100 * x, digits = 2L, format = "f"), "%")
cell <- function(b, s) paste0(f4(b), " (", f4(s), ")")
school_types <- c("Non-TP", "Mixed HC/TP", "TP only")
main_slopes <- sl[group == "pooled" & variant == "extended" & model == "main"]
muni_slopes <- sl[group == "pooled" & variant == "extended" & model == "municipality"]
pooled <- copy(main_slopes)
pooled[, `Main model` := cell(estimate, std_error)]
muni <- muni_slopes[match(paste(pooled$school_type, pooled$term), paste(school_type, term))]
pooled[, `Comuna-distance model` := cell(muni$estimate, muni$std_error)]
pooled[, `p value, main` := formatC(p_value, digits = 3L, format = "f")]
pooled_display <- pooled[, .(`School type` = school_type, Attribute = label,
  `Main model` = `Main model`, `Comuna-distance model` = `Comuna-distance model`,
  `p value, main` = `p value, main`)]
fwrite(pooled, file.path(table_dir, "pooled_total_slopes.csv"))
tex_display <- pooled[, .(`School type` = school_type, Attribute = label,
  `Published coordinates` = `Main model`, `Comuna centroids` = `Comuna-distance model`)]
bs <- intToUtf8(92L)
tex_escape <- function(x) {
  for (z in c("&", "%", "_", "#")) x <- gsub(z, paste0(bs, z), x, fixed = TRUE)
  x
}
tex_rows <- vapply(seq_len(nrow(tex_display)), function(i) paste0(
  paste(tex_escape(as.character(tex_display[i])), collapse = " & "), " ", bs, bs), character(1))
tex <- c(paste0(bs, "begin{table}[!htbp]"), paste0(bs, "centering"),
  paste0(bs, "caption{Preference slopes with 2022 SIMCE parent education}"),
  paste0(bs, "label{tab:school-preferences-parent-education-2022}"), paste0(bs, "small"),
  paste0(bs, "resizebox{", bs, "textwidth}{!}{%"), paste0(bs, "begin{tabular}{llcc}"),
  paste0(bs, "toprule"), paste0("School type & Attribute & Published coordinates & Comuna centroids ", bs, bs),
  paste0(bs, "midrule"), tex_rows, paste0(bs, "bottomrule"), paste0(bs, "end{tabular}"), "}",
  paste0(bs, "par", bs, "smallskip"), paste0(bs, "begin{minipage}{0.98", bs, "linewidth}", bs, "footnotesize"),
  paste0(bs, "textit{Notes:} Total utility slopes per one school-level standard deviation; parentheses contain student-clustered standard errors. The six school attributes are the four EB VAs, 2025-process PAES M1 school mean and 2022 SIMCE parent-education mean. Parent education is measured after SAE applications from 2018--2020; associations are descriptive. Both columns use the same complete submitted rankings. "),
  paste0(bs, "end{minipage}"), paste0(bs, "end{table}"))
writeLines(tex, file.path(table_dir, "pooled_total_slopes.tex"))

parent_main <- sl[group != "pooled" & variant == "extended" & model == "main" & suffix == "parent_education_2022"]
parent_muni <- sl[group != "pooled" & variant == "extended" & model == "municipality" & suffix == "parent_education_2022"]
subgroup <- copy(parent_main)
subgroup[, `Main model` := cell(estimate, std_error)]
sub_muni <- parent_muni[match(paste(subgroup$group, subgroup$school_type), paste(group, school_type))]
subgroup[, `Comuna-distance model` := cell(sub_muni$estimate, sub_muni$std_error)]
subgroup[, `Group` := group_labels[group]]
subgroup_display <- subgroup[, .(Group, `School type` = school_type, `Parent education years per SD` = `Main model`,
  `Comuna-distance model` = `Comuna-distance model`, `p value, main` = formatC(p_value, digits = 3L, format = "f"))]
fwrite(subgroup, file.path(table_dir, "parent_education_slopes_by_group.csv"))

base_slopes <- sl[variant == "matched_baseline" & model == "main",
  .(group, school_type, term, outcome, label, baseline = estimate, baseline_se = std_error)]
new_slopes <- sl[variant == "extended" & model == "main" & term %in% base_paes_specs$standardized,
  .(group, school_type, term, outcome, extended = estimate, extended_se = std_error)]
change <- merge(base_slopes, new_slopes, by = c("group", "school_type", "term", "outcome"), all = TRUE)
change[, change := extended - baseline]
setorder(change, group, school_type, term)
fwrite(change, file.path(table_dir, "existing_attribute_slope_changes.csv"))

samples <- ms[variant == "extended" & model == "main"]
sample_display <- samples[, .(Group = group_labels[group], Applicants = fmt_n(applicants),
  `Student-school pairs` = fmt_n(applicant_school_pairs), `Destination schools` = fmt_n(schools),
  `Log likelihood` = f4(log_likelihood))]
sample_display[, group_order := match(names(group_labels)[match(Group, group_labels)], names(group_labels))]
setorder(sample_display, group_order); sample_display[, group_order := NULL]
flow_display <- copy(flow)
flow_display[, `Share of PAES-M1 sample retained` := pct(parent_education_estimation_applicants / paes_m1_eligible)]
flow_display[, `:=`(Group = label, `PAES-M1 eligible applicants` = fmt_n(paes_m1_eligible),
  `Whole lists excluded for missing parent education` = fmt_n(excluded_missing_parent_education),
  `Applicants retained` = fmt_n(parent_education_estimation_applicants))]
flow_display <- flow_display[, .(Group, `PAES-M1 eligible applicants`,
  `Whole lists excluded for missing parent education`, `Applicants retained`, `Share of PAES-M1 sample retained`)]
normalization_display <- data.table(Statistic = c("Destination schools used for standardization", "Unweighted average across destination schools",
  "School-level SD", "Mother responses", "Father responses", "Families with at least one parent response",
  "Families with both parent responses"),
  Value = c(fmt_n(norm$reference_schools), f2(norm$reference_mean_years), f2(norm$reference_sd_years),
    fmt_n(sum(schools$n_mother_responses)), fmt_n(sum(schools$n_father_responses)),
    fmt_n(sum(schools$n_families_observed)), fmt_n(sum(schools$n_both_parents))))
destination_schools <- unique(as.data.table(readRDS(paes_input_path))[, .(rbd)])
destination_coverage <- merge(destination_schools,
  schools[, .(rbd, mean_parent_education_years, n_simce_students)], by = "rbd", all.x = TRUE)
coverage <- data.table(
  `Destination schools with 2022 survey records` = sum(!is.na(destination_coverage$n_simce_students)),
  `Destination schools with valid parent-education mean` = sum(is.finite(destination_coverage$mean_parent_education_years)),
  `Destination schools in PAES-M1 support` = nrow(destination_schools),
  `2022 SIMCE schools with valid mean` = sum(is.finite(schools$mean_parent_education_years)),
  `All 2022 SIMCE schools` = nrow(schools))
fwrite(coverage, file.path(table_dir, "coverage.csv"))

pooled_parent <- main_slopes[term == "z_parent_education"][match(school_types, school_type)]
positive <- pooled_parent[estimate > 0, school_type]
negative <- pooled_parent[estimate < 0, school_type]
interpretation <- paste0("Per one school-level SD in average parent education (", f2(norm$reference_sd_years),
  " years), the estimated utility association is ",
  paste0(pooled_parent$school_type, " ", f4(pooled_parent$estimate), " (SE ", f4(pooled_parent$std_error), ")", collapse = "; "),
  ". These are total slopes: the baseline parent-education coefficient plus the relevant school-type interaction. ")
if (length(positive)) interpretation <- paste0(interpretation, "The point estimates are positive for ", paste(positive, collapse = ", "), ". ")
if (length(negative)) interpretation <- paste0(interpretation, "The point estimates are negative for ", paste(negative, collapse = ", "), ". ")
interpretation <- paste0(interpretation,
  "The specification includes all four EB VAs, PAES M1, parent education, distance, two school-type indicators, and each of the six attribute-by-type interactions.")

table_html <- function(x) {
  head <- paste0("<th>", esc_html(names(x)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(x)), function(i) paste0("<tr>",
    paste0("<td>", vapply(x[i], function(z) esc_html(as.character(z)), character(1)), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<div class='table-wrap'><table><thead><tr>", head, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Parent education in school preferences</title>',
  '<style>body{font:16px/1.55 system-ui;color:#172536;margin:30px}main{max-width:1280px;margin:auto}h1,h2{line-height:1.2}table{border-collapse:collapse;width:100%;font-size:14px}td,th{padding:8px;border-bottom:1px solid #d9e0e8;text-align:right}td:first-child,th:first-child{text-align:left}.table-wrap{overflow:auto;margin:14px 0 28px}.note{background:#f3f6fa;padding:14px;border-radius:8px}details{margin:22px 0}</style></head><body><main>',
  '<h1>Do families value school effectiveness?</h1><p>Preference models with average parent education as a school attribute</p>',
  '<p class="note"><b>Interpretation:</b> ', esc_html(interpretation),
  'The parent-survey measure comes from 2nd-year high-school students in SIMCE 2022, after the 2018–2020 SAE applications. Its coefficient is descriptive and does not establish that families observed these later school averages when they applied.</p>',
  '<h2>Joint model: total attribute slopes by school type</h2>', table_html(pooled_display),
  '<p>Entries are conditional utility slopes per one destination-school SD, with student-clustered standard errors in parentheses. Total slopes add the attribute main effect to the school-type interaction. The two columns use the same applicants, schools, and submitted lists; only student distance differs.</p>',
  '<h2>Parent-education slopes across applicant groups</h2>', table_html(subgroup_display),
  '<p>Applicant groups retain the prior definitions: school-ending versus voluntary origin, crossed with prioritario status. These separate logits can have different utility-error scales, so compare signs and within-group patterns cautiously.</p>',
  '<h2>Sample coverage</h2>', table_html(flow_display),
  '<p>The parent-education measure is available for ', fmt_n(coverage[[2L]]), ' of ', fmt_n(coverage[[3L]]),
  ' destination schools in the PAES-M1 sample. If any school in a submitted list lacks a mean, the entire ranking is excluded.</p>',
  '<h2>Survey construction</h2>', table_html(normalization_display),
  '<p>For each student, I average the available mother and father education-year reports. I then average these family means across 2nd-year high-school SIMCE respondents at each RBD. One-school SD = ',
  f2(norm$reference_sd_years), ' years, with each school receiving equal weight in normalization. Unknown, blank, and double-marked responses are missing; when only one parent response is valid, it contributes the family mean.</p>',
  '<p>The two SIMCE items ask how far the student’s mother (or father’s partner) and father (or mother’s partner) advanced in education. Year conversions follow the response categories; tertiary categories are approximate years.</p>',
  '<h2>Estimation sample and fit</h2>', table_html(sample_display),
  '<p>The extended model enters all four paper EB VAs, 2025-process regular PAES M1 school mean, 2022 mean parent education, log(1 + distance), two school-type indicators and interactions of each of the six school attributes with mixed HC/TP and TP-only type. A matched five-attribute model uses the same retained rankings to show how other coefficients move when parent education is included. The submitted-set Plackett–Luce likelihood is fit by its exact stratified Cox representation; standard errors cluster by student.</p>',
  '<details><summary>Raw extended coefficients (pooled main-distance model)</summary>',
    table_html(co[group == "pooled" & variant == "extended" & model == "main", .(Term = term, Estimate = f4(estimate), `Clustered SE` = f4(std_error), `p value` = formatC(p_value, digits = 4, format = "f"))]), '</details>',
  '<details><summary>Change in existing VA and PAES slopes after adding parent education</summary>',
    table_html(change[group == "pooled", .(School_type = school_type, Attribute = label, `Five attributes` = cell(baseline, baseline_se), `With parent education` = cell(extended, extended_se), `Change` = f4(change))]), '</details>',
  '<h2>Files</h2><ul><li><a href="../../tables/school_preference_va/parent_education_2022/pooled_total_slopes.csv">Pooled total slopes (CSV)</a></li>',
  '<li><a href="../../tables/school_preference_va/parent_education_2022/pooled_total_slopes.tex">Pooled total slopes (LaTeX fragment)</a></li>',
  '<li><a href="../../tables/school_preference_va/parent_education_2022/parent_education_slopes_by_group.csv">Parent-education slopes by group (CSV)</a></li>',
  '<li><a href="../../tables/school_preference_va/parent_education_2022/existing_attribute_slope_changes.csv">Existing-slope changes (CSV)</a></li>',
  '<li><a href="../../tables/school_preference_va/parent_education_2022/coverage.csv">Destination coverage (CSV)</a></li></ul>',
  '<p>Reproducible scripts: 19_build_parent_education_2022.R, 20_fit_parent_education_2022.R, 22_verify_parent_education_2022.R and 21_report_parent_education_2022.R.</p>',
  '</main></body></html>')
writeLines(html, file.path(report_dir, "parent_education_2022.html"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "report_session_info.txt"))
fwrite(checks, file.path(clean_dir, "report_verification.csv"))
message("Saved parent-education report; all ", nrow(checks), " report checks passed.")
