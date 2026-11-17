# Summarize estimates and create a concise HTML report; no model fits here.
source("code/codex/school_preference_va/parent_income_common.R")
read_clean <- function(n) fread(file.path(clean_dir, n))
co <- read_clean("coefficients.csv"); sl <- read_clean("type_specific_slopes.csv")
ms <- read_clean("model_summary.csv"); flow <- read_clean("sample_flow.csv")
norm <- read_clean("parent_income_normalization.csv"); schools <- read_clean("school_parent_income_decile_4to.csv")
build_checks <- read_clean("build_verification.csv"); fit_checks <- read_clean("fit_verification.csv")
independent_checks <- read_clean("verification.csv")
check("Twenty models and every fit/construction/independent check pass", nrow(ms) == 20L &&
  all(build_checks$passed) && all(fit_checks$passed) && all(independent_checks$passed) &&
  !anyDuplicated(ms[, .(group, variant, model)]))
check("All estimates and standard errors are finite", all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0) && all(is.finite(sl$estimate)) &&
  all(is.finite(sl$std_error) & sl$std_error > 0))
f4 <- function(x) formatC(x, digits = 4L, format = "f", big.mark = ",")
f2 <- function(x) formatC(x, digits = 2L, format = "f", big.mark = ",")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
pct <- function(x) paste0(formatC(100 * x, digits = 2L, format = "f"), "%")
cell <- function(b, s) paste0(f4(b), " (", f4(s), ")")
main <- sl[group == "pooled" & variant == "extended" & model == "main"]
muni <- sl[group == "pooled" & variant == "extended" & model == "municipality"]
pooled <- copy(main); pooled[, published_coordinates := cell(estimate, std_error)]
mm <- muni[match(paste(pooled$school_type, pooled$term), paste(school_type, term))]
pooled[, comuna_centroids := cell(mm$estimate, mm$std_error)]
fwrite(pooled, file.path(table_dir, "pooled_total_slopes.csv"))
income <- sl[variant == "extended" & term == "z_parent_income_decile"]
income_main <- income[model == "main"]; income_muni <- income[model == "municipality"]
im <- income_muni[match(paste(income_main$group, income_main$school_type), paste(group, school_type))]
income_main[, `Published coordinates` := cell(estimate, std_error)]
income_main[, `Comuna centroids` := cell(im$estimate, im$std_error)]
income_main[, Group := group_labels[group]]
income_display <- income_main[, .(Group, `School type` = school_type,
  `Income-decile slope per SD` = `Published coordinates`, `Comuna-distance robustness` = `Comuna centroids`,
  `p value` = formatC(p_value, digits = 3L, format = "f"))]
fwrite(income_main, file.path(table_dir, "income_decile_slopes_by_group.csv"))
parent <- sl[group == "pooled" & variant == "extended" & model == "main" &
  term %in% c("z_parent_education", "z_parent_income_decile")]
parent[, `Estimate (clustered SE)` := cell(estimate, std_error)]
parent_display <- parent[, .(`School type` = school_type, Attribute = label, `Estimate (clustered SE)`,
  `p value` = formatC(p_value, digits = 3L, format = "f"))]
fwrite(parent, file.path(table_dir, "parent_characteristic_slopes.csv"))
baseline <- sl[variant == "matched_baseline" & model == "main",
  .(group, school_type, term, outcome, label, baseline = estimate, baseline_se = std_error)]
extended <- sl[variant == "extended" & model == "main" & term %in% base_parent_education_specs$standardized,
  .(group, school_type, term, outcome, extended = estimate, extended_se = std_error)]
change <- merge(baseline, extended, by = c("group", "school_type", "term", "outcome"), all = TRUE)
change[, change := extended - baseline]; fwrite(change, file.path(table_dir, "existing_attribute_slope_changes.csv"))
samples <- ms[variant == "extended" & model == "main"]
sample_display <- samples[, .(Group = group_labels[group], Applicants = fmt_n(applicants),
  `Student-school pairs` = fmt_n(applicant_school_pairs), `Destination schools` = fmt_n(schools),
  `Log likelihood` = f4(log_likelihood))]
flow[, retained_share := income_decile_estimation_applicants / parent_education_applicants]
flow_display <- flow[, .(Group = label, `Parent-education sample` = fmt_n(parent_education_applicants),
  `Whole lists excluded` = fmt_n(excluded_missing_income_decile), `Income model sample` = fmt_n(income_decile_estimation_applicants),
  `Share retained` = pct(retained_share))]
destination <- unique(as.data.table(readRDS(parent_income_input_path))[, .(rbd)])
coverage <- merge(destination, schools[, .(rbd, income_decile_mean, n_students_income_composition,
  income_imputed_share)], by = "rbd", all.x = TRUE)
coverage_table <- data.table(Measure = c("Destination schools", "Schools with existing income-decile mean",
  "Mean students per source-composition school", "Share of destination schools with any income imputation"),
  Value = c(fmt_n(nrow(destination)), fmt_n(sum(is.finite(coverage$income_decile_mean))),
    f2(mean(coverage$n_students_income_composition, na.rm = TRUE)), pct(mean(coverage$income_imputed_share > 0, na.rm = TRUE))))
fwrite(coverage_table, file.path(table_dir, "coverage.csv"))
normalization <- data.table(Measure = c("Destination schools in normalization", "Unweighted mean school decile",
  "School-level SD", "Schools in source file", "Students per source school, mean"),
  Value = c(fmt_n(norm$reference_schools), f2(norm$reference_mean), f2(norm$reference_sd),
    fmt_n(nrow(schools)), f2(mean(schools$n_students_income_composition))))
table_html <- function(x) {
  heads <- paste0("<th>", esc_html(names(x)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(x)), function(i) paste0("<tr>", paste0("<td>",
    vapply(x[i], function(z) esc_html(as.character(z)), character(1)), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<div class='table-wrap'><table><thead><tr>", heads, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}
total_income <- income_main[group == "pooled"][match(c("Non-TP", "Mixed HC/TP", "TP only"), school_type)]
interpretation <- paste0("The total utility slope for a one-school-SD increase in mean baseline income decile is ",
  paste0(total_income$school_type, " ", f4(total_income$estimate), " (SE ", f4(total_income$std_error), ")", collapse = "; "),
  ". Each slope includes the school-type interaction and conditions on the other six school attributes, school type and distance.")
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Parent income decile in school preferences</title>',
  '<style>body{font:16px/1.55 system-ui;color:#172536;margin:30px}main{max-width:1280px;margin:auto}table{border-collapse:collapse;width:100%;font-size:14px}td,th{padding:8px;border-bottom:1px solid #d9e0e8;text-align:right}td:first-child,th:first-child{text-align:left}.table-wrap{overflow:auto;margin:14px 0 28px}.note{background:#f3f6fa;padding:14px;border-radius:8px}details{margin:22px 0}</style></head><body><main>',
  '<h1>Do families value school effectiveness?</h1><p>Preference models adding the existing grade-4-SIMCE income decile</p>',
  '<p class="note"><b>Income-decile results:</b> ', esc_html(interpretation), ' The coefficients are conditional associations, not causal effects of peers.</p>',
  '<h2>Joint model: total attribute slopes by school type</h2>', table_html(pooled[, .(School_type = school_type, Attribute = label,
    `Published student coordinates` = published_coordinates, `Comuna-centroid robustness` = comuna_centroids)]),
  '<p>Entries are utility slopes per one destination-school SD; parentheses contain student-clustered standard errors. Both columns use the same lists.</p>',
  '<h2>Income-decile slopes across applicant groups</h2>', table_html(income_display),
  '<p>Groups cross school-ending versus voluntary origin with prioritario status. Separate logits can have different utility-error scales; compare within-group patterns cautiously.</p>',
  '<h2>Sample retention</h2>', table_html(flow_display), table_html(coverage_table),
  '<h2>Income measure</h2>', table_html(normalization),
  '<p>We reuse the existing mean school income decile from grade-4 SIMCE parent-survey data. It averages the established observed/imputed student income_decile_imputed measure among 2017–2020 grade-8 students attending each RBD. School means are standardized across destination schools with equal school weights. This is a later student-composition measure and was not necessarily known to families when they applied.</p>',
  '<h2>Estimation sample</h2>', table_html(sample_display),
  '<p>The extended model includes four EB VAs, PAES M1, parent education, income decile, log(1 + distance), type indicators and interactions of each of the seven school attributes with Mixed HC/TP and TP-only. The matched model omits income decile and uses the same applicants. Submitted rankings are modeled by the exact stratified Cox representation of Plackett–Luce, with student-clustered SEs.</p>',
  '<details><summary>All extended pooled coefficients</summary>', table_html(co[group == "pooled" & variant == "extended" & model == "main",
    .(Term = term, Estimate = f4(estimate), `Clustered SE` = f4(std_error), `p value` = formatC(p_value, digits = 4, format = "f"))]), '</details>',
  '<details><summary>How existing attributes move after adding income decile</summary>', table_html(change[group == "pooled",
    .(School_type = school_type, Attribute = label, `Without income decile` = cell(baseline, baseline_se),
      `With income decile` = cell(extended, extended_se), Change = f4(change))]), '</details>',
  '<p>Reproducible scripts: 23_build_parent_income_decile_4to.R, 24_fit_parent_income_decile_4to.R, 26_verify_parent_income_decile_4to.R and 25_report_parent_income_decile_4to.R.</p></main></body></html>')
writeLines(html, file.path(report_dir, "parent_income_decile_4to.html"), useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "report_session_info.txt"))
fwrite(checks, file.path(clean_dir, "report_verification.csv"))
message("Saved income-decile report; all ", nrow(checks), " report checks passed.")
