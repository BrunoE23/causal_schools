suppressPackageStartupMessages({library(data.table); library(ggplot2); library(jsonlite)})
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
base <- "data/clean/applicant_composition_trends"
dest <- file.path(base, "probability_weighted")
figdir <- "output/figures/applicant_composition_trends/probability_weighted"
reportdir <- "output/reports/applicant_composition_trends"
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
specs <- fread(file.path(dest, "outcome_definitions.csv"), encoding = "UTF-8")
main_outcomes <- specs[primary == TRUE, outcome]
cells <- fread(file.path(dest, "school_cohort_means_long.csv"), encoding = "UTF-8")
old_cells <- fread(file.path(base, "school_cohort_means_long.csv"), encoding = "UTF-8")
trends <- fread(file.path(dest, "school_trend_tests.csv"), encoding = "UTF-8")
old_trends <- fread(file.path(base, "school_trend_tests.csv"), encoding = "UTF-8")
comparison <- merge(trends, old_trends[, .(rbd, outcome, unweighted_slope = display_slope,
  unweighted_cohort_p = cohort_ols_p, unweighted_applicant_p = independent_applicant_p,
  unweighted_n_cohorts = n_cohorts)], by = c("rbd", "outcome"), all.x = TRUE)
setorder(comparison, rank, outcome)
fwrite(comparison, file.path(dest, "weighted_vs_unweighted_school_tests.csv"))
count_summary <- rbindlist(lapply(c("Unweighted", "Probability weighted"), function(version) {
  dir <- if (version == "Unweighted") base else dest
  fread(file.path(dir, "significant_trend_count_summary.csv"))[, weighting := version]
}))
fwrite(count_summary, file.path(dest, "weighted_vs_unweighted_count_summary.csv"))
audit <- fread(file.path(dest, "probability_linkage_audit.csv"))
coverage <- fread(file.path(dest, "weighted_coverage_summary.csv"))
outcome_summary <- fread(file.path(dest, "trend_summary.csv"))
top <- fread(file.path(dest, "top100_schools.csv"), encoding = "UTF-8")

# Compare levels using the same schools and three supported years for all measures.
balanced <- cells[primary == TRUE, .(n = sum(is.finite(mean))), by = .(rbd, outcome)][,
  .(complete = all(n == 3L)), by = rbd][complete == TRUE, rbd]
annual_cells <- rbindlist(list(
  cells[rbd %in% balanced & primary == TRUE, .(rbd, sae_proceso, outcome, label, scale, unit, mean, weighting = "Probability weighted")],
  old_cells[rbd %in% balanced & primary == TRUE, .(rbd, sae_proceso, outcome, label, scale, unit, mean, weighting = "Unweighted")]))
annual <- annual_cells[, .(school_mean = mean(mean * scale), schools = .N),
  by = .(sae_proceso, outcome, label, unit, weighting)]
fwrite(annual, file.path(dest, "matched_school_annual_means.csv"))
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(),
  strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"), legend.position = "bottom"))
annual[, panel := paste0(label, "\n(", unit, ")")]
p <- ggplot(annual, aes(sae_proceso, school_mean, color = weighting, linetype = weighting)) +
  geom_line(linewidth = .85) + geom_point(size = 2.4) + facet_wrap(~panel, nrow = 1, scales = "free_y") +
  scale_color_manual(values = c("Probability weighted" = "#b55c26", "Unweighted" = "#176b79")) +
  scale_x_continuous(breaks = 2018:2020) + labs(x = "SAE application year", y = NULL, color = NULL, linetype = NULL,
    title = "Applicant composition, with and without admission-probability weights",
    subtitle = paste0("Same ", length(balanced), " schools with all three cohorts supported for every primary outcome; equal school weights"))
ggsave(file.path(figdir, "matched_school_annual_means.png"), p, width = 13, height = 5, dpi = 170)
p <- ggplot(comparison[primary == TRUE & is.finite(display_slope)], aes(unweighted_slope, display_slope)) +
  geom_hline(yintercept = 0, color = "#cccccc") + geom_vline(xintercept = 0, color = "#cccccc") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#777777") +
  geom_point(aes(color = min_cell_effective_n < 10), alpha = .8, size = 1.8) +
  facet_wrap(~label, scales = "free", nrow = 1) +
  scale_color_manual(values = c("FALSE" = "#176b79", "TRUE" = "#b55c26"), labels = c("No", "Yes")) +
  labs(x = "Unweighted annual slope", y = "Probability-weighted annual slope", color = "Any supported cell with effective N < 10?",
    title = "School-specific annual trends",
    subtitle = "Same schools; supported year windows can differ. Female slopes are in percentage points per year.")
ggsave(file.path(figdir, "weighted_vs_unweighted_slopes.png"), p, width = 13, height = 5, dpi = 170)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
fmt <- function(x, digits = 3L) ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
table_html <- function(d) paste0('<div class="table"><table><thead><tr>',
  paste0('<th>', esc(names(d)), '</th>', collapse = ''), '</tr></thead><tbody>',
  paste(vapply(seq_len(nrow(d)), function(i) paste0('<tr>', paste0('<td>', esc(as.character(unlist(d[i]))), '</td>', collapse = ''), '</tr>'), character(1)), collapse = ''),
  '</tbody></table></div>')
count_display <- count_summary[, .(Weights = weighting,
  Test = ifelse(method == "cohort_ols_p", "Cohort means OLS", "Independent applicants HC2"),
  `Schools with a test` = schools_with_any_test, `At least one` = at_least_one,
  `Exactly one` = exactly_one, `Exactly two` = exactly_two, `All three` = all_three,
  `At least two` = at_least_two, `No test available` = no_testable_outcomes)]
outcome_display <- outcome_summary[match(main_outcomes, outcome), .(Outcome = label,
  `Cohort OLS: p < .05 / testable` = paste0(cohort_ols_p_lt_05, " / ", cohort_ols_testable),
  `Applicant HC2: p < .05 / testable` = paste0(independent_applicant_p_lt_05, " / ", independent_applicant_testable),
  `Median annual slope` = fmt(median_annual_slope), Unit = unit)]
coverage_display <- coverage[outcome %in% main_outcomes, .(Outcome = label, Year = sae_proceso,
  `Observed probability mass (%)` = fmt(100 * weighted_coverage, 1),
  `Median effective N` = fmt(median_effective_n, 1), `Minimum effective N` = fmt(minimum_effective_n, 1))]
support <- cells[primary == TRUE & n_applicants > 0L & (observed_probability_mass == 0 | effective_n < 10),
  .(Rank = rank, RBD = rbd, School = school_name, Year = sae_proceso, Outcome = label,
    `Applicants` = n_applicants, `Positive-weight observed N` = n_observed,
    `Observed probability mass` = fmt(observed_probability_mass), `Effective N` = fmt(effective_n, 1))]
payload <- comparison[, .(rank, rbd, school_name, comuna, outcome, label, scale, unit, n_cohorts,
  n_students, display_slope, unweighted_slope, cohort_ols_p, independent_applicant_p,
  unweighted_cohort_p, unweighted_applicant_p, cohort_ols_status, independent_applicant_status,
  min_cell_effective_n, min_coverage)]
js_data <- gsub("</", "<\\/", as.character(toJSON(payload, dataframe = "rows", na = "null", digits = 12)), fixed = TRUE)
opts <- paste0('<option value="', specs$outcome, '">', esc(specs$label), '</option>', collapse = '')
cohort_count <- count_summary[weighting == "Probability weighted" & method == "cohort_ols_p"]
student_count <- count_summary[weighting == "Probability weighted" & method == "independent_applicant_p"]
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Admission-probability-weighted applicant composition</title><style>',
  'body{font:15px/1.55 system-ui,sans-serif;color:#213344;background:#f3f6f8;margin:0}main{max-width:1450px;margin:24px auto;background:white;padding:32px;border-radius:12px}h1{font-size:30px}h2{margin-top:30px;font-size:22px}p{max-width:1150px}.meta{color:#5e6a76}.note{background:#fbf2e9;border-left:4px solid #b55c26;padding:12px 18px}.table{overflow:auto;max-height:650px}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px 10px;border-bottom:1px solid #e0e6ea;text-align:right}th:first-child,td:first-child{text-align:left}th{background:#edf3f5;position:sticky;top:0;z-index:1}td{white-space:nowrap}img{max-width:100%;height:auto;margin:15px 0}select,input{font:inherit;padding:8px;border:1px solid #a8b8c2;border-radius:5px;margin:5px 12px 12px 0}summary{cursor:pointer;font-weight:600;padding:13px;background:#edf3f5;margin-top:10px}a{color:#126472}.sig{font-weight:700;color:#9e4131}.small{font-size:13px}code{background:#eef2f5;padding:2px 4px}</style></head><body><main>',
  '<h1>Admission-probability-weighted applicant composition</h1><p class="meta">User-requested proxy | Same top 100 schools and SAE 2018-2020 applicants | 22 October 2026</p>',
  '<p>For each school, cohort and characteristic, calculate <code>weighted mean = sum(p_admission x X) / sum(p_admission)</code> among applicants with X observed. Probabilities are specific to the destination school, summed across its programs. This is a ratio of expected characteristic totals to expected admissions among observed applicants, used as a proxy for the composition of admitted students.</p>',
  '<h2>How many schools have significant trends?</h2><p>Raw two-sided p &lt; .05, with no multiple-testing adjustment. The three counted outcomes are average grade-4 SIMCE z-scores, female share, and reported parents\' schooling years.</p>',
  table_html(count_display),
  '<p>With probability weights, cohort-mean OLS detects at least one trend in <b>', cohort_count$at_least_one, ' of ', cohort_count$schools_with_any_test,
  ' testable schools</b>; all are in exactly one outcome. Independent-applicant HC2 detects at least one in <b>', student_count$at_least_one, ' of ', student_count$schools_with_any_test,
  '</b>: exactly one in ', student_count$exactly_one, ', exactly two in ', student_count$exactly_two, ', and all three in ', student_count$all_three, '.</p>',
  '<p>The school set is unchanged, but zero probability mass removes some cohort means, so available test counts and occasionally the year windows differ from the unweighted version. Schools without tests are not classified as having zero trends.</p>',
  '<h2>Which characteristics trend?</h2>', table_html(outcome_display),
  '<img src="../../figures/applicant_composition_trends/probability_weighted/matched_school_annual_means.png" alt="Weighted and unweighted cohort means for the same balanced school set">',
  '<img loading="lazy" src="../../figures/applicant_composition_trends/probability_weighted/weighted_vs_unweighted_slopes.png" alt="Weighted versus unweighted school trends">',
  '<h2>Every school and outcome</h2><label>Measure <select id="outcome">', opts, '</select></label><label>School <input id="search" placeholder="Name, RBD or comuna"></label><span id="count"></span>',
  '<div class="table"><table><thead><tr><th>Rank / school</th><th>Supported cohorts</th><th>Weighted slope</th><th>Unweighted slope</th><th>Weighted cohort p</th><th>Unweighted cohort p</th><th>Weighted HC2 p</th><th>Unweighted HC2 p</th><th>Min. effective N</th></tr></thead><tbody id="rows"></tbody></table></div>',
  '<p class="small">Slopes are per year. Female slopes are in percentage points. Effective N = (sum p)<sup>2</sup> / sum(p<sup>2</sup>) within school-cohort-outcome; it describes weight concentration and is not used as a regression degrees-of-freedom replacement. Bold p-values are below .05. Missing tests remain blank.</p>',
  '<h2>Probability linkage and missing characteristics</h2><p>All ', format(sum(audit$applicants), big.mark = ",", trim = TRUE), ' distinct applicants in the original top-school panel have a complete probability distribution summing to one, including the unmatched option. Of 212,608 listed applicant-school pairs, ',
  format(sum(audit$positive_probability_pairs), big.mark = ",", trim = TRUE), ' have positive probability and ', format(sum(audit$zero_probability_pairs), big.mark = ",", trim = TRUE),
  ' have zero probability. Absent school rows are treated as zero only after checking each applicant\'s complete distribution. Across all three cohorts, the listed-school weights sum to ', fmt(sum(audit$listed_top100_probability_mass), 2), ' expected admissions.</p>',
  '<p>Keep genuine submitted choices from the original panel. The simulation also has ', sum(audit$nonlisted_top100_probability_pairs), ' positive-probability top-school pairs absent from those submitted choices (total mass ', fmt(sum(audit$nonlisted_top100_probability_mass), 2),
  '); these are not added to the analysis. Program probabilities within the same RBD are summed before joining, so applicants are counted once per school.</p>',
  table_html(coverage_display),
  '<details><summary>Cells with no weighted mean or effective N below 10</summary>', table_html(support), '</details>',
  '<h2>Inference and interpretation</h2><div class="note"><b>Cohort OLS:</b> regress the weighted cohort mean on year, with equal weight for each supported cohort. Use the ordinary regression standard error and t(G - 2). Three cohorts leave one residual degree of freedom; two allow a slope but no residual-based test. With fewer than two supported cohorts no slope is reported.<br><br>',
  '<b>Independent applicants:</b> fit the individual characteristic on year with weight p / sum(p) within the observed school-cohort-outcome. This exactly reproduces the cohort-mean slope. Use regression HC2 standard errors and t(N_positive - 2), treating simulated probabilities as fixed. This ignores simulation uncertainty, assignment dependence, common cohort shocks and dependence by origin school. Zero-weight observations are excluded. Constant outcomes and undefined HC2 cases receive no artificial p-values.</div>',
  '<p>This proxy emphasizes applicants likely to be assigned to a school. It does not model offer take-up or actual enrollment, and a ratio of expected totals is not generally the expected realized mean when enrollment is random. Parent-education missingness and questionnaire comparability remain relevant. No new trimming, imputation, probability threshold or school reranking is introduced.</p>',
  '<h2>Files</h2><ul>',
  '<li><a href="../../../data/clean/applicant_composition_trends/probability_weighted/school_cohort_panel.csv">Weighted school-cohort panel, denominators and coverage</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/probability_weighted/weighted_vs_unweighted_school_tests.csv">All weighted and unweighted school tests</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/probability_weighted/school_significant_trend_counts.csv">School-level significant-outcome counts and identities</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/probability_weighted/weighted_vs_unweighted_count_summary.csv">Count comparison</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/probability_weighted/probability_linkage_audit.csv">Probability linkage audit</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/probability_weighted/verification.csv">Independent numerical verification</a></li>',
  '<li><a href="../../../code/codex/applicant_composition_trends/README.md">Reproduction instructions</a></li>',
  '<li><a href="../../../output/reports/applicant_composition_trends/report.html">Original unweighted report</a></li></ul>',
  '<script>const data=', js_data, ';const f=(x,d=3)=>x===null?"--":Number(x).toFixed(d);const e=s=>String(s).replace(/[&<>"\x27]/g,c=>"&#"+c.charCodeAt(0)+";");',
  'const cell=(p,status="")=>`<td title="${e(status)}" class="${p!==null&&p<.05?"sig":""}">${f(p,4)}</td>`;function draw(){const v=document.getElementById("outcome").value;const q=document.getElementById("search").value.toLowerCase();const a=data.filter(d=>d.outcome===v&&[d.school_name,d.rbd,d.comuna].join(" ").toLowerCase().includes(q));document.getElementById("count").textContent=a.length+" schools";document.getElementById("rows").innerHTML=a.map(d=>`<tr><td>${d.rank}. ${e(d.school_name)}<br><small>RBD ${d.rbd} | ${e(d.comuna)}</small></td><td>${d.n_cohorts}</td><td>${f(d.display_slope)}</td><td>${f(d.unweighted_slope)}</td>${cell(d.cohort_ols_p,d.cohort_ols_status)}${cell(d.unweighted_cohort_p)}${cell(d.independent_applicant_p,d.independent_applicant_status)}${cell(d.unweighted_applicant_p)}<td>${f(d.min_cell_effective_n,1)}</td></tr>`).join("");}document.getElementById("outcome").addEventListener("change",draw);document.getElementById("search").addEventListener("input",draw);draw();</script>',
  '</main></body></html>')
writeLines(html, file.path(reportdir, "probability_weighted.html"), useBytes = TRUE)
print(count_display)
message("Wrote probability-weighted report and comparison figures.")
