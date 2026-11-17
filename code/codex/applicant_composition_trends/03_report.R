suppressPackageStartupMessages({library(data.table); library(ggplot2); library(jsonlite)})
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
clean <- "data/clean/applicant_composition_trends"
figdir <- "output/figures/applicant_composition_trends"
reportdir <- "output/reports/applicant_composition_trends"
for (p in c(figdir, reportdir)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
cells <- fread(file.path(clean, "school_cohort_means_long.csv"))
trends <- fread(file.path(clean, "school_trend_tests.csv"))
summary <- fread(file.path(clean, "trend_summary.csv"))
changes <- fread(file.path(clean, "endpoint_change_summary.csv"))
corr <- fread(file.path(clean, "cross_cohort_school_correlations.csv"))
coverage <- fread(file.path(clean, "coverage_summary.csv"))
overlap <- fread(file.path(clean, "significant_trend_count_summary.csv"))
top <- fread(file.path(clean, "top100_schools.csv"))
specs <- fread(file.path(clean, "outcome_definitions.csv"))
main_outcomes <- specs[primary == TRUE, outcome]
balanced <- top[applicant_cohorts == 3L, rbd]
annual <- cells[rbd %in% balanced & n_observed > 0L,
  .(mean_across_schools = mean(mean), median_across_schools = median(mean), schools = .N),
  by = .(outcome, label, scale, unit, sae_proceso)]
annual[, display_mean := mean_across_schools * scale]
fwrite(annual, file.path(clean, "balanced_school_annual_means.csv"))

theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold")))
annual[, facet_label := paste0(label, "\n(", unit, ")")]
p <- ggplot(annual[outcome %in% main_outcomes], aes(sae_proceso, display_mean)) +
  geom_line(linewidth = .9, color = "#176b79") + geom_point(size = 2.7, color = "#176b79") +
  facet_wrap(~ facet_label, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = 2018:2020) + labs(x = "SAE application year", y = NULL,
    title = "Applicant composition over time",
    subtitle = "Fixed set of 86 schools observed in all three cohorts; equal weight per school",
    caption = "SIMCE averages math/verbal grade-4 z-scores. Parent education uses reported values. Panel axes differ.")
ggsave(file.path(figdir, "balanced_school_annual_means.png"), p, width = 13, height = 4.6, dpi = 180)

endpoint <- dcast(cells[outcome %in% main_outcomes & rbd %in% balanced],
  rbd + outcome + label + scale ~ sae_proceso, value.var = "mean")
endpoint[, `:=`(x = get("2018") * scale, y = get("2020") * scale)]
p <- ggplot(endpoint, aes(x, y)) + geom_abline(slope = 1, intercept = 0, color = "#9c9c9c", linetype = 2) +
  geom_point(color = "#176b79", alpha = .75, size = 1.7) + facet_wrap(~ label, scales = "free", nrow = 1) +
  labs(x = "2018 school mean", y = "2020 school mean", title = "Do schools retain similar applicant populations?",
       subtitle = "86 schools; dashed line is no change (female share in percentage points)")
ggsave(file.path(figdir, "school_means_2018_vs_2020.png"), p, width = 13, height = 4.5, dpi = 180)

gallery <- character()
for (v in main_outcomes) {
  sp <- specs[outcome == v]
  images <- character()
  for (page in 1:4) {
    schools <- top[rank > (page - 1L) * 25L & rank <= page * 25L]
    x <- cells[outcome == v & rbd %in% schools$rbd & n_observed > 0L]
    x[, school_label := paste0(rank, ". RBD ", rbd, "\n", substr(school_name, 1, 30))]
    x[, school_label := factor(school_label, levels = unique(x[order(rank), school_label]))]
    p <- ggplot(x, aes(sae_proceso, display_mean, group = rbd)) +
      geom_line(color = "#176b79", linewidth = .65) + geom_point(color = "#176b79", size = 1.5) +
      facet_wrap(~ school_label, ncol = 5, scales = "free_y") + scale_x_continuous(breaks = 2018:2020) +
      labs(x = "SAE application year", y = sp$unit,
        title = paste0(sp$label, ": schools ", (page - 1L) * 25L + 1L, "-", page * 25L),
        subtitle = "Each panel has its own vertical scale. A missing year means no applicants in the selected sample.") +
      theme(strip.text = element_text(size = 8), axis.text = element_text(size = 8), panel.spacing = grid::unit(1, "lines"))
    fn <- paste0(v, "_schools_", page, ".png")
    ggsave(file.path(figdir, fn), p, width = 15, height = 13, dpi = 150)
    images <- c(images, paste0('<img loading="lazy" src="../../figures/applicant_composition_trends/', fn, '" alt="School cohort means">'))
  }
  gallery <- c(gallery, paste0('<details><summary>', sp$label, ': all 100 school trajectories</summary>', paste(images, collapse = ""), '</details>'))
}

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
fmt <- function(x, digits = 3L) ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
table_html <- function(d) paste0('<div class="table"><table><thead><tr>',
  paste0('<th>', esc(names(d)), '</th>', collapse = ''), '</tr></thead><tbody>',
  paste(vapply(seq_len(nrow(d)), function(i) paste0('<tr>', paste0('<td>', esc(as.character(unlist(d[i]))), '</td>', collapse = ''), '</tr>'), character(1)), collapse = ''),
  '</tbody></table></div>')
sm <- summary[match(main_outcomes, outcome)]
cs <- changes[outcome %in% main_outcomes & first_cohort == 2018L]
cs <- cs[match(main_outcomes, outcome)]
rr <- corr[outcome %in% main_outcomes & year_from == 2018L & year_to == 2020L]
rr <- rr[match(main_outcomes, outcome)]
overview <- data.table(Measure = sm$label,
  `2018-2020 school correlation` = fmt(rr$pearson),
  `Median absolute 2018-2020 change` = paste(fmt(cs$median_absolute_change), sm$unit),
  `Cohort OLS: p < .05 / testable` = paste0(sm$cohort_ols_p_lt_05, " / ", sm$cohort_ols_testable),
  `Independent applicants: p < .05 / testable` = paste0(sm$independent_applicant_p_lt_05, " / ", sm$independent_applicant_testable))
cov <- coverage[outcome %in% main_outcomes, .(Measure = label, Year = sae_proceso,
  `Observed applicant-school pairs (%)` = fmt(100 * coverage, 1L),
  `Lowest school coverage (%)` = fmt(100 * minimum_school_coverage, 1L))]
overlap_display <- overlap[, .(Test = ifelse(method == "cohort_ols_p", "Cohort means OLS", "Independent applicants HC2"),
  `Schools with a test` = schools_with_any_test, `At least one` = at_least_one,
  `Exactly one` = exactly_one, `Exactly two` = exactly_two, `All three` = all_three,
  `At least two` = at_least_two, `No test available` = no_testable_outcomes)]

# Compact aggregate-only payload for browsing every school and every outcome.
payload <- trends[, .(rank, rbd, school_name, comuna, outcome, label, unit, scale,
  n_cohorts, n_students, first_cohort, last_cohort, first_mean, last_mean,
  display_slope, display_change, cohort_ols_se, cohort_ols_p, cohort_ols_ci_low, cohort_ols_ci_high,
  cohort_ols_status, independent_applicant_hc2_se, independent_applicant_p,
  independent_applicant_ci_low, independent_applicant_ci_high, min_coverage)]
js_data <- as.character(toJSON(payload, dataframe = "rows", na = "null", digits = 12))
js_data <- gsub("</", "<\\/", js_data, fixed = TRUE)
opts <- paste0('<option value="', specs$outcome, '">', esc(specs$label), '</option>', collapse = '')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>SAE applicant composition trends</title><style>',
  'body{font:15px/1.55 system-ui,sans-serif;color:#213344;background:#f3f6f8;margin:0}main{max-width:1400px;margin:24px auto;background:white;padding:32px;border-radius:12px}h1{font-size:30px;margin-bottom:8px}h2{margin-top:30px;font-size:22px}p{max-width:1100px}.meta{color:#5e6a76}.note{background:#eef5f5;border-left:4px solid #176b79;padding:12px 18px}.table{overflow:auto;max-height:700px}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px 10px;border-bottom:1px solid #e0e6ea;text-align:right}th:first-child,td:first-child{text-align:left}th{background:#edf3f5;position:sticky;top:0;z-index:1}td{white-space:nowrap}img{max-width:100%;height:auto;margin:15px 0}select,input{font:inherit;padding:8px;border:1px solid #a8b8c2;border-radius:5px;margin:5px 12px 12px 0}summary{cursor:pointer;font-weight:600;padding:13px;background:#edf3f5;margin-top:10px}a{color:#126472}.sig{font-weight:700;color:#9e4131}.small{font-size:13px}code{background:#eef2f5;padding:2px 4px}</style></head><body><main>',
  '<h1>SAE applicant composition trends</h1><p class="meta">Grade-9 applicants | SAE processes 2018-2020 | Top 100 schools by submitted applications | 22 October 2026</p>',
  '<p>The fixed school set contains <b>212,608 applicant-school pairs</b>. Each student contributes once to each school listed, irrespective of preference rank or admission priority. The same student can appear at several schools. There are <b>86 schools with three cohorts and 14 with two</b>. All p-values are unadjusted, as requested.</p>',
  '<h2>How many schools have trends in one, two, or all three outcomes?</h2><p>Count the three primary outcomes (baseline SIMCE, female share and parents\' education) with an unadjusted p-value below .05 in each school.</p>',
  table_html(overlap_display),
  '<p>The cohort-mean regressions detect at least one trend in <b>8 of 86 schools</b>: exactly one in each school, and none in two or three outcomes. Fourteen schools have only two cohorts and cannot be tested this way; they are not counted as having zero trends. The independent-applicant tests detect at least one trend in <b>81 of 100 schools</b>: exactly one in 56, exactly two in 23, and all three in 2. The two specifications estimate identical slopes but use different uncertainty assumptions, explained below.</p>',
  '<h2>What changes, and what stays similar?</h2><p>Schools retain similar relative applicant composition: the 2018-2020 correlations are 0.899 for baseline SIMCE, 0.978 for female share, and 0.968 for parents\' education. Relative stability does not imply unchanged levels. Among the 86 schools present in all three years, median reported parental education falls by 0.437 years from 2018 to 2020.</p>',
  table_html(overview),
  '<p class="small">Endpoint changes and correlations use the same 86 schools observed in 2018 and 2020. Test counts use all schools where the respective test is defined. Constant outcomes have no estimated sampling variance and are not assigned artificial p-values.</p>',
  '<img src="../../figures/applicant_composition_trends/balanced_school_annual_means.png" alt="Annual mean composition for the fixed set of 86 schools">',
  '<img loading="lazy" src="../../figures/applicant_composition_trends/school_means_2018_vs_2020.png" alt="2018 versus 2020 school composition">',
  '<h2>How to read the trend tests</h2><p>The coefficient is the annual change from the school-specific regression <code>cohort mean = intercept + slope x (SAE year - 2018)</code>, giving each observed cohort equal weight. It uses two or three cohort means, with no controls. With three consecutive years, the slope is exactly half the 2018-2020 change. Female trends are displayed in percentage points per year; parental education in years per year.</p>',
  '<div class="note"><b>Cohort OLS</b> uses the ordinary regression standard error and a t test with G - 2 residual degrees of freedom. Three cohorts leave only one degree of freedom. Two cohorts identify a change but cannot support this residual-based trend test. A large p-value is not evidence that composition is stable.<br><br>',
  '<b>Independent-applicant test</b> fits the individual outcome on year, with weight 1 / observed applicants in that school-cohort. This produces exactly the same slope as the cohort regression. HC2 regression standard errors retain individual residual variation; p-values use t(N - 2). These tests assume independent applicants within each school and do not allow common cohort shocks or origin-school dependence. They can detect changes between two cohorts, but are a different uncertainty model. They should not be read as evidence of a persistent trend beyond these years.</div>',
  '<h2>Every school and outcome</h2><p>Select a measure and search by school, RBD or comuna. The full CSV includes both sets of standard errors and confidence intervals.</p>',
  '<label>Measure <select id="outcome">', opts, '</select></label><label>School <input id="search" placeholder="Name, RBD or comuna"></label><span id="count"></span>',
  '<div class="table"><table><thead><tr><th>Rank / school</th><th>Years</th><th>Observed N</th><th>First mean</th><th>Last mean</th><th>Annual slope</th><th>Cohort OLS p</th><th>Applicant HC2 p</th><th>Min. coverage</th></tr></thead><tbody id="rows"></tbody></table></div>',
  '<p class="small">The CSV stores female means/slopes in share units; display columns multiply them by 100. Empty cohort OLS p-values reflect two cohorts or constant means. Bold p-values are below .05; no multiplicity adjustment is applied.</p>',
  '<h2>School trajectories</h2>', paste(gallery, collapse = ''),
  '<h2>Definitions and coverage</h2><p><b>Baseline SIMCE:</b> mean of the applicant\'s grade-4 math and verbal z-scores, requiring both. Existing z-scores standardize each subject within its SIMCE test year; this measures relative baseline achievement, not national raw-score trends. The appendix includes raw test-point averages and the separate standardized subjects.</p>',
  '<p><b>Female share:</b> GEN_ALU = 2 among valid gender codes 1 and 2. <b>Parents\' education:</b> mean of the available mother/father education years for each applicant, followed by an equal-weight average across applicants. One reported parent is sufficient. These are the project\'s already harmonized grade-4 questionnaire categories expressed as approximate schooling years. Appendix results use each parent separately, require both reported parents, and use the existing observed-plus-imputed parent measures.</p>',
  table_html(cov),
  '<p>Gender is complete. SIMCE and parent-survey coverage fall in the 2020 cohort, and the parent-education questionnaire differs across underlying baseline years (mostly 2014, 2015 and 2016). Thus the observed parent decline should be checked against measurement and missingness before treating it as a change in the full applicant population. The decline remains under the both-parent and existing-imputation definitions; those checks do not establish that missingness is ignorable.</p>',
  '<p><b>Population:</b> the existing 294,198-person paper roster of timely SAE grade-9 applicants in 2018-2020, age 12-16 at grade 8 and with an observed positive high-school RBD. This is not the full all-grade SAE system. No restriction to exam takers, lottery-risk applicants, or eventual attendees at the destination is imposed. Cohort denotes the application process year, which precedes grade-9 entry by one year.</p>',
  '<p><b>School selection:</b> rank pooled distinct applicant-school pairs from the raw regular-round C1 files after excluding <code>agregada_por_continuidad = 1</code>. Priority applications remain included, and multiple program/campus entries at one RBD are collapsed. Ties break by RBD. This retains 99 of the earlier top 100: RBD 4977 replaces RBD 52. Selection is fixed across years; missing school-cohort cells are empty, never zero-filled outcomes.</p>',
  '<h2>Downloads and reproducibility</h2><ul>',
  '<li><a href="../../../data/clean/applicant_composition_trends/school_cohort_panel.csv">School-cohort panel: means, counts and coverage</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/primary_school_trend_tests.csv">300 primary school-outcome trend records</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/school_trend_tests.csv">All 1,000 school-outcome trend records, including components and sensitivity checks</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/trend_summary.csv">Summary of unadjusted tests</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/school_significant_trend_counts.csv">Per-school counts and identities of significant outcomes</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/significant_trend_count_summary.csv">Counts of schools with one, two or three significant trends</a></li>',
  '<li><a href="../../../data/clean/applicant_composition_trends/input_manifest.csv">Input manifest</a></li>',
  '<li><a href="../../../code/codex/applicant_composition_trends/README.md">Definitions, run instructions and verification</a></li></ul>',
  '<script>const data=', js_data, ';const f=(x,d=3)=>x===null?"--":Number(x).toFixed(d);const e=s=>String(s).replace(/[&<>"\x27]/g,c=>"&#"+c.charCodeAt(0)+";");',
  'function draw(){const v=document.getElementById("outcome").value;const q=document.getElementById("search").value.toLowerCase();const a=data.filter(d=>d.outcome===v&&[d.school_name,d.rbd,d.comuna].join(" ").toLowerCase().includes(q));document.getElementById("count").textContent=a.length+" schools";document.getElementById("rows").innerHTML=a.map(d=>`<tr><td>${d.rank}. ${e(d.school_name)}<br><small>RBD ${d.rbd} | ${e(d.comuna)}</small></td><td>${d.first_cohort}-${d.last_cohort}</td><td>${d.n_students.toLocaleString()}</td><td>${f(d.first_mean*d.scale)}</td><td>${f(d.last_mean*d.scale)}</td><td>${f(d.display_slope)}</td><td title="${e(d.cohort_ols_status)}" class="${d.cohort_ols_p!==null&&d.cohort_ols_p<.05?"sig":""}">${f(d.cohort_ols_p,4)}</td><td class="${d.independent_applicant_p!==null&&d.independent_applicant_p<.05?"sig":""}">${f(d.independent_applicant_p,4)}</td><td>${f(100*d.min_coverage,1)}%</td></tr>`).join("");}document.getElementById("outcome").addEventListener("change",draw);document.getElementById("search").addEventListener("input",draw);draw();</script>',
  '</main></body></html>')
writeLines(html, file.path(reportdir, "report.html"), useBytes = TRUE)
message("Report and 14 figures written.")
