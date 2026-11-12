# Relate 2021-to-2025 SAE declared secondary capacity changes to saved school VA.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
task <- file.path(root, "data/clean/secondary_sae_spots")
out <- file.path(task, "va_correlations")
fig <- file.path(root, "output/figures/secondary_sae_spots")
report <- file.path(root, "output/reports/secondary_sae_spots")
for (p in c(out, fig, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
capacity_path <- file.path(task, "school_year_spots.csv")
va_path <- file.path(data_root, "data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
stopifnot(all(file.info(c(capacity_path, va_path))$size < 1e8))
c <- fread(capacity_path)
stopifnot(!anyDuplicated(c[, .(rbd, process_year)]), all(c$capacity >= 0))
va <- fread(va_path, select = c("outcome", "school_rbd", "analysis_sample",
  "controlled_value_added_eb_centered_student", "controlled_value_added_centered_student",
  "n_students_regression", "eb_reliability"))[analysis_sample == "All"]
setnames(va, c("school_rbd", "controlled_value_added_eb_centered_student", "controlled_value_added_centered_student"), c("rbd", "va_eb", "va_unshrunk"))
specs <- data.table(outcome = c("z_year_math_max", "admission_exam_taker", "high_paying_field_m1", "high_inst_m1",
  "z_year_leng_max", "higher_ed_enrolled_m1", "any_postulacion", "log_program_income_clp_m1"),
  label = c("Math achievement", "Exam taking", "High-premium field", "High-premium institution",
  "Language achievement", "Higher-ed enrollment", "Financial-aid application", "Log predicted program income"),
  primary = c(rep(TRUE, 4L), rep(FALSE, 4L)), ordering = 1:8)
stopifnot(all(specs$outcome %in% va$outcome), !anyDuplicated(va[, .(rbd, outcome)]))

endpoints <- function(start, end) {
  b <- c[process_year == start & capacity > 0, .(rbd, school_name, region, comuna, capacity_before = capacity)]
  a <- c[process_year == end, .(rbd, capacity_after = capacity, listed_after = TRUE)]
  z <- merge(b, a, by = "rbd", all.x = TRUE)
  z[is.na(listed_after), `:=`(listed_after = FALSE, capacity_after = 0)]
  z[, `:=`(start_year = start, end_year = end, delta_seats = capacity_after - capacity_before,
    pct_growth = 100 * (capacity_after / capacity_before - 1), exit_capacity = capacity_after == 0)]
  z[, log_growth := NA_real_]
  z[capacity_after > 0, log_growth := 100 * log(capacity_after / capacity_before)]
  stopifnot(nrow(z) == nrow(b), !anyDuplicated(z$rbd), !anyNA(z$capacity_after), all(is.finite(z$pct_growth)),
    sum(z$delta_seats) == sum(z$capacity_after) - sum(z$capacity_before))
  z
}
base <- endpoints(2021L, 2025L)
matched <- merge(base, va[outcome %in% specs$outcome], by = "rbd")
matched <- merge(matched, specs, by = "outcome")
stopifnot(!anyDuplicated(matched[, .(rbd, outcome)]), all(is.finite(matched$va_eb)), all(is.finite(matched$va_unshrunk)))
correlations <- function(x, sample, growth, va_col = "va_eb") {
  x <- x[is.finite(get(growth)) & is.finite(get(va_col))]
  x[, .(sample = sample, growth_measure = growth, va_measure = va_col, n_schools = .N,
    pearson_r = cor(get(growth), get(va_col)), spearman_r = cor(get(growth), get(va_col), method = "spearman")),
    by = .(outcome, label, primary, ordering)]
}
results <- rbindlist(list(
  correlations(matched, "All positive-capacity 2021 schools; exits retained", "pct_growth"),
  correlations(matched, "All positive-capacity 2021 schools; exits retained", "delta_seats"),
  correlations(matched, "Positive capacity in both endpoints", "log_growth"),
  correlations(matched[capacity_before >= 100], "At least 100 declared seats in 2021; exits retained", "pct_growth"),
  correlations(matched, "All positive-capacity 2021 schools; exits retained", "pct_growth", "va_unshrunk")
))
setorder(results, ordering, sample, growth_measure, va_measure)
coverage <- matched[, .(baseline_schools = nrow(base), matched_schools = .N,
  unmatched_schools = nrow(base) - .N, coverage_pct = 100*.N/nrow(base),
  matched_exits = sum(exit_capacity), median_va_students = as.numeric(median(n_students_regression)),
  median_reliability = median(eb_reliability)), by = .(outcome, label, primary, ordering)][order(ordering)]
annual <- rbindlist(lapply(2022:2025, function(y) {
  z <- merge(endpoints(y-1L, y), va, by = "rbd")
  z <- merge(z, specs, by = "outcome")
  correlations(z, "Positive-capacity schools in previous year; exits retained", "pct_growth")[, `:=`(start_year = y-1L, end_year = y)]
}))

main <- results[growth_measure == "pct_growth" & va_measure == "va_eb" & startsWith(sample, "All positive")]
seats <- results[growth_measure == "delta_seats", .(outcome, pearson_seat_change = pearson_r)]
logs <- results[growth_measure == "log_growth", .(outcome, n_continuing = n_schools, pearson_log_growth = pearson_r)]
larger <- results[startsWith(sample, "At least"), .(outcome, n_base_ge100 = n_schools, pearson_pct_base_ge100 = pearson_r)]
main <- Reduce(function(x,y) merge(x,y,by="outcome"), list(main, seats, logs, larger))[order(ordering)]
fwrite(main, file.path(out, "main_correlations_2021_2025.csv"))
fwrite(results, file.path(out, "all_correlations_and_sensitivities.csv"))
fwrite(annual, file.path(out, "annual_growth_correlations.csv"))
fwrite(coverage, file.path(out, "va_match_coverage.csv"))
unmatched <- rbindlist(lapply(specs$outcome, function(o) base[!rbd %in% va[outcome == o, rbd]][, outcome := o]))
fwrite(unmatched, file.path(out, "unmatched_baseline_schools.csv"))
fwrite(base, file.path(out, "capacity_endpoint_panel.csv"))
fwrite(matched, file.path(out, "matched_school_va_long.csv"))
fwrite(base[order(-pct_growth)][1:20], file.path(out, "largest_percentage_changes.csv"))
fwrite(data.table(path = c(capacity_path, va_path), bytes = file.info(c(capacity_path, va_path))$size,
  md5 = unname(tools::md5sum(c(capacity_path, va_path)))), file.path(out, "input_manifest.csv"))

# Plot every matched continuer, including extreme changes. Log growth limits the
# leverage of tiny baseline denominators without trimming or recoding observations.
plot_data <- matched[primary == TRUE & capacity_after > 0]
plot_data[, z_va := (va_eb - mean(va_eb))/sd(va_eb), by = outcome]
plot_data[, bin := pmin(20L, ceiling(frank(va_eb, ties.method = "average")/.N*20)), by = outcome]
bins <- plot_data[, .(mean_va = mean(z_va), mean_log_growth = mean(log_growth), n = .N), by = .(outcome, bin)]
fwrite(bins, file.path(out, "plot_bin_means.csv"))
png(file.path(fig, "capacity_growth_vs_va_2021_2025.png"), width = 1800, height = 1400, res = 180)
par(mfrow = c(2,2), mar = c(4.2,4.5,3,1), oma = c(3,0,2,0), las = 1)
for (o in specs[primary == TRUE, outcome]) {
  x <- plot_data[outcome == o]
  b <- bins[outcome == o]
  plot(x$z_va, x$log_growth, pch = 16, cex = .42, col = adjustcolor("#60798B", alpha.f = .22),
    xlab = "School EB VA (school SDs)", ylab = "100 x log(capacity 2025 / capacity 2021)",
    main = specs[outcome == o, label], ylim = range(plot_data$log_growth))
  abline(h = 0, col = "#b6c0c6", lty = 2)
  points(b$mean_va, b$mean_log_growth, pch = 19, cex = .8, col = "#006D77")
  abline(lm(log_growth ~ z_va, data = x), col = "#BA5A31", lwd = 1.5)
  legend("bottomleft", sprintf("r = %.3f; N = %s", cor(x$va_eb, x$log_growth), format(nrow(x), big.mark=",")), bty="n", cex=.9)
}
mtext("Secondary capacity growth and school value added, 2021-2025", outer=TRUE, side=3, line=.4, font=2, cex=1.1)
mtext("Positive capacity at both endpoints. All observations shown; green dots are means in 20 VA bins.\nSaved 2017-2020 cohort observational EB VA. Equal school weights; descriptive associations, not causal effects.", outer=TRUE, side=1, line=.8, cex=.75)
dev.off()

esc <- function(x) { x <- gsub("&", "&amp;", as.character(x), fixed=TRUE); x <- gsub("<", "&lt;", x, fixed=TRUE); gsub(">", "&gt;", x, fixed=TRUE) }
table <- function(x) paste0("<div class='table'><table><thead><tr>",paste0("<th>",esc(names(x)),"</th>",collapse=""),"</tr></thead><tbody>",paste(apply(as.data.frame(x),1,function(r)paste0("<tr>",paste0("<td>",esc(r),"</td>",collapse=""),"</tr>")),collapse=""),"</tbody></table></div>")
display <- main[, .(VA = label, Schools = n_schools, Pearson_pct_growth = sprintf("%.3f",pearson_r),
  Rank_pct_growth = sprintf("%.3f",spearman_r), Pearson_seat_change = sprintf("%.3f",pearson_seat_change),
  Pearson_log_growth_continuers = sprintf("%.3f",pearson_log_growth), Pearson_pct_base_100plus = sprintf("%.3f",pearson_pct_base_ge100))]
annual_display <- dcast(annual, label + ordering ~ end_year, value.var = "pearson_r")[order(ordering)][, !"ordering"]
for (v in setdiff(names(annual_display), "label")) set(annual_display,j=v,value=sprintf("%.3f",annual_display[[v]]))
html <- paste0("<!doctype html><html><head><meta charset='utf-8'><title>Capacity growth and VA, 2021-2025</title><style>body{font:16px/1.55 system-ui;color:#173047;background:#f4f6f8;margin:0}main{max-width:1200px;margin:auto;background:white;padding:36px}h1{font-size:32px}h2{margin-top:32px}img{width:100%}.table{overflow-x:auto}table{border-collapse:collapse;font-size:14px;width:100%}th,td{padding:9px;border-bottom:1px solid #dce3e8;text-align:right}th{background:#eaf0f5}td:first-child{text-align:left}.note{padding:16px;background:#eef4f8}a{color:#235789}</style></head><body><main>",
  "<h1>Do higher-VA schools expand secondary capacity?</h1><p>Change in SAE process-year declared secondary capacity from 2021 to 2025, matched by RBD to the project's saved 2017-2020 cohort school VA.</p>",
  "<p class='note'><b>The descriptive correlations are small.</b> The first four outcomes are the established school-preference VA measures; the remaining four are supplementary outcomes from the same saved file. Main correlations weight each school equally and use empirical-Bayes adjusted observational VA. They do not estimate a causal effect or a forecast based on information available in 2021.</p>",
  "<h2>Endpoint correlations</h2>",table(display),
  "<p>Percentage growth = 100 x (capacity in 2025 / capacity in 2021 - 1). Start with all ",nrow(base)," positive-capacity 2021 schools; ",sum(base$exit_capacity)," have zero or no secondary capacity listing in 2025 and enter as -100%. New listings with no positive 2021 capacity are excluded because their growth rate is undefined. Matched coverage is ",sprintf("%.1f",min(coverage$coverage_pct)),"%-",sprintf("%.1f",max(coverage$coverage_pct)),"%.</p>",
  "<p>Log growth uses positive-capacity continuers only. The final column restricts baseline capacity to at least 100 seats and retains exits. No matched school is trimmed in the main result. The baseline panel includes RBD 16970, EDUCERE COLLEGE, going from one declared seat to 140 (13,900% growth), and RBD 16780 growing 600%; neither has a matched VA estimate, so neither enters any reported correlation. The largest matched percentage growth is 354.3%. Rank/log/100-plus checks address the influence of small initial capacity among matched schools.</p>",
  "<img src='../../figures/secondary_sae_spots/capacity_growth_vs_va_2021_2025.png' alt='Four scatter plots of continuing-school log capacity growth versus math, exam, field, and institution value added'>",
  "<h2>Each annual change within 2021-2025</h2><p>Columns denote the ending process year, e.g. 2022 is growth from 2021 to 2022. Pearson correlations of percentage growth with fixed EB VA; each year's prior positive-capacity schools define the sample.</p>",table(annual_display),
  "<h2>Interpretation and scope</h2><p>Capacity is cupos_totales summed across grades 9-12 and programs in regular-round A1. This is not enrollment or advertised vacancies. Process years 2021-2025 concern entry years 2022-2026. These years have full national secondary SAE coverage. Missing 2025 listings are zero listed capacity, not proof of closure.</p>",
  "<p>The VA file uses grade-8 cohorts 2017-2020 and later outcomes, so its measurement period overlaps these capacity years. Estimates are adjusted observational school effects with EB shrinkage, not separately lottery-identified effects for every school. This is an unweighted association of estimated VA with capacity growth, with no controls or measurement-error correction. No p-values are used for the exploratory comparison.</p>",
  "<h2>Downloads</h2><p><a href='../../../data/clean/secondary_sae_spots/va_correlations/main_correlations_2021_2025.csv'>Main correlations</a> | <a href='../../../data/clean/secondary_sae_spots/va_correlations/all_correlations_and_sensitivities.csv'>All sensitivities including unshrunk VA</a> | <a href='../../../data/clean/secondary_sae_spots/va_correlations/matched_school_va_long.csv'>School-level matched data</a> | <a href='annual_secondary_sae_spots.html'>Original annual SAE report</a></p></main></body></html>")
writeLines(html,file.path(report,"capacity_growth_vs_va_2021_2025.html"),useBytes=TRUE)
print(display)
print(coverage)
print(annual_display)
