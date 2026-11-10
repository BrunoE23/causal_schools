suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
clean <- file.path(repo, "data/clean/continuous_institution_va")
report <- file.path(repo, "output/reports/continuous_institution_va")
figures <- file.path(repo, "output/figures/continuous_institution_va")
for (p in c(report, figures)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
v <- fread(file.path(clean, "school_value_added.csv"))
s <- fread(file.path(clean, "summary.csv"))
# lfe 3.1.1 btrap rounds bN upward to full memory/thread blocks.
vectors_per_thread <- max(2L, as.integer(min((500e6 / 2) / (s$n_students * 8), s$bootstrap_reps) / 4L))
vectors_per_block <- 4L * vectors_per_thread
actual_draws <- ceiling(s$bootstrap_reps / vectors_per_block) * vectors_per_block
fwrite(data.table(requested_bN = s$bootstrap_reps, actual_draws, threads = 4L,
  bootmem_MB = 500, eps = 1e-8, seed = s$seed), file.path(clean, "bootstrap_configuration.csv"))
coverage <- fread(file.path(clean, "fe_coverage.csv"))[, .(students = sum(students)), by = fe_source]
join <- fread(file.path(clean, "institution_join_audit.csv"))
stopifnot(uniqueN(v$outcome) == 1L, unique(v$outcome) == "institution_fe_enrolled_m1")
old_path <- file.path(data_root, "data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
old <- fread(old_path, select = c("school_rbd", "analysis_sample", "outcome", "n_students_regression", "n_students_regression_total",
  "controlled_value_added_centered_student", "controlled_value_added_eb_centered_student"))[
    analysis_sample == "All" & outcome == "high_inst_m1"]
stopifnot(!anyDuplicated(old$school_rbd), nrow(old) > 0L)
c <- merge(v[, .(school_rbd, continuous_n = n_students_regression,
  continuous_raw = controlled_value_added_centered_student, continuous_eb = controlled_value_added_eb_centered_student)],
  old[, .(school_rbd, binary_n = n_students_regression, binary_raw = controlled_value_added_centered_student,
    binary_eb = controlled_value_added_eb_centered_student)], by = "school_rbd")
weighted_cor <- function(x, y, w) {
  x <- x - weighted.mean(x, w); y <- y - weighted.mean(y, w)
  sum(w * x * y) / sqrt(sum(w * x^2) * sum(w * y^2))
}
corr <- data.table(n_common_schools = nrow(c), existing_binary_n = unique(old$n_students_regression_total),
  unshrunk_pearson = cor(c$continuous_raw, c$binary_raw), eb_pearson = cor(c$continuous_eb, c$binary_eb),
  eb_spearman = cor(c$continuous_eb, c$binary_eb, method = "spearman"),
  eb_student_weighted_pearson = weighted_cor(c$continuous_eb, c$binary_eb, c$continuous_n))
fwrite(c, file.path(clean, "binary_va_school_comparison.csv"))
fwrite(corr, file.path(clean, "binary_va_correlations.csv"))
# School-unweighted quantiles are labelled separately from student-weighted SDs.
q <- quantile(v$controlled_value_added_eb_centered_student, c(.1, .25, .5, .75, .9), names = FALSE)
fwrite(data.table(percentile = c(10,25,50,75,90), eb_va = q), file.path(clean, "eb_school_quantiles.csv"))
png(file.path(figures, "distribution_and_binary_comparison.png"), width = 1500, height = 630, res = 140)
par(mfrow = c(1,2), mar = c(4.8,4.5,3,1), las = 1)
hist(v$controlled_value_added_eb_centered_student, breaks = 60, col = "#3C7F92", border = "white",
  main = "Continuous institution-premium VA", xlab = "EB school VA (log-premium units)", ylab = "Schools")
abline(v = 0, lty = 2, col = "#444444")
plot(100*c$binary_eb, c$continuous_eb, pch = 16, cex = .45, col = adjustcolor("#3C7F92", .35),
  main = sprintf("Comparison with existing binary VA (r = %.3f)", corr$eb_pearson),
  xlab = "Existing EB high-premium-institution VA (pp)", ylab = "Continuous EB VA (log-premium units)")
abline(h = 0, v = 0, lty = 2, col = "#999999")
invisible(dev.off())
fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
html_table <- function(x, digits = 5L) {
  cells <- lapply(x, function(z) if (is.numeric(z)) format(round(z, digits), trim = TRUE) else as.character(z))
  c(paste0("<table><tr>", paste0("<th>", names(x), "</th>", collapse = ""), "</tr>"),
    vapply(seq_len(nrow(x)), function(i) paste0("<tr>", paste0("<td>", vapply(cells, `[`, character(1), i), "</td>", collapse = ""), "</tr>"), character(1)), "</table>")
}
recovered <- join[recovered_by_code == TRUE, sum(students)]
recovered_institutions <- join[recovered_by_code == TRUE, uniqueN(COD_INST_m1)]
lines <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Continuous institution-premium school VA</title>',
  '<style>body{font:16px/1.5 system-ui;max-width:1150px;margin:35px auto;padding:0 25px;color:#18323b}table{border-collapse:collapse;margin:20px 0;display:block;overflow-x:auto}td,th{padding:8px 12px;border-bottom:1px solid #d9e2e5;text-align:right}td:first-child,th:first-child{text-align:left}img{width:100%}code{font-size:14px}</style></head><body>',
  '<h1>Continuous institution-premium school value added</h1>',
  sprintf('<p><strong>%s students, %s high schools.</strong> Non-enrollees remain NA, as requested. The outcome is the centered institution FE from the existing MiFuturo model of log income on institution and field FEs, attached to first HE enrollment.</p>', fmt(s$n_students), fmt(s$n_schools)),
  sprintf('<p>The student-weighted SD is <strong>%.5f log-premium units</strong> for EB school VA (unshrunk: %.5f). The school-unweighted 90th-minus-10th-percentile EB spread is %.5f. These are effects on the institutional-premium index, not estimated effects on realized wages.</p>', s$eb_sd_student_weighted, s$unshrunk_sd_student_weighted, q[5]-q[1]),
  '<img src="../../figures/continuous_institution_va/distribution_and_binary_comparison.png" alt="Distribution of continuous VA and comparison with binary VA">',
  '<h2>Sample and specification</h2>',
  '<p>2017-2020 grade-8 cohorts; ages 12-16; observed high-school RBD; observed math or verbal entrance score; complete VA baseline controls; HE enrollment at an institution with an estimated premium. Same baseline specification as current VA: separate cubic grade-4 math/verbal, linear imputed income, parents and early-childhood controls, middle-school GPA/attendance, cohort/sex/age/comuna/middle-years indicators, and high-school and middle-school FEs. No downstream PAES-score controls.</p>',
  sprintf('<p>%s eligible observations are removed by the existing estimator singleton/perfect-fit rule. The fitted model is saved. School effects are centered using estimation-sample student counts. SEs use the current lfe centered-school bootstrap setting bN=100 (the package batches this into %s actual draws) with seed 20261110. EB shrinkage follows the existing variance-minus-noise rule; the student-weighted mean reliability is %.3f.</p>', fmt(s$estimator_dropped), fmt(actual_draws), s$mean_reliability_student_weighted),
  '<p>These are observational school VA estimates conditional on HE enrollment, a downstream outcome. No lottery-IV validation is included. SEs treat estimated MiFuturo institutional premia as fixed.</p>',
  '<h2>Coverage before estimator drops</h2>', html_table(coverage, 0),
  sprintf('<p>Joining on unique official institution codes recovers %s students at %s institutions whose FEs were missed by the older type/name key. The largest cause is INSITUTOS versus INSTITUTOS in professional-institute type labels. All formerly matched FEs are unchanged. Unsupported institutions remain NA. Existing binary outcomes and VA files are unchanged.</p>', fmt(recovered), fmt(recovered_institutions)),
  '<h2>School-value diagnostics</h2>', html_table(s[, .(n_students, n_schools, outcome_mean, outcome_sd, unshrunk_sd_student_weighted, eb_sd_student_weighted, eb_tau, mean_reliability_student_weighted)]),
  '<h2>Comparison with existing binary institution VA</h2>', html_table(corr),
  '<p>The comparison uses common schools. The existing binary VA includes non-enrollees as zero and retains its older institution matching. These correlations therefore reflect differences in outcome coding and sample as well as removal of the premium cutoff. Weighted correlation uses continuous-VA estimation counts.</p>',
  '<h2>Saved outputs and validation</h2>',
  '<p>School values, raw and EB, SEs, counts, coverage, institution crosswalk, comparison data, actual regression model and numerical checks are in <code>data/clean/continuous_institution_va/</code>. Code: <code>code/codex/continuous_institution_va/</code>. Preparation checks validate original student-level FEs, IDs, cohorts, programs, enrollment, the original binary cutoff, and source integrity. Estimation checks validate normal equations, counts, student-weighted centering and the EB identities.</p>',
  '</body></html>')
writeLines(lines, file.path(report, "report.html"), useBytes = TRUE)
print(s); print(corr); print(coverage)
