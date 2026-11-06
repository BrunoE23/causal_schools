suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "sae_application_geography", "q1_probability_by_income")
tab <- file.path(repo, "output", "tables", "sae_application_geography", "q1_probability_by_income")
fig <- file.path(repo, "output", "figures", "sae_application_geography", "q1_probability_by_income")
for (p in c(out, tab, fig)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
specs <- data.table(va_key = c("math", "verbal", "field", "institution"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1", "high_inst_m1"),
  label = c("Math", "Verbal", "High-premium field", "High-premium institution"),
  e_col = c("expected_math_adj_eb", "expected_leng_adj_eb", "expected_highpay_adj_eb", "expected_highinst_adj_eb"),
  saved_unknown = c("unknown_mass_math_adj_eb", "unknown_mass_leng_adj_eb", "unknown_mass_highpay_adj_eb", "unknown_mass_highinst_adj_eb"))
base <- readRDS(file.path(repo, "data", "clean", "baseline_by_va_quintile", "expected_va", "sae_applicants_with_expected_va.rds"))
setDT(base)
stopifnot(nrow(base) == 294198L, sum(base$at_risk) == 162633L,
          !anyDuplicated(base[, .(student_id, sae_proceso)]))
income <- base$income_decile_imputed
stopifnot(all(income[!is.na(income)] %in% 1:10))
assigned <- fread(file.path(repo, "data", "clean", "baseline_by_va_quintile", "school_va_quintile_assignments.csv"))
current <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
  "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
current <- merge(current[analysis_sample == "All" & is.finite(controlled_value_added_eb_centered_student)], specs, by = "outcome")
check <- merge(assigned, current, by = c("school_rbd", "outcome", "va_key"))
stopifnot(nrow(check) == nrow(assigned), nrow(check) == nrow(current),
          max(abs(check$va_eb - check$controlled_value_added_eb_centered_student)) < 1e-10)
lookup <- merge(dcast(assigned, school_rbd ~ va_key, value.var = "quintile"),
                dcast(assigned, school_rbd ~ va_key, value.var = "va_eb"), by = "school_rbd", suffixes = c("_q", "_value"))

# The DA inputs total about 30 MB. Use their full assignment distributions,
# including automatic continuity options; unmatched is a distinct non-school
# outcome and does not count as Q1. No probability renormalization is applied.
probabilities <- rbindlist(lapply(2018:2020, function(year) {
  p <- fread(file.path(root, "data", "clean", "DA_probs", paste0("DA_probs_", year, ".csv")),
    select = c("student_id", "school_id", "prob"), showProgress = FALSE)
  p <- p[student_id %in% base[sae_proceso == year, student_id]]
  stopifnot(!anyNA(p$prob), all(p$prob >= 0 & p$prob <= 1), !anyDuplicated(p[, .(student_id, school_id)]))
  p[, `:=`(school_rbd = suppressWarnings(as.numeric(sub("_.*$", "", school_id))),
            unmatched = tolower(trimws(school_id)) == "unmatched")]
  stopifnot(all(p[is.na(school_rbd), unmatched]))
  p <- merge(p, lookup, by = "school_rbd", all.x = TRUE, sort = FALSE)
  p[, unmatched_weight := prob * unmatched]
  s <- p[, .(total_mass = sum(prob), unmatched_mass_new = sum(unmatched_weight), max_probability = max(prob)), by = student_id]
  s[, at_risk_new := max_probability < 1]
  for (k in specs$va_key) {
    # Precompute products outside the student grouping, enabling fast GForce sums.
    p[, `:=`(q1_weight = prob * (!is.na(get(paste0(k, "_q"))) & get(paste0(k, "_q")) == 1L),
              other_weight = prob * (!is.na(get(paste0(k, "_q"))) & get(paste0(k, "_q")) != 1L),
              unknown_weight = prob * (!unmatched & is.na(get(paste0(k, "_q")))),
              e_weight = prob * fcoalesce(get(paste0(k, "_value")), 0))]
    a <- p[, .(p_q1 = sum(q1_weight), p_other = sum(other_weight),
                p_unknown = sum(unknown_weight), e_reconstructed = sum(e_weight)), by = student_id]
    setnames(a, c("p_q1", "p_other", "p_unknown", "e_reconstructed"), paste0(c("p_q1_", "p_other_", "p_unknown_", "e_check_"), k))
    s <- merge(s, a, by = "student_id", sort = FALSE)
  }
  s[, sae_proceso := year]
  s
}))
stopifnot(!anyDuplicated(probabilities[, .(student_id, sae_proceso)]))
needed <- c("mrun", "student_id", "sae_proceso", "at_risk", "income_decile_imputed", "unmatched_mass", specs$e_col, specs$saved_unknown)
d <- merge(base[, ..needed], probabilities, by = c("student_id", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == 294198L, !anyNA(d$total_mass), all(abs(d$total_mass - 1) < 1e-6),
  identical(d$at_risk, d$at_risk_new), max(abs(d$unmatched_mass - d$unmatched_mass_new)) < 1e-10)
audit <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]; k <- s$va_key
  lower <- d[[paste0("p_q1_", k)]]; unknown <- d[[paste0("p_unknown_", k)]]
  other <- d[[paste0("p_other_", k)]]
  partition_gap <- max(abs(lower + other + unknown + d$unmatched_mass_new - d$total_mass))
  e_gap <- max(abs(d[[paste0("e_check_", k)]] - d[[s$e_col]]))
  unknown_gap <- max(abs(unknown - d[[s$saved_unknown]]))
  stopifnot(all(lower >= 0), all(unknown >= 0), all(lower + unknown <= 1 + 1e-6),
            partition_gap < 1e-10, e_gap < 1e-10, unknown_gap < 1e-10)
  data.table(va_key = k, n_students = nrow(d), max_partition_gap = partition_gap,
             max_e_gap = e_gap, max_unknown_mass_gap = unknown_gap)
}))
fwrite(audit, file.path(out, "probability_validation.csv"))
saveRDS(d[, c("mrun", "student_id", "sae_proceso", "at_risk", "income_decile_imputed", "unmatched_mass_new",
  paste0("p_q1_", specs$va_key), paste0("p_unknown_", specs$va_key)), with = FALSE],
  file.path(out, "student_q1_assignment_probabilities.rds"))

summaries <- list()
for (g in c("All SAE applicants", "At risk")) for (i in seq_len(nrow(specs))) {
  s <- specs[i]; k <- s$va_key
  x <- if (g == "At risk") copy(d[at_risk == TRUE]) else copy(d)
  x[, `:=`(p_lower = get(paste0("p_q1_", k)), p_unknown = get(paste0("p_unknown_", k)))]
  by_income <- x[, .(n_students = .N, mean_p_q1 = mean(p_lower), mean_unknown_mass = mean(p_unknown),
    mean_unmatched_mass = mean(unmatched_mass_new), n_students_unknown_va = sum(p_unknown > 1e-12)), by = income_decile_imputed]
  overall <- x[, .(income_decile_imputed = 0L, n_students = .N, mean_p_q1 = mean(p_lower),
    mean_unknown_mass = mean(p_unknown), mean_unmatched_mass = mean(unmatched_mass_new), n_students_unknown_va = sum(p_unknown > 1e-12))]
  z <- rbindlist(list(by_income, overall), use.names = TRUE)
  z[, `:=`(group = g, va_key = k, outcome_label = s$label, pct_q1_lower = 100 * mean_p_q1,
            pct_q1_upper = 100 * (mean_p_q1 + mean_unknown_mass))]
  stopifnot(sum(by_income$n_students) == nrow(x),
    abs(weighted.mean(by_income$mean_p_q1, by_income$n_students) - overall$mean_p_q1) < 1e-12)
  summaries[[paste(g, k)]] <- z
}
results <- rbindlist(summaries)
fwrite(results, file.path(out, "q1_assignment_probability_by_income.csv"))
income_coverage <- d[, .(n_all = .N, n_income_observed = sum(!is.na(income_decile_imputed)), n_income_missing = sum(is.na(income_decile_imputed))),
  by = .(group = fifelse(at_risk, "At risk", "Not at risk"))]
fwrite(income_coverage, file.path(out, "income_coverage_by_risk.csv"))
for (g in c("All SAE applicants", "At risk")) {
  z <- results[group == g]
  fwrite(dcast(z, income_decile_imputed + n_students ~ outcome_label, value.var = "pct_q1_lower"),
         file.path(out, paste0(if (g == "At risk") "at_risk" else "all_sae", "_probability_table.csv")))
}

plot_data <- results[income_decile_imputed %in% 1:10]
plot_data[, `:=`(outcome_label = factor(outcome_label, levels = specs$label),
                 group = factor(group, levels = c("All SAE applicants", "At risk")))]
ymax <- 10 * ceiling(max(plot_data$pct_q1_upper) / 10)
p <- ggplot(plot_data, aes(income_decile_imputed, pct_q1_lower, color = group, fill = group)) +
  geom_ribbon(aes(ymin = pct_q1_lower, ymax = pct_q1_upper), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) + facet_wrap(~outcome_label, ncol = 2) +
  scale_color_manual(values = c("All SAE applicants" = "#244F78", "At risk" = "#BC4B37")) +
  scale_fill_manual(values = c("All SAE applicants" = "#244F78", "At risk" = "#BC4B37")) +
  scale_x_continuous(breaks = 1:10) + scale_y_continuous(breaks = seq(0, ymax, 10), labels = function(z) paste0(z, "%")) +
  coord_cartesian(ylim = c(0, ymax)) +
  labs(title = "SAE assignment probability of a Q1 school, by family income",
    subtitle = "Four separate current EB VA rankings | Q1 is lowest school VA | Equal applicant weights",
    x = "Existing family-income decile", y = "Average probability of Q1 assignment", color = NULL, fill = NULL,
    caption = paste0("Each applicant's probability sums the original SAE assignment probabilities over Q1 schools. Unmatched outcomes count zero; no renormalization.\n",
      "Lines: confirmed Q1 probability. Shading: maximum additional probability from schools without VA, not a confidence interval.\n",
      "Income-missing students are reported separately in the tables. Assignment probabilities include continuity options and do not model subsequent attendance.")) +
  theme_classic(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 17),
    strip.background = element_rect(fill = "#E6ECF2", color = NA), strip.text = element_text(face = "bold", size = 12),
    panel.spacing = grid::unit(1.5, "lines"), plot.caption = element_text(hjust = 0, size = 9), plot.margin = margin(15, 15, 12, 15))
ggsave(file.path(fig, "q1_assignment_probability_by_income.png"), p, width = 13, height = 8, dpi = 220, bg = "white")

sections <- vapply(c("All SAE applicants", "At risk"), function(g) {
  z <- results[group == g]
  rows <- vapply(c(as.character(1:10), "Missing", "All"), function(label) {
    a <- if (label == "Missing") z[is.na(income_decile_imputed)] else if (label == "All") z[income_decile_imputed == 0L] else z[income_decile_imputed == as.integer(label)]
    a <- a[match(specs$va_key, va_key)]
    stopifnot(nrow(a) == 4L, uniqueN(a$n_students) == 1L)
    paste0('<tr><th>', label, '</th><td>', format(a$n_students[1], big.mark = ",", trim = TRUE), '</td>',
      paste0('<td>', sprintf('%.2f-%.2f%%', a$pct_q1_lower, a$pct_q1_upper), '</td>', collapse = ""), '</tr>')
  }, character(1))
  paste0('<h2>', g, '</h2><table><thead><tr><th>Income decile</th><th>Students</th>',
    paste0('<th>', specs$label, '</th>', collapse = ""), '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')
}, character(1))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Q1 assignment probability by income</title>',
 '<style>body{font:16px system-ui,sans-serif;max-width:1250px;margin:35px auto;padding:0 20px;color:#172b40}h1{font-size:27px}h2{font-size:21px;margin-top:30px}table{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums}th,td{padding:10px;text-align:right}th:first-child{text-align:left}thead{background:#e6ecf2}tbody tr:nth-child(even){background:#f1f4f7}.notes{font-size:14px;line-height:1.6;color:#475569}</style></head><body>',
 '<h1>Average SAE probability of assignment to a Q1 school, by income</h1><p>Q1 is the lowest quintile, separately for math, verbal, high-premium field and high-premium institution EB VA.</p>',
 paste(sections, collapse = ""),
 '<div class="notes"><p>For each student and outcome, sum the original DA assignment probabilities over schools in that outcome\'s Q1, then average across applicants in each existing imputed income decile. Income deciles are not recomputed within SAE or risk groups. The missing-income row is retained separately; the All row includes it.</p>',
 '<p>Intervals reflect only school-VA coverage: the lower bound counts probability on confirmed Q1 schools, and the upper bound adds probability on real schools whose VA quintile is unknown. Unmatched outcomes have zero Q1 probability. All original probability mass, including continuity options, is retained without renormalization. These are SAE assignment probabilities, not predictions of subsequent actual school attendance. Quintile cutoffs are unchanged from the previous school-list tables.</p></div></body></html>')
writeLines(html, file.path(tab, "q1_assignment_probability_by_income.html"), useBytes = TRUE)
print(audit)
print(d[, .(n_all = .N, n_income_observed = sum(!is.na(income_decile_imputed)), n_income_missing = sum(is.na(income_decile_imputed)))])
print(dcast(results[income_decile_imputed %in% 1:10], group + income_decile_imputed + n_students ~ outcome_label, value.var = "pct_q1_lower"))
