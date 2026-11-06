suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
parent <- file.path(repo, "data", "clean", "baseline_by_va_quintile")
out <- file.path(parent, "expected_va")
fig <- file.path(repo, "output", "figures", "baseline_by_va_quintile", "expected_va")
tab <- file.path(repo, "output", "tables", "baseline_by_va_quintile", "expected_va")
for (p in c(out, fig, tab)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
specs <- data.table(va_key = c("math", "verbal", "field", "institution"),
  spec = c("math_adj_eb", "leng_adj_eb", "highpay_adj_eb", "highinst_adj_eb"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1", "high_inst_m1"),
  va_label = c("Expected math VA", "Expected verbal VA", "Expected high-premium field VA", "Expected high-premium institution VA"),
  display_multiplier = c(1, 1, 100, 100))
specs[, `:=`(e_col = paste0("expected_", spec), mass_col = paste0("mass_with_value_", spec),
              unknown_col = paste0("unknown_mass_", spec))]
variables <- unique(fread(file.path(parent, "baseline_characteristics_long.csv"))[, .(variable, characteristic, multiplier, digits)])
stopifnot(nrow(variables) == 8L)
# Reuse the preceding validated baseline population, without scanning the 1 GB
# universe again. No PAES-taking or baseline-completeness filter is introduced.
d <- readRDS(file.path(parent, "baseline_student_input.rds"))
setDT(d)
d <- d[sae_applicants == TRUE]
stopifnot(nrow(d) == 294198L, !anyDuplicated(d$mrun), !anyDuplicated(d[, .(student_id, sae_proceso)]),
          !anyNA(d$student_id), all(d$sae_proceso %in% 2018:2020))

va <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
  "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
va <- merge(va[analysis_sample == "All" & is.finite(controlled_value_added_eb_centered_student)], specs, by = "outcome")
stopifnot(!anyDuplicated(va[, .(school_rbd, va_key)]))
wide_va <- dcast(va, school_rbd ~ va_key, value.var = "controlled_value_added_eb_centered_student")

# Selected probability inputs total only about 30 MB. Retain all assignment
# options and their original probability mass, including simulated unmatched.
summaries <- lapply(2018:2020, function(year) {
  p <- fread(file.path(root, "data", "clean", "DA_probs", paste0("DA_probs_", year, ".csv")),
    select = c("student_id", "school_id", "prob"), showProgress = FALSE)
  p <- p[student_id %in% d[sae_proceso == year, student_id]]
  stopifnot(all(is.finite(p$prob)), all(p$prob >= 0 & p$prob <= 1),
            !anyDuplicated(p[, .(student_id, school_id)]))
  p[, `:=`(school_rbd = suppressWarnings(as.numeric(sub("_.*$", "", school_id))),
            unmatched = tolower(trimws(school_id)) == "unmatched")]
  stopifnot(all(p[is.na(school_rbd), unmatched]))
  p <- merge(p, wide_va, by = "school_rbd", all.x = TRUE, sort = FALSE)
  s <- p[, .(total_probability_mass = sum(prob), unmatched_mass = sum(prob * unmatched),
              probability_any_risk = as.integer(max(prob) < 1)), by = student_id]
  for (i in seq_len(nrow(specs))) {
    v <- specs[i]
    # Exact current main-IV E definition: unavailable VA contributes zero.
    # Do not renormalize over covered schools or apply a new coverage filter.
    a <- p[, .(e_value = sum(prob * fifelse(is.na(get(v$va_key)), 0, get(v$va_key))),
                known_mass = sum(prob * !is.na(get(v$va_key))),
                unknown_mass = sum(prob * (!unmatched & is.na(get(v$va_key))))), by = student_id]
    setnames(a, c("e_value", "known_mass", "unknown_mass"), c(v$e_col, v$mass_col, v$unknown_col))
    s <- merge(s, a, by = "student_id", sort = FALSE)
  }
  s[, sae_proceso := year]
  s
})
e <- rbindlist(summaries)
stopifnot(!anyDuplicated(e[, .(student_id, sae_proceso)]), all(abs(e$total_probability_mass - 1) < 1e-6))
x <- merge(d, e, by = c("student_id", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(x) == nrow(d))
fwrite(data.table(n_sae_applicants = nrow(x), n_with_probabilities = sum(!is.na(x$total_probability_mass)),
                  n_without_probabilities = sum(is.na(x$total_probability_mass))), file.path(out, "applicant_probability_coverage.csv"))

# Validate E itself and covered probability mass against the saved current main
# frame on every at-risk student, before any exam or baseline-case restriction.
main <- fread(file.path(repo, "data", "clean", "peer_lottery_variation", "main_regression_df_with_peer_shocks.csv"),
  select = c("mrun", "student_id", "sae_proceso", specs$e_col, specs$mass_col), showProgress = FALSE)
check <- merge(x, main, by = c("mrun", "student_id", "sae_proceso"), suffixes = c("", "_saved"))
stopifnot(nrow(check) == nrow(main), nrow(check) == 162633L,
          all(check$at_risk), all(check$probability_any_risk == 1L))
parity <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]
  e_gap <- max(abs(check[[s$e_col]] - check[[paste0(s$e_col, "_saved")]]))
  mass_gap <- max(abs(check[[s$mass_col]] - check[[paste0(s$mass_col, "_saved")]]))
  stopifnot(is.finite(e_gap), is.finite(mass_gap), e_gap < 1e-10, mass_gap < 1e-10)
  data.table(va_key = s$va_key, n_checked = nrow(check), max_expected_va_gap = e_gap, max_known_mass_gap = mass_gap)
}))
fwrite(parity, file.path(out, "saved_main_expected_va_parity.csv"))
saveRDS(x, file.path(out, "sae_applicants_with_expected_va.rds"))

stats <- counts <- definitions <- coverage <- list()
for (i in seq_len(nrow(specs))) {
  s <- specs[i]
  z <- copy(x[is.finite(get(s$e_col))])
  z[, quintile := pmin(5L, as.integer(ceiling(5 * frank(get(s$e_col), ties.method = "average") / .N)))]
  stopifnot(uniqueN(z$quintile) == 5L)
  definitions[[s$va_key]] <- z[, .(n_students = .N, n_distinct_e = uniqueN(get(s$e_col)),
    min_e = min(get(s$e_col)), max_e = max(get(s$e_col)), mean_e = mean(get(s$e_col))), by = quintile][,
    `:=`(va_key = s$va_key, expected_variable = s$e_col)]
  counts[[s$va_key]] <- z[, .(n_students = .N, mean_e_display = s$display_multiplier * mean(get(s$e_col))), by = quintile][,
    va_key := s$va_key]
  stats[[s$va_key]] <- rbindlist(lapply(seq_len(nrow(variables)), function(j) {
    info <- variables[j]; v <- info$variable
    z[, .(n_students = .N, n_observed = sum(is.finite(get(v))), mean = mean(get(v), na.rm = TRUE),
           sd = sd(get(v), na.rm = TRUE)), by = quintile][,
      `:=`(va_key = s$va_key, variable = v, characteristic = info$characteristic,
            multiplier = info$multiplier, digits = info$digits)]
  }))
  coverage[[s$va_key]] <- data.table(va_key = s$va_key, n_applicants = nrow(x), n_with_e = nrow(z),
    n_missing_e = nrow(x) - nrow(z), n_with_unknown_school_va_mass = sum(z[[s$unknown_col]] > 1e-12),
    mean_unknown_school_va_mass = mean(z[[s$unknown_col]]), mean_unmatched_mass = mean(z$unmatched_mass))
  fwrite(z[, c("mrun", "student_id", "sae_proceso", s$e_col, "quintile"), with = FALSE],
         file.path(out, paste0(s$va_key, "_student_e_quintiles.csv")))
}
stats <- rbindlist(stats); counts <- rbindlist(counts); definitions <- rbindlist(definitions); coverage <- rbindlist(coverage)
stats[, `:=`(display_mean = mean * multiplier, observed_pct = 100 * n_observed / n_students, n_missing = n_students - n_observed)]
stats[, cell := mapply(function(v, n) sprintf(paste0("%.", n, "f"), v), display_mean, digits)]
partition <- merge(counts[, .(n_quintile_total = sum(n_students)), by = va_key], coverage, by = "va_key")
stopifnot(nrow(stats) == 160L, nrow(counts) == 20L, all(partition$n_quintile_total == partition$n_with_e))
fwrite(stats, file.path(out, "baseline_characteristics_by_e_quintile.csv"))
fwrite(definitions, file.path(out, "student_e_quintile_definitions.csv"))
fwrite(coverage, file.path(out, "expected_va_probability_coverage.csv"))
fwrite(partition, file.path(out, "sample_partition_checks.csv"))

display <- rbindlist(list(counts[, .(va_key, quintile, characteristic = "Students", cell = format(n_students, big.mark = ",", trim = TRUE))],
  counts[, .(va_key, quintile, characteristic = "Mean E (see units below)", cell = sprintf("%.2f", mean_e_display))],
  stats[, .(va_key, quintile, characteristic, cell)]))
metric_order <- c("Students", "Mean E (see units below)", variables$characteristic)
display <- merge(display, specs[, .(va_key, va_label)], by = "va_key")
display[, `:=`(va_label = factor(va_label, levels = specs$va_label),
  characteristic = factor(characteristic, levels = rev(metric_order)),
  quintile = factor(quintile, levels = 1:5, labels = paste0("Q", 1:5)))]
wide <- dcast(display, va_key + characteristic ~ quintile, value.var = "cell")
wide[, row_order := match(as.character(characteristic), metric_order)]
setorder(wide, va_key, row_order); wide[, row_order := NULL]
fwrite(wide, file.path(out, "sae_applicants_e_quintile_table.csv"))
p <- ggplot(display, aes(quintile, characteristic)) +
  geom_tile(aes(fill = as.integer(characteristic) %% 2), color = "white", linewidth = 0.5) +
  geom_text(aes(label = cell, fontface = fifelse(characteristic == "Students", "bold", "plain")), size = 3.8) +
  facet_wrap(~va_label, ncol = 2) + scale_fill_gradient(low = "#FFFFFF", high = "#F1F4F7", guide = "none") +
  scale_x_discrete(position = "top", expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Baseline composition of SAE applicants by expected-VA quintile",
    subtitle = "Student-level E = probability-weighted EB school VA | Timely SAE applicants, 2018-2020 | No PAES score restriction",
    x = NULL, y = NULL, caption = paste0("Q1 = lowest E; Q5 = highest. Each outcome ranks applicants separately, with equal student weights and tied E values kept together.\n",
      "Mean E units: math/verbal in test-score SD; field/institution in percentage points. Baseline means use nonmissing values and existing imputations.\n",
      "E reproduces the main-IV rule: unavailable school VA contributes zero, without renormalizing assignment probabilities. Coverage is reported separately.")) +
  theme_minimal(base_size = 12) + theme(panel.grid = element_blank(), axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold", color = "#172B40", size = 12), axis.text.y = element_text(color = "#334155", size = 10),
    strip.text = element_text(face = "bold", color = "#172B40", size = 12.5, margin = margin(b = 9, t = 9)),
    strip.background = element_rect(fill = "#E6ECF2", color = NA), panel.spacing = grid::unit(1.5, "lines"),
    plot.title = element_text(face = "bold", size = 18, color = "#172B40"),
    plot.subtitle = element_text(size = 10.5, color = "#475569", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "#5B6470", margin = margin(t = 14)), plot.margin = margin(15, 18, 14, 16))
ggsave(file.path(fig, "sae_applicants_baseline_by_expected_va_quintile.png"), p, width = 14, height = 9.5, dpi = 220, bg = "white")

panels <- vapply(specs$va_key, function(o) {
  rows <- vapply(metric_order, function(m) {
    cells <- vapply(1:5, function(q) {
      a <- display[va_key == o & as.character(quintile) == paste0("Q", q) & characteristic == m]
      stopifnot(nrow(a) == 1L)
      cov <- stats[va_key == o & quintile == q & characteristic == m]
      small <- if (nrow(cov)) sprintf('<small>%.1f%% available</small>', cov$observed_pct) else ""
      paste0("<td>", a$cell, small, "</td>")
    }, character(1))
    paste0("<tr><th>", m, "</th>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  cvr <- coverage[va_key == o]
  paste0('<article><h2>', specs[va_key == o, va_label], '</h2><table><thead><tr><th>Characteristic</th>',
    paste0("<th>Q", 1:5, "</th>", collapse = ""), '</tr></thead><tbody>', paste(rows, collapse = ""),
    sprintf('</tbody></table><p>%s applicants with E; %s have positive probability on a real school without this VA estimate.</p></article>',
      format(cvr$n_with_e, big.mark = ",", trim = TRUE), format(cvr$n_with_unknown_school_va_mass, big.mark = ",", trim = TRUE)))
}, character(1))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>SAE baseline composition by expected VA</title>',
 '<style>body{font:15px system-ui,sans-serif;margin:30px;color:#172b40;background:#f7f9fb}h1{font-size:27px}h2{font-size:18px}.panels{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:22px}article{background:white;padding:20px;border:1px solid #dce3ea;border-radius:8px}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px 7px;text-align:right}th:first-child{text-align:left}thead{background:#e6ecf2}tbody tr:nth-child(even){background:#f1f4f7}small{display:block;font-size:10px;color:#667085;margin-top:2px}article p{font-size:12px;color:#667085}.notes{max-width:1200px;line-height:1.5}@media(max-width:1100px){.panels{grid-template-columns:1fr}}</style></head><body>',
 '<h1>Baseline composition of SAE applicants by expected-VA quintile</h1><p>E = sum of assignment probability times current unified R EB school VA.</p>',
 '<div class="panels">', paste(panels, collapse = ""), '</div><div class="notes"><p>Same timely SAE-applicant population and baseline definitions as the preceding attended-school table: 294,198 students, cohorts 2018-2020, within the broad-panel age and high-school-attendance restrictions. No PAES, at-risk-only or baseline-completeness filter. Quintiles rank students separately by each E, giving equal student weights and keeping ties together.</p>',
 '<p>Mean E is in test-score SD for math/verbal and percentage points for field/institution. Other means use nonmissing values; existing baseline income/education imputations are retained. Small percentages give available-variable coverage. E exactly follows the main-IV construction: unavailable school VA, including unmatched options, contributes zero. Probabilities are not renormalized and no new VA-coverage restriction is imposed. The associated CSVs report missing probability records and unknown real-school VA mass.</p></div></body></html>')
writeLines(html, file.path(tab, "sae_applicants_by_expected_va_quintile.html"), useBytes = TRUE)
stopifnot(length(gregexpr("<table>", html, fixed = TRUE)[[1]]) == 4L)
print(parity)
print(coverage)
print(wide)
