suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "baseline_by_va_quintile")
fig <- file.path(repo, "output", "figures", "baseline_by_va_quintile")
tab <- file.path(repo, "output", "tables", "baseline_by_va_quintile")
for (p in c(out, fig, tab)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
specs <- data.table(va_key = c("math", "verbal", "field", "institution"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1", "high_inst_m1"),
  va_label = c("Math VA", "Verbal VA", "High-premium field VA", "High-premium institution VA"))
variables <- data.table(variable = c("female", "EDAD_ALU", "in_rm", "z_sim_mat_4to", "z_sim_leng_4to",
    "income_decile_imputed", "mother_educ_years_imputed", "father_educ_years_imputed"),
  characteristic = c("Female (%)", "Grade 8 age (years)", "Home region: RM (%)", "Grade 4 math (SD)",
    "Grade 4 verbal (SD)", "Family income decile", "Mother's education (years)", "Father's education (years)"),
  multiplier = c(100, 1, 100, 1, 1, 1, 1, 1), digits = c(1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L))
populations <- c(broad_panel = "Broad panel", sae_applicants = "SAE applicants", at_risk = "At risk")

# Select 14 columns from the roughly 1 GB universe; do not load post-school
# scores/outcomes or condition on later test taking when describing baseline.
cols <- c("mrun", "student_id", "cohort_gr8", "EDAD_ALU", "GEN_ALU", "COD_REG_ALU", "most_time_RBD",
  "sae_proceso", "timely_sae", "z_sim_mat_4to", "z_sim_leng_4to", "income_decile_imputed",
  "mother_educ_years_imputed", "father_educ_years_imputed")
d <- fread(file.path(root, "data", "clean", "univ_gr8_df.csv"), select = cols,
           na.strings = c("", "NA"), showProgress = FALSE)
d <- d[cohort_gr8 %in% 2017:2020 & EDAD_ALU %between% c(12, 16) & most_time_RBD > 0]
setkey(d, mrun)
d <- d[!duplicated(mrun)]
stopifnot(nrow(d) == 931547L, !anyNA(d$mrun))
setnames(d, "most_time_RBD", "school_rbd")
d[, female := fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2L), NA_real_)]
d[, in_rm := fifelse(COD_REG_ALU %in% 1:16, as.numeric(COD_REG_ALU == 13L), NA_real_)]
d[, `:=`(broad_panel = TRUE, sae_applicants = cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L)]
# Reuse the existing full at-risk roster, before its later exam/complete-case
# filters, avoiding another scan of the large simulated-probability files.
risk <- fread(file.path(repo, "data", "clean", "peer_lottery_variation", "main_regression_df_with_peer_shocks.csv"),
  select = c("mrun", "student_id", "sae_proceso", "timely_sae", "any_risk"), showProgress = FALSE)
stopifnot(nrow(risk) == 162633L, !anyDuplicated(risk$mrun), all(risk$timely_sae == 1L), all(risk$any_risk == 1L))
check <- merge(d, risk, by = "mrun", suffixes = c("", "_risk"))
stopifnot(nrow(check) == nrow(risk), all(check$sae_applicants),
  all(check$student_id == check$student_id_risk), all(check$sae_proceso == check$sae_proceso_risk))
d[, at_risk := mrun %in% risk$mrun]
stopifnot(d[, sum(sae_applicants)] == 294198L, d[, sum(at_risk)] == 162633L)
saveRDS(d, file.path(out, "baseline_student_input.rds"))

# Audit baseline means against the existing unstratified balance table before
# matching to VA. Check its exact legacy coding separately from the valid-code
# means reported here: one gender and eight region codes were coded zero there.
reference <- fread(file.path(repo, "output", "tables", "balance_table", "balance_table.csv"))
reference_map <- c(broad_panel = "g_broad", sae_applicants = "g_sae_app", at_risk = "g_sae_risk")
baseline <- rbindlist(lapply(names(populations), function(pop) {
  x <- d[get(pop) == TRUE]
  rbindlist(lapply(variables$variable, function(v) {
    legacy <- if (v == "female") as.numeric(x$GEN_ALU == 2L) else if (v == "in_rm") as.numeric(x$COD_REG_ALU == 13L) else x[[v]]
    data.table(population = pop, variable = v, n_students = nrow(x),
      n_observed = sum(is.finite(x[[v]])), mean = mean(x[[v]], na.rm = TRUE),
      legacy_mean = mean(legacy, na.rm = TRUE), legacy_n_observed = sum(is.finite(legacy)))
  }))
}))
baseline[, reference_group := reference_map[population]]
reference[, variable := fifelse(var == "edad_alu", "EDAD_ALU", var)]
parity <- merge(baseline, reference[, .(reference_group = group, variable, reference_n = n,
                                      reference_mean = mean, reference_observed_pct = 100 * match)],
                by = c("reference_group", "variable"))
stopifnot(all(parity$n_students == parity$reference_n), max(abs(parity$legacy_mean - parity$reference_mean)) < 1e-10,
          max(abs(100 * parity$legacy_n_observed / parity$n_students - parity$reference_observed_pct)) < 1e-10)
parity[, valid_code_mean_difference := mean - legacy_mean]
fwrite(parity, file.path(out, "unstratified_balance_table_parity.csv"))

va <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
  "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
va <- merge(va[analysis_sample == "All" & is.finite(controlled_value_added_eb_centered_student)], specs, by = "outcome")
setnames(va, "controlled_value_added_eb_centered_student", "va_eb")
stopifnot(!anyDuplicated(va[, .(school_rbd, va_key)]))
# Fixed outcome-specific school quintiles, equal school weights, preserving ties.
# Use all finite-VA schools before matching to any student population.
va[, quintile := pmin(5L, as.integer(ceiling(5 * frank(va_eb, ties.method = "average") / .N))), by = va_key]
definitions <- va[, .(n_reference_schools = .N, min_va_eb = min(va_eb), max_va_eb = max(va_eb)),
                   by = .(va_key, va_label, quintile)]
stopifnot(nrow(definitions) == 20L)
fwrite(va, file.path(out, "school_va_quintile_assignments.csv"))
fwrite(definitions, file.path(out, "school_va_quintile_definitions.csv"))

stats <- counts <- coverage <- list()
for (o in specs$va_key) {
  v <- va[va_key == o]
  linked <- merge(d, v[, .(school_rbd, quintile)], by = "school_rbd", all.x = TRUE, sort = FALSE)
  stopifnot(nrow(linked) == nrow(d))
  for (pop in names(populations)) {
    x <- linked[get(pop) == TRUE]
    id <- paste(o, pop, sep = "_")
    coverage[[id]] <- data.table(population = pop, va_key = o, n_population = nrow(x),
      n_matched = sum(!is.na(x$quintile)), n_missing_school_va = sum(is.na(x$quintile)),
      pct_matched = 100 * mean(!is.na(x$quintile)))
    x <- x[!is.na(quintile)]
    counts[[id]] <- x[, .(n_students = .N, n_schools = uniqueN(school_rbd)), by = quintile][,
      `:=`(population = pop, va_key = o, student_share_pct = 100 * n_students / nrow(x))]
    stats[[id]] <- rbindlist(lapply(seq_len(nrow(variables)), function(i) {
      info <- variables[i]; z <- info$variable
      x[, .(n_students = .N, n_observed = sum(is.finite(get(z))),
             mean = mean(get(z), na.rm = TRUE), sd = sd(get(z), na.rm = TRUE)), by = quintile][,
        `:=`(population = pop, va_key = o, variable = z, characteristic = info$characteristic,
              multiplier = info$multiplier, digits = info$digits)]
    }))
  }
}
stats <- rbindlist(stats); counts <- rbindlist(counts); coverage <- rbindlist(coverage)
stats[, `:=`(n_missing = n_students - n_observed, observed_pct = 100 * n_observed / n_students,
              display_mean = multiplier * mean)]
stats[, cell := mapply(function(x, n) sprintf(paste0("%.", n, "f"), x), display_mean, digits)]
partition <- merge(counts[, .(n_quintile_sum = sum(n_students)), by = .(population, va_key)],
                    coverage, by = c("population", "va_key"))
stopifnot(nrow(stats) == 480L, nrow(counts) == 60L, all(partition$n_quintile_sum == partition$n_matched),
  all(stats$n_observed <= stats$n_students), all(counts$n_students > 0))
fwrite(stats, file.path(out, "baseline_characteristics_long.csv"))
fwrite(counts, file.path(out, "quintile_student_school_counts.csv"))
fwrite(coverage, file.path(out, "school_va_match_coverage.csv"))
fwrite(partition, file.path(out, "sample_partition_checks.csv"))

display <- rbindlist(list(
  counts[, .(population, va_key, quintile, characteristic = "Students", cell = format(n_students, big.mark = ",", trim = TRUE))],
  counts[, .(population, va_key, quintile, characteristic = "Schools", cell = format(n_schools, big.mark = ",", trim = TRUE))],
  stats[, .(population, va_key, quintile, characteristic, cell)]))
metric_order <- c("Students", "Schools", variables$characteristic)
display <- merge(display, specs[, .(va_key, va_label)], by = "va_key")
display[, `:=`(va_label = factor(va_label, levels = specs$va_label),
  characteristic = factor(characteristic, levels = rev(metric_order)),
  quintile = factor(quintile, levels = 1:5, labels = paste0("Q", 1:5)))]
for (pop in names(populations)) {
  x <- display[population == pop]
  wide <- dcast(x, va_key + characteristic ~ quintile, value.var = "cell")
  fwrite(wide, file.path(out, paste0(pop, "_table.csv")))
  p <- ggplot(x, aes(quintile, characteristic)) +
    geom_tile(aes(fill = as.integer(characteristic) %% 2), color = "white", linewidth = 0.5) +
    geom_text(aes(label = cell, fontface = fifelse(characteristic %in% c("Students", "Schools"), "bold", "plain")), size = 3.8) +
    facet_wrap(~va_label, ncol = 2) +
    scale_fill_gradient(low = "#FFFFFF", high = "#F1F4F7", guide = "none") +
    scale_x_discrete(position = "top", expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
    labs(title = "Baseline student composition by school VA quintile",
      subtitle = paste0(populations[[pop]], " | Current EB school VA | Including private schools | No PAES score restriction"),
      x = NULL, y = NULL,
      caption = paste0("Q1 = lowest VA; Q5 = highest. Outcome-specific quintiles give equal weight to schools; student means give equal weight to students.\n",
        "Students classified by most-attended high-school RBD. Means use observed values; income/education retain existing imputations.\n",
        "Students without the corresponding school VA are excluded. Full matching and variable-coverage tables accompany the results.")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(),
      axis.text.x = element_text(face = "bold", color = "#172B40", size = 12),
      axis.text.y = element_text(color = "#334155", size = 10),
      strip.text = element_text(face = "bold", color = "#172B40", size = 13, margin = margin(b = 9, t = 9)),
      strip.background = element_rect(fill = "#E6ECF2", color = NA),
      panel.spacing = grid::unit(1.5, "lines"),
      plot.title = element_text(face = "bold", size = 19, color = "#172B40"),
      plot.subtitle = element_text(size = 11, color = "#475569", margin = margin(b = 15)),
      plot.caption = element_text(hjust = 0, size = 9, color = "#5B6470", margin = margin(t = 14)),
      plot.margin = margin(15, 18, 14, 16))
  ggsave(file.path(fig, paste0(pop, "_baseline_by_va_quintile.png")), p, width = 14, height = 9.5, dpi = 220, bg = "white")
}

# Standalone table report, with population selector and per-cell nonmissing
# coverage; all cells come from the same CSV summaries as the figure.
sections <- vapply(names(populations), function(pop) {
  panels <- vapply(specs$va_key, function(o) {
    rows <- vapply(metric_order, function(m) {
      cells <- vapply(1:5, function(q) {
        z <- display[population == pop & va_key == o & as.character(quintile) == paste0("Q", q) & characteristic == m]
        stopifnot(nrow(z) == 1L)
        cov <- stats[population == pop & va_key == o & quintile == q & characteristic == m]
        small <- if (nrow(cov)) sprintf('<small>%.1f%% available</small>', cov$observed_pct) else ""
        paste0("<td>", z$cell, small, "</td>")
      }, character(1))
      paste0("<tr><th>", m, "</th>", paste(cells, collapse = ""), "</tr>")
    }, character(1))
    cvr <- coverage[population == pop & va_key == o]
    paste0('<article><h2>', specs[va_key == o, va_label], '</h2><table><thead><tr><th>Characteristic</th>',
      paste0("<th>Q", 1:5, "</th>", collapse = ""), '</tr></thead><tbody>', paste(rows, collapse = ""),
      sprintf('</tbody></table><p class="coverage">Matched %s of %s students (%.2f%%).</p></article>',
        format(cvr$n_matched, big.mark = ",", trim = TRUE), format(cvr$n_population, big.mark = ",", trim = TRUE), cvr$pct_matched))
  }, character(1))
  paste0('<section class="population" id="', pop, '" style="display:', if (pop == "broad_panel") "grid" else "none", '">',
         paste(panels, collapse = ""), '</section>')
}, character(1))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Baseline composition by VA quintile</title>',
 '<style>body{font:15px system-ui,sans-serif;margin:30px;color:#172b40;background:#f7f9fb}h1{font-size:27px}h2{font-size:19px}select{font:inherit;padding:7px;margin:10px 0 22px}.population{grid-template-columns:repeat(2,minmax(0,1fr));gap:22px}article{background:white;padding:20px;border:1px solid #dce3ea;border-radius:8px}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px 7px;text-align:right}th:first-child{text-align:left}thead{background:#e6ecf2}tbody tr:nth-child(even){background:#f1f4f7}small{display:block;font-size:10px;color:#667085;margin-top:2px}.coverage{font-size:12px;color:#667085}.notes{max-width:1100px;line-height:1.5}@media(max-width:1100px){.population{grid-template-columns:1fr}}</style></head><body>',
 '<h1>Baseline student composition by school VA quintile</h1><p>Current unified R EB estimates. Q1 is lowest VA; Q5 is highest VA.</p>',
 '<label>Population: <select onchange="document.querySelectorAll(\'.population\').forEach(s=>s.style.display=s.id===this.value?\'grid\':\'none\')">',
 paste0('<option value="', names(populations), '">', populations, '</option>', collapse = ""), '</select></label>',
 paste(sections, collapse = ""),
 '<div class="notes"><p>Grade 8 cohorts 2017-2020, age 12-16, positive most-attended high-school RBD, one row per student. Private schools included. No PAES/exam-taking or baseline-completeness restriction. SAE applicants are timely applicants in cohorts 2018-2020; at-risk students use the existing full lottery-risk roster.</p>',
 '<p>School quintiles use equal school weights and average ranks, separately by outcome, before student matching. Means give equal weight to students and use available values. Income and education retain existing baseline imputations. RM is Grade 8 residence in region 13. Female uses valid gender codes 1/2; one unusable code remains missing. Small percentages report nonmissing coverage, including imputed values where applicable. Matching coverage differs by outcome because school VA is not available everywhere. These are descriptive composition differences.</p></div></body></html>')
writeLines(html, file.path(tab, "baseline_by_va_quintile.html"), useBytes = TRUE)
stopifnot(length(gregexpr("<table>", html, fixed = TRUE)[[1]]) == 12L)
print(coverage)
print(stats[population == "broad_panel" & quintile %in% c(1L, 5L), .(va_key, quintile, characteristic, display_mean, observed_pct)])
