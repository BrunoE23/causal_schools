suppressPackageStartupMessages(library(data.table))
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "sae_application_geography", "q1_exposure")
tab <- file.path(repo, "output", "tables", "sae_application_geography", "q1_exposure")
for (p in c(out, tab)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
students <- readRDS(file.path(repo, "data", "clean", "baseline_by_va_quintile", "baseline_student_input.rds"))
setDT(students)
students <- students[sae_applicants == TRUE, .(mrun, sae_proceso, at_risk)]
stopifnot(nrow(students) == 294198L, sum(students$at_risk) == 162633L,
          !anyDuplicated(students[, .(mrun, sae_proceso)]))

# The three C1 files are about 48/87/78 MB. Select five columns and filter to
# the existing applicant sample before any join. These are actual submitted
# preferences; system-added continuity entries are excluded, as in the last table.
raw <- rbindlist(lapply(2018:2020, function(year) {
  p <- list.files(file.path(root, "data", "raw", year, paste0("SAE_", year)),
    pattern = "^C1_.*[.]csv$", full.names = TRUE)
  stopifnot(length(p) == 1L)
  x <- fread(p, select = c("mrun", "cod_nivel", "rbd", "preferencia_postulante", "agregada_por_continuidad"),
             na.strings = c("", "NA"), showProgress = FALSE)
  x <- x[cod_nivel == 9L & agregada_por_continuidad == 0L & mrun %in% students[sae_proceso == year, mrun]]
  x[, .(mrun, sae_proceso = year, school_rbd = as.numeric(rbd), preference = preferencia_postulante)]
}))
stopifnot(!anyNA(raw), all(raw$school_rbd > 0), all(raw$preference >= 1),
          all(raw$preference == as.integer(raw$preference)))
# Multiple programs at one school inherit the school's best original rank.
# Keep original preference positions: top three means raw C1 rank <= 3,
# rather than reranking distinct RBDs after dropping duplicate programs.
choices <- raw[, .(best_preference = min(preference), n_program_rows = .N), by = .(mrun, sae_proceso, school_rbd)]
choices <- merge(choices, students, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(choices$at_risk), nrow(choices) == 1007253L)
first <- choices[best_preference == 1L, .N, by = .(mrun, sae_proceso)]
stopifnot(nrow(first) == nrow(students), all(first$N == 1L))
fwrite(choices[, .(n_students = uniqueN(mrun), n_unique_school_choices = .N,
  n_program_rows = sum(n_program_rows), n_school_choices_collapsing_programs = sum(n_program_rows > 1L)), by = sae_proceso],
  file.path(out, "preference_rank_audit.csv"))
rm(raw)

specs <- data.table(va_key = c("math", "verbal", "field", "institution"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1", "high_inst_m1"),
  label = c("Math", "Verbal", "High-premium field", "High-premium institution"))
assigned <- fread(file.path(repo, "data", "clean", "baseline_by_va_quintile", "school_va_quintile_assignments.csv"))
va <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
  "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
va <- merge(va[analysis_sample == "All" & is.finite(controlled_value_added_eb_centered_student)], specs, by = "outcome")
check <- merge(assigned, va, by = c("school_rbd", "outcome", "va_key"))
stopifnot(nrow(check) == nrow(assigned), nrow(check) == nrow(va),
          max(abs(check$va_eb - check$controlled_value_added_eb_centered_student)) < 1e-10)
# Preserve the preceding outcome-specific equal-school-weight quintiles over
# all finite current EB school VAs, including private schools. No reranking
# within regions, SAE schools, application lists or risk groups.
fwrite(assigned[, .(n_reference_schools = .N, n_q1_schools = sum(quintile == 1L),
  q1_max_va = max(va_eb[quintile == 1L])), by = va_key], file.path(out, "q1_school_definitions.csv"))

exposure <- copy(students)
results <- list()
windows <- c("Any listed school", "Top 3", "Top 1")
for (i in seq_len(nrow(specs))) {
  s <- specs[i]
  v <- assigned[va_key == s$va_key]
  choices[, school_quintile := v$quintile[match(school_rbd, v$school_rbd)]]
  choices[, `:=`(is_q1 = !is.na(school_quintile) & school_quintile == 1L, unknown_va = is.na(school_quintile))]
  for (j in seq_along(windows)) {
    w <- if (j == 1L) choices else if (j == 2L) choices[best_preference <= 3L] else choices[best_preference == 1L]
    profile <- w[, .(n_choices = .N, n_q1 = sum(is_q1), n_unknown = sum(unknown_va)), by = .(mrun, sae_proceso)]
    profile <- merge(students, profile, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
    stopifnot(nrow(profile) == nrow(students), !anyNA(profile$n_choices), all(profile$n_choices > 0L))
    profile[, q1_exposure := fifelse(n_q1 > 0L, 1L, fifelse(n_unknown == 0L, 0L, NA_integer_))]
    exposure_col <- paste0(s$va_key, "_", c("any", "top3", "top1")[j])
    exposure[profile, on = .(mrun, sae_proceso), (exposure_col) := i.q1_exposure]
    for (g in c("All SAE applicants", "At risk")) {
      x <- if (g == "At risk") profile[at_risk == TRUE] else profile
      n_yes <- sum(x$q1_exposure == 1L, na.rm = TRUE)
      n_no <- sum(x$q1_exposure == 0L, na.rm = TRUE)
      n_unknown <- sum(is.na(x$q1_exposure))
      stopifnot(n_yes + n_no + n_unknown == nrow(x))
      results[[paste(s$va_key, j, g)]] <- data.table(va_key = s$va_key, outcome_label = s$label, window = windows[j], group = g,
        n_students = nrow(x), n_yes_q1 = n_yes, n_no_q1 = n_no, n_unknown_q1 = n_unknown,
        pct_confirmed_q1_all = 100 * n_yes / nrow(x), pct_possible_q1_all = 100 * (n_yes + n_unknown) / nrow(x),
        pct_q1_among_classified = 100 * n_yes / (n_yes + n_no),
        n_classified_students = n_yes + n_no, n_school_choices = sum(x$n_choices),
        n_unknown_va_school_choices = sum(x$n_unknown))
    }
  }
  a <- exposure[[paste0(s$va_key, "_any")]]
  b <- exposure[[paste0(s$va_key, "_top3")]]
  c <- exposure[[paste0(s$va_key, "_top1")]]
  stopifnot(all(a[which(b == 1L)] == 1L), all(b[which(c == 1L)] == 1L))
}
results <- rbindlist(results)
stopifnot(nrow(results) == 24L)
fwrite(results, file.path(out, "q1_list_exposure_summary.csv"))
saveRDS(exposure, file.path(out, "student_q1_exposure.rds"))
results[, interval := sprintf("%.2f-%.2f%%", pct_confirmed_q1_all, pct_possible_q1_all)]
fwrite(dcast(results, group + va_key + outcome_label ~ window, value.var = "interval"),
       file.path(out, "q1_list_exposure_bounds_table.csv"))
fwrite(dcast(results, group + va_key + outcome_label ~ window, value.var = "pct_q1_among_classified"),
       file.path(out, "q1_list_exposure_classified_table.csv"))

sections <- vapply(c("All SAE applicants", "At risk"), function(g) {
  rows <- vapply(specs$va_key, function(o) {
    a <- results[group == g & va_key == o][match(windows, window)]
    cells <- paste0('<td>', a$interval, '<small>', format(a$n_yes_q1, big.mark = ",", trim = TRUE),
      ' confirmed; ', format(a$n_unknown_q1, big.mark = ",", trim = TRUE), ' unclassified</small></td>', collapse = "")
    paste0('<tr><th>', specs[va_key == o, label], '</th>', cells, '</tr>')
  }, character(1))
  paste0('<h2>', g, ' (N=', format(results[group == g, n_students][1], big.mark = ",", trim = TRUE), ')</h2>',
    '<table><thead><tr><th>Q1 defined using</th><th>Anywhere in list</th><th>Top 3</th><th>Top 1</th></tr></thead><tbody>',
    paste(rows, collapse = ""), '</tbody></table>')
}, character(1))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Q1 school exposure in SAE lists</title>',
 '<style>body{font:16px system-ui,sans-serif;max-width:1200px;margin:35px auto;padding:0 20px;color:#172b40}h1{font-size:27px}h2{font-size:21px;margin-top:30px}table{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums}th,td{padding:12px;text-align:right}th:first-child{text-align:left}thead{background:#e6ecf2}tbody tr:nth-child(even){background:#f1f4f7}small{display:block;font-size:11px;color:#667085;margin-top:4px}.notes{font-size:14px;line-height:1.6;color:#475569}</style></head><body>',
 '<h1>Students listing at least one Q1 school</h1><p>Four separate current EB VA rankings; Q1 is the lowest quintile. Actual submitted Grade 9 SAE preferences, 2018-2020.</p>',
 paste(sections, collapse = ""),
 '<div class="notes"><p>Percentages use all applicants in each group. An interval gives the confirmed share and the maximum possible share if every unclassified student listed a Q1 school. Exposure is confirmed even if another school in the window lacks VA; it is unclassified only when no known Q1 school is present and at least one school has missing VA. The CSVs also report shares among students whose exposure can be classified.</p>',
 '<p>Quintiles are the same outcome-specific, equal-school-weight current unified R EB quintiles used in the previous baseline tables. They are not recomputed within SAE, regions or risk groups. Automatically added continuity options are excluded; other priority categories remain. Multiple programs at one RBD inherit that school\'s best original preference position. Top 3 uses original C1 preference rank at most 3; Top 1 uses rank 1. Every applicant has exactly one submitted first-ranked school. There is no home-region, PAES or baseline-completeness restriction.</p></div></body></html>')
writeLines(html, file.path(tab, "q1_school_exposure_in_application_lists.html"), useBytes = TRUE)
print(results[, .(group, outcome_label, window, n_students, n_yes_q1, n_unknown_q1,
                  pct_confirmed_q1_all, pct_possible_q1_all, pct_q1_among_classified)])
