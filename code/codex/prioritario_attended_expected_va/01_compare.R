suppressPackageStartupMessages({library(data.table); library(fixest)})
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "prioritario_attended_expected_va")
tab <- file.path(repo, "output", "tables", "prioritario_attended_expected_va")
for (p in c(out, tab)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2); setFixest_nthreads(2)
specs <- data.table(va_key = c("math", "verbal", "field", "institution"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1", "high_inst_m1"),
  e_col = c("expected_math_adj_eb", "expected_leng_adj_eb", "expected_highpay_adj_eb", "expected_highinst_adj_eb"),
  label = c("Math", "Verbal", "High-premium field", "High-premium institution"),
  multiplier = c(1, 1, 100, 100), units = c("SD", "SD", "pp", "pp"))
# Cached full SAE-applicant E construction was validated against every current
# main-IV at-risk student. It retains applicants without PAES/baseline scores.
d <- readRDS(file.path(repo, "data", "clean", "baseline_by_va_quintile", "expected_va", "sae_applicants_with_expected_va.rds"))
setDT(d)
stopifnot(nrow(d) == 294198L, !anyDuplicated(d$mrun), all(d$sae_applicants))

sources <- list()
priority <- rbindlist(lapply(2018:2020, function(year) {
  paths <- list.files(file.path(root, "data", "raw", year, paste0("SAE_", year)),
    pattern = "^B1_Postulantes_etapa_regular_.*PUBL\\.csv$", full.names = TRUE)
  stopifnot(length(paths) == 1L)
  # Three files total about 56 MB; only three original columns are read.
  x <- fread(paths[[1]], select = c("mrun", "cod_nivel", "prioritario"), sep = ";", dec = ",",
              na.strings = c("", "NA"), showProgress = FALSE)
  x <- x[cod_nivel == 9L, .(mrun = as.numeric(mrun), sae_proceso = year, prioritario = as.integer(prioritario))]
  sources[[as.character(year)]] <<- data.table(sae_proceso = year, source = paths[[1]], n_grade9_rows = nrow(x))
  x
}))
stopifnot(all(priority$prioritario[!is.na(priority$prioritario)] %in% 0:1))
valid <- unique(priority[!is.na(prioritario), .(mrun, sae_proceso, prioritario)])
conflicts <- valid[, .N, by = .(mrun, sae_proceso)][N > 1L]
# Do not arbitrarily select a status if the original file contains conflicting
# records. Such records would remain missing in the left join and be audited.
if (nrow(conflicts)) valid <- valid[!conflicts, on = .(mrun, sae_proceso)]
stopifnot(!anyDuplicated(valid[, .(mrun, sae_proceso)]))
fwrite(rbindlist(sources), file.path(out, "priority_source_files.csv"))
fwrite(conflicts, file.path(out, "conflicting_priority_records.csv"))
d <- merge(d, valid, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == 294198L)
fwrite(d[, .(n_applicants = .N, n_priority_known = sum(!is.na(prioritario)),
  n_priority_missing = sum(is.na(prioritario)), n_prioritario = sum(prioritario == 1L, na.rm = TRUE),
  n_non_prioritario = sum(prioritario == 0L, na.rm = TRUE))], file.path(out, "priority_coverage.csv"))

va <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
  "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
va <- merge(va[analysis_sample == "All" & is.finite(controlled_value_added_eb_centered_student)], specs, by = "outcome")
setnames(va, "controlled_value_added_eb_centered_student", "va_eb")
stopifnot(!anyDuplicated(va[, .(school_rbd, va_key)]))
# Confirm that the school values have not changed since construction of E.
old_va <- fread(file.path(repo, "data", "clean", "baseline_by_va_quintile", "school_va_quintile_assignments.csv"))
source_check <- merge(va, old_va[, .(school_rbd, va_key, old_value = va_eb)], by = c("school_rbd", "va_key"))
stopifnot(nrow(source_check) == nrow(va), nrow(source_check) == nrow(old_va),
          max(abs(source_check$va_eb - source_check$old_value)) < 1e-10)

results <- coefficients <- coverage <- expected_all <- list()
for (i in seq_len(nrow(specs))) {
  s <- specs[i]
  v <- va[va_key == s$va_key]
  x <- copy(d)
  x[, `:=`(attended_va = v$va_eb[match(school_rbd, v$school_rbd)], expected_va = get(s$e_col))]
  expected_all[[s$va_key]] <- x[!is.na(prioritario) & is.finite(expected_va),
    .(n_students = .N, mean_e = mean(expected_va), mean_e_display = s$multiplier * mean(expected_va)), by = prioritario][,
      `:=`(va_key = s$va_key, units = s$units)]
  coverage[[s$va_key]] <- x[, .(va_key = s$va_key, n_applicants = .N,
    n_known_priority = sum(!is.na(prioritario)), n_missing_attended_va = sum(!is.finite(attended_va)),
    n_missing_e = sum(!is.finite(expected_va)),
    n_common_sample = sum(!is.na(prioritario) & is.finite(attended_va) & is.finite(expected_va)))]
  # Exactly the same per-outcome sample for raw A, adjusted A and displayed E.
  x <- x[!is.na(prioritario) & is.finite(attended_va) & is.finite(expected_va)]
  stopifnot(uniqueN(x$prioritario) == 2L)
  raw <- feols(attended_va ~ prioritario, data = x, vcov = ~school_rbd, notes = FALSE)
  e_model <- feols(expected_va ~ prioritario, data = x, vcov = ~school_rbd, notes = FALSE)
  adjusted <- feols(attended_va ~ prioritario + expected_va, data = x, vcov = ~school_rbd, notes = FALSE)
  ebar <- mean(x$expected_va)
  cf <- coef(adjusted)
  adjusted_nonp <- unname(cf["(Intercept)"] + cf["expected_va"] * ebar)
  adjusted_p <- adjusted_nonp + unname(cf["prioritario"])
  m_a0 <- x[prioritario == 0L, mean(attended_va)]; m_a1 <- x[prioritario == 1L, mean(attended_va)]
  m_e0 <- x[prioritario == 0L, mean(expected_va)]; m_e1 <- x[prioritario == 1L, mean(expected_va)]
  stopifnot(nobs(raw) == nrow(x), nobs(adjusted) == nrow(x), nobs(e_model) == nrow(x),
    abs(unname(coef(raw)["prioritario"]) - (m_a1 - m_a0)) < 1e-10,
    abs(unname(coef(e_model)["prioritario"]) - (m_e1 - m_e0)) < 1e-10,
    abs((m_a1 - m_a0) - (cf["prioritario"] + cf["expected_va"] * (m_e1 - m_e0))) < 1e-10,
    abs(weighted.mean(c(adjusted_nonp, adjusted_p), c(sum(x$prioritario == 0L), sum(x$prioritario == 1L))) - mean(x$attended_va)) < 1e-10)
  results[[s$va_key]] <- data.table(va_key = s$va_key, outcome_label = s$label, units = s$units,
    n_non_prioritario = sum(x$prioritario == 0L), n_prioritario = sum(x$prioritario == 1L), n_total = nrow(x),
    attended_non_prioritario = s$multiplier * m_a0, attended_prioritario = s$multiplier * m_a1,
    attended_raw_gap = s$multiplier * (m_a1 - m_a0),
    attended_e_adjusted_non_prioritario = s$multiplier * adjusted_nonp,
    attended_e_adjusted_prioritario = s$multiplier * adjusted_p,
    attended_e_adjusted_gap = s$multiplier * unname(cf["prioritario"]),
    e_non_prioritario = s$multiplier * m_e0, e_prioritario = s$multiplier * m_e1,
    e_gap = s$multiplier * (m_e1 - m_e0), e_slope = unname(cf["expected_va"]), e_at_adjustment = ebar)
  coefficients[[s$va_key]] <- rbindlist(lapply(c("raw_attended", "e_adjusted_attended", "raw_e"), function(kind) {
    model <- switch(kind, raw_attended = raw, e_adjusted_attended = adjusted, raw_e = e_model)
    data.table(va_key = s$va_key, model = kind, term = names(coef(model)), coefficient = unname(coef(model)),
      se = unname(se(model)), n_obs = nobs(model), vcov = "Clustered by most-attended school RBD; EB values treated as fixed")
  }))
}
results <- rbindlist(results)
all_e <- rbindlist(expected_all)
all_e_wide <- dcast(all_e, va_key ~ prioritario, value.var = "mean_e_display")
setnames(all_e_wide, c("0", "1"), c("e_all_non_prioritario", "e_all_prioritario"))
all_e_wide[, e_all_gap := e_all_prioritario - e_all_non_prioritario]
results <- merge(results, all_e_wide, by = "va_key", sort = FALSE)
fwrite(results, file.path(out, "priority_attended_and_expected_va_comparison.csv"))
fwrite(rbindlist(coefficients), file.path(out, "priority_regression_coefficients.csv"))
fwrite(rbindlist(coverage), file.path(out, "outcome_sample_coverage.csv"))
fwrite(all_e, file.path(out, "expected_va_all_known_priority_applicants.csv"))

fmt <- function(z, units) sprintf(if (units == "SD") "%.3f" else "%.2f", z)
make_table <- function(title, columns, headings) {
  rows <- vapply(seq_len(nrow(results)), function(i) {
    r <- results[i]
    paste0('<tr><th>', r$outcome_label, ' (', r$units, ')</th>',
      paste(vapply(columns, function(k) paste0('<td>', fmt(r[[k]], r$units), '</td>'), character(1)), collapse = ""), '</tr>')
  }, character(1))
  paste0('<h2>', title, '</h2><table><thead><tr><th>School VA outcome</th>',
    paste0('<th>', headings, '</th>', collapse = ""), '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table>')
}
tables <- c(make_table("Attended-school VA: raw means", c("attended_non_prioritario", "attended_prioritario", "attended_raw_gap"),
                      c("Non-prioritario", "Prioritario", "Gap: P minus non-P")),
  make_table("Attended-school VA: adjusted for E", c("attended_e_adjusted_non_prioritario", "attended_e_adjusted_prioritario", "attended_e_adjusted_gap"),
             c("Non-prioritario", "Prioritario", "Adjusted gap")),
  make_table("Expected school VA (E): same attended-VA sample", c("e_non_prioritario", "e_prioritario", "e_gap"),
             c("Non-prioritario", "Prioritario", "Gap: P minus non-P")),
  make_table("Expected school VA (E): all SAE applicants", c("e_all_non_prioritario", "e_all_prioritario", "e_all_gap"),
             c("Non-prioritario", "Prioritario", "Gap: P minus non-P")))
ns <- paste0('<table><thead><tr><th>Outcome</th><th>Non-prioritario N</th><th>Prioritario N</th></tr></thead><tbody>',
  paste(vapply(seq_len(nrow(results)), function(i) paste0('<tr><th>', results$outcome_label[i], '</th><td>',
    format(results$n_non_prioritario[i], big.mark = ",", trim = TRUE), '</td><td>',
    format(results$n_prioritario[i], big.mark = ",", trim = TRUE), '</td></tr>'), character(1)), collapse = ""), '</tbody></table>')
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>School VA by prioritario status</title>',
 '<style>body{font:16px system-ui,sans-serif;max-width:1100px;margin:35px auto;padding:0 20px;color:#172b40}h1{font-size:27px}h2{font-size:21px;margin-top:30px}table{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums}th,td{padding:11px;text-align:right}th:first-child{text-align:left}thead{background:#e6ecf2}tbody tr:nth-child(even){background:#f1f4f7}.notes{font-size:14px;line-height:1.6;color:#475569}</style></head><body>',
 '<h1>Attended and expected school VA by SAE prioritario status</h1><p>Current unified R EB estimates; timely SAE applicants, 2018-2020.</p>',
 paste(tables, collapse = ""), '<h2>Common sample sizes</h2>', ns,
 '<div class="notes"><p>Within each outcome, raw attended VA, adjusted attended VA, and the first E panel use the same students: known SAE prioritario status and finite attended VA and E. The second E panel instead uses all 294,198 SAE applicants (113,185 non-prioritario and 181,013 prioritario), without requiring attended-VA coverage. No income, PAES, baseline-completeness or at-risk-only restriction is imposed. Prioritario comes from the Grade 9 SAE B1 file in the application year. Missing or conflicting priority status is not coded non-prioritario.</p>',
 '<p>The adjusted model is attended VA = intercept + prioritario coefficient + slope times matching outcome E. Adjusted group means evaluate both groups at the same pooled-sample mean E. No additional controls or group-specific E slopes are included. These are descriptive conditional differences, not causal effects. SD denotes test-score SD; pp denotes percentage points of binary-outcome VA.</p>',
 '<p>E uses the current main-IV probability-weighted EB definition, with unavailable VA contributing zero and no probability renormalization. Additional CSVs provide E means for all known-priority applicants without requiring attended-VA coverage. Regression covariance is clustered by most-attended school, treating the saved EB indices as fixed; this does not propagate VA-estimation uncertainty.</p></div></body></html>')
writeLines(html, file.path(tab, "prioritario_attended_expected_va.html"), useBytes = TRUE)
stopifnot(nrow(results) == 4L, length(gregexpr("<table>", html, fixed = TRUE)[[1]]) == 5L)
print(results)
print(rbindlist(coverage))
