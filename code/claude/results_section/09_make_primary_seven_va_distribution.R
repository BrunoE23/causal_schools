###############################################################################
# Paper-facing distribution table for all SEVEN primary outcomes (extends
# code/codex/results_section/09_make_primary_five_va_distribution.R, which
# only covered 5: math, verbal, high-premium institution, high-premium
# field, log projected income).
#
# Adds:
#   - admission_exam_taker ("Admission exam taken") -- this one is easy:
#     it already has a Stata EB regression (stata_eb_school_values_exam.csv)
#     built the exact same way as the original 5, so it's handled by the
#     SAME code path, just one more row in `specs`.
#   - any_postulacion ("Financial aid application") -- this one is NOT in
#     the Stata EB pipeline at all (06_combine_stata_va_eb_outputs.R's
#     outcome_keys list has no "anypost"/"postulacion" entry, and it is not
#     a column in stata_va_analytic_cache.dta). It DOES have a per-school EB
#     value added estimate, but from the OTHER (R/lfe-based) EB pipeline --
#     the one that actually feeds the paper's headline IV results
#     (main_table.tex / main_results.csv), via
#     code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R's
#     output eb_school_rbd_observational_values.csv (column
#     controlled_value_added_eb_centered_student, weight
#     n_students_regression). This is a DIFFERENT EB implementation
#     (lfe::getfe + bootstrap SEs) than the Stata one (areg + unadjusted
#     VCE) used for the other 6 rows -- a same-design cross-language parity
#     check per that pipeline's own README, not a competing estimator -- but
#     it is the one actually used elsewhere in the paper for this outcome,
#     so it is the right source here.
#   - A new "Debiased SD (RSS) [SE]" column for ALL SEVEN rows, from the
#     cross-cohort RSS Var/Cov exercise (rss_varcov_matrix.csv, produced by
#     code/claude/rss_debiased_varcov/01_construct_rss_debiased_varcov.R),
#     alongside the existing EB-shrunken "EB SD" column. This directly
#     supports the Corrected Variance/Covariance Matrix subsection's
#     EB-vs-debiased-SD comparison by putting both numbers in one table
#     instead of two.
#
# Run AFTER: 09_make_primary_five_va_distribution.R's own inputs are already
# built (Stata EB CSVs + stata_va_analytic_cache.dta), AND
# 01_construct_eb_school_values.R has produced
# eb_school_rbd_observational_values.csv, AND
# code/claude/rss_debiased_varcov/01_construct_rss_debiased_varcov.R has
# produced rss_varcov_matrix.csv.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

log_ts <- function(...) {
  message(format(Sys.time(), "[%H:%M:%S] "), ...)
  flush(stderr())
}

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }
  candidates[[1]]
}

find_existing_file <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    stop(
      "Could not find ", label, ". Checked:\n  ",
      paste(candidates, collapse = "\n  "),
      "\nSet ", env_var, "."
    )
  }
  candidates[[1]]
}

repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

results_dir <- file.path(repo_wd, "output", "tables", "results_section")
eb_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
cache_path <- file.path(eb_dir, "stata_va_analytic_cache.dta")
output_csv <- file.path(results_dir, "primary_seven_va_distribution.csv")
output_tex <- file.path(results_dir, "primary_seven_va_distribution.tex")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

clean_dir <- file.path(data_wd, "data", "clean")
eb_r_path <- find_existing_file(
  "EB_SCHOOL_RBD_OBS_VALUES_PATH",
  file.path(clean_dir, "empirical_bayes_school_va", "cohorts_2017_2020", "eb_school_rbd_observational_values.csv"),
  "eb_school_rbd_observational_values.csv (run code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R first)"
)
rss_sample_path <- find_existing_file(
  "RSS_FOUR_YEAR_NO_SAE_SAMPLE_PATH",
  file.path(clean_dir, "rss_debiased_varcov", "four_year_no_sae_sample.dta"),
  "four_year_no_sae_sample.dta (run code/claude/rss_debiased_varcov/00_build_four_year_no_sae_sample.R first)"
)
rss_varcov_path <- find_existing_file(
  "RSS_VARCOV_MATRIX_PATH",
  file.path(clean_dir, "rss_debiased_varcov", "four_year_no_sae", "rss_varcov_matrix.csv"),
  "rss_varcov_matrix.csv (run code/claude/rss_debiased_varcov/01_construct_rss_debiased_varcov.R first)"
)

# ------------------------- SPECS: 7 outcomes w/ Stata EB pipeline -----------
# Same as the original 5-outcome script, plus admission_exam_taker, which
# already has a Stata EB regression built the identical way.
specs <- data.table(
  outcome_key = c("math", "language", "exam", "enrolled", "highinst", "highpay", "program_income_full"),
  outcome     = c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "higher_ed_enrolled_m1", "high_inst_m1", "highpay_field_m1", "log_proginc_full_clp_m1"),
  eb_outcome  = c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "higher_ed_enrolled_m1", "high_inst_m1", "high_paying_field_m1", "log_program_income_full_clp_m1"),
  label       = c("Math", "Verbal", "Admission exam taken", "Higher-ed enrollment", "High-premium institution", "High-premium field", "Log proj. income"),
  # Join key into rss_varcov_matrix.csv's outcome_1/outcome_2 -- distinct
  # from `outcome`/`eb_outcome` because the RSS script uses yet a THIRD
  # alias for income (log_program_income_clp_m1, no "_full"; confirmed the
  # same underlying variable via 03_construct_person_level_income_outcomes.R's
  # own "backward-compatible alias" comment) and the RAW-column name for
  # high-pay field (high_paying_field_m1, not highpay_field_m1).
  rss_outcome = c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "higher_ed_enrolled_m1", "high_inst_m1", "high_paying_field_m1", "log_program_income_clp_m1")
)
specs[, eb_path := file.path(eb_dir, paste0("stata_eb_school_values_", outcome_key, ".csv"))]

weighted_quantile <- function(x, w, probs) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

weighted_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  mu <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

format_value <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))

controls <- c(
  "cohort_gr8", "GEN_ALU", "EDAD_ALU", "COD_COM_ALU", "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed", "father_indigenous_imputed",
  "mother_indigenous_imputed", "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed",
  "kinder_imputed", "z_gpa_middle_mean", "z_att_middle_mean", "middle_years_observed",
  "z_sim_mat_4to", "z_sim_mat_4to_sq", "z_sim_mat_4to_cu", "z_sim_leng_4to",
  "z_sim_leng_4to_sq", "z_sim_leng_4to_cu", "school_rbd", "most_time_RBD_middle"
)

missing_paths <- c(cache_path, specs$eb_path)[!file.exists(c(cache_path, specs$eb_path))]
if (length(missing_paths) > 0) stop("Missing inputs:\n", paste(missing_paths, collapse = "\n"))

cache <- as.data.table(read_dta(cache_path))
cache <- cache[cohort_gr8 >= 2017 & cohort_gr8 <= 2020]
missing_vars <- setdiff(c(specs$outcome, controls), names(cache))
if (length(missing_vars) > 0) stop("Cache missing: ", paste(missing_vars, collapse = ", "))

rows_1to6 <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  spec <- specs[i]
  eb <- fread(spec$eb_path, select = c("outcome", "va_eb_centered", "n_students", "n_total"))
  eb <- eb[outcome == spec$eb_outcome]
  if (nrow(eb) == 0) stop("No EB values for ", spec$outcome_key)

  sample_vars <- c(spec$outcome, controls)
  sample <- cache[
    complete.cases(cache[, ..sample_vars]) & is.finite(get(spec$outcome))
  ]
  expected_n <- unique(eb$n_total)
  if (length(expected_n) != 1L || nrow(sample) != expected_n) {
    stop("Regression-sample mismatch for ", spec$outcome_key, ": reconstructed N=", nrow(sample), ", Stata N=", paste(expected_n, collapse = ", "))
  }

  q <- weighted_quantile(eb$va_eb_centered, eb$n_students, c(0.10, 0.25, 0.50, 0.75, 0.90))
  data.table(
    outcome_key = spec$outcome_key,
    outcome = spec$outcome,
    rss_outcome = spec$rss_outcome,
    label = spec$label,
    sample_mean = mean(sample[[spec$outcome]]),
    sample_sd = sd(sample[[spec$outcome]]),
    eb_va_sd = weighted_sd(eb$va_eb_centered, eb$n_students),
    p10 = q[[1]], p25 = q[[2]], p50 = q[[3]], p75 = q[[4]], p90 = q[[5]],
    n_students = nrow(sample), n_schools = nrow(eb),
    eb_source = "stata"
  )
}))

# ------------------------- 7th ROW: any_postulacion -------------------------
# No Stata EB regression exists for this outcome (confirmed via
# 06_combine_stata_va_eb_outputs.R's hardcoded outcome_keys list, which has
# no "anypost"/"postulacion" entry, and it is not a column in
# stata_va_analytic_cache.dta). Source its EB VA instead from the R/lfe-based
# pipeline that actually feeds the paper's headline main_table.tex results
# for this outcome (spec anypost_adj_eb in
# code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R).
log_ts("Loading any_postulacion EB values: ", eb_r_path)
eb_r <- fread(eb_r_path, select = c("outcome", "controlled_value_added_eb_centered_student", "n_students_regression"))
eb_postulacion <- eb_r[outcome == "any_postulacion" & is.finite(controlled_value_added_eb_centered_student) & is.finite(n_students_regression) & n_students_regression > 0]
if (nrow(eb_postulacion) == 0) stop("No EB values for any_postulacion in ", eb_r_path)

# Student-level sample mean/SD: no covariate-complete "regression esample"
# is available off the shelf for this outcome outside a full pipeline rerun,
# so use the RSS build sample instead (same cohort-2017-2020 restriction the
# debiased SD below is itself estimated on), rather than mixing in a
# different sample definition. Flagged in the table notes.
log_ts("Loading RSS four_year_no_sae sample for any_postulacion sample moments: ", rss_sample_path)
rss_sample <- as.data.table(read_dta(rss_sample_path))
postulacion_vals <- rss_sample[["any_postulacion"]]
postulacion_vals <- postulacion_vals[is.finite(postulacion_vals)]

q_post <- weighted_quantile(
  eb_postulacion$controlled_value_added_eb_centered_student,
  eb_postulacion$n_students_regression,
  c(0.10, 0.25, 0.50, 0.75, 0.90)
)
row_7 <- data.table(
  outcome_key = "postulacion",
  outcome = "any_postulacion",
  rss_outcome = "any_postulacion",
  label = "Financial aid application",
  sample_mean = mean(postulacion_vals),
  sample_sd = sd(postulacion_vals),
  eb_va_sd = weighted_sd(eb_postulacion$controlled_value_added_eb_centered_student, eb_postulacion$n_students_regression),
  p10 = q_post[[1]], p25 = q_post[[2]], p50 = q_post[[3]], p75 = q_post[[4]], p90 = q_post[[5]],
  n_students = length(postulacion_vals), n_schools = nrow(eb_postulacion),
  eb_source = "r_lfe"
)

rows <- rbind(rows_1to6, row_7)

# ------------------------- MERGE: debiased SD from RSS Var/Cov -------------
log_ts("Loading RSS varcov matrix: ", rss_varcov_path)
varcov_dt <- fread(rss_varcov_path)
diag_dt <- varcov_dt[type == "variance"]
rss_sd_lookup <- setNames(sqrt(diag_dt$estimate), diag_dt$outcome_1)
rss_sd_se_lookup <- setNames(diag_dt$bootstrap_se / (2 * sqrt(diag_dt$estimate)), diag_dt$outcome_1)

rows[, debiased_sd := rss_sd_lookup[rss_outcome]]
rows[, debiased_sd_se := rss_sd_se_lookup[rss_outcome]]

fwrite(rows, output_csv)
log_ts("Wrote: ", output_csv)

# ------------------------- LATEX TABLE --------------------------------------
format_debiased <- function(sd, se) {
  ifelse(is.na(sd), "", sprintf("%.3f [%.3f]", sd, se))
}

latex_rows <- vapply(seq_len(nrow(rows)), function(i) {
  eb_cell <- format_value(rows$eb_va_sd[[i]])
  debiased_cell <- format_debiased(rows$debiased_sd[[i]], rows$debiased_sd_se[[i]])
  pct_cells <- format_value(unlist(rows[i, .(p10, p25, p50, p75, p90)]))
  paste0(
    rows$label[[i]], " & ",
    format_value(rows$sample_mean[[i]]), " & ", format_value(rows$sample_sd[[i]]), " & ",
    eb_cell, " & ", debiased_cell, " & ",
    paste(pct_cells, collapse = " & "), " \\\\"
  )
}, character(1))

latex <- c(
  "\\begin{table}[!htbp]", "\\centering", "\\begin{threeparttable}",
  "\\caption{Distribution of primary school value added: EB-shrunken and debiased}",
  "\\label{tab:school-va-distribution}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\footnotesize", "\\setlength{\\tabcolsep}{2pt}",
  "\\begin{tabular}{lcc|ccccccc}", "\\toprule",
  " & \\multicolumn{2}{c|}{Outcome distribution} & \\multicolumn{7}{c}{Value-added distribution} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-10}",
  "Outcome & \\shortstack{Sample\\\\Mean} & \\shortstack{Sample\\\\SD} & EB SD & \\shortstack{Debiased SD\\\\{[}bootstrap SE{]}} & P10 & P25 & P50 & P75 & P90 \\\\",
  "\\midrule", latex_rows, "\\bottomrule", "\\end{tabular}}",
  "\\begin{tablenotes}[flushleft]", "\\footnotesize",
  "\\item[] \\parbox{0.95\\textwidth}{Notes: The table reports, for all seven primary outcomes, student-weighted distributions of Empirical-Bayes (EB) shrunken, student-centered school value added (``EB SD'' and P10--P90), alongside the debiased standard deviation from the cross-cohort RSS estimator used in the Variance--Covariance Analysis (``Debiased SD'', with bootstrap SE in brackets; this is the same quantity reported on the diagonal of Table~\\ref{tab:rss-varcov-correlation}). Schools are weighted by the number of students in the corresponding value-added regression. For the six outcomes with a Stata-based EB regression, sample moments refer to the outcome-specific regression sample and the EB values come from that Stata pipeline. Financial aid application has no Stata-based EB regression; its EB SD and percentiles instead come from the R-based EB pipeline that feeds the paper's main IV results for this outcome, and its sample mean/SD are computed over the four-cohort (2017--2020) RSS estimation sample rather than a covariate-complete regression esample, since no such esample exists for this outcome outside a full pipeline rerun. Math and verbal are measured in admission-test standard deviations; high-premium institution, high-premium field, admission-exam taking, and financial-aid application are binary; projected program income is measured in logs.}",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
)
writeLines(latex, output_tex)
log_ts("Wrote: ", output_tex)
log_ts("Done.")
