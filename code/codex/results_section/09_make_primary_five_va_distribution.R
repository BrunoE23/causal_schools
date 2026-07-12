###############################################################################
# Paper-facing distribution table for the five primary EB value-added outcomes.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

repo_wd <- "C:/Users/brunem/Research/causal_schools"
results_dir <- file.path(repo_wd, "output", "tables", "results_section")
eb_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
cache_path <- file.path(eb_dir, "stata_va_analytic_cache.dta")
output_csv <- file.path(results_dir, "primary_five_va_distribution.csv")
output_tex <- file.path(results_dir, "primary_five_va_distribution.tex")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

specs <- data.table(
  outcome_key = c("math", "language", "highinst", "highpay", "program_income_full"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_inst_m1", "highpay_field_m1", "log_proginc_full_clp_m1"),
  eb_outcome = c("z_year_math_max", "z_year_leng_max", "high_inst_m1", "high_paying_field_m1", "log_program_income_full_clp_m1"),
  label = c("Math", "Verbal", "High-premium institution", "High-premium field", "Log proj. income")
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

rows <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
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
    label = spec$label,
    sample_mean = mean(sample[[spec$outcome]]),
    sample_sd = sd(sample[[spec$outcome]]),
    va_sd = weighted_sd(eb$va_eb_centered, eb$n_students),
    p10 = q[[1]], p25 = q[[2]], p50 = q[[3]], p75 = q[[4]], p90 = q[[5]],
    n_students = nrow(sample), n_schools = nrow(eb)
  )
}))

fwrite(rows, output_csv)

latex_rows <- vapply(seq_len(nrow(rows)), function(i) {
  values <- unlist(rows[i, .(sample_mean, sample_sd, va_sd, p10, p25, p50, p75, p90)])
  paste0(rows$label[[i]], " & ", paste(format_value(values), collapse = " & "), " \\\\")
}, character(1))

latex <- c(
  "\\begin{table}[!htbp]", "\\centering", "\\begin{threeparttable}",
  "\\caption{Distribution of primary EB-shrunken school value added}",
  "\\label{tab:school-va-distribution}", "\\footnotesize", "\\setlength{\\tabcolsep}{2pt}",
  "\\begin{tabular}{lcc|cccccc}", "\\toprule",
  " & \\multicolumn{2}{c|}{Outcome distribution} & \\multicolumn{6}{c}{Value-added distribution} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-9}",
  "Outcome & \\shortstack{Sample\\\\Mean} & \\shortstack{Sample\\\\SD} & VA SD & P10 & P25 & P50 & P75 & P90 \\\\",
  "\\midrule", latex_rows, "\\bottomrule", "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]", "\\footnotesize",
  "\\item[] \\parbox{0.84\\textwidth}{Notes: The table reports student-weighted distributions of EB-shrunken, student-centered school value added. Schools are weighted by the number of students in the corresponding Stata value-added regression. Sample moments refer to the outcome-specific value-added regression sample. Math and verbal are measured in admission-test standard deviations; the two high-return outcomes are binary; projected program income is measured in logs.}",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
)
writeLines(latex, output_tex)

message("Wrote: ", output_csv)
message("Wrote: ", output_tex)
