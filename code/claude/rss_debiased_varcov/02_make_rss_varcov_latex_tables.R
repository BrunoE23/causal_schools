###############################################################################
# Turn the RSS debiased Var/Cov output (rss_varcov_matrix.csv,
# rss_corr_matrix.csv, rss_implied_gains.csv, all produced by
# 01_construct_rss_debiased_varcov.R for RSS_VARCOV_SPEC=four_year_no_sae)
# into two LaTeX tables for the paper:
#
#   Table A (rss_varcov_correlation_table.tex): the 7x7 debiased school-VA
#     correlation matrix, lower triangle only. Diagonal cells show the
#     debiased SD (with bootstrap SE below, in brackets) instead of "1.000".
#
#   Table B (rss_implied_gains_table.tex): the FULL 7x7 (asymmetric)
#     "implied gains" matrix -- cell (row, col) = the implied increase in
#     the COLUMN outcome's true school VA from a 1-unit increase in the ROW
#     outcome's true school VA, i.e. beta = Cov(row, col) / Var(row), using
#     the SAME pair-specific common-school-set variance the correlation
#     matrix itself uses (so the numbers are internally consistent with
#     Table A). This generalizes rss_orthogonality_summary.csv's
#     beta_on_anchor (which only ever anchored on math) to every ordered
#     pair among the 7 outcomes.
#
# Run AFTER (re)running 01_construct_rss_debiased_varcov.R with
# RSS_VARCOV_SPEC=four_year_no_sae -- that step is what actually writes the
# three input CSVs this script reads. Because all 7 outcomes' regressions
# are already cached in component_cache/, rerunning 01 after this file's
# accompanying edit (which only added output-writing lines, not new fitting)
# is fast: it just reloads the 7 cached .rds fits and redoes the cheap
# pairwise arithmetic.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
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
      "\nSet ", env_var, ", or run 01_construct_rss_debiased_varcov.R first."
    )
  }
  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
spec <- Sys.getenv("RSS_VARCOV_SPEC", unset = "four_year_no_sae")

input_dir <- file.path(data_wd, "data", "clean", "rss_debiased_varcov", spec)
varcov_path <- file.path(input_dir, "rss_varcov_matrix.csv")
corr_path <- file.path(input_dir, "rss_corr_matrix.csv")
gains_path <- file.path(input_dir, "rss_implied_gains.csv")

varcov_dt <- fread(find_existing_file("RSS_VARCOV_MATRIX_PATH", varcov_path, "rss_varcov_matrix.csv"))
corr_dt <- fread(find_existing_file("RSS_CORR_MATRIX_PATH", corr_path, "rss_corr_matrix.csv"))
gains_dt <- fread(find_existing_file(
  "RSS_IMPLIED_GAINS_PATH", gains_path,
  paste0(
    "rss_implied_gains.csv (this file is written by the UPDATED ",
    "01_construct_rss_debiased_varcov.R -- rerun that script first if you ",
    "haven't since the implied-gains export was added)"
  )
))

out_dir <- file.path(repo_wd, "output", "tables", "rss_debiased_varcov")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------- OUTCOME ORDER + LABELS ---------------------------
outcome_order <- c(
  "z_year_math_max", "z_year_leng_max",
  "admission_exam_taker", "any_postulacion", "higher_ed_enrolled_m1",
  "high_inst_m1", "high_paying_field_m1"
)
outcome_labels <- c(
  z_year_math_max = "Math achievement",
  z_year_leng_max = "Verbal achievement",
  higher_ed_enrolled_m1 = "Higher-ed enrollment",
  high_inst_m1 = "High-premium institution",
  high_paying_field_m1 = "High-premium field",
  log_program_income_clp_m1 = "Log projected income",
  admission_exam_taker = "Admission exam taken",
  any_postulacion = "Financial aid application"
)
# Short forms for column headers -- the row labels above stay full-length,
# but the header row needs to fit eight columns inside \resizebox, so use
# compact names instead of the (1)...(7) + footnote-legend scheme.
outcome_short_labels <- c(
  z_year_math_max = "Math",
  z_year_leng_max = "Verbal",
  higher_ed_enrolled_m1 = "Higher ed.",
  high_inst_m1 = "High-inst.",
  high_paying_field_m1 = "HP field",
  log_program_income_clp_m1 = "Log inc.",
  admission_exam_taker = "Exam",
  any_postulacion = "Fin. aid app"
)
outcome_order <- intersect(outcome_order, unique(c(varcov_dt$outcome_1, varcov_dt$outcome_2)))
n <- length(outcome_order)
col_nums <- outcome_short_labels[outcome_order]

fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))

significance_stars <- function(p_value) {
  fifelse(
    is.na(p_value), "",
    fifelse(p_value < 0.01, "***", fifelse(p_value < 0.05, "**", fifelse(p_value < 0.10, "*", "")))
  )
}
two_sided_p <- function(estimate, se) {
  if (is.finite(estimate) && is.finite(se) && se > 0) 2 * stats::pnorm(-abs(estimate / se)) else NA_real_
}

# ------------------------- TABLE A: SD diagonal + correlation lower tri -----
# rss_corr_matrix.csv now carries a bootstrap_se column too (joint/paired
# school resample -- see bootstrap_gains_se() in 01_construct_rss_debiased_
# varcov.R), so off-diagonal correlation cells get stars (testing
# H0: correlation = 0) and their SE in brackets below, the same way the
# diagonal SD already did. NA on the diagonal (correlation is trivially 1).
diag_dt <- varcov_dt[type == "variance"]
sd_lookup <- setNames(sqrt(diag_dt$estimate), diag_dt$outcome_1)
sd_se_lookup <- setNames(diag_dt$bootstrap_se / (2 * sqrt(diag_dt$estimate)), diag_dt$outcome_1)

corr_lookup <- new.env()
corr_se_lookup <- new.env()
for (i in seq_len(nrow(corr_dt))) {
  a <- corr_dt$outcome_1[i]; b <- corr_dt$outcome_2[i]
  assign(paste(a, b, sep = "__"), corr_dt$correlation[i], envir = corr_lookup)
  assign(paste(b, a, sep = "__"), corr_dt$correlation[i], envir = corr_lookup)
  assign(paste(a, b, sep = "__"), corr_dt$bootstrap_se[i], envir = corr_se_lookup)
  assign(paste(b, a, sep = "__"), corr_dt$bootstrap_se[i], envir = corr_se_lookup)
}
get_corr <- function(a, b) {
  key <- paste(a, b, sep = "__")
  if (exists(key, envir = corr_lookup)) get(key, envir = corr_lookup) else NA_real_
}
get_corr_se <- function(a, b) {
  key <- paste(a, b, sep = "__")
  if (exists(key, envir = corr_se_lookup)) get(key, envir = corr_se_lookup) else NA_real_
}

tableA_lines <- c()
for (i in seq_len(n)) {
  row_label <- outcome_labels[[outcome_order[i]]]
  cells <- character(n)
  se_cells <- rep("", n)
  for (j in seq_len(n)) {
    if (j > i) { cells[j] <- ""; next }
    if (j == i) {
      cells[j] <- fmt3(sd_lookup[[outcome_order[i]]])
      se_cells[j] <- paste0("[", sprintf("%.3f", sd_se_lookup[[outcome_order[i]]]), "]")
    } else {
      rho <- get_corr(outcome_order[i], outcome_order[j])
      rho_se <- get_corr_se(outcome_order[i], outcome_order[j])
      cells[j] <- fmt3(rho)
      se_cells[j] <- if (is.finite(rho_se)) paste0("[", sprintf("%.3f", rho_se), "]") else ""
    }
  }
  tableA_lines <- c(
    tableA_lines,
    paste0(row_label, " & ", paste(cells, collapse = " & "), " \\\\"),
    paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
  )
}

tableA <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Debiased correlations in latent observational school value added}",
  "\\label{tab:rss-varcov-correlation}",
  "\\resizebox{\\textwidth}{!}{%",
  paste0("\\begin{tabular}{l", paste(rep("c", n), collapse = ""), "}"),
  "\\toprule",
  paste0(" & ", paste(col_nums, collapse = " & "), " \\\\"),
  "\\midrule",
  tableA_lines,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "\\textit{Notes:} Diagonal entries report debiased standard deviations of latent school value added; ",
    "off-diagonal entries report cross-outcome correlations estimated using the procedure in Section 3.5. ",
    "Bootstrap standard errors are in brackets. Math, verbal, high-premium-institution, and ",
    "high-premium-field value added are estimated among admission-exam takers; all other measures use the broader eligible sample."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(tableA, file.path(out_dir, "rss_varcov_correlation_table.tex"))
log_ts("Wrote: ", file.path(out_dir, "rss_varcov_correlation_table.tex"))

# ------------------------- TABLE B: implied gains (full matrix) ------------
# rss_implied_gains.csv now carries a bootstrap_se column (joint/paired
# school resample -- see 01_construct_rss_debiased_varcov.R's
# bootstrap_gains_se()), so each cell gets a beta with stars (testing
# H0: beta = 0, the natural null for a cross-outcome implied gain) and its
# SE on the line below, matching the SE-in-parens convention used elsewhere
# in the repo's generated tables (e.g. program_income_full_orthogonal_math_
# highinst_highpay_joint_iv.tex).
gains_beta_lookup <- new.env()
gains_se_lookup <- new.env()
for (i in seq_len(nrow(gains_dt))) {
  key <- paste(gains_dt$outcome_from[i], gains_dt$outcome_to[i], sep = "__")
  assign(key, gains_dt$beta[i], envir = gains_beta_lookup)
  assign(key, gains_dt$bootstrap_se[i], envir = gains_se_lookup)
}
get_gain <- function(a, b) {
  key <- paste(a, b, sep = "__")
  if (exists(key, envir = gains_beta_lookup)) get(key, envir = gains_beta_lookup) else NA_real_
}
get_gain_se <- function(a, b) {
  key <- paste(a, b, sep = "__")
  if (exists(key, envir = gains_se_lookup)) get(key, envir = gains_se_lookup) else NA_real_
}

tableB_lines <- c()
for (i in seq_len(n)) {
  row_label <- outcome_labels[[outcome_order[i]]]
  beta_cells <- character(n)
  se_cells <- character(n)
  for (j in seq_len(n)) {
    if (i == j) {
      beta_cells[j] <- "--"
      se_cells[j] <- ""
      next
    }
    beta <- get_gain(outcome_order[i], outcome_order[j])
    se <- get_gain_se(outcome_order[i], outcome_order[j])
    beta_cells[j] <- paste0(fmt3(beta), significance_stars(two_sided_p(beta, se)))
    se_cells[j] <- if (is.finite(se)) paste0("(", sprintf("%.3f", se), ")") else ""
  }
  tableB_lines <- c(
    tableB_lines,
    paste0(row_label, " & ", paste(beta_cells, collapse = " & "), " \\\\"),
    paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
  )
}

tableB <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Implied gains across latent observational value-added measures}",
  "\\label{tab:rss-implied-gains}",
  "\\resizebox{\\textwidth}{!}{%",
  paste0("\\begin{tabular}{l", paste(rep("c", n), collapse = ""), "}"),
  "\\toprule",
  paste0(" & ", paste(col_nums, collapse = " & "), " \\\\"),
  "\\midrule",
  tableB_lines,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "Notes: Row entries give the implied gain in each column outcome's ",
    "debiased latent observational school value added from a 1-unit increase in the row ",
    "outcome's debiased latent observational school value added, i.e. ",
    "$\\mathrm{Cov}(\\text{row},\\text{col})/\\mathrm{Var}(\\text{row})$, computed ",
    "with the RSS cross-cohort estimator on the common set of schools used ",
    "for that pair (matching the correlation matrix's own denominators). ",
    "The matrix is not symmetric: the implied gain in column $j$ from a unit ",
    "increase in row $i$ need not equal the implied gain in row $i$ from a ",
    "unit increase in column $j$, since the two outcomes generally have ",
    "different variances. Standard errors (in parentheses) are from a joint ",
    "school-level bootstrap that resamples schools once per replicate and ",
    "recomputes the row and column variances and their covariance together, ",
    "so the ratio's sampling variability is captured correctly; this is a ",
    "nonparametric approximation and does not replicate the analytic ",
    "delta-method standard errors derived in Rose, Schellenberg and ",
    "Shem-Tov's Appendix C. Stars test the null that the implied gain is ",
    "zero. With 21 outcome pairs (42 directed betas) tested here, some ",
    "results significant at conventional levels are expected by chance; no ",
    "multiple-testing adjustment is applied. Column headers abbreviate the ",
    "row labels. Math and ",
    "verbal VA are in standardized-test-score units; high-premium-",
    "institution, high-premium-field, admission-exam-taken, financial-",
    "aid-application, and higher-education-enrollment VA are in probability ",
    "(share-of-students) units. Math, verbal, high-premium-institution, and ",
    "high-premium-field estimates are restricted to admission-exam takers; ",
    "the remaining outcomes use the broader eligible sample. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(tableB, file.path(out_dir, "rss_implied_gains_table.tex"))
log_ts("Wrote: ", file.path(out_dir, "rss_implied_gains_table.tex"))

log_ts("Done.")
