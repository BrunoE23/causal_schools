###############################################################################
# Program-income EB scalar IV by grade-4 SIMCE math quintile
#
# This script uses the program-income EB expected-VA regression dataframe
# produced by 02_run_expected_va_scalar_iv_eb.R and repeats the IV separately by
# rank quintiles of baseline grade-4 math achievement.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

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
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

extract_coef <- function(model, term) {
  coefs <- coef(model)
  if (term %in% names(coefs)) {
    return(term)
  }
  fit_term <- paste0("fit_", term)
  if (fit_term %in% names(coefs)) {
    return(fit_term)
  }
  hit <- grep(term, names(coefs), fixed = TRUE, value = TRUE)
  if (length(hit) == 1) {
    return(hit)
  }
  stop("Could not find coefficient for term: ", term, call. = FALSE)
}

make_rank_quintile <- function(x) {
  out <- rep(NA_integer_, length(x))
  nonmissing <- !is.na(x)
  n <- sum(nonmissing)

  if (n == 0) {
    return(out)
  }

  ranks <- frank(x[nonmissing], ties.method = "average")
  out[nonmissing] <- pmin(5L, as.integer(ceiling(5 * ranks / n)))
  out
}

empty_result <- function(group_label, n_obs = 0L) {
  data.table(
    spec = "program_income_adj_eb",
    outcome_group = "Program income",
    group = group_label,
    beta = NA_real_,
    se = NA_real_,
    zstat = NA_real_,
    p_value = NA_real_,
    n_obs = n_obs,
    fs_beta = NA_real_,
    fs_se = NA_real_,
    fs_f = NA_real_,
    rc = 1L
  )
}

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

format_se <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
}

format_n <- function(x) {
  ifelse(is.na(x), "", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
eb_dir <- file.path(clean_dir, "empirical_bayes_school_va")
table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

regression_csv <- find_existing_file(
  "EB_IV_PROGRAM_INCOME_REGRESSION_DF",
  c(
    file.path(
      table_dir,
      "scalar_school_value_iv_program_income_expected_va_eb_regression_df.csv"
    ),
    file.path(
      eb_dir,
      "scalar_school_value_iv_expected_va_eb.csv"
    )
  ),
  "program-income EB IV regression dataframe"
)

results_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_program_income_expected_va_eb_by_simce4_math_quintile.csv"
)
main_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_program_income_expected_va_eb_by_simce4_math_quintile_main.csv"
)
main_tex <- file.path(
  table_dir,
  "scalar_school_value_iv_program_income_expected_va_eb_by_simce4_math_quintile.tex"
)

y_col <- "log_program_income_clp_m1"
d_col <- "d_program_income_adj_eb"
z_col <- "z_program_income_adj_eb"
expected_col <- "expected_program_income_adj_eb"

needed_cols <- c(
  "student_id",
  "mrun",
  "cohort_gr8",
  "sae_proceso",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  y_col,
  d_col,
  z_col,
  expected_col
)

message("Reading program-income EB IV regression dataframe: ", regression_csv)
dt <- fread(regression_csv, select = needed_cols, na.strings = c("", "NA"))

dt[, simce4_math_quintile := make_rank_quintile(z_sim_mat_4to)]
analysis_dt <- dt[!is.na(simce4_math_quintile)]

run_iv <- function(dt, group_label) {
  needed <- c(
    y_col,
    d_col,
    z_col,
    expected_col,
    "cohort_gr8",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "GEN_ALU",
    "EDAD_ALU"
  )
  reg_dt <- dt[complete.cases(dt[, ..needed])]

  if (nrow(reg_dt) == 0 ||
      uniqueN(reg_dt[[z_col]]) < 2 ||
      uniqueN(reg_dt[[d_col]]) < 2) {
    return(empty_result(group_label, nrow(reg_dt)))
  }

  controls <- c(
    "factor(cohort_gr8)",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "factor(GEN_ALU)",
    "factor(EDAD_ALU)",
    expected_col
  )
  control_rhs <- paste(controls, collapse = " + ")
  iv_formula <- as.formula(paste0(y_col, " ~ ", control_rhs, " | 0 | ", d_col, " ~ ", z_col))
  fs_formula <- as.formula(paste0(d_col, " ~ ", z_col, " + ", control_rhs))

  iv_model <- feols(iv_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  coef_term <- extract_coef(iv_model, d_col)
  beta <- coef(iv_model)[[coef_term]]
  se <- se(iv_model)[[coef_term]]
  zstat <- beta / se
  p_value <- 2 * stats::pnorm(-abs(zstat))

  fs_model <- feols(fs_formula, data = reg_dt, vcov = "hetero", notes = FALSE)
  fs_beta <- coef(fs_model)[[z_col]]
  fs_se <- se(fs_model)[[z_col]]
  fs_f <- (fs_beta / fs_se)^2

  data.table(
    spec = "program_income_adj_eb",
    outcome_group = "Program income",
    group = group_label,
    beta = beta,
    se = se,
    zstat = zstat,
    p_value = p_value,
    n_obs = nobs(iv_model),
    fs_beta = fs_beta,
    fs_se = fs_se,
    fs_f = fs_f,
    rc = 0L
  )
}

message("Running program-income EB IV by grade-4 math quintile.")
setFixest_nthreads(0)
results <- rbindlist(c(
  list(run_iv(analysis_dt, "All")),
  lapply(1:5, function(q) {
    run_iv(analysis_dt[simce4_math_quintile == q], paste0("Q", q))
  })
), use.names = TRUE)

results[, quintile_definition := fifelse(
  group == "All",
  "All students with nonmissing grade-4 math",
  "Rank quintile of z_sim_mat_4to within program-income EB IV regression dataframe"
)]

fwrite(results, results_csv)

main <- results[, .(
  group,
  theta_program_income = beta,
  se_program_income = se,
  p_value,
  n_obs,
  fs_beta,
  fs_se,
  fs_f,
  quintile_definition
)]
fwrite(main, main_csv)

latex_rows <- paste0(
  main$group,
  " & ",
  format_estimate(main$theta_program_income),
  " & ",
  format_se(main$se_program_income),
  " & ",
  format_n(main$n_obs),
  " & ",
  format_estimate(main$fs_f),
  " \\\\"
)

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Program-income EB scalar IV estimates by grade-4 SIMCE math quintile}",
  "\\label{tab:program_income_eb_iv_by_simce4_math_quintile}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Grade-4 math quintile & $\\theta^{EB}$ & SE & Observations & First-stage F \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(latex_table, main_tex)

message("Wrote: ", results_csv)
message("Wrote: ", main_csv)
message("Wrote: ", main_tex)

print(main)
