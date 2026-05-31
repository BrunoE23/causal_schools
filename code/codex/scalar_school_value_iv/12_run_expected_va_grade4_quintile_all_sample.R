###############################################################################
# Grade-4 SIMCE math-quintile heterogeneity for expected-VA scalar IV
#
# This script uses the full expected-VA regression dataframe produced by
# 08_run_expected_va_scalar_iv.R and bins students by their grade-4 math SIMCE
# score. Unlike 10_run_expected_va_grade4_quintile_heterogeneity.R, it does not
# restrict to the cohort with grade-8 SIMCE heterogeneity data.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
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
scalar_dir <- file.path(clean_dir, "scalar_school_value_iv")
table_dir <- file.path(repo_wd, "output", "tables", "scalar_school_value_iv")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

expected_va_csv <- file.path(
  scalar_dir,
  "scalar_school_value_iv_expected_va.csv"
)
results_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_expected_va_by_simce4_math_quintile_all_sample.csv"
)
main_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_expected_va_by_simce4_math_quintile_all_sample_main.csv"
)
main_tex <- file.path(
  table_dir,
  "scalar_school_value_iv_expected_va_by_simce4_math_quintile_all_sample_main.tex"
)

value_specs <- data.table(
  spec = c("math_adj", "leng_adj", "stem_adj"),
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "stem_enrollment_m1"
  ),
  outcome_group = c("Math", "Language", "STEM"),
  adjustment = "Adjusted"
)

needed_cols <- unique(c(
  "student_id", "mrun", "cohort_gr8", "sae_proceso", "GEN_ALU", "EDAD_ALU",
  "z_sim_mat_4to", "z_sim_leng_4to",
  value_specs$outcome,
  paste0("expected_", value_specs$spec),
  paste0("d_", value_specs$spec),
  paste0("z_", value_specs$spec)
))

message("Reading expected-VA regression dataframe: ", expected_va_csv)
dt <- fread(expected_va_csv, select = needed_cols, na.strings = c("", "NA"))

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

dt[, simce4_math_quintile := make_rank_quintile(z_sim_mat_4to)]
analysis_dt <- dt[!is.na(simce4_math_quintile)]

residualize <- function(x, controls) {
  as.numeric(qr.resid(qr(controls), x))
}

build_controls <- function(dt) {
  terms <- c("z_sim_mat_4to", "z_sim_leng_4to")

  if (uniqueN(dt$cohort_gr8) > 1) {
    terms <- c(terms, "factor(cohort_gr8)")
  }
  if (uniqueN(dt$GEN_ALU) > 1) {
    terms <- c(terms, "factor(GEN_ALU)")
  }
  if (uniqueN(dt$EDAD_ALU) > 1) {
    terms <- c(terms, "factor(EDAD_ALU)")
  }

  stats::model.matrix(
    stats::as.formula(paste("~", paste(terms, collapse = " + "))),
    data = dt
  )
}

empty_result <- function(spec_row, group_label, n_obs = 0L) {
  data.table(
    spec = spec_row$spec,
    outcome_group = spec_row$outcome_group,
    adjustment = spec_row$adjustment,
    group = group_label,
    beta = NA_real_,
    se = NA_real_,
    zstat = NA_real_,
    p_value = NA_real_,
    n_obs = n_obs,
    fs_beta = NA_real_,
    fs_se = NA_real_,
    fs_f = NA_real_
  )
}

run_scalar_iv <- function(dt, spec_row, group_label) {
  spec <- spec_row$spec
  y_col <- spec_row$outcome
  d_col <- paste0("d_", spec)
  z_col <- paste0("z_", spec)
  expected_col <- paste0("expected_", spec)

  needed <- c(
    y_col, d_col, z_col, expected_col,
    "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU"
  )
  reg_dt <- dt[complete.cases(dt[, ..needed])]

  if (nrow(reg_dt) == 0 ||
      uniqueN(reg_dt[[z_col]]) < 2 ||
      uniqueN(reg_dt[[d_col]]) < 2) {
    return(empty_result(spec_row, group_label, nrow(reg_dt)))
  }

  controls <- cbind(build_controls(reg_dt), expected = reg_dt[[expected_col]])
  y_resid <- residualize(reg_dt[[y_col]], controls)
  d_resid <- residualize(reg_dt[[d_col]], controls)
  z_resid <- residualize(reg_dt[[z_col]], controls)

  denom <- sum(z_resid * d_resid)
  if (!is.finite(denom) || abs(denom) < .Machine$double.eps) {
    return(empty_result(spec_row, group_label, nrow(reg_dt)))
  }

  beta <- sum(z_resid * y_resid) / denom
  u <- y_resid - beta * d_resid

  n <- length(u)
  k <- ncol(controls) + 1L
  small_sample_adj <- n / (n - k)
  se <- sqrt(small_sample_adj * sum((z_resid^2) * (u^2)) / denom^2)
  zstat <- beta / se
  p_value <- 2 * stats::pnorm(-abs(zstat))

  fs_beta <- sum(z_resid * d_resid) / sum(z_resid^2)
  fs_u <- d_resid - fs_beta * z_resid
  fs_se <- sqrt(
    small_sample_adj *
      sum((z_resid^2) * (fs_u^2)) /
      sum(z_resid^2)^2
  )
  fs_f <- (fs_beta / fs_se)^2

  data.table(
    spec = spec,
    outcome_group = spec_row$outcome_group,
    adjustment = spec_row$adjustment,
    group = group_label,
    beta = beta,
    se = se,
    zstat = zstat,
    p_value = p_value,
    n_obs = n,
    fs_beta = fs_beta,
    fs_se = fs_se,
    fs_f = fs_f
  )
}

message("Running full-sample SIMCE 4 math-quintile expected-VA IV specs.")
results <- rbindlist(lapply(seq_len(nrow(value_specs)), function(i) {
  rbindlist(c(
    list(run_scalar_iv(analysis_dt, value_specs[i], "All")),
    lapply(1:5, function(q) {
      run_scalar_iv(
        analysis_dt[simce4_math_quintile == q],
        value_specs[i],
        paste0("Q", q)
      )
    })
  ), use.names = TRUE)
}), use.names = TRUE)

fwrite(results, results_csv)

main <- results[group != "All"]
main[, outcome_group := factor(outcome_group, levels = c("Math", "Language", "STEM"))]
setorder(main, group, outcome_group)
main[, outcome_group := as.character(outcome_group)]

main_wide <- dcast(
  main,
  group ~ outcome_group,
  value.var = c("beta", "se", "n_obs")
)
main_wide[, group_number := as.integer(sub("^Q", "", group))]
setorder(main_wide, group_number)

presentation <- main_wide[, .(
  simce4_math_quintile = group,
  theta_math = beta_Math,
  se_math = se_Math,
  theta_language = beta_Language,
  se_language = se_Language,
  theta_stem = beta_STEM,
  se_stem = se_STEM,
  n_math = n_obs_Math,
  n_language = n_obs_Language,
  n_stem = n_obs_STEM
)]
fwrite(presentation, main_csv)

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

latex_rows <- paste0(
  presentation$simce4_math_quintile, " & ",
  format_estimate(presentation$theta_math), " & ",
  format_estimate(presentation$se_math), " & ",
  format_estimate(presentation$theta_language), " & ",
  format_estimate(presentation$se_language), " & ",
  format_estimate(presentation$theta_stem), " & ",
  format_estimate(presentation$se_stem), " \\\\"
)

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Expected-VA scalar IV estimates by grade-4 SIMCE math quintile, full sample}",
  "\\label{tab:scalar_school_value_iv_expected_va_by_simce4_math_quintile_all_sample}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Math VA} & \\multicolumn{2}{c}{Language VA} & \\multicolumn{2}{c}{STEM VA} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "Grade-4 math quintile & $\\theta$ & SE & $\\theta$ & SE & $\\theta$ & SE \\\\",
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

print(presentation)
