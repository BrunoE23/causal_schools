###############################################################################
# Grade-4 SIMCE math-tercile heterogeneity for expected-VA scalar IV
#
# CURRENT DEFAULT PATH:
# This script mirrors the grade-4 quintile exercise, but bins the same
# SIMCE-8-linked 2019 sample into terciles using the grade-4 math score.
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
simce8_dir <- file.path(clean_dir, "simce8_heterogeneity")
table_dir <- file.path(repo_wd, "output", "tables", "scalar_school_value_iv")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

expected_va_csv <- file.path(
  scalar_dir,
  "scalar_school_value_iv_expected_va.csv"
)
simce8_csv <- file.path(
  simce8_dir,
  "cohort_2019_math_heterogeneity.csv"
)
results_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_expected_va_by_simce4_math_tercile_same_sample.csv"
)
main_csv <- file.path(
  table_dir,
  "scalar_school_value_iv_expected_va_by_simce4_math_tercile_same_sample_main.csv"
)
main_tex <- file.path(
  table_dir,
  "scalar_school_value_iv_expected_va_by_simce4_math_tercile_same_sample_main.tex"
)

value_specs <- data.table(
  spec = c(
    "math_unadj", "math_adj",
    "leng_unadj", "leng_adj",
    "stem_unadj", "stem_adj"
  ),
  outcome = c(
    "z_year_math_max", "z_year_math_max",
    "z_year_leng_max", "z_year_leng_max",
    "stem_enrollment_m1", "stem_enrollment_m1"
  ),
  outcome_group = c("Math", "Math", "Language", "Language", "STEM", "STEM"),
  adjustment = c(
    "Unadjusted", "Adjusted",
    "Unadjusted", "Adjusted",
    "Unadjusted", "Adjusted"
  )
)

main_specs <- c("math_adj", "leng_adj", "stem_adj")

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

message("Reading SIMCE 4/8 math heterogeneity file: ", simce8_csv)
simce <- fread(
  simce8_csv,
  select = c(
    "mrun",
    "ptje_mate4b_alu",
    "simce8_math_quintile"
  ),
  na.strings = c("", "NA")
)
simce[, mrun := as.numeric(mrun)]
dt[, mrun := as.numeric(mrun)]

dt <- merge(dt, simce, by = "mrun", all.x = TRUE, sort = FALSE)

# Keep exactly the grade-8-linked sample used for the previous quintile tables.
analysis_dt <- dt[
  !is.na(simce8_math_quintile) &
    !is.na(ptje_mate4b_alu)
]

tercile_breaks <- stats::quantile(
  analysis_dt$ptje_mate4b_alu,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE,
  type = 7
)

analysis_dt[, simce4_math_tercile := fifelse(
  ptje_mate4b_alu <= tercile_breaks[[1]],
  1L,
  fifelse(ptje_mate4b_alu <= tercile_breaks[[2]], 2L, 3L)
)]

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
    return(data.table(
      spec = spec,
      outcome_group = spec_row$outcome_group,
      adjustment = spec_row$adjustment,
      group = group_label,
      beta = NA_real_,
      se = NA_real_,
      zstat = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(reg_dt),
      fs_beta = NA_real_,
      fs_se = NA_real_,
      fs_f = NA_real_
    ))
  }

  controls <- cbind(build_controls(reg_dt), expected = reg_dt[[expected_col]])
  y_resid <- residualize(reg_dt[[y_col]], controls)
  d_resid <- residualize(reg_dt[[d_col]], controls)
  z_resid <- residualize(reg_dt[[z_col]], controls)

  denom <- sum(z_resid * d_resid)
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

message("Running same-sample SIMCE 4 math-tercile expected-VA IV specs.")
results <- rbindlist(lapply(seq_len(nrow(value_specs)), function(i) {
  rbindlist(c(
    list(run_scalar_iv(analysis_dt, value_specs[i], "All same sample")),
    lapply(1:3, function(q) {
      run_scalar_iv(
        analysis_dt[simce4_math_tercile == q],
        value_specs[i],
        paste0("T", q)
      )
    })
  ), use.names = TRUE)
}), use.names = TRUE)

fwrite(results, results_csv)

main <- results[spec %chin% main_specs]
main[, outcome_group := factor(outcome_group, levels = c("Math", "Language", "STEM"))]
setorder(main, group, outcome_group)
main[, outcome_group := as.character(outcome_group)]
fwrite(
  main[, .(
    group, outcome_group, adjustment, beta, se, p_value, n_obs,
    fs_beta, fs_se, fs_f
  )],
  main_csv
)

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

format_n <- function(x) {
  ifelse(is.na(x), "", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
}

main_wide <- dcast(
  main,
  group ~ outcome_group,
  value.var = c("beta", "se", "n_obs")
)

tercile_rows <- main_wide[group != "All same sample"]
tercile_rows[, group_number := as.integer(sub("^T", "", group))]
setorder(tercile_rows, group_number)

latex_rows <- unlist(lapply(seq_len(nrow(tercile_rows)), function(i) {
  row <- tercile_rows[i]
  c(
    paste(
      row$group,
      format_estimate(row$beta_Math),
      format_estimate(row$beta_Language),
      format_estimate(row$beta_STEM),
      sep = " & "
    ),
    paste(
      "",
      paste0("(", format_estimate(row$se_Math), ")"),
      paste0("(", format_estimate(row$se_Language), ")"),
      paste0("(", format_estimate(row$se_STEM), ")"),
      sep = " & "
    ),
    paste(
      "N",
      format_n(row$n_obs_Math),
      format_n(row$n_obs_Language),
      format_n(row$n_obs_STEM),
      sep = " & "
    )
  )
}), use.names = FALSE)
latex_rows <- paste0(latex_rows, " \\\\")

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Expected-VA scalar IV estimates by grade-4 SIMCE math tercile}",
  "\\label{tab:scalar_school_value_iv_expected_va_by_simce4_math_tercile}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Grade-4 math tercile & Math VA & Language VA & STEM VA \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(latex_table, main_tex)

message("Grade-4 math score tercile cutoffs: ", paste(round(tercile_breaks, 3), collapse = ", "))
message("Wrote: ", results_csv)
message("Wrote: ", main_csv)
message("Wrote: ", main_tex)

print(main[group != "All same sample", .(
  group, outcome_group, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f
)])
