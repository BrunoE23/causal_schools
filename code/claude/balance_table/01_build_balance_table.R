###############################################################################
# Balance table: 5 groups across the broad panel and SAE samples
#
# Groups
# ------
#   1. Broad Panel            : cohorts 2017-2020, attended HS, age 12-16
#   2. Non-private HS          : broad panel, school dependency != 4 (pagado)
#   3. SAE applicants          : timely grade-9 SAE applicants, cohorts 2018-2020
#   4. SAE applicants w/ risk  : timely applicants with nondegenerate assignment risk
#   5. Private HS              : broad panel, school dependency == 4 (pagado)
#
# Covariates
# ----------
#   Student DB  : Female, Metropolitan Region (Santiago), Age
#   SIMCE       : 4th-grade math, 4th-grade verbal         (+ match rate)
#   Middle sch. : GPA z-score, Attendance z-score          (+ match rate)
#   Outcomes    : Std. math exam, Std. verbal exam,
#                 Higher ed. enrollment, Hi-prem. institution, Hi-prem. field
#
# Output
# ------
#   output/tables/balance_table/balance_table.tex
#   output/tables/balance_table/balance_table.csv
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) stop("Could not find ", label)
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
clean_dir  <- file.path(data_wd, "data", "clean")
table_dir  <- file.path(repo_wd, "output", "tables", "balance_table")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. Broad panel ─────────────────────────────────────────────────────────────
message("Reading univ_gr8_df.csv ...")
dt <- fread(file.path(clean_dir, "univ_gr8_df.csv"), na.strings = c("", "NA"))
# drop duplicate columns (e.g. both MRUN and mrun) before lowercasing
new_names <- tolower(names(dt))
dupes <- duplicated(new_names)
if (any(dupes)) dt <- dt[, !dupes, with = FALSE]
setnames(dt, tolower(names(dt)))

for (v in c("mrun", "cohort_gr8", "most_time_rbd", "edad_alu", "cod_reg_alu", "gen_alu")) {
  if (v %in% names(dt)) dt[[v]] <- suppressWarnings(as.numeric(dt[[v]]))
}

dt <- dt[cohort_gr8 %in% 2017:2020]
dt <- dt[!is.na(most_time_rbd) & most_time_rbd > 0]
dt <- dt[!is.na(edad_alu) & edad_alu >= 12 & edad_alu <= 16]
# dedup by mrun (keep first occurrence per student, matching VA sample logic)
setkey(dt, mrun)
dt <- dt[!duplicated(mrun)]
message("  Broad panel: ", nrow(dt), " students")

dt[, female := as.integer(gen_alu == 2)]
dt[, in_rm   := as.integer(cod_reg_alu == 13)]

# Standardise exam scores within year (if raw scores present but z-scores not)
if (!("z_year_math_max" %in% names(dt)) && "math_max" %in% names(dt)) {
  dt[, math_max := suppressWarnings(as.numeric(math_max))]
  dt[!is.na(psu_year) & !is.na(math_max) & math_max > 0,
     z_year_math_max := (math_max - mean(math_max, na.rm = TRUE)) / sd(math_max, na.rm = TRUE),
     by = psu_year]
}
if (!("z_year_leng_max" %in% names(dt)) && "leng_max" %in% names(dt)) {
  dt[, leng_max := suppressWarnings(as.numeric(leng_max))]
  dt[!is.na(psu_year) & !is.na(leng_max) & leng_max > 0,
     z_year_leng_max := (leng_max - mean(leng_max, na.rm = TRUE)) / sd(leng_max, na.rm = TRUE),
     by = psu_year]
}

if (!("admission_exam_taker" %in% names(dt))) {
  exam_score_vars <- intersect(c("z_year_math_max", "z_year_leng_max", "math_max", "leng_max"), names(dt))
  if (length(exam_score_vars) > 0) {
    dt[, admission_exam_taker := as.integer(rowSums(!is.na(.SD)) > 0), .SDcols = exam_score_vars]
  }
}

# ── 2. School dependency (lookup by school RBD, no student-level join needed) ─
message("Reading school_predictors.csv ...")
sch <- fread(
  file.path(repo_wd, "data", "clean", "staff_lasso_va", "school_predictors.csv"),
  select = c("RBD", "school__dependency_4"),
  na.strings = c("", "NA")
)
setnames(sch, c("rbd", "dep3"))
sch[, rbd  := suppressWarnings(as.numeric(rbd))]
sch[, dep3 := suppressWarnings(as.numeric(dep3))]
sch <- unique(sch, by = "rbd")
# join on school RBD — use a plain lookup to avoid any column collision
dep_map <- sch$dep3;  names(dep_map) <- as.character(sch$rbd)
dt[, private := dep_map[as.character(most_time_rbd)]]
dt[, private := as.integer(private == 1)]

# ── 3. SAE applicant and lottery-risk flags ───────────────────────────────────
# Assignment risk is defined from the full simulated assignment distribution;
# it does not use the legacy k=100 probability-control support restriction.
message("Reading 2018--2020 assignment probabilities ...")
risk_flags <- rbindlist(lapply(2018:2020, function(year) {
  probs <- fread(
    file.path(clean_dir, "DA_probs", paste0("DA_probs_", year, ".csv")),
    select = c("student_id", "prob"),
    na.strings = c("", "NA"),
    showProgress = FALSE
  )
  probs[, student_id := suppressWarnings(as.numeric(student_id))]
  probs[, .(any_risk = as.integer(max(prob, na.rm = TRUE) < 1)), by = student_id][
    , sae_proceso := year
  ]
}))
dt[, student_id := suppressWarnings(as.numeric(student_id))]
dt[, sae_proceso := suppressWarnings(as.integer(sae_proceso))]
dt <- merge(dt, risk_flags, by = c("student_id", "sae_proceso"), all.x = TRUE, sort = FALSE)
dt[is.na(any_risk), any_risk := 0L]

# ── 4. Group indicators ───────────────────────────────────────────────────────
dt[, g_broad      := 1L]
dt[, g_nonprivate := as.integer(!is.na(private) & private == 0)]
dt[, g_sae_app    := as.integer(cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L)]
dt[, g_sae_risk   := as.integer(cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L & any_risk == 1)]
dt[, g_private    := as.integer(!is.na(private) & private == 1)]

message("Group sizes:")
message("  Broad panel:           ", sum(dt$g_broad, na.rm = TRUE))
message("  Public Funding school: ", sum(dt$g_nonprivate, na.rm = TRUE))
message("  SAE applicants:        ", sum(dt$g_sae_app, na.rm = TRUE))
message("  SAE applicants w risk: ", sum(dt$g_sae_risk, na.rm = TRUE))
message("  Private HS:            ", sum(dt$g_private, na.rm = TRUE))

# ── 5. Higher ed enrollment from SIES code ───────────────────────────────────
if ("cod_sies_m1" %in% names(dt)) {
  dt[, cod_sies_m1 := trimws(as.character(cod_sies_m1))]
  dt[, higher_ed_enrolled_m1 := as.integer(
    !is.na(cod_sies_m1) & nzchar(cod_sies_m1) & cod_sies_m1 != "NA"
  )]
}

# ── Numeric outcome columns ───────────────────────────────────────────────────
num_cols <- c(
  "female", "in_rm", "edad_alu",
  "income_decile_imputed", "mother_educ_years_imputed",
  "z_sim_mat_4to", "z_sim_leng_4to",
  "z_gpa_middle_mean", "z_att_middle_mean",
  "z_year_math_max", "z_year_leng_max",
  "admission_exam_taker",
  "higher_ed_enrolled_m1", "high_inst_m1", "high_paying_field_m1"
)
for (v in num_cols) {
  if (v %in% names(dt)) dt[[v]] <- suppressWarnings(as.numeric(dt[[v]]))
}

# ── 6. Compute means (and match rates for SIMCE / middle school) ───────────────
groups <- c("g_broad", "g_nonprivate", "g_sae_app", "g_sae_risk", "g_private")

mean_row <- function(data, group_col, var) {
  sub <- data[get(group_col) == 1]
  n   <- nrow(sub)
  if (n == 0 || !var %in% names(sub)) return(list(mean = NA_real_, n = 0L, match = NA_real_))
  obs <- sub[!is.na(get(var))]
  list(
    mean  = mean(sub[[var]], na.rm = TRUE),
    n     = n,
    match = nrow(obs) / n
  )
}

build_group_stats <- function(var, match_var = FALSE) {
  lapply(groups, function(g) {
    r <- mean_row(dt, g, var)
    r$group <- g
    r$var   <- var
    r$show_match <- match_var
    r
  })
}

# ── 7. Define rows ────────────────────────────────────────────────────────────
row_specs <- list(
  list(var = "female",               label = "Female",                       match = FALSE),
  list(var = "in_rm",                label = "Metropolitan Region (Santiago)", match = FALSE),
  list(var = "edad_alu",             label = "Age",                          match = FALSE),
  list(var = "income_decile_imputed", label = "Income decile",                match = FALSE),
  list(var = "mother_educ_years_imputed", label = "Mother education years",   match = FALSE),
  list(var = "z_sim_mat_4to",        label = "SIMCE 4th math (z)",           match = FALSE),
  list(var = "z_sim_leng_4to",       label = "SIMCE 4th verbal (z)",         match = FALSE),
  list(var = "z_gpa_middle_mean",    label = "Middle school GPA (z)",        match = FALSE),
  list(var = "z_att_middle_mean",    label = "Middle school attend. (z)",    match = FALSE),
  list(var = "z_year_math_max",      label = "Std. math exam",               match = FALSE),
  list(var = "z_year_leng_max",      label = "Std. verbal exam",             match = FALSE),
  list(var = "admission_exam_taker", label = "Admission exam taker",          match = FALSE),
  list(var = "higher_ed_enrolled_m1",label = "Higher ed. enrolled",          match = FALSE),
  list(var = "high_inst_m1",         label = "Hi-premium institution",       match = FALSE),
  list(var = "high_paying_field_m1", label = "Hi-premium field",             match = FALSE)
)

# drop any row_specs whose variable isn't present in dt
row_specs <- Filter(function(rs) rs$var %in% names(dt), row_specs)

# Compute a flat results table
results <- rbindlist(lapply(row_specs, function(rs) {
  rbindlist(lapply(groups, function(g) {
    r <- mean_row(dt, g, rs$var)
    data.table(
      var       = rs$var,
      label     = rs$label,
      show_match= rs$match,
      group     = g,
      n         = r$n,
      mean      = r$mean,
      match     = r$match
    )
  }))
}), fill = TRUE)

# ── 8. Group Ns ───────────────────────────────────────────────────────────────
group_ns <- sapply(groups, function(g) sum(dt[[g]], na.rm = TRUE))

# ── 9. Build LaTeX ────────────────────────────────────────────────────────────
group_labels <- c(
  g_broad      = "Broad Panel\\\\(All HS)",
  g_nonprivate = "Public Funding\\\\HS",
  g_sae_app    = "SAE\\\\Applicants",
  g_sae_risk   = "SAE Applicants\\\\w/ risk",
  g_private    = "Private\\\\HS"
)

fmt_mean  <- function(x, var) {
  if (is.na(x)) return("---")
  if (var %in% c("female", "in_rm", "admission_exam_taker",
                 "higher_ed_enrolled_m1", "high_inst_m1", "high_paying_field_m1")) {
    sprintf("%.3f", x)   # proportions: 3 dp
  } else if (var %in% c("edad_alu", "income_decile_imputed", "mother_educ_years_imputed")) {
    sprintf("%.1f", x)
  } else {
    sprintf("%.3f", x)
  }
}

fmt_match <- function(x) if (is.na(x)) "---" else sprintf("%.1f\\%%", x * 100)

block_match_rate <- function(group_col, vars) {
  vars <- vars[vars %in% names(dt)]
  if (length(vars) == 0) return(NA_real_)
  sub <- dt[get(group_col) == 1]
  if (nrow(sub) == 0) return(NA_real_)
  mean(complete.cases(sub[, ..vars]))
}

make_block_match_row <- function(label, vars, selected_groups) {
  match_cells <- sapply(selected_groups, function(g) fmt_match(block_match_rate(g, vars)))
  paste0(
    "\\quad\\footnotesize{", label, "} & ",
    paste(paste0("\\footnotesize{", match_cells, "}"), collapse = " & "),
    " \\\\"
  )
}

make_data_row <- function(rs_label, rs_var, rs_match, selected_groups) {
  cells <- sapply(selected_groups, function(g) {
    r <- results[var == rs_var & group == g]
    fmt_mean(r$mean, rs_var)
  })
  main_row <- paste0(rs_label, " & ", paste(cells, collapse = " & "), " \\\\")

  if (rs_match) {
    match_cells <- sapply(selected_groups, function(g) {
      r <- results[var == rs_var & group == g]
      fmt_match(r$match)
    })
    match_row <- paste0("\\quad\\footnotesize{\\% matched} & ",
                        paste(paste0("\\footnotesize{", match_cells, "}"), collapse = " & "),
                        " \\\\")
    c(main_row, match_row)
  } else {
    main_row
  }
}

section_header <- function(txt, n_groups) {
  paste0("\\multicolumn{", n_groups + 1L, "}{l}{\\textit{", txt, "}} \\\\[2pt]")
}

maybe_row <- function(label, var, match, selected_groups) {
  if (!var %in% names(dt)) return(NULL)
  make_data_row(label, var, match, selected_groups)
}

baseline_simce_vars <- c(
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "income_decile_imputed",
  "mother_educ_years_imputed"
)
middle_school_vars <- c("z_gpa_middle_mean", "z_att_middle_mean")

make_body_rows <- function(selected_groups) {
  n_groups <- length(selected_groups)
  c(
    section_header("Student characteristics", n_groups),
    maybe_row("Female", "female", FALSE, selected_groups),
    maybe_row("Metropolitan Region (Santiago)", "in_rm", FALSE, selected_groups),
    maybe_row("Age", "edad_alu", FALSE, selected_groups),
    "\\midrule",
    section_header("Baseline SIMCE variables", n_groups),
    maybe_row("Math 4th grade (z)", "z_sim_mat_4to", FALSE, selected_groups),
    maybe_row("Verbal 4th grade (z)", "z_sim_leng_4to", FALSE, selected_groups),
    maybe_row("Income decile", "income_decile_imputed", FALSE, selected_groups),
    maybe_row("Mother education years", "mother_educ_years_imputed", FALSE, selected_groups),
    make_block_match_row("\\% matched all SIMCE vars", baseline_simce_vars, selected_groups),
    "\\midrule",
    section_header("Middle school", n_groups),
    maybe_row("GPA (z)", "z_gpa_middle_mean", FALSE, selected_groups),
    maybe_row("Attendance (z)", "z_att_middle_mean", FALSE, selected_groups),
    make_block_match_row("\\% matched all middle-school vars", middle_school_vars, selected_groups),
    "\\midrule",
    section_header("Outcomes", n_groups),
    maybe_row("Std. math exam", "z_year_math_max", FALSE, selected_groups),
    maybe_row("Std. verbal exam", "z_year_leng_max", FALSE, selected_groups),
    maybe_row("Admission exam taker", "admission_exam_taker", FALSE, selected_groups),
    maybe_row("Higher ed. enrolled", "higher_ed_enrolled_m1", FALSE, selected_groups),
    maybe_row("Hi-premium institution", "high_inst_m1", FALSE, selected_groups),
    maybe_row("Hi-premium field", "high_paying_field_m1", FALSE, selected_groups)
  )
}

make_balance_latex <- function(selected_groups, caption, table_label, notes) {
  col_header <- paste0(
    " & ",
    paste(sapply(group_labels[selected_groups], function(l) paste0("\\shortstack{", l, "}")), collapse = " & "),
    " \\\\"
  )
  n_row <- paste0(
    "$N$ & ",
    paste(format(group_ns[selected_groups], big.mark = ","), collapse = " & "),
    " \\\\"
  )
  pct_row <- paste0(
    "Percent of broad panel & ",
    paste(sprintf("%.1f\\%%", 100 * group_ns[selected_groups] / group_ns[["g_broad"]]), collapse = " & "),
    " \\\\"
  )
  c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", table_label, "}"),
    "\\small",
    "\\setlength{\\tabcolsep}{4pt}",
    paste0("\\begin{tabular}{@{}l", paste(rep("c", length(selected_groups)), collapse = ""), "@{}}"),
    "\\toprule",
    col_header,
    "\\midrule",
    make_body_rows(selected_groups),
    "\\midrule",
    n_row,
    pct_row,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\medskip\\footnotesize",
    "\\begin{minipage}{\\textwidth}",
    notes,
    "\\end{minipage}",
    "\\end{table}"
  )
}

sample_groups <- c("g_broad", "g_sae_app", "g_sae_risk")
sector_groups <- c("g_broad", "g_nonprivate", "g_private")

# The comparison groups overlap, so the usual independent-samples standard error
# is incorrect. These influence-function standard errors retain that covariance.
overlap_mean_difference <- function(var, positive_group, negative_group) {
  y <- dt[[var]]
  positive_obs <- dt[[positive_group]] == 1L & !is.na(y)
  negative_obs <- dt[[negative_group]] == 1L & !is.na(y)
  n_positive <- sum(positive_obs)
  n_negative <- sum(negative_obs)
  mean_positive <- mean(y[positive_obs])
  mean_negative <- mean(y[negative_obs])
  influence <- numeric(length(y))
  influence[positive_obs] <- influence[positive_obs] + (y[positive_obs] - mean_positive) / n_positive
  influence[negative_obs] <- influence[negative_obs] - (y[negative_obs] - mean_negative) / n_negative
  list(
    difference = mean_positive - mean_negative,
    se = sqrt(sum(influence^2)),
    influence = influence
  )
}

joint_difference_test <- function(positive_group, negative_group, vars) {
  stats <- lapply(
    vars,
    overlap_mean_difference,
    positive_group = positive_group,
    negative_group = negative_group
  )
  differences <- vapply(stats, `[[`, numeric(1), "difference")
  influence <- do.call(cbind, lapply(stats, `[[`, "influence"))
  variance <- crossprod(influence)
  eig <- eigen(variance, symmetric = TRUE)
  keep <- eig$values > max(eig$values) * 1e-10
  projections <- crossprod(eig$vectors[, keep, drop = FALSE], differences)
  statistic <- sum(projections^2 / eig$values[keep])
  pchisq(statistic, df = sum(keep), lower.tail = FALSE)
}

make_sample_data_row <- function(label, var, show_match) {
  if (!var %in% names(dt)) return(NULL)
  row_var <- var
  means <- sapply(sample_groups, function(g) {
    row_group <- g
    r <- results[which(results$var == row_var & results$group == row_group)]
    fmt_mean(r$mean, var)
  })
  app_diff <- overlap_mean_difference(var, "g_sae_app", "g_broad")
  risk_diff <- overlap_mean_difference(var, "g_sae_risk", "g_broad")
  risk_app_diff <- overlap_mean_difference(var, "g_sae_risk", "g_sae_app")
  row <- paste0(
    label, " & ", means[["g_broad"]],
    " & ", means[["g_sae_app"]], " & ", sprintf("%.3f (%.3f)", app_diff$difference, app_diff$se),
    " & ", means[["g_sae_risk"]], " & ", sprintf("%.3f (%.3f)", risk_diff$difference, risk_diff$se),
    " & ", sprintf("%.3f (%.3f)", risk_app_diff$difference, risk_app_diff$se),
    " \\\\"
  )
  if (!show_match) return(row)
  matches <- sapply(sample_groups, function(g) {
    row_group <- g
    r <- results[which(results$var == row_var & results$group == row_group)]
    fmt_match(r$match)
  })
  c(
    row,
    paste0(
      "\\quad\\footnotesize{\\% matched} & ",
      paste0("\\footnotesize{", matches[["g_broad"]], "}"),
      " & ", paste0("\\footnotesize{", matches[["g_sae_app"]], "}"), " & ",
      " & ", paste0("\\footnotesize{", matches[["g_sae_risk"]], "}"), " & ",
      " & ",
      " \\\\"
    )
  )
}

make_sample_block_match_row <- function(label, vars) {
  matches <- sapply(sample_groups, function(g) fmt_match(block_match_rate(g, vars)))
  paste0(
    "\\quad\\footnotesize{", label, "} & ",
    paste0("\\footnotesize{", matches[["g_broad"]], "}"),
    " & ", paste0("\\footnotesize{", matches[["g_sae_app"]], "}"), " & ",
    " & ", paste0("\\footnotesize{", matches[["g_sae_risk"]], "}"), " & ",
    " & ",
    " \\\\"
  )
}

make_sample_body <- function() {
  c(
    section_header("Student characteristics", 6L),
    make_sample_data_row("Female", "female", FALSE),
    make_sample_data_row("Metropolitan Region (Santiago)", "in_rm", FALSE),
    make_sample_data_row("Age", "edad_alu", FALSE),
    "\\midrule",
    section_header("Baseline SIMCE variables", 6L),
    make_sample_data_row("Math 4th grade (z)", "z_sim_mat_4to", FALSE),
    make_sample_data_row("Verbal 4th grade (z)", "z_sim_leng_4to", FALSE),
    make_sample_data_row("Income decile", "income_decile_imputed", FALSE),
    make_sample_data_row("Mother education years", "mother_educ_years_imputed", FALSE),
    make_sample_block_match_row("\\% matched all SIMCE vars", baseline_simce_vars),
    "\\midrule",
    section_header("Middle school", 6L),
    make_sample_data_row("GPA (z)", "z_gpa_middle_mean", FALSE),
    make_sample_data_row("Attendance (z)", "z_att_middle_mean", FALSE),
    make_sample_block_match_row("\\% matched all middle-school vars", middle_school_vars),
    "\\midrule",
    section_header("Outcomes", 6L),
    make_sample_data_row("Std. math exam", "z_year_math_max", FALSE),
    make_sample_data_row("Std. verbal exam", "z_year_leng_max", FALSE),
    make_sample_data_row("Admission exam taker", "admission_exam_taker", FALSE),
    make_sample_data_row("Higher ed. enrolled", "higher_ed_enrolled_m1", FALSE),
    make_sample_data_row("Hi-premium institution", "high_inst_m1", FALSE),
    make_sample_data_row("Hi-premium field", "high_paying_field_m1", FALSE)
  )
}

joint_vars <- c(
  "female", "in_rm", "edad_alu", "income_decile_imputed", "mother_educ_years_imputed",
  "z_sim_mat_4to", "z_sim_leng_4to",
  "z_gpa_middle_mean", "z_att_middle_mean"
)
joint_p_app <- joint_difference_test("g_sae_app", "g_broad", joint_vars)
joint_p_risk <- joint_difference_test("g_sae_risk", "g_broad", joint_vars)
joint_p_risk_app <- joint_difference_test("g_sae_risk", "g_sae_app", joint_vars)
fmt_p <- function(x) if (x < 0.001) "$<0.001$" else sprintf("%.3f", x)

sample_latex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Balance across analysis samples}",
  "\\label{tab:balance_samples}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{2pt}",
  "\\begin{tabular}{@{}lcccccc@{}}",
  "\\toprule",
  " & \\shortstack{Broad Panel\\\\(All HS)} & \\multicolumn{2}{c}{SAE Applicants} & \\multicolumn{2}{c}{SAE Applicants w/ risk} & \\shortstack{Risk SAE -\\\\All SAE} \\\\",
  "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}",
  " & Mean & Mean & \\shortstack{Diff.\\\\(2)-(1)\\\\(SE)} & Mean & \\shortstack{Diff.\\\\(4)-(1)\\\\(SE)} & \\shortstack{Diff.\\\\(4)-(2)\\\\(SE)} \\\\",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  make_sample_body(),
  "\\midrule",
  paste0("$N$ & ", format(group_ns[["g_broad"]], big.mark = ","), " & ",
         format(group_ns[["g_sae_app"]], big.mark = ","), " & & ",
         format(group_ns[["g_sae_risk"]], big.mark = ","), " & & \\\\"),
  paste0("Percent of broad panel & 100.0\\% & ",
         sprintf("%.1f\\%%", 100 * group_ns[["g_sae_app"]] / group_ns[["g_broad"]]), " & & ",
         sprintf("%.1f\\%%", 100 * group_ns[["g_sae_risk"]] / group_ns[["g_broad"]]), " & & \\\\"),
  paste0("Joint test $p$-value & & & ", fmt_p(joint_p_app), " & & ", fmt_p(joint_p_risk), " & ", fmt_p(joint_p_risk_app), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\medskip\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "Notes: Broad Panel comprises cohorts 2017--2020 with an observed attended high-school RBD and age 12--16. ",
    "SAE Applicants are timely grade-9 applicants in cohorts 2018--2020; ",
    "\\textit{w/ risk} further requires $\\max_j p_{ij} < 1$ in the full simulated assignment distribution. ",
    "Difference columns report raw mean differences: columns (2)--(1), (4)--(1), and (4)--(2), respectively. ",
    "Parentheses contain influence-function standard errors ",
    "that account for overlap between the groups. The joint test covers the baseline characteristics above the Outcomes section. ",
    "Percentages are relative to the Broad Panel. The SIMCE and middle-school blocks report complete-variable match rates."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

sector_latex <- make_balance_latex(
  sector_groups,
  "Balance by school funding type",
  "tab:balance_school_sector",
  paste0(
    "Notes: Broad Panel comprises cohorts 2017--2020 with an observed attended high-school RBD and age 12--16. ",
    "Public Funding HS includes municipal, local public education service, and subsidized private high schools; ",
    "Private HS denotes particular pagado high schools. Students with missing school dependency are excluded ",
    "from both school-type columns. Percentages are relative to the Broad Panel. The SIMCE and middle-school blocks ",
    "report group means among matched students together with complete-variable match rates. Outcome means use all available observations."
  )
)

sample_tex_path <- file.path(table_dir, "balance_table_samples.tex")
sector_tex_path <- file.path(table_dir, "balance_table_school_sector.tex")
tex_path <- file.path(table_dir, "balance_table.tex")
csv_path <- file.path(table_dir, "balance_table.csv")
writeLines(sample_latex, sample_tex_path)
writeLines(sector_latex, sector_tex_path)
writeLines(c(sample_latex, "", sector_latex), tex_path)
fwrite(results, csv_path)
message("Wrote: ", sample_tex_path)
message("Wrote: ", sector_tex_path)
message("Wrote: ", tex_path)
message("Wrote: ", csv_path)
print(dcast(results, label ~ group, value.var = "mean"))
