###############################################################################
# Residual balance diagnostic
#
# For each baseline observable W, compare SAE applicants with assignment risk to
# the broad panel after residualizing W on the remaining VA-style controls.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
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
clean_dir <- file.path(data_wd, "data", "clean")
output_dir <- file.path(repo_wd, "output", "tables", "balance_table")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading univ_gr8_df.csv ...")
dt <- fread(file.path(clean_dir, "univ_gr8_df.csv"), na.strings = c("", "NA"))
new_names <- tolower(names(dt))
dt <- dt[, !duplicated(new_names), with = FALSE]
setnames(dt, tolower(names(dt)))

num_vars <- c(
  "mrun", "student_id", "cohort_gr8", "sae_proceso", "timely_sae",
  "most_time_rbd", "edad_alu", "gen_alu", "cod_reg_alu", "cod_com_alu",
  "z_sim_mat_4to", "z_sim_leng_4to", "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean"
)
for (v in intersect(num_vars, names(dt))) {
  dt[[v]] <- suppressWarnings(as.numeric(dt[[v]]))
}

dt <- dt[cohort_gr8 %in% 2017:2020]
dt <- dt[!is.na(most_time_rbd) & most_time_rbd > 0]
dt <- dt[!is.na(edad_alu) & edad_alu >= 12 & edad_alu <= 16]
setkey(dt, mrun)
dt <- dt[!duplicated(mrun)]

dt[, female := as.integer(gen_alu == 2)]
dt[, metropolitan_region_santiago := as.integer(cod_reg_alu == 13)]

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
dt[, sae_risk := as.integer(cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L & any_risk == 1L)]

diagnostic_vars <- data.table(
  var = c(
    "female",
    "metropolitan_region_santiago",
    "edad_alu",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "income_decile_imputed",
    "mother_educ_years_imputed",
    "z_gpa_middle_mean",
    "z_att_middle_mean"
  ),
  label = c(
    "Female",
    "Metropolitan Region (Santiago)",
    "Age",
    "SIMCE math 4th grade (z)",
    "SIMCE verbal 4th grade (z)",
    "Income decile",
    "Mother education years",
    "Middle school GPA (z)",
    "Middle school attendance (z)"
  )
)
diagnostic_vars <- diagnostic_vars[var %in% names(dt)]

linear_controls <- c(
  "z_sim_mat_4to",
  "I(z_sim_mat_4to^2)",
  "I(z_sim_mat_4to^3)",
  "z_sim_leng_4to",
  "I(z_sim_leng_4to^2)",
  "I(z_sim_leng_4to^3)",
  "income_decile_imputed",
  "father_educ_years_imputed",
  "mother_educ_years_imputed",
  "father_indigenous_imputed",
  "mother_indigenous_imputed",
  "sala_cuna_imputed",
  "jardin_imputed",
  "prekinder_imputed",
  "kinder_imputed",
  "z_gpa_middle_mean",
  "z_att_middle_mean"
)
fixed_effect_controls <- c("cohort_gr8", "gen_alu", "edad_alu", "cod_com_alu", "middle_years_observed")
candidate_controls <- c(linear_controls, fixed_effect_controls)
controls_needed <- unique(gsub("^I\\((.*)\\^.*\\)$", "\\1", candidate_controls))
controls_needed <- controls_needed[controls_needed %in% names(dt)]

raw_difference <- function(y, g) {
  sample_obs <- g == 1L & !is.na(y)
  broad_obs <- !is.na(y)
  n_sample <- sum(sample_obs)
  n_broad <- sum(broad_obs)
  mean_sample <- mean(y[sample_obs])
  mean_broad <- mean(y[broad_obs])
  influence <- numeric(length(y))
  influence[sample_obs] <- influence[sample_obs] + (y[sample_obs] - mean_sample) / n_sample
  influence[broad_obs] <- influence[broad_obs] - (y[broad_obs] - mean_broad) / n_broad
  list(
    sample_mean = mean_sample,
    broad_mean = mean_broad,
    difference = mean_sample - mean_broad,
    se = sqrt(sum(influence^2)),
    n_sample = n_sample,
    n_broad = n_broad
  )
}

format_controls <- function(var) {
  controls <- linear_controls
  fixed_effects <- fixed_effect_controls
  if (var == "female") fixed_effects <- setdiff(fixed_effects, "gen_alu")
  if (var == "metropolitan_region_santiago") fixed_effects <- setdiff(fixed_effects, "cod_com_alu")
  if (var == "edad_alu") fixed_effects <- setdiff(fixed_effects, "edad_alu")
  if (var == "z_sim_mat_4to") controls <- setdiff(controls, c("z_sim_mat_4to", "I(z_sim_mat_4to^2)", "I(z_sim_mat_4to^3)"))
  if (var == "z_sim_leng_4to") controls <- setdiff(controls, c("z_sim_leng_4to", "I(z_sim_leng_4to^2)", "I(z_sim_leng_4to^3)"))
  if (var == "income_decile_imputed") controls <- setdiff(controls, "income_decile_imputed")
  if (var == "mother_educ_years_imputed") controls <- setdiff(controls, "mother_educ_years_imputed")
  if (var == "z_gpa_middle_mean") controls <- setdiff(controls, "z_gpa_middle_mean")
  if (var == "z_att_middle_mean") controls <- setdiff(controls, "z_att_middle_mean")
  list(controls = controls, fixed_effects = fixed_effects)
}

diagnose_one <- function(var, label) {
  control_spec <- format_controls(var)
  rhs <- if (length(control_spec$controls)) paste(control_spec$controls, collapse = " + ") else "1"
  fe_rhs <- if (length(control_spec$fixed_effects)) paste(control_spec$fixed_effects, collapse = " + ") else "0"
  formula_text <- paste(var, "~", rhs, "|", fe_rhs)
  needed <- unique(c(var, "sae_risk", controls_needed))
  needed <- needed[needed %in% names(dt)]
  model_data <- dt[complete.cases(dt[, ..needed])]
  raw <- raw_difference(model_data[[var]], model_data$sae_risk)
  fit <- feols(as.formula(formula_text), data = model_data, fixef.rm = "none", notes = FALSE)
  model_data[, residual := resid(fit)]
  residual <- raw_difference(model_data$residual, model_data$sae_risk)
  data.table(
    variable = var,
    label = label,
    n_broad = raw$n_broad,
    n_sae_risk = raw$n_sample,
    broad_mean = raw$broad_mean,
    sae_risk_mean = raw$sample_mean,
    raw_difference = raw$difference,
    raw_se = raw$se,
    residual_difference = residual$difference,
    residual_se = residual$se,
    residual_t = residual$difference / residual$se,
    residual_p = 2 * pnorm(-abs(residual$difference / residual$se))
  )
}

results <- rbindlist(Map(diagnose_one, diagnostic_vars$var, diagnostic_vars$label))
results[, row_order := match(variable, diagnostic_vars$var)]
setorder(results, row_order)
results[, row_order := NULL]

csv_path <- file.path(output_dir, "residual_balance_sae_risk_vs_broad.csv")
tex_path <- file.path(output_dir, "residual_balance_sae_risk_vs_broad.tex")
fwrite(results, csv_path)

fmt_num <- function(x, digits = 3) ifelse(is.na(x), "---", sprintf(paste0("%.", digits, "f"), x))
fmt_p <- function(x) ifelse(is.na(x), "---", ifelse(x < 0.001, "$<0.001$", sprintf("%.3f", x)))

lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Residual balance of SAE applicants with assignment risk relative to the broad panel}",
  "\\label{tab:residual-balance-sae-risk}",
  "\\small",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Variable & Broad mean & Risk mean & Raw diff. & Residual diff. & $p$-value \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(results))) {
  lines <- c(lines, paste0(
    results$label[i], " & ",
    fmt_num(results$broad_mean[i]), " & ",
    fmt_num(results$sae_risk_mean[i]), " & ",
    fmt_num(results$raw_difference[i]), " & ",
    fmt_num(results$residual_difference[i]), " & ",
    fmt_p(results$residual_p[i]), " \\\\"
  ))
}
lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{0.95\\textwidth}",
  "Notes: The raw difference is the SAE assignment-risk mean minus the broad-panel mean. The residual difference is the same difference after residualizing each variable on the value-added control set, excluding the variable or variable family being tested. The control set includes cohort, gender, age, student comuna, grade-4 SIMCE score polynomials, SIMCE family-background variables, early-childhood indicators, and middle-school controls when they are not the tested variable.",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(lines, tex_path)

message("Wrote: ", csv_path)
message("Wrote: ", tex_path)
print(results[, .(label, raw_difference, residual_difference, residual_p)])
