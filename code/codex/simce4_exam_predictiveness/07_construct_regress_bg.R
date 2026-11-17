# Predicted entrance-exam performance and the realized score gap, 2017/2019.
# Run from the project root. Existing paper outcomes are read from the master.
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})
setDTthreads(min(4L, getDTthreads()))
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
clean_dir <- file.path(repo_wd, "data/clean/simce4_exam_predictiveness/bg_decomposition")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(clean_dir, "prediction_models"), showWarnings = FALSE)
master_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
simce_paths <- c(
  `2017` = file.path(repo_wd, "tmp/simce8_inputs/simce8b2017_students.csv"),
  `2019` = file.path(repo_wd, "tmp/simce8_inputs/simce8b2019_students.csv")
)
stopifnot(all(file.exists(c(master_path, simce_paths))))

checks <- data.table(check = character(), passed = logical())
check <- function(label, passed) {
  checks <<- rbind(checks, data.table(check = label, passed = isTRUE(passed)))
  if (!isTRUE(passed)) stop("Failed: ", label)
  message("PASS: ", label)
}
control_slopes <- c(
  "income_decile_imputed", "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed", "sala_cuna_imputed",
  "jardin_imputed", "prekinder_imputed", "kinder_imputed", "z_gpa_middle_mean", "z_att_middle_mean"
)
fixed_effects <- c("cohort_gr8", "GEN_ALU", "EDAD_ALU", "COD_COM_ALU", "middle_years_observed", "most_time_rbd_middle")
baseline_scores <- c("z_sim_mat_4to", "z_sim_leng_4to")
keep <- unique(c("mrun", fixed_effects, control_slopes, baseline_scores, "psu_year", "math_max", "leng_max",
                 "year_1st_app", "high_paying_field_m1", "COD_SIES_m1"))
header <- names(fread(master_path, nrows = 0L))
check("Master contains all existing outcomes and baseline controls", all(keep %in% header))
fwrite(data.table(path = master_path, bytes = file.info(master_path)$size,
  source_columns = length(header), selected_columns = length(keep),
  strategy = "Read selected columns only, then immediately filter to grade-8 cohorts 2017/2019"),
  file.path(clean_dir, "read_preflight.csv"))
message("Reading selected columns from the paper's linked master...")
d <- fread(master_path, select = keep, na.strings = c("", "NA"))
d <- d[cohort_gr8 %in% c(2017L, 2019L)]
check("One row per student in the two master cohorts", !anyNA(d$mrun) && !anyDuplicated(d$mrun))
flow <- data.table()
record_flow <- function(stage, x) {
  a <- x[, .(N = .N), by = cohort_gr8]
  a[, cohort_gr8 := as.character(cohort_gr8)]
  a <- rbind(a, data.table(cohort_gr8 = "All", N = nrow(x)))
  a[, stage := stage]
  flow <<- rbind(flow, a, use.names = TRUE)
}
record_flow("1. Master grade-8 cohorts 2017 and 2019", d)
d[, timely_exam_taker := as.integer(!is.na(psu_year) & psu_year == cohort_gr8 + 5L &
  is.finite(math_max) & math_max > 0 & is.finite(leng_max) & leng_max > 0)]
d <- d[timely_exam_taker == 1L]
record_flow("2. Both entrance-exam components observed in the expected admission year", d)

# Put PDT 2022 and PAES 2024 on comparable units before fitting predictions.
# The composite itself also has SD one within each admission year.
score_norm <- d[, .(N = .N, math_mean = mean(math_max), math_sd = sd(math_max),
  verbal_mean = mean(leng_max), verbal_sd = sd(leng_max)), by = psu_year]
d[, z_math := (math_max - mean(math_max)) / sd(math_max), by = psu_year]
d[, z_verbal := (leng_max - mean(leng_max)) / sd(leng_max), by = psu_year]
d[, composite_raw := (z_math + z_verbal) / 2]
composite_norm <- d[, .(composite_mean = mean(composite_raw), composite_sd = sd(composite_raw)), by = psu_year]
score_norm <- merge(score_norm, composite_norm, by = "psu_year", sort = TRUE)
d[, actual_score := (composite_raw - mean(composite_raw)) / sd(composite_raw), by = psu_year]
check("Actual composite has mean zero and SD one in each exam year",
  all(d[, abs(mean(actual_score)) < 1e-10 && abs(sd(actual_score) - 1) < 1e-10, by = psu_year]$V1))
fwrite(score_norm, file.path(clean_dir, "score_normalization.csv"))

read_simce8 <- function(path, year) {
  quote_setting <- if (year == 2019L) "" else "\""
  h <- names(fread(path, nrows = 0L, sep = "|", quote = quote_setting))
  h_clean <- gsub('"', "", h, fixed = TRUE)
  wanted <- c("mrun", "ptje_mate8b_alu", "ptje_lect8b_alu")
  stopifnot(all(wanted %in% h_clean))
  x <- fread(path, sep = "|", quote = quote_setting, select = match(wanted, h_clean), na.strings = c("", "NA"))
  setnames(x, wanted)
  x[, (wanted) := lapply(.SD, function(v) suppressWarnings(as.numeric(v))), .SDcols = wanted]
  x <- x[is.finite(mrun)]
  # Reuse the preceding exercise's treatment of duplicate scored/unscored rows.
  score_mean <- function(v) { v <- unique(v[is.finite(v)]); if (length(v)) mean(v) else NA_real_ }
  x <- x[, .(simce8_math_raw = score_mean(ptje_mate8b_alu),
    simce8_verbal_raw = score_mean(ptje_lect8b_alu)), by = mrun]
  x[, z_simce8_math := (simce8_math_raw - mean(simce8_math_raw, na.rm = TRUE)) / sd(simce8_math_raw, na.rm = TRUE)]
  x[, z_simce8_verbal := (simce8_verbal_raw - mean(simce8_verbal_raw, na.rm = TRUE)) / sd(simce8_verbal_raw, na.rm = TRUE)]
  x[, cohort_gr8 := year]
  x
}
message("Linking the existing grade-8 SIMCE extracts...")
simce8 <- rbindlist(lapply(names(simce_paths), function(y) read_simce8(simce_paths[[y]], as.integer(y))))
check("SIMCE lookup is unique by student and cohort", !anyDuplicated(simce8[, .(mrun, cohort_gr8)]))
d <- merge(d, simce8, by = c("mrun", "cohort_gr8"), all.x = TRUE, sort = FALSE)
d[, observed_simce8 := as.integer(is.finite(z_simce8_math) & is.finite(z_simce8_verbal))]
d <- d[observed_simce8 == 1L]
record_flow("3. Both grade-8 SIMCE scores observed", d)
required_controls <- c(control_slopes, fixed_effects, baseline_scores)
complete_controls <- d[, Reduce(`&`, lapply(.SD, is.finite)), .SDcols = required_controls]
d <- d[complete_controls]
record_flow("4. Complete baseline controls, including middle-school identity", d)
check("Expected admission years are 2022 and 2024", setequal(d$psu_year, c(2022L, 2024L)) && all(d$psu_year == d$cohort_gr8 + 5L))

# Reuse paper outcomes without rebuilding HE application or field definitions.
# year_1st_app covers any observed regular centralized application in 2021-2026.
d[, he_applied := as.integer(!is.na(year_1st_app))]
d[, premium_field := high_paying_field_m1]
check("Observed application years are within the master coverage window", all(d[!is.na(year_1st_app), year_1st_app] %in% 2021:2026))
check("Observed premium-field values are binary", all(d[!is.na(premium_field), premium_field] %in% c(0L, 1L)))
d[, fold := as.integer(abs(mrun) %% 5L) + 1L]
cubic <- function(v) paste0(v, " + I(", v, "^2) + I(", v, "^3)")
predictor_scores <- c(baseline_scores, "z_simce8_math", "z_simce8_verbal")
rhs <- paste(c(vapply(predictor_scores, cubic, character(1)), control_slopes), collapse = " + ")
prediction_formula <- as.formula(paste("actual_score ~", rhs, "|", paste(fixed_effects, collapse = " + ")))
writeLines(deparse(prediction_formula, width.cutoff = 500L), file.path(clean_dir, "prediction_formula.txt"))
d[, b_i := NA_real_]
d[, unseen_fixed_effect := FALSE]
fold_diagnostics <- list()
for (f in 1:5) {
  message("Fitting baseline score prediction, held-out fold ", f, "/5...")
  train <- d[fold != f]
  test <- d[fold == f]
  check(paste("Fold", f, "has no student overlap"), !any(test$mrun %in% train$mrun))
  fit <- feols(prediction_formula, data = train, warn = FALSE, notes = FALSE, nthreads = 4L, fixef.rm = "none")
  p <- as.numeric(predict(fit, newdata = test))
  unseen <- Reduce(`|`, lapply(fixed_effects, function(v) !test[[v]] %in% train[[v]]))
  check(paste("Fold", f, "missing predictions arise only from unseen fixed effects"), all(is.finite(p) | unseen))
  d[fold == f, `:=`(b_i = p, unseen_fixed_effect = unseen)]
  fold_diagnostics[[f]] <- data.table(fold = f, N_train = nrow(train), N_test = nrow(test),
    N_finite_prediction = sum(is.finite(p)), N_unseen_fixed_effect = sum(unseen),
    N_missing_prediction = sum(!is.finite(p)))
  saveRDS(fit, file.path(clean_dir, "prediction_models", paste0("fold_", f, ".rds")))
}
fwrite(rbindlist(fold_diagnostics), file.path(clean_dir, "fold_diagnostics.csv"))
d[, g_i := actual_score - b_i]
saveRDS(d, file.path(clean_dir, "prediction_roster.rds"))
d <- d[is.finite(b_i) & is.finite(g_i)]
record_flow("5. Out-of-fold prediction available", d)
d <- d[!is.na(premium_field)]
record_flow("6. Common sample for both requested HE outcomes", d)
check("One complete common student sample", !anyDuplicated(d$mrun) && all(is.finite(d$b_i) & is.finite(d$g_i)))
check("Predicted score plus gap equals actual score", max(abs(d$b_i + d$g_i - d$actual_score)) < 1e-12)
check("Non-enrollees retain the paper's zero premium-field definition",
  all(d[is.na(COD_SIES_m1) | trimws(as.character(COD_SIES_m1)) %in% c("", "NA"), premium_field] == 0L))

prediction_metrics <- function(x, label) data.table(cohort_gr8 = label, N = nrow(x),
  r_squared = 1 - sum(x$g_i^2) / sum((x$actual_score - mean(x$actual_score))^2),
  prediction_correlation = cor(x$actual_score, x$b_i),
  b_mean = mean(x$b_i), b_sd = sd(x$b_i), g_mean = mean(x$g_i), g_sd = sd(x$g_i),
  b_g_correlation = cor(x$b_i, x$g_i))
pd <- rbindlist(c(list(prediction_metrics(d, "All")), lapply(c(2017L, 2019L), function(y)
  prediction_metrics(d[cohort_gr8 == y], as.character(y)))))
fwrite(pd, file.path(clean_dir, "prediction_diagnostics.csv"))

models <- list(); covariances <- list(); coefficients <- list(); summaries <- list()
for (outcome in c("he_applied", "premium_field")) {
  fit <- lm(reformulate(c("b_i", "g_i"), response = outcome), data = d, x = TRUE, y = TRUE, model = TRUE)
  x <- fit$x
  bread <- solve(crossprod(x))
  cluster_scores <- rowsum(x * as.numeric(residuals(fit)), d$most_time_rbd_middle, reorder = FALSE)
  G <- nrow(cluster_scores); N <- nrow(x); K <- ncol(x)
  V <- G / (G - 1) * (N - 1) / (N - K) * bread %*% crossprod(cluster_scores) %*% bread
  dimnames(V) <- list(colnames(x), colnames(x))
  b <- coef(fit); se <- sqrt(diag(V)); p <- 2 * pt(-abs(b / se), df = G - 1)
  critical <- qt(.975, df = G - 1)
  coefficients[[outcome]] <- data.table(outcome = outcome, term = names(b), estimate = as.numeric(b),
    std_error = se, p_value = p, lower95 = b - critical * se, upper95 = b + critical * se)
  contrast <- c(0, 1, -1)
  contrast_se <- sqrt(as.numeric(t(contrast) %*% V %*% contrast))
  summaries[[outcome]] <- data.table(outcome = outcome, N = N, middle_schools = G,
    mean_outcome = mean(d[[outcome]]), r_squared = summary(fit)$r.squared,
    b_equals_g_p = 2 * pt(-abs((b["b_i"] - b["g_i"]) / contrast_se), df = G - 1))
  models[[outcome]] <- fit
  covariances[[outcome]] <- V
  check(paste(outcome, "has finite clustered covariance"), all(is.finite(V)) && all(eigen(V, symmetric = TRUE)$values > 0))
}
saveRDS(d, file.path(clean_dir, "student_scores.rds"))
saveRDS(models, file.path(clean_dir, "second_stage_models.rds"))
saveRDS(covariances, file.path(clean_dir, "covariance_matrices.rds"))
fwrite(rbindlist(coefficients), file.path(clean_dir, "coefficients.csv"))
fwrite(rbindlist(summaries), file.path(clean_dir, "regression_summary.csv"))
fwrite(flow, file.path(clean_dir, "sample_flow.csv"))
fwrite(d[, .(N = .N, middle_schools = uniqueN(most_time_rbd_middle),
  application_share = mean(he_applied), premium_field_share = mean(premium_field)), by = cohort_gr8],
  file.path(clean_dir, "outcomes_by_cohort.csv"))
manifest_paths <- c(master_path, simce_paths)
manifest <- data.table(role = c("Paper master database", "Staged grade-8 SIMCE 2017", "Staged grade-8 SIMCE 2019"),
  path = as.character(manifest_paths), bytes = file.info(manifest_paths)$size,
  modified = as.character(file.info(manifest_paths)$mtime))
fwrite(manifest, file.path(clean_dir, "source_manifest.csv"))
fwrite(checks, file.path(clean_dir, "construction_verification.csv"))
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
message("Finished: ", nrow(d), " students; ", nrow(checks), " construction checks passed.")
print(pd)
print(rbindlist(coefficients)[term %in% c("b_i", "g_i")])
