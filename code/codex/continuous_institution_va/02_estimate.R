# School VA for the continuous, field-adjusted institution premium.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
options(lfe.threads = 4L, lfe.bootmem = 500, lfe.eps = 1e-8)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
clean <- file.path(repo, "data/clean/continuous_institution_va")
spec <- readRDS(file.path(clean, "specification.rds"))
va_lines <- readLines(spec$source_code, warn = FALSE)
va_env <- new.env(parent = globalenv())
eval(parse(text = va_lines[seq_len(grep("^# ---.*Load and prepare data", va_lines) - 1L)]), envir = va_env)
d <- as.data.table(readRDS(file.path(clean, "analysis_data.rds")))
weighted_var <- function(x, w) sum(w * (x - weighted.mean(x, w))^2) / sum(w)
outcomes <- "institution_fe_enrolled_m1"
if ("--resume-completed" %in% commandArgs(trailingOnly = TRUE)) {
  # Recover final exports after an interruption only when each outcome already
  # has its complete SE/EB file and passing estimation checks. No refitting.
  existing_checks <- fread(file.path(clean, "estimation_verification.csv"))
  existing_summary <- fread(file.path(clean, "summary.csv"))
  stopifnot(setequal(existing_checks$outcome, outcomes), all(existing_checks$passed),
    setequal(existing_summary$outcome, outcomes))
  complete_values <- rbindlist(lapply(outcomes, function(o) {
    z <- fread(file.path(clean, paste0(o, "_school_values.csv")))
    stopifnot(all(z$outcome == o), all(is.finite(z$controlled_value_added_se)),
      all(is.finite(z$controlled_value_added_eb_centered_student)),
      sum(z$n_students_regression) == existing_summary[outcome == o, n_students])
    z
  }))
  fwrite(complete_values, file.path(clean, "school_value_added.csv"))
  saveRDS(sessionInfo(), file.path(clean, "session_info.rds"))
  message("Recovered final exports from completed and verified SE/EB checkpoints")
  quit(save = "no", status = 0L)
}
results <- list(); summaries <- list(); checks <- list()
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]
  x <- as.data.frame(d[is.finite(get(outcome)), c("MRUN", outcome, spec$control_vars, spec$fixed_effect_vars), with = FALSE])
  form <- as.formula(paste(outcome, "~", paste(spec$control_terms, collapse = " + "), "|", paste(spec$fixed_effect_vars, collapse = " + ")))
  message(format(Sys.time()), ": fitting ", outcome, "; eligible N=", nrow(x))
  fit <- feols(form, data = x, notes = FALSE)
  saveRDS(fit, file.path(clean, paste0(outcome, "_model.rds")), compress = FALSE)
  used <- x[obs(fit), , drop = FALSE]
  used$.model_resid <- as.numeric(resid(fit))
  outcome_mean <- mean(used[[outcome]])
  school_fe <- fixef(fit)[["school_rbd"]]
  counts <- as.data.table(used)[, .(n_students_regression = .N, controlled_value_added_resid_sd = sd(.model_resid)), by = school_rbd]
  value <- merge(data.table(school_rbd = as.numeric(names(school_fe)), controlled_value_added = as.numeric(school_fe)), counts, by = "school_rbd")
  value[, `:=`(analysis_sample = "All", outcome = outcome, n_students_regression_total = nobs(fit))]
  value[, controlled_value_added_centered_student := controlled_value_added - weighted.mean(controlled_value_added, n_students_regression)]
  value[, controlled_value_added_centered := controlled_value_added - mean(controlled_value_added)]
  value[, controlled_adjusted_mean_student := outcome_mean + controlled_value_added_centered_student]
  # Check the actual regression residuals against every included FE and regressor.
  X <- model.matrix(as.formula(paste("~", paste(spec$control_terms, collapse = " + "))), used)
  normal_error <- max(abs(crossprod(X, used$.model_resid))) / nrow(used)
  fe_errors <- vapply(spec$fixed_effect_vars, function(v) {
    max(abs(rowsum(used$.model_resid, used[[v]], reorder = FALSE))) / nrow(used)
  }, numeric(1))
  rm(X)
  stopifnot(normal_error < 1e-5, max(fe_errors) < 1e-5,
    sum(value$n_students_regression) == nobs(fit), !anyDuplicated(value$school_rbd))
  fwrite(as.data.table(used)[, .(MRUN, school_rbd)], file.path(clean, paste0(outcome, "_estimation_ids.csv")))
  fwrite(value, file.path(clean, paste0(outcome, "_point_estimates.csv")))
  n_eligible <- nrow(x); model_n <- nobs(fit)
  model_r2 <- 1 - sum(used$.model_resid^2) / sum((used[[outcome]] - outcome_mean)^2)
  # The saved fixest model carries a large score matrix. Release it before the
  # auxiliary lfe/bootstrap allocation; all point-estimate checks are complete.
  rm(fit, x); invisible(gc())
  message(format(Sys.time()), ": estimating centered-school SEs with bN=", spec$bootstrap_reps)
  set.seed(spec$seed + j - 1L)
  se <- va_env$estimate_school_regression_se(regression_data = used, outcome = outcome,
    controls = spec$control_terms, fixed_effect_vars = spec$fixed_effect_vars,
    regression_counts = counts, vcov_type = "iid", bootstrap_reps = spec$bootstrap_reps,
    estimable_function = "school_centered_student")
  value <- merge(value, as.data.table(se), by = "school_rbd", all.x = TRUE)
  stopifnot(all(is.finite(value$controlled_value_added_se)), all(value$controlled_value_added_se >= 0))
  # Identical student-weighted normal-normal EB rule to the main VA pipeline.
  w <- value$n_students_regression
  raw <- value$controlled_value_added_centered_student
  se2 <- value$controlled_value_added_se^2
  mu <- weighted.mean(raw, w)
  observed_var <- weighted_var(raw, w)
  mean_noise <- weighted.mean(se2, w)
  tau2 <- max(observed_var - mean_noise, 0)
  value[, `:=`(eb_tau2 = tau2, eb_tau = sqrt(tau2), eb_noise_variance = se2,
    eb_reliability = if (tau2 == 0) 0 else tau2 / (tau2 + se2))]
  value[, controlled_value_added_eb := mu + eb_reliability * (controlled_value_added_centered_student - mu)]
  value[, controlled_value_added_eb_centered_student := controlled_value_added_eb - weighted.mean(controlled_value_added_eb, n_students_regression)]
  value[, controlled_value_added_eb_centered := controlled_value_added_eb - mean(controlled_value_added_eb)]
  value[, eb_method := "normal_normal_weighted_by_n_students_regression"]
  stopifnot(abs(weighted.mean(raw, w)) < 1e-12,
    abs(weighted.mean(value$controlled_value_added_eb_centered_student, w)) < 1e-12,
    all(value$eb_reliability >= 0 & value$eb_reliability <= 1))
  # Verify shrinkage with a separately reconstructed vector calculation.
  eb_check <- mu + (tau2 / (tau2 + se2)) * (raw - mu)
  if (tau2 == 0) eb_check[] <- mu
  eb_check <- eb_check - weighted.mean(eb_check, w)
  stopifnot(max(abs(eb_check - value$controlled_value_added_eb_centered_student)) < 1e-12)
  summaries[[j]] <- data.table(outcome, n_eligible = n_eligible, n_students = model_n,
    n_schools = nrow(value), estimator_dropped = n_eligible - model_n, outcome_mean = mean(used[[outcome]]),
    outcome_sd = sd(used[[outcome]]), unshrunk_sd_student_weighted = sqrt(observed_var),
    mean_noise_sd_student_weighted = sqrt(mean_noise), eb_tau = sqrt(tau2),
    eb_sd_student_weighted = sqrt(weighted_var(value$controlled_value_added_eb_centered_student, w)),
    mean_reliability_student_weighted = weighted.mean(value$eb_reliability, w),
    bootstrap_reps = spec$bootstrap_reps, seed = spec$seed + j - 1L,
    r2 = model_r2)
  checks[[j]] <- data.table(outcome, normal_equation_error = normal_error, fe_normal_equation_error = max(fe_errors),
    weighted_raw_mean = weighted.mean(raw, w), weighted_eb_mean = weighted.mean(eb_check, w), passed = TRUE)
  results[[j]] <- value
  fwrite(value, file.path(clean, paste0(outcome, "_school_values.csv")))
  fwrite(rbindlist(summaries), file.path(clean, "summary.csv"))
  fwrite(rbindlist(checks), file.path(clean, "estimation_verification.csv"))
  message(format(Sys.time()), ": completed ", outcome)
  print(summaries[[j]])
  rm(used); invisible(gc())
}
fwrite(rbindlist(results), file.path(clean, "school_value_added.csv"))
saveRDS(sessionInfo(), file.path(clean, "session_info.rds"))
