###############################################################################
# Lottery validity test: forecast bias + overidentification (Angrist, Hull,
# Pathak & Walters 2016b decomposition; see "Methods for measuring school
# effectiveness," Handbook of the Economics of Education, Section 5.3).
#
# This is a COMPANION to the existing codex pipeline, not a replacement for
# it. It reuses, unchanged, the estimation_df written by
#   code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R
# (student_id, sae_proceso, most_time_RBD, rbd_treated_1R, d_<spec>,
# z_<spec>, expected_<spec>, raw outcome columns, controls) -- this script
# reads that file directly rather than re-deriving the merges, so there is
# no risk of silently drifting from the sample the paper table actually used.
#
# WHAT'S NEW HERE: the existing script's z_<spec> instrument is already
# collapsed to ONE scalar per student (the VA of whichever single school was
# actually offered). That's exactly identified (1 instrument, 1 endogenous
# regressor: d_<spec>), so there's no way to test overidentification with it
# -- the forecast coefficient IS the whole test. To get a genuine
# overidentification test (does the forecast relationship hold uniformly
# across individual lotteries, not just on average?), this script instead
# instruments d_<spec> with the FULL set of risk-adjusted offer indicators
# (1{rbd_treated_1R == j} - prob_ij) for every school j with genuine
# oversubscription variation -- not the scalar collapse.
#
# IMPORTANT CAVEAT, checked and printed loudly below rather than assumed
# away: this is a "many instruments" problem, structurally different from
# (but not necessarily safer than) the literal-dummy + g(p_i) collinearity
# that broke code/claude/rc_vam/01_construct_rc_vam_school_values_noreg.R.
# There, school fixed effects collided with g(p_i) controls in ONE
# regression. Here there are no school fixed effects at all -- just one
# endogenous variable and many instruments -- but if the oversubscribed-
# school count L is large relative to the sample, naive 2SLS is still biased
# toward OLS (Bekker 1994; the many/weak-instruments literature), which is
# why AHPW's own Table 7 used an "asymptotically efficient GMM procedure"
# rather than textbook 2SLS. This script:
#   1. Reports L (the actual oversubscribed-school count) at several
#      thresholds BEFORE running anything, so that number is visible rather
#      than assumed.
#   2. Runs the overidentified IV via fixest's native multi-instrument
#      syntax (efficient/optimal-weight GMM once overidentified).
#   3. Prints an explicit WARNING per spec if L looks uncomfortably large
#      relative to the regression sample size, instead of quietly trusting
#      the result.
#
# OUTPUT: output/tables/lottery_validity_test/lottery_validity_test_results.csv
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
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

# Same idea as find_existing_path() but for files, not directories -- used
# below to find whichever regression_df CSV actually exists on this machine
# without requiring an env var to be set (RStudio users typically don't want
# to fuss with Sys.setenv() every session).
find_existing_file <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(
      "Could not find ", label, ". Checked:\n  ", paste(candidates, collapse = "\n  "),
      "\nSet ", env_var, " to point at the exact file you want, or run ",
      "code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R first.",
      call. = FALSE
    )
  }
  existing[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/xd-br/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    # Known-good repo root checked BEFORE getwd() -- dir.exists(getwd()) is
    # true almost by definition (you're always somewhere real), so if
    # getwd() were checked first it would "win" even when RStudio's working
    # directory is some other subfolder of the repo (e.g. this exact bug:
    # getwd() == .../code/claude/rss_debiased_varcov, a real directory, but
    # not the repo root that actually has output/tables/... under it).
    "C:/Users/brunem/Research/causal_schools",
    getwd(),
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
prob_dir <- file.path(clean_dir, "DA_probs")
eb_dir <- file.path(clean_dir, "empirical_bayes_school_va")
table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
out_dir <- file.path(repo_wd, "output", "tables", "lottery_validity_test")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Points at the SAME estimation_df the existing scalar-IV script wrote.
# Checked in order -- first one that actually exists on disk wins, no env
# var required:
#   1. the Stata-VA/EB "main five" regression_df -- this is the exact file
#      behind the paper's Table 3 numbers (0.884/0.885/0.497/0.803/0.311).
#   2. the Stata-VA/EB full regression_df (all specs, not just the main
#      five).
#   3. the plain-R EB IV script's own default output path.
# Set LOTTERY_TEST_REGRESSION_DF_PATH to override if you want a specific run.
regression_df_path <- find_existing_file(
  "LOTTERY_TEST_REGRESSION_DF_PATH",
  c(
    file.path(table_dir, "stata_scalar_school_value_iv_main_five_expected_va_eb_regression_df.csv"),
    file.path(table_dir, "stata_scalar_school_value_iv_expected_va_eb_regression_df.csv"),
    file.path(eb_dir, "scalar_school_value_iv_expected_va_eb.csv")
  ),
  "regression_df CSV"
)
sae_min_year <- as.integer(Sys.getenv("EB_IV_SAE_MIN_YEAR", unset = "2018"))
sae_max_year <- as.integer(Sys.getenv("EB_IV_SAE_MAX_YEAR", unset = "2020"))
oversub_min_students <- as.integer(Sys.getenv("LOTTERY_TEST_OVERSUB_MIN", unset = "50"))
warn_instrument_ratio <- as.numeric(Sys.getenv("LOTTERY_TEST_WARN_RATIO", unset = "0.01"))

results_path <- file.path(out_dir, "lottery_validity_test_results.csv")
oversub_diagnostics_path <- file.path(out_dir, "oversubscribed_school_diagnostics.csv")
log_ts("Reading existing estimation_df: ", regression_df_path)
estimation_df <- fread(regression_df_path, na.strings = c("", "NA"))
log_ts("  -> ", nrow(estimation_df), " rows, ", ncol(estimation_df), " cols.")

# Same spec -> outcome / require_exam_taker table as
# 02_run_expected_va_scalar_iv_eb.R's value_specs -- copied here rather than
# sourced, since that script doesn't expose it as a separate object. Kept in
# sync manually; if you add a spec there, add it here too.
value_specs <- data.table(
  spec = c(
    "math_adj_eb", "leng_adj_eb", "exam_adj_eb", "highered_adj_eb",
    "stem_adj_eb", "highpay_adj_eb", "highinst_adj_eb",
    "program_income_area_adj_eb", "program_income_institution_adj_eb",
    "program_income_full_adj_eb", "program_income_adj_eb",
    "progcert_adj_eb", "instcert_adj_eb", "anypost_adj_eb"
  ),
  outcome = c(
    "z_year_math_max", "z_year_leng_max", "admission_exam_taker", "higher_ed_enrolled_m1",
    "stem_enrollment_m1", "high_paying_field_m1", "high_inst_m1",
    "log_program_income_area_clp_m1", "log_program_income_institution_clp_m1",
    "log_program_income_full_clp_m1", "log_program_income_clp_m1",
    "program_certified_years_m1", "inst_certified_years_m1", "any_postulacion"
  ),
  require_exam_taker = c(
    TRUE, TRUE, FALSE, FALSE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,
    TRUE, TRUE,
    TRUE, TRUE, FALSE
  )
)

d_cols <- grep("^d_", names(estimation_df), value = TRUE)
specs_present <- sub("^d_", "", d_cols)
specs_present <- specs_present[
  paste0("z_", specs_present) %chin% names(estimation_df) &
    paste0("expected_", specs_present) %chin% names(estimation_df)
]
value_specs <- value_specs[spec %chin% specs_present & outcome %chin% names(estimation_df)]
log_ts("Specs usable in this estimation_df: ", paste(value_specs$spec, collapse = ", "))
if (nrow(value_specs) == 0) {
  stop("No usable spec found -- check LOTTERY_TEST_REGRESSION_DF_PATH.", call. = FALSE)
}

###############################################################################
# Step 1: build the LONG risk-adjusted-offer table (student_id, sae_proceso,
# rbd_prob, prob) -- same source files and same parsing as
# 02_run_expected_va_scalar_iv_eb.R's own DA_probs read, so "prob" here means
# exactly the same thing as the "expected_<spec>" columns already in
# estimation_df.
###############################################################################
log_ts("Reading long DA probability files (", sae_min_year, "-", sae_max_year, ")...")
prob_list <- lapply(seq.int(sae_min_year, sae_max_year), function(year) {
  path <- file.path(prob_dir, paste0("DA_probs_", year, ".csv"))
  if (!file.exists(path)) {
    log_ts("  (skipping missing file: ", path, ")")
    return(NULL)
  }
  dt <- fread(path, select = c("student_id", "school_id", "prob"))
  dt[, sae_proceso := as.integer(year)]
  dt[, rbd_prob := suppressWarnings(as.numeric(sub("_.*$", "", school_id)))]
  dt[, .(student_id = as.numeric(student_id), sae_proceso, rbd_prob, prob)]
})
prob_long <- rbindlist(prob_list[!sapply(prob_list, is.null)], use.names = TRUE)
log_ts("  -> ", nrow(prob_long), " (student, school) probability rows.")

sample_keys <- unique(estimation_df[, .(student_id, sae_proceso)])
prob_long <- merge(prob_long, sample_keys, by = c("student_id", "sae_proceso"))
log_ts("  -> ", nrow(prob_long), " rows after restricting to the regression sample's students.")

###############################################################################
# Step 2: THE DIAGNOSTIC THAT MUST BE CHECKED BEFORE TRUSTING ANYTHING BELOW.
# A school only contributes real identifying variation to the overid test if
# it has genuine oversubscription -- i.e. prob strictly between 0 and 1 for
# enough students (p=1 means certain, not a lottery; p just barely >0 for one
# student is not a usable instrument column). Count schools by how many
# students have 0 < prob < 1 there.
###############################################################################
oversub_counts <- prob_long[
  prob > 0 & prob < 1,
  .(n_oversub_students = uniqueN(student_id)),
  by = rbd_prob
]
setorder(oversub_counts, -n_oversub_students)
fwrite(oversub_counts, oversub_diagnostics_path)

thresholds <- c(10, 25, 50, 100, 200)
log_ts("=====================================================================")
log_ts("OVERSUBSCRIBED-SCHOOL DIAGNOSTIC -- read this before trusting the IV")
log_ts("results below. This is the actual instrument count L this test would")
log_ts("use at each candidate threshold (min students with 0<prob<1):")
for (thr in thresholds) {
  n_schools_at_thr <- sum(oversub_counts$n_oversub_students >= thr)
  log_ts("  threshold >= ", thr, " students -> L = ", n_schools_at_thr, " schools")
}
log_ts("Full per-school table written to: ", oversub_diagnostics_path)
log_ts("=====================================================================")

oversub_schools <- oversub_counts[n_oversub_students >= oversub_min_students, rbd_prob]
L <- length(oversub_schools)
log_ts(
  "Using LOTTERY_TEST_OVERSUB_MIN = ", oversub_min_students,
  " -> L = ", L, " oversubscribed schools used as instruments."
)
if (L < 2) {
  stop(
    "Fewer than 2 oversubscribed schools at this threshold -- overidentification ",
    "test is not meaningful. Lower LOTTERY_TEST_OVERSUB_MIN and re-check the ",
    "diagnostic table above first.", call. = FALSE
  )
}

###############################################################################
# Step 3: build the WIDE instrument matrix -- one pair of columns per
# oversubscribed school (its probability, and the offer-indicator-minus-
# probability instrument itself). Sparse, not densely collinear: unlike
# g(p_i), these are used as INSTRUMENTS for one endogenous variable, not as
# CONTROLS alongside literal per-school fixed effects.
###############################################################################
log_ts("Building wide risk-adjusted-offer instrument matrix (", L, " columns)...")
prob_wide <- dcast(
  prob_long[rbd_prob %in% oversub_schools],
  student_id + sae_proceso ~ rbd_prob,
  value.var = "prob",
  fill = 0
)
instrument_school_ids <- setdiff(names(prob_wide), c("student_id", "sae_proceso"))
prob_cols <- paste0("iv_prob_", instrument_school_ids)
setnames(prob_wide, instrument_school_ids, prob_cols)

iv_wide <- merge(
  estimation_df[, .(student_id, sae_proceso, rbd_treated_1R)],
  prob_wide,
  by = c("student_id", "sae_proceso"),
  all.x = TRUE
)
for (col in prob_cols) {
  iv_wide[is.na(get(col)), (col) := 0]
}
offer_cols <- character(L)
for (i in seq_along(instrument_school_ids)) {
  school_j <- as.numeric(instrument_school_ids[i])
  offer_col <- paste0("iv_offer_", instrument_school_ids[i])
  offer_cols[i] <- offer_col
  iv_wide[, (offer_col) := as.integer(!is.na(rbd_treated_1R) & rbd_treated_1R != 0 & rbd_treated_1R == school_j) - get(prob_cols[i])]
}
iv_wide <- iv_wide[, c("student_id", "sae_proceso", offer_cols), with = FALSE]
rm(prob_wide, prob_long)
gc(verbose = FALSE)
log_ts("Instrument matrix built (", length(offer_cols), " instrument columns).")

###############################################################################
# Step 4: for each spec, run (a) the forecast-bias test -- same single-
# instrument IV as the existing script, refit here on the exact same rows
# used for the overid test so the two are directly comparable -- and (b) the
# overidentified IV using the full instrument matrix. Sargan/Hansen J-stat is
# computed manually (regress the structural residual on the full instrument
# set + controls, N*R^2 ~ chi-sq(L-1) under the null of valid/homogeneous
# instruments) rather than via fitstat()'s "sargan" type -- that type's exact
# return shape isn't worth guessing at: a wrong silent number is worse than
# this fully self-contained, independently checkable computation.
###############################################################################
controls_base <- c("factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to", "factor(GEN_ALU)", "factor(EDAD_ALU)")
controls_rhs <- paste(controls_base, collapse = " + ")

# fixest's IV models sometimes name the endogenous-variable coefficient
# fit_<term> instead of <term>, depending on version -- same issue the
# existing 02_run_expected_va_scalar_iv_eb.R script guards against with its
# own extract_coef(). Reusing that exact fallback logic here.
extract_coef_name <- function(model, term) {
  coefs <- coef(model)
  if (term %in% names(coefs)) return(term)
  fit_term <- paste0("fit_", term)
  if (fit_term %in% names(coefs)) return(fit_term)
  hit <- grep(term, names(coefs), fixed = TRUE, value = TRUE)
  if (length(hit) == 1) return(hit)
  stop("Could not find coefficient for term: ", term, call. = FALSE)
}

manual_sargan <- function(iv_model_overid, reg_dt, instrument_rhs_vars, expected) {
  resid_struct <- resid(iv_model_overid)
  aux_dt <- copy(reg_dt)
  aux_dt[, resid_struct_ := resid_struct]
  aux_rhs <- paste(c(instrument_rhs_vars, expected, controls_base), collapse = " + ")
  aux_formula <- as.formula(paste0("resid_struct_ ~ ", aux_rhs))
  aux_model <- feols(aux_formula, data = aux_dt, notes = FALSE)
  fitted_aux <- fitted(aux_model)
  y_aux <- aux_dt$resid_struct_
  ss_res <- sum((y_aux - fitted_aux)^2, na.rm = TRUE)
  ss_tot <- sum((y_aux - mean(y_aux, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  n <- nrow(aux_dt)
  stat <- n * r2
  df <- length(instrument_rhs_vars) - 1L
  list(stat = stat, df = df, p_value = if (df > 0) stats::pchisq(stat, df = df, lower.tail = FALSE) else NA_real_)
}

results_list <- list()

for (i in seq_len(nrow(value_specs))) {
  spec <- value_specs$spec[i]
  y <- value_specs$outcome[i]
  require_exam_taker <- value_specs$require_exam_taker[i]
  log_ts("---- spec: ", spec, " (outcome: ", y, ") ----")

  d <- paste0("d_", spec)
  z <- paste0("z_", spec)
  expected <- paste0("expected_", spec)

  needed <- c(y, d, z, expected, "cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU", "student_id", "sae_proceso")
  if (require_exam_taker) needed <- c(needed, "admission_exam_taker")
  if (!all(needed %chin% names(estimation_df))) {
    log_ts("  Missing required columns for this spec -- skipping.")
    next
  }

  reg_dt <- estimation_df[complete.cases(estimation_df[, ..needed])]
  if (require_exam_taker) reg_dt <- reg_dt[admission_exam_taker == 1L]
  reg_dt <- merge(reg_dt, iv_wide, by = c("student_id", "sae_proceso"))
  n_obs <- nrow(reg_dt)
  log_ts("  n_obs (joined to instrument matrix) = ", n_obs)

  if (n_obs == 0 || uniqueN(reg_dt[[z]]) < 2 || uniqueN(reg_dt[[d]]) < 2) {
    log_ts("  Not enough variation to run this spec -- skipping.")
    next
  }

  instrument_ratio <- L / n_obs
  ratio_warning <- instrument_ratio > warn_instrument_ratio
  if (ratio_warning) {
    log_ts(
      "  WARNING: L/n_obs = ", round(instrument_ratio, 4), " exceeds ",
      warn_instrument_ratio, " -- many-instruments bias is a real risk here. ",
      "Treat the overidentification result for this spec with caution."
    )
  }

  # (a) Forecast-bias test: single scalar instrument, same formula as the
  # existing script, refit on these exact rows for a clean comparison.
  fb_formula <- as.formula(paste0(y, " ~ ", controls_rhs, " + ", expected, " | 0 | ", d, " ~ ", z))
  fb_model <- tryCatch(feols(fb_formula, data = reg_dt, vcov = "hetero", notes = FALSE), error = function(e) NULL)
  if (is.null(fb_model)) {
    log_ts("  Forecast-bias model failed to fit -- skipping spec.")
    next
  }
  fb_term <- extract_coef_name(fb_model, d)
  fb_beta <- coef(fb_model)[[fb_term]]
  fb_se <- se(fb_model)[[fb_term]]
  forecast_bias_stat <- ((fb_beta - 1) / fb_se)^2
  forecast_bias_p <- stats::pchisq(forecast_bias_stat, df = 1, lower.tail = FALSE)
  forecast_zero_stat <- (fb_beta / fb_se)^2
  forecast_zero_p <- stats::pchisq(forecast_zero_stat, df = 1, lower.tail = FALSE)

  # (b) Overidentified IV: full instrument matrix instead of the single
  # collapsed scalar.
  overid_formula <- as.formula(
    paste0(y, " ~ ", controls_rhs, " + ", expected, " | 0 | ", d, " ~ ", paste(offer_cols, collapse = " + "))
  )
  overid_model <- tryCatch(feols(overid_formula, data = reg_dt, vcov = "hetero", notes = FALSE), error = function(e) e)
  if (inherits(overid_model, "error")) {
    log_ts("  Overidentified IV model failed to fit: ", conditionMessage(overid_model))
    results_list[[spec]] <- data.table(
      spec = spec, outcome = y, n_obs = n_obs, L = L, instrument_ratio = instrument_ratio,
      many_instruments_warning = ratio_warning,
      forecast_beta = fb_beta, forecast_se = fb_se,
      forecast_bias_stat = forecast_bias_stat, forecast_bias_p = forecast_bias_p,
      forecast_zero_stat = forecast_zero_stat, forecast_zero_p = forecast_zero_p,
      overid_beta = NA_real_, overid_se = NA_real_,
      overid_stat = NA_real_, overid_df = NA_real_, overid_p = NA_real_,
      overid_method = "failed",
      omnibus_stat = NA_real_, omnibus_df = NA_real_, omnibus_p = NA_real_
    )
    next
  }

  overid_term <- extract_coef_name(overid_model, d)
  overid_beta <- coef(overid_model)[[overid_term]]
  overid_se <- se(overid_model)[[overid_term]]

  sargan_result <- tryCatch(
    c(manual_sargan(overid_model, reg_dt, offer_cols, expected), method = "manual_sargan"),
    error = function(e) list(stat = NA_real_, df = L - 1L, p_value = NA_real_, method = paste("failed:", conditionMessage(e)))
  )

  omnibus_stat <- forecast_bias_stat + sargan_result$stat
  omnibus_df <- 1L + sargan_result$df
  omnibus_p <- stats::pchisq(omnibus_stat, df = omnibus_df, lower.tail = FALSE)

  log_ts(
    "  forecast beta = ", round(fb_beta, 3), " (se ", round(fb_se, 3), "); ",
    "forecast-bias p = ", signif(forecast_bias_p, 3), "; ",
    "overid p = ", signif(sargan_result$p_value, 3), " (", sargan_result$method, "); ",
    "omnibus p = ", signif(omnibus_p, 3)
  )

  results_list[[spec]] <- data.table(
    spec = spec, outcome = y, n_obs = n_obs, L = L, instrument_ratio = instrument_ratio,
    many_instruments_warning = ratio_warning,
    forecast_beta = fb_beta, forecast_se = fb_se,
    forecast_bias_stat = forecast_bias_stat, forecast_bias_p = forecast_bias_p,
    forecast_zero_stat = forecast_zero_stat, forecast_zero_p = forecast_zero_p,
    overid_beta = overid_beta, overid_se = overid_se,
    overid_stat = sargan_result$stat, overid_df = sargan_result$df, overid_p = sargan_result$p_value,
    overid_method = sargan_result$method,
    omnibus_stat = omnibus_stat, omnibus_df = omnibus_df, omnibus_p = omnibus_p
  )
}

if (length(results_list) == 0) {
  stop("No spec produced a usable result -- nothing to write.", call. = FALSE)
}
results <- rbindlist(results_list, use.names = TRUE)
fwrite(results, results_path)
log_ts("Wrote: ", results_path)
print(results)
log_ts("Done.")
