###############################################################################
# RSS-style (Rose, Schellenberg & Shem-Tov 2025) debiased Var/Cov estimator
# for RC VAM school effects -- generalizes the math/STEM pairwise logic in
# code/codex/orthogonal_school_va/01_construct_orthogonal_school_va.R from
# 2 outcomes to the full RC VAM outcome list.
#
# DESIGN: "four_year_no_sae" -- cohorts 2017-2020 (4 cohorts), NO g(p_i)/SAE
#   risk controls. Reads the cached sample built by
#   00_build_four_year_no_sae_sample.R (run that first, and after any change
#   to its construction logic). 2017 is includable here specifically BECAUSE
#   no g(p_i) is needed (DA_probs_2017.csv doesn't exist -- see that
#   script's header comment).
#
#   This is the only design this script runs -- an earlier "three_year_sae_
#   gpi" alternative (cohorts 2018-2020, WITH g(p_i) risk controls, reading
#   rc_vam_stata_analysis_sample.dta) was dropped once the RC VAM g(p_i)
#   literal-dummy design turned out not to be estimable at Chile's scale
#   (see code/claude/rc_vam/). If that ever needs reviving, see version
#   control history for the RSS_VARCOV_SPEC-toggled branching this replaced.
#
# METHOD (EB-free cross-cohort covariance logic, matching
# orthogonal_school_va exactly, generalized to N outcomes):
#   1. For each outcome, fit feols(outcome ~ controls [+ gp_mat] | school_rbd
#      [+ most_time_RBD_middle]) -- ABSORBED fixed effects (not literal
#      dummies: RSS only needs residuals + the FE component per observation,
#      not an exact per-school SE, so absorption is memory-safe here, unlike
#      code/claude/rc_vam/01_construct_rc_vam_school_values_noreg.R which
#      needed literal dummies for the analytic SE).
#   2. Aggregate residual + FE component to a school x cohort mean
#      ("ybar_school_component"), per outcome.
#   3. Debiased own-variance per outcome, per school: signal2 = (sum_ybar^2 -
#      sum_ybar2) / (T*(T-1)) for schools with T (cohorts) >= 2 -- this is
#      the cross-cohort product trick that cancels same-cohort sampling
#      noise. Aggregate across schools using the same student-count weights as
#      the corresponding pooled VA regression. The weighted centering correction
#      also uses cross-cohort products, so sampling noise is removed from both
#      the second moment and the squared weighted mean.
#   4. Debiased cross-outcome covariance, per school pair of outcomes: same
#      idea but using cross-cohort, cross-outcome products (excluding same-
#      cohort pairs, which share sampling noise across the two outcomes).
#   5. Full N x N matrix assembled from all outcome pairs; correlation
#      matrix = Cov / sqrt(Var_i * Var_j) computed on each pair's own common
#      school set (matching the original script's paper_corr_stem_math).
#   6. School-level bootstrap SEs (resample the school-level signal table
#      with replacement, B replicates, no refitting of regressions needed --
#      the signals are already extracted per school). Var and Cov each get
#      their OWN independent resample (bootstrap_diag_se/bootstrap_cov_se).
#      The implied-gains betas (step 7 below and rss_implied_gains.csv) and
#      the correlation in rss_corr_matrix.csv instead get a separate
#      JOINT/paired resample (bootstrap_gains_se): since beta = Cov/Var and
#      rho = Cov/sqrt(Var1*Var2) are ratios of quantities estimated from
#      the same schools, their SEs need Var1, Var2, and Cov recomputed on
#      the SAME resampled draw each replicate, not on independent draws.
#   7. Orthogonality summary: for each longer-run outcome, orthogonalize its
#      pooled VA against math (z_year_math_max) using beta = Cov/Var(math),
#      report naive correlation before/after -- generalizes the single
#      stem-on-math example in orthogonal_school_va to every longer-run
#      outcome in the RC VAM list.
#
# OUTPUTS, in data/clean/rss_debiased_varcov/four_year_no_sae/:
#   rss_varcov_matrix.csv        -- long-format Var/Cov entries
#   rss_corr_matrix.csv          -- long-format correlation entries, plus a
#                                    joint/paired bootstrap_se column (see
#                                    step 6 above; NA on the diagonal, where
#                                    correlation is trivially 1)
#   rss_implied_gains.csv        -- long-format, BOTH directions per pair:
#                                    beta = Cov(from,to)/Var(from), i.e. the
#                                    implied gain in outcome_to's true VA per
#                                    1-unit increase in outcome_from's true
#                                    VA (generalizes beta_on_anchor below to
#                                    every ordered pair, not just vs. math),
#                                    plus a joint/paired bootstrap_se column
#                                    (see step 6 above)
#   rss_orthogonality_summary.csv-- corr before/after vs. math anchor
#   rss_school_cohort_means.csv  -- underlying school x cohort x outcome
#                                    residual means (for audit)
#   rss_diagnostics.csv          -- sample sizes, schools used, etc.
#   component_cache/<outcome>.rds-- per-outcome fitted-model cache (see below)
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(fixest)
})

options(expressions = 500000)

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

lowercase_dedupe_names <- function(dt, label = deparse(substitute(dt))) {
  new_names <- tolower(names(dt))
  dupes <- duplicated(new_names)
  if (any(dupes)) {
    warning(
      label, ": dropping ", sum(dupes), " duplicate column(s) after lowercasing."
    )
    dt <- dt[, !dupes, with = FALSE]
    new_names <- new_names[!dupes]
  }
  setnames(dt, new_names)
  dt
}

# ------------------------- CONFIG -------------------------------------------
# Only design this script runs -- see header comment. Kept as a named
# constant (rather than inlined everywhere) so the output-folder path and
# diagnostics column below stay self-documenting without needing an env var.
spec <- "four_year_no_sae"

outcomes <- c(
  "z_year_math_max", "z_year_leng_max",
  "high_inst_m1", "high_paying_field_m1", "log_program_income_clp_m1",
  "admission_exam_taker", "any_postulacion"
)
academic_outcomes <- c("z_year_math_max", "z_year_leng_max")
math_anchor <- "z_year_math_max"
n_boot <- as.integer(Sys.getenv("RSS_VARCOV_N_BOOT", unset = "500"))
boot_seed <- as.integer(Sys.getenv("RSS_VARCOV_BOOT_SEED", unset = "20260921"))

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

# data/clean/rss_debiased_varcov/four_year_no_sae/ -- fixed location, no
# per-spec branching needed since this is the only design.
output_dir <- file.path(clean_dir, "rss_debiased_varcov", spec)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
out <- function(name) file.path(output_dir, paste0(name, ".csv"))

# Per-outcome regression cache -- the expensive step here is fitting each
# outcome's feols(). The pairwise covariance arithmetic afterward is cheap
# (just operates on already-extracted school-cohort signal tables), so only
# the per-outcome fit needs checkpointing. Set RSS_VARCOV_FORCE=1 to ignore
# the cache and refit everything (e.g. after a real code change to
# estimate_outcome_components).
cache_dir <- file.path(output_dir, "component_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
cache_path <- function(y) file.path(cache_dir, paste0(y, ".rds"))
force_rerun <- Sys.getenv("RSS_VARCOV_FORCE", unset = "0") %in% c("1", "true", "TRUE")

# ------------------------- DATA LOADING -------------------------------------
# Reads the cached sample built by 00_build_four_year_no_sae_sample.R
# (cohorts 2017-2020, no g(p_i)/SAE join) instead of rebuilding from
# univ_gr8_df.csv on every run. Run 00_build_four_year_no_sae_sample.R first
# (and after any change to its construction logic) to (re)produce this file.
cached_sample_path <- file.path(clean_dir, "rss_debiased_varcov", "four_year_no_sae_sample.dta")
if (!file.exists(cached_sample_path)) {
  stop(
    "Could not find ", cached_sample_path, ". Run ",
    "code/claude/rss_debiased_varcov/00_build_four_year_no_sae_sample.R first.",
    call. = FALSE
  )
}
log_ts("Loading cached four_year_no_sae sample: ", cached_sample_path)
analytic <- as.data.table(read_dta(cached_sample_path))
log_ts("Loaded: ", nrow(analytic), " rows x ", ncol(analytic), " cols.")

x_terms <- c(
  "factor(cohort_gr8)", "factor(gen_alu)", "factor(edad_alu)", "factor(cod_com_alu)",
  "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "z_gpa_middle_mean", "z_att_middle_mean", "factor(middle_years_observed)",
  "z_sim_mat_4to", "z_sim_mat_4to_2", "z_sim_mat_4to_3",
  "z_sim_leng_4to", "z_sim_leng_4to_2", "z_sim_leng_4to_3"
)
x_vars_needed <- c(
  "cohort_gr8", "gen_alu", "edad_alu", "cod_com_alu", "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean",
  "z_sim_mat_4to", "z_sim_leng_4to"
)
x_terms_vars <- c(x_vars_needed, "z_sim_mat_4to_2", "z_sim_mat_4to_3",
                   "z_sim_leng_4to_2", "z_sim_leng_4to_3")
fixed_effect_vars <- c("school_rbd", "most_time_rbd_middle")
gp_candidates <- character()  # no g(p_i) in this design
zero_variance_tol <- 1e-8
admission_exam_col <- "admission_exam_taker"
cohort_col <- "cohort_gr8"

setFixest_nthreads(0)

# ------------------------- PER-OUTCOME REGRESSION + RESIDUALS --------------
estimate_outcome_components <- function(data, y, gp_candidates, x_terms, x_terms_vars,
                                         fixed_effect_vars, admission_exam_col, cohort_col,
                                         zero_variance_tol) {
  d <- copy(data)
  if (!(y %in% c("admission_exam_taker", "higher_ed_enrolled_m1", "any_postulacion", "any_asignacion", "grat_asignacion"))) {
    d <- d[get(admission_exam_col) == 1]
  }

  keep_cols <- unique(c(fixed_effect_vars, cohort_col, y, x_terms_vars, gp_candidates))
  keep_cols <- intersect(keep_cols, names(d))
  d <- d[, ..keep_cols]
  d <- d[!is.na(get(fixed_effect_vars[1])) & !is.na(get(y))]
  d <- d[complete.cases(d[, intersect(x_vars_needed, names(d)), with = FALSE])]

  if (nrow(d) == 0 || uniqueN(d[[fixed_effect_vars[1]]]) < 2) {
    warning("No usable regression data for outcome: ", y, call. = FALSE)
    return(NULL)
  }

  # Rescan g(p_i) usable-variance columns HERE, on this outcome's own filtered
  # subsample -- a column can have variance in the full sample but collapse
  # to zero within, e.g., the exam-takers-only subsample, in which case it
  # must be dropped for THIS outcome specifically (matches
  # 01_construct_rc_vam_school_values_noreg.R's per-outcome rescan exactly).
  gp_used <- character()
  if (length(gp_candidates) > 0) {
    for (v in gp_candidates) {
      sd_v <- sd(d[[v]], na.rm = TRUE)
      if (!is.na(sd_v) && sd_v > zero_variance_tol) gp_used <- c(gp_used, v)
    }
  }
  log_ts("  g(p_i) terms with usable variance for ", y, ": ", length(gp_used),
         " (of ", length(gp_candidates), " candidates)")

  rhs_terms <- x_terms
  if (length(gp_used) > 0) {
    gp_mat <- as.matrix(d[, ..gp_used])
    d$gp_mat <- gp_mat
    rm(gp_mat)
    rhs_terms <- c(x_terms, "gp_mat")
  }

  model_formula <- as.formula(
    paste0(y, " ~ ", paste(rhs_terms, collapse = " + "), " | ", paste(fixed_effect_vars, collapse = " + "))
  )
  log_ts("  Fitting feols for ", y, " (", nrow(d), " obs, ", uniqueN(d[[fixed_effect_vars[1]]]), " schools)...")
  model <- feols(model_formula, data = d, notes = FALSE)
  school_fe <- fixef(model)[[fixed_effect_vars[1]]]

  d[, school_fe_component := as.numeric(school_fe[as.character(get(fixed_effect_vars[1]))])]
  d[, school_component_residual := stats::residuals(model) + school_fe_component]

  school_cohort_means <- d[
    ,
    .(ybar = mean(school_component_residual, na.rm = TRUE), n_students = .N),
    by = c(fixed_effect_vars[1], cohort_col)
  ]
  setnames(school_cohort_means, c(fixed_effect_vars[1], cohort_col), c("school_rbd", "cohort_gr8"))
  school_cohort_means[, outcome := y]

  pooled_va <- data.table(
    school_rbd = as.numeric(names(school_fe)),
    outcome = y,
    controlled_value_added = as.numeric(school_fe)
  )

  list(
    school_cohort_means = school_cohort_means,
    pooled_va = pooled_va,
    n_obs = nobs(model),
    n_schools = uniqueN(d[[fixed_effect_vars[1]]]),
    n_gp_terms_used = length(gp_used)
  )
}

components <- list()
for (y in outcomes) {
  log_ts("---- Outcome: ", y, " ----")

  if (file.exists(cache_path(y)) && !force_rerun) {
    log_ts("  Found cached fit: ", cache_path(y), " -- loading instead of refitting.")
    comp <- readRDS(cache_path(y))
    components[[y]] <- comp
    next
  }

  comp <- tryCatch(
    estimate_outcome_components(analytic, y, gp_candidates, x_terms, x_terms_vars,
                                 fixed_effect_vars, admission_exam_col, cohort_col,
                                 zero_variance_tol),
    error = function(e) {
      warning("Skipping ", y, ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (!is.null(comp)) {
    components[[y]] <- comp
    saveRDS(comp, cache_path(y))
    log_ts("  Cached fit saved: ", cache_path(y))
  }
}
fitted_outcomes <- names(components)
log_ts("Fitted outcomes: ", paste(fitted_outcomes, collapse = ", "))

# Exact school weights for pairwise covariances: number of students in the
# intersection of the two outcome-specific pooled VA regression samples.
pair_student_counts <- list()
if (length(fitted_outcomes) >= 2) {
  weight_vars <- unique(c(fixed_effect_vars, x_vars_needed, admission_exam_col, fitted_outcomes))
  weight_data <- analytic[, ..weight_vars]
  weight_data <- weight_data[complete.cases(weight_data[, c(fixed_effect_vars, x_vars_needed), with = FALSE])]
  for (p in combn(fitted_outcomes, 2, simplify = FALSE)) {
    y1 <- p[[1]]; y2 <- p[[2]]
    d_pair <- weight_data[!is.na(get(y1)) & !is.na(get(y2))]
    unrestricted <- c(
      "admission_exam_taker", "higher_ed_enrolled_m1", "any_postulacion",
      "any_asignacion", "grat_asignacion"
    )
    if (any(!c(y1, y2) %in% unrestricted)) d_pair <- d_pair[get(admission_exam_col) == 1]
    pair_student_counts[[paste(y1, y2, sep = "__")]] <-
      d_pair[, .(n_students_pair = .N), by = school_rbd]
  }
  rm(weight_data)
}
rm(analytic)
gc(full = TRUE)

# ------------------------- SINGLE-OUTCOME DEBIASED SIGNAL TABLES -----------
make_single_outcome_stats <- function(school_cohort_means) {
  out <- school_cohort_means[
    ,
    .(
      T = .N,
      n_students = sum(n_students),
      mean_ybar = mean(ybar, na.rm = TRUE),
      sum_ybar = sum(ybar, na.rm = TRUE),
      sum_ybar2 = sum(ybar^2, na.rm = TRUE)
    ),
    by = school_rbd
  ]
  out[, signal2 := fifelse(T >= 2, (sum_ybar^2 - sum_ybar2) / (T * (T - 1)), NA_real_)]
  out
}

signal_tables <- lapply(components, function(comp) make_single_outcome_stats(comp$school_cohort_means))
names(signal_tables) <- fitted_outcomes

normalize_weights <- function(weight) {
  if (any(!is.finite(weight)) || any(weight <= 0) || sum(weight) <= 0) {
    stop("RSS aggregation weights must be finite and strictly positive.", call. = FALSE)
  }
  weight / sum(weight)
}

variance_from_signals <- function(signal2, mean_ybar, weight) {
  w <- normalize_weights(weight)
  sum(w * signal2) - sum(w * mean_ybar)^2 +
    sum(w^2 * (mean_ybar^2 - signal2))
}
covariance_from_signals <- function(signal_xy, mean_x, mean_y, weight) {
  w <- normalize_weights(weight)
  sum(w * signal_xy) - sum(w * mean_x) * sum(w * mean_y) +
    sum(w^2 * (mean_x * mean_y - signal_xy))
}

# ------------------------- DIAGONAL (OWN VARIANCE) --------------------------
diag_results <- list()
for (y in fitted_outcomes) {
  st <- signal_tables[[y]][T >= 2 & is.finite(signal2) & is.finite(mean_ybar) &
                              is.finite(n_students) & n_students > 0]
  J <- nrow(st)
  var_y <- if (J >= 2) variance_from_signals(st$signal2, st$mean_ybar, st$n_students) else NA_real_
  diag_results[[y]] <- list(J = J, var = var_y, schools = st$school_rbd)
}

# ------------------------- OFF-DIAGONAL (COVARIANCE) ------------------------
pair_results <- list()
if (length(fitted_outcomes) >= 2) {
  pairs <- combn(fitted_outcomes, 2, simplify = FALSE)
  for (p in pairs) {
    y1 <- p[1]; y2 <- p[2]
    st1 <- signal_tables[[y1]]
    st2 <- signal_tables[[y2]]

    same_cohort <- merge(
      components[[y1]]$school_cohort_means[, .(school_rbd, cohort_gr8, ybar1 = ybar)],
      components[[y2]]$school_cohort_means[, .(school_rbd, cohort_gr8, ybar2 = ybar)],
      by = c("school_rbd", "cohort_gr8"), sort = FALSE
    )
    same_stats <- same_cohort[, .(same_product_sum = sum(ybar1 * ybar2, na.rm = TRUE), n_same = .N), by = school_rbd]

    stats_dt <- merge(st1[, .(school_rbd, T1 = T, mean1 = mean_ybar, sum1 = sum_ybar,
                              signal2_1 = signal2, n_students_1 = n_students)],
                       st2[, .(school_rbd, T2 = T, mean2 = mean_ybar, sum2 = sum_ybar,
                              signal2_2 = signal2, n_students_2 = n_students)],
                       by = "school_rbd")
    stats_dt <- merge(stats_dt, same_stats, by = "school_rbd", all.x = TRUE)
    stats_dt[is.na(same_product_sum), same_product_sum := 0]
    stats_dt[is.na(n_same), n_same := 0L]
    stats_dt[, n_cross := T1 * T2 - n_same]
    stats_dt[, cross_signal := fifelse(n_cross > 0, (sum1 * sum2 - same_product_sum) / n_cross, NA_real_)]
    stats_dt <- merge(
      stats_dt,
      pair_student_counts[[paste(y1, y2, sep = "__")]],
      by = "school_rbd", all.x = TRUE
    )

    usable <- stats_dt[T1 >= 2 & T2 >= 2 & is.finite(signal2_1) & is.finite(signal2_2) &
                         is.finite(mean1) & is.finite(mean2) & is.finite(cross_signal) &
                         is.finite(n_students_pair) & n_students_pair > 0]
    J <- nrow(usable)
    if (J >= 2) {
      var1_J <- variance_from_signals(usable$signal2_1, usable$mean1, usable$n_students_pair)
      var2_J <- variance_from_signals(usable$signal2_2, usable$mean2, usable$n_students_pair)
      cov_J <- covariance_from_signals(usable$cross_signal, usable$mean1, usable$mean2, usable$n_students_pair)
      corr_J <- if (is.finite(var1_J) && is.finite(var2_J) && var1_J > 0 && var2_J > 0) {
        cov_J / sqrt(var1_J * var2_J)
      } else NA_real_
    } else {
      var1_J <- NA_real_; var2_J <- NA_real_; cov_J <- NA_real_; corr_J <- NA_real_
    }
    pair_results[[paste(y1, y2, sep = "__")]] <- list(
      y1 = y1, y2 = y2, J = J, cov = cov_J, corr = corr_J,
      var1_J = var1_J, var2_J = var2_J, usable = usable
    )
  }
}

# ------------------------- BOOTSTRAP SEs (school-level resample) -----------
set.seed(boot_seed)
bootstrap_diag_se <- function(st, B) {
  st <- st[T >= 2 & is.finite(signal2) & is.finite(mean_ybar) &
             is.finite(n_students) & n_students > 0]
  J <- nrow(st)
  if (J < 2) return(NA_real_)
  reps <- replicate(B, {
    idx <- sample.int(J, J, replace = TRUE)
    variance_from_signals(st$signal2[idx], st$mean_ybar[idx], st$n_students[idx])
  })
  stats::sd(reps, na.rm = TRUE)
}
bootstrap_cov_se <- function(usable, B) {
  J <- nrow(usable)
  if (J < 2) return(NA_real_)
  reps <- replicate(B, {
    idx <- sample.int(J, J, replace = TRUE)
    covariance_from_signals(usable$cross_signal[idx], usable$mean1[idx], usable$mean2[idx],
                            usable$n_students_pair[idx])
  })
  stats::sd(reps, na.rm = TRUE)
}

# Joint (paired) bootstrap for the implied-gains betas AND the correlation --
# beta_{2|1} = Cov/Var1, beta_{1|2} = Cov/Var2, and rho = Cov/sqrt(Var1*Var2)
# are all RATIOS built from Var1, Var2, and Cov, quantities estimated from
# the same schools, so their sampling errors are correlated with each
# other. bootstrap_diag_se() and bootstrap_cov_se() above draw INDEPENDENT
# resamples for Var and Cov, which is fine for each of those on its own but
# cannot be combined into a valid SE for a ratio of the two (or three).
# This function instead draws ONE resampled set of schools per replicate
# and recomputes Var1, Var2, AND Cov together on that same draw, then forms
# both beta ratios AND the correlation from it -- so the SD across
# replicates correctly reflects the joint sampling variability of
# numerator and denominator for all three objects. This is still a
# nonparametric resample-schools bootstrap, not the paper's analytic
# formula (see the script header), but unlike a naive combination of the
# separate diag/cov bootstraps, it is at least internally valid for each
# ratio itself.
bootstrap_gains_se <- function(usable, B) {
  J <- nrow(usable)
  if (J < 2) return(list(se_2_on_1 = NA_real_, se_1_on_2 = NA_real_, se_corr = NA_real_))
  reps <- replicate(B, {
    idx <- sample.int(J, J, replace = TRUE)
    v1 <- variance_from_signals(usable$signal2_1[idx], usable$mean1[idx], usable$n_students_pair[idx])
    v2 <- variance_from_signals(usable$signal2_2[idx], usable$mean2[idx], usable$n_students_pair[idx])
    cv <- covariance_from_signals(usable$cross_signal[idx], usable$mean1[idx], usable$mean2[idx],
                                  usable$n_students_pair[idx])
    c(
      beta_2_on_1 = if (is.finite(v1) && v1 > 0) cv / v1 else NA_real_,
      beta_1_on_2 = if (is.finite(v2) && v2 > 0) cv / v2 else NA_real_,
      corr = if (is.finite(v1) && is.finite(v2) && v1 > 0 && v2 > 0) cv / sqrt(v1 * v2) else NA_real_
    )
  })
  list(
    se_2_on_1 = stats::sd(reps["beta_2_on_1", ], na.rm = TRUE),
    se_1_on_2 = stats::sd(reps["beta_1_on_2", ], na.rm = TRUE),
    se_corr = stats::sd(reps["corr", ], na.rm = TRUE)
  )
}

log_ts("Bootstrapping SEs (B = ", n_boot, ")...")
diag_se <- sapply(fitted_outcomes, function(y) {
  bootstrap_diag_se(signal_tables[[y]][T >= 2 & !is.na(signal2)], n_boot)
})
cov_se <- sapply(pair_results, function(pr) bootstrap_cov_se(pr$usable, n_boot))
gains_se_list <- lapply(pair_results, function(pr) bootstrap_gains_se(pr$usable, n_boot))

# ------------------------- ASSEMBLE OUTPUT TABLES ---------------------------
varcov_rows <- rbindlist(c(
  lapply(fitted_outcomes, function(y) {
    data.table(outcome_1 = y, outcome_2 = y, type = "variance",
               J_schools = diag_results[[y]]$J, estimate = diag_results[[y]]$var,
               bootstrap_se = diag_se[[y]])
  }),
  lapply(names(pair_results), function(k) {
    pr <- pair_results[[k]]
    data.table(outcome_1 = pr$y1, outcome_2 = pr$y2, type = "covariance",
               J_schools = pr$J, estimate = pr$cov, bootstrap_se = cov_se[[k]])
  })
))

corr_rows <- rbindlist(c(
  lapply(fitted_outcomes, function(y) data.table(outcome_1 = y, outcome_2 = y, correlation = 1, bootstrap_se = NA_real_)),
  lapply(names(pair_results), function(k) {
    pr <- pair_results[[k]]
    data.table(outcome_1 = pr$y1, outcome_2 = pr$y2, correlation = pr$corr,
               bootstrap_se = gains_se_list[[k]]$se_corr)
  })
))

# ------------------------- IMPLIED GAINS (pairwise, both directions) -------
# "Implied gain in outcome_to's true VA from a 1-unit increase in outcome_
# from's true VA" = Cov(from, to) / Var(from). Uses the SAME pair-specific,
# common-school-set variance (var1_J / var2_J from pair_results) as the
# denominator, not the marginal single-outcome variance in diag_results --
# this exactly matches the beta_on_anchor logic already used in the
# orthogonality summary below (which is the math-anchored special case of
# this same quantity), so the numbers are internally consistent with each
# other and with rss_corr_matrix.csv's own correlation denominators.
implied_gains_rows <- rbindlist(lapply(names(pair_results), function(k) {
  pr <- pair_results[[k]]
  if (!is.finite(pr$var1_J) || !is.finite(pr$var2_J) || pr$var1_J <= 0 || pr$var2_J <= 0 || !is.finite(pr$cov)) {
    return(data.table())
  }
  gse <- gains_se_list[[k]]
  rbindlist(list(
    data.table(outcome_from = pr$y1, outcome_to = pr$y2,
               beta = pr$cov / pr$var1_J, bootstrap_se = gse$se_2_on_1, J_schools = pr$J),
    data.table(outcome_from = pr$y2, outcome_to = pr$y1,
               beta = pr$cov / pr$var2_J, bootstrap_se = gse$se_1_on_2, J_schools = pr$J)
  ))
}))

school_cohort_means_long <- rbindlist(lapply(components, function(comp) comp$school_cohort_means))

# ------------------------- ORTHOGONALITY SUMMARY (vs. math anchor) --------
orth_rows <- list()
if (math_anchor %in% fitted_outcomes) {
  pooled_math <- components[[math_anchor]]$pooled_va
  pooled_math_mean <- mean(pooled_math$controlled_value_added, na.rm = TRUE)
  pooled_math[, va_centered := controlled_value_added - pooled_math_mean]

  longer_run <- setdiff(fitted_outcomes, academic_outcomes)
  for (y in longer_run) {
    key <- if (paste(math_anchor, y, sep = "__") %in% names(pair_results)) {
      paste(math_anchor, y, sep = "__")
    } else {
      paste(y, math_anchor, sep = "__")
    }
    if (!key %in% names(pair_results)) next
    pr <- pair_results[[key]]
    var_math <- if (pr$y1 == math_anchor) pr$var1_J else pr$var2_J
    beta_y_on_math <- if (is.finite(var_math) && var_math > 0) pr$cov / var_math else NA_real_

    pooled_y <- components[[y]]$pooled_va
    pooled_y_mean <- mean(pooled_y$controlled_value_added, na.rm = TRUE)
    pooled_y[, va_centered := controlled_value_added - pooled_y_mean]

    merged <- merge(pooled_math[, .(school_rbd, math_va = va_centered)],
                     pooled_y[, .(school_rbd, y_va = va_centered)],
                     by = "school_rbd")
    merged[, y_va_orth := y_va - beta_y_on_math * math_va]

    corr_before <- suppressWarnings(stats::cor(merged$math_va, merged$y_va, use = "complete.obs"))
    corr_after <- suppressWarnings(stats::cor(merged$math_va, merged$y_va_orth, use = "complete.obs"))

    orth_rows[[y]] <- data.table(
      outcome = y, anchor = math_anchor, beta_on_anchor = beta_y_on_math,
      J_schools_used_for_beta = pr$J,
      corr_before = corr_before, corr_after = corr_after,
      n_schools_merged = nrow(merged)
    )
  }
}
orth_summary <- if (length(orth_rows) > 0) rbindlist(orth_rows) else data.table()

diagnostics <- data.table(
  spec = spec,
  aggregation_weight = "pooled_va_regression_student_count",
  pair_weight = "intersection_of_pair_regression_samples",
  n_outcomes_requested = length(outcomes),
  n_outcomes_fitted = length(fitted_outcomes),
  outcomes_fitted = paste(fitted_outcomes, collapse = ";"),
  outcomes_skipped = paste(setdiff(outcomes, fitted_outcomes), collapse = ";"),
  n_gp_candidates = length(gp_candidates),
  n_boot = n_boot
)

# Per-outcome g(p_i) usage detail (candidates vs. actually-used-per-outcome
# can differ -- see the per-outcome rescan in estimate_outcome_components()).
gp_usage_by_outcome <- rbindlist(lapply(fitted_outcomes, function(y) {
  data.table(outcome = y, n_gp_terms_used = components[[y]]$n_gp_terms_used)
}))
diagnostics <- cbind(diagnostics, data.table(
  gp_usage_by_outcome = paste(
    gp_usage_by_outcome$outcome, gp_usage_by_outcome$n_gp_terms_used,
    sep = "=", collapse = ";"
  )
))

# ------------------------- WRITE --------------------------------------------
log_ts("Writing outputs to: ", output_dir)
fwrite(varcov_rows, out("rss_varcov_matrix"))
fwrite(corr_rows, out("rss_corr_matrix"))
fwrite(implied_gains_rows, out("rss_implied_gains"))
fwrite(school_cohort_means_long, out("rss_school_cohort_means"))
fwrite(orth_summary, out("rss_orthogonality_summary"))
fwrite(diagnostics, out("rss_diagnostics"))

log_ts("Done. Spec = ", spec, ". Fitted ", length(fitted_outcomes), "/", length(outcomes), " outcomes.")
