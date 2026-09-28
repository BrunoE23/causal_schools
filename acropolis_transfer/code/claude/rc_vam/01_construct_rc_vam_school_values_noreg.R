###############################################################################
# Risk-Controlled Value-Added (RC VAM) -- R port -- NO REGULARIZATION
# (fast sibling of 01_construct_rc_vam_school_values.do / _lasso.do)
#
# WHY THIS EXISTS: the Stata noreg script (ibn.school_rbd + regress) builds
# ~3,000+ literal school dummy columns in ONE regression alongside ~2,656
# g(p_i) terms + ~50 X controls -- a ~5,800-regressor OLS that stalls in
# Stata (regress inverts the full X'X, which scales roughly cubically in
# regressor count).
#
# REVISED 2026-09-21 (after a real run hit "node stack overflow" /
# "evaluation nested too deeply"): the first version of this script tried
# fixest::feols() with school_rbd ABSORBED (fast demeaning) plus
# lfe::getfe(bN=...) bootstrapping for the per-school SE. That absorption
# approach itself worked, but R's formula machinery choked building a
# formula with ~2,700 additive terms pasted together as text -- R's parser/
# evaluator represents "a + b + c + ..." as a deeply right-nested call tree,
# and a few thousand terms overflows the interpreter's node stack regardless
# of package (this would have hit lfe::felm() the same way for the SE step).
#
# FIX: fixest::xpd() lets you write a SHORT formula with a placeholder
# (e.g. `y ~ 0 + factor(school_rbd) + ..ctrl`) and substitute a character
# vector of variable names for `..ctrl` separately -- fixest expands that
# internally without going through R's normal formula-parsing path, so it
# never builds the problematic nested "+"  tree. This also let us drop the
# absorption + bootstrap design entirely: with xpd avoiding the stack
# overflow, there's no longer any obstacle to including school_rbd as a
# literal, non-absorbed `factor(school_rbd)` term (no reference category,
# via `0 +`) alongside everything else in ONE feols fit -- which is exactly
# the original Stata `ibn.school_rbd + regress` design, just fit by fixest
# instead of Stata. That means school-level point estimates AND their
# standard errors both come straight off fixest's own coefficient table
# (Estimate / Std. Error columns) -- an EXACT analytic SE, not a bootstrap
# approximation. This is a strictly better outcome for the EB shrinkage step
# downstream (code/claude/rc_vam/02_construct_eb_rc_vam_values.R), which uses
# each school's SE directly to estimate signal variance (tau2) and shrinkage
# weight (reliability) -- an approximate SE would have biased every school's
# shrinkage amount, not just its own reported uncertainty.
#
# No more lfe/felm/getfe/bootstrap anywhere in this script.
#
# Ridge/lasso still need the literal-dummy trick in Stata (a ridge pseudo-
# observation must represent "no school", which doesn't map onto a factor
# level) -- this script is deliberately ONLY the no-regularization sibling.
#
# INPUT: rc_vam_stata_analysis_sample.dta, built by
#   code/claude/rc_vam/00_build_rc_vam_analysis_sample.R
# (same cached file the Stata ridge/lasso/noreg scripts use).
#
# REQUIRES: install.packages(c("fixest", "data.table", "dplyr", "haven"))
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(haven)
  library(fixest)
})

# Cheap extra safety net on top of the xpd() fix above -- raises R's own
# nested-expression ceiling (default 5000) in case anything else in this
# script ever builds a moderately deep call tree. Not load-bearing for the
# xpd() path itself, which is designed to avoid needing this at all.
options(expressions = 500000)

# Timestamped, force-flushed logging. Plain message() writes to R's stderr
# connection, but when stderr is redirected to a FILE (e.g. `> run_log.txt
# 2>&1` under nohup, as opposed to a live terminal), stdio commonly switches
# from line-buffered to fully-buffered -- output can sit in a buffer and
# never reach the file until the buffer fills or the process exits, making a
# perfectly healthy long-running job look "stuck" with an empty/stale log.
# Wrapping every message in an explicit flush() forces it out immediately
# regardless of that buffering behavior, and the timestamp lets you see
# exactly how long each step took from the log alone.
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

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
cached_sample_path <- file.path(clean_dir, "rc_vam_stata_analysis_sample.dta")
output_dir <- file.path(clean_dir, "rc_vam_school_values")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Per-outcome output files, in their own subfolder -- EACH outcome gets its
# own CSV (rc_vam_school_values_r_noreg_<outcome>.csv), rather than every
# outcome sharing one combined CSV. Previously, every outcome's rows lived
# in one shared file: each save read the whole thing back, dropped this
# outcome's old rows, appended the new ones, and rewrote it. That merge
# step is exactly what caused real confusion -- e.g. downloading that one
# shared file after a rerun and not being able to tell at a glance whether
# a given outcome's rows were actually fresh or stale/left over from a
# previous run, since the file's own mtime updates on every single write
# regardless of which outcome changed. With one file per outcome, each
# file's own mtime and content answer that unambiguously, and a save is a
# plain overwrite of that outcome's own file, never a read-merge-rewrite
# of everyone else's rows too.
#
# 01b_combine_rc_vam_noreg_by_outcome.R reads every file in this folder and
# assembles the combined results/diagnostics CSVs that
# 02_construct_eb_rc_vam_values.R actually consumes -- run that whenever you
# want an updated combined file; it's cheap and safe to rerun anytime.
by_outcome_dir <- file.path(output_dir, "by_outcome")
dir.create(by_outcome_dir, recursive = TRUE, showWarnings = FALSE)
outcome_output_path <- function(y) file.path(by_outcome_dir, paste0("rc_vam_school_values_r_noreg_", y, ".csv"))

zero_variance_tol <- 1e-8

if (!file.exists(cached_sample_path)) {
  stop(
    "Could not find ", cached_sample_path, ". Run ",
    "code/claude/rc_vam/00_build_rc_vam_analysis_sample.R first.",
    call. = FALSE
  )
}
log_ts("Script started.")
file_info <- file.info(cached_sample_path)
log_ts(
  "Loading cached RC VAM estimation sample (", round(file_info$size / 1e9, 2),
  " GB): ", cached_sample_path
)
log_ts("This read_dta() call can genuinely take several minutes for a file this size -- next message fires when it's done.")
.t0_read <- Sys.time()
analytic <- as.data.table(read_dta(cached_sample_path))
log_ts(
  "Finished loading: ", nrow(analytic), " rows x ", ncol(analytic), " cols. (",
  round(as.numeric(difftime(Sys.time(), .t0_read, units = "secs")), 1), " sec)"
)

prob_vars <- grep("^prob_", names(analytic), value = TRUE)
iszero_vars <- grep("^iszero_", names(analytic), value = TRUE)
log_ts(
  "g(p_i) control columns: ", length(prob_vars), " prob_* + ",
  length(iszero_vars), " iszero_*"
)

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

# Actual columns referenced by x_terms (strips the factor() wrapper, and
# adds the squared/cubed SIMCE terms that x_vars_needed deliberately leaves
# out -- x_vars_needed is only the set checked for missingness, not every
# column x_terms uses). Needed so `d`/`df` subsetting below doesn't drop
# columns the model formula actually references.
x_terms_vars <- c(x_vars_needed, "z_sim_mat_4to_2", "z_sim_mat_4to_3",
                   "z_sim_leng_4to_2", "z_sim_leng_4to_3")

all_outcomes <- c(
  "z_year_math_max", "z_year_leng_max", "admission_exam_taker",
  "high_inst_m1", "high_paying_field_m1", "log_program_income_clp_m1",
  "any_postulacion", "any_asignacion"
)

# RAM, not runtime, is the actual constraint on this machine (two different
# outcomes have now each hit "cannot allocate vector" at a different size,
# simply because each outcome's own sample size / school count differs --
# this isn't something a code tweak fixes away, it's the data being too big
# for available RAM once g(p_i) is this wide). R also doesn't reliably
# return all freed memory to the OS mid-session even with gc(), so the most
# reliable way to run this is one outcome (or a small batch) per fresh
# `Rscript` invocation (guarantees a clean memory slate every time), not one
# big loop over all 8.
#
# THREE ways to pick which outcomes run this invocation (checked in this
# order -- first one that's actually set wins):
#
# 1. MANUAL LIST BELOW -- comment/uncomment individual lines directly. Edit
#    this and rerun; nothing else needed. Leave all commented to fall
#    through to option 2 or 3.

outcomes_manual <- c(
  "z_year_math_max",
  "z_year_leng_max",
  "admission_exam_taker",
  "high_inst_m1",
  "high_paying_field_m1",
  "log_program_income_clp_m1",
  "any_postulacion",
  "any_asignacion"
)
#
# 2. BATCHES -- all_outcomes split into groups of 4 in the order listed
#    above (batch 1 = first 4, batch 2 = next 4, etc.; with 8 outcomes
#    that's 2 batches today, but this scales if outcomes are ever added).
#    Run one batch via:
#    RC_VAM_NOREG_BATCH=1   Rscript 01_construct_rc_vam_school_values_noreg.R
#
# 3. RC_VAM_NOREG_OUTCOMES -- comma-separated list, e.g.:
#      RC_VAM_NOREG_OUTCOMES=higher_ed_enrolled_m1 Rscript 01_construct_rc_vam_school_values_noreg.R
#
# Leaving all three unset/empty runs every outcome in one pass (fine for the
# smaller-sample ones; use one of the three above for the bigger ones).
# Results are always merged into the existing output CSV by outcome (see the
# writing step below), so partial runs from different invocations accumulate
# instead of overwriting each other.
batches <- split(all_outcomes, ceiling(seq_along(all_outcomes) / 4))
requested_batch <- Sys.getenv("RC_VAM_NOREG_BATCH", unset = "")
requested_outcomes <- Sys.getenv("RC_VAM_NOREG_OUTCOMES", unset = "")

if (length(outcomes_manual) > 0) {
  message("Outcome selection: manual list in the script.")
  outcomes <- all_outcomes[all_outcomes %in% outcomes_manual]
} else if (nzchar(trimws(requested_batch))) {
  batch_num <- suppressWarnings(as.integer(trimws(requested_batch)))
  if (is.na(batch_num) || batch_num < 1 || batch_num > length(batches)) {
    stop(
      "RC_VAM_NOREG_BATCH must be an integer between 1 and ", length(batches),
      " (", length(batches), " batches of up to 4 from ", length(all_outcomes),
      " outcomes).", call. = FALSE
    )
  }
  message("Outcome selection: batch ", batch_num, " of ", length(batches), ".")
  outcomes <- batches[[batch_num]]
} else if (nzchar(trimws(requested_outcomes))) {
  requested_outcomes <- trimws(strsplit(requested_outcomes, ",", fixed = TRUE)[[1]])
  unknown_outcomes <- setdiff(requested_outcomes, all_outcomes)
  if (length(unknown_outcomes) > 0) {
    stop(
      "RC_VAM_NOREG_OUTCOMES includes unknown outcome(s): ",
      paste(unknown_outcomes, collapse = ", "), call. = FALSE
    )
  }
  message("Outcome selection: RC_VAM_NOREG_OUTCOMES.")
  outcomes <- all_outcomes[all_outcomes %in% requested_outcomes]
} else {
  message("Outcome selection: none specified -- running all outcomes.")
  outcomes <- all_outcomes
}

# Skip any outcome that's already in the output file -- avoids both
# re-running an expensive fit for nothing AND (more importantly) avoids the
# merge step below silently overwriting a good prior result with, say, a
# crashed/partial rerun. Set RC_VAM_NOREG_FORCE=1 to recompute anyway (e.g.
# after a real code change) and overwrite what's there for those outcomes.
force_rerun <- Sys.getenv("RC_VAM_NOREG_FORCE", unset = "0") %in% c("1", "true", "TRUE")
if (!force_rerun) {
  already_done <- outcomes[sapply(outcomes, function(y) file.exists(outcome_output_path(y)))]
  if (length(already_done) > 0) {
    message(
      "Skipping (file already exists): ", paste(already_done, collapse = ", "),
      " -- set RC_VAM_NOREG_FORCE=1 to recompute anyway."
    )
  }
  outcomes <- setdiff(outcomes, already_done)
}

if (length(outcomes) == 0) {
  message("Nothing to do -- all requested outcomes already have a file in ", by_outcome_dir, ".")
  quit(save = "no", status = 0)
}
message("Outcomes to run this pass: ", paste(outcomes, collapse = ", "))

results_list <- list()

for (y in outcomes) {
  log_ts("---- RC VAM outcome: ", y, " ----")
  gc(full = TRUE)  # reclaim the previous iteration's d/gp_mat/df/model before allocating more
  log_ts("  gc() done, subsetting sample for this outcome...")

  d <- copy(analytic)
  if (!(y %in% c("admission_exam_taker", "higher_ed_enrolled_m1", "any_postulacion", "any_asignacion", "grat_asignacion"))) {
    d <- d[admission_exam_taker == 1]
  }

  keep_cols <- unique(c("school_rbd", y, x_terms_vars, prob_vars, iszero_vars))
  d <- d[, ..keep_cols]
  d <- d[!is.na(school_rbd) & !is.na(get(y))]
  d <- d[complete.cases(d[, c(x_vars_needed), with = FALSE])]
  log_ts("  Subsetting done: ", nrow(d), " rows.")

  if (nrow(d) == 0) {
    log_ts("  No observations left for outcome ", y, " -- skipping.")
    next
  }

  log_ts("  Scanning ", length(prob_vars) + length(iszero_vars), " g(p_i) columns for usable variance...")
  gp_used <- character()
  for (v in c(prob_vars, iszero_vars)) {
    sd_v <- sd(d[[v]], na.rm = TRUE)
    if (!is.na(sd_v) && sd_v > zero_variance_tol) gp_used <- c(gp_used, v)
  }
  log_ts("  g(p_i) terms with usable variance: ", length(gp_used))

  n_schools <- uniqueN(d$school_rbd)
  if (n_schools < 2) {
    log_ts("  Fewer than 2 distinct schools for outcome ", y, " -- skipping.")
    next
  }

  # REVISED 2026-09-21, THEN REVISED BACK on Acropolis: on a laptop, a dense
  # (rows) x (thousands of school dummies + 2,656 g(p_i) terms) matrix
  # genuinely didn't fit in memory ("cannot allocate vector of size 7.1 Gb",
  # then "5.4 Gb" for a different, larger-sample outcome), which is why this
  # was switched to absorbing school_rbd (feols demeaning, never
  # materializes the dummy block) with a naive sigma_hat/sqrt(n_j) SE
  # (the standard fallback in the VAM/teacher-effects literature -- Kane &
  # Staiger 2008, Chetty/Friedman/Rockoff 2014 -- when a full joint-
  # regression SE isn't computationally tractable with thousands of FEs).
  #
  # On a machine with enough RAM (Acropolis), that memory constraint doesn't
  # apply -- so this goes back to literal `factor(school_rbd)` dummies (no
  # reference level, via `0 +`, matching Stata's original
  # `ibn.school_rbd + regress` design) in ONE feols fit. That gives each
  # school's point estimate AND its standard error straight off feols's own
  # coefficient table -- an EXACT analytic SE, not the sigma_hat/sqrt(n_j)
  # approximation. This matters beyond just "more precise": the EB
  # shrinkage step downstream
  # (code/claude/rc_vam/02_construct_eb_rc_vam_values.R) uses each school's
  # SE directly to estimate signal variance (tau2) and shrinkage weight
  # (reliability) -- an approximate SE biases every school's shrinkage
  # amount, not just its own reported uncertainty. The g(p_i) block still
  # goes in as a single matrix column (gp_mat) rather than ~2,700 separate
  # formula terms -- that was never about memory, it was about R's formula
  # parser overflowing on that many additive terms, and that constraint is
  # unrelated to which machine this runs on. If this is ever run on a
  # memory-constrained machine again, switch the formula back to
  # `y ~ x_terms + gp_mat | school_rbd` (absorbed) and restore the
  # fixef()/sigma()-based SE -- see the acropolis_transfer copy's git
  # history for that version.
  log_ts("  Building gp_mat (", nrow(d), " x ", length(gp_used), ") matrix column...")
  gp_mat <- as.matrix(d[, ..gp_used])
  df <- as.data.frame(d[, c("school_rbd", y, x_terms_vars), with = FALSE])
  df$gp_mat <- gp_mat
  rm(gp_mat, d)
  gc(full = TRUE)  # d and gp_mat's OWN name are no longer needed once df holds
                    # the data (df$gp_mat references the same memory, not a
                    # copy) -- drop d specifically since it duplicates every
                    # gp_used column df$gp_mat already has, right before the
                    # single biggest allocation in this loop.
  log_ts("  gp_mat built, memory freed. Constructing model formula and fitting...")

  model_formula <- as.formula(
    paste0(y, " ~ 0 + factor(school_rbd) + ", paste(x_terms, collapse = " + "), " + gp_mat")
  )

  log_ts(
    "  Fitting feols (", n_schools, " literal school dummies + ", length(gp_used),
    " g(p_i) terms via a single matrix column, all unregularized) on ",
    nrow(df), " observations -- this is usually the single longest step in",
    " the loop, next message fires when it's done..."
  )
  .t0_fit <- Sys.time()
  model <- feols(model_formula, data = df, notes = FALSE)
  log_ts(
    "  feols fit done. (", round(as.numeric(difftime(Sys.time(), .t0_fit, units = "secs")), 1), " sec)"
  )

  ct <- as.data.frame(fixest::coeftable(model))
  is_school_term <- grepl("^factor\\(school_rbd\\)", rownames(ct))
  school_ct <- ct[is_school_term, , drop = FALSE]
  school_rbd_vals <- suppressWarnings(as.numeric(
    sub("^factor\\(school_rbd\\)", "", rownames(school_ct))
  ))

  model_obs <- obs(model)
  regression_counts <- as.data.table(list(school_rbd = df$school_rbd[model_obs]))[
    , .(n_students_regression = .N), by = school_rbd
  ]
  n_total <- sum(regression_counts$n_students_regression)

  fe_tbl <- tibble(
    school_rbd = school_rbd_vals,
    controlled_value_added = school_ct[["Estimate"]],
    controlled_value_added_se = school_ct[["Std. Error"]]
  ) %>%
    filter(!is.na(school_rbd)) %>%
    left_join(as_tibble(regression_counts), by = "school_rbd")

  weighted_mean <- with(
    fe_tbl,
    sum(controlled_value_added * n_students_regression, na.rm = TRUE) /
      sum(n_students_regression, na.rm = TRUE)
  )
  fe_tbl$controlled_value_added_centered_student <-
    fe_tbl$controlled_value_added - weighted_mean

  out <- fe_tbl %>%
    mutate(
      outcome = y,
      n_students_regression_total = n_total,
      n_gp_terms_candidate = length(gp_used),
      n_gp_terms_used = length(gp_used),
      controlled_value_added_se_method =
        "r_feols_factor_school_rbd_noconstant_analytic_se_no_regularization"
    ) %>%
    select(
      outcome, school_rbd, controlled_value_added, controlled_value_added_se,
      controlled_value_added_centered_student, n_students_regression,
      n_students_regression_total, n_gp_terms_candidate, n_gp_terms_used,
      controlled_value_added_se_method
    ) %>%
    mutate(analysis_sample = "SAE_timely_RC_VAM_r")

  results_list[[y]] <- out

  # Write THIS outcome's own file -- a plain overwrite, never a read of
  # anyone else's rows. Per-outcome incremental saving is still what
  # protects against losing work if the process is killed mid-loop; it's
  # just one file per outcome now instead of a shared file that every
  # outcome's save had to read-merge-rewrite.
  this_path <- outcome_output_path(y)
  fwrite(as.data.table(out), this_path)
  log_ts("  Saved to disk: ", this_path)
  log_ts("---- Done with outcome: ", y, " (", n_schools, " schools) ----")
}

log_ts(
  "Done with this pass. Run 01b_combine_rc_vam_noreg_by_outcome.R to ",
  "(re)build the combined results/diagnostics CSVs from everything in ",
  by_outcome_dir, "."
)
