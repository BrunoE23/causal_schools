####################################
# ARCHIVED 2026-09-21 -- superseded by
# code/claude/rc_vam/00_build_rc_vam_analysis_sample.R, which consolidates
# this script's probs/roster/support-threshold construction AND
# 00b_build_gp_wide_dta.R's wide pivot AND the rich X/outcomes construction
# that used to live in Steps 1-3 of 01_construct_rc_vam_school_values.do
# into one script, writing one file (rc_vam_stata_analysis_sample.dta) that
# all three Stata regression scripts (ridge/lasso/noreg) load directly.
# See setup_md_codex/project_memory/decisions/
# 2026-09-21-consolidate-rc-vam-data-prep-into-one-r-script.md.
#
# Kept here for provenance only -- do not run as part of the current
# pipeline.
####################################

####################################
# Build g(p_i) probability controls for ALL timely SAE applicants -- not just
# those with genuine assignment risk.
#
# This is a modified copy of
#   code/codex/admission_probability_controls/01_export_probability_controls_k25.R
# The only substantive change: the population used to define "practical
# school support" and to build the wide prob_*/iszero_* control matrix is
# ALL timely SAE applicants (risk_flags), not just the subset with
# any_risk == 1.
#
# RATIONALE: RC VAM (Angrist, Hull, Pathak & Walters 2024) identifies school
# effects by CONDITIONING on p_i via g(p_i), not by instrumenting with offer
# variation. A student with degenerate risk (certain assignment to one
# school, e.g. via sibling priority) still has a well-defined p_i to
# condition on -- CIA doesn't require risk variation for that student
# specifically, only for the school-level Sargan/specification TEST that
# validates CIA. Restricting the estimation sample to any_risk == 1 (as the
# existing k25/k100 export does) was cutting the usable RC VAM sample roughly
# in half for no identification-theoretic reason: 318,813 timely SAE
# applicants vs. only 163,154 with genuine assignment risk (see
# probability_export_diagnostics_k100_timely_risk.csv).
#
# The at-risk-only matrix this script leaves untouched
# (probs_columns_wide_k100_timely_risk.csv) remains the right input for the
# AHPW-style specification test, since only at-risk students provide
# quasi-experimental offer variation to test CIA against. This script's
# output is for the RC VAM point estimates themselves.
####################################

library(tidyverse)

# ------------------------- Timing instrumentation ----------------------------
# Tracks wall-clock time per stage so runtime is recorded automatically,
# without needing to time the whole script by hand. Written out alongside the
# diagnostics file at the end; also printed to the console as each stage
# finishes.
.timing_log <- list()
.script_start_time <- Sys.time()

log_stage <- function(label, start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message(sprintf("  [timing] %s: %.1f sec", label, elapsed))
  .timing_log[[length(.timing_log) + 1]] <<- tibble(
    stage = label,
    elapsed_sec = elapsed,
    elapsed_min = elapsed / 60
  )
  invisible(elapsed)
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

probability_years <- 2018:2021
support_k <- as.integer(Sys.getenv("RC_VAM_MIN_SUPPORT", unset = "1"))
min_students_positive_prob <- support_k
probability_digits <- 2L
write_stata_dta <- FALSE

probs_dir <- file.path(data_wd, "data", "clean", "DA_probs")
universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")

# New stub: "all_timely_sae" instead of "timely_risk", so this never
# overwrites the at-risk-only matrix used for the specification test.
#
# STORAGE FORMAT: the g(p_i) matrix is overwhelmingly zeros (each student has
# a positive probability at only a handful of schools), so it is stored LONG/
# SPARSE -- one row per (student, school) pair with a nonzero probability --
# rather than pivoted wide. A dense wide CSV at k=100 here would be roughly
# 410k students x ~3,200 prob_*/iszero_* columns (~1.3 billion cells to store
# a few million real numbers), which is what made the wide export slow to
# write and huge on disk. The long file plus the roster file below are
# reconstructed into the same wide prob_wide object *in R memory* by
# 01_construct_rc_vam_school_values.R immediately before the regression --
# nothing changes about what feols() sees, only how it gets there.
output_stub <- paste0("k", min_students_positive_prob, "_all_timely_sae")
long_csv_path <- file.path(probs_dir, paste0("probs_long_", output_stub, ".csv"))
roster_path <- file.path(probs_dir, paste0("probs_roster_", output_stub, ".csv"))
support_path <- file.path(probs_dir, paste0("probability_support_", output_stub, ".csv"))
diagnostics_path <- file.path(probs_dir, paste0("probability_export_diagnostics_", output_stub, ".csv"))

read_da_prob_file <- function(year) {
  path <- file.path(probs_dir, paste0("DA_probs_", year, ".csv"))
  read_csv(
    path,
    col_types = cols(
      .default = col_skip(),
      student_id = col_double(),
      school_id = col_character(),
      prob = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(
      school_id = if_else(school_id == "unmatched", paste0("unmatched_", year), school_id),
      sae_proceso = as.integer(coalesce(str_extract(school_id, "\\d{4}$"), as.character(year))),
      rbd_prob = trimws(as.character(sub("_.*", "", school_id)))
    ) %>%
    select(student_id, sae_proceso, rbd_prob, prob)
}

.t0 <- Sys.time()
message("Reading broad universe sample: ", universe_path)
universe <- read_csv(
  universe_path,
  col_types = cols_only(
    mrun = col_double(),
    student_id = col_double(),
    cohort_gr8 = col_integer(),
    sae_proceso = col_integer(),
    timely_sae = col_integer(),
    rbd_treated_1R = col_double()
  ),
  show_col_types = FALSE
) %>%
  filter(timely_sae == 1L) %>%
  distinct(student_id, .keep_all = TRUE)
log_stage("read_universe", .t0)

.t0 <- Sys.time()
message("Reading simulated DA probabilities.")
prob_files <- tibble(sae_proceso = probability_years) %>%
  mutate(path = file.path(probs_dir, paste0("DA_probs_", sae_proceso, ".csv"))) %>%
  filter(file.exists(path))

missing_years <- setdiff(probability_years, prob_files$sae_proceso)
if (length(missing_years) > 0) {
  warning("Missing DA probability files for process year(s): ", paste(missing_years, collapse = ", "))
}

probs_all <- map_dfr(prob_files$sae_proceso, read_da_prob_file)
log_stage("read_da_probs_all_years", .t0)

.t0 <- Sys.time()
message("Keeping timely SAE students with matching probability rows.")
probs_estimation <- probs_all %>%
  inner_join(
    universe %>%
      select(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1R),
    by = c("student_id", "sae_proceso")
  ) %>%
  group_by(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1R, rbd_prob) %>%
  summarise(prob_raw = sum(prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(prob_r = round(prob_raw, probability_digits))
log_stage("build_probs_estimation", .t0)

.t0 <- Sys.time()
risk_flags <- probs_estimation %>%
  group_by(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1R) %>%
  summarise(
    any_risk = as.integer(max(prob_raw, na.rm = TRUE) < 1),
    total_probability_mass = sum(prob_raw, na.rm = TRUE),
    n_positive_probability_options = sum(prob_raw > 0, na.rm = TRUE),
    .groups = "drop"
  )
log_stage("build_risk_flags", .t0)

# KEY CHANGE vs. the k25/k100 "timely_risk" export: no `filter(any_risk == 1L)`
# here. all_timely_students covers every timely SAE applicant with a
# probability row, degenerate risk or not.
all_timely_students <- risk_flags

.t0 <- Sys.time()
message("Defining practical school support using k = ", min_students_positive_prob,
        " over ALL timely SAE applicants (not just at-risk).")
school_support <- probs_estimation %>%
  semi_join(all_timely_students %>% select(student_id, sae_proceso), by = c("student_id", "sae_proceso")) %>%
  filter(prob_raw > 0) %>%
  group_by(rbd_prob) %>%
  summarise(
    n_students_positive_prob = n_distinct(student_id),
    mean_positive_prob = mean(prob_raw, na.rm = TRUE),
    total_positive_prob = sum(prob_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_students_positive_prob >= min_students_positive_prob) %>%
  arrange(rbd_prob)
log_stage("compute_school_support", .t0)
message("  -> ", length(school_support$rbd_prob), " supported schools at k = ", min_students_positive_prob)

support_rbd <- school_support$rbd_prob

.t0 <- Sys.time()
# This IS the artifact we write to disk: only nonzero (student, school,
# probability) triples, filtered to schools that cleared the practical-
# support bar. No pivot, no zero-filling, no iszero_ columns -- those are all
# reconstructed downstream from what's absent here.
probs_supported <- probs_estimation %>%
  semi_join(all_timely_students %>% select(student_id, sae_proceso), by = c("student_id", "sae_proceso")) %>%
  filter(rbd_prob %in% support_rbd, prob_r != 0) %>%
  select(student_id, sae_proceso, rbd_prob, prob_r)
log_stage("filter_probs_supported", .t0)

.t0 <- Sys.time()
message("Computing retained probability mass (sum of nonzero supported probs per student).")
retained_mass <- probs_supported %>%
  group_by(student_id, sae_proceso) %>%
  summarise(retained_probability_mass = sum(prob_r, na.rm = TRUE), .groups = "drop")

# Roster: every all-timely-SAE student, one row each, with the passthrough
# columns the regression script needs plus retained probability mass. This is
# small (one row per student) regardless of how many schools are supported,
# unlike the old wide file.
roster <- all_timely_students %>%
  left_join(retained_mass, by = c("student_id", "sae_proceso")) %>%
  mutate(retained_probability_mass = replace_na(retained_probability_mass, 0))
log_stage("build_roster_and_retained_mass", .t0)

diagnostics <- tibble(
  measure = c(
    "broad_universe_rows",
    "timely_sae_students",
    "probability_rows_raw",
    "timely_students_with_probability_rows",
    "all_timely_students_used",
    "at_risk_students_for_comparison",
    "supported_probability_options",
    "long_file_nonzero_rows",
    "mean_total_probability_mass",
    "mean_retained_probability_mass",
    "min_retained_probability_mass"
  ),
  value = c(
    NA_real_,
    nrow(universe),
    nrow(probs_all),
    n_distinct(probs_estimation$student_id),
    nrow(all_timely_students),
    sum(risk_flags$any_risk == 1L),
    length(support_rbd),
    nrow(probs_supported),
    mean(risk_flags$total_probability_mass, na.rm = TRUE),
    mean(roster$retained_probability_mass, na.rm = TRUE),
    min(roster$retained_probability_mass, na.rm = TRUE)
  )
)

diagnostics$value[diagnostics$measure == "broad_universe_rows"] <- readr::read_csv(
  universe_path,
  col_types = cols_only(student_id = col_double()),
  show_col_types = FALSE
) %>%
  nrow()

write_csv_fast <- function(x, path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(x, path)
  } else {
    readr::write_csv(x, path)
  }
}

.t0 <- Sys.time()
message("Writing outputs (long/sparse format -- see header note).")
write_csv_fast(probs_supported, long_csv_path)
write_csv_fast(roster, roster_path)
write_csv(school_support, support_path)
write_csv(diagnostics, diagnostics_path)
log_stage("write_outputs", .t0)

if (write_stata_dta) {
  message("Skipped .dta export: write_stata_dta is for the old wide format and no longer applies to the long/sparse export.")
}

.timing_log_df <- bind_rows(.timing_log) %>%
  mutate(total_script_elapsed_sec = as.numeric(difftime(Sys.time(), .script_start_time, units = "secs")))
timing_path <- file.path(probs_dir, paste0("probability_export_timing_", output_stub, ".csv"))
write_csv(.timing_log_df, timing_path)

message("Done.")
message("Long/sparse probability controls (nonzero triples): ", long_csv_path)
message("Roster (one row per student, passthrough cols + retained mass): ", roster_path)
message("School support file: ", support_path)
message("Diagnostics file: ", diagnostics_path)
message("Timing file: ", timing_path)
message(sprintf("Total elapsed: %.1f sec", .timing_log_df$total_script_elapsed_sec[1]))
