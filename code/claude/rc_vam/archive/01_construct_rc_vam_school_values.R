###############################################################################
# Risk-Controlled Value-Added (RC VAM), following Angrist, Hull, Pathak &
# Walters (2024, "Credible School Value-Added with Undersubscribed School
# Lotteries"), equation (4):
#
#   Y_i = alpha_0 + sum_j alpha_j D_ij + X_i'Gamma + g(p_i) + eta_i
#
# where D_ij are school-attendance dummies (absorbed here as a school fixed
# effect on most_time_RBD), X_i is the same rich pre-treatment control vector
# used for the broad-panel observational VA in
# code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R,
# and g(p_i) is AHPW's risk-control function: linear terms in each student's
# simulated assignment-probability vector p_i, plus a dummy for each school
# where p_ij == 0.
#
# UNLIKE the broad-panel observational VA, this regression is estimated only
# on timely SAE applicants (people who actually went through the centralized
# assignment mechanism), because g(p_i) is only defined/estimable there. This
# is the key sample restriction discussed for moving toward RC VAM: it drops
# private non-subsidized schools automatically (they never apply to SAE) and
# restricts comparisons to families who went through the same assignment
# process, rather than the ~758k-student broad panel used for the current
# observational VA.
#
# NOTE: the sample here is ALL timely SAE applicants (timely_sae == 1), NOT
# restricted to genuine assignment risk (any_risk == 1). That risk
# restriction matters for IV VAM (offer variation only has power where risk
# is nondegenerate) but is not required by RC VAM's identification, which
# comes from CONDITIONING on p_i via g(p_i), not from instrumenting with it.
# A student with degenerate risk (certain assignment to one school) still has
# a well-defined p_i to condition on. Restricting to at-risk-only would cut
# this sample from ~319k to ~163k for no identification-theoretic reason.
# The at-risk-only subsample remains the right one for the separate
# AHPW-style Sargan/specification test that validates CIA, since only
# at-risk students provide quasi-experimental offer variation -- that test is
# not implemented in this script.
#
# g(p_i) is built from a probability control matrix covering ALL timely SAE
# applicants:
#   code/claude/rc_vam/00_export_probability_controls_all_timely_sae.R
# which narrows the school-probability columns to schools with enough
# students at positive probability to be informative (out of ~4,133 total
# schools), following the same practical-support logic as the existing
# at-risk-only k25/k100 export in
# code/codex/admission_probability_controls/01_export_probability_controls_k25.R.
# Run 00_export_probability_controls_all_timely_sae.R first if
# probs_long_k100_all_timely_sae.csv / probs_roster_k100_all_timely_sae.csv do
# not exist yet.
#
# STORAGE NOTE: g(p_i) is stored on disk as a long/sparse file (nonzero
# student-school probability triples) plus a small per-student roster, NOT as
# a dense wide CSV -- a dense wide file here would be ~410k students x ~3,200
# prob_*/iszero_* columns to hold a few million real numbers, which was slow
# to write and huge on disk. This script reconstructs the wide prob_wide
# object in R memory from those two small files right before the regression;
# the regression itself is unchanged.
#
# COMPUTATIONAL NOTE: the g(p_i) control block has ~2 x (num supported
# schools) continuous/binary columns (~2,026 at k=100 support) entered as
# regular covariates (not absorbed fixed effects), on top of the absorbed
# most_time_RBD school fixed effect. This is a genuinely heavy regression
# (comparable in spirit to the abandoned reg_rcvam.do prototype, which needed
# `set maxvar 20000` in Stata). Expect this to take real time and several GB
# of RAM per outcome. If it is not tractable on your machine, lower
# RC_VAM_MIN_SUPPORT (see below) to use a coarser support threshold.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(fixest)
  library(lfe)
  library(purrr)
  library(readr)
  library(tidyr)
})

# ------------------------- Path resolution -------------------------

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

parse_env_list <- function(var) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(trimws(value))) {
    return(character())
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
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
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

# k=100 practical-support probability control matrix, built over ALL timely
# SAE applicants (see 00_export_probability_controls_all_timely_sae.R).
# Lower RC_VAM_MIN_SUPPORT and rerun that script with a smaller support_k if
# the k=100 matrix is computationally infeasible.
rc_vam_min_support <- Sys.getenv("RC_VAM_MIN_SUPPORT", unset = "100")
prob_support_stub <- paste0("k", rc_vam_min_support, "_all_timely_sae")

clean_dir <- file.path(data_wd, "data", "clean")
prob_dir <- file.path(clean_dir, "DA_probs")
middle_school_controls_path <- file.path(
  clean_dir, "middle_school_controls", "middle_school_controls.csv"
)
program_income_path <- Sys.getenv(
  "PROGRAM_INCOME_OUTCOMES_PATH",
  unset = file.path(
    repo_wd,
    "output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv"
  )
)
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
# g(p_i) is stored LONG/SPARSE on disk (nonzero student-school probability
# triples + a small per-student roster), not as a dense wide CSV -- a dense
# wide file at k=100 over all timely SAE applicants would be roughly 410k
# students x ~3,200 prob_*/iszero_* columns to store a few million real
# numbers, which is slow to write/read and huge on disk for no benefit. The
# wide prob_wide object below is reconstructed in R memory from these two
# small files right before the regression; nothing downstream changes.
prob_long_path <- file.path(
  prob_dir, paste0("probs_long_", prob_support_stub, ".csv")
)
prob_roster_path <- file.path(
  prob_dir, paste0("probs_roster_", prob_support_stub, ".csv")
)

output_dir <- file.path(clean_dir, "rc_vam_school_values")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_path <- Sys.getenv(
  "RC_VAM_SCHOOL_VALUES_OUTPUT_PATH",
  unset = file.path(output_dir, "rc_vam_school_values.csv")
)
diagnostics_path <- file.path(output_dir, "rc_vam_school_values_diagnostics.csv")

configured_outcome_filter <- parse_env_list("RC_VAM_OUTCOMES")

# ------------------------- Configuration (mirrors Step-1 observational VA) --

school_var <- "most_time_RBD"
gender_var <- "GEN_ALU"
male_gender_code <- 1
female_gender_code <- 2
age_var <- "EDAD_ALU"
min_age <- 12
max_age <- 16

score_year_var <- "psu_year"
raw_score_outcomes <- c("math_max", "leng_max")
old_score_scale_years <- c(2021, 2022)
old_to_new_scale_factor <- 1000 / 850

# Same outcome set as the broad-panel observational VA and the current EB
# shrinkage step (code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R),
# so RC VAM estimates slot into the existing EB / scalar-IV pipeline with a
# path change rather than a schema change.
rc_vam_outcomes <- c(
  "z_year_math_max",
  "z_year_leng_max",
  "admission_exam_taker",
  "higher_ed_enrolled_m1",
  "stem_enrollment_m1",
  "log_program_income_clp_m1",
  "program_certified_years_m1",
  "inst_certified_years_m1"
)
if (length(configured_outcome_filter) > 0) {
  missing_filtered <- setdiff(configured_outcome_filter, rc_vam_outcomes)
  if (length(missing_filtered) > 0) {
    stop(
      "RC_VAM_OUTCOMES includes outcomes not configured for RC VAM: ",
      paste(missing_filtered, collapse = ", "),
      call. = FALSE
    )
  }
  rc_vam_outcomes <- rc_vam_outcomes[rc_vam_outcomes %in% configured_outcome_filter]
}

# Rich X_i control vector -- IDENTICAL to the broad-panel observational VA
# control set, so RC VAM and the current observational VA differ only in (a)
# sample (lottery-risk vs. broad panel) and (b) the addition of g(p_i), not in
# what individual-level information is conditioned on. This is the "use all
# the information" unification discussed: previously the pass-through IV
# stage only used a parsimonious W_i (cohort, gender, age, grade-4 scores)
# while the rich X_i only entered the broad-panel VA stage.
baseline_score_vars <- c("z_sim_mat_4to", "z_sim_leng_4to")
baseline_score_poly_terms <- unlist(lapply(baseline_score_vars, function(var) {
  c(var, paste0("I(", var, "^", 2:3, ")"))
}), use.names = FALSE)
baseline_cpad_control_vars <- c(
  "father_educ_years_imputed",
  "mother_educ_years_imputed",
  "father_indigenous_imputed",
  "mother_indigenous_imputed",
  "sala_cuna_imputed",
  "jardin_imputed",
  "prekinder_imputed",
  "kinder_imputed"
)
middle_school_control_vars <- c("z_gpa_middle_mean", "z_att_middle_mean")
middle_school_fixed_effect_vars <- c("most_time_RBD_middle")

x_control_terms <- c(
  "factor(cohort_gr8)",
  "factor(GEN_ALU)",
  "factor(EDAD_ALU)",
  "factor(COD_COM_ALU)",
  "income_decile_imputed",
  baseline_cpad_control_vars,
  middle_school_control_vars,
  "factor(middle_years_observed)",
  baseline_score_poly_terms
)
x_control_vars <- c(
  "cohort_gr8", "GEN_ALU", "EDAD_ALU", "COD_COM_ALU",
  "income_decile_imputed",
  baseline_cpad_control_vars,
  middle_school_control_vars,
  "middle_years_observed",
  baseline_score_vars
)

low_count_threshold <- 20L
zero_variance_tol <- 1e-8

# ------------------------- Helpers (mirrors Step-1 script) -------------------

stop_if_missing <- function(data, vars, label) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(label, " missing required columns: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
}

z_within_group <- function(x) {
  x_sd <- sd(x, na.rm = TRUE)
  if (is.na(x_sd) || x_sd == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / x_sd
}

read_program_income_outcomes <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Program income outcome file does not exist: ", path,
      ". Run code/codex/mifuturo_matricula_income/03_construct_person_level_income_outcomes.R first.",
      call. = FALSE
    )
  }
  keep_cols <- c("MRUN", "program_income_clp_m1", "log_program_income_clp_m1")
  header <- names(fread(path, nrows = 0, showProgress = FALSE))
  missing_cols <- setdiff(keep_cols, header)
  if (length(missing_cols) > 0) {
    stop("Program income outcome file is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  fread(path, select = keep_cols, na.strings = c("", "NA"), showProgress = FALSE) %>%
    as_tibble() %>%
    mutate(MRUN = as.character(MRUN)) %>%
    distinct(MRUN, .keep_all = TRUE)
}

# Bootstrap SEs for the centered school RC-VAM fixed effect, matching the
# approach used for the broad-panel observational VA
# (estimate_school_regression_se() in 01_construct_school_rbd_values.R), but
# with g(p_i) added to the control set passed to felm/getfe.
estimate_rc_vam_se <- function(regression_data, outcome, controls, fixed_effect_vars,
                               regression_counts, bootstrap_reps = 100L) {
  se_formula <- as.formula(
    paste0(outcome, " ~ ", paste(controls, collapse = " + "), " | ", paste(fixed_effect_vars, collapse = " + "))
  )

  se_model <- tryCatch(
    { fit <- felm(se_formula, data = as.data.frame(regression_data)); fit$call$formula <- se_formula; fit },
    error = function(e) e
  )
  if (inherits(se_model, "error")) {
    return(tibble(
      school_rbd = regression_counts$school_rbd,
      controlled_value_added_se = NA_real_,
      controlled_value_added_se_method = "lfe_getfe_school_rbd_iid_failed"
    ))
  }

  alpha_template <- getfe(se_model, se = FALSE, ef = "ref")
  school_rows <- which(alpha_template$fe == "school_rbd")
  school_ids <- as.character(alpha_template$idx[school_rows])
  weight_lookup <- regression_counts$n_students_regression
  names(weight_lookup) <- as.character(regression_counts$school_rbd)
  school_weights <- as.numeric(weight_lookup[school_ids])
  school_weights[is.na(school_weights)] <- 0
  if (sum(school_weights) <= 0) school_weights <- rep(1, length(school_rows))
  school_weights <- school_weights / sum(school_weights)

  school_centered_student_ef <- function(v, addnames) {
    school_values <- v[school_rows]
    out <- school_values - sum(school_weights * school_values)
    if (addnames) names(out) <- paste0("school_rbd.", school_ids)
    out
  }

  school_se_raw <- getfe(se_model, se = TRUE, ef = school_centered_student_ef, bN = bootstrap_reps)
  tibble(
    term = rownames(school_se_raw),
    controlled_value_added_se = school_se_raw$se
  ) %>%
    transmute(
      school_rbd = suppressWarnings(as.numeric(sub("^school_rbd\\.", "", term))),
      controlled_value_added_se
    ) %>%
    right_join(regression_counts %>% select(school_rbd), by = "school_rbd") %>%
    mutate(controlled_value_added_se_method = paste0("lfe_getfe_school_rbd_centered_student_iid_bN", bootstrap_reps))
}

estimate_rc_vam_value_added <- function(data, outcome, x_terms, gp_terms,
                                        control_vars_needed, fixed_effect_vars = "school_rbd") {
  all_needed <- unique(c(fixed_effect_vars, outcome, control_vars_needed, gp_terms))
  regression_data <- data %>%
    select(all_of(all_needed)) %>%
    filter(!is.na(school_rbd), !is.na(.data[[outcome]])) %>%
    drop_na(all_of(c(control_vars_needed, fixed_effect_vars)))

  # Drop g(p_i) columns with (near) zero variance in this outcome's estimation
  # sample -- otherwise perfectly collinear prob_/iszero_ columns for schools
  # nobody in the sample has any risk at will break the regression.
  gp_var <- vapply(regression_data[gp_terms], function(v) stats::var(v, na.rm = TRUE), numeric(1))
  gp_terms_used <- gp_terms[!is.na(gp_var) & gp_var > zero_variance_tol]

  controls <- c(x_terms, gp_terms_used)

  if (nrow(regression_data) == 0 || n_distinct(regression_data$school_rbd) < 2) {
    return(tibble(
      school_rbd = numeric(), outcome = outcome,
      controlled_value_added = numeric(), controlled_value_added_se = numeric(),
      controlled_value_added_centered_student = numeric(),
      n_students_regression = integer(), n_students_regression_total = integer(),
      n_gp_terms_used = length(gp_terms_used)
    ))
  }

  model_formula <- as.formula(
    paste0(outcome, " ~ ", paste(controls, collapse = " + "), " | ", paste(fixed_effect_vars, collapse = " + "))
  )
  model <- feols(model_formula, data = regression_data, notes = FALSE)
  school_fe <- fixef(model)[["school_rbd"]]
  model_obs <- obs(model)
  regression_used_data <- regression_data[model_obs, , drop = FALSE]

  regression_counts <- regression_used_data %>%
    group_by(school_rbd) %>%
    summarise(n_students_regression = n(), .groups = "drop")

  school_se <- estimate_rc_vam_se(
    regression_data = regression_used_data,
    outcome = outcome,
    controls = controls,
    fixed_effect_vars = fixed_effect_vars,
    regression_counts = regression_counts
  )

  fe_values <- tibble(
    school_rbd = as.numeric(names(school_fe)),
    outcome = outcome,
    controlled_value_added = as.numeric(school_fe),
    n_students_regression_total = nobs(model)
  ) %>%
    left_join(regression_counts, by = "school_rbd") %>%
    left_join(school_se, by = "school_rbd")

  fe_weighted_mean <- weighted.mean(fe_values$controlled_value_added, fe_values$n_students_regression, na.rm = TRUE)

  fe_values %>%
    mutate(
      controlled_value_added_centered_student = controlled_value_added - fe_weighted_mean,
      n_gp_terms_used = length(gp_terms_used)
    )
}

# ------------------------- Load rich X_i + outcomes (student-level) ---------

if (!file.exists(universe_path)) stop("Input file does not exist: ", universe_path, call. = FALSE)

message("Reading broad grade-8 universe (student-level controls + outcomes): ", universe_path)
input_cols <- unique(c(
  "MRUN", "mrun", "student_id", school_var, gender_var, age_var, "cohort_gr8", "sae_proceso",
  score_year_var, raw_score_outcomes,
  "COD_SIES_m1", "field_reclassified_m1", "f_science_m1", "f_eng_m1",
  "ACREDITADA_CARR_m1", "ACREDITADA_INST_m1", "ACRE_INST_ANIO_m1",
  "program_certified_years_m1", "institution_accredited_m1",
  x_control_vars
))
available_cols <- names(fread(universe_path, nrows = 0, showProgress = FALSE))
input_cols <- intersect(input_cols, available_cols)

df <- fread(universe_path, select = input_cols, na.strings = c("", "NA"), showProgress = TRUE) %>%
  as_tibble()

program_income_outcomes <- read_program_income_outcomes(program_income_path)
df <- df %>% mutate(MRUN = as.character(MRUN)) %>% left_join(program_income_outcomes, by = "MRUN")

if (!file.exists(middle_school_controls_path)) {
  stop("Middle-school controls file does not exist: ", middle_school_controls_path, call. = FALSE)
}
middle_school_controls <- read_csv(middle_school_controls_path, show_col_types = FALSE) %>%
  mutate(MRUN = as.character(MRUN)) %>%
  select(MRUN, most_time_RBD_middle, middle_years_observed, z_gpa_middle_mean, z_att_middle_mean)

df <- df %>% mutate(MRUN = as.character(MRUN)) %>% left_join(middle_school_controls, by = "MRUN")

stop_if_missing(df, c(school_var, x_control_vars, middle_school_fixed_effect_vars, age_var, "student_id"), "Main input")

analytic <- df %>%
  mutate(
    school_rbd = as.numeric(.data[[school_var]]),
    student_id = as.numeric(student_id),
    sae_proceso = as.integer(sae_proceso)
  ) %>%
  filter(!is.na(school_rbd), school_rbd > 0, !is.na(.data[[age_var]]), .data[[age_var]] >= min_age, .data[[age_var]] <= max_age)

# Outcome construction -- identical logic to Step-1 observational VA.
for (raw_var in raw_score_outcomes) {
  analytic[[raw_var]] <- ifelse(is.na(analytic[[raw_var]]) | analytic[[raw_var]] <= 0, NA_real_, as.numeric(analytic[[raw_var]]))
}
analytic <- analytic %>%
  mutate(across(all_of(raw_score_outcomes), ~ ifelse(!is.na(.data[[score_year_var]]) & .data[[score_year_var]] %in% old_score_scale_years, .x * old_to_new_scale_factor, .x))) %>%
  group_by(.data[[score_year_var]]) %>%
  mutate(z_year_math_max = z_within_group(math_max), z_year_leng_max = z_within_group(leng_max)) %>%
  ungroup() %>%
  mutate(
    admission_exam_taker = !is.na(math_max) | !is.na(leng_max),
    higher_ed_enrolled_m1 = as.integer(!is.na(trimws(as.character(COD_SIES_m1))) & nzchar(trimws(as.character(COD_SIES_m1))) & trimws(as.character(COD_SIES_m1)) != "NA"),
    f_science_m1 = ifelse(is.na(f_science_m1), 0, as.numeric(f_science_m1)),
    f_eng_m1 = ifelse(is.na(f_eng_m1), 0, as.numeric(f_eng_m1)),
    stem_enrollment_m1 = as.integer(f_science_m1 == 1 | f_eng_m1 == 1),
    observed_matricula_m1 = !is.na(ACREDITADA_CARR_m1) | !is.na(ACREDITADA_INST_m1) | !is.na(ACRE_INST_ANIO_m1),
    program_certified_years_m1 = case_when(
      is.na(program_certified_years_m1) & !observed_matricula_m1 ~ 0,
      TRUE ~ as.numeric(program_certified_years_m1)
    ),
    inst_certified_years_m1 = case_when(
      institution_accredited_m1 == 1L ~ as.numeric(ACRE_INST_ANIO_m1),
      institution_accredited_m1 == 0L ~ 0,
      is.na(institution_accredited_m1) & !observed_matricula_m1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-observed_matricula_m1)

if (anyDuplicated(analytic$student_id) > 0) {
  stop("Universe is not unique at the student_id level after filtering.", call. = FALSE)
}

# ------------------------- Load g(p_i): reconstruct wide matrix in memory ----
# from the long/sparse export (see 00_export_probability_controls_all_timely_sae.R).

if (!file.exists(prob_long_path) || !file.exists(prob_roster_path)) {
  stop(
    "Long/roster probability-control files do not exist: ", prob_long_path,
    " / ", prob_roster_path,
    ". Run code/claude/rc_vam/00_export_probability_controls_all_timely_sae.R first",
    " (with RC_VAM_MIN_SUPPORT = ", rc_vam_min_support, ").",
    call. = FALSE
  )
}

message("Reading g(p_i) long/sparse probability triples: ", prob_long_path)
prob_long <- fread(prob_long_path, na.strings = c("", "NA"))
prob_long[, student_id := as.numeric(student_id)]
prob_long[, sae_proceso := as.integer(sae_proceso)]
prob_long[, rbd_prob := as.character(rbd_prob)]

message("Reading g(p_i) roster (one row per student): ", prob_roster_path)
prob_roster <- fread(prob_roster_path, na.strings = c("", "NA"))
prob_roster[, student_id := as.numeric(student_id)]
prob_roster[, sae_proceso := as.integer(sae_proceso)]

message(
  "Reconstructing wide prob_*/iszero_* matrix in memory (",
  n_distinct(prob_long$rbd_prob), " supported schools x ",
  nrow(prob_roster), " students) -- this happens once, in RAM, right before the regression."
)
prob_wide <- prob_long %>%
  as_tibble() %>%
  pivot_wider(
    id_cols = c(student_id, sae_proceso),
    names_from = rbd_prob,
    values_from = prob_r,
    names_prefix = "prob_",
    values_fill = 0
  )

prob_wide <- prob_roster %>%
  as_tibble() %>%
  left_join(prob_wide, by = c("student_id", "sae_proceso"))

prob_cols <- names(prob_wide)[startsWith(names(prob_wide), "prob_")]
prob_wide <- prob_wide %>%
  mutate(across(all_of(prob_cols), ~ replace_na(.x, 0))) %>%
  mutate(
    across(
      all_of(prob_cols),
      ~ as.integer(.x == 0),
      .names = "{sub('prob_', 'iszero_', .col)}"
    )
  )
iszero_cols <- names(prob_wide)[startsWith(names(prob_wide), "iszero_")]
gp_terms_all <- c(prob_cols, iszero_cols)

message("g(p_i) control columns reconstructed: ", length(prob_cols), " prob_* + ", length(iszero_cols), " iszero_* = ", length(gp_terms_all))

keep_prob_cols <- c("student_id", "sae_proceso", "timely_sae", "rbd_treated_1R",
                     "any_risk", "total_probability_mass", "retained_probability_mass",
                     "n_positive_probability_options", gp_terms_all)
keep_prob_cols <- intersect(keep_prob_cols, names(prob_wide))
prob_wide <- prob_wide[, keep_prob_cols]

# ------------------------- RC VAM estimation sample --------------------------
# Restricting to timely SAE applicants (ALL of them, not just any_risk == 1).
# This is the sample restriction at the heart of moving to RC VAM: it is
# strictly narrower than the ~758k-student broad panel used for the current
# observational VA, but it (a) never includes private non-subsidized schools,
# since they never apply through SAE, and (b) restricts comparisons to
# families who went through the same centralized assignment process,
# addressing both the sector-comparability point and the deeper
# Dale-and-Krueger-style comparability point discussed for RC VAM. It does
# NOT further restrict to genuine assignment risk -- that restriction
# belongs to the separate specification test, not to the point estimate
# (see header note). any_risk is kept as a passthrough column so the output
# / a downstream script can still subset to the at-risk sample if wanted.
message("Merging student-level controls/outcomes with g(p_i) sample (all timely SAE applicants).")
estimation_df <- analytic %>%
  inner_join(as_tibble(prob_wide), by = c("student_id", "sae_proceso")) %>%
  filter(timely_sae == 1)

message("RC VAM estimation sample: ", nrow(estimation_df), " students, ",
        n_distinct(estimation_df$school_rbd), " distinct attended schools.")

# ------------------------- Run RC VAM per outcome -----------------------------

outcome_requires_exam_taker <- function(outcome_name) {
  !(outcome_name == "admission_exam_taker" || grepl("^higher_ed_enrolled_", outcome_name))
}

rc_vam_values <- map_dfr(rc_vam_outcomes, function(outcome_name) {
  message("---- RC VAM outcome: ", outcome_name, " ----")
  sample_data <- estimation_df
  if (outcome_requires_exam_taker(outcome_name)) {
    sample_data <- sample_data %>% filter(admission_exam_taker)
  }
  estimate_rc_vam_value_added(
    data = sample_data,
    outcome = outcome_name,
    x_terms = x_control_terms,
    gp_terms = gp_terms_all,
    control_vars_needed = x_control_vars
  ) %>%
    mutate(analysis_sample = "SAE_timely_RC_VAM")
})

if (anyDuplicated(rc_vam_values[c("school_rbd", "outcome")]) > 0) {
  stop("RC VAM output has duplicate school_rbd-outcome rows.", call. = FALSE)
}

message("Writing RC VAM school values: ", output_path)
write_csv(rc_vam_values, output_path)

diagnostics <- rc_vam_values %>%
  group_by(outcome) %>%
  summarise(
    n_schools = n_distinct(school_rbd),
    n_students_regression = sum(n_students_regression, na.rm = TRUE),
    n_gp_terms_used = first(n_gp_terms_used),
    sd_rc_vam = sd(controlled_value_added_centered_student, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(diagnostics, diagnostics_path)

message("Done.")
message("Wrote: ", output_path)
message("Wrote: ", diagnostics_path)
print(diagnostics)
