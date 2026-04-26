####################################
# Build scalar school-value IV regression dataframe.
#
# This script is the construction layer. It starts from the broad grade-8
# universe produced by universe_reg_df.R, merges reduced probability
# controls, constructs the scalar school-value treatments and instruments, and
# exports a regression-ready dataframe.
####################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

data_wd <- "C:/Users/xd-br/Dropbox/causal_schools"

write_stata_dta <- FALSE
support_k <- 100L
output_stub <- paste0("k", support_k, "_timely_risk")

clean_dir <- file.path(data_wd, "data", "clean")
prob_dir <- file.path(clean_dir, "DA_probs")
values_dir <- file.path(clean_dir, "school_rbd_observational_values")
output_dir <- file.path(clean_dir, "scalar_school_value_iv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
probability_path <- file.path(prob_dir, paste0("probs_columns_wide_", output_stub, ".csv"))
support_path <- file.path(prob_dir, paste0("probability_support_", output_stub, ".csv"))
school_values_path <- file.path(values_dir, "school_rbd_observational_values.csv")

output_csv <- file.path(output_dir, paste0("scalar_school_value_iv_", output_stub, ".csv"))
output_dta <- file.path(output_dir, paste0("scalar_school_value_iv_", output_stub, ".dta"))
diagnostics_csv <- file.path(output_dir, paste0("scalar_school_value_iv_", output_stub, "_diagnostics.csv"))
baseline_csv <- file.path(output_dir, paste0("scalar_school_value_iv_", output_stub, "_baselines.csv"))

value_specs <- tibble::tribble(
  ~value_name,       ~outcome,                            ~value_column,
  "math_unadj",      "z_year_math_max",                   "raw_mean_centered_student",
  "math_adj",        "z_year_math_max",                   "controlled_value_added_centered_student",
  "leng_unadj",      "z_year_leng_max",                   "raw_mean_centered_student",
  "leng_adj",        "z_year_leng_max",                   "controlled_value_added_centered_student",
  "stem_unadj",      "stem_enrollment_m1",                "raw_mean_centered_student",
  "stem_adj",        "stem_enrollment_m1",                "controlled_value_added_centered_student",
  "stem_gap_adj",    "gender_gap__stem_enrollment_m1",    "controlled_value_added_centered_student"
)

z_within_group <- function(x) {
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sigma
}

write_csv_fast <- function(x, path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(x, path)
  } else {
    readr::write_csv(x, path)
  }
}

read_csv_fast <- function(path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    tibble::as_tibble(data.table::fread(path, showProgress = TRUE))
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }
}

message("Reading broad grade-8 universe: ", universe_path)
universe <- read_csv_fast(universe_path) %>%
  mutate(
    student_id = as.numeric(student_id),
    sae_proceso = as.integer(sae_proceso),
    rbd_treated_1R = as.numeric(rbd_treated_1R),
    most_time_RBD = as.numeric(most_time_RBD)
  )

if (!"z_year_math_max" %in% names(universe)) {
  universe <- universe %>%
    group_by(psu_year) %>%
    mutate(z_year_math_max = z_within_group(math_max)) %>%
    ungroup()
}

if (!"z_year_leng_max" %in% names(universe)) {
  universe <- universe %>%
    group_by(psu_year) %>%
    mutate(z_year_leng_max = z_within_group(leng_max)) %>%
    ungroup()
}

if (!"stem_enrollment_m1" %in% names(universe)) {
  universe <- universe %>%
    mutate(
      f_science_m1 = replace_na(as.numeric(f_science_m1), 0),
      f_eng_m1 = replace_na(as.numeric(f_eng_m1), 0),
      stem_enrollment_m1 = as.integer(f_science_m1 == 1 | f_eng_m1 == 1)
    )
}

message("Reading reduced k=25 probability controls: ", probability_path)
probabilities <- read_csv_fast(probability_path) %>%
  mutate(
    student_id = as.numeric(student_id),
    sae_proceso = as.integer(sae_proceso),
    rbd_treated_1R = as.numeric(rbd_treated_1R)
  )

message("Reading supported probability-control schools: ", support_path)
supported_rbd <- read_csv(support_path, show_col_types = FALSE) %>%
  transmute(supported_rbd = suppressWarnings(as.numeric(rbd_prob))) %>%
  filter(!is.na(supported_rbd)) %>%
  distinct()

message("Reading school observational values: ", school_values_path)
school_values_raw <- read_csv(school_values_path, show_col_types = FALSE) %>%
  mutate(school_rbd = as.numeric(school_rbd))

build_value_spec <- function(spec_row, school_values) {
  spec <- as.list(spec_row)

  school_values %>%
    filter(outcome == spec$outcome) %>%
    transmute(
      school_rbd,
      value_name = spec$value_name,
      value = .data[[spec$value_column]]
    )
}

school_values_long <- bind_rows(
  lapply(seq_len(nrow(value_specs)), function(i) {
    build_value_spec(value_specs[i, ], school_values_raw)
  })
)

school_values_wide <- school_values_long %>%
  pivot_wider(
    names_from = value_name,
    values_from = value,
    names_prefix = "school_value_"
  )

message("Constructing estimation sample from universe + probabilities.")
estimation_df <- universe %>%
  inner_join(
    probabilities,
    by = c("student_id", "sae_proceso"),
    suffix = c("", "_prob")
  ) %>%
  mutate(
    rbd_treated_1R = coalesce(rbd_treated_1R_prob, rbd_treated_1R),
    supported_offer_rbd = as.integer(rbd_treated_1R %in% supported_rbd$supported_rbd)
  ) %>%
  select(-ends_with("_prob"))

estimation_df <- estimation_df %>%
  left_join(
    school_values_wide %>% rename_with(~ paste0("attended_", .x), starts_with("school_value_")),
    by = c("most_time_RBD" = "school_rbd")
  ) %>%
  left_join(
    school_values_wide %>% rename_with(~ paste0("offered_", .x), starts_with("school_value_")),
    by = c("rbd_treated_1R" = "school_rbd")
  )

message("Constructing D and Z variables with outside-support offered-school baselines.")
baseline_rows <- list()

for (value_name in value_specs$value_name) {
  attended_col <- paste0("attended_school_value_", value_name)
  offered_col <- paste0("offered_school_value_", value_name)
  d_col <- paste0("d_", value_name)
  z_col <- paste0("z_", value_name)

  baseline_value <- estimation_df %>%
    filter(
      supported_offer_rbd == 0,
      !is.na(rbd_treated_1R),
      rbd_treated_1R != 0,
      !is.na(.data[[offered_col]])
    ) %>%
    summarise(value = mean(.data[[offered_col]], na.rm = TRUE), .groups = "drop") %>%
    pull(value)

  if (length(baseline_value) == 0 || is.na(baseline_value)) {
    warning("Outside-support offered-school baseline is missing for ", value_name, "; using zero.")
    baseline_value <- 0
  }

  baseline_rows[[value_name]] <- tibble(
    value_name = value_name,
    outside_support_offered_baseline = baseline_value
  )

  estimation_df[[d_col]] <- estimation_df[[attended_col]] - baseline_value
  estimation_df[[z_col]] <- if_else(
    estimation_df$supported_offer_rbd == 1 & !is.na(estimation_df[[offered_col]]),
    estimation_df[[offered_col]] - baseline_value,
    0
  )
}

estimation_df <- estimation_df %>%
  select(
    -any_of("MRUN"),
    -starts_with("attended_school_value_"),
    -starts_with("offered_school_value_")
  )

core_export_vars <- c(
  "student_id",
  "mrun",
  "cohort_gr8",
  "sae_proceso",
  "timely_sae",
  "rbd_treated_1R",
  "most_time_RBD",
  "supported_offer_rbd",
  "any_risk",
  "total_probability_mass",
  "retained_probability_mass",
  "n_positive_probability_options",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  "math_max",
  "leng_max",
  "psu_year",
  "z_year_math_max",
  "z_year_leng_max",
  "stem_enrollment_m1"
)

estimation_df <- estimation_df %>%
  select(
    any_of(core_export_vars),
    starts_with("prob_"),
    starts_with("iszero_"),
    starts_with("d_"),
    starts_with("z_")
  )

baseline_df <- bind_rows(baseline_rows)

diagnostics <- tibble(
  measure = c(
    "universe_rows",
    "probability_rows",
    "estimation_rows_after_probability_join",
    "supported_rbd_count",
    "supported_offer_share",
    "mean_retained_probability_mass",
    "min_retained_probability_mass"
  ),
  value = c(
    nrow(universe),
    nrow(probabilities),
    nrow(estimation_df),
    nrow(supported_rbd),
    mean(estimation_df$supported_offer_rbd, na.rm = TRUE),
    mean(estimation_df$retained_probability_mass, na.rm = TRUE),
    min(estimation_df$retained_probability_mass, na.rm = TRUE)
  )
)

message("Writing regression-ready dataframe.")
write_csv_fast(estimation_df, output_csv)
write_csv(baseline_df, baseline_csv)
write_csv(diagnostics, diagnostics_csv)

if (write_stata_dta && requireNamespace("haven", quietly = TRUE)) {
  haven::write_dta(estimation_df, output_dta)
} else if (write_stata_dta) {
  warning("Package haven is not installed; skipped .dta export.")
} else {
  message("Skipped .dta export because write_stata_dta is FALSE.")
}

message("Done.")
message("Regression CSV: ", output_csv)
message("Baselines CSV: ", baseline_csv)
message("Diagnostics CSV: ", diagnostics_csv)
