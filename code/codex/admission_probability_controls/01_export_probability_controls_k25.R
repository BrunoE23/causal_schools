####################################
# Build reduced admission-probability controls for the IV estimation sample.
#
# This is a Codex-owned version of code/prob_admission_export.R. It avoids the
# old top-by-spots school filter and instead defines the practical support from
# timely, at-risk SAE applicants in the current broad universe.
####################################

library(tidyverse)

data_wd <- "C:/Users/xd-br/Dropbox/causal_schools"

probability_years <- 2018:2021
support_k <- 100L
min_students_positive_prob <- support_k
probability_digits <- 2L
write_stata_dta <- FALSE

probs_dir <- file.path(data_wd, "data", "clean", "DA_probs")
universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")

output_stub <- paste0("k", min_students_positive_prob, "_timely_risk")
wide_csv_path <- file.path(probs_dir, paste0("probs_columns_wide_", output_stub, ".csv"))
wide_dta_path <- file.path(probs_dir, paste0("probs_columns_wide_", output_stub, ".dta"))
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

message("Reading simulated DA probabilities.")
prob_files <- tibble(sae_proceso = probability_years) %>%
  mutate(path = file.path(probs_dir, paste0("DA_probs_", sae_proceso, ".csv"))) %>%
  filter(file.exists(path))

missing_years <- setdiff(probability_years, prob_files$sae_proceso)
if (length(missing_years) > 0) {
  warning("Missing DA probability files for process year(s): ", paste(missing_years, collapse = ", "))
}

probs_all <- map_dfr(prob_files$sae_proceso, read_da_prob_file)

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

risk_flags <- probs_estimation %>%
  group_by(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1R) %>%
  summarise(
    any_risk = as.integer(max(prob_raw, na.rm = TRUE) < 1),
    total_probability_mass = sum(prob_raw, na.rm = TRUE),
    n_positive_probability_options = sum(prob_raw > 0, na.rm = TRUE),
    .groups = "drop"
  )

at_risk_students <- risk_flags %>%
  filter(any_risk == 1L)

message("Defining practical school support using k = ", min_students_positive_prob, ".")
school_support <- probs_estimation %>%
  semi_join(at_risk_students %>% select(student_id, sae_proceso), by = c("student_id", "sae_proceso")) %>%
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

support_rbd <- school_support$rbd_prob

probs_supported <- probs_estimation %>%
  semi_join(at_risk_students %>% select(student_id, sae_proceso), by = c("student_id", "sae_proceso")) %>%
  filter(rbd_prob %in% support_rbd) %>%
  select(student_id, sae_proceso, rbd_prob, prob_r)

message("Pivoting supported probabilities wide.")
probs_supported_wide <- probs_supported %>%
  pivot_wider(
    id_cols = c(student_id, sae_proceso),
    names_from = rbd_prob,
    values_from = prob_r,
    names_prefix = "prob_",
    values_fill = 0
  )

probs_wide <- at_risk_students %>%
  left_join(probs_supported_wide, by = c("student_id", "sae_proceso"))

prob_cols <- names(probs_wide)[startsWith(names(probs_wide), "prob_")]

probs_wide <- probs_wide %>%
  mutate(across(all_of(prob_cols), ~ replace_na(.x, 0))) %>%
  mutate(retained_probability_mass = rowSums(across(all_of(prob_cols)), na.rm = TRUE)) %>%
  mutate(
    across(
      all_of(prob_cols),
      ~ as.integer(.x == 0),
      .names = "{sub('prob_', 'iszero_', .col)}"
    )
  )

diagnostics <- tibble(
  measure = c(
    "broad_universe_rows",
    "timely_sae_students",
    "probability_rows_raw",
    "timely_students_with_probability_rows",
    "at_risk_students",
    "supported_probability_options",
    "wide_probability_columns",
    "wide_iszero_columns",
    "mean_total_probability_mass",
    "mean_retained_probability_mass",
    "min_retained_probability_mass"
  ),
  value = c(
    NA_real_,
    nrow(universe),
    nrow(probs_all),
    n_distinct(probs_estimation$student_id),
    nrow(at_risk_students),
    length(support_rbd),
    length(prob_cols),
    length(prob_cols),
    mean(risk_flags$total_probability_mass, na.rm = TRUE),
    mean(probs_wide$retained_probability_mass, na.rm = TRUE),
    min(probs_wide$retained_probability_mass, na.rm = TRUE)
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

message("Writing outputs.")
write_csv_fast(probs_wide, wide_csv_path)
write_csv(school_support, support_path)
write_csv(diagnostics, diagnostics_path)

if (write_stata_dta && requireNamespace("haven", quietly = TRUE)) {
  haven::write_dta(probs_wide, wide_dta_path)
} else if (write_stata_dta) {
  warning("Package haven is not installed; skipped .dta export.")
} else {
  message("Skipped .dta export because write_stata_dta is FALSE.")
}

message("Done.")
message("Wide probability controls: ", wide_csv_path)
message("School support file: ", support_path)
message("Diagnostics file: ", diagnostics_path)
