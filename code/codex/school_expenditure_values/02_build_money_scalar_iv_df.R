suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Build a regression-ready scalar IV dataframe using school public funding as D.
#
# This reuses the existing scalar school-value IV dataframe so that the sample,
# probability controls, and baseline controls match the main IV exercise.

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

support_k <- 100L
output_stub <- paste0("k", support_k, "_timely_risk")

clean_dir <- file.path(data_wd, "data/clean")
scalar_iv_dir <- file.path(clean_dir, "scalar_school_value_iv")
funding_dir <- file.path(clean_dir, "school_expenditure_values")
output_dir <- file.path(clean_dir, "scalar_school_value_iv_money")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

base_scalar_path <- file.path(scalar_iv_dir, paste0("scalar_school_value_iv_", output_stub, ".csv"))
funding_path <- file.path(funding_dir, "school_public_funding_per_student_2017_2021.csv")

output_csv <- file.path(output_dir, paste0("scalar_school_money_iv_", output_stub, ".csv"))
baseline_csv <- file.path(output_dir, paste0("scalar_school_money_iv_", output_stub, "_baselines.csv"))
diagnostics_csv <- file.path(output_dir, paste0("scalar_school_money_iv_", output_stub, "_diagnostics.csv"))

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

money_specs <- tibble::tribble(
  ~value_name,              ~funding_column,                                                          ~scale,
  "money_level_millions",   "enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021",    1e6,
  "money_growth_millions",  "change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018",    1e6
)

message("Reading existing scalar IV dataframe: ", base_scalar_path)
base_df <- read_csv_fast(base_scalar_path) %>%
  mutate(
    most_time_RBD = as.numeric(most_time_RBD),
    rbd_treated_1R = as.numeric(rbd_treated_1R),
    supported_offer_rbd = as.integer(supported_offer_rbd)
  )

message("Reading school public funding values: ", funding_path)
funding_values <- read_csv(funding_path, show_col_types = FALSE) %>%
  mutate(school_rbd = as.numeric(school_rbd)) %>%
  select(school_rbd, all_of(money_specs$funding_column))

for (i in seq_len(nrow(money_specs))) {
  spec <- money_specs[i, ]
  funding_values[[paste0("school_value_", spec$value_name)]] <-
    funding_values[[spec$funding_column]] / spec$scale
}

funding_values <- funding_values %>%
  select(school_rbd, starts_with("school_value_"))

money_df <- base_df %>%
  left_join(
    funding_values %>% rename_with(~ paste0("attended_", .x), starts_with("school_value_")),
    by = c("most_time_RBD" = "school_rbd")
  ) %>%
  left_join(
    funding_values %>% rename_with(~ paste0("offered_", .x), starts_with("school_value_")),
    by = c("rbd_treated_1R" = "school_rbd")
  )

baseline_rows <- list()

for (value_name in money_specs$value_name) {
  attended_col <- paste0("attended_school_value_", value_name)
  offered_col <- paste0("offered_school_value_", value_name)
  d_col <- paste0("d_", value_name)
  z_col <- paste0("z_", value_name)

  baseline_value <- money_df %>%
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

  money_df[[d_col]] <- money_df[[attended_col]] - baseline_value
  money_df[[z_col]] <- if_else(
    money_df$supported_offer_rbd == 1 & !is.na(money_df[[offered_col]]),
    money_df[[offered_col]] - baseline_value,
    0
  )
}

money_df <- money_df %>%
  select(
    -starts_with("attended_school_value_"),
    -starts_with("offered_school_value_")
  )

baseline_df <- bind_rows(baseline_rows)

diagnostics <- tibble(
  measure = c(
    "base_rows",
    "nonmissing_attended_money_level",
    "nonmissing_offered_money_level",
    "nonmissing_attended_money_growth",
    "nonmissing_offered_money_growth",
    "supported_offer_share"
  ),
  value = c(
    nrow(money_df),
    sum(!is.na(money_df$d_money_level_millions)),
    sum(money_df$supported_offer_rbd == 1 & money_df$z_money_level_millions != 0, na.rm = TRUE),
    sum(!is.na(money_df$d_money_growth_millions)),
    sum(money_df$supported_offer_rbd == 1 & money_df$z_money_growth_millions != 0, na.rm = TRUE),
    mean(money_df$supported_offer_rbd, na.rm = TRUE)
  )
)

message("Writing money IV dataframe.")
write_csv_fast(money_df, output_csv)
write_csv(baseline_df, baseline_csv)
write_csv(diagnostics, diagnostics_csv)

message("Done.")
message("Regression CSV: ", output_csv)
message("Baselines CSV: ", baseline_csv)
message("Diagnostics CSV: ", diagnostics_csv)
