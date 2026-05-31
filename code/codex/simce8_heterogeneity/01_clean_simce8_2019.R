####################################
# SIMCE 8B 2019 heterogeneity inputs
#
# This script follows the conventions in code/clean_simce_survey.R:
# - read SIMCE student scores and CPAD survey income from raw private files
# - preserve the raw score variables
# - convert CPAD income bins to midpoint CLP values using the same 15-bin map
# - construct z-scores and deciles after cleaning to one row per MRUN
####################################

library(tidyverse)

find_data_wd <- function() {
  candidates <- c(
    Sys.getenv("CAUSAL_SCHOOLS_DATA_WD"),
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  )

  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find data_wd. Set CAUSAL_SCHOOLS_DATA_WD or update candidates.")
  }

  candidates[[1]]
}

parse_number_quiet <- function(x) {
  readr::parse_number(as.character(x), locale = readr::locale(decimal_mark = "."))
}

income_midpoint_15_bins <- function(income_code) {
  dplyr::case_when(
    income_code == 1  ~   50000,
    income_code == 2  ~  150000,
    income_code == 3  ~  250000,
    income_code == 4  ~  350000,
    income_code == 5  ~  450000,
    income_code == 6  ~  550000,
    income_code == 7  ~  700000,
    income_code == 8  ~  900000,
    income_code == 9  ~ 1100000,
    income_code == 10 ~ 1300000,
    income_code == 11 ~ 1500000,
    income_code == 12 ~ 1700000,
    income_code == 13 ~ 1900000,
    income_code == 14 ~ 2100000,
    income_code == 15 ~ 2300000,
    TRUE ~ NA_real_
  )
}

row_mean2 <- function(x, y) {
  out <- rowMeans(cbind(x, y), na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}

read_pipe_header <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")

  # Some SIMCE planos have the entire header line quoted, with internal quotes
  # escaped as doubled quotes. Data rows are still ordinary pipe-delimited rows.
  if (stringr::str_detect(header, '^".*""\\|')) {
    header <- stringr::str_remove(header, '^"')
    header <- stringr::str_remove(header, '"$')
    header <- stringr::str_replace_all(header, '""', '"')
  }

  stringr::str_split(header, stringr::fixed("|"))[[1]] %>%
    stringr::str_remove('^"') %>%
    stringr::str_remove('"$')
}

read_pipe_csv <- function(path, cols) {
  header <- read_pipe_header(path)

  df <- readr::read_delim(
    file = path,
    delim = "|",
    quote = "\"",
    skip = 1,
    col_names = header,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  df %>%
    select(any_of(cols))
}

max_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

summarise_prediction <- function(df, x_var, y_var, label) {
  d <- df %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))

  if (nrow(d) == 0) {
    return(tibble(
      measure = label,
      x_var = x_var,
      y_var = y_var,
      n = 0L,
      correlation = NA_real_,
      slope = NA_real_,
      intercept = NA_real_,
      r_squared = NA_real_,
      same_decile_share = NA_real_,
      within_one_decile_share = NA_real_,
      mean_abs_decile_gap = NA_real_
    ))
  }

  fit <- lm(stats::reformulate(x_var, y_var), data = d)

  tibble(
    measure = label,
    x_var = x_var,
    y_var = y_var,
    n = nrow(d),
    correlation = stats::cor(d[[x_var]], d[[y_var]], use = "complete.obs"),
    slope = unname(stats::coef(fit)[[x_var]]),
    intercept = unname(stats::coef(fit)[["(Intercept)"]]),
    r_squared = summary(fit)$r.squared,
    same_decile_share = mean(d[[x_var]] == d[[y_var]]),
    within_one_decile_share = mean(abs(d[[x_var]] - d[[y_var]]) <= 1),
    mean_abs_decile_gap = mean(abs(d[[x_var]] - d[[y_var]]))
  )
}

add_math_heterogeneity_vars <- function(df) {
  df %>%
    mutate(
      simce8_math_quintile = dplyr::ntile(ptje_mate8b_alu, 5),
      simce8_vs_4to_math_decile_change = simce8_math_decile - simce4_math_decile,
      simce8_math_improved_gt1_decile = case_when(
        is.na(simce8_vs_4to_math_decile_change) ~ NA_integer_,
        simce8_vs_4to_math_decile_change > 1 ~ 1L,
        TRUE ~ 0L
      ),
      simce8_math_within1_decile = case_when(
        is.na(simce8_vs_4to_math_decile_change) ~ NA_integer_,
        abs(simce8_vs_4to_math_decile_change) <= 1 ~ 1L,
        TRUE ~ 0L
      ),
      simce8_math_worsened_gt1_decile = case_when(
        is.na(simce8_vs_4to_math_decile_change) ~ NA_integer_,
        simce8_vs_4to_math_decile_change < -1 ~ 1L,
        TRUE ~ 0L
      ),
      simce8_math_decile_movement = case_when(
        is.na(simce8_vs_4to_math_decile_change) ~ NA_character_,
        simce8_vs_4to_math_decile_change > 1 ~ "improved_more_than_1_decile",
        abs(simce8_vs_4to_math_decile_change) <= 1 ~ "within_1_decile",
        simce8_vs_4to_math_decile_change < -1 ~ "worsened_more_than_1_decile"
      )
    )
}

make_transition <- function(df, x_var, y_var, label) {
  df %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]])) %>%
    count(
      measure = label,
      grade4_decile = .data[[x_var]],
      grade8_decile = .data[[y_var]],
      name = "n"
    ) %>%
    group_by(measure, grade4_decile) %>%
    mutate(row_share = n / sum(n)) %>%
    ungroup()
}

data_wd <- find_data_wd()
setwd(data_wd)

simce_root <- file.path(data_wd, "data", "raw", "simce")
simce8_parent <- list.dirs(simce_root, recursive = FALSE, full.names = TRUE)
simce8_parent <- simce8_parent[
  stringr::str_detect(
    basename(simce8_parent),
    stringr::regex("Simce octavo.*2019.*privada", ignore_case = TRUE)
  )
]

if (length(simce8_parent) != 1) {
  stop("Expected exactly one SIMCE 8B 2019 private folder, found: ", length(simce8_parent))
}

txt_dir <- file.path(simce8_parent, "Archivos TXT (Planos)")

alu_path <- list.files(
  txt_dir,
  pattern = "simce8b2019_alu.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)
cpad_path <- list.files(
  txt_dir,
  pattern = "simce8b2019_cpad.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(alu_path) != 1) {
  stop("Expected exactly one SIMCE 8B ALU csv, found: ", length(alu_path))
}
if (length(cpad_path) != 1) {
  stop("Expected exactly one SIMCE 8B CPAD csv, found: ", length(cpad_path))
}

output_dir <- file.path(data_wd, "data", "clean", "simce8_heterogeneity")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

alu_raw <- read_pipe_csv(
  alu_path,
  c(
    "agno",
    "grado",
    "idalumno",
    "mrun",
    "rbd",
    "cod_curso",
    "gen_alu",
    "ptje_mate8b_alu",
    "ptje_lect8b_alu",
    "ptje_soc8b_alu",
    "noptje_aus_lect8b_alu",
    "noptje_aus_mate8b_alu",
    "noptje_nul_lect8b_alu",
    "noptje_nul_mate8b_alu",
    "noptje_bco_lect8b_alu",
    "noptje_bco_mate8b_alu"
  )
)

cpad_raw <- read_pipe_csv(
  cpad_path,
  c("agno", "idalumno", "rbd", "codigocurso", "cpad_p11", "noaplica", "noptje_seg")
)

alu <- alu_raw %>%
  transmute(
    idalumno = parse_number_quiet(idalumno),
    mrun = parse_number_quiet(mrun),
    simce_year = parse_number_quiet(agno),
    simce_grade = grado,
    simce_rbd_8b = parse_number_quiet(rbd),
    simce_course_8b = parse_number_quiet(cod_curso),
    gen_alu_8b = parse_number_quiet(gen_alu),
    ptje_mate8b_alu = parse_number_quiet(ptje_mate8b_alu),
    ptje_lect8b_alu = parse_number_quiet(ptje_lect8b_alu),
    ptje_soc8b_alu = parse_number_quiet(ptje_soc8b_alu),
    noptje_aus_lect8b_alu = parse_number_quiet(noptje_aus_lect8b_alu),
    noptje_aus_mate8b_alu = parse_number_quiet(noptje_aus_mate8b_alu),
    noptje_nul_lect8b_alu = parse_number_quiet(noptje_nul_lect8b_alu),
    noptje_nul_mate8b_alu = parse_number_quiet(noptje_nul_mate8b_alu),
    noptje_bco_lect8b_alu = parse_number_quiet(noptje_bco_lect8b_alu),
    noptje_bco_mate8b_alu = parse_number_quiet(noptje_bco_mate8b_alu)
  )

cpad <- cpad_raw %>%
  transmute(
    idalumno = parse_number_quiet(idalumno),
    simce_year = parse_number_quiet(agno),
    cpad_rbd_8b = parse_number_quiet(rbd),
    cpad_course_8b = parse_number_quiet(codigocurso),
    income_8b = parse_number_quiet(cpad_p11),
    cpad_noaplica_8b = parse_number_quiet(noaplica),
    cpad_noptje_seg_8b = parse_number_quiet(noptje_seg)
  ) %>%
  mutate(
    income_mid_8b = income_midpoint_15_bins(income_8b),
    income_8b_missing_code = income_8b %in% c(0, 99)
  ) %>%
  group_by(idalumno, simce_year) %>%
  summarise(
    cpad_rows_8b = n(),
    cpad_rbd_8b = dplyr::first(cpad_rbd_8b),
    cpad_course_8b = dplyr::first(cpad_course_8b),
    income_8b = dplyr::first(income_8b[!is.na(income_mid_8b)], default = NA_real_),
    income_mid_8b = dplyr::first(income_mid_8b[!is.na(income_mid_8b)], default = NA_real_),
    any_income_8b_missing_code = any(income_8b_missing_code, na.rm = TRUE),
    cpad_noaplica_8b = max_or_na(cpad_noaplica_8b),
    cpad_noptje_seg_8b = max_or_na(cpad_noptje_seg_8b),
    .groups = "drop"
  )

simce8_joined <- alu %>%
  left_join(cpad, by = c("idalumno", "simce_year")) %>%
  mutate(
    simce8_score_avg = row_mean2(ptje_mate8b_alu, ptje_lect8b_alu),
    all_simce8_scores_missing = is.na(ptje_mate8b_alu) & is.na(ptje_lect8b_alu)
  )

simce8_2019 <- simce8_joined %>%
  filter(!is.na(mrun)) %>%
  group_by(mrun) %>%
  arrange(
    desc(!is.na(simce8_score_avg)),
    desc(simce8_score_avg),
    idalumno,
    .by_group = TRUE
  ) %>%
  mutate(simce8_rows_per_mrun = n()) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    z_sim_mat_8b = as.numeric(scale(ptje_mate8b_alu)),
    z_sim_leng_8b = as.numeric(scale(ptje_lect8b_alu)),
    simce8_math_decile = dplyr::ntile(ptje_mate8b_alu, 10),
    simce8_math_quintile = dplyr::ntile(ptje_mate8b_alu, 5),
    simce8_leng_decile = dplyr::ntile(ptje_lect8b_alu, 10),
    simce8_score_avg_decile = dplyr::ntile(simce8_score_avg, 10),
    income_decile_8b = dplyr::ntile(income_mid_8b, 10)
  ) %>%
  select(
    mrun,
    idalumno,
    simce_year,
    simce_grade,
    simce_rbd_8b,
    simce_course_8b,
    gen_alu_8b,
    ptje_mate8b_alu,
    ptje_lect8b_alu,
    ptje_soc8b_alu,
    z_sim_mat_8b,
    z_sim_leng_8b,
    simce8_math_decile,
    simce8_math_quintile,
    simce8_leng_decile,
    simce8_score_avg,
    simce8_score_avg_decile,
    income_8b,
    income_mid_8b,
    income_decile_8b,
    cpad_rows_8b,
    cpad_rbd_8b,
    cpad_course_8b,
    any_income_8b_missing_code,
    cpad_noaplica_8b,
    cpad_noptje_seg_8b,
    all_simce8_scores_missing,
    simce8_rows_per_mrun,
    starts_with("noptje_")
  )

load(file.path(data_wd, "data", "clean", "simce_4to.Rdata"))

simce4_for_check <- simce_4to %>%
  transmute(
    mrun,
    simce_year_4to = simce_year,
    ptje_mate4b_alu,
    ptje_lect4b_alu,
    simce4_score_avg = row_mean2(ptje_mate4b_alu, ptje_lect4b_alu),
    income_mid_4to = income_mid,
    income_decile_4to = income_decile
  ) %>%
  mutate(
    simce4_math_decile = dplyr::ntile(ptje_mate4b_alu, 10),
    simce4_leng_decile = dplyr::ntile(ptje_lect4b_alu, 10),
    simce4_score_avg_decile = dplyr::ntile(simce4_score_avg, 10)
  )

simce4_to_simce8 <- simce4_for_check %>%
  inner_join(simce8_2019, by = "mrun") %>%
  add_math_heterogeneity_vars()

cohort_2019 <- readr::read_csv(
  file.path(data_wd, "data", "clean", "universe_controls.csv"),
  col_select = c("MRUN", "cohort_gr8"),
  show_col_types = FALSE,
  progress = FALSE
) %>%
  transmute(
    mrun = MRUN,
    MRUN,
    cohort_gr8
  ) %>%
  filter(cohort_gr8 == 2019) %>%
  distinct(mrun, .keep_all = TRUE)

cohort_2019_math_heterogeneity <- cohort_2019 %>%
  left_join(simce4_for_check, by = "mrun") %>%
  left_join(simce8_2019, by = "mrun") %>%
  add_math_heterogeneity_vars() %>%
  select(
    mrun,
    MRUN,
    cohort_gr8,
    simce_year_4to,
    simce_year_8b = simce_year,
    ptje_mate4b_alu,
    ptje_mate8b_alu,
    simce4_math_decile,
    simce8_math_decile,
    simce8_math_quintile,
    simce8_vs_4to_math_decile_change,
    simce8_math_decile_movement,
    simce8_math_improved_gt1_decile,
    simce8_math_within1_decile,
    simce8_math_worsened_gt1_decile
  )

prediction_summary <- bind_rows(
  summarise_prediction(
    simce4_to_simce8,
    "simce4_math_decile",
    "simce8_math_decile",
    "math_score_decile"
  ),
  summarise_prediction(
    simce4_to_simce8,
    "simce4_leng_decile",
    "simce8_leng_decile",
    "reading_score_decile"
  ),
  summarise_prediction(
    simce4_to_simce8,
    "simce4_score_avg_decile",
    "simce8_score_avg_decile",
    "math_reading_average_decile"
  ),
  summarise_prediction(
    simce4_to_simce8,
    "income_decile_4to",
    "income_decile_8b",
    "household_income_decile"
  )
)

transition_matrices <- bind_rows(
  make_transition(
    simce4_to_simce8,
    "simce4_math_decile",
    "simce8_math_decile",
    "math_score_decile"
  ),
  make_transition(
    simce4_to_simce8,
    "simce4_leng_decile",
    "simce8_leng_decile",
    "reading_score_decile"
  ),
  make_transition(
    simce4_to_simce8,
    "simce4_score_avg_decile",
    "simce8_score_avg_decile",
    "math_reading_average_decile"
  ),
  make_transition(
    simce4_to_simce8,
    "income_decile_4to",
    "income_decile_8b",
    "household_income_decile"
  )
)

diagnostics <- tibble(
  diagnostic = c(
    "alu_rows_raw",
    "cpad_rows_raw",
    "alu_rows_missing_mrun",
    "simce8_rows_after_one_per_mrun",
    "simce8_mruns_with_duplicate_alu_rows",
    "simce8_math_nonmissing",
    "simce8_reading_nonmissing",
    "simce8_income_mid_nonmissing",
    "simce4_to_simce8_overlap",
    "overlap_math_decile_nonmissing",
    "overlap_reading_decile_nonmissing",
    "overlap_income_decile_nonmissing",
    "cohort_2019_rows",
    "cohort_2019_math_decile_nonmissing",
    "cohort_2019_math_quintile_nonmissing",
    "cohort_2019_math_movement_nonmissing"
  ),
  value = c(
    nrow(alu_raw),
    nrow(cpad_raw),
    sum(is.na(alu$mrun)),
    nrow(simce8_2019),
    sum(simce8_2019$simce8_rows_per_mrun > 1, na.rm = TRUE),
    sum(!is.na(simce8_2019$simce8_math_decile)),
    sum(!is.na(simce8_2019$simce8_leng_decile)),
    sum(!is.na(simce8_2019$income_mid_8b)),
    nrow(simce4_to_simce8),
    sum(!is.na(simce4_to_simce8$simce4_math_decile) & !is.na(simce4_to_simce8$simce8_math_decile)),
    sum(!is.na(simce4_to_simce8$simce4_leng_decile) & !is.na(simce4_to_simce8$simce8_leng_decile)),
    sum(!is.na(simce4_to_simce8$income_decile_4to) & !is.na(simce4_to_simce8$income_decile_8b)),
    nrow(cohort_2019_math_heterogeneity),
    sum(!is.na(cohort_2019_math_heterogeneity$simce8_math_decile)),
    sum(!is.na(cohort_2019_math_heterogeneity$simce8_math_quintile)),
    sum(!is.na(cohort_2019_math_heterogeneity$simce8_math_decile_movement))
  )
)

math_movement_summary <- cohort_2019_math_heterogeneity %>%
  count(simce8_math_decile_movement, name = "n") %>%
  mutate(
    share_all_cohort = n / nrow(cohort_2019_math_heterogeneity),
    share_nonmissing_movement = if_else(
      is.na(simce8_math_decile_movement),
      NA_real_,
      n / sum(n[!is.na(simce8_math_decile_movement)])
    )
  )

readr::write_csv(simce8_2019, file.path(output_dir, "simce8_2019_clean.csv"))
save(simce8_2019, file = file.path(output_dir, "simce8_2019_clean.Rdata"))

readr::write_csv(
  simce4_to_simce8,
  file.path(output_dir, "simce4_to_simce8_overlap.csv")
)
readr::write_csv(
  cohort_2019_math_heterogeneity,
  file.path(output_dir, "cohort_2019_math_heterogeneity.csv")
)
readr::write_csv(
  prediction_summary,
  file.path(output_dir, "simce4_to_simce8_prediction_summary.csv")
)
readr::write_csv(
  transition_matrices,
  file.path(output_dir, "simce4_to_simce8_transition_matrices.csv")
)
readr::write_csv(
  diagnostics,
  file.path(output_dir, "simce8_2019_diagnostics.csv")
)
readr::write_csv(
  math_movement_summary,
  file.path(output_dir, "cohort_2019_math_movement_summary.csv")
)

print(diagnostics)
print(prediction_summary)
print(math_movement_summary)
