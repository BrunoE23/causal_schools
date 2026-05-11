suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
})

# Build school-level public funding per student from MINEDUC Subvenciones-a-EE.
#
# First-pass measure:
#   annual public funding per student =
#     sum of selected monthly subsidy/payment components in a year
#     divided by average monthly enrollment in the same year.
#
# This is public funding received by the school, not total expenditure.
# Monetary amounts are converted to 2021 pesos using Chile's annual CPI index
# from World Bank indicator FP.CPI.TOTL, sourced from IMF IFS.

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

finance_root <- file.path(data_wd, "data/raw/school_finances")
output_dir <- file.path(data_wd, "data/clean/school_expenditure_values")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

school_year_output <- file.path(output_dir, "school_public_funding_per_student_year.csv")
school_average_output <- file.path(output_dir, "school_public_funding_per_student_2017_2021.csv")
diagnostics_output <- file.path(output_dir, "school_public_funding_per_student_diagnostics.csv")

finance_years <- 2017:2021
real_peso_base_year <- 2021L
early_growth_years <- 2017:2018
late_growth_years <- 2019:2021

cpi_index <- tibble::tribble(
  ~year, ~cpi_2010_100,
  2017L, 125.56654482907,
  2018L, 128.62395183831,
  2019L, 131.91356697484,
  2020L, 135.93098258439,
  2021L, 142.08127284455
) %>%
  mutate(
    cpi_base_year = cpi_2010_100[year == real_peso_base_year],
    inflation_adjustment_to_2021 = cpi_base_year / cpi_2010_100
  )

clean_names <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  make.unique(x, sep = "_")
}

as_number <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  readr::parse_number(
    as.character(x),
    locale = locale(decimal_mark = ",", grouping_mark = ".")
  )
}

find_subvenciones_file <- function(year) {
  year_dir <- file.path(finance_root, as.character(year), paste0("Subvenciones-a-EE-", year))

  if (!dir.exists(year_dir)) {
    stop("Missing Subvenciones directory for year ", year, ": ", year_dir, call. = FALSE)
  }

  preferred <- list.files(
    year_dir,
    pattern = paste0("Subvenciones.*", year, ".*\\.(csv|xlsx)$"),
    full.names = TRUE,
    ignore.case = TRUE
  )

  preferred <- preferred[!str_detect(basename(preferred), regex("diccionario|glosario|pdf", ignore_case = TRUE))]

  if (length(preferred) == 0) {
    preferred <- list.files(
      year_dir,
      pattern = "\\.(csv|xlsx)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    preferred <- preferred[!str_detect(basename(preferred), regex("diccionario|glosario|pdf", ignore_case = TRUE))]
  }

  if (length(preferred) == 0) {
    stop("No Subvenciones data file found for year ", year, call. = FALSE)
  }

  preferred[[1]]
}

read_subvenciones_file <- function(year) {
  path <- find_subvenciones_file(year)
  message("Reading ", year, ": ", path)

  if (str_detect(tolower(path), "\\.xlsx$")) {
    raw <- read_excel(path, sheet = 1)
  } else {
    raw <- read_delim(
      path,
      delim = ";",
      locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = "."),
      show_col_types = FALSE,
      name_repair = "minimal"
    )
  }

  names(raw) <- clean_names(names(raw))

  raw %>%
    mutate(source_year = year, source_file = path)
}

# Monetary subsidy/payment components available across 2017-2021.
# `liquido_pago` is kept as a diagnostic when available, but is not included in
# the component sum because it can duplicate other payment fields.
component_candidates <- c(
  "escolaridad",
  "internado",
  "monto_asig_zona",
  "montoasignacionzona",
  "zona",
  "descuento_ficom",
  "aporte_estado_ficom",
  "ruralidad",
  "piso_rural",
  "pag_pend",
  "discrepancia",
  "descuento_escolaridad",
  "decuento_escolaridad",
  "donacionaplicada",
  "reintegros",
  "retenciones",
  "multas",
  "desempeno_dificil",
  "desempeno_dificil_nodoc",
  "subv_adicional_especial",
  "subv_asistentes_educacion",
  "subvnodocente",
  "profesor_encargado",
  "profesorencargado",
  "reliquidacion",
  "apor_gratuidad",
  "subv_normal",
  "sub_normal",
  "sep",
  "sep_prio",
  "sep_pref",
  "mantenimiento",
  "sned",
  "avdi",
  "aep",
  "proretencion",
  "adeco",
  "reforzamiento",
  "brp"
)

# `escolaridad_pie` is intentionally excluded because it is already contained
# in `escolaridad`; including both would double-count PIE-related schooling
# payments.

standardize_subvenciones <- function(data) {
  year_col <- intersect(c("agno", "agnio", "ano", "a_o", "source_year"), names(data))[[1]]
  if (is.na(year_col) || !"rbd" %in% names(data) || !"mes" %in% names(data) || !"matricula" %in% names(data)) {
    stop("Subvenciones file is missing year/RBD/month/enrollment columns.", call. = FALSE)
  }

  component_cols <- intersect(component_candidates, names(data))

  data %>%
    mutate(
      year = as.integer(.data[[year_col]]),
      month = as.integer(as_number(mes)),
      school_rbd = as.numeric(as_number(rbd)),
      enrollment = as_number(matricula),
      reported_liquido_pago = if ("liquido_pago" %in% names(data)) as_number(liquido_pago) else NA_real_
    ) %>%
    mutate(across(all_of(component_cols), as_number)) %>%
    mutate(
      monthly_public_funding_components_pesos = rowSums(across(all_of(component_cols)), na.rm = TRUE),
      n_components_used = length(component_cols)
    ) %>%
    transmute(
      year,
      month,
      school_rbd,
      enrollment,
      monthly_public_funding_components_pesos,
      reported_liquido_pago,
      n_components_used,
      source_file
    ) %>%
    filter(!is.na(year), !is.na(month), !is.na(school_rbd))
}

monthly_funding <- bind_rows(
  lapply(finance_years, function(year) {
    read_subvenciones_file(year) %>%
      standardize_subvenciones()
  })
) %>%
  left_join(cpi_index, by = "year") %>%
  mutate(
    monthly_public_funding_components_2021_pesos =
      monthly_public_funding_components_pesos * inflation_adjustment_to_2021,
    reported_liquido_pago_2021_pesos =
      reported_liquido_pago * inflation_adjustment_to_2021
  )

school_year <- monthly_funding %>%
  group_by(school_rbd, year) %>%
  summarise(
    n_month_rows = n(),
    n_months_observed = n_distinct(month),
    avg_monthly_enrollment = mean(enrollment, na.rm = TRUE),
    total_public_funding_components_pesos = sum(monthly_public_funding_components_pesos, na.rm = TRUE),
    total_public_funding_components_2021_pesos = sum(monthly_public_funding_components_2021_pesos, na.rm = TRUE),
    total_reported_liquido_pago = if (all(is.na(reported_liquido_pago))) NA_real_ else sum(reported_liquido_pago, na.rm = TRUE),
    total_reported_liquido_pago_2021_pesos = if (all(is.na(reported_liquido_pago_2021_pesos))) NA_real_ else sum(reported_liquido_pago_2021_pesos, na.rm = TRUE),
    public_funding_per_student = total_public_funding_components_pesos / avg_monthly_enrollment,
    public_funding_per_student_2021_pesos = total_public_funding_components_2021_pesos / avg_monthly_enrollment,
    liquido_pago_per_student = total_reported_liquido_pago / avg_monthly_enrollment,
    liquido_pago_per_student_2021_pesos = total_reported_liquido_pago_2021_pesos / avg_monthly_enrollment,
    cpi_2010_100 = first(cpi_2010_100),
    inflation_adjustment_to_2021 = first(inflation_adjustment_to_2021),
    .groups = "drop"
  ) %>%
  mutate(
    public_funding_per_student = if_else(is.finite(public_funding_per_student), public_funding_per_student, NA_real_),
    public_funding_per_student_2021_pesos = if_else(is.finite(public_funding_per_student_2021_pesos), public_funding_per_student_2021_pesos, NA_real_),
    liquido_pago_per_student = if_else(is.finite(liquido_pago_per_student), liquido_pago_per_student, NA_real_),
    liquido_pago_per_student_2021_pesos = if_else(is.finite(liquido_pago_per_student_2021_pesos), liquido_pago_per_student_2021_pesos, NA_real_)
  )

school_average <- school_year %>%
  filter(!is.na(public_funding_per_student)) %>%
  group_by(school_rbd) %>%
  summarise(
    first_year_observed = min(year, na.rm = TRUE),
    last_year_observed = max(year, na.rm = TRUE),
    n_years_observed = n_distinct(year),
    mean_public_funding_per_student_2017_2021 = mean(public_funding_per_student, na.rm = TRUE),
    mean_public_funding_per_student_2021_pesos_2017_2021 = mean(public_funding_per_student_2021_pesos, na.rm = TRUE),
    median_public_funding_per_student_2017_2021 = median(public_funding_per_student, na.rm = TRUE),
    median_public_funding_per_student_2021_pesos_2017_2021 = median(public_funding_per_student_2021_pesos, na.rm = TRUE),
    enrollment_weighted_public_funding_per_student_2017_2021 =
      sum(total_public_funding_components_pesos, na.rm = TRUE) / sum(avg_monthly_enrollment, na.rm = TRUE),
    enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021 =
      sum(total_public_funding_components_2021_pesos, na.rm = TRUE) / sum(avg_monthly_enrollment, na.rm = TRUE),
    mean_avg_monthly_enrollment_2017_2021 = mean(avg_monthly_enrollment, na.rm = TRUE),
    total_public_funding_components_pesos_2017_2021 = sum(total_public_funding_components_pesos, na.rm = TRUE),
    total_public_funding_components_2021_pesos_2017_2021 = sum(total_public_funding_components_2021_pesos, na.rm = TRUE),
    mean_public_funding_per_student_2021_pesos_2017_2018 =
      mean(public_funding_per_student_2021_pesos[year %in% early_growth_years], na.rm = TRUE),
    mean_public_funding_per_student_2021_pesos_2019_2021 =
      mean(public_funding_per_student_2021_pesos[year %in% late_growth_years], na.rm = TRUE),
    change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018 =
      mean_public_funding_per_student_2021_pesos_2019_2021 -
      mean_public_funding_per_student_2021_pesos_2017_2018,
    pct_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018 =
      change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018 /
      mean_public_funding_per_student_2021_pesos_2017_2018,
    log_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018 =
      log(mean_public_funding_per_student_2021_pesos_2019_2021) -
      log(mean_public_funding_per_student_2021_pesos_2017_2018),
    has_early_growth_year = any(year %in% early_growth_years),
    has_late_growth_year = any(year %in% late_growth_years),
    .groups = "drop"
  ) %>%
  mutate(
    enrollment_weighted_public_funding_per_student_2017_2021 = if_else(
      is.finite(enrollment_weighted_public_funding_per_student_2017_2021),
      enrollment_weighted_public_funding_per_student_2017_2021,
      NA_real_
    ),
    enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021 = if_else(
      is.finite(enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021),
      enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021,
      NA_real_
    ),
    across(
      c(
        mean_public_funding_per_student_2021_pesos_2017_2018,
        mean_public_funding_per_student_2021_pesos_2019_2021,
        change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018,
        pct_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018,
        log_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018
      ),
      ~ if_else(is.finite(.x), .x, NA_real_)
    )
  )

diagnostics <- bind_rows(
  monthly_funding %>%
    group_by(year) %>%
    summarise(
      measure = "monthly_rows",
      value = n(),
      .groups = "drop"
    ),
  school_year %>%
    group_by(year) %>%
    summarise(
      measure = "school_year_rows",
      value = n(),
      .groups = "drop"
    ),
  school_year %>%
    group_by(year) %>%
    summarise(
      measure = "median_public_funding_per_student",
      value = median(public_funding_per_student, na.rm = TRUE),
      .groups = "drop"
    ),
  school_year %>%
    group_by(year) %>%
    summarise(
      measure = "median_public_funding_per_student_2021_pesos",
      value = median(public_funding_per_student_2021_pesos, na.rm = TRUE),
      .groups = "drop"
    ),
  school_year %>%
    group_by(year) %>%
    summarise(
      measure = "missing_public_funding_per_student",
      value = sum(is.na(public_funding_per_student)),
      .groups = "drop"
    ),
  cpi_index %>%
    transmute(
      year,
      measure = "cpi_2010_100",
      value = cpi_2010_100
    ),
  cpi_index %>%
    transmute(
      year,
      measure = "inflation_adjustment_to_2021",
      value = inflation_adjustment_to_2021
    ),
  tibble(
    year = NA_integer_,
    measure = "schools_with_early_and_late_growth_periods",
    value = sum(school_average$has_early_growth_year & school_average$has_late_growth_year, na.rm = TRUE)
  )
) %>%
  arrange(year, measure)

write_csv(school_year, school_year_output)
write_csv(school_average, school_average_output)
write_csv(diagnostics, diagnostics_output)

message("Wrote school-year file: ", school_year_output)
message("Wrote 2017-2021 school average file: ", school_average_output)
message("Wrote diagnostics: ", diagnostics_output)
