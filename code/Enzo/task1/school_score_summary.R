suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(rlang)
})

# The original assignment focuses on 2016-2018. If you want exactly that scope,
# change these year vectors before running the script.
legacy_years <- 2012:2020
modern_years <- 2021:2025

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  normalizePath("code/Enzo/task1/school_score_summary.R")
}

script_dir <- dirname(script_path)
project_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
output_dir <- script_dir

legacy_score_specs <- tibble::tribble(
  ~source,         ~prefix,          ~count_suffix,
  "LENG_ACTUAL",   "LENG_ACTUAL",    "LENG",
  "MATE_ACTUAL",   "MATE_ACTUAL",    "MATE",
  "HCSO_ACTUAL",   "HCSO_ACTUAL",    "HCSO",
  "CIEN_ACTUAL",   "CIEN_ACTUAL",    "CIEN",
  "PTJE_NEM",      "PTJE_NEM",       "PTJE_NEM"
)

modern_score_specs <- tibble::tribble(
  ~source,       ~prefix,          ~count_suffix,
  "CLEC_MAX",    "LENG_ACTUAL",    "LENG",
  "MATE1_MAX",   "MATE_ACTUAL",    "MATE",
  "MATE2_MAX",   "MATE2_MAX",      "MATE2",
  "HCSOC_MAX",   "HCSO_ACTUAL",    "HCSO",
  "CIEN_MAX",    "CIEN_ACTUAL",    "CIEN",
  "PTJE_NEM",    "PTJE_NEM",       "PTJE_NEM"
)

legacy_metadata <- c(
  GRUPO_DEPENDENCIA = "GRUPO_DEPENDENCIA",
  CODIGO_REGION = "CODIGO_REGION",
  CODIGO_COMUNA = "CODIGO_COMUNA"
)

modern_metadata <- c(
  GRUPO_DEPENDENCIA = "DEPENDENCIA",
  CODIGO_REGION = "CODIGO_REGION_EGRESO",
  CODIGO_COMUNA = "CODIGO_COMUNA_EGRESO"
)

first_non_missing <- function(x) {
  x <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_quantile <- function(x, prob) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

parse_score <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x <- as.character(x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(readr::parse_number(x, locale = locale(decimal_mark = ".")))
}

find_single_file <- function(year, pattern) {
  root <- file.path(project_root, "data", "raw", year)
  matches <- list.files(
    path = root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(matches) == 0) {
    stop(sprintf("No file found for year %s with pattern %s", year, pattern), call. = FALSE)
  }

  if (length(matches) > 1) {
    stop(
      sprintf(
        "Multiple files found for year %s with pattern %s:\n%s",
        year,
        pattern,
        paste(matches, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  matches[[1]]
}

build_summary_expressions <- function(score_specs) {
  out <- list()

  for (i in seq_len(nrow(score_specs))) {
    source <- score_specs$source[[i]]
    prefix <- score_specs$prefix[[i]]
    count_suffix <- score_specs$count_suffix[[i]]
    source_sym <- sym(source)

    out[[paste0("avg_", prefix)]] <- expr(safe_mean(!!source_sym))
    out[[paste0(prefix, "_p10")]] <- expr(safe_quantile(!!source_sym, 0.10))
    out[[paste0(prefix, "_p25")]] <- expr(safe_quantile(!!source_sym, 0.25))
    out[[paste0(prefix, "_MEDIAN")]] <- expr(safe_quantile(!!source_sym, 0.50))
    out[[paste0(prefix, "_p75")]] <- expr(safe_quantile(!!source_sym, 0.75))
    out[[paste0(prefix, "_p90")]] <- expr(safe_quantile(!!source_sym, 0.90))
    out[[paste0("n_Students_", count_suffix)]] <- expr(sum(!is.na(!!source_sym)))
  }

  out
}

build_metadata_expressions <- function(metadata_map) {
  out <- list()

  for (name in names(metadata_map)) {
    out[[name]] <- expr(first_non_missing(!!sym(metadata_map[[name]])))
  }

  out
}

warn_metadata_conflicts <- function(data, metadata_map, year_label) {
  metadata_cols <- unname(metadata_map)

  conflicts <- data %>%
    group_by(RBD) %>%
    summarise(
      across(
        all_of(metadata_cols),
        ~ n_distinct(.x[!is.na(.x) & trimws(as.character(.x)) != ""])
      ),
      .groups = "drop"
    ) %>%
    filter(if_any(all_of(metadata_cols), ~ .x > 1))

  if (nrow(conflicts) > 0) {
    warning(
      sprintf(
        "%s: %s schools have conflicting metadata values. The script keeps the first non-missing value per school.",
        year_label,
        nrow(conflicts)
      ),
      call. = FALSE
    )
  }
}

summarise_school_scores <- function(data, score_specs, metadata_map, year) {
  warn_metadata_conflicts(data, metadata_map, sprintf("Year %s", year))

  metadata_exprs <- build_metadata_expressions(metadata_map)
  score_exprs <- build_summary_expressions(score_specs)

  data %>%
    group_by(RBD) %>%
    summarise(
      !!!metadata_exprs,
      !!!score_exprs,
      n_Students_RBD = n(),
      .groups = "drop"
    ) %>%
    mutate(year = year, .before = 1L)
}

load_legacy_year <- function(year) {
  file_path <- find_single_file(year, sprintf("^ArchivoC_Adm%s\\.csv$", year))

  read_delim(
    file = file_path,
    delim = ";",
    show_col_types = FALSE,
    progress = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    filter(SITUACION_EGRESO == 1) %>%
    filter(!is.na(RBD), trimws(as.character(RBD)) != "", RBD != 0) %>%
    select(
      RBD,
      all_of(unname(legacy_metadata)),
      all_of(legacy_score_specs$source)
    ) %>%
    mutate(across(all_of(legacy_score_specs$source), parse_score))
}

load_modern_year <- function(year) {
  file_path <- find_single_file(year, "^A_INSCRITOS_PUNTAJES.*PUB_MRUN\\.csv$")

  read_delim(
    file = file_path,
    delim = ";",
    show_col_types = FALSE,
    progress = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    filter(RINDIO_PROCESO_ANTERIOR == 0) %>%
    filter(!is.na(RBD), trimws(as.character(RBD)) != "", RBD != 0) %>%
    select(
      RBD,
      all_of(unname(modern_metadata)),
      all_of(modern_score_specs$source)
    ) %>%
    mutate(across(all_of(modern_score_specs$source), parse_score))
}

make_year_summary <- function(data, score_specs, metadata_map, year, zeros_as_missing) {
  score_cols <- score_specs$source

  if (zeros_as_missing) {
    data <- data %>%
      mutate(across(all_of(score_cols), ~ dplyr::na_if(.x, 0)))
  }

  summarise_school_scores(
    data = data,
    score_specs = score_specs,
    metadata_map = metadata_map,
    year = year
  )
}

legacy_raw <- set_names(map(legacy_years, load_legacy_year), legacy_years)
modern_raw <- set_names(map(modern_years, load_modern_year), modern_years)

legacy_grouped <- map2_dfr(
  legacy_raw,
  as.integer(names(legacy_raw)),
  ~ make_year_summary(.x, legacy_score_specs, legacy_metadata, .y, zeros_as_missing = FALSE)
) %>%
  arrange(RBD, year)

legacy_grouped_na <- map2_dfr(
  legacy_raw,
  as.integer(names(legacy_raw)),
  ~ make_year_summary(.x, legacy_score_specs, legacy_metadata, .y, zeros_as_missing = TRUE)
) %>%
  arrange(RBD, year)

modern_grouped <- map2_dfr(
  modern_raw,
  as.integer(names(modern_raw)),
  ~ make_year_summary(.x, modern_score_specs, modern_metadata, .y, zeros_as_missing = FALSE)
) %>%
  arrange(RBD, year)

modern_grouped_na <- map2_dfr(
  modern_raw,
  as.integer(names(modern_raw)),
  ~ make_year_summary(.x, modern_score_specs, modern_metadata, .y, zeros_as_missing = TRUE)
) %>%
  arrange(RBD, year)

write_csv(legacy_grouped, file.path(output_dir, "Adm_combined_grouped_2012_2020.csv"))
write_csv(legacy_grouped_na, file.path(output_dir, "Adm_combined_grouped_na_2012_2020.csv"))
write_csv(modern_grouped, file.path(output_dir, "Adm_combined_21_25.csv"))
write_csv(modern_grouped_na, file.path(output_dir, "Adm_combined_na_21_25.csv"))
