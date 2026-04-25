suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(rlang)
})

years <- 2016:2018

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else {
  normalizePath("code/Enzo/task1/psu_school_summary_2016_2018.R")
}

script_dir <- dirname(script_path)
project_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
output_dir <- script_dir

score_specs <- tibble::tribble(
  ~source,         ~prefix,          ~count_suffix,
  "LENG_ACTUAL",   "LENG_ACTUAL",    "LENG",
  "MATE_ACTUAL",   "MATE_ACTUAL",    "MATE",
  "HCSO_ACTUAL",   "HCSO_ACTUAL",    "HCSO",
  "CIEN_ACTUAL",   "CIEN_ACTUAL",    "CIEN",
  "PTJE_NEM",      "PTJE_NEM",       "PTJE_NEM"
)

metadata_map <- c(
  GRUPO_DEPENDENCIA = "GRUPO_DEPENDENCIA",
  CODIGO_REGION = "CODIGO_REGION",
  CODIGO_COMUNA = "CODIGO_COMUNA"
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

find_psu_file <- function(year) {
  matches <- list.files(
    path = file.path(project_root, "data", "raw", year),
    pattern = sprintf("^ArchivoC_Adm%s\\.csv$", year),
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(matches) != 1) {
    stop(
      sprintf(
        "Expected exactly one ArchivoC file for %s, found %s.",
        year,
        length(matches)
      ),
      call. = FALSE
    )
  }

  matches[[1]]
}

build_summary_expressions <- function() {
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

build_metadata_expressions <- function() {
  out <- list()

  for (name in names(metadata_map)) {
    out[[name]] <- expr(first_non_missing(!!sym(metadata_map[[name]])))
  }

  out
}

warn_metadata_conflicts <- function(data, year) {
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
        "Year %s: %s schools have conflicting metadata values. Keeping the first non-missing value per school.",
        year,
        nrow(conflicts)
      ),
      call. = FALSE
    )
  }
}

load_year <- function(year) {
  read_delim(
    file = find_psu_file(year),
    delim = ";",
    show_col_types = FALSE,
    progress = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    filter(SITUACION_EGRESO == 1) %>%
    filter(!is.na(RBD), trimws(as.character(RBD)) != "", RBD != 0) %>%
    select(
      RBD,
      all_of(unname(metadata_map)),
      all_of(score_specs$source)
    )
}

summarise_year <- function(data, year) {
  warn_metadata_conflicts(data, year)

  data %>%
    mutate(across(all_of(score_specs$source), ~ na_if(.x, 0))) %>%
    group_by(RBD) %>%
    summarise(
      !!!build_metadata_expressions(),
      !!!build_summary_expressions(),
      n_Students_RBD = n(),
      .groups = "drop"
    ) %>%
    mutate(year = year, .before = 1L)
}

yearly_raw <- map_dfr(
  years,
  \(year) load_year(year) %>% mutate(year = year, .before = 1L)
)

psu_summary <- yearly_raw %>%
  split(.$year) %>%
  imap_dfr(\(data, year) summarise_year(data, as.integer(year))) %>%
  arrange(RBD, year)

rbd_year_lookup <- yearly_raw %>%
  distinct(year, RBD)

rbd_overlap <- tidyr::expand_grid(
  year_from = years,
  year_to = years
) %>%
  rowwise() %>%
  mutate(
    n_rbd_from = sum(rbd_year_lookup$year == year_from),
    n_rbd_to = sum(rbd_year_lookup$year == year_to),
    n_common_rbd = {
      from_rbd <- rbd_year_lookup$RBD[rbd_year_lookup$year == year_from]
      to_rbd <- rbd_year_lookup$RBD[rbd_year_lookup$year == year_to]
      length(intersect(from_rbd, to_rbd))
    },
    share_from_found_in_to = n_common_rbd / n_rbd_from,
    share_to_found_in_from = n_common_rbd / n_rbd_to
  ) %>%
  ungroup()

rbd_metadata_by_year <- yearly_raw %>%
  group_by(year, RBD) %>%
  summarise(
    !!!build_metadata_expressions(),
    .groups = "drop"
  )

rbd_metadata_changes <- rbd_metadata_by_year %>%
  group_by(RBD) %>%
  summarise(
    years_observed = paste(sort(unique(year)), collapse = ","),
    n_years = n_distinct(year),
    n_dependency_values = n_distinct(GRUPO_DEPENDENCIA[!is.na(GRUPO_DEPENDENCIA)]),
    n_region_values = n_distinct(CODIGO_REGION[!is.na(CODIGO_REGION)]),
    n_comuna_values = n_distinct(CODIGO_COMUNA[!is.na(CODIGO_COMUNA)]),
    dependency_values = paste(sort(unique(GRUPO_DEPENDENCIA[!is.na(GRUPO_DEPENDENCIA)])), collapse = ","),
    region_values = paste(sort(unique(CODIGO_REGION[!is.na(CODIGO_REGION)])), collapse = ","),
    comuna_values = paste(sort(unique(CODIGO_COMUNA[!is.na(CODIGO_COMUNA)])), collapse = ","),
    .groups = "drop"
  ) %>%
  filter(
    n_dependency_values > 1 |
      n_region_values > 1 |
      n_comuna_values > 1
  ) %>%
  arrange(RBD)

metadata_key_collisions <- rbd_metadata_by_year %>%
  group_by(year, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA) %>%
  summarise(
    n_rbd = n_distinct(RBD),
    rbd_list = paste(sort(unique(RBD)), collapse = ","),
    .groups = "drop"
  ) %>%
  filter(n_rbd > 1) %>%
  arrange(year, desc(n_rbd))

write_csv(psu_summary, file.path(output_dir, "psu_school_summary_2016_2018.csv"))
write_csv(rbd_overlap, file.path(output_dir, "psu_school_rbd_overlap_2016_2018.csv"))
write_csv(rbd_metadata_changes, file.path(output_dir, "psu_school_rbd_metadata_changes_2016_2018.csv"))
write_csv(metadata_key_collisions, file.path(output_dir, "psu_school_rbd_metadata_collisions_2016_2018.csv"))
