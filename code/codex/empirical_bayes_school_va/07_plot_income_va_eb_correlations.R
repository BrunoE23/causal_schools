###############################################################################
# Income x attended EB VA correlations by cohort
#
# This descriptive access/equity plot correlates grade-4 income measures with
# the EB-shrunken VA of the high school a student attended.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

required_file <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
  path
}

has_sit_fin <- function(x) {
  !is.na(x) & trimws(as.character(x)) != ""
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
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

clean_dir <- file.path(data_wd, "data", "clean")
raw_tracking_dir <- file.path(data_wd, "data", "raw", "student_tracking")
school_directory_root <- file.path(data_wd, "data", "raw", "school_directory")
table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
figure_dir <- file.path(repo_wd, "output", "figures", "empirical_bayes_school_va")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
simce_4to_path <- file.path(clean_dir, "simce_4to.Rdata")
school_values_path <- Sys.getenv(
  "EB_SCHOOL_VALUES_INPUT_PATH",
  unset = file.path(table_dir, "stata_eb_school_rbd_observational_values_for_iv.csv")
)

correlation_csv <- file.path(table_dir, "income_va_eb_correlation_by_cohort.csv")
income_coverage_csv <- file.path(table_dir, "income_va_eb_income_proxy_coverage.csv")
plot_path <- file.path(figure_dir, "income_va_eb_correlation_by_cohort.png")
percentile_csv <- file.path(table_dir, "income_va_eb_by_income_percentile.csv")
percentile_plot_path <- file.path(figure_dir, "income_va_eb_by_income_percentile.png")
private_paid_exclusion_csv <- file.path(
  table_dir,
  "income_va_eb_no_private_paid_sample_coverage.csv"
)
correlation_no_private_paid_csv <- file.path(
  table_dir,
  "income_va_eb_correlation_by_cohort_no_private_paid.csv"
)
correlation_no_private_paid_plot_path <- file.path(
  figure_dir,
  "income_va_eb_correlation_by_cohort_no_private_paid.png"
)
percentile_no_private_paid_csv <- file.path(
  table_dir,
  "income_va_eb_by_income_percentile_no_private_paid.csv"
)
percentile_no_private_paid_plot_path <- file.path(
  figure_dir,
  "income_va_eb_by_income_percentile_no_private_paid.png"
)
percentile_by_cohort_csv <- file.path(
  table_dir,
  "income_va_eb_by_income_percentile_by_cohort.csv"
)
percentile_by_cohort_plot_path <- file.path(
  figure_dir,
  "income_va_eb_by_income_percentile_by_cohort.png"
)
percentile_by_cohort_no_private_paid_csv <- file.path(
  table_dir,
  "income_va_eb_by_income_percentile_by_cohort_no_private_paid.csv"
)
percentile_by_cohort_no_private_paid_plot_path <- file.path(
  figure_dir,
  "income_va_eb_by_income_percentile_by_cohort_no_private_paid.png"
)

income_median_min_n <- 15L
income_percentile_targets <- c(10L, 25L, 50L, 75L, 90L)
income_percentile_window_half_width <- 0.025

outcome_specs <- data.table(
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "admission_exam_taker",
    "higher_ed_enrolled_m1",
    "stem_enrollment_m1",
    "log_program_income_clp_m1",
    "program_certified_years_m1",
    "inst_certified_years_m1"
  ),
  outcome_key = c(
    "math",
    "language",
    "exam_taking",
    "higher_ed_enrollment",
    "stem_enrollment",
    "program_income",
    "program_certified_years",
    "institutional_quality"
  ),
  outcome_label = c(
    "Math",
    "Language",
    "Exam taking",
    "Higher-ed enrollment",
    "STEM enrollment",
    "Program income",
    "Program-certified years",
    "Institutional quality"
  ),
  plot_order = seq_len(8L)
)

message("Reading grade-4 SIMCE income donors: ", simce_4to_path)
simce_env <- new.env(parent = emptyenv())
load(required_file(simce_4to_path, "Grade-4 SIMCE file"), envir = simce_env)
if (!exists("simce_4to", envir = simce_env)) {
  stop("Grade-4 SIMCE file does not contain object simce_4to.", call. = FALSE)
}
simce_4to <- as.data.table(get("simce_4to", envir = simce_env))
rm(simce_env)

required_simce_cols <- c("simce_rbd_4to", "income_mid")
missing_simce_cols <- setdiff(required_simce_cols, names(simce_4to))
if (length(missing_simce_cols) > 0) {
  stop(
    "simce_4to is missing required columns: ",
    paste(missing_simce_cols, collapse = ", "),
    call. = FALSE
  )
}

simce_4to[, `:=`(
  simce_rbd_4to = as.numeric(simce_rbd_4to),
  income_mid = as.numeric(income_mid)
)]
simce_4to[!is.finite(income_mid) | income_mid <= 0, income_mid := NA_real_]

school_income <- simce_4to[
  !is.na(simce_rbd_4to) & !is.na(income_mid),
  .(
    income_school_median = stats::median(income_mid, na.rm = TRUE),
    income_school_n = .N
  ),
  by = simce_rbd_4to
][income_school_n >= income_median_min_n]

message("School-income donor schools with n >= ", income_median_min_n, ": ", nrow(school_income))

message("Reading current student universe: ", universe_path)
current_students <- fread(
  required_file(universe_path, "Student universe"),
  select = c(
    "mrun",
    "cohort_gr8",
    "most_time_RBD",
    "income_mid",
    "simce_rbd_4to",
    "simce_year"
  ),
  na.strings = c("", "NA")
)

current_students[, `:=`(
  mrun = as.numeric(mrun),
  MRUN = as.character(mrun),
  cohort_gr8 = as.integer(cohort_gr8),
  most_time_RBD = as.numeric(most_time_RBD),
  income_mid = as.numeric(income_mid),
  simce_rbd_4to = as.numeric(simce_rbd_4to),
  simce_year = as.integer(simce_year),
  cohort_frame_source = "univ_gr8_df",
  grade4_tracking_year = NA_integer_
)]
current_students[!is.finite(income_mid) | income_mid <= 0, income_mid := NA_real_]
current_students <- current_students[cohort_gr8 %between% c(2017L, 2020L)]

find_rar_extractor <- function() {
  candidates <- c(
    Sys.getenv("RAR_EXTRACTOR"),
    unname(Sys.which(c("7z", "7za", "unrar", "rar"))),
    "C:/Program Files/7-Zip/7z.exe",
    "C:/Program Files (x86)/7-Zip/7z.exe"
  )
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[[1]]
}

run_archive_extract <- function(archive_path, out_dir) {
  extractor <- find_rar_extractor()
  if (is.na(extractor)) {
    stop(
      "No RAR extractor found. Install 7-Zip or set RAR_EXTRACTOR, then rerun. ",
      "Needed archive: ", archive_path,
      call. = FALSE
    )
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tool <- tolower(basename(extractor))
  if (tool %in% c("7z.exe", "7za.exe", "7z", "7za")) {
    args <- c("x", "-y", paste0("-o", normalizePath(out_dir, winslash = "/", mustWork = FALSE)), archive_path)
  } else {
    args <- c("x", "-o+", archive_path, normalizePath(out_dir, winslash = "/", mustWork = FALSE))
  }

  message("Extracting ", archive_path, " with ", extractor)
  result <- system2(extractor, args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "Archive extraction failed for ",
      archive_path,
      ":\n",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }
}

choose_tracking_csv <- function(csv_files, year) {
  if (length(csv_files) == 0) {
    return(NA_character_)
  }

  lower_name <- tolower(basename(csv_files))
  keep <- !grepl("frecuencia|glosa|diccionario|codigo|codebook", lower_name)
  candidates <- csv_files[keep]
  if (length(candidates) == 0) {
    candidates <- csv_files
  }

  if (length(candidates) == 1) {
    return(candidates[[1]])
  }

  preferred <- grepl("rendimiento", tolower(basename(candidates))) &
    grepl(as.character(year), basename(candidates))
  if (sum(preferred) == 1) {
    return(candidates[preferred][[1]])
  }

  sizes <- file.info(candidates)$size
  candidates[which.max(sizes)]
}

tracking_csv_for_year <- function(year) {
  raw_folder <- file.path(raw_tracking_dir, year, paste0("Rendimiento-", year))
  if (dir.exists(raw_folder)) {
    csv_files <- list.files(raw_folder, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
    selected <- choose_tracking_csv(csv_files, year)
    if (!is.na(selected)) {
      return(selected)
    }
  }

  archive_path <- file.path(raw_tracking_dir, year, paste0("Rendimiento-", year, ".rar"))
  required_file(archive_path, paste0("Raw tracking archive for ", year))
  run_archive_extract(archive_path, raw_folder)

  csv_files <- list.files(raw_folder, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)
  selected <- choose_tracking_csv(csv_files, year)
  if (is.na(selected)) {
    stop("No tracking CSV found after extracting archive for ", year, ".", call. = FALSE)
  }
  selected
}

read_tracking_year <- function(year, keep_mruns = NULL) {
  csv_file <- tracking_csv_for_year(year)
  header <- names(fread(csv_file, sep = ";", nrows = 0, encoding = "Latin-1"))
  header_upper <- toupper(header)
  needed <- c("AGNO", "RBD", "COD_ENSE", "COD_GRADO", "MRUN", "ASISTENCIA", "SIT_FIN")
  selected_original <- header[match(needed, header_upper)]
  missing_cols <- needed[is.na(selected_original)]
  if (length(missing_cols) > 0) {
    stop(
      "Tracking file for ",
      year,
      " is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  message("Reading tracking year ", year, ": ", csv_file)
  dt <- fread(
    csv_file,
    sep = ";",
    select = selected_original,
    encoding = "Latin-1",
    na.strings = c("", "NA"),
    showProgress = FALSE
  )
  setnames(dt, names(dt), toupper(names(dt)))

  dt[, `:=`(
    AGNO = as.integer(AGNO),
    RBD = as.numeric(RBD),
    COD_ENSE = as.numeric(COD_ENSE),
    COD_GRADO = as.numeric(COD_GRADO),
    MRUN = as.character(MRUN),
    ASISTENCIA = as.numeric(ASISTENCIA),
    SIT_FIN = as.character(SIT_FIN)
  )]
  dt <- dt[!is.na(MRUN) & MRUN != ""]
  if (!is.null(keep_mruns)) {
    dt <- dt[MRUN %chin% keep_mruns]
  }
  dt
}

resolve_student_year <- function(dt) {
  dt <- copy(dt)
  dt[, nonzero_att := fifelse(!is.na(ASISTENCIA) & ASISTENCIA > 0, 1L, 0L)]
  dt[, passed := fifelse(has_sit_fin(SIT_FIN) & SIT_FIN == "P", 1L, 0L)]
  setorder(dt, MRUN, AGNO, -nonzero_att, -passed, -ASISTENCIA, RBD)
  dt <- dt[, .SD[1L], by = .(MRUN, AGNO)]
  dt[, c("nonzero_att", "passed") := NULL]
  dt
}

read_private_paid_schools <- function(root) {
  if (!dir.exists(root)) {
    warning("School directory root does not exist. Private-paid exclusion will be empty: ", root)
    return(data.table(
      school_rbd = numeric(),
      attended_private_paid = logical(),
      private_paid_directory_years = integer(),
      directory_years_observed = integer()
    ))
  }

  directory_files <- list.files(
    root,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(directory_files) == 0) {
    warning("No school directory CSVs found. Private-paid exclusion will be empty: ", root)
    return(data.table(
      school_rbd = numeric(),
      attended_private_paid = logical(),
      private_paid_directory_years = integer(),
      directory_years_observed = integer()
    ))
  }

  directory_year <- as.integer(regmatches(
    directory_files,
    regexpr("\\b20[0-9]{2}\\b", directory_files)
  ))

  metadata <- rbindlist(
    lapply(seq_along(directory_files), function(i) {
      directory_file <- directory_files[[i]]
      header <- names(fread(
        directory_file,
        nrows = 0,
        encoding = "Latin-1",
        showProgress = FALSE
      ))
      header_upper <- toupper(trimws(header))
      rbd_col <- header[match("RBD", header_upper)]
      cod_depe_col <- header[match("COD_DEPE", header_upper)]
      cod_depe2_col <- header[match("COD_DEPE2", header_upper)]

      if (is.na(rbd_col)) {
        warning("School directory has no RBD column and will be skipped: ", directory_file)
        return(data.table())
      }

      selected_cols <- c(
        rbd_col,
        cod_depe_col[!is.na(cod_depe_col)],
        cod_depe2_col[!is.na(cod_depe2_col)]
      )
      dt <- fread(
        directory_file,
        select = selected_cols,
        encoding = "Latin-1",
        showProgress = FALSE
      )
      setnames(dt, names(dt), toupper(trimws(names(dt))))

      if (!"COD_DEPE" %in% names(dt)) {
        dt[, COD_DEPE := NA_character_]
      }
      if (!"COD_DEPE2" %in% names(dt)) {
        dt[, COD_DEPE2 := NA_character_]
      }

      dt[, .(
        school_rbd = as.numeric(RBD),
        directory_year = directory_year[[i]],
        private_paid = as.character(COD_DEPE) == "4" |
          as.character(COD_DEPE2) == "3"
      )]
    }),
    use.names = TRUE,
    fill = TRUE
  )

  metadata <- metadata[!is.na(school_rbd)]
  if (nrow(metadata) == 0) {
    warning("No valid RBD rows found in school directory files.")
    return(data.table(
      school_rbd = numeric(),
      attended_private_paid = logical(),
      private_paid_directory_years = integer(),
      directory_years_observed = integer()
    ))
  }

  metadata[, .(
    attended_private_paid = any(private_paid, na.rm = TRUE),
    private_paid_directory_years = sum(private_paid, na.rm = TRUE),
    directory_years_observed = .N,
    directory_year_min = min(directory_year, na.rm = TRUE),
    directory_year_max = max(directory_year, na.rm = TRUE)
  ), by = school_rbd]
}

construct_historical_students <- function() {
  message("Constructing historical grade-8 cohorts and school links.")

  grade8 <- rbindlist(
    lapply(2009:2016, function(year) {
      read_tracking_year(year)[
        COD_GRADO == 8 &
          COD_ENSE == 110 &
          has_sit_fin(SIT_FIN) &
          !is.na(RBD)
      ]
    }),
    use.names = TRUE,
    fill = TRUE
  )
  grade8 <- resolve_student_year(grade8)
  historical_cohorts <- grade8[
    ,
    .(cohort_gr8 = min(AGNO, na.rm = TRUE)),
    by = MRUN
  ][cohort_gr8 %between% c(2013L, 2016L)]

  if (nrow(historical_cohorts) == 0) {
    warning("No historical cohorts 2013-2016 were constructed.")
    return(data.table())
  }

  historical_mruns <- historical_cohorts$MRUN

  grade4 <- rbindlist(
    lapply(2009:2012, function(year) {
      read_tracking_year(year, keep_mruns = historical_mruns)[
        COD_GRADO == 4 &
          has_sit_fin(SIT_FIN) &
          !is.na(RBD)
      ]
    }),
    use.names = TRUE,
    fill = TRUE
  )
  grade4 <- merge(grade4, historical_cohorts, by = "MRUN", all.x = FALSE, sort = FALSE)
  grade4 <- grade4[AGNO == cohort_gr8 - 4L]
  grade4 <- resolve_student_year(grade4)
  grade4 <- grade4[, .(
    MRUN,
    simce_rbd_4to = RBD,
    grade4_tracking_year = as.integer(AGNO)
  )]

  post_tracking <- rbindlist(
    lapply(2014:2020, function(year) {
      read_tracking_year(year, keep_mruns = historical_mruns)[
        has_sit_fin(SIT_FIN) &
          !is.na(RBD)
      ]
    }),
    use.names = TRUE,
    fill = TRUE
  )
  post_tracking <- merge(
    post_tracking,
    historical_cohorts,
    by = "MRUN",
    all.x = FALSE,
    sort = FALSE
  )
  post_tracking <- post_tracking[
    AGNO > cohort_gr8 &
      AGNO <= cohort_gr8 + 4L
  ]
  post_tracking <- resolve_student_year(post_tracking)
  post_tracking[, year_rel_cohort8 := AGNO - cohort_gr8]

  most_time <- post_tracking[
    ,
    .(
      n_years = .N,
      last_year = max(year_rel_cohort8, na.rm = TRUE)
    ),
    by = .(MRUN, RBD)
  ]
  setorder(most_time, MRUN, -n_years, -last_year, RBD)
  most_time <- most_time[, .SD[1L], by = MRUN]
  most_time <- most_time[, .(MRUN, most_time_RBD = RBD)]

  historical_students <- merge(
    historical_cohorts,
    grade4,
    by = "MRUN",
    all.x = TRUE,
    sort = FALSE
  )
  historical_students <- merge(
    historical_students,
    most_time,
    by = "MRUN",
    all.x = TRUE,
    sort = FALSE
  )

  historical_students[, `:=`(
    mrun = as.numeric(MRUN),
    income_mid = NA_real_,
    simce_year = as.integer(cohort_gr8 - 4L),
    cohort_frame_source = "historical_tracking"
  )]

  historical_students[, .(
    mrun,
    MRUN,
    cohort_gr8,
    most_time_RBD,
    income_mid,
    simce_rbd_4to,
    simce_year,
    cohort_frame_source,
    grade4_tracking_year
  )]
}

historical_students <- construct_historical_students()

student_universe <- rbindlist(
  list(
    current_students[, .(
      mrun,
      MRUN,
      cohort_gr8,
      most_time_RBD,
      income_mid,
      simce_rbd_4to,
      simce_year,
      cohort_frame_source,
      grade4_tracking_year
    )],
    historical_students
  ),
  use.names = TRUE,
  fill = TRUE
)

student_universe <- merge(
  student_universe,
  school_income,
  by = "simce_rbd_4to",
  all.x = TRUE,
  sort = FALSE
)

student_universe[, income_4to_direct_or_school_median := fcase(
  !is.na(income_mid), income_mid,
  is.na(income_mid) & !is.na(income_school_median), income_school_median,
  default = NA_real_
)]
student_universe[, income_proxy_source := fcase(
  !is.na(income_mid), "direct_income_mid",
  is.na(income_mid) & !is.na(income_school_median), "fallback_simce_rbd_4to",
  default = "missing"
)]
student_universe[, log_income_direct_or_school_median := log(income_4to_direct_or_school_median)]
student_universe[, income_4to_school_median_for_everyone := income_school_median]
student_universe[, log_income_school_median := log(income_4to_school_median_for_everyone)]

income_coverage <- student_universe[!is.na(cohort_gr8), .(
  n_students = .N,
  n_from_univ_gr8_df = sum(cohort_frame_source == "univ_gr8_df"),
  n_from_historical_tracking = sum(cohort_frame_source == "historical_tracking"),
  n_valid_attended_rbd = sum(!is.na(most_time_RBD) & most_time_RBD > 0),
  n_grade4_rbd_available = sum(!is.na(simce_rbd_4to)),
  n_grade4_rbd_from_historical_tracking = sum(
    cohort_frame_source == "historical_tracking" & !is.na(simce_rbd_4to)
  ),
  n_direct_income_mid = sum(!is.na(income_mid)),
  n_school_median_available = sum(!is.na(income_4to_school_median_for_everyone)),
  n_direct_or_school_median = sum(!is.na(income_4to_direct_or_school_median)),
  n_income_missing = sum(is.na(income_4to_direct_or_school_median)),
  share_valid_attended_rbd = mean(!is.na(most_time_RBD) & most_time_RBD > 0),
  share_grade4_rbd_available = mean(!is.na(simce_rbd_4to)),
  share_direct_income_mid = mean(!is.na(income_mid)),
  share_school_median_available = mean(!is.na(income_4to_school_median_for_everyone)),
  share_direct_or_school_median = mean(!is.na(income_4to_direct_or_school_median))
), by = cohort_gr8]
setorder(income_coverage, cohort_gr8)

message("Writing income proxy coverage: ", income_coverage_csv)
fwrite(income_coverage, income_coverage_csv)

message("Reading EB school values: ", school_values_path)
school_values <- fread(
  required_file(school_values_path, "EB school values"),
  select = c(
    "school_rbd",
    "analysis_sample",
    "outcome",
    "controlled_value_added_eb_centered_student"
  ),
  na.strings = c("", "NA")
)
school_values <- school_values[
  analysis_sample == "All" &
    outcome %chin% outcome_specs$outcome,
  .(
    most_time_RBD = as.numeric(school_rbd),
    outcome,
    attended_va_eb = as.numeric(controlled_value_added_eb_centered_student)
  )
]
school_values <- merge(
  school_values,
  outcome_specs,
  by = "outcome",
  all.x = TRUE,
  sort = FALSE
)

if (anyDuplicated(school_values[, .(most_time_RBD, outcome)]) > 0) {
  stop("EB school values have duplicate school-outcome rows.", call. = FALSE)
}

base_students_main <- student_universe[
  cohort_gr8 %between% c(2017L, 2020L) &
    !is.na(most_time_RBD) &
    most_time_RBD > 0 &
    is.finite(log_income_direct_or_school_median),
  .(
    mrun,
    cohort_gr8,
    most_time_RBD,
    income_measure_key = "direct_or_school_median",
    income_measure_label = "Direct income + school median fallback",
    income_measure_order = 1L,
    income_4to_for_correlation = income_4to_direct_or_school_median,
    log_income_4to_for_correlation = log_income_direct_or_school_median,
    income_proxy_source
  )
]

base_students_school_median <- student_universe[
  cohort_gr8 %between% c(2013L, 2020L) &
    !is.na(most_time_RBD) &
    most_time_RBD > 0 &
    is.finite(log_income_school_median),
  .(
    mrun,
    cohort_gr8,
    most_time_RBD,
    income_measure_key = "school_median_for_everyone",
    income_measure_label = "School median for everyone",
    income_measure_order = 2L,
    income_4to_for_correlation = income_4to_school_median_for_everyone,
    log_income_4to_for_correlation = log_income_school_median,
    income_proxy_source = "school_median_for_everyone"
  )
]

base_students <- rbindlist(
  list(base_students_main, base_students_school_median),
  use.names = TRUE
)

message("Reading school-directory private-paid flags: ", school_directory_root)
private_paid_schools <- read_private_paid_schools(school_directory_root)
message("Private-paid schools identified across directories: ", private_paid_schools[
  attended_private_paid == TRUE,
  .N
])

base_students <- merge(
  base_students,
  private_paid_schools[, .(
    most_time_RBD = school_rbd,
    attended_private_paid,
    private_paid_directory_years,
    directory_years_observed,
    directory_year_min,
    directory_year_max
  )],
  by = "most_time_RBD",
  all.x = TRUE,
  sort = FALSE
)
base_students[, private_paid_metadata_match := !is.na(attended_private_paid)]
base_students[is.na(attended_private_paid), attended_private_paid := FALSE]

private_paid_exclusion <- base_students[, .(
  n_students_with_income_and_rbd = .N,
  n_with_school_directory_match = sum(private_paid_metadata_match),
  n_without_school_directory_match = sum(!private_paid_metadata_match),
  n_attended_private_paid = sum(attended_private_paid),
  n_kept_no_private_paid = sum(!attended_private_paid),
  share_with_school_directory_match = mean(private_paid_metadata_match),
  share_attended_private_paid = mean(attended_private_paid),
  share_kept_no_private_paid = mean(!attended_private_paid)
), by = .(
  cohort_gr8,
  income_measure_key,
  income_measure_label,
  income_measure_order
)]
setorder(private_paid_exclusion, income_measure_order, cohort_gr8)

message("Writing no-private-paid sample coverage: ", private_paid_exclusion_csv)
fwrite(private_paid_exclusion, private_paid_exclusion_csv)

base_students_no_private_paid <- base_students[attended_private_paid == FALSE]

base_counts <- base_students[, .(
  n_students_with_income_and_rbd = .N
), by = .(cohort_gr8, income_measure_key, income_measure_label, income_measure_order)]

panel <- merge(
  base_students,
  school_values,
  by = "most_time_RBD",
  all.x = FALSE,
  allow.cartesian = TRUE,
  sort = FALSE
)
panel <- panel[is.finite(attended_va_eb)]

correlation_dt <- panel[, {
  n <- .N
  rho <- if (n >= 2L) {
    stats::cor(log_income_4to_for_correlation, attended_va_eb)
  } else {
    NA_real_
  }
  .(
    rho = rho,
    n_students = n,
    mean_log_income_4to = mean(log_income_4to_for_correlation),
    mean_attended_va_eb = mean(attended_va_eb),
    sd_log_income_4to = stats::sd(log_income_4to_for_correlation),
    sd_attended_va_eb = stats::sd(attended_va_eb)
  )
}, by = .(
  cohort_gr8,
  income_measure_key,
  income_measure_label,
  income_measure_order,
  outcome,
  outcome_key,
  outcome_label,
  plot_order
)]
correlation_dt <- merge(
  correlation_dt,
  base_counts,
  by = c("cohort_gr8", "income_measure_key", "income_measure_label", "income_measure_order"),
  all.x = TRUE,
  sort = FALSE
)
correlation_dt[, va_match_rate_among_income_rbd := n_students / n_students_with_income_and_rbd]
setorder(correlation_dt, plot_order, income_measure_order, cohort_gr8)

message("Writing cohort correlations: ", correlation_csv)
fwrite(correlation_dt, correlation_csv)

if (nrow(correlation_dt[is.finite(rho)]) == 0) {
  stop("No finite cohort correlations were computed.", call. = FALSE)
}

plot_dt <- copy(correlation_dt[is.finite(rho)])
plot_dt[, outcome_label := factor(
  outcome_label,
  levels = outcome_specs[order(plot_order), outcome_label]
)]
plot_dt[, income_measure_label := factor(
  income_measure_label,
  levels = c(
    "Direct income + school median fallback",
    "School median for everyone"
  )
)]

plot <- ggplot(
  plot_dt,
  aes(
    x = cohort_gr8,
    y = rho,
    color = outcome_label,
    linetype = income_measure_label,
    group = interaction(outcome_label, income_measure_label)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "grey55") +
  geom_line(linewidth = 0.85, alpha = 0.9) +
  geom_point(aes(shape = income_measure_label), size = 2.2) +
  scale_x_continuous(
    breaks = sort(unique(plot_dt$cohort_gr8)),
    minor_breaks = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Direct income + school median fallback" = "solid",
      "School median for everyone" = "dashed"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Direct income + school median fallback" = 16,
      "School median for everyone" = 1
    )
  ) +
  labs(
    title = "Correlation Between Grade-4 Income and Attended High School EB VA",
    x = "Grade-8 cohort",
    y = expression(rho),
    color = "Outcome",
    linetype = "Income measure",
    shape = "Income measure"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

message("Writing figure: ", plot_path)
ggsave(
  filename = plot_path,
  plot = plot,
  width = 12.5,
  height = 7,
  dpi = 300
)

ranked_panel <- copy(panel)
setorder(
  ranked_panel,
  income_measure_order,
  outcome,
  log_income_4to_for_correlation,
  mrun
)
ranked_panel[, income_rank_share := seq_len(.N) / .N, by = .(
  income_measure_key,
  outcome
)]

percentile_dt <- ranked_panel[, {
  group_dt <- .SD
  rbindlist(lapply(income_percentile_targets, function(target_percentile) {
    target_share <- target_percentile / 100
    window_lower <- max(0, target_share - income_percentile_window_half_width)
    window_upper <- min(1, target_share + income_percentile_window_half_width)
    window_dt <- group_dt[
      income_rank_share >= window_lower &
        income_rank_share <= window_upper
    ]

    .(
      target_percentile = target_percentile,
      target_percentile_label = paste0("p", target_percentile),
      window_lower_percentile = 100 * window_lower,
      window_upper_percentile = 100 * window_upper,
      n_students_total = nrow(group_dt),
      n_students = nrow(window_dt),
      cohort_min = min(window_dt$cohort_gr8, na.rm = TRUE),
      cohort_max = max(window_dt$cohort_gr8, na.rm = TRUE),
      income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      log_income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$log_income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      mean_income_4to = mean(window_dt$income_4to_for_correlation, na.rm = TRUE),
      mean_log_income_4to = mean(window_dt$log_income_4to_for_correlation, na.rm = TRUE),
      mean_attended_va_eb = mean(window_dt$attended_va_eb, na.rm = TRUE),
      sd_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE),
      se_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE) /
        sqrt(nrow(window_dt))
    )
  }))
}, by = .(
  income_measure_key,
  income_measure_label,
  income_measure_order,
  outcome,
  outcome_key,
  outcome_label,
  plot_order
)]
setorder(percentile_dt, plot_order, income_measure_order, target_percentile)

message("Writing EB VA by income percentile: ", percentile_csv)
fwrite(percentile_dt, percentile_csv)

percentile_plot_dt <- copy(percentile_dt[is.finite(mean_attended_va_eb)])
percentile_plot_dt[, outcome_label := factor(
  outcome_label,
  levels = outcome_specs[order(plot_order), outcome_label]
)]
percentile_plot_dt[, income_measure_label := factor(
  income_measure_label,
  levels = c(
    "Direct income + school median fallback",
    "School median for everyone"
  )
)]

percentile_plot <- ggplot(
  percentile_plot_dt,
  aes(
    x = target_percentile,
    y = mean_attended_va_eb,
    color = income_measure_label,
    linetype = income_measure_label,
    shape = income_measure_label,
    group = income_measure_label
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2.1) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 4) +
  scale_x_continuous(
    breaks = income_percentile_targets,
    labels = paste0("p", income_percentile_targets),
    minor_breaks = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Direct income + school median fallback" = "solid",
      "School median for everyone" = "dashed"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Direct income + school median fallback" = 16,
      "School median for everyone" = 1
    )
  ) +
  labs(
    title = "Attended High School EB VA by Grade-4 Income Percentile",
    subtitle = "Points are local means in a +/-2.5 percentile-point window around each income percentile.",
    x = "Grade-4 income percentile",
    y = "Mean attended high school EB VA",
    color = "Income measure",
    linetype = "Income measure",
    shape = "Income measure"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

message("Writing percentile figure: ", percentile_plot_path)
ggsave(
  filename = percentile_plot_path,
  plot = percentile_plot,
  width = 13,
  height = 8,
  dpi = 300
)

ranked_panel_by_cohort <- copy(panel)
setorder(
  ranked_panel_by_cohort,
  cohort_gr8,
  income_measure_order,
  outcome,
  log_income_4to_for_correlation,
  mrun
)
ranked_panel_by_cohort[, income_rank_share := seq_len(.N) / .N, by = .(
  cohort_gr8,
  income_measure_key,
  outcome
)]

percentile_by_cohort_dt <- ranked_panel_by_cohort[, {
  group_dt <- .SD
  rbindlist(lapply(income_percentile_targets, function(target_percentile) {
    target_share <- target_percentile / 100
    window_lower <- max(0, target_share - income_percentile_window_half_width)
    window_upper <- min(1, target_share + income_percentile_window_half_width)
    window_dt <- group_dt[
      income_rank_share >= window_lower &
        income_rank_share <= window_upper
    ]

    .(
      target_percentile = target_percentile,
      target_percentile_label = paste0("p", target_percentile),
      window_lower_percentile = 100 * window_lower,
      window_upper_percentile = 100 * window_upper,
      n_students_total = nrow(group_dt),
      n_students = nrow(window_dt),
      income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      log_income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$log_income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      mean_income_4to = mean(window_dt$income_4to_for_correlation, na.rm = TRUE),
      mean_log_income_4to = mean(window_dt$log_income_4to_for_correlation, na.rm = TRUE),
      mean_attended_va_eb = mean(window_dt$attended_va_eb, na.rm = TRUE),
      sd_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE),
      se_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE) /
        sqrt(nrow(window_dt))
    )
  }))
}, by = .(
  cohort_gr8,
  income_measure_key,
  income_measure_label,
  income_measure_order,
  outcome,
  outcome_key,
  outcome_label,
  plot_order
)]
setorder(
  percentile_by_cohort_dt,
  plot_order,
  income_measure_order,
  target_percentile,
  cohort_gr8
)

message("Writing EB VA by income percentile and cohort: ", percentile_by_cohort_csv)
fwrite(percentile_by_cohort_dt, percentile_by_cohort_csv)

percentile_by_cohort_plot_dt <- copy(
  percentile_by_cohort_dt[is.finite(mean_attended_va_eb)]
)
percentile_by_cohort_plot_dt[, outcome_label := factor(
  outcome_label,
  levels = outcome_specs[order(plot_order), outcome_label]
)]
percentile_by_cohort_plot_dt[, income_measure_label := factor(
  income_measure_label,
  levels = c(
    "Direct income + school median fallback",
    "School median for everyone"
  )
)]
percentile_by_cohort_plot_dt[, target_percentile_label := factor(
  target_percentile_label,
  levels = paste0("p", income_percentile_targets)
)]

percentile_by_cohort_plot <- ggplot(
  percentile_by_cohort_plot_dt,
  aes(
    x = cohort_gr8,
    y = mean_attended_va_eb,
    color = target_percentile_label,
    linetype = income_measure_label,
    shape = target_percentile_label,
    group = interaction(target_percentile_label, income_measure_label)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.75, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.95) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 4) +
  scale_x_continuous(
    breaks = sort(unique(percentile_by_cohort_plot_dt$cohort_gr8)),
    minor_breaks = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Direct income + school median fallback" = "solid",
      "School median for everyone" = "dashed"
    )
  ) +
  labs(
    title = "Attended High School EB VA by Grade-4 Income Percentile and Cohort",
    subtitle = "Percentiles are recomputed within each grade-8 cohort; points are local means in a +/-2.5 percentile-point window.",
    x = "Grade-8 cohort",
    y = "Mean attended high school EB VA",
    color = "Income percentile",
    linetype = "Income measure",
    shape = "Income percentile"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

message("Writing percentile-by-cohort figure: ", percentile_by_cohort_plot_path)
ggsave(
  filename = percentile_by_cohort_plot_path,
  plot = percentile_by_cohort_plot,
  width = 14,
  height = 8.5,
  dpi = 300
)

panel_no_private_paid <- merge(
  base_students_no_private_paid,
  school_values,
  by = "most_time_RBD",
  all.x = FALSE,
  allow.cartesian = TRUE,
  sort = FALSE
)
panel_no_private_paid <- panel_no_private_paid[is.finite(attended_va_eb)]

base_counts_no_private_paid <- base_students_no_private_paid[, .(
  n_students_with_income_and_rbd = .N
), by = .(cohort_gr8, income_measure_key, income_measure_label, income_measure_order)]

correlation_no_private_paid_dt <- panel_no_private_paid[, {
  n <- .N
  rho <- if (n >= 2L) {
    stats::cor(log_income_4to_for_correlation, attended_va_eb)
  } else {
    NA_real_
  }
  .(
    rho = rho,
    n_students = n,
    mean_log_income_4to = mean(log_income_4to_for_correlation),
    mean_attended_va_eb = mean(attended_va_eb),
    sd_log_income_4to = stats::sd(log_income_4to_for_correlation),
    sd_attended_va_eb = stats::sd(attended_va_eb)
  )
}, by = .(
  cohort_gr8,
  income_measure_key,
  income_measure_label,
  income_measure_order,
  outcome,
  outcome_key,
  outcome_label,
  plot_order
)]
correlation_no_private_paid_dt <- merge(
  correlation_no_private_paid_dt,
  base_counts_no_private_paid,
  by = c("cohort_gr8", "income_measure_key", "income_measure_label", "income_measure_order"),
  all.x = TRUE,
  sort = FALSE
)
correlation_no_private_paid_dt[
  ,
  va_match_rate_among_income_rbd := n_students / n_students_with_income_and_rbd
]
setorder(correlation_no_private_paid_dt, plot_order, income_measure_order, cohort_gr8)

message("Writing no-private-paid cohort correlations: ", correlation_no_private_paid_csv)
fwrite(correlation_no_private_paid_dt, correlation_no_private_paid_csv)

correlation_no_private_paid_plot_dt <- copy(correlation_no_private_paid_dt[is.finite(rho)])
correlation_no_private_paid_plot_dt[, outcome_label := factor(
  outcome_label,
  levels = outcome_specs[order(plot_order), outcome_label]
)]
correlation_no_private_paid_plot_dt[, income_measure_label := factor(
  income_measure_label,
  levels = c(
    "Direct income + school median fallback",
    "School median for everyone"
  )
)]

correlation_no_private_paid_plot <- ggplot(
  correlation_no_private_paid_plot_dt,
  aes(
    x = cohort_gr8,
    y = rho,
    color = outcome_label,
    linetype = income_measure_label,
    group = interaction(outcome_label, income_measure_label)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "grey55") +
  geom_line(linewidth = 0.85, alpha = 0.9) +
  geom_point(aes(shape = income_measure_label), size = 2.2) +
  scale_x_continuous(
    breaks = sort(unique(correlation_no_private_paid_plot_dt$cohort_gr8)),
    minor_breaks = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Direct income + school median fallback" = "solid",
      "School median for everyone" = "dashed"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Direct income + school median fallback" = 16,
      "School median for everyone" = 1
    )
  ) +
  labs(
    title = "Correlation Between Grade-4 Income and Attended High School EB VA",
    subtitle = "Excluding students whose attended high school is Particular Pagado.",
    x = "Grade-8 cohort",
    y = expression(rho),
    color = "Outcome",
    linetype = "Income measure",
    shape = "Income measure"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

message("Writing no-private-paid correlation figure: ", correlation_no_private_paid_plot_path)
ggsave(
  filename = correlation_no_private_paid_plot_path,
  plot = correlation_no_private_paid_plot,
  width = 12.5,
  height = 7,
  dpi = 300
)

ranked_panel_no_private_paid <- copy(panel_no_private_paid)
setorder(
  ranked_panel_no_private_paid,
  income_measure_order,
  outcome,
  log_income_4to_for_correlation,
  mrun
)
ranked_panel_no_private_paid[, income_rank_share := seq_len(.N) / .N, by = .(
  income_measure_key,
  outcome
)]

percentile_no_private_paid_dt <- ranked_panel_no_private_paid[, {
  group_dt <- .SD
  rbindlist(lapply(income_percentile_targets, function(target_percentile) {
    target_share <- target_percentile / 100
    window_lower <- max(0, target_share - income_percentile_window_half_width)
    window_upper <- min(1, target_share + income_percentile_window_half_width)
    window_dt <- group_dt[
      income_rank_share >= window_lower &
        income_rank_share <= window_upper
    ]

    .(
      target_percentile = target_percentile,
      target_percentile_label = paste0("p", target_percentile),
      window_lower_percentile = 100 * window_lower,
      window_upper_percentile = 100 * window_upper,
      n_students_total = nrow(group_dt),
      n_students = nrow(window_dt),
      cohort_min = min(window_dt$cohort_gr8, na.rm = TRUE),
      cohort_max = max(window_dt$cohort_gr8, na.rm = TRUE),
      income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      log_income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$log_income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      mean_income_4to = mean(window_dt$income_4to_for_correlation, na.rm = TRUE),
      mean_log_income_4to = mean(window_dt$log_income_4to_for_correlation, na.rm = TRUE),
      mean_attended_va_eb = mean(window_dt$attended_va_eb, na.rm = TRUE),
      sd_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE),
      se_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE) /
        sqrt(nrow(window_dt))
    )
  }))
}, by = .(
  income_measure_key,
  income_measure_label,
  income_measure_order,
  outcome,
  outcome_key,
  outcome_label,
  plot_order
)]
setorder(percentile_no_private_paid_dt, plot_order, income_measure_order, target_percentile)

message("Writing no-private-paid EB VA by income percentile: ", percentile_no_private_paid_csv)
fwrite(percentile_no_private_paid_dt, percentile_no_private_paid_csv)

percentile_no_private_paid_plot_dt <- copy(
  percentile_no_private_paid_dt[is.finite(mean_attended_va_eb)]
)
percentile_no_private_paid_plot_dt[, outcome_label := factor(
  outcome_label,
  levels = outcome_specs[order(plot_order), outcome_label]
)]
percentile_no_private_paid_plot_dt[, income_measure_label := factor(
  income_measure_label,
  levels = c(
    "Direct income + school median fallback",
    "School median for everyone"
  )
)]

percentile_no_private_paid_plot <- ggplot(
  percentile_no_private_paid_plot_dt,
  aes(
    x = target_percentile,
    y = mean_attended_va_eb,
    color = income_measure_label,
    linetype = income_measure_label,
    shape = income_measure_label,
    group = income_measure_label
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2.1) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 4) +
  scale_x_continuous(
    breaks = income_percentile_targets,
    labels = paste0("p", income_percentile_targets),
    minor_breaks = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Direct income + school median fallback" = "solid",
      "School median for everyone" = "dashed"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Direct income + school median fallback" = 16,
      "School median for everyone" = 1
    )
  ) +
  labs(
    title = "Attended High School EB VA by Grade-4 Income Percentile",
    subtitle = "Excluding Particular Pagado; points are local means in a +/-2.5 percentile-point window.",
    x = "Grade-4 income percentile",
    y = "Mean attended high school EB VA",
    color = "Income measure",
    linetype = "Income measure",
    shape = "Income measure"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

message("Writing no-private-paid percentile figure: ", percentile_no_private_paid_plot_path)
ggsave(
  filename = percentile_no_private_paid_plot_path,
  plot = percentile_no_private_paid_plot,
  width = 13,
  height = 8,
  dpi = 300
)

ranked_panel_by_cohort_no_private_paid <- copy(panel_no_private_paid)
setorder(
  ranked_panel_by_cohort_no_private_paid,
  cohort_gr8,
  income_measure_order,
  outcome,
  log_income_4to_for_correlation,
  mrun
)
ranked_panel_by_cohort_no_private_paid[, income_rank_share := seq_len(.N) / .N, by = .(
  cohort_gr8,
  income_measure_key,
  outcome
)]

percentile_by_cohort_no_private_paid_dt <- ranked_panel_by_cohort_no_private_paid[, {
  group_dt <- .SD
  rbindlist(lapply(income_percentile_targets, function(target_percentile) {
    target_share <- target_percentile / 100
    window_lower <- max(0, target_share - income_percentile_window_half_width)
    window_upper <- min(1, target_share + income_percentile_window_half_width)
    window_dt <- group_dt[
      income_rank_share >= window_lower &
        income_rank_share <= window_upper
    ]

    .(
      target_percentile = target_percentile,
      target_percentile_label = paste0("p", target_percentile),
      window_lower_percentile = 100 * window_lower,
      window_upper_percentile = 100 * window_upper,
      n_students_total = nrow(group_dt),
      n_students = nrow(window_dt),
      income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      log_income_4to_cutpoint = as.numeric(stats::quantile(
        group_dt$log_income_4to_for_correlation,
        probs = target_share,
        na.rm = TRUE,
        names = FALSE
      )),
      mean_income_4to = mean(window_dt$income_4to_for_correlation, na.rm = TRUE),
      mean_log_income_4to = mean(window_dt$log_income_4to_for_correlation, na.rm = TRUE),
      mean_attended_va_eb = mean(window_dt$attended_va_eb, na.rm = TRUE),
      sd_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE),
      se_attended_va_eb = stats::sd(window_dt$attended_va_eb, na.rm = TRUE) /
        sqrt(nrow(window_dt))
    )
  }))
}, by = .(
  cohort_gr8,
  income_measure_key,
  income_measure_label,
  income_measure_order,
  outcome,
  outcome_key,
  outcome_label,
  plot_order
)]
setorder(
  percentile_by_cohort_no_private_paid_dt,
  plot_order,
  income_measure_order,
  target_percentile,
  cohort_gr8
)

message("Writing no-private-paid EB VA by income percentile and cohort: ", percentile_by_cohort_no_private_paid_csv)
fwrite(percentile_by_cohort_no_private_paid_dt, percentile_by_cohort_no_private_paid_csv)

percentile_by_cohort_no_private_paid_plot_dt <- copy(
  percentile_by_cohort_no_private_paid_dt[is.finite(mean_attended_va_eb)]
)
percentile_by_cohort_no_private_paid_plot_dt[, outcome_label := factor(
  outcome_label,
  levels = outcome_specs[order(plot_order), outcome_label]
)]
percentile_by_cohort_no_private_paid_plot_dt[, income_measure_label := factor(
  income_measure_label,
  levels = c(
    "Direct income + school median fallback",
    "School median for everyone"
  )
)]
percentile_by_cohort_no_private_paid_plot_dt[, target_percentile_label := factor(
  target_percentile_label,
  levels = paste0("p", income_percentile_targets)
)]

percentile_by_cohort_no_private_paid_plot <- ggplot(
  percentile_by_cohort_no_private_paid_plot_dt,
  aes(
    x = cohort_gr8,
    y = mean_attended_va_eb,
    color = target_percentile_label,
    linetype = income_measure_label,
    shape = target_percentile_label,
    group = interaction(target_percentile_label, income_measure_label)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.75, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.95) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 4) +
  scale_x_continuous(
    breaks = sort(unique(percentile_by_cohort_no_private_paid_plot_dt$cohort_gr8)),
    minor_breaks = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Direct income + school median fallback" = "solid",
      "School median for everyone" = "dashed"
    )
  ) +
  labs(
    title = "Attended High School EB VA by Grade-4 Income Percentile and Cohort",
    subtitle = "Excluding Particular Pagado; percentiles are recomputed within each grade-8 cohort.",
    x = "Grade-8 cohort",
    y = "Mean attended high school EB VA",
    color = "Income percentile",
    linetype = "Income measure",
    shape = "Income percentile"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

message("Writing no-private-paid percentile-by-cohort figure: ", percentile_by_cohort_no_private_paid_plot_path)
ggsave(
  filename = percentile_by_cohort_no_private_paid_plot_path,
  plot = percentile_by_cohort_no_private_paid_plot,
  width = 14,
  height = 8.5,
  dpi = 300
)

print(correlation_dt[, .(
  cohort_gr8,
  income_measure_label,
  outcome_label,
  rho,
  n_students,
  va_match_rate_among_income_rbd
)])
