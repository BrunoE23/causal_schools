###############################################################################
# Construct middle-school controls for the grade-8 universe
#
# Inputs:
#   C:/Users/xd-br/Dropbox/causal_schools/data/clean/universe.csv
#   C:/Users/xd-br/Dropbox/causal_schools/data/clean/tracking_univ8gr.RData
#   C:/Users/xd-br/Dropbox/causal_schools/data/raw/student_tracking/2014-2016/
#
# Outputs:
#   C:/Users/xd-br/Dropbox/causal_schools/data/clean/middle_school_controls/
#     middle_school_controls.csv
#     middle_school_controls_diagnostics.csv
#
# Definition:
#   Middle-school records are observed grade 5-8 enrollment rows between three
#   years before and the year of the student's first observed grade-8 cohort.
#   The saved full-universe tracking object starts in 2017, so this script reads
#   raw 2014-2016 enrollment files to complete the historical middle-school
#   window for the earliest grade-8 cohorts.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

find_existing_path <- function(candidates, label) {
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0L) {
    stop("Could not find ", label, ". Update candidates.", call. = FALSE)
  }

  candidates[[1L]]
}

data_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Dropbox/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools"
  ),
  "data_wd"
)

input_universe <- file.path(data_wd, "data/clean/universe.csv")
input_tracking_recent <- file.path(data_wd, "data/clean/tracking_univ8gr.RData")
input_raw_tracking_dir <- file.path(data_wd, "data/raw/student_tracking")
output_dir <- file.path(data_wd, "data/clean/middle_school_controls")
output_controls <- file.path(output_dir, "middle_school_controls.csv")
output_diagnostics <- file.path(output_dir, "middle_school_controls_diagnostics.csv")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading universe: ", input_universe)
universe <- fread(input_universe, select = c("MRUN", "cohort_gr8"))
universe[, MRUN := as.character(MRUN)]

needed_cols <- c(
  "MRUN", "RBD", "AGNO", "COD_GRADO", "PROM_GRAL", "ASISTENCIA", "SIT_FIN",
  "school_grade_avg_GPA", "school_grade_sd_GPA",
  "school_grade_avg_ATT", "school_grade_sd_ATT"
)

raw_needed_cols <- c(
  "AGNO", "RBD", "COD_GRADO", "MRUN", "PROM_GRAL", "ASISTENCIA", "SIT_FIN"
)

read_raw_tracking_year <- function(year, universe_dt) {
  folder_path <- file.path(input_raw_tracking_dir, year, paste0("Rendimiento-", year))
  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    stop("No CSV files found in folder: ", folder_path)
  }
  if (length(csv_files) > 1) {
    stop("Multiple CSV files found in folder: ", paste(csv_files, collapse = ", "))
  }

  message("Reading raw historical tracking year ", year, ": ", csv_files[1])
  header <- names(fread(csv_files[1], sep = ";", nrows = 0, encoding = "Latin-1"))
  header_upper <- toupper(header)
  selected_original <- header[match(raw_needed_cols, header_upper)]

  missing_raw_cols <- raw_needed_cols[is.na(selected_original)]
  if (length(missing_raw_cols) > 0) {
    stop("Raw tracking file for ", year, " is missing columns: ",
         paste(missing_raw_cols, collapse = ", "))
  }

  dt <- fread(
    csv_files[1],
    sep = ";",
    select = selected_original,
    encoding = "Latin-1",
    na.strings = c("", "NA")
  )
  setnames(dt, names(dt), toupper(names(dt)))

  dt <- dt[
    !is.na(SIT_FIN) &
      COD_GRADO >= 5 & COD_GRADO <= 8 &
      !is.na(RBD)
  ]

  # The school-grade moments must be computed before filtering to the universe,
  # matching the upstream tracking construction.
  dt[PROM_GRAL != 0, gpa_for_moment := PROM_GRAL]
  dt[ASISTENCIA != 0, att_for_moment := ASISTENCIA]
  dt[, `:=`(
    school_grade_avg_GPA = mean(gpa_for_moment, na.rm = TRUE),
    school_grade_sd_GPA = sd(gpa_for_moment, na.rm = TRUE),
    school_grade_avg_ATT = mean(att_for_moment, na.rm = TRUE),
    school_grade_sd_ATT = sd(att_for_moment, na.rm = TRUE)
  ), by = .(RBD, COD_GRADO)]
  dt[, c("gpa_for_moment", "att_for_moment") := NULL]

  dt[, MRUN := as.character(MRUN)]
  dt <- universe_dt[dt, on = "MRUN", nomatch = 0L]
  dt <- dt[AGNO >= cohort_gr8 - 3 & AGNO <= cohort_gr8]
  dt[, ..needed_cols]
}

message("Loading full-universe tracking object: ", input_tracking_recent)
load(input_tracking_recent)
setDT(tracking_all)

missing_cols <- setdiff(needed_cols, names(tracking_all))
if (length(missing_cols) > 0) {
  stop("tracking_all is missing required columns: ", paste(missing_cols, collapse = ", "))
}

max_recent_year <- max(universe$cohort_gr8, na.rm = TRUE)
tracking_recent <- tracking_all[
  AGNO >= 2017 & AGNO <= max_recent_year,
  ..needed_cols
]
rm(tracking_all)
gc(verbose = FALSE)

tracking_recent[, MRUN := as.character(MRUN)]
tracking_recent <- universe[tracking_recent, on = "MRUN", nomatch = 0L]
tracking_recent <- tracking_recent[
    AGNO >= cohort_gr8 - 3 &
    AGNO <= cohort_gr8 &
    COD_GRADO >= 5 & COD_GRADO <= 8 &
    !is.na(RBD),
  ..needed_cols
]

tracking_historical <- rbindlist(
  lapply(2014:2016, read_raw_tracking_year, universe_dt = universe),
  use.names = TRUE,
  fill = TRUE
)

tracking <- rbindlist(
  list(tracking_historical, tracking_recent),
  use.names = TRUE,
  fill = TRUE
)

rm(tracking_historical, tracking_recent)
gc(verbose = FALSE)

raw_rows <- nrow(tracking)
raw_students <- uniqueN(tracking$MRUN)
raw_multi_student_years <- tracking[, .N, by = .(MRUN, AGNO)][N > 1L, .N]

message("Resolving duplicate student-year enrollment records")
tracking[, nonzero_att := fifelse(!is.na(ASISTENCIA) & ASISTENCIA > 0, 1L, 0L)]
tracking[, passed := fifelse(!is.na(SIT_FIN) & SIT_FIN == "P", 1L, 0L)]

# Match the existing project convention: when a student has multiple rows in
# the same year, prefer nonzero attendance, then passed records, then highest
# attendance. Any remaining exact ties are broken deterministically by RBD.
setorder(tracking, MRUN, AGNO, -nonzero_att, -passed, -ASISTENCIA, RBD)
tracking_clean <- tracking[, .SD[1L], by = .(MRUN, AGNO)]

rm(tracking)
gc(verbose = FALSE)

multi_after_clean <- tracking_clean[, .N, by = .(MRUN, AGNO)][N > 1L, .N]

message("Constructing most-time middle-school RBD")
most_time <- tracking_clean[, .(
  n_years = .N,
  last_year = max(AGNO, na.rm = TRUE)
), by = .(MRUN, RBD)]

setorder(most_time, MRUN, -n_years, -last_year, RBD)
most_time <- most_time[, .SD[1L], by = MRUN]
setnames(
  most_time,
  c("RBD", "n_years", "last_year"),
  c("most_time_RBD_middle", "most_time_RBD_middle_n_years", "most_time_RBD_middle_last_year")
)

message("Constructing middle-school GPA and attendance z-score summaries")
tracking_clean[
  PROM_GRAL != 0 & !is.na(school_grade_sd_GPA) & school_grade_sd_GPA > 0,
  z_gpa_middle_record := (PROM_GRAL - school_grade_avg_GPA) / school_grade_sd_GPA
]

tracking_clean[
  ASISTENCIA != 0 & !is.na(school_grade_sd_ATT) & school_grade_sd_ATT > 0,
  z_att_middle_record := (ASISTENCIA - school_grade_avg_ATT) / school_grade_sd_ATT
]

z_summaries <- tracking_clean[, .(
  middle_years_observed = .N,
  middle_first_year = min(AGNO, na.rm = TRUE),
  middle_last_year = max(AGNO, na.rm = TRUE),
  middle_first_grade_observed = min(COD_GRADO, na.rm = TRUE),
  middle_last_grade_observed = max(COD_GRADO, na.rm = TRUE),
  z_gpa_middle_mean = mean(z_gpa_middle_record, na.rm = TRUE),
  z_gpa_middle_n = sum(!is.na(z_gpa_middle_record)),
  z_att_middle_mean = mean(z_att_middle_record, na.rm = TRUE),
  z_att_middle_n = sum(!is.na(z_att_middle_record))
), by = MRUN]

z_summaries[is.nan(z_gpa_middle_mean), z_gpa_middle_mean := NA_real_]
z_summaries[is.nan(z_att_middle_mean), z_att_middle_mean := NA_real_]

controls <- merge(
  universe[, .(MRUN, cohort_gr8)],
  most_time[, .(
    MRUN,
    most_time_RBD_middle,
    most_time_RBD_middle_n_years,
    most_time_RBD_middle_last_year
  )],
  by = "MRUN",
  all.x = TRUE
)

controls <- merge(controls, z_summaries, by = "MRUN", all.x = TRUE)
setorder(controls, MRUN)

diagnostics <- rbindlist(list(
  data.table(metric = "universe_students", value = uniqueN(universe$MRUN)),
  data.table(metric = "raw_middle_rows", value = raw_rows),
  data.table(metric = "students_with_any_middle_record", value = raw_students),
  data.table(metric = "students_with_most_time_RBD_middle", value = sum(!is.na(controls$most_time_RBD_middle))),
  data.table(metric = "distinct_most_time_RBD_middle", value = uniqueN(controls$most_time_RBD_middle, na.rm = TRUE)),
  data.table(metric = "raw_multi_student_years", value = raw_multi_student_years),
  data.table(metric = "multi_student_years_after_cleaning", value = multi_after_clean),
  data.table(metric = "students_with_z_gpa_middle_mean", value = sum(!is.na(controls$z_gpa_middle_mean))),
  data.table(metric = "students_with_z_att_middle_mean", value = sum(!is.na(controls$z_att_middle_mean))),
  data.table(metric = "mean_middle_years_observed", value = mean(controls$middle_years_observed, na.rm = TRUE)),
  data.table(metric = "median_middle_years_observed", value = median(controls$middle_years_observed, na.rm = TRUE)),
  data.table(metric = "students_with_3plus_middle_years_observed", value = sum(controls$middle_years_observed >= 3, na.rm = TRUE)),
  data.table(metric = "students_with_4_middle_years_observed", value = sum(controls$middle_years_observed >= 4, na.rm = TRUE))
), fill = TRUE)

message("Writing controls: ", output_controls)
fwrite(controls, output_controls)

message("Writing diagnostics: ", output_diagnostics)
fwrite(diagnostics, output_diagnostics)

message("Done.")
print(diagnostics)
