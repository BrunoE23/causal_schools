####################################
# Build reusable pre/post-SAE grade-8 cohort and high-school-link files.
#
# The construction mirrors def_sample_frame.R and def_school.R, but extends the
# grade-8 cohorts backward to 2014 for the accounting exercise.
####################################

suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)

if (.Platform$OS.type == "windows") {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
code_output_wd <- normalizePath(".", winslash = "/")

tracking_root <- file.path(data_wd, "data/raw/student_tracking")
out_dir <- file.path(code_output_wd, "data/clean/pre_sae_va_accounting")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cohort_years <- 2014:2020
grade8_read_years <- 2013:2020
tracking_read_years <- 2015:2024

tracking_file <- function(year) {
  folder <- file.path(tracking_root, year, paste0("Rendimiento-", year))
  files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) != 1L) stop("Expected one tracking CSV for ", year, ", found ", length(files))
  files[[1L]]
}

available_select <- function(path, wanted) {
  header <- names(fread(path, nrows = 0, sep = ";", encoding = "Latin-1", showProgress = FALSE))
  header_upper <- toupper(header)
  header[match(intersect(wanted, header_upper), header_upper)]
}

read_tracking_min <- function(year, wanted) {
  path <- tracking_file(year)
  cols <- available_select(path, wanted)
  d <- fread(
    path,
    sep = ";",
    select = cols,
    encoding = "Latin-1",
    na.strings = c("", "NA"),
    showProgress = FALSE
  )
  setnames(d, toupper(names(d)))
  for (v in setdiff(wanted, names(d))) d[, (v) := NA]
  d <- d[, ..wanted]
  numeric_cols <- setdiff(wanted, "SIT_FIN")
  for (v in numeric_cols) d[, (v) := suppressWarnings(as.numeric(get(v)))]
  d[, AGNO := as.integer(AGNO)]
  d[]
}

estimate_age <- function(d) {
  if (!"EDAD_ALU" %in% names(d)) d[, EDAD_ALU := NA_real_]
  if (!"FEC_NAC_ALU" %in% names(d)) d[, FEC_NAC_ALU := NA_real_]
  d[is.na(EDAD_ALU) & !is.na(FEC_NAC_ALU), birth_year := suppressWarnings(as.integer(substr(as.character(as.integer(FEC_NAC_ALU)), 1, 4)))]
  d[is.na(EDAD_ALU) & !is.na(birth_year), EDAD_ALU := AGNO - birth_year]
  d[, birth_year := NULL]
  d
}

message("Building first observed grade-8 universe for cohorts 2014-2020.")
grade8_cols <- c(
  "AGNO", "RBD", "COD_ENSE", "COD_GRADO", "MRUN", "GEN_ALU",
  "FEC_NAC_ALU", "EDAD_ALU", "GD_ALU", "COD_COM_ALU", "COD_REG_ALU",
  "COD_DEPE2", "ASISTENCIA", "SIT_FIN"
)
grade8_all <- rbindlist(lapply(grade8_read_years, function(year) {
  message("  grade 8 year ", year)
  d <- read_tracking_min(year, grade8_cols)
  d <- estimate_age(d)
  d <- d[!is.na(SIT_FIN) & COD_GRADO == 8 & COD_ENSE == 110 & !is.na(MRUN) & !is.na(RBD)]
  d[, .(
    mrun = as.character(as.integer(MRUN)),
    cohort_gr8 = AGNO,
    origin_rbd = as.numeric(RBD),
    GEN_ALU,
    EDAD_ALU,
    COD_COM_ALU,
    COD_REG_ALU,
    origin_cod_depe2 = COD_DEPE2,
    ASISTENCIA,
    passed = as.integer(trimws(SIT_FIN) == "P")
  )]
}), use.names = TRUE, fill = TRUE)

setorder(grade8_all, mrun, cohort_gr8, -ASISTENCIA, -passed, origin_rbd)
grade8_first <- grade8_all[, .SD[1L], by = mrun]
grade8 <- grade8_first[cohort_gr8 %in% cohort_years]
stopifnot(!anyDuplicated(grade8$mrun))
setkey(grade8, mrun)

message("Building registry grade-9 continuation indicators.")
origin_grade9 <- rbindlist(lapply(cohort_years, function(cohort) {
  year <- cohort + 1L
  message("  grade 9 registry year ", year)
  d <- read_tracking_min(year, c("AGNO", "RBD", "COD_ENSE", "COD_GRADO"))
  d[, cohort_gr8 := cohort]
  d[
    COD_GRADO == 1 & !is.na(COD_ENSE) & COD_ENSE != 110 & !is.na(RBD),
    .(has_grade9_next_year = TRUE),
    by = .(cohort_gr8, origin_rbd = as.numeric(RBD))
  ]
}), use.names = TRUE, fill = TRUE)
origin_grade9 <- unique(origin_grade9, by = c("cohort_gr8", "origin_rbd"))

message("Building cleaned post-grade-8 tracking window.")
tracking_cols <- c("AGNO", "RBD", "COD_ENSE", "COD_GRADO", "MRUN", "COD_DEPE2", "ASISTENCIA", "SIT_FIN")
tracking_window <- rbindlist(lapply(tracking_read_years, function(year) {
  message("  tracking year ", year)
  d <- read_tracking_min(year, tracking_cols)
  d <- d[!is.na(SIT_FIN) & !is.na(MRUN) & !is.na(RBD)]
  d[, mrun := as.character(as.integer(MRUN))]
  d <- d[grade8, on = "mrun", nomatch = 0L]
  d[, year_rel_cohort8 := AGNO - cohort_gr8]
  d <- d[year_rel_cohort8 > 0 & year_rel_cohort8 <= 4]
  d[, .(
    mrun,
    cohort_gr8,
    year_rel_cohort8,
    RBD = as.numeric(RBD),
    ASISTENCIA = as.numeric(ASISTENCIA),
    passed = as.integer(trimws(SIT_FIN) == "P"),
    COD_DEPE2 = as.numeric(COD_DEPE2)
  )]
}), use.names = TRUE, fill = TRUE)

tracking_window[, n_school_year_entries := .N, by = .(mrun, year_rel_cohort8)]
tracking_window <- tracking_window[!(n_school_year_entries > 1 & ASISTENCIA == 0)]
tracking_window[, n_school_year_entries := .N, by = .(mrun, year_rel_cohort8)]
tracking_window[, any_passed := any(passed == 1L, na.rm = TRUE), by = .(mrun, year_rel_cohort8)]
tracking_window <- tracking_window[!(n_school_year_entries > 1 & any_passed & passed != 1L)]
tracking_window[, n_school_year_entries := .N, by = .(mrun, year_rel_cohort8)]
tracking_window[, max_attendance := max(ASISTENCIA, na.rm = TRUE), by = .(mrun, year_rel_cohort8)]
tracking_window[!is.finite(max_attendance), max_attendance := NA_real_]
tracking_window <- tracking_window[n_school_year_entries == 1 | is.na(max_attendance) | ASISTENCIA == max_attendance]

other_counts <- tracking_window[, .(years_at_rbd = uniqueN(year_rel_cohort8)), by = .(mrun, RBD)]
tracking_window <- merge(tracking_window, other_counts, by = c("mrun", "RBD"), all.x = TRUE, sort = FALSE)
setorder(tracking_window, mrun, year_rel_cohort8, -years_at_rbd, RBD)
tracking_window <- tracking_window[, .SD[1L], by = .(mrun, year_rel_cohort8)]
tracking_window[, c("n_school_year_entries", "any_passed", "max_attendance", "years_at_rbd") := NULL]

first_last <- tracking_window[, .(
  RBD_rel1 = RBD[match(1L, year_rel_cohort8)],
  RBD_rel4 = RBD[match(4L, year_rel_cohort8)]
), by = mrun]
most_time <- tracking_window[, .(
  n_years = .N,
  last_year = max(year_rel_cohort8),
  most_time_cod_depe2 = COD_DEPE2[which.max(year_rel_cohort8)]
), by = .(mrun, RBD)]
setorder(most_time, mrun, -n_years, -last_year, RBD)
most_time <- most_time[, .SD[1L], by = mrun]
setnames(most_time, "RBD", "most_time_RBD")

student_links <- merge(grade8, first_last, by = "mrun", all.x = TRUE, sort = FALSE)
student_links <- merge(student_links, most_time, by = "mrun", all.x = TRUE, sort = FALSE)
student_links <- merge(student_links, origin_grade9, by = c("cohort_gr8", "origin_rbd"), all.x = TRUE, sort = FALSE)
student_links[is.na(has_grade9_next_year), has_grade9_next_year := FALSE]
student_links[, first_last_same := RBD_rel1 == RBD_rel4]
student_links[, destination_nonprivate := !is.na(most_time_cod_depe2) & most_time_cod_depe2 != 4]
student_links[, actual_switch := !is.na(most_time_RBD) & most_time_RBD != origin_rbd]
student_links[, switch_group := fifelse(
  !has_grade9_next_year,
  "School ends before grade 9",
  fifelse(has_grade9_next_year & actual_switch & destination_nonprivate, "Voluntary switch to non-private HS", NA_character_)
)]

sample_accounting <- student_links[, .(
  students = .N,
  with_post_school = sum(!is.na(most_time_RBD)),
  school_ends = sum(!has_grade9_next_year),
  voluntary_switch_nonprivate = sum(has_grade9_next_year & actual_switch & destination_nonprivate, na.rm = TRUE),
  unresolved_for_primary_groups = sum(is.na(switch_group))
), by = cohort_gr8]

input_manifest <- data.table(
  path = sapply(unique(c(grade8_read_years, tracking_read_years)), tracking_file),
  bytes = file.info(sapply(unique(c(grade8_read_years, tracking_read_years)), tracking_file))$size
)

fwrite(input_manifest, file.path(out_dir, "cleaning_input_manifest.csv"))
fwrite(origin_grade9, file.path(out_dir, "origin_grade9_registry.csv"))
fwrite(sample_accounting, file.path(out_dir, "cleaning_sample_accounting.csv"))
saveRDS(grade8, file.path(out_dir, "pre_post_grade8_universe.rds"))
saveRDS(tracking_window, file.path(out_dir, "post_grade8_tracking_window_clean.rds"))
saveRDS(student_links, file.path(out_dir, "pre_post_student_high_school_links.rds"))
writeLines(capture.output(sessionInfo()), file.path(out_dir, "cleaning_session_info.txt"))

print(sample_accounting)
