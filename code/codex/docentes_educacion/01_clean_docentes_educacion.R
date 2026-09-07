suppressPackageStartupMessages(library(data.table))

# 2013-2025 staff cleaning and past-only inputs for the 2018-2024 VA window.
# See --help. Raw data are read only; no PCA or school-level weights are imposed.
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %chin% args) {
  cat(paste(
    "Usage: Rscript 01_clean_docentes_educacion.R [INPUT_DIR] [OUTPUT_DIR]",
    "  --preflight                Inspect file sizes, years and headers only.",
    "  --dry-run                  Build/check in memory without writing outputs.",
    "  --sample-rows=N --dry-run   Test first N appointment rows from each year.",
    "  --overwrite                Explicitly permit replacing this build's outputs.",
    "",
    "Default input: Box/causal_schools/data/raw/docentes_educacion.",
    "Default output: PROJECT/data/clean/docentes_educacion.",
    "School-period aggregation and PCA are deliberately separate next steps.",
    sep = "\n"
  ), "\n")
  quit(save = "no", status = 0L)
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Run this entry point with Rscript.")
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg),
                                    winslash = "/", mustWork = TRUE))
source(file.path(script_dir, "staff_cleaning_helpers.R"))
setDTthreads(min(4L, getDTthreads()))
preflight_only <- "--preflight" %chin% args
dry_run <- "--dry-run" %chin% args
overwrite <- "--overwrite" %chin% args
sample_arg <- grep("^--sample-rows=", args, value = TRUE)
if (length(sample_arg) > 1L) stop("Specify --sample-rows only once.")
sample_rows <- if (length(sample_arg)) suppressWarnings(
  as.numeric(sub("^--sample-rows=", "", sample_arg))) else Inf
if (length(sample_arg) && (!is.finite(sample_rows) || sample_rows < 1L ||
                          sample_rows != trunc(sample_rows))) stop("Invalid --sample-rows.")
if (is.finite(sample_rows) && !dry_run) stop("--sample-rows requires --dry-run; sampled data are never exported.")
args <- args[!args %chin% c("--preflight", "--dry-run", "--overwrite", sample_arg)]
if (any(startsWith(args, "--")) || length(args) > 2L) stop("Unknown arguments; use --help.")
default_input <- "C:/Users/brunem/Box/causal_schools/data/raw/docentes_educacion"
input_dir <- normalizePath(if (length(args)) args[[1L]] else default_input,
                           winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(script_dir, "../../.."), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(
  if (length(args) >= 2L) args[[2L]] else file.path(project_dir, "data/clean/docentes_educacion"),
  winslash = "/", mustWork = FALSE
)
output_compare <- tolower(gsub("\\\\", "/", output_dir))
input_compare <- tolower(gsub("\\\\", "/", input_dir))
if (output_compare == input_compare || startsWith(output_compare, paste0(input_compare, "/")) ||
    grepl("(^|/)raw(/|$)", output_compare)) {
  stop("Output must not be inside the raw-data tree.")
}
csv_files <- sort(list.files(input_dir, pattern = "\\.csv$", recursive = TRUE,
                             full.names = TRUE, ignore.case = TRUE))
if (!length(csv_files)) stop("No annual CSV files found under ", input_dir)

message("Preflight: checking ", length(csv_files), " annual files (no full data read).")
manifest <- rbindlist(lapply(csv_files, function(path) {
  first <- fread(path, sep = ";", nrows = 1L, encoding = "Latin-1", showProgress = FALSE)
  missing_required <- setdiff(c("AGNO", "RBD", "MRUN", "PERSONAS", "ID_IFP"), names(first))
  if (length(missing_required)) stop("Missing required fields in ", basename(path), ": ",
                                    paste(missing_required, collapse = ", "))
  year <- suppressWarnings(as.integer(first$AGNO))
  if (length(year) != 1L || is.na(year) || year < 2013L || year > 2025L) {
    stop("Unexpected annual process year in ", basename(path))
  }
  data.table(
    AGNO = year, SOURCE_FILE = basename(path), SOURCE_BYTES = file.info(path)$size,
    SOURCE_COLUMNS = ncol(first), HAS_SECONDARY_ROLE = "ID_IFS" %chin% names(first),
    MISSING_OPTIONAL_COLUMNS = paste(setdiff(staff_raw_fields(), names(first)), collapse = ";")
  )
}))
setorder(manifest, AGNO)
if (anyDuplicated(manifest$AGNO)) stop("Multiple annual input files for the same AGNO; resolve explicitly.")
print(manifest[, .(AGNO, SOURCE_FILE, SOURCE_MB = round(SOURCE_BYTES / 1024^2, 1),
                   HAS_SECONDARY_ROLE)])
if (any(diff(manifest$AGNO) != 1L)) {
  warning("Input years have gaps; histories count observed years and flag missing calendar years.")
}
if (preflight_only) {
  print(manifest[, .(AGNO, MISSING_OPTIONAL_COLUMNS)])
  message("Preflight complete; no full data read and no files written.")
  quit(save = "no", status = 0L)
}
year_tag <- paste(min(manifest$AGNO), max(manifest$AGNO), sep = "_")
output_names <- c(
  main = paste0("docentes_educacion_", year_tag, "_clean.csv.gz"),
  appointments = paste0("docentes_educacion_", year_tag, "_appointments.csv.gz"),
  person_history = paste0("docentes_educacion_", year_tag, "_person_year_history.csv.gz"),
  school_history = paste0("docentes_educacion_", year_tag, "_person_school_year_history.csv.gz"),
  va_staff = "docentes_educacion_va_2018_2024_staff_features.csv.gz",
  audit = paste0("docentes_educacion_", year_tag, "_cleaning_audit.csv"),
  functions = paste0("docentes_educacion_", year_tag, "_main_function_counts.csv"),
  coverage = paste0("docentes_educacion_", year_tag, "_role_coverage.csv"),
  manifest = paste0("docentes_educacion_", year_tag, "_input_manifest.csv"),
  feature_missingness = "docentes_educacion_va_2018_2024_feature_missingness.csv"
)
output_paths <- file.path(output_dir, output_names)
names(output_paths) <- names(output_names)
if (!dry_run && !overwrite && any(file.exists(output_paths))) {
  stop("Output(s) already exist; choose a new output directory or explicitly pass --overwrite: ",
       paste(names(output_paths)[file.exists(output_paths)], collapse = ", "))
}

message("Cleaning selected columns", if (is.finite(sample_rows)) " [SAMPLED DRY RUN]" else "", "...")
cleaned <- lapply(csv_files, function(path) {
  message("  ", basename(path))
  staff_clean_annual(path, nrows = sample_rows)
})
audit <- rbindlist(lapply(cleaned, function(x) x$audit), fill = TRUE)
appointments <- rbindlist(lapply(cleaned, function(x) x$data), use.names = TRUE, fill = TRUE)
rm(cleaned)
invisible(gc())
setorder(audit, AGNO)
setorder(appointments, AGNO, MRUN, RBD, SOURCE_ROW)
if (anyDuplicated(appointments[PERSONAS == 1L & VALID_PERSON_ID == 1L],
                  by = c("MRUN", "AGNO"))) {
  stop("Conflicting main person-year records. No outputs written; source resolution is required.")
}
message("Building past-only person and person-school histories...")
histories <- staff_build_histories(appointments)

# Preserve the old main-position output and EVER_* definition for career reports.
# These full-window flags are RETROSPECTIVE and excluded from the VA feature file.
main <- appointments[PERSONAS == 1L]
main[VALID_PERSON_ID == 1L, EVER_TEACHER := as.integer(any(ID_IFP == 1L, na.rm = TRUE)), by = MRUN]
main[VALID_PERSON_ID == 1L, EVER_ORIENTADOR := as.integer(any(ID_IFP == 9L, na.rm = TRUE)), by = MRUN]
main[, EVER_FLAGS_ARE_RETROSPECTIVE := 1L]
function_counts <- main[, .N, by = .(AGNO, ID_IFP, FUNCION_PRINCIPAL)]
setorder(function_counts, AGNO, ID_IFP)
# Retain broad coverage for auditing, alongside the HS-only teacher definition.
# No PERSONAS restriction is imposed on school-specific staffing appointments.
coverage <- appointments[, .(
  N_APPOINTMENTS = .N,
  N_VALID_PERSONS = uniqueN(MRUN[VALID_PERSON_ID == 1L]),
  N_RBDS = uniqueN(RBD[VALID_SCHOOL_ID == 1L]),
  N_MAIN_APPOINTMENTS = sum(PERSONAS == 1L, na.rm = TRUE),
  N_TEACHER_PRIMARY = sum(TEACHER_PRIMARY == 1L, na.rm = TRUE),
  N_TEACHER_SECONDARY_ONLY = sum(TEACHER_SECONDARY_ONLY == 1L, na.rm = TRUE),
  N_TEACHER_HS_PRIMARY = sum(TEACHER_HS_PRIMARY == 1L, na.rm = TRUE),
  N_TEACHER_HS_SECONDARY_ONLY = sum(TEACHER_HS_SECONDARY_ONLY == 1L, na.rm = TRUE),
  N_TEACHER_HS_UNKNOWN = sum(TEACHER_ANY == 1L & is.na(HS_ASSIGNMENT_ANY), na.rm = TRUE),
  N_ORIENTADOR_PRIMARY = sum(ORIENTADOR_PRIMARY == 1L, na.rm = TRUE),
  N_ORIENTADOR_SECONDARY_ONLY = sum(ORIENTADOR_SECONDARY_ONLY == 1L, na.rm = TRUE)
), by = AGNO]
setorder(coverage, AGNO)

message("Preparing 2018-2024 candidate staff rows (not final school aggregates)...")
va_staff <- appointments[
  IN_VA_WINDOW == 1L & VALID_PERSON_ID == 1L & VALID_SCHOOL_ID == 1L &
    (TEACHER_HS_ANY == 1L | ORIENTADOR_ANY == 1L)
]
# A basic-only teacher who is also an orientador can remain for the latter
# measure, but must not enter the teacher measure through the combined file.
va_staff[, VA_TEACHER_ELIGIBLE := as.integer(TEACHER_HS_ANY %in% 1L)]
va_staff[, VA_ORIENTADOR_ELIGIBLE := as.integer(ORIENTADOR_ANY %in% 1L)]
# Update joins avoid repeatedly copying the wide appointment/credential table.
attach_history <- function(history, join_keys) {
  new_cols <- setdiff(names(history), join_keys)
  if (any(new_cols %chin% names(va_staff))) stop("Unexpected history-column collision.")
  va_staff[history, on = join_keys, (new_cols) := mget(paste0("i.", new_cols))]
}
attach_history(histories$person_year, c("MRUN", "AGNO"))
attach_history(histories$person_school_year, c("MRUN", "RBD", "AGNO"))
setorder(va_staff, RBD, AGNO, MRUN, SOURCE_ROW)
# Each feature row remains an appointment row, with AGNO/SOURCE_FILE/SOURCE_ROW as
# its unique key. History years are deduplicated across simultaneous appointments.
stopifnot(!anyDuplicated(va_staff, by = c("AGNO", "SOURCE_FILE", "SOURCE_ROW")))
stopifnot(!any(grepl("^EVER_", names(va_staff))))
stopifnot(all(va_staff$VA_TEACHER_ELIGIBLE == 1L | va_staff$VA_ORIENTADOR_ELIGIBLE == 1L))
numeric_features <- names(va_staff)[vapply(va_staff, is.numeric, logical(1L))]
feature_missingness <- rbindlist(lapply(numeric_features, function(field) {
  va_staff[, .(FEATURE = field, N_ROWS = .N, N_OBSERVED = sum(!is.na(get(field))),
               N_MISSING = sum(is.na(get(field)))), by = AGNO]
}), fill = TRUE)
print(audit)
print(coverage)
message(
  "Validated ", format(nrow(appointments), big.mark = ","), " appointment rows; ",
  format(nrow(main), big.mark = ","), " main-position rows; ",
  format(nrow(va_staff), big.mark = ","), " VA-window candidate appointment rows."
)
if (dry_run) {
  message("Dry run complete. No files written; sampled runs are not population estimates.")
} else {
  if (!overwrite && any(file.exists(output_paths))) {
    stop("An output appeared during this run; refusing to overwrite it.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_gz <- function(dt, key) {
    message("Writing ", basename(output_paths[[key]]))
    fwrite(dt, output_paths[[key]], compress = "gzip", na = "")
  }
  write_gz(main, "main")
  write_gz(appointments, "appointments")
  write_gz(histories$person_year, "person_history")
  write_gz(histories$person_school_year, "school_history")
  write_gz(va_staff, "va_staff")
  fwrite(audit, output_paths[["audit"]])
  fwrite(function_counts, output_paths[["functions"]])
  fwrite(coverage, output_paths[["coverage"]])
  fwrite(manifest, output_paths[["manifest"]])
  fwrite(feature_missingness, output_paths[["feature_missingness"]])
  message("Completed: ", output_dir)
}
