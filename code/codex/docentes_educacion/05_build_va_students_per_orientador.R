suppressPackageStartupMessages(library(data.table))

# Small, standalone staffing-count build; does not rerun VA or full staff history.
args <- commandArgs(trailingOnly = TRUE)
if ("--help" %chin% args) {
  cat(paste(
    "Usage: Rscript 05_build_va_students_per_orientador.R [INPUT_DIR] [OUTPUT_DIR]",
    "  --preflight    Check seven staff files and the saved broad VA counts only.",
    "  --dry-run      Compute in memory without exporting.",
    "  --overwrite    Permit replacing this script's own outputs.",
    "Input defaults to Box staff raw data; output to PROJECT/data/clean/docentes_educacion.",
    "Numerator: saved broad VA n_students by most_time_RBD (grade-8 2017-2020).",
    "Denominator: arithmetic mean of seven annual distinct-person counts, 2018-2024.",
    sep = "\n"), "\n")
  quit(save = "no", status = 0L)
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Run with Rscript.")
task_dir <- dirname(normalizePath(sub("^--file=", "", script_arg), winslash = "/"))
project_dir <- normalizePath(file.path(task_dir, "../../.."), winslash = "/")
source(file.path(task_dir, "staff_cleaning_helpers.R"))
source(file.path(task_dir, "orientador_ratio_helpers.R"))
setDTthreads(min(4L, getDTthreads()))
preflight <- "--preflight" %chin% args
dry_run <- "--dry-run" %chin% args
overwrite <- "--overwrite" %chin% args
args <- args[!args %chin% c("--preflight", "--dry-run", "--overwrite")]
if (length(args) > 2L || any(startsWith(args, "--"))) stop("Unknown arguments; use --help.")
input_dir <- normalizePath(if (length(args)) args[1L] else
  "C:/Users/brunem/Box/causal_schools/data/raw/docentes_educacion", winslash = "/", mustWork = TRUE)
output_dir <- if (length(args) == 2L) args[2L] else file.path(project_dir, "data/clean/docentes_educacion")
# Resolve an existing ancestor before checking a not-yet-created output path.
ori_resolve_output <- function(path) {
  if (dir.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  parent <- dirname(path)
  if (identical(parent, path)) stop("Cannot resolve output path.")
  file.path(ori_resolve_output(parent), basename(path))
}
output_dir <- ori_resolve_output(output_dir)
output_compare <- tolower(gsub("\\\\", "/", output_dir))
input_compare <- tolower(input_dir)
if (grepl("(^|/)raw(/|$)", output_compare) || output_compare == input_compare ||
    startsWith(output_compare, paste0(input_compare, "/"))) stop("Output must not be in the raw-data tree.")

years <- 2018:2024
va_path <- file.path(project_dir, "output/tables/empirical_bayes_school_va/stata_va_eb_input_exam.csv")
if (!file.exists(va_path)) stop("Missing saved broad VA count source: ", va_path)
if (file.info(va_path)$size > 10 * 1024^2) stop("Unexpectedly large VA count source; inspect before loading.")
va <- fread(va_path)
va_counts <- ori_validate_va_counts(va)
message("VA sample: ", sum(va_counts$N_VA_STUDENTS), " students; ", nrow(va_counts), " schools.")
selected <- c("AGNO", "RBD", "MRUN", "ID_IFP", "ID_IFS")
files <- sort(list.files(input_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
manifest <- rbindlist(lapply(files, function(path) {
  first <- fread(path, sep = ";", nrows = 1L, encoding = "Latin-1", showProgress = FALSE)
  if (!"AGNO" %in% names(first) || nrow(first) != 1L) stop("Cannot identify year: ", path)
  year <- suppressWarnings(as.integer(first$AGNO))
  if (is.na(year)) stop("Invalid file year: ", path)
  if (!year %in% years) return(NULL)
  if (!all(selected %in% names(first))) stop("Missing required staff columns: ", path)
  data.table(AGNO = year, SOURCE_PATH = normalizePath(path, winslash = "/"),
             SOURCE_BYTES = file.info(path)$size,
             SOURCE_MTIME = as.character(file.info(path)$mtime))
}))
if (!nrow(manifest) || anyDuplicated(manifest$AGNO) || !setequal(manifest$AGNO, years)) {
  stop("Need exactly one staff input for every year 2018-2024.")
}
setorder(manifest, AGNO)
print(manifest[, .(AGNO, SOURCE_FILE = basename(SOURCE_PATH), SOURCE_MB = round(SOURCE_BYTES / 1024^2, 1))])
message("Full run selects only five columns and immediately collapses to VA-school counts.")
if (preflight) {
  message("Preflight complete; no annual data loaded and no files written.")
  quit(save = "no", status = 0L)
}
names_out <- c(period = "va_students_per_orientador_2018_2024.csv",
  annual = "va_schools_orientador_headcounts_2018_2024.csv",
  diagnostics = "va_students_per_orientador_2018_2024_diagnostics.csv",
  manifest = "va_students_per_orientador_2018_2024_input_manifest.csv")
paths_out <- setNames(file.path(output_dir, names_out), names(names_out))
if (!dry_run && !overwrite && any(file.exists(paths_out))) stop("Outputs exist; explicitly use --overwrite to replace them.")

annual <- rbindlist(lapply(seq_len(nrow(manifest)), function(k) {
  message("Counting orientadores: ", manifest$AGNO[k])
  dt <- fread(manifest$SOURCE_PATH[k], sep = ";", select = selected,
              colClasses = list(character = "MRUN"), encoding = "Latin-1",
              na.strings = c("", "NA"), showProgress = FALSE)
  for (field in setdiff(selected, "MRUN")) {
    raw <- trimws(as.character(dt[[field]]))
    value <- suppressWarnings(as.numeric(raw))
    invalid <- !is.na(raw) & nzchar(raw) & (is.na(value) | !is.finite(value) | value != trunc(value))
    if (any(invalid)) stop("Invalid numeric code in ", field, ", year ", manifest$AGNO[k])
    set(dt, j = field, value = as.integer(value))
  }
  if (anyNA(dt$AGNO) || any(dt$AGNO != manifest$AGNO[k])) stop("Mixed/invalid file year.")
  out <- ori_annual_counts(dt, va_counts$RBD)
  manifest[k, `:=`(SOURCE_ROWS = nrow(dt), VA_SCHOOL_APPOINTMENT_ROWS = sum(out$N_APPOINTMENTS))]
  out
}))
result <- ori_build_period_ratios(va_counts, annual, years)
diagnostics <- rbindlist(lapply(c("PRIMARY", "ANY"), function(scope) {
  dt <- result$period
  status <- dt[[paste0("RATIO_STATUS_", scope)]]
  ratio <- dt[[paste0("VA_STUDENTS_PER_AVG_ORIENTADOR_", scope)]]
  available <- ratio[status == "available"]
  qs <- if (length(available)) quantile(available, c(.1, .5, .9), names = FALSE) else rep(NA_real_, 3L)
  data.table(SCOPE = scope, N_SCHOOLS = nrow(dt), N_VA_STUDENTS = sum(dt$N_VA_STUDENTS),
    N_SCHOOLS_COMPLETE_STAFF = sum(status != "incomplete_staff_coverage"),
    N_SCHOOLS_ZERO_ORIENTADORES = sum(status == "zero_orientadores_full_period"),
    N_SCHOOLS_INCOMPLETE_STAFF = sum(status == "incomplete_staff_coverage"),
    N_SCHOOLS_RATIO_AVAILABLE = length(available),
    N_VA_STUDENTS_RATIO_AVAILABLE = sum(dt$N_VA_STUDENTS[status == "available"]),
    RATIO_MEAN_UNWEIGHTED = if (length(available)) mean(available) else NA_real_,
    RATIO_P10_UNWEIGHTED = qs[1L], RATIO_MEDIAN_UNWEIGHTED = qs[2L], RATIO_P90_UNWEIGHTED = qs[3L])
}))
print(diagnostics)
# Basic source immutability/stability check; no writes have targeted the inputs.
after <- file.info(manifest$SOURCE_PATH)
stopifnot(identical(as.numeric(after$size), as.numeric(manifest$SOURCE_BYTES)),
          identical(as.character(after$mtime), manifest$SOURCE_MTIME))
manifest[, VA_COUNT_SOURCE := normalizePath(va_path, winslash = "/")]
manifest[, VA_COUNT_SOURCE_MD5 := unname(tools::md5sum(va_path))]
if (!dry_run) {
  if (!overwrite && any(file.exists(paths_out))) stop("An output appeared during computation; refusing to overwrite.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  outputs <- c(result, list(diagnostics = diagnostics, manifest = manifest))
  for (key in names(paths_out)) {
    fwrite(outputs[[key]], paths_out[[key]], na = "NA")
    message("Wrote: ", paths_out[[key]])
  }
} else message("Dry run complete; no files written.")
