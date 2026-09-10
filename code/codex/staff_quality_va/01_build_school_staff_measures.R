suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %chin% c("--preflight", "--overwrite", "--aggregate-cache"))) stop("Unknown argument.")
overwrite <- "--overwrite" %chin% args
aggregate_cache <- "--aggregate-cache" %chin% args
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task_dir <- dirname(normalizePath(script, winslash = "/"))
root <- normalizePath(file.path(task_dir, "../../.."), winslash = "/")
source(file.path(root, "code/codex/docentes_educacion/staff_cleaning_helpers.R"))
source(file.path(task_dir, "staff_quality_helpers.R"))
setDTthreads(min(4L, getDTthreads()))
raw_dir <- "C:/Users/brunem/Box/causal_schools/data/raw/docentes_educacion"
out_dir <- file.path(root, "data/clean/staff_quality_va")
if (grepl("(^|/)raw(/|$)", tolower(out_dir))) stop("Cannot write raw data.")
va_path <- file.path(root, "output/tables/empirical_bayes_school_va/stata_va_eb_input_exam.csv")
va <- fread(va_path, select = c("school_rbd", "n_students", "n_total", "outcome", "analysis_sample"))
stopifnot(!anyDuplicated(va$school_rbd), all(va$outcome == "admission_exam_taker"),
          all(va$analysis_sample == "All"), nrow(va) == 3682L, sum(va$n_students) == 757999L)
school_ids <- va$school_rbd
paths <- list.files(raw_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
manifest <- rbindlist(lapply(paths, function(path) {
  one <- fread(path, sep = ";", nrows = 1L, encoding = "Latin-1", showProgress = FALSE)
  if (!all(c("AGNO", q_history_columns()[!q_history_columns() %chin% c("VALID_PERSON_ID", "VALID_SCHOOL_ID",
        "TEACHER_PRIMARY", "TEACHER_HS_PRIMARY", "ORIENTADOR_PRIMARY", "TEACHER_ANY", "TEACHER_HS_ANY", "ORIENTADOR_ANY")])
      %in% names(one))) stop("Missing basic staff columns: ", path)
  year <- as.integer(one$AGNO)
  if (!year %in% 2013:2024) return(NULL)
  data.table(AGNO = year, SOURCE_PATH = path, SOURCE_BYTES = file.info(path)$size,
              SOURCE_MTIME = as.character(file.info(path)$mtime))
}))
setorder(manifest, AGNO)
stopifnot(identical(manifest$AGNO, 2013:2024), !anyDuplicated(manifest$AGNO))
message("Preflight: 12 annual files; current staff 2018-2024; histories since 2013.")
print(manifest[, .(AGNO, MB = round(SOURCE_BYTES/1024^2, 1))])
message("One year cleaned at a time; retain thin history rows and deduplicated VA-school staff only.")
if ("--preflight" %chin% args) quit(save = "no", status = 0L)
out_names <- c("staff_person_school_year_features.rds", "staff_school_year_measures.csv.gz",
  "staff_school_period_measures.csv", "staff_school_year_roster.csv", "staff_metric_dictionary.csv",
  "staff_quality_cleaning_audit.csv", "staff_numeric_conflicts.csv", "staff_tenure_audit.csv",
  "staff_feature_coverage.csv", "staff_source_manifest.csv", "staff_school_context.csv")
if (!overwrite && any(file.exists(file.path(out_dir, out_names)))) stop("Outputs exist; use --overwrite explicitly.")
if (!aggregate_cache) {
people_years <- list(); roster_years <- list(); history_years <- list(); audits <- list(); conflicts <- list()
for (k in seq_len(nrow(manifest))) {
  year <- manifest$AGNO[k]
  message(format(Sys.time(), "%H:%M:%S"), " Cleaning staff year ", year)
  x <- staff_clean_annual(manifest$SOURCE_PATH[k])
  audits[[as.character(year)]] <- x$audit
  history_years[[as.character(year)]] <- x$data[, q_history_columns(), with = FALSE]
  if (year >= 2018L) {
    if (x$data[RBD %in% school_ids, any(VALID_PERSON_ID != 1L)]) {
      stop("Unidentified staff at VA schools: audit before constructing school means.")
    }
    prepared <- q_prepare_people(x$data, school_ids)
    people_years[[as.character(year)]] <- prepared$people
    roster_years[[as.character(year)]] <- prepared$roster
    conflicts[[as.character(year)]] <- cbind(AGNO = year, prepared$conflicts)
    message("  Retained ", nrow(prepared$people), " current person-school-years.")
    rm(prepared)
  }
  rm(x)
  invisible(gc())
}
message(format(Sys.time(), "%H:%M:%S"), " Building histories for current VA-school staff.")
people <- rbindlist(people_years); history <- rbindlist(history_years); roster <- rbindlist(roster_years)
rm(people_years, history_years, roster_years); invisible(gc())
people <- q_attach_histories(people, history)
rm(history); invisible(gc())
} else {
  message("Reaggregating verified existing staff-person features; no raw data reload.")
  cached_manifest <- fread(file.path(out_dir, "staff_source_manifest.csv"))
  stopifnot(identical(as.numeric(manifest$SOURCE_BYTES), as.numeric(cached_manifest$SOURCE_BYTES)),
            identical(as.character(manifest$SOURCE_MTIME), as.character(cached_manifest$SOURCE_MTIME)),
            identical(manifest$SOURCE_PATH, cached_manifest$SOURCE_PATH))
  people <- readRDS(file.path(out_dir, "staff_person_school_year_features.rds"))
  roster <- fread(file.path(out_dir, "staff_school_year_roster.csv"))
  audits <- list(fread(file.path(out_dir, "staff_quality_cleaning_audit.csv")))
  conflicts <- list(fread(file.path(out_dir, "staff_numeric_conflicts.csv")))
}
stopifnot(all(people$AGNO %in% 2018:2024), all(people$RBD %in% school_ids),
          !anyDuplicated(people, by = c("MRUN", "RBD", "AGNO")))
# Exact reconciliation to the earlier orientador-headcount build.
old_counts <- fread(file.path(root, "data/clean/docentes_educacion/va_schools_orientador_headcounts_2018_2024.csv"))
check <- merge(old_counts[, .(RBD, AGNO, N_ORIENTADORES_ANY)], roster[, .(RBD, AGNO, N_COUNSELOR)],
                by = c("RBD", "AGNO"), all = TRUE)
stopifnot(isTRUE(all.equal(check$N_ORIENTADORES_ANY, check$N_COUNSELOR, check.attributes = FALSE)))
message(format(Sys.time(), "%H:%M:%S"), " Aggregating characteristics with explicit coverage gates.")
measures <- q_aggregate_measures(people, roster, school_ids)
tenure <- rbindlist(lapply(c("counselor", "teacher"), function(role) {
  dt <- people[if (role == "counselor") ORIENTADOR_ANY == 1L else TEACHER_HS_ANY == 1L]
  dt[, .(ROLE = role, N_PERSON_SCHOOL_YEARS = .N,
    N_TENURE_REPORTED = sum(!is.na(YEARS_AT_SCHOOL)), N_TENURE_ZERO = sum(YEARS_AT_SCHOOL == 0, na.rm = TRUE),
    N_TENURE_POSITIVE = sum(YEARS_AT_SCHOOL > 0, na.rm = TRUE),
    N_PREVIOUSLY_SEEN_AT_SCHOOL = sum(SCHOOL_PRIOR_RECORD_YEARS_SINCE_2013 > 0, na.rm = TRUE),
    N_ZERO_TENURE_PREVIOUSLY_SEEN = sum(YEARS_AT_SCHOOL == 0 & SCHOOL_PRIOR_RECORD_YEARS_SINCE_2013 > 0, na.rm = TRUE)),
    by = AGNO]
}))
coverage <- measures$period[, .(N_SCHOOLS = .N, N_OBSERVED = sum(is.finite(VALUE)),
  N_INDEX_YEAR_SUPPORT = sum(is.finite(VALUE) & N_ACTIVE_YEARS >= 3L),
  MEAN = if (any(is.finite(VALUE))) mean(VALUE[is.finite(VALUE)]) else NA_real_,
  SD = sd(VALUE, na.rm = TRUE)), by = .(ROLE, METRIC)]
directory_path <- "C:/Users/brunem/Box/causal_schools/data/raw/school_directory/2024/20240912_Directorio_Oficial_EE_2024_20240430_WEB.csv"
context_fields <- c("RBD", "NOM_RBD", "COD_DEPE", "COD_DEPE2", "COD_REG_RBD", "RURAL_RBD", paste0("ENS_", sprintf("%02d", 1:11)))
directory <- fread(directory_path, sep = ";", select = context_fields, encoding = "Latin-1")
directory <- unique(directory[RBD %in% school_ids])
if (anyDuplicated(directory$RBD)) stop("Conflicting school-directory metadata; no arbitrary row selection.")
ens <- as.matrix(directory[, grep("^ENS_", names(directory), value = TRUE), with = FALSE])
directory[, HAS_TP_OR_ARTISTIC := as.integer(rowSums(matrix(ens %in% c(410L,510L,610L,710L,810L,910L), nrow = nrow(ens))) > 0)]
directory[, HAS_BASIC := as.integer(rowSums(ens == 110L, na.rm = TRUE) > 0)]
context <- merge(va[, .(RBD = school_rbd, N_VA_STUDENTS = n_students)],
                 directory[, setdiff(names(directory), grep("^ENS_", names(directory), value = TRUE)), with = FALSE],
                 by = "RBD", all.x = TRUE)
context[, `:=`(COHORT_GR8_START = 2017L, COHORT_GR8_END = 2020L, STAFF_YEAR_START = 2018L, STAFF_YEAR_END = 2024L)]
after <- file.info(manifest$SOURCE_PATH)
stopifnot(identical(as.numeric(after$size), as.numeric(manifest$SOURCE_BYTES)),
          identical(as.character(after$mtime), manifest$SOURCE_MTIME))
if (!overwrite && any(file.exists(file.path(out_dir, out_names)))) stop("Output appeared during run; refusing overwrite.")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!aggregate_cache) saveRDS(people, file.path(out_dir, out_names[1L]), compress = "gzip")
csvs <- list(measures$annual, measures$period, roster, measures$dictionary, rbindlist(audits, fill = TRUE),
              rbindlist(conflicts), tenure, coverage, manifest, context)
for (k in seq_along(csvs)) fwrite(csvs[[k]], file.path(out_dir, out_names[k + 1L]), na = "NA")
print(coverage[METRIC %chin% c("prior_role_years", "prior_school_role_years", "university_share", "teaching_title_share", "hs_teaching_title_share", "math_subject_match", "language_subject_match")])
print(tenure)
message("Completed school staff measures: ", out_dir)
