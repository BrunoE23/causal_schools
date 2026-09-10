suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task <- dirname(normalizePath(script, winslash = "/")); root <- normalizePath(file.path(task, "../../.."), winslash = "/")
source(file.path(task, "../docentes_educacion/staff_cleaning_helpers.R"))
source(file.path(task, "../staff_quality_va/staff_quality_helpers.R"))
source(file.path(task, "leadership_helpers.R"))
setDTthreads(min(4L, getDTthreads()))
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %chin% c("--preflight", "--overwrite", "--aggregate-cache"))) stop("Unknown argument.")
out <- file.path(root, "data/clean/leadership_quality_va")
old <- file.path(root, "data/clean/staff_quality_va")
manifest <- fread(file.path(old, "staff_source_manifest.csv"))
stopifnot(identical(manifest$AGNO, 2013:2024), all(file.exists(manifest$SOURCE_PATH)))
info <- file.info(manifest$SOURCE_PATH)
manifest[, `:=`(SOURCE_BYTES = info$size, SOURCE_MTIME = as.character(info$mtime))]
for (i in seq_len(nrow(manifest))) {
  header <- fread(manifest$SOURCE_PATH[i], sep = ";", nrows = 1, select = c("AGNO", "MRUN", "RBD", "ID_IFP", "ID_IFS", "PERSONAS"))
  stopifnot(header$AGNO == manifest$AGNO[i])
}
context <- fread(file.path(old, "staff_school_context.csv"))
schools <- context$RBD
stopifnot(uniqueN(schools) == 3682L, sum(context$N_VA_STUDENTS) == 757999L)
message("Preflight: one annual file at a time; retain thin all-position histories and current VA-school leaders only.")
print(manifest[, .(AGNO, MB = round(SOURCE_BYTES/1024^2, 1))])
if ("--preflight" %chin% args) quit(save = "no", status = 0L)
owned <- c("leadership_person_school_year.rds", "leadership_school_year_roster.csv", "leadership_school_year_measures.csv.gz",
  "leadership_school_period_measures.csv", "leadership_metric_dictionary.csv", "leadership_feature_coverage.csv",
  "leadership_tenure_audit.csv", "leadership_source_manifest.csv", "leadership_numeric_conflicts.csv",
  "leadership_cleaning_audit.csv", "leadership_preserved_sources.csv", "leadership_source_appointments.csv.gz")
if (!"--overwrite" %chin% args && any(file.exists(file.path(out, owned)))) stop("Outputs exist; use --overwrite explicitly.")
protected_paths <- c(list.files(old, full.names = TRUE),
  file.path(root, "output/pdf/staff_quality_and_school_va_report.pdf"),
  list.files(file.path(root, "output/figures/staff_quality_va"), full.names = TRUE),
  file.path(root, "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv"))
protected <- data.table(PATH = protected_paths, MD5 = unname(tools::md5sum(protected_paths)))
if (!"--aggregate-cache" %chin% args) {
  current <- list(); rosters <- list(); history <- list(); conflicts <- list(); audits <- list()
  for (i in seq_len(nrow(manifest))) {
    year <- manifest$AGNO[i]; message(format(Sys.time(), "%H:%M:%S"), " Leadership build: ", year)
    x <- staff_clean_annual(manifest$SOURCE_PATH[i])
    history[[i]] <- x$data[, .(MRUN, RBD, AGNO, PERSONAS, ID_IFP, ID_IFS, VALID_PERSON_ID, VALID_SCHOOL_ID)]
    audits[[i]] <- x$audit
    if (year >= 2018L) {
      prepared <- l_prepare(x$data, schools)
      current[[as.character(year)]] <- prepared$people
      rosters[[as.character(year)]] <- prepared$roster
      conflicts[[as.character(year)]] <- cbind(AGNO = year, prepared$conflicts)
      message("  Retained ", nrow(prepared$people), " leader person-school-years.")
      rm(prepared)
    }
    rm(x); invisible(gc())
  }
  people <- rbindlist(current); roster <- rbindlist(rosters); rows <- rbindlist(history)
  rm(current, rosters, history); invisible(gc())
  rows <- rows[MRUN %chin% unique(people$MRUN)]
  message(format(Sys.time(), "%H:%M:%S"), " Leadership histories for ", uniqueN(people$MRUN), " people.")
  hist <- l_histories(rows, unique(people$MRUN))
  people <- l_attach(people, hist)
  rm(hist); invisible(gc())
} else {
  cached <- fread(file.path(out, "leadership_source_manifest.csv"))
  stopifnot(identical(manifest, cached, ignore.environment = TRUE) ||
    isTRUE(all.equal(manifest, cached, check.attributes = FALSE)))
  people <- readRDS(file.path(out, owned[1]))
  roster <- fread(file.path(out, owned[2]))
  conflicts <- list(fread(file.path(out, owned[9])))
  audits <- list(fread(file.path(out, owned[10])))
}
stopifnot(all(people$AGNO %in% 2018:2024), all(people$LEADER_ANY == 1L), !anyDuplicated(people, by = c("RBD", "AGNO", "MRUN")))
measures <- l_aggregate(people, roster, schools)
coverage <- measures$period[, .(N_SCHOOLS = .N, N_OBSERVED = sum(is.finite(VALUE)),
  N_WITH_THREE_ACTIVE_YEARS = sum(is.finite(VALUE) & N_ACTIVE_YEARS >= 3),
  MEAN = if (any(is.finite(VALUE))) mean(VALUE, na.rm = TRUE) else NA_real_, SD = sd(VALUE, na.rm = TRUE)), by = METRIC]
tenure <- people[, .(N = .N, N_KNOWN = sum(!is.na(YEARS_AT_SCHOOL)), N_ZERO = sum(YEARS_AT_SCHOOL == 0, na.rm = TRUE),
  N_PREVIOUSLY_SEEN = sum(SCHOOL_PRIOR_OBSERVED_YEARS > 0, na.rm = TRUE),
  N_ZERO_PREVIOUSLY_SEEN = sum(YEARS_AT_SCHOOL == 0 & SCHOOL_PRIOR_OBSERVED_YEARS > 0, na.rm = TRUE)), by = AGNO]
after <- file.info(manifest$SOURCE_PATH)
stopifnot(identical(as.numeric(after$size), as.numeric(manifest$SOURCE_BYTES)),
  identical(as.character(after$mtime), manifest$SOURCE_MTIME),
  identical(unname(tools::md5sum(protected$PATH)), protected$MD5))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
if (!"--aggregate-cache" %chin% args) {
  saveRDS(people, file.path(out, owned[1]), compress = "gzip")
  fwrite(rows, file.path(out, owned[12]), na = "NA")
}
csvs <- list(measures$roster, measures$annual, measures$period, measures$dictionary, coverage,
             tenure, manifest, rbindlist(conflicts), rbindlist(audits), protected)
for (i in seq_along(csvs)) fwrite(csvs[[i]], file.path(out, owned[i+1]), na = "NA")
print(coverage[METRIC %chin% c("prior_role_years", "school_role_spell_years", "university_share", "teaching_title_share")])
print(tenure)
message("Completed leadership measures; prior analyses and source VA remain byte-identical.")
