suppressPackageStartupMessages(library(data.table))
test_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task_dir <- normalizePath(file.path(dirname(test_file), ".."), winslash = "/", mustWork = TRUE)
source(file.path(task_dir, "staff_cleaning_helpers.R"))
source(file.path(task_dir, "orientador_ratio_helpers.R"))
setDTthreads(2L)
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop("FAILED: ", label)
  checks <<- checks + 1L
  cat("PASS: ", label, "\n", sep = "")
}
fails <- function(expr) inherits(tryCatch(force(expr), error = function(e) e), "error")
row <- function(rbd, id, primary = 9L, secondary = 0L, year = 2018L) {
  data.table(RBD = as.integer(rbd), MRUN = as.character(id), ID_IFP = as.integer(primary),
              ID_IFS = as.integer(secondary), AGNO = as.integer(year))
}
raw <- rbindlist(list(
  row(1, "100"), row(1, "100"), row(1, "101", 1, 9),
  row(2, "100"), # Same person at another school counts there too.
  row(3, "102", 1, 0),
  row(4, "103", 1, NA),
  row(5, "104", NA, 9),
  row(6, "0", 9, 0),
  row(7, "105", 9, 0), row(7, "105", NA, NA),
  row(8, "106", 1, 0), row(8, "106", NA, NA),
  row(9, "0", 1, 0),
  row(10, "107", 99, 99),
  row(999, "108")
))
before <- copy(raw)
annual <- ori_annual_counts(raw, 1:10)
check(identical(raw, before), "Annual helper does not mutate input")
check(annual[RBD == 1, N_ORIENTADORES_PRIMARY] == 1L, "Duplicate appointments count one primary person")
check(annual[RBD == 1, N_ORIENTADORES_ANY] == 2L, "Secondary-only orientador enters any-function headcount")
check(annual[RBD == 2, N_ORIENTADORES_ANY] == 1L, "Person counted at each reported school")
check(annual[RBD == 3, N_ORIENTADORES_ANY] == 0L, "Covered all-nonorientador roster has zero")
check(annual[RBD == 4, N_ORIENTADORES_PRIMARY] == 0L && is.na(annual[RBD == 4, N_ORIENTADORES_ANY]),
      "Unknown secondary role does not contaminate known primary count")
check(is.na(annual[RBD == 5, N_ORIENTADORES_PRIMARY]) && annual[RBD == 5, N_ORIENTADORES_ANY] == 1L,
      "Known secondary positive resolves any role despite unknown primary")
check(is.na(annual[RBD == 6, N_ORIENTADORES_ANY]), "Unidentified potential orientador makes headcount unknown")
check(annual[RBD == 7, N_ORIENTADORES_PRIMARY] == 1L, "Known positive appointment dominates same-person unknown")
check(is.na(annual[RBD == 8, N_ORIENTADORES_ANY]), "Known negative plus unknown remains unknown")
check(annual[RBD == 9, N_ORIENTADORES_ANY] == 0L, "Invalid ID on known nonorientador does not inflate or invalidate count")
check(is.na(annual[RBD == 10, N_ORIENTADORES_ANY]), "Unmapped roles are unknown, not negative")
check(!999 %in% annual$RBD, "Schools outside VA support are excluded")
check(nrow(ori_annual_counts(raw, 888L)) == 0L, "Annual source with no VA matches is handled")
check(fails(ori_annual_counts(rbind(raw, row(1, "111", year = 2019)), 1:10)), "Mixed annual years rejected")

va_raw <- data.table(school_rbd = 1:10, analysis_sample = "All", outcome = "admission_exam_taker",
                      n_students = 700L, n_total = 7000L)
va <- ori_validate_va_counts(va_raw)
check(sum(va$N_VA_STUDENTS) == 7000L, "VA school totals reconcile")
bad <- copy(va_raw); bad[1L, n_total := 6999L]
check(fails(ori_validate_va_counts(bad)), "Inconsistent VA total rejected")
bad <- rbind(va_raw, va_raw[1L])
check(fails(ori_validate_va_counts(bad)), "Duplicate VA schools rejected")
bad <- copy(va_raw); bad[, outcome := "z_year_math_max"]
check(fails(ori_validate_va_counts(bad)), "Score-observed VA cannot silently replace broad sample")

raw_years <- rbindlist(lapply(2018:2024, function(y) rbindlist(list(
  row(1, as.character(1000 + y), year = y), # Annual turnover is not seven simultaneous people.
  if (y == 2018L) row(1, "201", year = y),
  row(2, "202", 1, 9, y), row(3, "203", 1, 0, y),
  if (y < 2024L) row(4, "204", year = y),
  row(5, "205", 1, NA, y), row(6, "206", NA, 9, y),
  row(7, "0", 9, 0, y),
  row(8, "208", if (y == 2018L) 9L else 1L, 0, y)
))))
annual_years <- rbindlist(lapply(2018:2024, function(y) ori_annual_counts(raw_years[AGNO == y], va$RBD)))
result <- ori_build_period_ratios(va, annual_years)
dt <- result$period
check(nrow(dt) == 10L && nrow(result$annual) == 70L, "All VA schools and all seven years retained")
check(abs(dt[RBD == 1, MEAN_ORIENTADORES_PRIMARY] - 8/7) < 1e-12, "Arithmetic mean annual headcount, not ever-staff count")
check(dt[RBD == 1, VA_STUDENTS_PER_AVG_ORIENTADOR_PRIMARY] == 612.5, "Pooled students divided by period-average staff")
check(dt[RBD == 2, RATIO_STATUS_PRIMARY] == "zero_orientadores_full_period", "Primary-zero school explicitly flagged")
check(is.na(dt[RBD == 2, VA_STUDENTS_PER_AVG_ORIENTADOR_PRIMARY]), "Zero denominator yields NA, not zero or infinity")
check(dt[RBD == 2, VA_STUDENTS_PER_AVG_ORIENTADOR_ANY] == 700, "Secondary-only staffing produces any-function ratio")
check(dt[RBD == 3, NO_ORIENTADOR_FULL_PERIOD_ANY] == 1L, "Covered school without orientadores has presence flag")
check(is.na(dt[RBD == 4, MEAN_ORIENTADORES_ANY]), "Missing year cannot become zero or a six-year primary denominator")
check(dt[RBD == 4, MEAN_ORIENTADORES_OBSERVED_YEARS_ANY] == 1, "Partial-coverage mean kept only as labeled diagnostic")
check(dt[RBD == 4, N_YEARS_KNOWN_ANY] == 6L, "Known-year support is explicit")
check(dt[RBD == 5, RATIO_STATUS_PRIMARY] == "zero_orientadores_full_period" &&
        dt[RBD == 5, RATIO_STATUS_ANY] == "incomplete_staff_coverage", "Role-specific missingness preserved")
check(dt[RBD == 6, RATIO_STATUS_ANY] == "available" &&
        dt[RBD == 6, RATIO_STATUS_PRIMARY] == "incomplete_staff_coverage", "Known any-role count usable with primary unknown")
check(dt[RBD == 8, VA_STUDENTS_PER_AVG_ORIENTADOR_PRIMARY] == 4900, "Six confirmed zero years stay in the seven-year mean")
check(dt[RBD == 9, N_YEARS_ROSTER_OBSERVED] == 0L && is.na(dt[RBD == 9, NO_ORIENTADOR_FULL_PERIOD_ANY]),
      "Entirely absent school has missing staffing, not zero orientadores")
check(fails(ori_build_period_ratios(va, rbind(annual_years, annual_years[1L]))), "Duplicate annual keys rejected")
check(fails(ori_build_period_ratios(va, annual_years, c(2018L, 2020L))), "Nonconsecutive staff period rejected")

# End-to-end fixtures: no real individual records are written by the tests.
fixture_dir <- tempfile("orientador-ratio-tests-")
dir.create(fixture_dir)
input_dir <- file.path(fixture_dir, "raw")
output_dir <- file.path(fixture_dir, "clean")
dir.create(input_dir)
for (y in 2018:2024) fwrite(raw_years[AGNO == y], file.path(input_dir, paste0(y, ".csv")), sep = ";")
input_paths <- list.files(input_dir, full.names = TRUE)
hash_before <- tools::md5sum(input_paths)
entry <- file.path(task_dir, "05_build_va_students_per_orientador.R")
run <- function(out, extra = character()) {
  log <- tempfile(tmpdir = fixture_dir)
  status <- system2(file.path(R.home("bin"), "Rscript.exe"),
                    c("--vanilla", shQuote(entry), shQuote(input_dir), shQuote(out), extra),
                    stdout = log, stderr = log)
  if (status != 0L && !length(extra)) cat(paste(readLines(log, warn = FALSE), collapse = "\n"), "\n")
  status
}
check(run(output_dir, "--preflight") == 0L && !dir.exists(output_dir), "Preflight makes no outputs")
check(run(output_dir, "--dry-run") == 0L && !dir.exists(output_dir), "Dry run makes no outputs")
check(run(output_dir) == 0L, "Full fixture build succeeds")
saved <- fread(file.path(output_dir, "va_students_per_orientador_2018_2024.csv"))
check(nrow(saved) == 3682L && sum(saved$N_VA_STUDENTS) == 757999L, "Export preserves actual broad VA support and numerator")
check(all(is.na(saved[RATIO_STATUS_ANY != "available", VA_STUDENTS_PER_AVG_ORIENTADOR_ANY])), "Missing/zero ratios survive CSV export")
check(run(output_dir, character()) != 0L, "Existing outputs protected without overwrite flag")
check(run(output_dir, "--overwrite") == 0L, "Explicit overwrite only replaces owned outputs")
check(run(input_dir, "--preflight") != 0L, "Raw-output path rejected")
check(identical(hash_before, tools::md5sum(input_paths)), "Raw fixture hashes unchanged")
cat("All ", checks, " orientador-ratio checks passed.\n", sep = "")
