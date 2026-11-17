# Count genuine submitted choices spanning multiple destination regions.
local_lib <- "C:/Users/brunem/AppData/Local/R/win-library/4.5"
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
out <- "data/clean/sae_cross_region"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
one_file <- function(folder, pattern) {
  p <- list.files(folder, pattern, full.names = TRUE)
  stopifnot(length(p) == 1L)
  p
}
roster <- as.data.table(readRDS("data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds"))[, .(mrun, sae_proceso)]
stopifnot(nrow(roster) == 294198L, !anyDuplicated(roster$mrun))
counts <- list(); distributions <- list(); people <- list(); audits <- list()
for (year in 2018:2020) {
  message("Counting SAE ", year)
  app_path <- one_file(file.path(data_root, "data/raw", year, paste0("SAE_", year)), "^C1_.*[.]csv$")
  dir_path <- one_file(file.path(data_root, "data/raw/school_directory", year), "[.]csv$")
  stopifnot(file.info(app_path)$size < 100e6, file.info(dir_path)$size < 100e6)
  d <- fread(dir_path, select = c("RBD", "COD_REG_RBD"), showProgress = FALSE)
  setnames(d, c("rbd", "region"))
  d <- unique(d)
  stopifnot(!anyDuplicated(d$rbd), !anyNA(d$region), all(d$region %in% 1:16))
  x <- fread(app_path, sep = ";", select = c("mrun", "rbd", "cod_nivel", "agregada_por_continuidad"), showProgress = FALSE)
  source_rows <- nrow(x)
  stopifnot(!anyNA(x), all(x$mrun > 0L), all(x$agregada_por_continuidad %in% 0:1))
  automatic_rows <- x[agregada_por_continuidad == 1L, .N]
  x <- unique(x[agregada_por_continuidad == 0L, .(mrun, rbd, cod_nivel)])
  x <- merge(x, d, by = "rbd", all.x = TRUE, sort = FALSE)
  audits[[as.character(year)]] <- data.table(sae_proceso = year, source_rows, automatic_rows,
    submitted_student_school_grade_pairs = nrow(x), missing_region_pairs = sum(is.na(x$region)),
    source = app_path, geography = dir_path)
  stopifnot(!anyNA(x$region))
  for (scope in c("All grades", "Grade 9", "Paper grade-9 sample")) {
    z <- if (scope == "All grades") x else x[cod_nivel == 9L]
    if (scope == "Paper grade-9 sample") z <- z[mrun %in% roster[sae_proceso == year, mrun]]
    p <- unique(z[, .(mrun, region)])[, .(n_regions = .N,
      n_regions_historical15 = uniqueN(fifelse(region == 16L, 8L, region))), by = mrun]
    p[, `:=`(scope = scope, sae_proceso = year)]
    people[[paste(year, scope)]] <- p
    counts[[paste(year, scope)]] <- p[, .(scope = unique(scope), sae_proceso = as.character(year),
      applicants = .N, multiple_regions = sum(n_regions >= 2L),
      percent_multiple_regions = 100 * mean(n_regions >= 2L),
      multiple_regions_historical15 = sum(n_regions_historical15 >= 2L))]
    distributions[[paste(year, scope)]] <- p[, .(applicants = .N), by = .(scope, sae_proceso, n_regions)]
  }
}
p <- rbindlist(people)
annual <- rbindlist(counts)
pooled <- p[, .(sae_proceso = "2018-2020 applicant-years", applicants = .N,
  multiple_regions = sum(n_regions >= 2L), percent_multiple_regions = 100 * mean(n_regions >= 2L),
  multiple_regions_historical15 = sum(n_regions_historical15 >= 2L)), by = scope]
unique_people <- p[, .(ever_multiple_regions_same_year = any(n_regions >= 2L)), by = .(scope, mrun)][,
  .(unique_people = .N, unique_people_ever_multiple_regions_same_year = sum(ever_multiple_regions_same_year),
    percent = 100 * mean(ever_multiple_regions_same_year)), by = scope]
stopifnot(p[scope == "Paper grade-9 sample", .N] == nrow(roster))
# Verify annual denominators against independently saved all-grade tabulations.
old <- fread("data/clean/sae_applications_by_grade/counts_by_grade_year.csv")
check <- merge(annual[scope == "Grade 9", .(sae_year = as.integer(sae_proceso), applicants)],
  old[cod_nivel == 9L, .(sae_year, previous_applicants = applicants)], by = "sae_year")
stopifnot(nrow(check) == 3L, all(check$applicants == check$previous_applicants))
fwrite(rbindlist(list(annual, pooled), use.names = TRUE), file.path(out, "counts_by_sample_year.csv"))
fwrite(unique_people, file.path(out, "unique_people_counts.csv"))
fwrite(rbindlist(distributions), file.path(out, "number_of_regions_distribution.csv"))
fwrite(rbindlist(audits), file.path(out, "source_audit.csv"))
print(rbindlist(list(annual, pooled), use.names = TRUE))
print(unique_people)
