# Append baseline parent education to the unchanged high-math, region-linked sample.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
dest <- file.path(repo, "data/clean/parent_education_he_enrollment")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
sample_path <- file.path(repo, "data/clean/high_math_main_region_cubic_income_contribution/analysis_data.rds")
parent_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
mapping_path <- file.path(repo, "code/clean_simce_survey.R")
paths <- c(sample_path, parent_path, mapping_path)
stopifnot(all(file.exists(paths)))
before <- file.info(paths)
small_hashes <- unname(tools::md5sum(paths[c(1L, 3L)]))
a <- as.data.table(readRDS(sample_path))
stopifnot(nrow(a) == 93423L, !anyNA(a$mrun), !anyDuplicated(a$mrun),
  all(a$z_math >= 1), all(a$region_origin_code %in% 1:16))
cols <- c("mrun", "cohort_gr8", "simce_year", "father_educ_years", "mother_educ_years",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_educ_years_was_imputed", "mother_educ_years_was_imputed")
header <- names(fread(parent_path, nrows = 0L))
stopifnot(all(cols %in% header))
# Preflight: the source is 1.23 GB. Read only nine columns, then immediately
# retain the 93,423 requested IDs; do not load or rebuild the wide master.
message("Reading nine parent/linkage columns from ", round(before$size[2L] / 1e6), " MB source.")
p <- fread(parent_path, select = cols, na.strings = c("", "NA"), showProgress = FALSE)
p <- p[mrun %in% a$mrun]
stopifnot(!anyDuplicated(p$mrun), nrow(p) == nrow(a))
p <- p[match(a$mrun, mrun)]
stopifnot(!anyNA(p$mrun), all(p$mrun == a$mrun), all(p$cohort_gr8 == a$cohort_gr8))
for (v in setdiff(cols, c("mrun", "cohort_gr8"))) set(a, j = v, value = p[[v]])
for (parent in c("father", "mother")) {
  reported <- a[[paste0(parent, "_educ_years")]]
  imputed <- a[[paste0(parent, "_educ_years_imputed")]]
  flag <- a[[paste0(parent, "_educ_years_was_imputed")]]
  stopifnot(all(reported >= 0 & reported <= 21, na.rm = TRUE),
    all(imputed >= 0 & imputed <= 21, na.rm = TRUE),
    all(imputed[is.finite(reported)] == reported[is.finite(reported)]),
    all(flag[is.finite(reported)] == 0),
    all(flag[!is.finite(reported) & is.finite(imputed)] == 1))
}
a[, `:=`(both_reported = is.finite(father_educ_years) & is.finite(mother_educ_years),
  both_available = is.finite(father_educ_years_imputed) & is.finite(mother_educ_years_imputed))]
coverage <- a[, .(N = .N, father_reported = sum(is.finite(father_educ_years)),
  mother_reported = sum(is.finite(mother_educ_years)), both_reported = sum(both_reported),
  both_available = sum(both_available), father_imputed = sum(father_educ_years_was_imputed, na.rm = TRUE),
  mother_imputed = sum(mother_educ_years_was_imputed, na.rm = TRUE),
  n_missing_after_imputation = sum(!both_available))]
fwrite(coverage, file.path(dest, "parent_coverage.csv"))
fwrite(a[, .(N = .N, both_reported = sum(both_reported), both_available = sum(both_available),
  enrollment_rate = mean(enrolled)), by = psu_year], file.path(dest, "coverage_by_admission_year.csv"))
fwrite(rbindlist(lapply(c("father", "mother"), function(parent) {
  counts <- a[, .(N = .N), by = .(education_years = get(paste0(parent, "_educ_years")))]
  counts[, parent := parent]
  counts
})), file.path(dest, "reported_education_distribution.csv"))
saveRDS(a, file.path(dest, "analysis_data.rds"))
after <- file.info(paths)
stopifnot(identical(before$size, after$size), identical(before$mtime, after$mtime),
  identical(small_hashes, unname(tools::md5sum(paths[c(1L, 3L)]))))
fwrite(data.table(path = paths, bytes = before$size, modified = as.character(before$mtime),
  md5 = c(small_hashes[1L], NA_character_, small_hashes[2L]),
  integrity_check = c("checksum", "size and modification time; selected-column read only", "checksum")),
  file.path(dest, "source_manifest.csv"))
print(coverage)
message("Matched all original sample IDs; reported values and source files preserved.")
