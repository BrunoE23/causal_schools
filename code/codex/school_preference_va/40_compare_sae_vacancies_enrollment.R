# Advertised grade-9 SAE vacancies relative to enrollment at basic-plus-HS RBDs.
source("code/codex/school_preference_va/common.R")
out <- file.path(clean_dir, "destination_entry_grades/vacancies_vs_enrollment")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
school_path <- file.path(clean_dir, "destination_entry_grades/school_entry_and_continuation.csv")
grade9_path <- file.path(clean_dir, "destination_entry_grades/school_year_continuation.csv")
schools <- fread(school_path)[directory_primary == TRUE & directory_secondary == TRUE]
check("Same 1918 basic-plus-secondary destinations as previous audit", nrow(schools) == 1918L && !anyDuplicated(schools$rbd))
grade9 <- fread(grade9_path)[rbd %in% schools$rbd, .(rbd, cohort_gr8, grade9_students)]
check("Grade9 enrollment keys unique", !anyDuplicated(grade9[, .(rbd, cohort_gr8)]))
offer_paths <- vapply(2018:2020, function(y) one_source(y, "A1"), character(1))
tracking_paths <- vapply(2018:2020, function(y) {
  p <- list.files(file.path(data_root, "data/raw/student_tracking", y, paste0("Rendimiento-", y)), pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(p) == 1L); p
}, character(1))
fwrite(manifest_rows(c(school_path, grade9_path, offer_paths, tracking_paths)), file.path(out, "input_manifest.csv"))

offers <- rbindlist(lapply(seq_along(offer_paths), function(j) {
  year <- (2018:2020)[j]
  x <- fread(offer_paths[j], sep = ";", select = c("rbd", "cod_nivel", "vacantes", "cupos_totales"), showProgress = FALSE)
  x <- x[cod_nivel == 9L & rbd %in% schools$rbd]
  check(paste(year, "vacancies and capacities are observed nonnegative"), all(is.finite(x$vacantes) & x$vacantes >= 0) &&
    all(is.finite(x$cupos_totales) & x$cupos_totales >= 0))
  x[, .(sae_spots = sum(vacantes), sae_capacity = sum(cupos_totales), offering_rows = .N), by = rbd][, cohort_gr8 := year]
}))

# Read five fields from each ~500 MB source, filter to grade 8 immediately,
# and deduplicate person-school records exactly as for the grade-9 denominator.
grade8 <- if ("--use-cache" %in% commandArgs(trailingOnly = TRUE)) readRDS(file.path(out, "grade8_enrollment.rds")) else rbindlist(lapply(seq_along(tracking_paths), function(j) {
  year <- (2018:2020)[j]
  wanted <- c("MRUN", "RBD", "COD_ENSE", "COD_GRADO", "SIT_FIN")
  h <- names(fread(tracking_paths[j], nrows = 0L, sep = ";", showProgress = FALSE))
  selected <- h[match(wanted, toupper(h))]
  check(paste(year, "grade8 source fields available"), !anyNA(selected))
  message("Counting grade-8 pupils from five selected fields: ", year)
  x <- fread(tracking_paths[j], sep = ";", select = selected, na.strings = c("", "NA"), showProgress = FALSE)
  setnames(x, wanted)
  x <- unique(x[COD_ENSE == 110L & COD_GRADO == 8L & RBD %in% schools$rbd & !is.na(MRUN) & !is.na(SIT_FIN), .(MRUN, RBD)])
  x[, .(grade8_students = .N), by = .(rbd = RBD)][, cohort_gr8 := year]
}))
saveRDS(grade8, file.path(out, "grade8_enrollment.rds"))

panel <- CJ(rbd = schools$rbd, cohort_gr8 = 2018:2020)
panel <- merge(panel, offers, by = c("rbd", "cohort_gr8"), all.x = TRUE)
panel <- merge(panel, grade8, by = c("rbd", "cohort_gr8"), all.x = TRUE)
panel <- merge(panel, grade9, by = c("rbd", "cohort_gr8"), all.x = TRUE)
panel[, in_sae_grade9_offering := !is.na(offering_rows)]
panel[is.na(grade8_students), grade8_students := 0L]
panel[is.na(grade9_students), grade9_students := 0L]
panel[, admission_year := cohort_gr8 + 1L]
panel[, `:=`(spots_per_grade9_student = fifelse(grade9_students > 0, sae_spots / grade9_students, NA_real_),
  spots_per_grade8_student = fifelse(grade8_students > 0, sae_spots / grade8_students, NA_real_))]
check("School-year panel preserves the fixed 1918 schools", nrow(panel) == 3L * 1918L && !anyDuplicated(panel[, .(rbd, cohort_gr8)]))
coverage <- panel[, .(schools_in_fixed_population = .N, schools_in_sae_grade9_offering = sum(in_sae_grade9_offering),
  excluded_school_years = sum(!in_sae_grade9_offering),
  excluded_grade8_enrollment = sum(grade8_students[!in_sae_grade9_offering]),
  excluded_grade9_enrollment = sum(grade9_students[!in_sae_grade9_offering]),
  zero_grade8_among_sae_schools = sum(grade8_students == 0 & in_sae_grade9_offering),
  zero_grade9_among_sae_schools = sum(grade9_students == 0 & in_sae_grade9_offering)), by = .(cohort_gr8, admission_year)]
aggregate_panel <- function(x) x[, .(schools = uniqueN(rbd), school_years = .N, sae_spots = sum(sae_spots),
  sae_capacity = sum(sae_capacity), grade9_students = sum(grade9_students), grade8_students = sum(grade8_students))]
summary <- rbindlist(lapply(2018:2020, function(y) {
  z <- aggregate_panel(panel[cohort_gr8 == y & in_sae_grade9_offering == TRUE]); z[, `:=`(sae_process = as.character(y), grade9_year = as.character(y + 1L))]; z
}))
summary[, `:=`(spots_per_grade9_student = sae_spots / grade9_students, spots_per_grade8_student = sae_spots / grade8_students)]
totals <- aggregate_panel(panel[in_sae_grade9_offering == TRUE])
check("Three separate annual counts exhaust matched school-years", nrow(summary) == 3L && all(vapply(c("sae_spots", "sae_capacity", "grade9_students", "grade8_students", "school_years"),
  function(v) sum(summary[[v]]) == totals[[v]], logical(1))))
check("Each separate annual comparison has positive enrollment denominators", all(summary$grade9_students > 0 & summary$grade8_students > 0))
fwrite(panel, file.path(out, "school_year_vacancies_enrollment.csv"))
fwrite(summary, file.path(out, "vacancies_enrollment_summary.csv"))
fwrite(coverage, file.path(out, "coverage.csv"))
fwrite(checks, file.path(out, "verification.csv"))
print(summary); print(coverage)
