# School entry grades and actual internal continuation; no preference refits.
source("code/codex/school_preference_va/common.R")
out <- file.path(clean_dir, "destination_entry_grades")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
peer_path <- file.path(clean_dir, "peer_populations_all_schools/source_peer_characteristics_used.csv")
grade_path <- file.path(clean_dir, "continuation_grade12_audit/school_ense_grade_record_counts.csv")
origin_path <- file.path(root, "data/clean/pre_sae_va_accounting/pre_post_student_high_school_links.rds")
schools <- unique(fread(peer_path)[, .(rbd, school_name, comuna)])
check("Latest preference destination population", nrow(schools) == 2454L && !anyDuplicated(schools$rbd))
cells <- fread(grade_path)[rbd %in% schools$rbd]
historical <- cells[, .(has_grade8_2018_2020 = any(cod_ense == 110L & cod_grado == 8L),
  has_primary_2018_2020 = any(cod_ense == 110L & cod_grado %in% 1:8)), by = rbd]
schools <- merge(schools, historical, by = "rbd", all.x = TRUE)

# The existing origin cache uses first observed grade 8, with its established
# attendance/pass-status tie-breaks. It includes all students, not only SAE.
origins <- as.data.table(readRDS(origin_path))[cohort_gr8 %in% 2018:2020, .(mrun, cohort_gr8, origin_rbd)]
origins[, mrun := as.numeric(mrun)]
check("All-cohort grade-8 origin links unique", !anyDuplicated(origins$mrun))
raw_paths <- vapply(2019:2021, function(y) {
  paths <- list.files(file.path(data_root, "data/raw/student_tracking", y, paste0("Rendimiento-", y)), pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(paths) == 1L); paths
}, character(1))
paths <- c(peer_path, grade_path, origin_path, raw_paths)
fwrite(manifest_rows(paths), file.path(out, "input_manifest.csv"))
enrollment <- if ("--use-cache" %in% commandArgs(trailingOnly = TRUE)) as.data.table(readRDS(file.path(out, "grade9_school_student_continuation.rds"))) else rbindlist(lapply(seq_along(raw_paths), function(j) {
  year <- (2019:2021)[j]
  wanted <- c("MRUN", "RBD", "COD_ENSE", "COD_GRADO", "SIT_FIN")
  header <- names(fread(raw_paths[j], nrows = 0L, sep = ";", showProgress = FALSE))
  selected <- header[match(wanted, toupper(header))]
  check(paste(year, "tracking columns available"), !anyNA(selected))
  message("Reading five selected enrollment fields for ", year, "; filtering immediately to grade 9 destinations.")
  # Sources are about 500 MB. Read only the five fields needed and filter before joins.
  x <- fread(raw_paths[j], sep = ";", select = selected, showProgress = FALSE, na.strings = c("", "NA"))
  setnames(x, wanted)
  x <- unique(x[COD_ENSE %in% c(310L, 410L, 510L, 610L, 710L, 810L, 910L) & COD_GRADO == 1L &
    RBD %in% schools$rbd & !is.na(MRUN) & !is.na(SIT_FIN), .(mrun = as.numeric(MRUN), rbd = as.numeric(RBD))])
  x[, cohort_gr8 := year - 1L]
  x <- merge(x, origins[cohort_gr8 == year - 1L], by = c("mrun", "cohort_gr8"), all.x = TRUE)
  x[, `:=`(origin_observed = !is.na(origin_rbd), internal_continuer = !is.na(origin_rbd) & origin_rbd == rbd)]
  check(paste(year, "grade9 school-person records unique"), !anyDuplicated(x[, .(mrun, rbd)]))
  x
}))
saveRDS(enrollment, file.path(out, "grade9_school_student_continuation.rds"))
check("Internal continuers are unique people within cohort", !anyDuplicated(enrollment[internal_continuer == TRUE, .(mrun, cohort_gr8)]))
# Enrollment denominators are unique person-school pairs, not unique people:
# a pupil may have valid grade-9 records at more than one school in a year.
multiple <- enrollment[, .(school_records = .N), by = .(mrun, cohort_gr8)]
fwrite(multiple[, .(unique_people = .N, people_at_multiple_schools = sum(school_records > 1L),
  student_school_records = sum(school_records), excess_school_records = sum(school_records - 1L)), by = cohort_gr8],
  file.path(out, "multiple_school_enrollment_audit.csv"))
annual <- enrollment[, .(grade9_students = .N, origin_observed = sum(origin_observed),
  internal_continuers = sum(internal_continuer), external_known = sum(origin_observed & !internal_continuer),
  unknown_origin = sum(!origin_observed)), by = .(rbd, cohort_gr8)]
annual[, share_internal_all_grade9 := internal_continuers / grade9_students]
pooled <- annual[, .(grade9_students = sum(grade9_students), origin_observed = sum(origin_observed),
  internal_continuers = sum(internal_continuers), external_known = sum(external_known), unknown_origin = sum(unknown_origin),
  admission_years_observed = .N, years_with_internal_continuers = sum(internal_continuers > 0)), by = rbd]
schools <- merge(schools, pooled, by = "rbd", all.x = TRUE)
for (v in c("grade9_students", "origin_observed", "internal_continuers", "external_known", "unknown_origin", "admission_years_observed", "years_with_internal_continuers"))
  set(schools, which(is.na(schools[[v]])), v, 0L)
schools[, historical_type := fcase(has_grade8_2018_2020 %in% TRUE, "Grade 8 observed at this RBD",
  has_primary_2018_2020 %in% TRUE, "Other primary grades, but no grade 8 observed",
  !is.na(has_primary_2018_2020), "No primary grades observed (grade-9-entry candidate)", default = "Historical school records missing")]

# Independent 2024 Directory classification: do not label basic education as
# guaranteed grade 8, or equate its presence with actual internal continuation.
directory_path <- list.files(file.path(data_root, "data/raw/school_directory/2024"), pattern = "[.]csv$", full.names = TRUE)
stopifnot(length(directory_path) == 1L)
dir <- fread(directory_path, sep = ";", encoding = "Latin-1")
setnames(dir, toupper(names(dir)))
ens <- grep("^ENS_[0-9]+$", names(dir), value = TRUE)
dir[, `:=`(directory_primary = rowSums(.SD == 110L, na.rm = TRUE) > 0,
  directory_preschool = rowSums(.SD == 10L, na.rm = TRUE) > 0,
  directory_secondary = rowSums(as.data.frame(lapply(.SD, function(z) z %in% c(310L, 410L, 510L, 610L, 710L, 810L, 910L)))) > 0), .SDcols = ens]
check("Directory join is unique", !anyDuplicated(dir$RBD))
schools <- merge(schools, dir[, .(rbd = RBD, directory_primary, directory_preschool, directory_secondary, directory_status = ESTADO_ESTAB)], by = "rbd", all.x = TRUE)
schools[, directory_type := fcase(is.na(directory_primary), "Missing from 2024 Directory",
  directory_secondary == FALSE, "No regular secondary listed in 2024",
  directory_primary == TRUE, "Secondary plus regular basic education",
  directory_preschool == TRUE, "Secondary plus preschool, no regular basic education",
  default = "Secondary only: no regular basic education or preschool")]
schools[, share_internal_all_grade9 := fifelse(grade9_students > 0, internal_continuers / grade9_students, NA_real_)]
check("School and student accounting exhausts the population", nrow(schools) == 2454L &&
  all(schools$internal_continuers + schools$external_known + schools$unknown_origin == schools$grade9_students))
historical_summary <- schools[, .(schools = .N, schools_with_continuers = sum(internal_continuers > 0),
  grade9_students = sum(grade9_students), internal_continuers = sum(internal_continuers), unknown_origin = sum(unknown_origin)), by = historical_type]
directory_summary <- schools[, .(schools = .N, share_schools = .N / nrow(schools), schools_with_continuers = sum(internal_continuers > 0),
  grade9_students = sum(grade9_students), internal_continuers = sum(internal_continuers)), by = directory_type]
continuation_summary <- schools[, .(schools = .N, with_grade9_students = sum(grade9_students > 0),
  schools_with_internal_continuers = sum(internal_continuers > 0), schools_without_observed_internal_continuers = sum(internal_continuers == 0),
  grade9_students = sum(grade9_students), internal_continuers = sum(internal_continuers),
  external_known = sum(external_known), unknown_origin = sum(unknown_origin),
  share_internal_all_grade9 = sum(internal_continuers) / sum(grade9_students),
  share_internal_known_origins = sum(internal_continuers) / sum(origin_observed))]
fwrite(schools, file.path(out, "school_entry_and_continuation.csv"))
fwrite(annual, file.path(out, "school_year_continuation.csv"))
fwrite(historical_summary, file.path(out, "historical_grade_presence_summary.csv"))
fwrite(directory_summary, file.path(out, "directory_2024_summary.csv"))
fwrite(continuation_summary, file.path(out, "actual_continuation_summary.csv"))
check("Every latest preference destination has observed grade9 enrollment", all(schools$grade9_students > 0))
check("Every internal continuer school had historical grade8", schools[internal_continuers > 0, all(has_grade8_2018_2020)])
fwrite(checks, file.path(out, "verification.csv"))
print(directory_summary); print(historical_summary); print(continuation_summary)
