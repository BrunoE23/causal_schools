# Construct 2022 2nd-year-high-school parent-education school means and add them
# to all original SAE destination rankings. Launch with the 2022 SIMCE source
# folder as the process working directory; this avoids Windows path-encoding
# problems in the accented Box folder name.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1L])
project_root <- normalizePath(file.path(dirname(script_arg), "../../.."), winslash = "/")
cpad_path <- file.path("Archivos CSV (Planos)", "simce2m2022_cpad_final_SEG.csv")
stopifnot(file.exists(cpad_path))
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)

message("Reading only student ID, RBD and both parent-education responses from 2022 CPAD.")
cpad <- fread(cpad_path, select = c("idAlumno", "rbd", "cpad_p04", "cpad_p05", "agno", "grado"),
  showProgress = FALSE)
cpad_md5 <- unname(tools::md5sum(cpad_path))
cpad_bytes <- file.info(cpad_path)$size
gloss_path <- list.files(pattern = "GLOSAS.*[.]xlsx$", full.names = TRUE)
stopifnot(length(gloss_path) == 1L)
gloss_md5 <- unname(tools::md5sum(gloss_path))
gloss_bytes <- file.info(gloss_path)$size
setnames(cpad, c("idAlumno", "rbd", "cpad_p04", "cpad_p05", "agno", "grado"),
  c("simce_student_id", "rbd", "mother_education_code", "father_education_code", "simce_year", "grade"))
for (v in c("simce_student_id", "rbd", "mother_education_code", "father_education_code", "simce_year"))
  set(cpad, j = v, value = as.numeric(cpad[[v]]))
map_education_years <- function(x) fcase(
  x == 1L, 0, x %between% c(2L, 9L), x - 1,
  x %between% c(10L, 13L), x - 1, x == 14L, 12,
  x == 15L, 13, x %in% c(16L, 17L), 14,
  x == 18L, 16, x == 19L, 18, x == 20L, 21,
  default = NA_real_)
cpad[, `:=`(mother_education_years = map_education_years(mother_education_code),
  father_education_years = map_education_years(father_education_code))]
cpad[, family_mean_parent_education_years := rowMeans(
  cbind(mother_education_years, father_education_years), na.rm = TRUE)]
cpad[!is.finite(family_mean_parent_education_years), family_mean_parent_education_years := NA_real_]
cpad[, `:=`(mother_education_observed = !is.na(mother_education_years),
  father_education_observed = !is.na(father_education_years),
  any_parent_education_observed = !is.na(family_mean_parent_education_years))]

setwd(project_root)
source("code/codex/school_preference_va/parent_education_common.R")
check("2022 CPAD grade and year are correct", nrow(cpad) == uniqueN(cpad$simce_student_id) &&
  all(cpad$simce_year == 2022L) && all(cpad$grade == "2m"))
check("Education conversion keeps only documented response codes", all(is.na(cpad$mother_education_years) |
  cpad$mother_education_code %in% 1:20) && all(is.na(cpad$father_education_years) |
  cpad$father_education_code %in% 1:20))
check("Family means equal the available mother/father response average", {
  expected <- rowMeans(cbind(cpad$mother_education_years, cpad$father_education_years), na.rm = TRUE)
  expected[!is.finite(expected)] <- NA_real_
  same_missing <- identical(is.na(expected), is.na(cpad$family_mean_parent_education_years))
  same_values <- all(abs(expected[!is.na(expected)] - cpad$family_mean_parent_education_years[
    !is.na(expected)]) < 1e-12)
  same_missing && same_values
})

school <- cpad[is.finite(rbd) & rbd > 0L, .(
  mean_parent_education_years = mean(family_mean_parent_education_years, na.rm = TRUE),
  n_simce_students = .N, n_families_observed = sum(any_parent_education_observed),
  n_mother_responses = sum(mother_education_observed), n_father_responses = sum(father_education_observed),
  n_both_parents = sum(mother_education_observed & father_education_observed),
  family_response_share = mean(any_parent_education_observed)), by = rbd]
school[!is.finite(mean_parent_education_years), mean_parent_education_years := NA_real_]
check("School survey means are unique by destination RBD", !anyDuplicated(school$rbd))
check("School means have valid respondent counts", all(school$n_families_observed <= school$n_simce_students) &&
  all(school$n_mother_responses <= school$n_simce_students) && all(school$n_father_responses <= school$n_simce_students))

full <- as.data.table(readRDS(file.path(base_paes_dir, "ranking_paes_m1.rds")))
roster <- fread(file.path(base_paes_dir, "ranking_roster.csv"))
check("Source ranking data retain the full paper list population", nrow(full) == 1007253L &&
  nrow(roster) == 294198L && !anyDuplicated(full[, .(ranking_id, rbd)]))
school_match <- match(full$rbd, school$rbd)
full[, `:=`(mean_parent_education_years = school$mean_parent_education_years[school_match],
  n_simce_students_parent_survey = school$n_simce_students[school_match],
  n_families_parent_education_observed = school$n_families_observed[school_match])]
full[, parent_education_school_observed := is.finite(mean_parent_education_years)]
list_coverage <- full[, .(list_all_parent_education_observed = all(parent_education_school_observed)), by = ranking_id]
roster <- merge(roster, list_coverage, by = "ranking_id", all.x = TRUE, sort = FALSE)
roster[is.na(list_all_parent_education_observed), list_all_parent_education_observed := FALSE]
roster[, parent_education_estimation_eligible := paes_estimation_eligible & list_all_parent_education_observed]
check("Every original applicant receives one parent-education list flag", nrow(roster) == 294198L &&
  !anyDuplicated(roster$ranking_id) && !anyNA(roster$list_all_parent_education_observed))
check("Only complete lists are eligible for the parent-education model", all(roster$parent_education_estimation_eligible ==
  (roster$paes_estimation_eligible & roster$list_all_parent_education_observed)))
roster_index <- roster[, .(ranking_id, sample, continuation_group, prioritario, group,
  parent_education_estimation_eligible)]
full <- merge(full, roster_index, by = "ranking_id", all.x = TRUE, sort = FALSE)
check("Full ranking rows preserve every original school pair", nrow(full) == 1007253L &&
  !anyDuplicated(full[, .(ranking_id, rbd)]))
base_est <- as.data.table(readRDS(paes_input_path))
eligible_ids <- roster[parent_education_estimation_eligible == TRUE, ranking_id]
est <- copy(base_est[ranking_id %in% eligible_ids])
new_school_index <- match(est$rbd, school$rbd)
est[, `:=`(mean_parent_education_years = school$mean_parent_education_years[new_school_index],
  n_simce_students_parent_survey = school$n_simce_students[new_school_index],
  n_families_parent_education_observed = school$n_families_observed[new_school_index])]
check("New estimation data preserve complete PAES rankings and all alternatives", nrow(est) > 0L &&
  !anyDuplicated(est[, .(ranking_id, rbd)]) && all(est[, .N, by = ranking_id]$N >= 2L) &&
  setequal(unique(est$ranking_id), roster[parent_education_estimation_eligible == TRUE, ranking_id]))

reference_schools <- unique(est[, .(rbd, mean_parent_education_years)])
reference_mean <- mean(reference_schools$mean_parent_education_years)
reference_sd <- sd(reference_schools$mean_parent_education_years)
check("School-level parent education has finite positive standard deviation", is.finite(reference_mean) &&
  is.finite(reference_sd) && reference_sd > 0)
est[, z_parent_education := (mean_parent_education_years - reference_mean) / reference_sd]
normalization <- data.table(metric = "mean_parent_education_years", cohort = "SIMCE 2nd-year high school 2022",
  reference_schools = nrow(reference_schools), reference_mean_years = reference_mean,
  reference_sd_years = reference_sd, weighting = "Equal weight per unique destination RBD in pooled estimation sample",
  missing_rule = "Exclude entire ranking if any listed destination school lacks a valid school mean")
full[, parent_education_z := (mean_parent_education_years - reference_mean) / reference_sd]

flow <- rbindlist(lapply(c("pooled", setdiff(names(group_labels), "pooled")), function(g) {
  x <- if (g == "pooled") roster else roster[group == g]
  data.table(group = g, label = group_labels[[g]],
    original_applicants = if (g == "pooled") nrow(roster) else sum(roster$group == g),
    paes_m1_eligible = sum(x$paes_estimation_eligible),
    excluded_missing_parent_education = sum(x$paes_estimation_eligible & !x$list_all_parent_education_observed),
    parent_education_estimation_applicants = sum(x$parent_education_estimation_eligible))
}))
annual_flow <- roster[, .(original_applicants = .N, paes_m1_eligible = sum(paes_estimation_eligible),
  excluded_missing_parent_education = sum(paes_estimation_eligible & !list_all_parent_education_observed),
  parent_education_estimation_applicants = sum(parent_education_estimation_eligible)), by = .(sae_proceso)]
fwrite(school, file.path(clean_dir, "school_parent_education_2022.csv"))
fwrite(normalization, file.path(clean_dir, "parent_education_normalization.csv"))
fwrite(flow, file.path(clean_dir, "sample_flow.csv"))
fwrite(annual_flow, file.path(clean_dir, "sample_flow_by_year.csv"))
fwrite(roster, file.path(clean_dir, "ranking_roster.csv"))
saveRDS(full, file.path(clean_dir, "ranking_parent_education_2022.rds"))
saveRDS(est, parent_education_input_path)
fwrite(checks, file.path(clean_dir, "build_verification.csv"))
manifest <- data.table(role = c("2022 SIMCE CPAD raw", "2022 SIMCE gloss workbook",
  "PAES-M1 school-ranking input", "PAES-M1 applicant roster"),
  source = c("2022 grade-2m SIMCE CPAD private CSV, relative to source folder", "2022 private SIMCE questionnaire glossary",
    file.path("data/clean/school_preference_va/paes_m1", "ranking_paes_m1.rds"),
    file.path("data/clean/school_preference_va/paes_m1", "ranking_roster.csv")),
  md5 = c(cpad_md5, gloss_md5,
    unname(tools::md5sum(file.path(base_paes_dir, "ranking_paes_m1.rds"))),
    unname(tools::md5sum(file.path(base_paes_dir, "ranking_roster.csv")))))
manifest[, bytes := c(cpad_bytes, gloss_bytes,
  file.info(file.path(base_paes_dir, "ranking_paes_m1.rds"))$size,
  file.info(file.path(base_paes_dir, "ranking_roster.csv"))$size)]
fwrite(manifest, file.path(clean_dir, "source_manifest.csv"))
writeLines(c(
  "School-average parent education from 2022 SIMCE 2nd-year-high-school CPAD.",
  "School RBD comes directly from CPAD; each SIMCE student/respondent contributes one family average.",
  "Mother is cpad_p04 and father is cpad_p05. Valid response codes1-20 follow the 2022 glossary; 0,21,99 and other codes are missing.",
  "Education-year conversion: no schooling0; basic grades1-8 map to1-8; secondary grades I-IV map to9-12; IV/V TP12; incomplete/complete CFT-IP13/14; incomplete university14; complete university16; master's18; doctorate21.",
  "For each family, average available mother/father years; then average family values within school. A family with one valid parent response contributes one family mean.",
  "Parent education is standardized across unique destination schools in the new pooled estimation sample, equally weighted by school.",
  "Whole submitted lists are excluded when any listed school lacks a school mean; lists are never shortened.",
  "SIMCE 2022 postdates SAE application cohorts2018-2020; estimates are descriptive and do not establish information available when choices were made."
), file.path(clean_dir, "construction_definition.txt"))
message("Built 2022 parent-education means for ", nrow(school), " schools; complete parent-education rankings: ",
  uniqueN(est$ranking_id), "; all ", nrow(checks), " build checks passed.")
print(flow)
