# Join the existing grade-4-SIMCE-derived school income-decile composition to
# the complete submitted rankings already used in the parent-education model.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
source("code/codex/school_preference_va/parent_income_common.R")
source_school_path <- file.path(root, "data/clean/staff_lasso_va/school_student_composition.csv")
stopifnot(file.exists(source_school_path))
source_md5 <- unname(tools::md5sum(source_school_path)); source_bytes <- file.info(source_school_path)$size
composition <- fread(source_school_path, select = c("RBD", "N_STUDENTS", "income_decile_mean", "income_imputed_share"))
setnames(composition, c("RBD", "N_STUDENTS", "income_decile_mean", "income_imputed_share"),
  c("rbd", "n_students_income_composition", "income_decile_mean", "income_imputed_share"))
composition[, rbd := as.integer(rbd)]
check("Income composition has unique numeric school RBDs", !anyDuplicated(composition$rbd) &&
  all(is.finite(composition$rbd)))
check("Income deciles are within the established 1-10 range", all(composition[is.finite(income_decile_mean), income_decile_mean] >= 1 &
  composition[is.finite(income_decile_mean), income_decile_mean] <= 10))
check("Composition source preserves its student and imputation counts", all(composition$n_students_income_composition > 0) &&
  all(composition$income_imputed_share >= 0 & composition$income_imputed_share <= 1))

full <- as.data.table(readRDS(file.path(parent_education_dir, "ranking_parent_education_2022.rds")))
roster <- fread(file.path(parent_education_dir, "ranking_roster.csv"))
check("Ranking source retains original school pairs and applicant roster", nrow(full) == 1007253L &&
  !anyDuplicated(full[, .(ranking_id, rbd)]) && nrow(roster) == 294198L)
school_match <- match(full$rbd, composition$rbd)
full[, `:=`(income_decile_mean = composition$income_decile_mean[school_match],
  n_students_income_composition = composition$n_students_income_composition[school_match],
  income_imputed_share = composition$income_imputed_share[school_match])]
full[, income_school_observed := is.finite(income_decile_mean)]
income_coverage <- full[, .(list_all_income_deciles_observed = all(income_school_observed)), by = ranking_id]
roster <- merge(roster, income_coverage, by = "ranking_id", all.x = TRUE, sort = FALSE)
roster[is.na(list_all_income_deciles_observed), list_all_income_deciles_observed := FALSE]
roster[, income_estimation_eligible := parent_education_estimation_eligible & list_all_income_deciles_observed]
check("Income estimation sample is nested in parent-education sample", all(!roster$income_estimation_eligible |
  roster$parent_education_estimation_eligible))
roster_index <- roster[, .(ranking_id, group, income_estimation_eligible)]
full <- merge(full, roster_index, by = "ranking_id", all.x = TRUE, sort = FALSE)
base_est <- as.data.table(readRDS(file.path(parent_education_dir, "estimation_data.rds")))
eligible_ids <- roster[income_estimation_eligible == TRUE, ranking_id]
est <- copy(base_est[ranking_id %in% eligible_ids])
school_match <- match(est$rbd, composition$rbd)
est[, `:=`(income_decile_mean = composition$income_decile_mean[school_match],
  n_students_income_composition = composition$n_students_income_composition[school_match],
  income_imputed_share = composition$income_imputed_share[school_match])]
check("Estimation data preserve complete submitted lists", nrow(est) > 0L &&
  !anyDuplicated(est[, .(ranking_id, rbd)]) && all(est[, .N, by = ranking_id]$N >= 2L) &&
  setequal(unique(est$ranking_id), eligible_ids))
reference <- unique(est[, .(rbd, income_decile_mean)])
reference_mean <- mean(reference$income_decile_mean); reference_sd <- sd(reference$income_decile_mean)
check("Destination-school income decile has positive finite SD", is.finite(reference_mean) && is.finite(reference_sd) && reference_sd > 0)
est[, z_parent_income_decile := (income_decile_mean - reference_mean) / reference_sd]
full[, parent_income_decile_z := (income_decile_mean - reference_mean) / reference_sd]
normalization <- data.table(metric = "income_decile_mean", source = "Existing grade-4 SIMCE parent-survey income decile",
  reference_schools = nrow(reference), reference_mean = reference_mean, reference_sd = reference_sd,
  weighting = "Equal weight per unique destination RBD in pooled estimation sample",
  missing_rule = "Exclude entire ranking if any listed school lacks the existing school-level mean")
flow <- rbindlist(lapply(c("pooled", setdiff(names(group_labels), "pooled")), function(g) {
  x <- if (g == "pooled") roster else roster[group == g]
  data.table(group = g, label = group_labels[[g]], parent_education_applicants = sum(x$parent_education_estimation_eligible),
    excluded_missing_income_decile = sum(x$parent_education_estimation_eligible & !x$list_all_income_deciles_observed),
    income_decile_estimation_applicants = sum(x$income_estimation_eligible))
}))
annual_flow <- roster[, .(parent_education_applicants = sum(parent_education_estimation_eligible),
  excluded_missing_income_decile = sum(parent_education_estimation_eligible & !list_all_income_deciles_observed),
  income_decile_estimation_applicants = sum(income_estimation_eligible)), by = sae_proceso]
fwrite(composition, file.path(clean_dir, "school_parent_income_decile_4to.csv"))
fwrite(normalization, file.path(clean_dir, "parent_income_normalization.csv"))
fwrite(flow, file.path(clean_dir, "sample_flow.csv")); fwrite(annual_flow, file.path(clean_dir, "sample_flow_by_year.csv"))
fwrite(roster, file.path(clean_dir, "ranking_roster.csv")); saveRDS(full, file.path(clean_dir, "ranking_parent_income_decile_4to.rds"))
saveRDS(est, parent_income_input_path); fwrite(checks, file.path(clean_dir, "build_verification.csv"))
manifest <- data.table(role = c("Existing school-level grade-4-SIMCE income composition", "Parent-education ranking input"),
  source = c("data/clean/staff_lasso_va/school_student_composition.csv", "data/clean/school_preference_va/parent_education_2022"),
  md5 = c(source_md5, unname(tools::md5sum(file.path(parent_education_dir, "ranking_parent_education_2022.rds")))),
  bytes = c(source_bytes, file.info(file.path(parent_education_dir, "ranking_parent_education_2022.rds"))$size))
fwrite(manifest, file.path(clean_dir, "source_manifest.csv"))
writeLines(c("School-level mean income decile reuses the existing grade-4-SIMCE parent-survey measure in staff_lasso_va/school_student_composition.csv.",
  "The student income_decile_imputed measure is averaged among students in grades-8 cohorts 2017-2020 who attended each RBD; observed and baseline-context-imputed values are retained as in the established construction.",
  "The preference regressor is standardized across unique destination schools in the final pooled estimation sample, equally weighted by school.",
  "The ranking sample is nested within the existing complete parent-education sample. A ranking is excluded whole if any listed school lacks an income mean; lists are never shortened.",
  "This is a retrospective school-composition measure formed from grade-4 SIMCE reports of students subsequently attending the school; it is not necessarily information observed by 2018-2020 applicants."),
  file.path(clean_dir, "construction_definition.txt"))
message("Joined existing grade-4-SIMCE income deciles for ", nrow(composition), " schools; retained ", uniqueN(est$ranking_id),
  " complete rankings; all ", nrow(checks), " build checks passed.")
print(flow)
