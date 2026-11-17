# Retain complete original D lists with all three expanded peer populations.
source("code/codex/school_preference_va/four_specifications_common.R")
out <- file.path(base_clean_dir, "peer_populations_all_schools")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
peer_path <- file.path(root, "data/clean/applicant_composition_trends/all_school_group_averages/school_group_averages_2018_2020.csv")
peer <- fread(peer_path)
groups <- c("weighted_applicants", "first_round_offered", "actual_attendees")
metrics <- c("parents_education", "simce_baseline", "female", "income_decile_imputed")
check("Expanded peer keys are unique", !anyDuplicated(peer[, .(rbd, group)]) && setequal(peer$group, groups))
peer[, complete := Reduce(`&`, lapply(.SD, is.finite)), .SDcols = paste0("mean_equal_cohort_", metrics)]
complete_schools <- peer[, .(complete = .N == 3L && all(complete)), by = rbd][complete == TRUE, rbd]
needed <- c("ranking_id", "mrun", "rbd", "rank_school", unname(distance_vars), specification_terms$D)
d <- as.data.table(readRDS(four_spec_input_path))
year_field <- intersect(c("cohort_gr8", "sae_proceso"), names(d))[1L]
check("Cohort is available for sample accounting", !is.na(year_field))
d[, cohort_gr8 := get(year_field)]
d <- d[, c(needed, "cohort_gr8"), with = FALSE]
check("Original D population is intact", uniqueN(d$ranking_id) == 273184L && nrow(d) == 969377L && uniqueN(d$rbd) == 2481L)
flags <- d[, .(schools = .N, cohort_gr8 = cohort_gr8[1L], all_peer_complete = all(rbd %in% complete_schools)), by = ranking_id]
for (g in groups) {
  supported <- peer[group == g & complete == TRUE, rbd]
  missing <- d[, .(missing = any(!rbd %in% supported)), by = ranking_id]
  flags[missing, on = "ranking_id", (paste0("missing_", g)) := i.missing]
}
by_year <- flags[, .(original_rankings = .N, retained_rankings = sum(all_peer_complete),
  excluded_rankings = sum(!all_peer_complete), missing_applicants = sum(missing_weighted_applicants),
  missing_offers = sum(missing_first_round_offered), missing_attendees = sum(missing_actual_attendees)), by = cohort_gr8]
fwrite(by_year, file.path(out, "sample_accounting_by_year.csv"))
retained <- d[ranking_id %in% flags[all_peer_complete == TRUE, ranking_id]]
check("Every retained ranking preserves all alternatives", all(retained[, identical(as.integer(rank_school), seq_len(.N)), by = ranking_id]$V1) &&
  sum(flags[all_peer_complete == TRUE, schools]) == nrow(retained))
coverage <- data.table(stage = c("Original D common sample", "Complete lists with all three peer populations"),
  rankings = c(nrow(flags), uniqueN(retained$ranking_id)), school_pairs = c(nrow(d), nrow(retained)),
  schools = c(uniqueN(d$rbd), uniqueN(retained$rbd)))
coverage[, share_original := rankings / rankings[1L]]
fwrite(coverage, file.path(out, "preflight_coverage.csv"))
fwrite(peer[rbd %in% d$rbd, .(schools = .N, complete_schools = sum(complete)), by = group], file.path(out, "preflight_peer_support.csv"))
fwrite(peer[rbd %in% setdiff(d$rbd, complete_schools)], file.path(out, "incomplete_peer_schools.csv"))
fwrite(flags, file.path(out, "ranking_eligibility.csv"))
if (!"--accounting-only" %in% commandArgs(trailingOnly = TRUE)) saveRDS(retained[, ..needed], file.path(out, "complete_rankings.rds"))
fwrite(checks, file.path(out, "preflight_verification.csv"))
print(coverage)
print(peer[rbd %in% d$rbd, .(schools = .N, complete_schools = sum(complete)), by = group])
