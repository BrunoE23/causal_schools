# Coverage preflight for adding the supplied top-100 school peer populations.
source("code/codex/school_preference_va/four_specifications_common.R")
peer_clean_dir <- file.path(base_clean_dir, "peer_populations")
dir.create(peer_clean_dir, recursive = TRUE, showWarnings = FALSE)
peer_path <- file.path(root, "data/clean/applicant_composition_trends/group_averages/school_group_averages_2018_2020.csv")
peer <- fread(peer_path)
groups <- c("weighted_applicants", "first_round_offered", "actual_attendees")
metrics <- c("parents_education", "simce_baseline", "female", "income_decile_imputed")
peer_columns <- paste0("mean_equal_cohort_", metrics)
stopifnot(nrow(peer) == 300L, uniqueN(peer$rbd) == 100L, !anyDuplicated(peer[, .(rbd, group)]), setequal(peer$group, groups))
peer[, complete := Reduce(`&`, lapply(.SD, is.finite)), .SDcols = peer_columns]
complete_schools <- peer[, .(complete = .N == 3L && all(complete)), by = rbd][complete == TRUE, rbd]
needed <- c("ranking_id", "mrun", "rbd", "rank_school", unname(distance_vars), specification_terms$D)
message("Checking full-list coverage of the supplied 100 schools...")
d <- as.data.table(readRDS(four_spec_input_path))[, ..needed]
flags <- d[, .(n_schools = .N, all_in_100 = all(rbd %in% peer$rbd),
  all_peer_complete = all(rbd %in% complete_schools), any_in_100 = any(rbd %in% peer$rbd)), by = ranking_id]
coverage <- data.table(stage = c("Original D common sample", "At least one listed school among supplied 100", "Entire submitted list among supplied 100", "Entire list has all three complete peer groups"),
  rankings = c(nrow(flags), flags[any_in_100 == TRUE, .N], flags[all_in_100 == TRUE, .N], flags[all_peer_complete == TRUE, .N]))
coverage[, share_original := rankings / rankings[1L]]
fwrite(coverage, file.path(peer_clean_dir, "preflight_coverage.csv"))
fwrite(peer[, .(schools = .N, complete_schools = sum(complete)), by = group], file.path(peer_clean_dir, "preflight_peer_support.csv"))
restricted <- d[ranking_id %in% flags[all_peer_complete == TRUE, ranking_id]]
support <- rbindlist(lapply(c("log1p_distance_main", specification_terms$D), function(v)
  data.table(term = v, unique_values = uniqueN(restricted[[v]]), nonzero_pairs = sum(restricted[[v]] != 0))))
fwrite(support, file.path(peer_clean_dir, "preflight_D_term_support.csv"))
saveRDS(restricted, file.path(peer_clean_dir, "top100_complete_rankings.rds"))
fwrite(data.table(path = c(four_spec_input_path, peer_path), bytes = file.info(c(four_spec_input_path, peer_path))$size),
  file.path(peer_clean_dir, "preflight_source_manifest.csv"))
print(coverage); print(data.table(complete_peer_schools = length(complete_schools), used_schools = uniqueN(restricted$rbd), school_pairs = nrow(restricted)))
print(support)
