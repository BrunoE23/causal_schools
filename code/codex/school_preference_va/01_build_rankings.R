# Build submitted student-school rankings for the joint VA preference model.
# Run from the repository root; raw inputs and existing VA estimates are read-only.
source(file.path("code", "codex", "school_preference_va", "common.R"))
source(file.path(code_dir, "geography.R"))

message("Reading the existing Table 2 applicant roster and pooled EB school values.")
roster <- as.data.table(readRDS(roster_path))[, .(mrun, sae_proceso, cohort_gr8)]
setorder(roster, sae_proceso, mrun)
roster[, ranking_id := .I]
check("paper_roster_294198_unique_students", nrow(roster) == 294198L && !anyDuplicated(roster$mrun))
check("paper_roster_timely_2018_2020", all(roster$sae_proceso %in% 2018:2020 & roster$cohort_gr8 == roster$sae_proceso))

raw_columns <- c("mrun", "cod_nivel", "rbd", "cod_curso", "preferencia_postulante",
  "agregada_por_continuidad", "prioridad_matriculado", "prioridad_hermano",
  "prioridad_hijo_funcionario", "prioridad_exalumno")
app_paths <- vapply(2018:2020, one_source, character(1L), prefix = "C1")
raw_counts <- list()
programs <- rbindlist(lapply(seq_along(app_paths), function(k) {
  year <- (2018:2020)[k]
  message("Reading genuine submitted preferences: SAE ", year)
  a <- fread(app_paths[k], sep = ";", select = raw_columns,
    colClasses = list(character = "cod_curso"), showProgress = FALSE)
  a[, source_row := .I]
  a <- a[cod_nivel == 9L]
  a[, sae_proceso := year]
  a <- a[roster[sae_proceso == year], on = .(mrun, sae_proceso), nomatch = 0L]
  check(paste0("continuity_flag_valid_", year), !anyNA(a$agregada_por_continuidad) && all(a$agregada_por_continuidad %in% 0:1))
  raw_counts[[as.character(year)]] <<- data.table(sae_proceso = year,
    raw_program_rows = nrow(a), auto_continuity_rows_removed = sum(a$agregada_por_continuidad == 1L),
    genuine_program_rows = sum(a$agregada_por_continuidad == 0L), roster_rankings = roster[sae_proceso == year, .N])
  # Keep genuine choices with sibling, incumbent or other priority; those are
  # submitted preferences. Only system-added continuity alternatives are removed.
  a <- a[agregada_por_continuidad == 0L]
  setnames(a, "preferencia_postulante", "rank_program")
  a
}))
setorder(programs, ranking_id, rank_program)
check("genuine_program_rows_1013909", nrow(programs) == 1013909L)
check("every_roster_student_has_submitted_list", uniqueN(programs$ranking_id) == nrow(roster))
check("valid_positive_program_rank", !anyNA(programs$rank_program) && all(programs$rank_program >= 1L))
check("unique_program_alternatives", !anyDuplicated(programs[, .(ranking_id, rbd, cod_curso)]))
check("unique_program_ranks", !anyDuplicated(programs[, .(ranking_id, rank_program)]))
list_counts <- programs[, .(n_programs = .N, n_schools = uniqueN(rbd),
  min_program_rank = min(rank_program), max_program_rank = max(rank_program)), by = ranking_id]
check("submitted_program_ranks_contiguous", all(list_counts$min_program_rank == 1L & list_counts$max_program_rank == list_counts$n_programs))

# A school repeats nonadjacently when another RBD occurs between its first and
# last programs. Preserve the flag; the approved school ranking uses its first
# submitted position, rather than treating repeated programs as separate schools.
school_runs <- programs[, .(programs_at_school = .N, first_program_rank = min(rank_program),
  last_program_rank = max(rank_program)), by = .(ranking_id, rbd)]
interleaved <- school_runs[, .(interleaved_school_programs = any(last_program_rank - first_program_rank + 1L > programs_at_school)), by = ranking_id]
roster <- merge(roster, list_counts, by = "ranking_id", sort = FALSE)
roster <- merge(roster, interleaved, by = "ranking_id", sort = FALSE)
roster[, `:=`(repeat_school_programs = n_programs > n_schools,
  multi_program_single_school = n_programs > 1L & n_schools == 1L)]
check("repeated_rbd_lists_4875", sum(roster$repeat_school_programs) == 4875L)
check("interleaved_rbd_lists_278", sum(roster$interleaved_school_programs) == 278L)

va <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome",
  "controlled_value_added_eb_centered_student"), showProgress = FALSE)
va <- va[analysis_sample == "All" & outcome %in% specs$outcome]
check("va_unique_school_outcome", !anyDuplicated(va[, .(school_rbd, outcome)]))
check("available_va_values_finite", all(is.finite(va$controlled_value_added_eb_centered_student)))
wide <- dcast(va, school_rbd ~ outcome, value.var = "controlled_value_added_eb_centered_student")
setnames(wide, c("school_rbd", specs$outcome), c("rbd", specs$raw))
selected_complete_schools <- sum(complete.cases(wide[, specs$raw, with = FALSE]))
check("selected_va_joint_school_support_positive", selected_complete_schools > 1L,
  paste(selected_complete_schools, "schools with all", nrow(specs), "selected VA measures"))
programs <- merge(programs, wide, by = "rbd", all.x = TRUE, sort = FALSE)

# Use Directorio school-level TP offerings, including mixed HC/TP schools.
# Verify agreement with the paper's existing classification.
# Unmatched schools remain missing; they are never implicitly coded as HC.
tp_path <- file.path(root, "data/clean/staff_lasso_va/school_predictors.csv")
artistic_path <- file.path(root, "data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv")
tp_map <- fread(tp_path, select = c("RBD", "school__has_tp_or_artistic"), showProgress = FALSE)
artistic <- fread(artistic_path, select = "RBD", showProgress = FALSE)
check("tp_source_school_keys_unique", !anyDuplicated(tp_map$RBD))
check("artistic_source_school_keys_unique", !anyDuplicated(artistic$RBD))
check("tp_or_artistic_source_binary", all(tp_map$school__has_tp_or_artistic %in% 0:1))
tp_map[, tp_school := as.integer(school__has_tp_or_artistic - as.integer(RBD %in% artistic$RBD))]
check("tp_school_map_binary_after_artistic_exclusion", all(tp_map$tp_school %in% 0:1))
setnames(tp_map, "RBD", "rbd")
# Verify the reused paper flag directly against Directorio teaching codes.
tp_directory_path <- list.files(file.path(data_root, "data/raw/school_directory/2024"),
  pattern = "[.]csv$", full.names = TRUE)
check("one_2024_directory_source", length(tp_directory_path) == 1L)
tp_header <- names(fread(tp_directory_path, nrows = 0L))
tp_ens <- grep("^ENS_", toupper(tp_header), value = TRUE)
tp_direct <- fread(tp_directory_path, select = tp_header[match(c("RBD", tp_ens), toupper(tp_header))],
  showProgress = FALSE)
setnames(tp_direct, toupper(names(tp_direct)))
tp_direct[, tp_directory := as.integer(rowSums(.SD == 410L | .SD == 510L | .SD == 610L |
  .SD == 710L | .SD == 810L, na.rm = TRUE) > 0L), .SDcols = tp_ens]
tp_comparison <- merge(tp_map[, .(rbd, tp_school)], tp_direct[, .(rbd = RBD, tp_directory)],
  by = "rbd", all = TRUE)
check("paper_TP_matches_Directorio_teaching_codes", all(tp_comparison[!is.na(tp_directory) & !is.na(tp_school), tp_school == tp_directory]))
fwrite(tp_comparison[rbd %in% programs$rbd], file.path(clean_dir, "tp_directory_verification.csv"))
programs <- merge(programs, tp_direct[, .(rbd = RBD, tp_school = tp_directory)], by = "rbd", all.x = TRUE, sort = FALSE)
setorder(programs, ranking_id, rank_program)
message("Attaching published geography and the separate home-comuna sensitivity distance.")
program_rows_before_geography <- nrow(programs)
programs <- attach_geography(programs, roster[, .(mrun, sae_proceso, cohort_gr8)])
geo_manifest <- attr(programs, "geography_manifest")
programs <- as.data.table(programs)
check("geography_preserves_all_program_rows", nrow(programs) == program_rows_before_geography)
check("geography_preserves_unique_programs", !anyDuplicated(programs[, .(ranking_id, rbd, cod_curso)]))
geo_required <- c("distance_main_km", "distance_comuna_km", "log1p_distance_main", "log1p_distance_comuna")
check("geography_distance_fields_present", all(geo_required %in% names(programs)))
for (v in geo_required) check(paste0("nonnegative_observed_", v), all(is.na(programs[[v]]) | (is.finite(programs[[v]]) & programs[[v]] >= 0)))
check("main_distance_transform_exact", all(is.na(programs$distance_main_km) | abs(log1p(programs$distance_main_km) - programs$log1p_distance_main) < 1e-12))
check("comuna_distance_transform_exact", all(is.na(programs$distance_comuna_km) | abs(log1p(programs$distance_comuna_km) - programs$log1p_distance_comuna) < 1e-12))

setorder(programs, ranking_id, rank_program)
schools <- programs[!duplicated(programs[, .(ranking_id, rbd)])]
schools[, rank_school := seq_len(.N), by = ranking_id]
schools[, first_program_rank := rank_program]
check("school_pairs_1007253", nrow(schools) == 1007253L)
check("unique_student_school_pairs", !anyDuplicated(schools[, .(ranking_id, rbd)]))
check("school_rank_first_appearance", all(schools$rank_program == school_runs[schools, on = .(ranking_id, rbd), first_program_rank]))
school_rank_lookup <- schools[, .(ranking_id, rbd, rank_school, first_program_rank)]
programs[school_rank_lookup, on = .(ranking_id, rbd), `:=`(rank_school = i.rank_school,
  first_program_rank = i.first_program_rank)]
programs[, first_school_occurrence := rank_program == first_program_rank]

raw_terms <- specs$raw
schools[, all_selected_va := rowSums(is.finite(as.matrix(.SD))) == length(raw_terms), .SDcols = raw_terms]
schools[, `:=`(main_distance_observed = is.finite(distance_main_km) & is.finite(log1p_distance_main),
  comuna_distance_observed = is.finite(distance_comuna_km) & is.finite(log1p_distance_comuna),
  tp_observed = is.finite(tp_school) & tp_school %in% 0:1)]
list_coverage <- schools[, .(list_all_selected_va = all(all_selected_va),
  list_all_tp_observed = all(tp_observed),
  list_main_distance_observed = all(main_distance_observed),
  list_comuna_distance_observed = all(comuna_distance_observed)), by = ranking_id]
roster <- merge(roster, list_coverage, by = "ranking_id", sort = FALSE)
roster[, at_least_two_schools := n_schools >= 2L]
roster[, estimation_eligible := at_least_two_schools & list_all_selected_va &
  list_main_distance_observed & list_comuna_distance_observed]
roster[, `:=`(eligible_estimation = estimation_eligible, list_complete_va = list_all_selected_va,
  list_complete_geography = list_main_distance_observed & list_comuna_distance_observed)]
roster[, exclusion_reason := fcase(!at_least_two_schools, "single_school",
  !list_all_selected_va, "missing_any_va_in_submitted_list",
  !list_main_distance_observed, "missing_main_distance_in_submitted_list",
  !list_comuna_distance_observed, "missing_comuna_distance_in_submitted_list",
  default = "included")]
check("selected_va_complete_lists_positive", sum(roster$list_all_selected_va) > 0L,
  paste(sum(roster$list_all_selected_va), "complete submitted lists"))
check("selected_va_informative_lists_positive", sum(roster$list_all_selected_va & roster$at_least_two_schools) > 0L,
  paste(sum(roster$list_all_selected_va & roster$at_least_two_schools), "complete informative submitted lists"))
check("estimation_sample_not_empty", sum(roster$estimation_eligible) > 0L)
flags <- c("n_programs", "n_schools", "repeat_school_programs", "interleaved_school_programs",
  "multi_program_single_school", "at_least_two_schools", "list_all_selected_va", "list_all_tp_observed",
  "list_main_distance_observed", "list_comuna_distance_observed", "estimation_eligible", "eligible_estimation",
  "list_complete_va", "list_complete_geography", "exclusion_reason")
for (v in flags) {
  schools[roster, on = "ranking_id", (v) := get(paste0("i.", v))]
  programs[roster, on = "ranking_id", (v) := get(paste0("i.", v))]
}

# Standardize each pooled EB index using equal weight per unique destination
# school in the final shared estimation support. Application frequency and list
# length do not determine the reference moments. SD uses the usual N-1 divisor.
normalization_schools <- unique(schools[estimation_eligible == TRUE, c("rbd", raw_terms), with = FALSE], by = "rbd")
normalization <- rbindlist(lapply(seq_len(nrow(specs)), function(k) {
  v <- specs$raw[k]
  data.table(outcome = specs$outcome[k], label = specs$label[k], raw_variable = v,
    standardized_variable = specs$standardized[k], reference_mean = mean(normalization_schools[[v]]),
    reference_sd = sd(normalization_schools[[v]]), n_reference_schools = nrow(normalization_schools),
    weighting = "Equal weight per unique RBD in final pooled estimation sample",
    sd_denominator = "N-1", source_va = "All; controlled_value_added_eb_centered_student; cohorts_2017_2020")
}))
check("positive_finite_normalization_sds", all(is.finite(normalization$reference_sd) & normalization$reference_sd > 0))
for (k in seq_len(nrow(specs))) {
  raw_v <- specs$raw[k]; z_v <- specs$standardized[k]
  mu <- normalization$reference_mean[k]; sig <- normalization$reference_sd[k]
  schools[, (z_v) := (get(raw_v) - mu) / sig]
  programs[, (z_v) := (get(raw_v) - mu) / sig]
  z_reference <- (normalization_schools[[raw_v]] - mu) / sig
  check(paste0("normalized_school_moments_", z_v), abs(mean(z_reference)) < 1e-10 && abs(sd(z_reference) - 1) < 1e-10)
}

# Coverage is assessed on original complete submitted lists. Missing alternatives
# never silently disappear from an otherwise retained estimation ranking.
message("Saving coverage, rank integrity and shared-sample diagnostics.")
coverage <- rbindlist(lapply(c(2018:2020, NA_integer_), function(year) {
  x <- if (is.na(year)) schools else schools[sae_proceso == year]
  z <- if (is.na(year)) roster else roster[sae_proceso == year]
  rbindlist(lapply(seq_len(nrow(specs) + 1L), function(k) {
    joint <- k > nrow(specs)
    covered <- if (joint) x$all_selected_va else is.finite(x[[specs$raw[k]]])
    q <- data.table(ranking_id = x$ranking_id, covered = covered)[, .(complete_list = all(covered)), by = ranking_id]
    q <- merge(q, z[, .(ranking_id, at_least_two_schools)], by = "ranking_id", sort = FALSE)
    data.table(sae_proceso = if (is.na(year)) "All" else as.character(year),
      dimension = if (joint) "All four jointly" else specs$label[k],
      outcome = if (joint) "all_selected_va" else specs$outcome[k],
      school_pairs = nrow(x), covered_school_pairs = sum(covered),
      program_rows = sum(x$n_programs[!duplicated(x$ranking_id)]),
      schools = uniqueN(x$rbd), covered_schools = uniqueN(x$rbd[covered]),
      rankings = nrow(q), complete_rankings = sum(q$complete_list),
      complete_ranking_share = mean(q$complete_list),
      complete_informative_rankings = sum(q$complete_list & q$at_least_two_schools))
  }))
}))

flow_for <- function(year) {
  z <- if (is.na(year)) roster else roster[sae_proceso == year]
  stages <- list(
    all_submitted = rep(TRUE, nrow(z)),
    two_or_more_schools = z$at_least_two_schools,
    all_selected_va = z$at_least_two_schools & z$list_all_selected_va,
    main_distance = z$at_least_two_schools & z$list_all_selected_va & z$list_main_distance_observed,
    shared_main_and_comuna_distance = z$estimation_eligible)
  rbindlist(lapply(seq_along(stages), function(k) {
    ids <- z$ranking_id[stages[[k]]]
    x <- schools[ranking_id %in% ids]
    data.table(sae_proceso = if (is.na(year)) "All" else as.character(year), stage_order = k,
      stage = names(stages)[k], rankings = length(ids), unique_students = uniqueN(x$mrun),
      school_pairs = nrow(x), unique_schools = uniqueN(x$rbd), share_of_roster = length(ids) / nrow(z))
  }))
}
flow <- rbindlist(lapply(c(2018:2020, NA_integer_), flow_for))
tp_coverage <- rbindlist(lapply(c(2018:2020, NA_integer_), function(year) {
  x <- if (is.na(year)) schools else schools[sae_proceso == year]
  z <- if (is.na(year)) roster else roster[sae_proceso == year]
  rbindlist(lapply(c("All submitted rankings", "Final estimation rankings"), function(population) {
    if (population == "Final estimation rankings") {
      x <- x[estimation_eligible == TRUE]
      z <- z[estimation_eligible == TRUE]
    }
    data.table(sae_proceso = if (is.na(year)) "All" else as.character(year), population = population,
      rankings = nrow(z), complete_tp_rankings = sum(z$list_all_tp_observed),
      complete_tp_ranking_share = mean(z$list_all_tp_observed), school_pairs = nrow(x),
      classified_school_pairs = sum(x$tp_observed), missing_school_pairs = sum(!x$tp_observed),
      tp_school_pairs = sum(x$tp_school == 1L, na.rm = TRUE),
      unique_schools = uniqueN(x$rbd), classified_schools = uniqueN(x$rbd[x$tp_observed]),
      missing_schools = uniqueN(x$rbd[!x$tp_observed]),
      tp_schools = uniqueN(x$rbd[x$tp_school %in% 1L]),
      definition = "2024 directory school TP offerings; mixed HC/TP=1; pure artistic=0; missing retained as NA")
  }))
}))
attrition <- roster[, .(rankings = .N, program_rows = sum(n_programs), school_pairs = sum(n_schools)),
  by = .(sae_proceso, exclusion_reason)]
attrition_all <- roster[, .(sae_proceso = "All", rankings = .N,
  program_rows = sum(n_programs), school_pairs = sum(n_schools)), by = exclusion_reason]
attrition[, sae_proceso := as.character(sae_proceso)]
attrition <- rbindlist(list(attrition, attrition_all), use.names = TRUE)
audit <- merge(rbindlist(raw_counts), roster[, .(genuine_rankings = .N,
  unique_school_pairs = sum(n_schools), single_school_rankings = sum(n_schools == 1L),
  repeated_school_rankings = sum(repeat_school_programs), interleaved_school_rankings = sum(interleaved_school_programs),
  multi_program_single_school_rankings = sum(multi_program_single_school),
  maximum_program_list_length = max(n_programs), maximum_school_list_length = max(n_schools)), by = sae_proceso],
  by = "sae_proceso", sort = TRUE)

estimation_columns <- c("mrun", "ranking_id", "sae_proceso", "rbd", "rank_school",
  specs$standardized, controls, "log1p_distance_main", "log1p_distance_comuna")
estimation <- schools[estimation_eligible == TRUE, ..estimation_columns]
setorder(estimation, ranking_id, rank_school)
check("estimation_no_missing_covariates", all(vapply(estimation, function(x) all(is.finite(x)), logical(1L))))
check("estimation_lists_complete", nrow(estimation) == sum(roster[estimation_eligible == TRUE, n_schools]))
check("estimation_has_at_least_two_schools", all(estimation[, .N, by = ranking_id]$N >= 2L))
check("estimation_rank_sequences_complete", all(estimation[, .(ok = min(rank_school) == 1L && max(rank_school) == .N), by = ranking_id]$ok))
check("same_student_clusters_as_ranking_ids", uniqueN(estimation$mrun) == uniqueN(estimation$ranking_id))

setorder(programs, ranking_id, rank_program)
setorder(schools, ranking_id, rank_school)
setorder(roster, ranking_id)
input_manifest <- rbindlist(list(manifest_rows(roster_path, "Existing Table 2 applicant roster"),
  manifest_rows(va_path, "Existing pooled controlled EB school VA"),
  manifest_rows(tp_path, "Paper school-level TP-or-artistic classification from 2024 directory"),
  manifest_rows(artistic_path, "Pure artistic RBDs excluded from the paper TP indicator"),
  manifest_rows(tp_directory_path, "Direct verification against 2024 Directorio ENS TP teaching codes"),
  manifest_rows(app_paths, "Regular-round submitted rankings, before continuity exclusion"),
  as.data.table(geo_manifest)), use.names = TRUE, fill = TRUE)
input_manifest <- unique(input_manifest)
saveRDS(programs, file.path(clean_dir, "program_rankings.rds"))
saveRDS(schools, file.path(clean_dir, "student_school_rankings.rds"))
saveRDS(estimation, file.path(clean_dir, "estimation_data.rds"))
fwrite(roster, file.path(clean_dir, "ranking_roster.csv"))
fwrite(normalization, file.path(clean_dir, "va_normalization.csv"))
fwrite(coverage, file.path(clean_dir, "va_coverage.csv"))
fwrite(flow, file.path(clean_dir, "sample_flow.csv"))
fwrite(tp_coverage, file.path(clean_dir, "tp_coverage.csv"))
fwrite(attrition, file.path(clean_dir, "sample_attrition.csv"))
fwrite(audit, file.path(clean_dir, "ranking_audit.csv"))
fwrite(input_manifest, file.path(clean_dir, "source_manifest.csv"))
save_checks("data_verification.csv")
message("Saved ", format(nrow(estimation), big.mark = ","), " student-school rows in ",
  format(uniqueN(estimation$ranking_id), big.mark = ","), " shared complete rankings.")
