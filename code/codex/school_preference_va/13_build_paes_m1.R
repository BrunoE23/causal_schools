# Current-graduate, current-regular PAES M1 means; preserve all baseline outputs.
source("code/codex/school_preference_va/paes_common.R")
capture_prior_outputs()
processes <- c(2024L, 2025L)
raw_paths <- c(
  "2024" = file.path(data_root, "data/raw/2024/PAES-2024-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PAES_2024_PUB_MRUN.csv"),
  "2025" = file.path(data_root, "data/raw/2025/PAES_2025_Inscritos_Puntajes/A_INSCRITOS_PUNTAJES_PAES_2025_PUB_MRUN.csv"))
full_path <- file.path(base_clean_dir, "student_school_rankings.rds")
base_est_path <- file.path(base_clean_dir, "estimation_data.rds")
roster_input <- file.path(base_clean_dir, "by_priority_continuation/group_roster.csv")
va_norm_input <- file.path(base_clean_dir, "va_normalization.csv")
base_inputs <- c(full_path, base_est_path, roster_input, va_norm_input)
input_paths <- c(unname(raw_paths), base_inputs)
stopifnot(all(file.exists(input_paths)))
base_md5 <- tools::md5sum(base_inputs)
manifest <- manifest_rows(input_paths)
manifest[, md5 := unname(tools::md5sum(path))]
manifest[, role := c(rep("Raw PAES current-regular scores", length(raw_paths)),
  "Complete original submitted school rankings", "Original minimal preference estimation sample",
  "Original applicant group roster", "Original four-VA normalization")]
fwrite(manifest, file.path(clean_dir, "source_manifest.csv"))

# Each 230-234 MB raw CSV is scanned with nine selected columns (~11 MB in R).
# Header/size preflight precedes reading; no full-column raw object is constructed.
raw_columns <- c("MRUN", "RBD", "ANYO_PROCESO", "ANYO_DE_EGRESO",
  "MATE1_REG_ACTUAL", "MATE1_INV_ACTUAL", "MATE1_MAX",
  "RINDIO_PROCESO_ANTERIOR", "RINDIO_PROCESO_ACTUAL")
individual <- score_audit <- school_values <- list()
for (process in processes) {
  path <- raw_paths[[as.character(process)]]
  header <- fread(path, nrows = 0L, showProgress = FALSE)
  check(paste(process, "raw score headers complete"), all(raw_columns %in% names(header)))
  message("Reading PAES ", process, ": ", round(file.info(path)$size / 1e6, 1),
    " MB CSV; selecting ", length(raw_columns), " columns.")
  raw <- fread(path, select = raw_columns, na.strings = c("", "NA"), showProgress = FALSE)
  check(paste(process, "one row per student"), !anyDuplicated(raw$MRUN))
  check(paste(process, "process year agrees with source path"), all(raw$ANYO_PROCESO == process))
  d <- raw[, .(mrun = as.numeric(MRUN), rbd = as.numeric(RBD),
    paes_process = as.integer(ANYO_PROCESO), graduation_year = as.integer(ANYO_DE_EGRESO),
    paes_m1_regular = as.numeric(MATE1_REG_ACTUAL), paes_m1_winter = as.numeric(MATE1_INV_ACTUAL),
    paes_m1_best = as.numeric(MATE1_MAX), prior_process_flag = as.integer(RINDIO_PROCESO_ANTERIOR),
    current_process_flag = as.integer(RINDIO_PROCESO_ACTUAL))]
  d[, is_current_graduate := !is.na(graduation_year) & graduation_year == process - 1L]
  d[, valid_rbd := is.finite(rbd) & rbd > 0]
  d[, m1_regular_observed := is.finite(paes_m1_regular) & paes_m1_regular >= 100 & paes_m1_regular <= 1000]
  d[, included := is_current_graduate & valid_rbd & m1_regular_observed]
  check(paste(process, "positive current-regular scores use 100-1000 scale"),
    !any(is.finite(d$paes_m1_regular) & d$paes_m1_regular > 0 & !d$m1_regular_observed))
  school_values[[as.character(process)]] <- d[included == TRUE, .(
    graduation_year = process - 1L, paes_m1_mean = mean(paes_m1_regular),
    n_m1_takers = .N, sd_m1 = sd(paes_m1_regular),
    min_m1 = min(paes_m1_regular), max_m1 = max(paes_m1_regular)),
    by = .(rbd, paes_process)]
  score_audit[[as.character(process)]] <- data.table(
    paes_process = process, graduation_year = process - 1L,
    regular_sitting_calendar_year = process - 1L, source_rows = nrow(d),
    unique_students = uniqueN(d$mrun), selected_object_bytes = as.numeric(object.size(d)),
    current_graduate_registrants = sum(d$is_current_graduate),
    current_graduates_positive_rbd = sum(d$is_current_graduate & d$valid_rbd),
    included_current_regular_m1_takers = sum(d$included),
    covered_schools = uniqueN(d[included == TRUE, rbd]),
    current_graduate_valid_m1_missing_rbd = sum(d$is_current_graduate & d$m1_regular_observed & !d$valid_rbd),
    current_graduate_positive_rbd_missing_m1 = sum(d$is_current_graduate & d$valid_rbd & !d$m1_regular_observed),
    included_with_prior_process_flag = sum(d$included & d$prior_process_flag %in% 1L),
    valid_best_among_current_graduates_with_rbd = sum(d$is_current_graduate & d$valid_rbd &
      is.finite(d$paes_m1_best) & d$paes_m1_best >= 100 & d$paes_m1_best <= 1000),
    valid_current_regular_and_best_differ = sum(d$included & is.finite(d$paes_m1_best) &
      d$paes_m1_best > 0 & d$paes_m1_regular != d$paes_m1_best),
    mean_current_regular_m1 = mean(d[included == TRUE, paes_m1_regular]),
    score_column = "MATE1_REG_ACTUAL", graduate_restriction = "ANYO_DE_EGRESO == ANYO_PROCESO - 1",
    score_restriction = "100 <= MATE1_REG_ACTUAL <= 1000; RBD > 0",
    other_restrictions = "No GPA, SAE, prior-process, school-type, minimum-taker-count or sector filter")
  individual[[as.character(process)]] <- d
  rm(raw, d); invisible(gc())
}
school <- rbindlist(school_values)
setorder(school, paes_process, rbd)
check("Annual school score key unique", !anyDuplicated(school[, .(rbd, paes_process)]))
check("Annual school means valid and taker counts positive", all(is.finite(school$paes_m1_mean) &
  school$paes_m1_mean >= 100 & school$paes_m1_mean <= 1000 & school$n_m1_takers > 0L))
fwrite(school, file.path(clean_dir, "school_paes_m1.csv"))
fwrite(rbindlist(score_audit), file.path(clean_dir, "raw_score_audit.csv"))
saveRDS(rbindlist(individual), file.path(clean_dir, "paes_individual_scores.rds"))
rm(individual, school_values); invisible(gc())

# The existing 72 MB compressed full-ranking cache is read once, immediately
# reduced to six identity/rank columns, then released before joining score data.
message("Reading existing full ranking cache once, then immediately selecting IDs and ranks.")
full <- as.data.table(readRDS(full_path))
rank_columns <- c("ranking_id", "mrun", "sae_proceso", "rbd", "rank_school", "rank_program")
check("Full school-ranking IDs and ranks available", all(rank_columns %in% names(full)))
ranks <- copy(full[, ..rank_columns])
rm(full); invisible(gc())
check("Full rankings unique by ranking-school", !anyDuplicated(ranks[, .(ranking_id, rbd)]))
for (process in processes) {
  s <- school[paes_process == process]
  index <- match(ranks$rbd, s$rbd)
  ranks[, (paste0("paes_m1_mean_", process)) := s$paes_m1_mean[index]]
  ranks[, (paste0("n_m1_takers_", process)) := s$n_m1_takers[index]]
  ranks[, (paste0("school_m1_observed_", process)) := is.finite(s$paes_m1_mean[index])]
}
list_coverage <- ranks[, .(
  list_all_m1_observed_2024 = all(school_m1_observed_2024),
  list_all_m1_observed_2025 = all(school_m1_observed_2025)), by = ranking_id]
roster <- fread(roster_input, na.strings = c("", "NA"))
old_roster_columns <- copy(names(roster))
old_roster_snapshot <- copy(roster)
check("Original applicant roster unique by ranking", !anyDuplicated(roster$ranking_id))
check("Full school pairs cover original applicant roster", setequal(ranks$ranking_id, roster$ranking_id))
check("Original paper roster has 294198 applicants", nrow(roster) == 294198L)
for (v in c("list_all_m1_observed_2024", "list_all_m1_observed_2025")) {
  roster[, (v) := list_coverage[[v]][match(ranking_id, list_coverage$ranking_id)]]
}
roster[, list_all_m1_observed := get(paste0("list_all_m1_observed_", primary_paes_process))]
roster[, paes_estimation_eligible := estimation_eligible & list_all_m1_observed]
roster[, paes_group_eligible := group_eligible & list_all_m1_observed]
check("All original group-roster values preserved", all(vapply(old_roster_columns,
  function(v) identical(roster[[v]], old_roster_snapshot[[v]]), logical(1))))
for (v in c("list_all_m1_observed_2024", "list_all_m1_observed_2025", "list_all_m1_observed", "paes_estimation_eligible")) {
  ranks[, (v) := roster[[v]][match(ranking_id, roster$ranking_id)]]
}
ranks[, baseline_estimation_eligible := roster$estimation_eligible[match(ranking_id, roster$ranking_id)]]
ranks[, paes_m1_mean := get(paste0("paes_m1_mean_", primary_paes_process))]
ranks[, n_m1_takers := get(paste0("n_m1_takers_", primary_paes_process))]
check("Primary complete-list rule uses 2025 alone", identical(roster$paes_estimation_eligible,
  roster$estimation_eligible & roster$list_all_m1_observed_2025))
fwrite(roster, file.path(clean_dir, "ranking_roster.csv"))
setorder(ranks, ranking_id, rank_school)
saveRDS(ranks, file.path(clean_dir, "ranking_paes_m1.rds"))

message("Reading minimal baseline estimation input: ", round(file.info(base_est_path)$size / 1e6, 1), " MB.")
base_est <- as.data.table(readRDS(base_est_path))
base_columns <- names(base_est)
ids <- roster[paes_estimation_eligible == TRUE, ranking_id]
est <- copy(base_est[ranking_id %in% ids])
check("Every retained ranking was in the original estimation sample", setequal(est$ranking_id, ids))
setorder(est, ranking_id, rank_school)
baseline_retained <- base_est[ranking_id %in% ids]
setorder(baseline_retained, ranking_id, rank_school)
check("Original estimation rows and regressors preserved exactly", all(vapply(base_columns,
  function(v) identical(est[[v]], baseline_retained[[v]]), logical(1))))
primary <- school[paes_process == primary_paes_process]
index <- match(est$rbd, primary$rbd)
est[, paes_m1_mean := primary$paes_m1_mean[index]]
est[, n_m1_takers := primary$n_m1_takers[index]]
group_columns <- intersect(c("sample", "continuation_group", "prioritario", "group", "group_eligible",
  "paes_group_eligible", "list_all_m1_observed", "paes_estimation_eligible"), names(roster))
for (v in setdiff(group_columns, names(est))) {
  est[, (v) := roster[[v]][match(ranking_id, roster$ranking_id)]]
}
normalization_schools <- unique(est[, .(rbd, paes_m1_mean)], by = "rbd")
normalization <- data.table(metric = "paes_m1_mean", standardized_variable = "z_paes_m1",
  paes_process = primary_paes_process, graduation_year = primary_paes_process - 1L,
  regular_sitting_calendar_year = primary_paes_process - 1L,
  reference_mean = mean(normalization_schools$paes_m1_mean),
  reference_sd = sd(normalization_schools$paes_m1_mean),
  n_reference_schools = nrow(normalization_schools),
  weighting = "Equal weight per unique RBD in final pooled PAES-complete estimation sample",
  sd_denominator = "N-1", score_column = "MATE1_REG_ACTUAL")
check("PAES normalization SD finite and positive", is.finite(normalization$reference_sd) && normalization$reference_sd > 0)
est[, z_paes_m1 := (paes_m1_mean - normalization$reference_mean) / normalization$reference_sd]
z_schools <- (normalization_schools$paes_m1_mean - normalization$reference_mean) / normalization$reference_sd
check("PAES school normalization zero mean and unit sample SD", abs(mean(z_schools)) < 1e-12 && abs(sd(z_schools) - 1) < 1e-12)
check("Retained primary score regressors complete", all(is.finite(est$paes_m1_mean) & is.finite(est$z_paes_m1)))
check("Retained ranking lengths unchanged", all(est[, .N, by = ranking_id][
  roster[, .(ranking_id, n_schools)], on = "ranking_id", nomatch = 0L][, N == n_schools]))
check("Retained original school ranks consecutive", all(est[, .(valid = identical(as.integer(rank_school), seq_len(.N))), by = ranking_id]$valid))
check("No interaction product columns saved", !any(c(interaction_terms, metric_interactions$term) %in% names(est)))
saveRDS(est, file.path(clean_dir, "estimation_data.rds"))
fwrite(normalization, file.path(clean_dir, "paes_m1_normalization.csv"))
check("Original four-VA normalization copied", file.copy(va_norm_input,
  file.path(clean_dir, "va_normalization.csv"), overwrite = TRUE))
check("Original four-VA normalization byte identical", identical(unname(tools::md5sum(va_norm_input)),
  unname(tools::md5sum(file.path(clean_dir, "va_normalization.csv")))))

flow <- rbindlist(lapply(names(group_labels), function(group_name) {
  x <- if (group_name == "pooled") roster else roster[group == group_name]
  rbindlist(lapply(c(2018:2020, NA_integer_), function(year) {
    z <- if (is.na(year)) x else x[sae_proceso == year]
    data.table(group = group_name, label = group_labels[[group_name]],
      sae_proceso = if (is.na(year)) "All" else as.character(year),
      original_applicants = nrow(z), baseline_eligible = sum(z$estimation_eligible),
      excluded_missing_m1 = sum(z$estimation_eligible & !z$list_all_m1_observed),
      estimation_applicants = sum(z$paes_estimation_eligible),
      comparable_2024_complete_applicants = sum(z$estimation_eligible & z$list_all_m1_observed_2024),
      missing_2025_despite_2024_coverage = sum(z$estimation_eligible & !z$list_all_m1_observed_2025 & z$list_all_m1_observed_2024))
  }))
}))
check("PAES sample-flow arithmetic exact", all(flow$baseline_eligible -
  flow$excluded_missing_m1 == flow$estimation_applicants))
check("Pooled flow reproduces saved estimation rankings", flow[group == "pooled" & sae_proceso == "All", estimation_applicants] ==
  uniqueN(est$ranking_id))
fwrite(flow, file.path(clean_dir, "sample_flow.csv"))
check("Original baseline input files unchanged", identical(unname(base_md5), unname(tools::md5sum(base_inputs))))
save_checks("build_verification.csv")
writeLines(c(
  "Primary score: mean current-regular PAES M1 (MATE1_REG_ACTUAL), process 2025, graduates of 2024.",
  "The process label is the university admissions year; the regular sitting and graduation are in calendar 2024.",
  "2024 is a separate coverage comparison (graduates and regular sitting 2023), never a fallback for missing 2025.",
  "School means average observed current-graduate M1 takers; zero or missing scores are not zero-achievement pupils.",
  "No prior-process-flag, GPA, sector, school-type or SAE filter defines the raw school means.",
  "PAES is standardized across unique schools in the final pooled complete-list sample; original four-VA normalization is unchanged.",
  "The contemporary school score is a future school attribute relative to SAE 2018-2020 applications, not historical information proven available to those parents."),
  file.path(clean_dir, "score_definition.txt"))
message("PAES data ready: ", uniqueN(est$ranking_id), " complete rankings, ", nrow(est),
  " school pairs and ", nrow(normalization_schools), " schools.")
print(flow[sae_proceso == "All"])
