# Reuse true unadjusted school outcome means alongside the existing PAES M1 mean.
# Raw means precede control-completeness restrictions; no VA is re-estimated.
source("code/codex/school_preference_va/four_specifications_common.R")

outcome_values_path <- file.path(data_root,
  "data/clean/school_rbd_observational_values/cohorts_2017_2020/school_rbd_observational_values.csv")
input_paths <- c(four_spec_base_input_path, outcome_values_path, va_path)
check("School-mean inputs exist and are distinct", all(file.exists(input_paths)) && !anyDuplicated(input_paths))
preservation <- data.table(path = input_paths, md5_before = unname(tools::md5sum(input_paths)))
fwrite(preservation, file.path(school_mean_dir, "source_preservation.csv"))
manifest <- manifest_rows(input_paths)
manifest[, md5 := unname(tools::md5sum(path))]
fwrite(manifest, file.path(school_mean_dir, "source_manifest.csv"))

# The observational source is about 115 MB. Inspect its header, then read only
# the eight columns required for raw means, denominators, and EB provenance.
source_columns <- names(fread(outcome_values_path, nrows = 0L, showProgress = FALSE))
selected_columns <- c("school_rbd", "analysis_sample", "outcome", "raw_mean",
  "n_students_school", "n_students_outcome", "n_students_regression",
  "controlled_value_added_centered_student")
check("Observational source exposes raw means and their denominators", all(selected_columns %in% source_columns))
fwrite(data.table(path = outcome_values_path, bytes = file.info(outcome_values_path)$size,
  available_columns = length(source_columns), selected_columns = paste(selected_columns, collapse = ","),
  read_method = "data.table::fread with explicit column selection; filter All and three outcomes before joins"),
  file.path(school_mean_dir, "source_preflight.csv"))
raw_values <- fread(outcome_values_path, select = selected_columns, showProgress = FALSE)
raw_values <- raw_values[analysis_sample == "All" & outcome %chin% school_mean_specs$outcome]
check("Raw school means have unique school-outcome keys", !anyDuplicated(raw_values[, .(school_rbd, outcome)]))
check("Raw binary-outcome means are finite probabilities with positive denominators",
  all(is.finite(raw_values$raw_mean) & raw_values$raw_mean >= 0 & raw_values$raw_mean <= 1) &&
    all(raw_values$n_students_outcome > 0L & raw_values$n_students_outcome <= raw_values$n_students_school))

# The EB export omits raw means. Check that its underlying unshrunk effects and
# regression counts match this observational precursor exactly.
eb_values <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome",
  "controlled_value_added_centered_student", "n_students_regression"), showProgress = FALSE)
eb_values <- eb_values[analysis_sample == "All" & outcome %chin% school_mean_specs$outcome]
check("EB precursor keys are unique", !anyDuplicated(eb_values[, .(school_rbd, outcome)]))
provenance <- merge(eb_values, raw_values, by = c("school_rbd", "analysis_sample", "outcome"),
  suffixes = c("_eb", "_raw_source"), all.x = TRUE, sort = FALSE)
check("Every current EB estimate matches the same observational precursor",
  nrow(provenance) == nrow(eb_values) && all(is.finite(provenance$raw_mean)) &&
    all(provenance$n_students_regression_eb == provenance$n_students_regression_raw_source) &&
    all(abs(provenance$controlled_value_added_centered_student_eb -
      provenance$controlled_value_added_centered_student_raw_source) <= 1e-12))
fwrite(provenance[, .(eb_schools = .N,
  max_unshrunk_va_difference = max(abs(controlled_value_added_centered_student_eb - controlled_value_added_centered_student_raw_source)),
  identical_regression_counts = all(n_students_regression_eb == n_students_regression_raw_source)), by = outcome],
  file.path(school_mean_dir, "eb_precursor_verification.csv"))
rm(eb_values, provenance)

base <- as.data.table(readRDS(four_spec_base_input_path))
check("Previous common sample contains the expected applicants, pairs, and schools",
  uniqueN(base$ranking_id) == 273184L && nrow(base) == 969377L && uniqueN(base$rbd) == 2481L)
check("Existing submitted lists are unique and complete",
  !anyDuplicated(base[, .(ranking_id, rbd)]) && uniqueN(base$mrun) == uniqueN(base$ranking_id) &&
    all(base[, .(valid = .N >= 2L && identical(as.integer(rank_school), seq_len(.N))), by = ranking_id]$valid))
schools <- data.table(rbd = sort(unique(base$rbd)))
normalization <- list()
for (k in seq_len(nrow(school_mean_specs))) {
  s <- school_mean_specs[k]
  x <- raw_values[outcome == s$outcome]
  ix <- match(schools$rbd, x$school_rbd)
  check(paste(s$suffix, "has an observed raw mean for every destination school"),
    !anyNA(ix) && all(is.finite(x$raw_mean[ix])))
  set(schools, j = s$raw, value = x$raw_mean[ix])
  set(schools, j = paste0("n_mean_", s$suffix), value = x$n_students_outcome[ix])
  set(schools, j = paste0("n_regression_", s$suffix), value = x$n_students_regression[ix])
  mu <- mean(schools[[s$raw]])
  sigma <- sd(schools[[s$raw]])
  check(paste(s$suffix, "has a positive equal-school sample SD"), is.finite(sigma) && sigma > 0)
  set(schools, j = s$standardized, value = (schools[[s$raw]] - mu) / sigma)
  normalization[[k]] <- data.table(suffix = s$suffix, outcome = s$outcome, raw = s$raw,
    standardized = s$standardized, mean = mu, sd = sigma, schools = nrow(schools),
    weighting = "One observation per unique destination RBD, pooled across application years",
    sd_definition = "R sample SD with denominator N-1")
  check(paste(s$suffix, "standardization has zero mean and unit sample SD"),
    abs(mean(schools[[s$standardized]])) < 1e-12 && abs(sd(schools[[s$standardized]]) - 1) < 1e-12)
}
normalization <- rbindlist(normalization)
fwrite(schools, file.path(school_mean_dir, "school_outcome_means.csv"))
fwrite(normalization, file.path(school_mean_dir, "normalization.csv"))
raw_values[, in_preference_sample := school_rbd %in% schools$rbd]
fwrite(raw_values, file.path(school_mean_dir, "school_outcome_means_long.csv"))
coverage <- raw_values[, .(source_schools = .N, source_outcome_students = sum(n_students_outcome),
  source_regression_students = sum(n_students_regression, na.rm = TRUE),
  destination_schools = sum(in_preference_sample),
  destination_outcome_students = sum(n_students_outcome[in_preference_sample]),
  destination_regression_students = sum(n_students_regression[in_preference_sample], na.rm = TRUE)), by = outcome]
coverage <- merge(coverage, school_mean_specs[, .(outcome, label)], by = "outcome", sort = FALSE)
coverage[, denominator := fifelse(outcome == "admission_exam_taker",
  "All age-eligible students with a valid attended high-school RBD",
  "Admission-exam takers with an observed binary outcome; non-enrollees remain zero")]
fwrite(coverage, file.path(school_mean_dir, "coverage.csv"))
check("Source raw means retain the documented broad cohort denominators",
  coverage[outcome == "admission_exam_taker", source_outcome_students] == 931547L &&
    all(coverage[outcome != "admission_exam_taker", source_outcome_students] == 657134L))

est <- copy(base)
ix <- match(est$rbd, schools$rbd)
for (v in setdiff(names(schools), "rbd")) set(est, j = v, value = schools[[v]][ix])
check("All old variables and complete submitted lists remain exactly unchanged",
  nrow(est) == nrow(base) && all(vapply(names(base), function(v) identical(est[[v]], base[[v]]), logical(1))))
check("Every applicant retains every original ranked alternative and all new means",
  uniqueN(est$ranking_id) == 273184L && nrow(est) == 969377L &&
    all(vapply(est[, c(school_mean_specs$raw, school_mean_specs$standardized), with = FALSE],
      function(x) all(is.finite(x)), logical(1))))
saveRDS(est, four_spec_input_path, compress = "gzip")
sample_flow <- data.table(stage = c("Previous four-specification sample", "Complete school-outcome means"),
  applicants = c(uniqueN(base$ranking_id), uniqueN(est$ranking_id)),
  applicant_school_pairs = c(nrow(base), nrow(est)), distinct_schools = c(uniqueN(base$rbd), uniqueN(est$rbd)),
  excluded_rankings = 0L)
fwrite(sample_flow, file.path(school_mean_dir, "sample_flow.csv"))
fwrite(est[, .(applicants = uniqueN(ranking_id), applicant_school_pairs = .N, distinct_schools = uniqueN(rbd)), by = sae_proceso],
  file.path(school_mean_dir, "sample_flow_by_year.csv"))
school_metrics <- unique(est[, c("rbd", school_mean_terms, va_terms), with = FALSE])
check("All school-level raw means, PAES, and VAs are invariant within RBD", nrow(school_metrics) == nrow(schools))
correlation <- as.data.table(as.table(cor(as.matrix(school_metrics[, -"rbd"]))))
setnames(correlation, c("metric_1", "metric_2", "correlation"))
fwrite(correlation, file.path(school_mean_dir, "metric_correlations.csv"))

writeLines(c(
  "School outcome averages reuse the unadjusted raw_mean field from the existing All-sample observational school-value source; no VA or individual outcome is reconstructed.",
  "High-school assignment is most_time_RBD, the paper's high-school attended-most definition. Source grade-8 cohorts are 2017-2020; grade-8 age is restricted to 12-16, with a positive nonmissing school RBD.",
  "Raw means precede baseline-control completeness and therefore use a broader sample than the controlled VA regressions. n_mean_* records the actual raw-mean denominator; n_regression_* is retained separately.",
  "Exam taking means a nonmissing math_max or leng_max score and averages over all 931547 age-eligible source students, including non-takers as zero.",
  "High-premium field and institution rates average over the 657134 source admission-exam takers, including non-enrollees as zero. These rates are not conditional on higher-education enrollment.",
  "High-premium field is the existing Science/Law/Engineering/Medicine+ positive-list indicator; other observed enrollment and non-enrollment are zero.",
  "High-premium institution is the existing enrollment indicator for centered MiFuturo institution FE above 0.1; other enrollment, missing institution FE, and non-enrollment are zero.",
  "The retained math observable remains the already constructed PAES M1 school mean for admission process 2025. It is not the older cohort's exam-outcome mean, so its vintage and risk set differ from the other three averages.",
  "Each new mean is standardized with the unweighted mean and sample SD across the same 2481 destination RBDs in the fixed preference estimation sample, pooled over years. Existing PAES and VA normalizations are unchanged.",
  "All 273184 complete submitted rankings and 969377 applicant-school pairs are retained. Prior prices, parental attributes, ranks, and geographic variables are preserved exactly.",
  "School averages are descriptive later outcome/composition measures, not causal VA, and may postdate the 2018-2020 preference decisions."
), file.path(school_mean_dir, "construction_definition.txt"))
preservation[, md5_after := unname(tools::md5sum(path))]
preservation[, unchanged := md5_before == md5_after]
check("Existing source data are preserved byte for byte", all(preservation$unchanged))
fwrite(preservation, file.path(school_mean_dir, "source_preservation.csv"))
fwrite(checks, file.path(school_mean_dir, "build_verification.csv"))
writeLines(capture.output(sessionInfo()), file.path(school_mean_dir, "session_info.txt"))
message("Saved ", four_spec_input_path, ": ", uniqueN(est$ranking_id), " rankings, ", nrow(est),
  " applicant-school pairs, ", nrow(schools), " schools; ", nrow(checks), " checks passed.")
print(normalization[, .(outcome, mean, sd, schools)])
print(coverage)
