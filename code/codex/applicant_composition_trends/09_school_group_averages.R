# School composition averaged across 2018-2020, for three requested groups.
# Attendees explicitly include all cohort students, using most_time_RBD.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
base <- "data/clean/applicant_composition_trends"
dest <- file.path(base, "group_averages")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
universe_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
weighted_path <- file.path(base, "probability_weighted/applicant_school_frame.rds")
roster_path <- "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds"
prob_paths <- file.path(data_root, "data/clean/DA_probs", paste0("DA_probs_", 2018:2020, ".csv"))
paths <- c(universe_path, weighted_path, roster_path, prob_paths)
stopifnot(all(file.exists(paths)))
fwrite(data.table(path = paths, bytes = file.info(paths)$size, modified = as.character(file.info(paths)$mtime)),
       file.path(dest, "input_manifest.csv"))
top <- fread(file.path(base, "top100_schools.csv"), encoding = "UTF-8")
roster <- as.data.table(readRDS(roster_path))[, .(mrun, sae_proceso, cohort_gr8)]
stopifnot(nrow(roster) == 294198L, !anyDuplicated(roster$mrun), all(roster$sae_proceso == roster$cohort_gr8))

# Preflighted 1.23 GB master: select only group and baseline fields, filter early.
columns <- c("mrun", "cohort_gr8", "sae_proceso", "timely_sae", "GEN_ALU", "EDAD_ALU",
  "rbd_treated_1R", "offered_1R", "n_distinct_offered_rbd_1R", "most_time_RBD",
  "father_educ_years", "mother_educ_years", "z_sim_mat_4to", "z_sim_leng_4to",
  "income_decile_imputed", "income_decile_observed")
stopifnot(all(columns %in% names(fread(universe_path, nrows = 0L))))
message("Reading 16 selected fields from the master student universe.")
u <- fread(universe_path, select = columns, na.strings = c("", "NA"), showProgress = FALSE)
u <- u[cohort_gr8 %in% 2018:2020]
stopifnot(!anyDuplicated(u$mrun))
u[, female := fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2), NA_real_)]
u[, simce_baseline := (z_sim_mat_4to + z_sim_leng_4to) / 2]
u[, parents_education := {
  n <- as.integer(!is.na(father_educ_years)) + as.integer(!is.na(mother_educ_years))
  numerator <- fifelse(is.na(father_educ_years), 0, father_educ_years) +
               fifelse(is.na(mother_educ_years), 0, mother_educ_years)
  fifelse(n > 0L, numerator / n, NA_real_)
}]
stopifnot(all(u$income_decile_imputed %in% 1:10 | is.na(u$income_decile_imputed)),
          all(u$income_decile_observed %in% 1:10 | is.na(u$income_decile_observed)))
variables <- c("parents_education", "simce_baseline", "female", "income_decile_imputed", "income_decile_observed")
specs <- data.table(outcome = variables,
  label = c("Parents' education years", "Baseline SIMCE (average subject z)", "Female share",
            "Income decile (observed plus existing imputation)", "Income decile (observed only)"),
  primary = c(TRUE, TRUE, TRUE, TRUE, FALSE),
  scale = c(1, 1, 100, 1, 1), unit = c("years", "average subject z-score", "share (0-1)", "decile", "decile"))
fwrite(specs, file.path(dest, "outcome_definitions.csv"))

weighted <- as.data.table(readRDS(weighted_path))
baseline <- u[, c("mrun", "cohort_gr8", variables), with = FALSE]
old_check <- merge(weighted[, .(mrun, cohort_gr8, parents_education, simce_baseline, female)],
  baseline[, .(mrun, cohort_gr8, parents_education, simce_baseline, female)],
  by = c("mrun", "cohort_gr8"), suffixes = c("_cached", "_master"))
stopifnot(nrow(old_check) == nrow(weighted))
for (v in variables[1:3]) stopifnot(isTRUE(all.equal(old_check[[paste0(v, "_cached")]],
  old_check[[paste0(v, "_master")]], tolerance = 1e-12)))
weighted <- merge(weighted[, .(mrun, cohort_gr8, rbd, weight = admission_probability)],
  baseline, by = c("mrun", "cohort_gr8"), all.x = TRUE)
weighted[, group := "weighted_applicants"]

# Actual offers use the full existing SAE applicant roster, without conditioning
# on attendance at the offered school or on being a genuine top-100 listing.
applicants <- u[roster, on = .(mrun, cohort_gr8, sae_proceso), nomatch = 0L]
stopifnot(nrow(applicants) == nrow(roster), !anyNA(applicants$offered_1R),
          all(applicants$n_distinct_offered_rbd_1R <= 1L),
          all(applicants$offered_1R == as.integer(applicants$rbd_treated_1R > 0)))
offers <- applicants[rbd_treated_1R %in% top$rbd]
offers[, `:=`(rbd = rbd_treated_1R, weight = 1, group = "first_round_offered")]

# Attach offered-school probabilities only to identify the supplementary
# strictly stochastic offer subset (0 < p_school < 1), not to weight actual offers.
offer_probability_audit <- list()
offer_probs <- rbindlist(lapply(seq_along(prob_paths), function(j) {
  year <- (2018:2020)[j]
  ids <- offers[sae_proceso == year, mrun]
  p <- fread(prob_paths[j], select = c("student_id", "school_id", "prob"), showProgress = FALSE)
  p <- p[student_id %in% ids]
  stopifnot(!anyNA(p$prob), all(p$prob >= 0 & p$prob <= 1), !anyDuplicated(p[, .(student_id, school_id)]))
  mass <- p[, .(probability_mass = sum(prob)), by = student_id]
  stopifnot(nrow(mass) == length(ids), max(abs(mass$probability_mass - 1)) < 1e-6)
  p[, rbd := suppressWarnings(as.integer(sub("_.*$", "", school_id)))]
  p <- p[!is.na(rbd) & rbd > 0, .(offer_probability = sum(prob)), by = .(student_id, rbd)]
  z <- merge(offers[sae_proceso == year, .(student_id = mrun, rbd)], p, by = c("student_id", "rbd"), all.x = TRUE)
  z[is.na(offer_probability), offer_probability := 0]
  stopifnot(all(z$offer_probability >= 0 & z$offer_probability <= 1 + 1e-12))
  offer_probability_audit[[j]] <<- z[, .(cohort = year, offers = .N,
    zero_simulated_probability = sum(offer_probability == 0),
    strictly_stochastic = sum(offer_probability > 0 & offer_probability < 1),
    probability_one = sum(offer_probability >= 1))]
  setnames(z, "student_id", "mrun"); z[, cohort_gr8 := year]
}))
offers <- merge(offers, offer_probs, by = c("mrun", "cohort_gr8", "rbd"), all.x = TRUE)
offers[, genuine_listing := paste(mrun, cohort_gr8, rbd) %chin% paste(weighted$mrun, weighted$cohort_gr8, weighted$rbd)]
fwrite(offers[, .(students = .N), by = .(cohort_gr8, genuine_listing)], file.path(dest, "offer_listing_audit.csv"))
fwrite(rbindlist(offer_probability_audit), file.path(dest, "offer_probability_audit.csv"))
at_risk <- copy(offers[offer_probability > 0 & offer_probability < 1])
at_risk[, group := "offered_at_risk"]

# The user's explicit definition: all students, using school attended most often.
# No SAE, offer, age or exam restriction is added to this attendee population.
attendees <- u[most_time_RBD %in% top$rbd]
attendees[, `:=`(rbd = most_time_RBD, weight = 1, group = "actual_attendees")]
fwrite(attendees[, .(students = .N, in_existing_sae_roster = sum(mrun %in% roster$mrun),
  outside_age_12_16 = sum(is.na(EDAD_ALU) | EDAD_ALU < 12 | EDAD_ALU > 16)), by = cohort_gr8],
  file.path(dest, "attendee_population_audit.csv"))

keep <- c("mrun", "cohort_gr8", "rbd", "group", "weight", variables)
frame <- rbindlist(lapply(list(weighted, offers, attendees, at_risk), function(x) x[, ..keep]))
stopifnot(!anyDuplicated(frame[, .(mrun, cohort_gr8, rbd, group)]),
          all(frame$weight >= 0), all(frame$rbd %in% top$rbd))
saveRDS(frame, file.path(dest, "group_student_frame.rds"))
groups <- data.table(group = c("weighted_applicants", "first_round_offered", "actual_attendees", "offered_at_risk"),
  group_label = c("Probability-weighted applicants", "First-round SAE offers", "All actual attendees (most-time HS)", "First-round offers with 0 < p < 1"),
  primary_group = c(TRUE, TRUE, TRUE, FALSE),
  population_definition = c("Existing submitted-choice panel; timely SAE paper roster; destination-specific simulated probability weights",
    "Existing full SAE paper roster with a first-round offer to this RBD; no attendance restriction",
    "All master-universe students in grade-8 cohorts 2018-2020 whose most_time_RBD equals this RBD; no SAE or age restriction",
    "First-round offer recipients with simulated probability strictly between zero and one at their offered RBD"),
  weight_rule = c("Simulated school admission probability", rep("One per student", 3)))
fwrite(groups, file.path(dest, "group_definitions.csv"))
fwrite(data.table(column_prefix = c("mean_equal_cohort", "mean_all_three_cohorts", "mean_pooled", "n_cohorts", "coverage_pooled"),
  definition = c("Arithmetic mean of available school-cohort means within 2018-2020; consult n_cohorts",
    "Arithmetic mean of the three cohort means; missing unless all three exist",
    "Sum of student weight times characteristic divided by observed student weight across all three years",
    "Number of school-cohorts with positive observed weight for this characteristic, from zero to three",
    "Observed characteristic weight divided by total group weight across all three years")),
  file.path(dest, "averaging_definitions.csv"))
grid <- CJ(rbd = top$rbd, cohort_gr8 = 2018:2020, group = groups$group)
denoms <- frame[, .(n_students = .N, n_positive_weight = sum(weight > 0), total_weight = sum(weight)),
  by = .(rbd, cohort_gr8, group)]
denoms <- merge(grid, denoms, by = c("rbd", "cohort_gr8", "group"), all.x = TRUE)
for (v in c("n_students", "n_positive_weight", "total_weight")) set(denoms, which(is.na(denoms[[v]])), v, 0)
cohorts <- rbindlist(lapply(variables, function(v) {
  z <- frame[, {
    ok <- !is.na(get(v)) & weight > 0
    .(n_observed = sum(ok), weighted_numerator = sum(weight[ok] * get(v)[ok]),
      observed_weight = sum(weight[ok]), weight_squared_sum = sum(weight[ok]^2))
  }, by = .(rbd, cohort_gr8, group)]
  z <- merge(denoms, z, by = c("rbd", "cohort_gr8", "group"), all.x = TRUE)
  for (col in c("n_observed", "weighted_numerator", "observed_weight", "weight_squared_sum")) set(z, which(is.na(z[[col]])), col, 0)
  z[, `:=`(outcome = v, mean = fifelse(observed_weight > 0, weighted_numerator / observed_weight, NA_real_),
    coverage = fifelse(total_weight > 0, observed_weight / total_weight, NA_real_),
    effective_n = fifelse(weight_squared_sum > 0, observed_weight^2 / weight_squared_sum, NA_real_))]
  z
}))
averages <- cohorts[, {
  observed <- is.finite(mean)
  n_years <- sum(observed)
  .(n_cohorts = n_years, cohort_years = paste(cohort_gr8[observed], collapse = ";"),
    mean_equal_cohort = if (n_years > 0) mean(mean[observed]) else NA_real_,
    mean_all_three_cohorts = if (n_years == 3L) mean(mean) else NA_real_,
    mean_pooled = if (sum(observed_weight) > 0) sum(weighted_numerator) / sum(observed_weight) else NA_real_,
    n_students = sum(n_students), n_observed = sum(n_observed), total_weight = sum(total_weight),
    observed_weight = sum(observed_weight), coverage_pooled = if (sum(total_weight) > 0) sum(observed_weight) / sum(total_weight) else NA_real_,
    effective_n_pooled = if (sum(weight_squared_sum) > 0) sum(observed_weight)^2 / sum(weight_squared_sum) else NA_real_)
}, by = .(rbd, group, outcome)]
for (name in c("cohorts", "averages")) {
  x <- get(name)
  x <- merge(x, top[, .(rank, rbd, school_name, comuna)], by = "rbd", all.x = TRUE)
  x <- merge(x, specs, by = "outcome", all.x = TRUE)
  x <- merge(x, groups, by = "group", all.x = TRUE)
  setorderv(x, intersect(c("rank", "group", "outcome", "cohort_gr8"), names(x)))
  assign(name, x)
}
fwrite(cohorts, file.path(dest, "school_group_cohort_means.csv"))
fwrite(averages, file.path(dest, "school_group_averages_long.csv"))

# Main saved table: one row per school/group (300 rows), four requested measures,
# both aggregation rules, strict all-three means and per-outcome coverage counts.
main <- averages[primary_group == TRUE & primary == TRUE]
wide <- dcast(main, rank + rbd + school_name + comuna + group + group_label ~ outcome,
  value.var = c("mean_equal_cohort", "mean_pooled", "mean_all_three_cohorts", "n_cohorts", "n_observed", "coverage_pooled"))
fwrite(wide, file.path(dest, "school_group_averages_2018_2020.csv"))
schoolwide <- dcast(main, rank + rbd + school_name + comuna ~ group + outcome,
  value.var = c("mean_equal_cohort", "mean_pooled", "n_cohorts"))
fwrite(schoolwide, file.path(dest, "school_composition_100_schools.csv"))
saveRDS(list(school_group_averages = wide, school_averages = schoolwide, long = averages, cohort_means = cohorts,
             group_definitions = groups, outcome_definitions = specs),
  file.path(dest, "school_group_averages.rds"))
coverage_summary <- main[, .(schools = .N, all_three_cohorts = sum(n_cohorts == 3L),
  two_cohorts = sum(n_cohorts == 2L), one_cohort = sum(n_cohorts == 1L), no_observations = sum(n_cohorts == 0L),
  total_students = sum(n_students), observed_students = sum(n_observed), total_weight = sum(total_weight),
  coverage = sum(observed_weight) / sum(total_weight)), by = .(group, group_label, outcome, label)]
fwrite(coverage_summary, file.path(dest, "coverage_summary.csv"))
print(frame[, .(student_school_rows = .N, distinct_students = uniqueN(mrun), total_weight = sum(weight)), by = group])
print(coverage_summary[, .(group, outcome, all_three_cohorts, two_cohorts, one_cohort, no_observations, coverage)])
message("Saved ", nrow(wide), " main school-group rows and ", nrow(schoolwide), " school-wide rows.")
