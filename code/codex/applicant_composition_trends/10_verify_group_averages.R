suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
base <- "data/clean/applicant_composition_trends"
dest <- file.path(base, "group_averages")
d <- as.data.table(readRDS(file.path(dest, "group_student_frame.rds")))
cohorts <- fread(file.path(dest, "school_group_cohort_means.csv"))
averages <- fread(file.path(dest, "school_group_averages_long.csv"))
main <- fread(file.path(dest, "school_group_averages_2018_2020.csv"))
schools <- fread(file.path(dest, "school_composition_100_schools.csv"))
specs <- fread(file.path(dest, "outcome_definitions.csv"))
checks <- list()
check <- function(label, passed, error = 0) {
  checks[[length(checks) + 1L]] <<- data.table(check = label, passed = isTRUE(passed), max_error = error)
  if (!isTRUE(passed)) stop(label)
}
check("requested table dimensions", nrow(main) == 300L && nrow(schools) == 100L && uniqueN(main$rbd) == 100L)
check("unique records within each group", !anyDuplicated(d[, .(group, mrun, cohort_gr8, rbd)]))
check("one attendee school per person", !anyDuplicated(d[group == "actual_attendees", mrun]))
check("one offered school per person", !anyDuplicated(d[group == "first_round_offered", mrun]))
check("realized offer and attendance groups are unweighted", d[group != "weighted_applicants", all(weight == 1)])
check("all attendee school outcomes span all three years", averages[group == "actual_attendees" & primary == TRUE, all(n_cohorts == 3L)])
check("missing cohort means never become zero", cohorts[observed_weight == 0, all(is.na(mean))])
check("strict three-year means require three years", averages[n_cohorts < 3L, all(is.na(mean_all_three_cohorts))])
check("strict and available means agree with full coverage", averages[n_cohorts == 3L, all(mean_equal_cohort == mean_all_three_cohorts)])
check("female means are shares", averages[outcome == "female", all(mean_equal_cohort >= 0 & mean_equal_cohort <= 1, na.rm = TRUE)])
check("income remains on existing decile scale", averages[grepl("income_decile", outcome),
  all(mean_equal_cohort >= 1 & mean_equal_cohort <= 10, na.rm = TRUE)])

# Direct base-R weighted.mean checks the pooled result without using cohort sums.
for (v in specs$outcome) {
  for (g in unique(d$group)) {
    x <- as.data.frame(d[group == g & weight > 0 & !is.na(get(v)), .(rbd, y = get(v), w = weight)])
    direct <- do.call(rbind, lapply(split(x, x$rbd), function(z) data.frame(rbd = z$rbd[1L], value = weighted.mean(z$y, z$w), n = nrow(z))))
    m <- merge(averages[group == g & outcome == v, .(rbd, mean_pooled, n_observed)], direct, by = "rbd")
    err <- max(abs(m$mean_pooled - m$value))
    check(paste(g, v, "direct pooled weighted mean"), err < 1e-10 && all(m$n_observed == m$n), err)
    # Average the cohort means separately and verify the supported-year count.
    c <- as.data.frame(cohorts[group == g & outcome == v & is.finite(mean), .(rbd, mean)])
    z <- aggregate(mean ~ rbd, data = c, FUN = mean)
    n <- aggregate(mean ~ rbd, data = c, FUN = length)
    m <- merge(averages[group == g & outcome == v, .(rbd, mean_equal_cohort, n_cohorts)], z, by = "rbd")
    err <- max(abs(m$mean_equal_cohort - m$mean))
    check(paste(g, v, "equal-cohort mean"), err < 1e-10, err)
    m <- merge(m, n, by = "rbd", suffixes = c("", "_count"))
    check(paste(g, v, "supported cohort count"), all(m$n_cohorts == m$mean_count))
  }
}
previous <- fread(file.path(base, "probability_weighted/school_cohort_means_long.csv"))
z <- merge(cohorts[group == "weighted_applicants" & outcome %chin% c("parents_education", "simce_baseline", "female"),
  .(rbd, sae_proceso = cohort_gr8, outcome, new_mean = mean, new_weight = observed_weight)],
  previous[, .(rbd, sae_proceso, outcome, mean, observed_probability_mass)], by = c("rbd", "sae_proceso", "outcome"))
check("previous weighted composition reproduced", nrow(z) == 900L &&
  isTRUE(all.equal(z$new_mean, z$mean, tolerance = 1e-10)) &&
  isTRUE(all.equal(z$new_weight, z$observed_probability_mass, tolerance = 1e-10)))
offer_frame <- d[group == "first_round_offered", .(mrun, cohort_gr8, rbd)]
risk_frame <- d[group == "offered_at_risk", .(mrun, cohort_gr8, rbd)]
check("stochastic-offer supplement is subset of actual offers", nrow(risk_frame[!offer_frame, on = .(mrun, cohort_gr8, rbd)]) == 0L)
fwrite(rbindlist(checks), file.path(dest, "verification.csv"))
message("Passed ", length(checks), " checks for group composition averages.")
