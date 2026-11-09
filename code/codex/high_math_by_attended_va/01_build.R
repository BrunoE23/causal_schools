# Repeat high-math descriptive outcomes by student-weighted attended-school VA deciles.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
base <- file.path(repo, "data/clean/math_high_score_by_income")
clean <- file.path(repo, "data/clean/high_math_by_attended_va")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(base, "income_enrollment/high_math_with_income.rds")
score_cache <- file.path(base, "mean_scores/high_math_with_verbal.rds")
manifest <- fread(file.path(base, "source_manifest.csv"))
va_path <- file.path(dirname(manifest$path[1L]),
                    "empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
inputs <- c(cache, score_cache, va_path)
hashes <- unname(tools::md5sum(inputs))
fwrite(data.table(path = inputs, bytes = file.info(inputs)$size, md5 = hashes),
       file.path(clean, "source_manifest.csv"))
d <- as.data.table(readRDS(cache))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun), all(d$z_math >= 1),
          all(d[enrolled == TRUE, valid_income]))
# Reuse admission-year standardization from the original score figure.
# Missing verbal scores affect only verbal means, never sample selection or bins.
scores <- as.data.table(readRDS(score_cache))
si <- match(d$mrun, scores$mrun)
stopifnot(nrow(scores) == nrow(d), !anyDuplicated(scores$mrun), !anyNA(si),
          identical(d$psu_year, scores$psu_year[si]),
          identical(d$z_math, scores$z_math[si]))
d[, `:=`(z_verbal = scores$z_verbal[si], verbal_observed = scores$verbal_observed[si])]
stopifnot(!anyNA(d$verbal_observed), all(d$verbal_observed == is.finite(d$z_verbal)))
d[, school_rbd := as.numeric(most_time_RBD)]
spec <- data.table(
  measure = c("math", "high_premium_field", "high_premium_institution"),
  va_label = c("Math VA", "High-premium field VA", "High-premium institution VA"),
  outcome = c("z_year_math_max", "high_paying_field_m1", "high_inst_m1"))
v <- fread(va_path, select = c("outcome", "school_rbd", "analysis_sample",
                              "controlled_value_added_eb_centered_student"))
v <- v[analysis_sample == "All" & outcome %chin% spec$outcome]
v <- merge(v, spec, by = "outcome")
setnames(v, "controlled_value_added_eb_centered_student", "va")
stopifnot(!anyNA(v$school_rbd), !anyDuplicated(v[, .(measure, school_rbd)]))
w <- dcast(v[is.finite(va)], school_rbd ~ measure, value.var = "va")
common_school_ids <- w[complete.cases(w), school_rbd]
coverage <- rbindlist(lapply(spec$measure, function(k) {
  ids <- v[measure == k & is.finite(va), school_rbd]
  data.table(measure = k, original_students = nrow(d), matched_students = sum(d$school_rbd %in% ids),
             common_students = sum(d$school_rbd %in% common_school_ids))
}))
fwrite(coverage, file.path(clean, "va_coverage.csv"))
fwrite(d[!school_rbd %in% common_school_ids, .(n_students = .N), by = school_rbd],
       file.path(clean, "unmatched_schools.csv"))
a <- d[school_rbd %in% common_school_ids]
stopifnot(nrow(a) > 0L, !anyNA(a[, .(high_paying_field_m1, high_inst_m1, high_either, high_both, enrolled)]))
weights <- a[, .(n_selected_students = .N), by = school_rbd]
fwrite(data.table(original_n = nrow(d), matched_n = nrow(a), excluded_n = nrow(d) - nrow(a),
                 n_attended_schools = nrow(weights), n_enrolled = sum(a$enrolled),
                 n_verbal = sum(a$verbal_observed), n_missing_verbal = sum(!a$verbal_observed)),
       file.path(clean, "sample_counts.csv"))

# Weighted inverse empirical CDF: the first school VA attaining each 10% mass.
# All students at one school receive one decile; boundaries may be imbalanced.
school_maps <- list(); cutoffs <- list(); summaries <- list(); cohort_summaries <- list()
summarize <- function(x, groups) {
  ans <- x[, .(n_students = .N, n_schools = uniqueN(school_rbd), n_enrolled = sum(enrolled),
               n_high_program = sum(high_paying_field_m1), n_high_institution = sum(high_inst_m1),
               n_high_either = sum(high_either), n_high_both = sum(high_both),
               share_high_program = mean(high_paying_field_m1), share_high_institution = mean(high_inst_m1),
               share_high_either = mean(high_either), share_high_both = mean(high_both),
               share_enrolled = mean(enrolled),
               mean_log_income_enrolled = mean(log_program_income_full_clp_m1[enrolled]),
               geometric_mean_income_clp_enrolled = exp(mean(log_program_income_full_clp_m1[enrolled])),
               mean_math_score = mean(z_math), mean_verbal_score = mean(z_verbal[verbal_observed]),
               n_math = .N, n_verbal = sum(verbal_observed), n_missing_verbal = sum(!verbal_observed),
               mean_math_with_verbal = mean(z_math[verbal_observed]),
               mean_household_income_decile = mean(income_decile_imputed),
               mean_attended_va = mean(va), min_va = min(va), max_va = max(va)), by = groups]
  setorderv(ans, groups)
  stopifnot(all(ans$n_high_either == ans$n_high_program + ans$n_high_institution - ans$n_high_both),
            all(ans$n_high_either <= ans$n_enrolled), all(ans$n_enrolled <= ans$n_students),
            all(ans$n_enrolled > 0L), all(is.finite(ans$mean_log_income_enrolled)),
            all(ans$n_math == ans$n_students), all(ans$n_verbal + ans$n_missing_verbal == ans$n_students),
            all(ans$n_verbal > 0L), all(is.finite(ans$mean_verbal_score)))
  ans
}
for (k in spec$measure) {
  sm <- merge(v[measure == k, .(school_rbd, va)], weights, by = "school_rbd")
  setorder(sm, va, school_rbd)
  stopifnot(nrow(sm) == nrow(weights), all(is.finite(sm$va)))
  mass <- sm[, .(weight = sum(n_selected_students)), keyby = va]
  mass[, cumulative_n := cumsum(weight)]
  target <- (1:9) * sum(mass$weight) / 10
  q <- vapply(target, function(t) mass[cumulative_n >= t, va][1L], numeric(1))
  stopifnot(length(unique(q)) == 9L)
  sm[, va_decile := as.integer(cut(va, breaks = c(-Inf, q, Inf), right = TRUE, labels = FALSE))]
  sm[, measure := k]
  stopifnot(!anyNA(sm$va_decile), all(1:10 %in% sm$va_decile))
  cutoffs[[k]] <- data.table(measure = k, quantile = (1:9) / 10, cutoff_va = q,
                            cumulative_target_n = target,
                            cumulative_actual_n = vapply(q, function(c) sum(sm[va <= c, n_selected_students]), integer(1)))
  school_maps[[k]] <- sm
  x <- merge(a, sm[, .(school_rbd, va, va_decile)], by = "school_rbd", all.x = TRUE, sort = FALSE)
  stopifnot(nrow(x) == nrow(a), !anyNA(x$va_decile), uniqueN(x$mrun) == nrow(a))
  sums <- summarize(x, "va_decile")
  sums[, `:=`(measure = k, va_label = spec[measure == k, va_label], student_share = n_students / nrow(x))]
  stopifnot(sum(sums$n_students) == nrow(a), sum(sums$n_enrolled) == sum(a$enrolled),
            sum(sums$n_high_program) == sum(a$high_paying_field_m1),
            all(diff(sums$min_va) >= 0), all(diff(sums$max_va) >= 0),
            all(sums$max_va[-10L] < sums$min_va[-1L]))
  # Cumulative deviation at each boundary is smaller than the largest school.
  stopifnot(all(abs(cumsum(sums$n_students)[1:9] - target) <= max(weights$n_selected_students)))
  summaries[[k]] <- sums
  cohort_summaries[[k]] <- summarize(x, c("cohort_gr8", "va_decile"))[, measure := k]
}
summary <- rbindlist(summaries)
fwrite(summary, file.path(clean, "outcomes_by_attended_va_decile.csv"))
fwrite(rbindlist(cohort_summaries), file.path(clean, "outcomes_by_cohort_attended_va_decile.csv"))
fwrite(rbindlist(school_maps), file.path(clean, "school_va_decile_map.csv"))
fwrite(rbindlist(cutoffs), file.path(clean, "student_weighted_decile_cutoffs.csv"))
fwrite(spec, file.path(clean, "va_definitions.csv"))
stopifnot(identical(unname(tools::md5sum(inputs)), hashes))
fwrite(data.table(check = c("Unique school VA per dimension", "Common matched high-math sample",
                           "Student-weighted empirical decile cutoffs", "Schools never split across bins",
                           "All ten ordered VA bins represented", "Counts reconcile for every VA dimension",
                           "Premium union/intersection identities", "Enrollee-only mean log income",
                           "Cumulative bin imbalances bounded by school size",
                           "Score cache matches original selected sample and math normalization",
                           "Verbal observed and missing counts reconcile within each bin",
                           "Original inputs unchanged"), passed = TRUE),
       file.path(clean, "verification.csv"))
print(coverage)
print(summary[, .(measure, va_decile, n_students, n_schools, share_high_program,
                  share_high_institution, share_high_either, share_high_both,
                  mean_log_income_enrolled, share_enrolled, mean_math_score, mean_verbal_score,
                  n_missing_verbal)])
