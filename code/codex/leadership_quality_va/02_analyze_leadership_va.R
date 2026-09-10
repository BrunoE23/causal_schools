suppressPackageStartupMessages({library(data.table); library(sandwich)})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task <- dirname(normalizePath(script, winslash = "/")); root <- normalizePath(file.path(task, "../../.."), winslash = "/")
source(file.path(task, "../staff_quality_va/staff_association_helpers.R"))
source(file.path(task, "leadership_helpers.R"))
setDTthreads(min(4L, getDTthreads()))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %chin% c("--indices-only", "--overwrite")))
out <- file.path(root, "data/clean/leadership_quality_va")
old <- file.path(root, "data/clean/staff_quality_va")
owned <- c("leadership_indices_and_measures.csv", "leadership_index_standardizations.csv", "leadership_pca_loadings.csv",
  "leadership_index_diagnostics.csv", "leadership_core_component_correlations.csv", "leadership_analysis_metric_dictionary.csv",
  "leadership_va_correlations.csv", "leadership_va_index_robustness.csv", "leadership_va_outcome_dictionary.csv",
  "leadership_va_analysis_warnings.csv", "leadership_person_metrics.csv.gz")
if (!"--overwrite" %chin% args && any(file.exists(file.path(out, owned)))) stop("Outputs exist; use --overwrite.")
period <- fread(file.path(out, "leadership_school_period_measures.csv"))
roster <- fread(file.path(out, "leadership_school_year_roster.csv"))
context <- fread(file.path(old, "staff_school_context.csv"))
dictionary <- fread(file.path(out, "leadership_metric_dictionary.csv"))
annual <- fread(file.path(out, "leadership_school_year_measures.csv.gz"), select = c("RBD", "AGNO", "METRIC", "VALUE"))
wide <- dcast(period, RBD ~ METRIC, value.var = "VALUE")
support <- unique(period[, .(RBD, N_ACTIVE_YEARS, N_ROLE_YEARS_KNOWN)])
stopifnot(!anyDuplicated(support$RBD))
wide <- merge(merge(wide, support, by = "RBD"), context, by = "RBD", all.x = TRUE)
counts <- roster[, .(mean_headcount = if (all(!is.na(N_ROLE))) mean(N_ROLE) else NA_real_,
  primary_headcount_mean = if (all(!is.na(N_PRIMARY))) mean(N_PRIMARY) else NA_real_,
  role_presence_share = if (all(!is.na(N_ROLE))) mean(N_ROLE > 0) else NA_real_), by = RBD]
wide <- merge(wide, counts, by = "RBD", all.x = TRUE)
wide[, `:=`(staff_per1000_va_students = 1000 * mean_headcount / N_VA_STUDENTS,
  va_students_per_staff = fifelse(mean_headcount > 0, N_VA_STUDENTS / mean_headcount, NA_real_))]
wide[, log_va_students_per_staff := log(va_students_per_staff)]
baseline <- dcast(annual[AGNO == 2018 & METRIC %chin% c("reported_school_tenure", "reported_system_tenure")],
  RBD ~ METRIC, value.var = "VALUE")
setnames(baseline, c("reported_school_tenure", "reported_system_tenure"), c("reported_school_tenure_2018", "reported_system_tenure_2018"))
wide <- merge(wide, baseline, by = "RBD", all.x = TRUE)
wide[, ROLE := "leadership"]
components <- c("prior_role_years", "school_role_spell_years", "university_share", "teaching_title_share")
fit <- q_fit_indices(wide, "leadership", components)
alternate <- copy(wide)
alternate[, `:=`(prior_role_years = prior_role_years_2015, school_role_spell_years = school_role_spell_years_2015)]
alt <- q_fit_indices(alternate, "leadership_history2015", components)
fit$wide[, balanced_index_history2015 := alt$wide$balanced_index]
params <- rbind(fit$parameters, alt$parameters)
extra <- data.table(METRIC = c("mean_headcount", "primary_headcount_mean", "role_presence_share", "staff_per1000_va_students",
  "va_students_per_staff", "log_va_students_per_staff", "reported_school_tenure_2018", "reported_system_tenure_2018",
  "career_index", "credentials_index", "balanced_index", "pca_index", "balanced_index_history2015"),
  LABEL = c("Mean annual leadership headcount", "Mean primary leadership headcount", "Share of years with leadership present",
    "Leaders per 1,000 VA-sample students", "VA-sample students per average leader", "Log VA-sample students per average leader",
    "Reported school tenure in 2018", "Reported system tenure in 2018", "Experience block index", "Credentials block index",
    "Balanced experience-credentials index", "PCA first-component index", "Balanced index with both histories since 2015"),
  BLOCK = c(rep("staffing", 6), rep("baseline_tenure", 2), rep("index", 4), "history_sensitivity"), ANALYZE = TRUE)
dictionary <- rbind(dictionary, extra, fill = TRUE)
dictionary[, ROLE := "leadership"]
stopifnot(!anyDuplicated(dictionary$METRIC), nrow(fit$wide) == 3682)
exports <- list(fit$wide, params, rbind(fit$loadings, alt$loadings), rbind(fit$diagnostics, alt$diagnostics),
  rbind(fit$component_correlations, alt$component_correlations), dictionary)
for (i in seq_along(exports)) fwrite(exports[[i]], file.path(out, owned[i]), na = "NA")
members <- l_members(readRDS(file.path(out, "leadership_person_school_year.rds")))
fwrite(members[, c("MRUN", "RBD", "AGNO", l_flags, dictionary[!is.na(SOURCE_FIELD), METRIC]), with = FALSE],
  file.path(out, owned[11]), na = "NA")
print(rbind(fit$diagnostics, alt$diagnostics)); print(fit$loadings)
if ("--indices-only" %chin% args) {
  message("Indices fixed without reading any VA outcomes."); quit(save = "no", status = 0L)
}
wide <- fread(file.path(out, owned[1]))
outcomes <- fread(file.path(old, "staff_va_outcome_dictionary.csv"))
va <- fread(file.path(root, "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "n_students_regression", "eb_reliability",
    "controlled_value_added_centered_student", "controlled_value_added_eb_centered_student"))
va <- va[analysis_sample == "All" & outcome %chin% outcomes$OUTCOME]
stopifnot(!anyDuplicated(va, by = c("school_rbd", "outcome")), setequal(va$outcome, outcomes$OUTCOME))
setnames(va, c("school_rbd", "outcome", "controlled_value_added_centered_student", "controlled_value_added_eb_centered_student"),
  c("RBD", "OUTCOME", "Y_RAW", "Y"))
results <- list(); robustness <- list(); captured <- list()
run <- function(dt, metric, outcome, subsample = "main") withCallingHandlers(q_correlations(dt), warning = function(w) {
  captured[[length(captured)+1L]] <<- data.table(METRIC = metric, OUTCOME = outcome, SUBSAMPLE = subsample, MESSAGE = conditionMessage(w))
  invokeRestart("muffleWarning")
})
for (outcome in outcomes$OUTCOME) {
  message(format(Sys.time(), "%H:%M:%S"), " Leadership associations: ", outcome)
  base <- merge(wide, va[OUTCOME == outcome], by = "RBD", all.x = TRUE)
  for (metric in dictionary[ANALYZE == TRUE, METRIC]) {
    dt <- base[, .(X = get(metric), Y, Y_RAW, N_VA_STUDENTS, COD_DEPE, COD_REG_RBD, RURAL_RBD,
      HAS_TP_OR_ARTISTIC, HAS_BASIC, N_ACTIVE_YEARS)]
    results[[length(results)+1L]] <- cbind(ROLE = "leadership", METRIC = metric, OUTCOME = outcome, run(dt, metric, outcome))
    if (metric %chin% c("balanced_index", "pca_index", "career_index", "credentials_index")) {
      filters <- list(no_private_paid = !is.na(dt$COD_DEPE) & dt$COD_DEPE != 4L,
        at_least_100_va_students = dt$N_VA_STUDENTS >= 100L,
        at_least_5_active_staff_years = dt$N_ACTIVE_YEARS >= 5L)
      for (subsample in names(filters)) robustness[[length(robustness)+1L]] <- cbind(ROLE = "leadership", METRIC = metric,
        OUTCOME = outcome, SUBSAMPLE = subsample, run(dt[filters[[subsample]]], metric, outcome, subsample))
    }
  }
}
results <- rbindlist(results)
results[, `:=`(PEARSON_Q_BH = p.adjust(PEARSON_P, "BH"), ADJUSTED_Q_BH = p.adjust(ADJUSTED_P, "BH"))]
results <- merge(results, dictionary[, .(METRIC, METRIC_LABEL = LABEL, BLOCK)], by = "METRIC", all.x = TRUE)
results <- merge(results, outcomes[, .(OUTCOME, OUTCOME_LABEL = LABEL, OUTCOME_FAMILY = FAMILY)], by = "OUTCOME", all.x = TRUE)
robustness <- rbindlist(robustness)
robustness[, ADJUSTED_Q_BH := p.adjust(ADJUSTED_P, "BH"), by = SUBSAMPLE]
warnings <- if (length(captured)) rbindlist(captured) else data.table(METRIC = character(), OUTCOME = character(), SUBSAMPLE = character(), MESSAGE = character())
stopifnot(nrow(results) == dictionary[ANALYZE == TRUE, .N]*12L, nrow(robustness) == 144L)
exports <- list(results, robustness, outcomes, warnings)
for (i in seq_along(exports)) fwrite(exports[[i]], file.path(out, owned[i+6]), na = "NA")
protected <- fread(file.path(out, "leadership_preserved_sources.csv"))
stopifnot(identical(unname(tools::md5sum(protected$PATH)), protected$MD5))
print(results[METRIC == "balanced_index", .(OUTCOME_LABEL, N, PEARSON_R, ADJUSTED_BETA_SD, ADJUSTED_Q_BH)])
message("Completed leadership associations; earlier outputs remain byte-identical.")
