suppressPackageStartupMessages({library(data.table); library(sandwich)})
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %chin% c("--indices-only", "--overwrite"))) stop("Unknown argument.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task <- dirname(normalizePath(script, winslash = "/"))
root <- normalizePath(file.path(task, "../../.."), winslash = "/")
source(file.path(task, "staff_association_helpers.R"))
setDTthreads(min(4L, getDTthreads()))
out <- file.path(root, "data/clean/staff_quality_va")
owned <- c("school_staff_indices_and_measures.csv", "staff_index_standardizations.csv", "staff_pca_loadings.csv",
  "staff_index_diagnostics.csv", "staff_core_component_correlations.csv", "staff_analysis_metric_dictionary.csv",
  "staff_va_correlations.csv", "staff_va_index_robustness.csv", "staff_va_outcome_dictionary.csv",
  "staff_va_analysis_warnings.csv")
if (!"--overwrite" %chin% args && any(file.exists(file.path(out, owned)))) stop("Outputs exist; use --overwrite.")
period <- fread(file.path(out, "staff_school_period_measures.csv"))
roster <- fread(file.path(out, "staff_school_year_roster.csv"))
context <- fread(file.path(out, "staff_school_context.csv"))
dictionary <- fread(file.path(out, "staff_metric_dictionary.csv"))
annual <- fread(file.path(out, "staff_school_year_measures.csv.gz"), select = c("RBD", "AGNO", "ROLE", "METRIC", "VALUE"))
ratios <- fread(file.path(root, "data/clean/docentes_educacion/va_students_per_orientador_2018_2024.csv"))
fits <- lapply(c("counselor", "teacher"), function(role) {
  wide <- dcast(period[ROLE == role], RBD ~ METRIC, value.var = "VALUE")
  support <- unique(period[ROLE == role, .(RBD, N_ACTIVE_YEARS, N_ROLE_YEARS_KNOWN)])
  if (anyDuplicated(support$RBD)) stop("Inconsistent role support across metrics.")
  wide <- merge(wide, support, by = "RBD")
  wide <- merge(wide, context, by = "RBD", all.x = TRUE)
  counts <- merge(CJ(RBD = context$RBD, AGNO = 2018:2024), roster[, .(RBD, AGNO,
    N_ROLE = get(if (role == "counselor") "N_COUNSELOR" else "N_TEACHER"))], by = c("RBD", "AGNO"), all.x = TRUE)
  counts <- counts[, .(mean_headcount = if (all(!is.na(N_ROLE))) mean(N_ROLE) else NA_real_,
    role_presence_share = if (all(!is.na(N_ROLE))) mean(N_ROLE > 0) else NA_real_), by = RBD]
  wide <- merge(wide, counts, by = "RBD", all.x = TRUE)
  wide[, staff_per1000_va_students := 1000 * mean_headcount / N_VA_STUDENTS]
  wide[, va_students_per_staff := fifelse(mean_headcount > 0, N_VA_STUDENTS / mean_headcount, NA_real_)]
  wide[, log_va_students_per_staff := log(va_students_per_staff)]
  baseline <- dcast(annual[ROLE == role & AGNO == 2018L & METRIC %chin% c("reported_school_tenure", "reported_system_tenure")],
                    RBD ~ METRIC, value.var = "VALUE")
  setnames(baseline, c("reported_school_tenure", "reported_system_tenure"),
           c("reported_school_tenure_2018", "reported_system_tenure_2018"))
  wide <- merge(wide, baseline, by = "RBD", all.x = TRUE)
  if (role == "counselor") {
    wide <- merge(wide, ratios[, .(RBD, primary_headcount_mean = MEAN_ORIENTADORES_PRIMARY,
      va_students_per_primary_counselor = VA_STUDENTS_PER_AVG_ORIENTADOR_PRIMARY)], by = "RBD", all.x = TRUE)
    wide[, log_va_students_per_primary_counselor := log(va_students_per_primary_counselor)]
  }
  wide[, ROLE := role]
  fit <- q_fit_indices(wide, role)
  # Teacher-history sensitivity changes the explicitly named experience window.
  if (role == "teacher") {
    alternate <- copy(wide)
    alternate[, prior_role_years := prior_hs_years_since2016]
    alt <- q_fit_indices(alternate, role)
    fit$wide[, balanced_index_history2016 := alt$wide$balanced_index]
    alt$parameters[, ROLE := "teacher_history2016_sensitivity"]
    fit$parameters <- rbind(fit$parameters, alt$parameters)
  }
  fit
})
wide <- rbindlist(lapply(fits, `[[`, "wide"), fill = TRUE)
params <- rbindlist(lapply(fits, `[[`, "parameters"))
loadings <- rbindlist(lapply(fits, `[[`, "loadings"))
diag <- rbindlist(lapply(fits, `[[`, "diagnostics"))
component_cor <- rbindlist(lapply(fits, `[[`, "component_correlations"))
extra <- data.table(METRIC = c("mean_headcount", "role_presence_share", "staff_per1000_va_students",
    "va_students_per_staff", "log_va_students_per_staff", "reported_school_tenure_2018", "reported_system_tenure_2018",
    "career_index", "credentials_index", "balanced_index", "pca_index"),
  LABEL = c("Mean annual staff headcount", "Share of years with staff present", "Staff per 1,000 VA-sample students",
    "VA-sample students per average staff member", "Log VA-sample students per average staff member",
    "Reported school tenure in 2018", "Reported system tenure in 2018", "Experience block index",
    "Credentials block index", "Balanced experience-credentials index", "PCA first-component index"),
  BLOCK = c(rep("staffing", 5), "baseline_tenure", "baseline_tenure", rep("index", 4)))
extra <- rbindlist(lapply(c("counselor", "teacher"), function(role) cbind(ROLE = role, extra)))
extra <- rbind(extra, data.table(ROLE = "counselor", METRIC = c("primary_headcount_mean", "va_students_per_primary_counselor",
  "log_va_students_per_primary_counselor"), LABEL = c("Mean primary-counselor headcount", "VA students per average primary counselor",
    "Log VA students per average primary counselor"), BLOCK = "staffing"))
extra <- rbind(extra, data.table(ROLE = "teacher", METRIC = "balanced_index_history2016",
  LABEL = "Balanced index using HS history since 2016", BLOCK = "history_sensitivity"))
dictionary <- rbind(dictionary, extra, fill = TRUE)
dictionary[, ANALYZE := !BLOCK %chin% c("tenure_audit", "history_audit", "applicability")]
stopifnot(!anyDuplicated(dictionary, by = c("ROLE", "METRIC")), !anyDuplicated(wide, by = c("RBD", "ROLE")))
fwrite(wide, file.path(out, owned[1]), na = "NA")
fwrite(params, file.path(out, owned[2]), na = "NA")
fwrite(loadings, file.path(out, owned[3]), na = "NA")
fwrite(diag, file.path(out, owned[4]), na = "NA")
fwrite(component_cor, file.path(out, owned[5]), na = "NA")
fwrite(dictionary, file.path(out, owned[6]), na = "NA")
print(diag); print(loadings)
if ("--indices-only" %chin% args) {
  message("Indices fitted without reading any VA outcomes.")
  quit(save = "no", status = 0L)
}

# Analyze the exact exported doubles so the independent audit uses the same input.
wide <- fread(file.path(out, owned[1]))

outcomes <- data.table(OUTCOME = c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "higher_ed_enrolled_m1",
  "stem_enrollment_m1", "high_paying_field_m1", "high_inst_m1", "log_program_income_full_clp_m1",
  "log_program_income_area_clp_m1", "log_program_income_institution_clp_m1", "program_certified_years_m1", "inst_certified_years_m1"),
  LABEL = c("Math score", "Language score", "Admission-exam taking", "Higher-ed enrollment", "STEM enrollment",
    "High-premium field", "High-premium institution", "Projected income (full)", "Projected income (field)",
    "Projected income (institution)", "Program accreditation years", "Institution accreditation years"),
  FAMILY = c("scores", "scores", "transition", rep("higher_education", 9)))
va_path <- file.path(root, "output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv")
va <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome", "n_students_regression", "eb_reliability",
  "controlled_value_added_centered_student", "controlled_value_added_eb_centered_student"))
va <- va[analysis_sample == "All" & outcome %chin% outcomes$OUTCOME]
stopifnot(!anyDuplicated(va, by = c("school_rbd", "outcome")), setequal(va$outcome, outcomes$OUTCOME))
setnames(va, c("school_rbd", "outcome", "controlled_value_added_centered_student", "controlled_value_added_eb_centered_student"),
         c("RBD", "OUTCOME", "Y_RAW", "Y"))
results <- list(); robustness <- list(); captured_warnings <- list(); k <- 0L; j <- 0L
run_association <- function(dt, role, metric, outcome, subsample = "main") {
  withCallingHandlers(q_correlations(dt), warning = function(w) {
    captured_warnings[[length(captured_warnings) + 1L]] <<- data.table(
      ROLE = role, METRIC = metric, OUTCOME = outcome, SUBSAMPLE = subsample, MESSAGE = conditionMessage(w))
    invokeRestart("muffleWarning")
  })
}
for (role in c("counselor", "teacher")) {
  for (outcome in outcomes$OUTCOME) {
    message(format(Sys.time(), "%H:%M:%S"), " Associations: ", role, " / ", outcome)
    base <- merge(wide[ROLE == role], va[OUTCOME == outcome], by = "RBD", all.x = TRUE)
    for (metric in dictionary[ROLE == role & ANALYZE == TRUE, METRIC]) {
      dt <- base[, .(X = get(metric), Y, Y_RAW, N_VA_STUDENTS, COD_DEPE, COD_REG_RBD, RURAL_RBD,
                      HAS_TP_OR_ARTISTIC, HAS_BASIC, N_ACTIVE_YEARS)]
      k <- k + 1L
      results[[k]] <- cbind(ROLE = role, METRIC = metric, OUTCOME = outcome,
                            run_association(dt, role, metric, outcome))
      if (metric %chin% c("balanced_index", "pca_index", "career_index", "credentials_index")) {
        filters <- list(no_private_paid = !is.na(dt$COD_DEPE) & dt$COD_DEPE != 4L,
          at_least_100_va_students = dt$N_VA_STUDENTS >= 100L,
          at_least_5_active_staff_years = dt$N_ACTIVE_YEARS >= 5L)
        for (subsample in names(filters)) {
          j <- j + 1L
          robustness[[j]] <- cbind(ROLE = role, METRIC = metric, OUTCOME = outcome, SUBSAMPLE = subsample,
                                    run_association(dt[filters[[subsample]]], role, metric, outcome, subsample))
        }
      }
    }
  }
}
results <- rbindlist(results)
results[, PEARSON_Q_BH := p.adjust(PEARSON_P, method = "BH"), by = ROLE]
results[, ADJUSTED_Q_BH := p.adjust(ADJUSTED_P, method = "BH"), by = ROLE]
results <- merge(results, dictionary[, .(ROLE, METRIC, METRIC_LABEL = LABEL, BLOCK)], by = c("ROLE", "METRIC"), all.x = TRUE)
results <- merge(results, outcomes[, .(OUTCOME, OUTCOME_LABEL = LABEL, OUTCOME_FAMILY = FAMILY)], by = "OUTCOME", all.x = TRUE)
robustness <- rbindlist(robustness)
robustness[, ADJUSTED_Q_BH := p.adjust(ADJUSTED_P, method = "BH"), by = .(ROLE, SUBSAMPLE)]
stopifnot(nrow(results) == nrow(dictionary[ANALYZE == TRUE])*nrow(outcomes),
          !anyDuplicated(results, by = c("ROLE", "METRIC", "OUTCOME")))
fwrite(results, file.path(out, owned[7]), na = "NA")
fwrite(robustness, file.path(out, owned[8]), na = "NA")
fwrite(outcomes, file.path(out, owned[9]), na = "NA")
warnings_table <- if (length(captured_warnings)) rbindlist(captured_warnings) else
  data.table(ROLE = character(), METRIC = character(), OUTCOME = character(), SUBSAMPLE = character(), MESSAGE = character())
fwrite(warnings_table, file.path(out, owned[10]), na = "NA")
print(results[METRIC == "balanced_index", .(ROLE, OUTCOME_LABEL, N, PEARSON_R, SPEARMAN_R,
  UNSHRUNK_VA_R, ADJUSTED_BETA_SD, ADJUSTED_Q_BH)])
message("Completed all staff-VA associations; source VA estimates unchanged.")
