# Independent checks for the parent-education build and all saved preference fits.
source("code/codex/school_preference_va/parent_education_common.R")
same_num <- function(a, b, tol = 1e-10) length(a) == length(b) && all((is.na(a) & is.na(b)) |
  (!is.na(a) & !is.na(b) & abs(a - b) <= tol))
ranking <- as.data.table(readRDS(file.path(clean_dir, "ranking_parent_education_2022.rds")))
est <- as.data.table(readRDS(parent_education_input_path))
roster <- fread(file.path(clean_dir, "ranking_roster.csv"))
old_roster <- fread(file.path(base_paes_dir, "ranking_roster.csv"))
old_est <- as.data.table(readRDS(paes_input_path))
school <- fread(file.path(clean_dir, "school_parent_education_2022.csv"))
norm <- fread(file.path(clean_dir, "parent_education_normalization.csv"))
co <- fread(file.path(clean_dir, "coefficients.csv"))
sl <- fread(file.path(clean_dir, "type_specific_slopes.csv"))
ms <- fread(file.path(clean_dir, "model_summary.csv"))

check("All original submitted alternatives remain in augmented ranking data",
  nrow(ranking) == 1007253L && !anyDuplicated(ranking[, .(ranking_id, rbd)]) &&
    !anyDuplicated(ranking[, .(ranking_id, rank_school)]))
check("Original applicant roster and group classifications are preserved", nrow(roster) == 294198L &&
  setequal(roster$ranking_id, old_roster$ranking_id) &&
  all(vapply(c("mrun", "sae_proceso", "sample", "continuation_group", "prioritario", "group", "paes_estimation_eligible"),
    function(v) identical(roster[[v]][match(old_roster$ranking_id, roster$ranking_id)], old_roster[[v]]), logical(1))))
school_join_map <- c(mean_parent_education_years = "mean_parent_education_years",
  n_simce_students_parent_survey = "n_simce_students",
  n_families_parent_education_observed = "n_families_observed")
check("RBD school means and survey counts join exactly", all(vapply(names(school_join_map), function(v) {
  a <- ranking[[v]]; b <- school[[school_join_map[[v]]]][match(ranking$rbd, school$rbd)]
  same_num(a, b)
}, logical(1))))
recalc_coverage <- ranking[, .(list_all_parent_education_observed = all(is.finite(mean_parent_education_years))), by = ranking_id]
check("Complete-list flags independently match all original school alternatives", {
  ix <- match(recalc_coverage$ranking_id, roster$ranking_id)
  identical(as.logical(recalc_coverage$list_all_parent_education_observed),
    as.logical(roster$list_all_parent_education_observed[ix]))
})
eligible <- roster[parent_education_estimation_eligible == TRUE, ranking_id]
check("Estimation data contain exactly PAES-M1 complete lists with complete school means",
  setequal(unique(est$ranking_id), eligible) && all(est[, .N, by = ranking_id]$N >= 2L))
check("Each retained list preserves its original submitted ranks and schools", {
  a <- est[, .(ranking_id, rbd, rank_school)]
  b <- old_est[ranking_id %in% eligible, .(ranking_id, rbd, rank_school)]
  setorder(a, ranking_id, rank_school); setorder(b, ranking_id, rank_school)
  identical(a, b)
})
school_ref <- unique(est[, .(rbd, mean_parent_education_years)])
check("Saved education normalization uses equal-weight sample of destination RBDs",
  nrow(norm) == 1L && nrow(school_ref) == norm$reference_schools &&
    abs(mean(school_ref$mean_parent_education_years) - norm$reference_mean_years) < 1e-10 &&
    abs(sd(school_ref$mean_parent_education_years) - norm$reference_sd_years) < 1e-10)
check("Standardized parent education has zero mean and unit school SD",
  abs(mean((school_ref$mean_parent_education_years - norm$reference_mean_years) / norm$reference_sd_years)) < 1e-10 &&
    abs(sd((school_ref$mean_parent_education_years - norm$reference_mean_years) / norm$reference_sd_years) - 1) < 1e-10)
check("All retained list regressors are finite", all(vapply(est[, c(metric_terms, unname(distance_vars)), with = FALSE],
  function(z) all(is.finite(z)), logical(1))))
check("Extended and matched baseline fits cover every group and distance", nrow(ms) == 20L &&
  setequal(ms$group, names(group_labels)) && setequal(ms$variant, names(variant_terms)) &&
  setequal(ms$model, names(distance_vars)) && !anyDuplicated(ms[, .(group, variant, model)]))
check("All saved coefficient and slope estimates are finite", all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0) && all(is.finite(sl$estimate)) &&
  all(is.finite(sl$std_error) & sl$std_error > 0))

for (g_name in names(group_labels)) for (variant_name in names(variant_terms)) for (model_name in names(distance_vars)) {
  key <- paste(g_name, variant_name, model_name, sep = "_")
  path <- paste0(parent_education_model_path(g_name, variant_name, model_name), ".rds")
  fit <- readRDS(path)
  wanted <- c(variant_terms[[variant_name]], unname(distance_vars[model_name]))
  b <- co[group == g_name & variant == variant_name & model == model_name]
  check(paste(key, "saved model and coefficient terms agree"), identical(names(coef(fit)), wanted) &&
    setequal(b$term, wanted))
  check(paste(key, "saved coefficients equal fitted model"), same_num(coef(fit)[wanted], b$estimate[match(wanted, b$term)]))
  cov_file <- fread(file.path(clean_dir, paste0("vcov_", key, ".csv")))
  V <- vcov(fit)
  check(paste(key, "student-clustered covariance export matches model"),
    identical(cov_file$term, rownames(V)) && same_num(as.matrix(cov_file[, ..wanted]), as.vector(V), 1e-10))
  x <- if (g_name == "pooled") copy(est) else copy(est[group == g_name])
  x <- add_metric_interactions(x)
  check(paste(key, "exact submitted-list likelihood matches saved fit"),
    abs(paes_pl_loglik(x, coef(fit), wanted) - as.numeric(logLik(fit))) < 1e-5)
  if (variant_name == "extended") {
    for (school_type_name in c("Non-TP", "Mixed HC/TP", "TP only")) {
      a <- sl[group == g_name & variant == variant_name & model == model_name & school_type == school_type_name]
      active <- metric_specs[standardized %in% wanted]
      check(paste(key, school_type_name, "total slopes match main effects plus type interactions"), {
        expected <- co[group == g_name & variant == variant_name & model == model_name]
        expected_values <- expected[match(active$standardized, term), estimate]
        if (school_type_name != "Non-TP") {
          target <- if (school_type_name == "Mixed HC/TP") "mixed_hc_tp" else "tp_only"
          suffix <- metric_interactions[control == target][match(active$standardized, metric), term]
          expected_values <- expected_values + expected[match(suffix, term), estimate]
        }
        setequal(a$term, active$standardized) && same_num(a$estimate[match(active$standardized, a$term)], expected_values)
      })
    }
  }
}
check("2022 survey is dated after the SAE application cohorts", all(ms$group %in% names(group_labels)) &&
  all(c(2018L, 2019L, 2020L) < 2022L))
fwrite(checks, file.path(clean_dir, "verification.csv"))
message("Independent parent-education verification: ", sum(checks$passed), "/", nrow(checks), " checks passed.")
print(checks[passed == FALSE])
