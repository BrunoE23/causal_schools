# Independently validate the school income-decile join and saved preference models.
source("code/codex/school_preference_va/parent_income_common.R")
same_num <- function(a, b, tol = 1e-9) length(a) == length(b) && all((is.na(a) & is.na(b)) |
  (!is.na(a) & !is.na(b) & abs(a - b) <= tol))
est <- as.data.table(readRDS(parent_income_input_path))
roster <- fread(file.path(clean_dir, "ranking_roster.csv"))
source_roster <- fread(file.path(parent_education_dir, "ranking_roster.csv"))
income <- fread(file.path(clean_dir, "school_parent_income_decile_4to.csv"))
composition <- fread(file.path(root, "data/clean/staff_lasso_va/school_student_composition.csv"))
co <- fread(file.path(clean_dir, "coefficients.csv")); sl <- fread(file.path(clean_dir, "type_specific_slopes.csv"))
ms <- fread(file.path(clean_dir, "model_summary.csv")); norm <- fread(file.path(clean_dir, "parent_income_normalization.csv"))
check("Ranking sample nests the parent-education applicant roster", nrow(roster) == nrow(source_roster) &&
  setequal(roster$ranking_id, source_roster$ranking_id) && all(!roster$income_estimation_eligible |
    roster$parent_education_estimation_eligible))
check("School income composition exactly reuses established grade-4 measure", {
  z <- merge(income, composition[, .(rbd = RBD, expected = income_decile_mean)], by = "rbd", all = TRUE)
  nrow(z) == nrow(income) && all(is.finite(z$expected)) && same_num(z$income_decile_mean, z$expected)
})
check("Income-model estimation contains exactly all eligible whole rankings", {
  ids <- roster[income_estimation_eligible == TRUE, ranking_id]
  setequal(unique(est$ranking_id), ids) && !anyDuplicated(est[, .(ranking_id, rbd)]) &&
    all(est[, .N, by = ranking_id]$N >= 2L) && all(is.finite(est$z_parent_income_decile))
})
school_ref <- unique(est[, .(rbd, income_decile_mean)])
check("Saved income normalization uses equal-weight destination RBDs", nrow(norm) == 1L &&
  nrow(school_ref) == norm$reference_schools && abs(mean(school_ref$income_decile_mean) - norm$reference_mean) < 1e-10 &&
  abs(sd(school_ref$income_decile_mean) - norm$reference_sd) < 1e-10)
check("Standardized school income decile has mean zero and unit SD", abs(mean(est[!duplicated(rbd), z_parent_income_decile])) < 1e-10 &&
  abs(sd(est[!duplicated(rbd), z_parent_income_decile]) - 1) < 1e-10)
check("Twenty model summaries cover all groups, variants and distance definitions", nrow(ms) == 20L &&
  !anyDuplicated(ms[, .(group, variant, model)]) && setequal(ms$group, names(group_labels)) &&
  setequal(ms$variant, names(variant_terms)) && setequal(ms$model, names(distance_vars)))
check("All fits converged without missing model terms", all(ms$iterations < 60L) && all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0))
for (g in names(group_labels)) for (v in names(variant_terms)) for (m in names(distance_vars)) {
  key <- paste(g, v, m, sep = "_")
  fit <- readRDS(paste0(parent_income_model_path(g, v, m), ".rds"))
  wanted <- c(variant_terms[[v]], unname(distance_vars[m]))
  b <- co[group == g & variant == v & model == m]
  check(paste(key, "saved coefficient order matches model specification"), identical(names(coef(fit)), wanted))
  check(paste(key, "saved coefficients match exported table"), same_num(coef(fit)[wanted], b$estimate[match(wanted, b$term)]))
  V <- vcov(fit); Vfile <- fread(file.path(clean_dir, paste0("vcov_", key, ".csv")))
  check(paste(key, "exported clustered covariance matches model"), identical(Vfile$term, rownames(V)) &&
    same_num(as.matrix(Vfile[, ..wanted]), as.vector(V)))
  expected_n <- if (g == "pooled") nrow(est) else nrow(est[group == g])
  check(paste(key, "model used the full group-specific estimation sample"), fit$n == expected_n && is.null(fit$na.action))
}
check("Adding income weakly increases likelihood within each group and distance model", {
  paired <- merge(ms[variant == "extended", .(group, model, ll1 = log_likelihood)],
    ms[variant == "matched_baseline", .(group, model, ll0 = log_likelihood)], by = c("group", "model"))
  nrow(paired) == 10L && all(paired$ll1 >= paired$ll0 - 1e-7)
})
fwrite(checks, file.path(clean_dir, "verification.csv"))
message("Independent parent-income-decile verification: ", sum(checks$passed), "/", nrow(checks), " checks passed.")
print(checks[passed == FALSE])
