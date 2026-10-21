# Independently verify the four pooled specifications and their saved estimates.
source("code/codex/school_preference_va/four_specifications_common.R")
suppressPackageStartupMessages(library(survival))
same_num <- function(a, b, tol = 1e-9) {
  if (length(a) != length(b)) return(FALSE)
  a <- as.numeric(a); b <- as.numeric(b)
  all((is.na(a) & is.na(b)) | (is.finite(a) & is.finite(b) & abs(a - b) <= tol))
}
same_values <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) same_num(a, b, 0) else identical(a, b)
}
expected_fees <- c(paste0("enrollment_fee_band_", 2:6), paste0("monthly_fee_band_", 3:6), "fee_information_unknown")
expected_vas <- c("z_va_math", "z_va_exam", "z_va_highfield", "z_va_highinst")
expected_mean_specs <- data.table(outcome = c("admission_exam_taker", "high_paying_field_m1", "high_inst_m1"),
  raw = c("mean_exam", "mean_highfield", "mean_highinst"),
  standardized = c("z_mean_exam", "z_mean_highfield", "z_mean_highinst"))
expected_mean_terms <- c("z_paes_m1", expected_mean_specs$standardized)
expected_types_means <- c("mixed_hc_tp", "tp_only", expected_mean_terms)
expected_specifications <- list(A = expected_fees, B = c(expected_fees, expected_types_means),
  C = c(expected_fees, expected_vas), D = c(expected_fees, "mixed_hc_tp", "tp_only", expected_vas))
check("Four specifications exactly match the requested additive models", identical(specification_terms, expected_specifications))
check("The complete block of four school outcome means appears only in B", identical(school_mean_terms, expected_mean_terms) &&
  all(vapply(expected_mean_terms, function(term) identical(names(specification_terms)[vapply(
    specification_terms, function(terms) term %in% terms, logical(1))], "B"), logical(1))))
check("School outcome means use the requested raw outcomes", identical(school_mean_specs[, .(outcome, raw, standardized)], expected_mean_specs))
check("Only the ten identified fee terms enter all specifications", identical(active_fee_terms, expected_fees) &&
  !"monthly_fee_band_2" %in% unlist(specification_terms))
check("No parental characteristics or interaction terms enter any model", !any(grepl(
  "parent|income|_x_|:", unlist(specification_terms))))
preservation <- fread(file.path(clean_dir, "source_preservation.csv"))
check("All previous inputs and fitted results retain their recorded hashes", nrow(preservation) > 0L &&
  all(file.exists(preservation$path)) && identical(unname(tools::md5sum(preservation$path)), preservation$md5_before))
current_model_paths <- vapply(c("A", "B", "C", "D"), function(s) vapply(names(distance_vars), function(m)
  four_spec_model_path(s, m), character(1)), character(length(distance_vars)))
preserved_models <- preservation[path %in% as.vector(current_model_paths)]
check("All fitted objects recorded as unchanged retain their hashes", !anyDuplicated(preserved_models$path) &&
  identical(unname(tools::md5sum(preserved_models$path)), preserved_models$md5_before))
check("The expanded input builds on the unchanged directory-price rankings", normalizePath(four_spec_base_input_path, winslash = "/") ==
  normalizePath(file.path(source_price_dir, "estimation_data.rds"), winslash = "/"))
d <- as.data.table(readRDS(four_spec_input_path))
base <- as.data.table(readRDS(four_spec_base_input_path))
setorder(d, ranking_id, rank_school); setorder(base, ranking_id, rank_school)
check("Every previously constructed ranking column is preserved exactly", nrow(d) == nrow(base) &&
  all(names(base) %in% names(d)) && all(vapply(names(base), function(v) same_values(d[[v]], base[[v]]), logical(1))))
rm(base); invisible(gc()); d[, event := 1L]
check("The complete estimation sample contains the agreed applicants, pairs and schools", nrow(d) == 969377L &&
  uniqueN(d$ranking_id) == 273184L && uniqueN(d$mrun) == 273184L && uniqueN(d$rbd) == 2481L)
check("Submitted school lists remain unique and have consecutive ranks", !anyDuplicated(d[, .(ranking_id, rbd)]) &&
  !anyDuplicated(d[, .(ranking_id, rank_school)]) && all(d[, .(ok = .N >= 2L && min(rank_school) == 1L &&
    max(rank_school) == .N), by = ranking_id]$ok))
roster <- fread(file.path(source_price_dir, "ranking_roster.csv"))
check("Estimation includes exactly the eligible whole lists", setequal(unique(d$ranking_id),
  roster[price_estimation_eligible == TRUE, ranking_id]))
mean_manifest <- fread(file.path(school_mean_dir, "source_manifest.csv"))
check("School-outcome source files retain their recorded hashes", all(file.exists(mean_manifest$path)) &&
  identical(unname(tools::md5sum(mean_manifest$path)), mean_manifest$md5))
outcome_source_path <- file.path(data_root,
  "data/clean/school_rbd_observational_values/cohorts_2017_2020/school_rbd_observational_values.csv")
raw_outcomes <- fread(outcome_source_path, select = c("school_rbd", "analysis_sample", "outcome", "raw_mean",
  "n_students_outcome", "n_students_regression"), showProgress = FALSE)
raw_outcomes <- raw_outcomes[analysis_sample == "All" & outcome %chin% expected_mean_specs$outcome]
check("Unadjusted All-sample outcome means have unique school-outcome keys", !anyDuplicated(
  raw_outcomes[, .(school_rbd, outcome)]) && all(is.finite(raw_outcomes$raw_mean)) &&
  all(raw_outcomes$raw_mean >= 0 & raw_outcomes$raw_mean <= 1) && all(raw_outcomes$n_students_outcome > 0))
school_means <- fread(file.path(school_mean_dir, "school_outcome_means.csv"))
normalization <- fread(file.path(school_mean_dir, "normalization.csv"))
check("School-mean lookup covers precisely the fixed destination-school sample", nrow(school_means) == 2481L &&
  !anyDuplicated(school_means$rbd) && setequal(school_means$rbd, d$rbd))
check("Three new normalization records cover the exact raw outcome means", nrow(normalization) == 3L &&
  !anyDuplicated(normalization$outcome) && setequal(normalization$outcome, expected_mean_specs$outcome))
for (k in seq_len(nrow(expected_mean_specs))) {
  s <- expected_mean_specs[k]; suffix <- sub("^mean_", "", s$raw)
  raw_rows <- raw_outcomes[outcome == s$outcome]
  ix <- match(school_means$rbd, raw_rows$school_rbd)
  check(paste(s$outcome, "school lookup exactly reuses unadjusted source means"), !anyNA(ix) &&
    same_num(school_means[[s$raw]], raw_rows$raw_mean[ix], 0))
  check(paste(s$outcome, "raw-mean and regression denominators are kept separately"),
    same_num(school_means[[paste0("n_mean_", suffix)]], raw_rows$n_students_outcome[ix], 0) &&
    same_num(school_means[[paste0("n_regression_", suffix)]], raw_rows$n_students_regression[ix], 0))
  mu <- mean(raw_rows$raw_mean[ix]); sigma <- sd(raw_rows$raw_mean[ix])
  n <- normalization[outcome == s$outcome]
  check(paste(s$outcome, "normalization uses equal-weight destination schools"), nrow(n) == 1L &&
    n$schools == 2481L && n$raw == s$raw && n$standardized == s$standardized &&
    abs(n$mean - mu) < 1e-12 && abs(n$sd - sigma) < 1e-12 && sigma > 0)
  expected_z <- (raw_rows$raw_mean[ix] - mu) / sigma
  check(paste(s$outcome, "standardization has zero school mean and unit sample SD"),
    same_num(school_means[[s$standardized]], expected_z, 1e-12) && abs(mean(expected_z)) < 1e-12 &&
    abs(sd(expected_z) - 1) < 1e-12)
  di <- match(d$rbd, school_means$rbd)
  check(paste(s$outcome, "all applicant-school pairs inherit correct raw and standardized school means"),
    same_num(d[[s$raw]], school_means[[s$raw]][di], 0) &&
    same_num(d[[s$standardized]], expected_z[di], 1e-12) &&
    same_num(d[[paste0("n_mean_", suffix)]], school_means[[paste0("n_mean_", suffix)]][di], 0))
}
denominators <- raw_outcomes[, .(students = sum(n_students_outcome)), by = outcome]
check("Raw-mean source denominators match the broad outcome-specific populations", denominators[
  outcome == "admission_exam_taker", students] == 931547L && all(denominators[
    outcome != "admission_exam_taker", students] == 657134L))
rm(raw_outcomes); invisible(gc())
directory_path <- list.files(file.path(data_root, "data/raw/school_directory/2024"), pattern = "[.]csv$", full.names = TRUE)
stopifnot(length(directory_path) == 1L)
raw <- fread(directory_path, sep = ";", encoding = "UTF-8", select = c("AGNO", "RBD", "PAGO_MATRICULA", "PAGO_MENSUAL"))
raw_map <- c("SIN INFORMACION" = 0L, "GRATUITO" = 1L, "$1.000 A $10.000" = 2L,
  "$10.001 A $25.000" = 3L, "$25.001 A $50.000" = 4L, "$50.001 A $100.000" = 5L, "MAS DE $100.000" = 6L)
ix <- match(d$rbd, raw$RBD)
check("Every listed estimation school matches a unique 2024 directory school", !anyDuplicated(raw$RBD) &&
  !anyNA(ix) && all(raw$AGNO[ix] == 2024L))
for (field_name in c("PAGO_MATRICULA", "PAGO_MENSUAL")) {
  expected <- unname(raw_map[toupper(trimws(raw[[field_name]][ix]))])
  check(paste(field_name, "source fee codes are unchanged"), !anyNA(expected) &&
    same_num(d[[paste0(field_name, "_CODE")]], expected, 0))
  prefix <- if (field_name == "PAGO_MATRICULA") "enrollment_fee" else "monthly_fee"
  for (band in 2:6) {
    term <- paste0(prefix, "_band_", band)
    check(paste(term, "indicator matches the raw directory price band"), same_num(d[[term]], as.integer(expected == band), 0))
  }
}
check("Unknown fee information is shared and kept distinct from free", identical(d$PAGO_MATRICULA_CODE == 0L,
  d$PAGO_MENSUAL_CODE == 0L) && same_num(d$fee_information_unknown, as.integer(d$PAGO_MENSUAL_CODE == 0L), 0))
check("The excluded lowest monthly band has no observations", !any(d$PAGO_MENSUAL_CODE == 2L) &&
  all(d$monthly_fee_band_2 == 0L))
all_requested <- c(unique(unlist(expected_specifications)), unname(distance_vars))
check("All selected covariates and both distances are finite", all(vapply(d[, all_requested, with = FALSE],
  function(z) all(is.finite(z)), logical(1))))
co <- fread(file.path(clean_dir, "coefficients.csv"))
ms <- fread(file.path(clean_dir, "model_summary.csv"))
check("Eight fitted models cover four specifications and both distances", nrow(ms) == 8L &&
  !anyDuplicated(ms[, .(specification, model)]) && setequal(ms$specification, c("A", "B", "C", "D")) &&
  setequal(ms$model, names(distance_vars)))
check("All model summaries retain the identical complete sample", all(ms$applicants == 273184L) &&
  all(ms$rankings == 273184L) && all(ms$applicant_school_pairs == 969377L) && all(ms$schools == 2481L) &&
  all(ms$informative_rank_decisions == 696193L))
check("All coefficients and clustered errors are finite", all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0) && !any(grepl("parent|income|_x_|:", co$term)))
check("The eight exported school-mean coefficients occur only in B", nrow(co[term %in% expected_mean_terms]) == 8L &&
  all(co[term %in% expected_mean_terms, specification] == "B"))
ranking_group <- match(d$ranking_id, unique(d$ranking_id)); ranking_n <- tabulate(ranking_group)
# Both names are available so residual reconstruction honors the stored data call.
x <- d
for (s in c("A", "B", "C", "D")) for (m in names(distance_vars)) {
  key <- paste(s, m, sep = "_")
  wanted <- c(unname(distance_vars[m]), expected_specifications[[s]])
  fit <- readRDS(four_spec_model_path(s, m))
  b <- coef(fit); V <- vcov(fit)
  tab <- co[specification == s & model == m]; row <- ms[specification == s & model == m]
  check(paste(key, "saved coefficients exactly match the requested specification"), identical(names(b), wanted) &&
    nrow(tab) == length(wanted) && setequal(tab$term, wanted) && row$parameters == length(wanted))
  check(paste(key, "exports reproduce fitted coefficients and clustered errors"), same_num(b, tab$estimate[match(wanted, tab$term)]) &&
    same_num(sqrt(diag(V)), tab$std_error[match(wanted, tab$term)]))
  vf <- fread(file.path(clean_dir, paste0("vcov_", key, ".csv")))
  check(paste(key, "exported covariance matches the model"), identical(vf$term, wanted) &&
    same_num(as.matrix(vf[, ..wanted]), as.vector(V)) && max(abs(V - t(V))) < 1e-10 && all(diag(V) > 0))
  check(paste(key, "inference explicitly clusters by student"), identical(as.character(fit$call$cluster), "mrun") &&
    !is.null(fit$naive.var))
  check(paste(key, "model used every submitted alternative and converged"), fit$n == nrow(d) &&
    is.null(fit$na.action) && fit$iter < 60L && row$iterations == fit$iter &&
    abs(row$log_likelihood - as.numeric(logLik(fit))) < 1e-7 &&
    abs(row$null_log_likelihood - fit$loglik[1L]) < 1e-7)
  information <- solve(fit$naive.var)
  check(paste(key, "observed likelihood information is positive definite"),
    min(eigen(information, symmetric = TRUE, only.values = TRUE)$values) > 0 && all(is.finite(V)))
  X <- as.matrix(d[, ..wanted]); sums <- rowsum(X, ranking_group, reorder = FALSE)
  gram <- crossprod(X) - crossprod(sums / sqrt(ranking_n))
  eig <- eigen(gram, symmetric = TRUE, only.values = TRUE)$values
  check(paste(key, "requested covariates are identified within rankings"), sum(eig > max(eig) * 1e-10) == length(wanted))
  rm(X, sums, gram)
  score <- residuals(fit, type = "score")
  check(paste(key, "score gradient is small per applicant ranking"),
    max(abs(colSums(score))) / uniqueN(d$ranking_id) < 1e-7)
  if (s == "D" && m == "main") {
    cluster_score <- rowsum(score, d$mrun, reorder = FALSE)
    sandwich <- fit$naive.var %*% crossprod(cluster_score) %*% fit$naive.var
    check("Specification D main student-clustered covariance reconstructs independently",
      same_num(as.vector(sandwich), as.vector(V), 1e-9))
    rm(cluster_score, sandwich)
  }
  rm(score, fit); invisible(gc())
}
for (m in names(distance_vars)) {
  ll <- setNames(ms[model == m, log_likelihood], ms[model == m, specification])
  check(paste(m, "adding school type and four outcome means weakly improves A to B"), ll["B"] >= ll["A"] - 1e-7)
  check(paste(m, "adding four VAs weakly improves A to C"), ll["C"] >= ll["A"] - 1e-7)
  check(paste(m, "adding school type alone weakly improves C to D"), ll["D"] >= ll["C"] - 1e-7)
}
check("All preceding input and model hashes are unchanged after verification", identical(
  unname(tools::md5sum(preservation$path)), preservation$md5_before))
save_checks("verification.csv")
message("Independent four-specification verification: ", sum(checks$passed), "/", nrow(checks), " checks passed.")
