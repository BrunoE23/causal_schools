# Independent likelihood and student-cluster covariance checks for the joint ROL.
source("code/codex/school_preference_va/common.R")
suppressPackageStartupMessages(library(survival))

input_path <- file.path(clean_dir, "estimation_data.rds")
stopifnot(file.exists(input_path))
message("Validation input: ", format(file.info(input_path)$size / 1024^2, digits = 3), " MB")
est <- as.data.table(readRDS(input_path))
required <- c("mrun", "ranking_id", "sae_proceso", "rbd", "rank_school", model_static_terms, distance_vars)
check("Validation input columns", all(required %in% names(est)))
check("No missing joint-regression inputs", all(complete.cases(est[, ..required])))
check("Unique school within each submitted ranking", !anyDuplicated(est[, .(ranking_id, rbd)]))
check("Unique rank within each submitted ranking", !anyDuplicated(est[, .(ranking_id, rank_school)]))
setorder(est, ranking_id, rank_school)
roster <- est[, .(alternatives = .N, students = uniqueN(mrun), years = uniqueN(sae_proceso),
  consecutive = identical(as.integer(rank_school), seq_len(.N))), by = ranking_id]
check("Every estimation ranking has at least two schools", all(roster$alternatives >= 2L))
check("Rankings contain one student and process", all(roster$students == 1L & roster$years == 1L))
check("Collapsed school ranks are consecutive", all(roster$consecutive))
check("All VA, school-type and distance regressors finite", all(vapply(est[, c(model_static_terms, distance_vars), with = FALSE],
  function(z) all(is.finite(z)), logical(1))))
split_type_terms <- c("mixed_hc_tp", "tp_only")
check("Requested specification contains four VAs and separate mixed and TP-only indicators",
  identical(controls, split_type_terms) && identical(model_static_terms, c(va_terms, split_type_terms)) &&
    !"tp_school" %in% model_static_terms)
check("School-type indicators are observed unstandardized binaries",
  all(vapply(est[, ..split_type_terms], function(x) all(x %in% 0:1), logical(1))))
check("Mixed HC/TP and TP-only indicators are mutually exclusive",
  all(est$mixed_hc_tp + est$tp_only <= 1L))
check("Both school-type indicators are constant within destination school",
  all(est[, lapply(.SD, uniqueN), by = rbd, .SDcols = split_type_terms][,
    unlist(.SD), .SDcols = split_type_terms] == 1L))
type_source <- fread(file.path(clean_dir, "school_type_classification.csv"))
check("School-type classification source contains unique destination schools",
  all(c("rbd", split_type_terms) %in% names(type_source)) && !anyDuplicated(type_source$rbd))
type_index <- match(est$rbd, type_source$rbd)
check("All estimation schools match the school-type classification source", !anyNA(type_index))
for (type_term in split_type_terms) {
  check(paste(type_term, "matches saved Directorio school-type classification"),
    all(est[[type_term]] == type_source[[type_term]][type_index]))
}
if ("tp_school" %in% names(est)) {
  check("Retained TP metadata remains unstandardized binary", all(est$tp_school %in% c(0L, 1L)))
  check("Retained TP metadata constant within destination school", all(est[, uniqueN(tp_school), by = rbd]$V1 == 1L))
  check("Split school-type indicators partition the retained any-TP metadata",
    all(est$mixed_hc_tp + est$tp_only == est$tp_school))
}
check("Standardized VA constant by school across both specifications", all(est[,
  lapply(.SD, uniqueN), by = rbd, .SDcols = va_terms][, unlist(.SD), .SDcols = va_terms] == 1L))
normalization <- fread(file.path(clean_dir, "va_normalization.csv"))
school_z <- unique(est[, c("rbd", va_terms), with = FALSE], by = "rbd")
source_va <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome",
  "controlled_value_added_eb_centered_student"))
source_va <- source_va[analysis_sample == "All" & school_rbd %in% school_z$rbd]
for (k in seq_len(nrow(specs))) {
  term <- specs$standardized[k]
  norm <- normalization[standardized_variable == term]
  check(paste(term, "one saved normalization"), nrow(norm) == 1L)
  raw <- source_va[outcome == specs$outcome[k]]
  index <- match(school_z$rbd, raw$school_rbd)
  expected_z <- (raw$controlled_value_added_eb_centered_student[index] - norm$reference_mean) / norm$reference_sd
  check(paste(term, "matches original EB index and saved normalization"),
    all(is.finite(expected_z)) && max(abs(expected_z - school_z[[term]])) < 1e-12)
  check(paste(term, "normalization uses unique estimation schools"),
    norm$n_reference_schools == nrow(school_z) && abs(mean(school_z[[term]])) < 1e-12 &&
      abs(sd(school_z[[term]]) - 1) < 1e-12)
}
est[, event1 := 1L]

# A deterministic regular subsample, enriched with long lists and repeated-student
# rankings where those exist. Whole rankings, rather than individual stages, enter.
max_rankings <- 1500L
regular_ids <- roster$ranking_id[unique(as.integer(round(seq(1, nrow(roster),
  length.out = min(1000L, nrow(roster))))))]
long_ids <- roster[order(-alternatives, ranking_id), head(ranking_id, 250L)]
repeat_students <- est[, .(rankings = uniqueN(ranking_id)), by = mrun][rankings > 1L, mrun]
repeat_ids <- unique(est[mrun %in% repeat_students, ranking_id])
builder_roster <- fread(file.path(clean_dir, "ranking_roster.csv"),
  select = c("ranking_id", "repeat_school_programs", "interleaved_school_programs", "estimation_eligible"))
interleaved_ids <- builder_roster[estimation_eligible == TRUE & interleaved_school_programs == TRUE,
  head(ranking_id, 50L)]
repeated_campus_ids <- builder_roster[estimation_eligible == TRUE & repeat_school_programs == TRUE,
  head(ranking_id, 100L)]
sample_ids <- unique(c(interleaved_ids, repeated_campus_ids, regular_ids, long_ids, head(repeat_ids, 250L)))
sample_ids <- head(sample_ids, max_rankings)
sub <- copy(est[ranking_id %in% sample_ids])
sub[, validation_ranking := match(ranking_id, unique(ranking_id))]
check("Independent-estimator subsample contains complete rankings", nrow(sub) ==
  roster[ranking_id %in% sample_ids, sum(alternatives)])

# Each conditional-logit stratum is one rank decision among the remaining schools.
# The final singleton stage contributes exactly zero and is omitted here.
expanded <- rbindlist(lapply(split(sub, by = "ranking_id", keep.by = TRUE), function(z) {
  rbindlist(lapply(seq_len(nrow(z) - 1L), function(r) {
    part <- copy(z[r:nrow(z)])
    part[, `:=`(chosen = as.integer(seq_len(.N) == 1L),
      stage_id = paste0(validation_ranking[1L], "_", r))]
    part
  }))
}))
check("One chosen alternative per exploded risk set", all(expanded[, sum(chosen), by = stage_id]$V1 == 1L))

direct_formula <- function(distance) as.formula(paste("Surv(rank_school, event1) ~",
  paste(c(model_static_terms, distance), collapse = " + "), "+ strata(ranking_id)"))
expanded_formula <- function(distance) as.formula(paste("chosen ~",
  paste(c(model_static_terms, distance), collapse = " + "), "+ strata(stage_id)"))

# Centering within each ranking prevents overflow; reverse cumulative sums are
# exactly the submitted-list Plackett-Luce denominators at each ranked position.
pl_loglik <- function(x, beta, terms) {
  eta <- as.numeric(as.matrix(x[, ..terms]) %*% beta)
  work <- data.table(ranking_id = x$ranking_id, eta = eta)
  work[, {
    centered <- eta - max(eta)
    denominator <- rev(cumsum(rev(exp(centered))))
    if (any(denominator == 0)) {
      # Defensive stable log-add-exp fallback for extreme fitted utilities.
      log_denominator <- centered
      if (.N > 1L) for (r in seq.int(.N - 1L, 1L)) {
        a <- centered[r]; b <- log_denominator[r + 1L]; largest <- max(a, b)
        log_denominator[r] <- largest + log(exp(a - largest) + exp(b - largest))
      }
    } else log_denominator <- log(denominator)
    .(loglik = sum(centered - log_denominator))
  }, by = ranking_id][, sum(loglik)]
}

comparison <- list()
conditions <- list()
saved_models <- list()
control <- coxph.control(eps = 1e-10, iter.max = 100L)
for (model_name in names(distance_vars)) {
  message("Validating joint rank-ordered logit: ", model_name)
  distance <- unname(distance_vars[model_name])
  terms <- c(model_static_terms, distance)
  fit <- readRDS(file.path(clean_dir, "models", paste0(model_name, ".rds")))
  saved_models[[model_name]] <- fit
  check(paste(model_name, "saved object is coxph"), inherits(fit, "coxph"))
  check(paste(model_name, "all requested coefficients"), identical(names(coef(fit)), terms))
  check(paste(model_name, "single any-TP indicator absent from fitted coefficients and formula"),
    !"tp_school" %in% names(coef(fit)) && !"tp_school" %in% all.vars(fit$call$formula))
  check(paste(model_name, "finite coefficients and student-cluster SEs"),
    all(is.finite(coef(fit))) && all(is.finite(vcov(fit))) && all(diag(vcov(fit)) > 0))
  check(paste(model_name, "saved robust covariance is available"), !is.null(fit$naive.var))
  check(paste(model_name, "saved cluster is student"),
    identical(gsub("\\s", "", paste(deparse(fit$call$cluster), collapse = "")), "mrun"))
  check(paste(model_name, "saved observations equal common ranking dataset"), fit$n == nrow(est))
  check(paste(model_name, "saved event count equals full rankings"), fit$nevent == nrow(est))
  check(paste(model_name, "saved event times and ranks equal common input"),
    !is.null(fit$y) && identical(as.numeric(fit$y[, 1L]), as.numeric(est$rank_school)) &&
      all(fit$y[, 2L] == 1))
  check(paste(model_name, "Breslow likelihood"), identical(fit$method, "breslow"))
  independent_likelihood <- pl_loglik(est, coef(fit), terms)
  stored_likelihood <- unname(tail(fit$loglik, 1L))
  full_difference <- abs(independent_likelihood - stored_likelihood)
  check(paste(model_name, "independent full-sample Plackett-Luce likelihood"), full_difference < 1e-6,
    format(full_difference, digits = 10))

  direct <- coxph(direct_formula(distance), data = sub, ties = "breslow", cluster = mrun,
    na.action = na.fail, control = control)
  exploded <- clogit(expanded_formula(distance), data = expanded, method = "breslow", cluster = mrun,
    na.action = na.fail, control = control)
  beta_difference <- max(abs(coef(direct) - coef(exploded)))
  vcov_difference <- max(abs(vcov(direct) - vcov(exploded)))
  likelihood_difference <- abs(tail(direct$loglik, 1L) - tail(exploded$loglik, 1L))
  check(paste(model_name, "Cox and exploded conditional-logit coefficients"), beta_difference < 1e-7,
    format(beta_difference, digits = 10))
  check(paste(model_name, "Cox and exploded student-cluster covariance"), vcov_difference < 1e-7,
    format(vcov_difference, digits = 10))
  check(paste(model_name, "Cox and exploded conditional-logit likelihood"), likelihood_difference < 1e-7,
    format(likelihood_difference, digits = 10))
  comparison[[model_name]] <- data.table(model = model_name, rankings = uniqueN(sub$ranking_id),
    student_clusters = uniqueN(sub$mrun), alternatives = nrow(sub), exploded_rows = nrow(expanded),
    max_abs_coefficient_difference = beta_difference, max_abs_student_cluster_vcov_difference = vcov_difference,
    abs_subsample_loglik_difference = likelihood_difference, independent_full_loglik = independent_likelihood,
    stored_full_loglik = stored_likelihood, abs_full_loglik_difference = full_difference)

  # Rank of within-ranking differences is the relevant design-rank check: levels
  # constant within a ranking cancel from this conditional likelihood.
  x <- as.matrix(est[, ..terms])
  group_index <- match(est$ranking_id, unique(est$ranking_id))
  group_sums <- rowsum(x, group_index, reorder = FALSE)
  group_n <- tabulate(group_index)
  gram <- crossprod(x) - crossprod(sweep(group_sums, 1L, sqrt(group_n), "/"))
  gram <- (gram + t(gram)) / 2
  gram_eigen <- eigen(gram, symmetric = TRUE, only.values = TRUE)$values
  rank_within <- qr(gram, tol = 1e-10)$rank
  information <- solve(fit$naive.var)
  information <- (information + t(information)) / 2
  information_eigen <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
  check(paste(model_name, "all within-ranking regressors identified"), rank_within == length(terms) &&
    all(is.finite(gram_eigen)) && min(gram_eigen) > max(gram_eigen) * 1e-12)
  check(paste(model_name, "finite positive observed likelihood information"),
    all(is.finite(information)) && all(is.finite(information_eigen)) && min(information_eigen) > 0)
  conditions[[model_name]] <- data.table(model = model_name, within_ranking_design_rank = rank_within,
    within_ranking_gram_min_eigenvalue = min(gram_eigen),
    within_ranking_gram_condition = max(gram_eigen) / min(gram_eigen),
    observed_information_min_eigenvalue = min(information_eigen),
    observed_information_condition = max(information_eigen) / min(information_eigen))
  rm(x, group_sums)
}

check("Both models use the same rankings and normalization", all(vapply(saved_models,
  function(z) z$n == nrow(est), logical(1))) &&
  identical(names(coef(saved_models$main))[seq_along(model_static_terms)],
    names(coef(saved_models$municipality))[seq_along(model_static_terms)]) &&
  identical(attr(saved_models$main, "preference_model")$estimation_data,
    attr(saved_models$municipality, "preference_model")$estimation_data) &&
  identical(attr(saved_models$main, "preference_model")$normalization_file,
    attr(saved_models$municipality, "preference_model")$normalization_file))
fwrite(rbindlist(comparison), file.path(clean_dir, "validation_comparison.csv"))
fwrite(rbindlist(conditions), file.path(clean_dir, "validation_condition_diagnostics.csv"))
save_checks("verification.csv")
message("All ", nrow(checks), " independent model verification checks passed.")
