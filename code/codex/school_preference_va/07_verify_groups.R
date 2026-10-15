# Independently validate the four priority-by-continuation preference models.
# Run only after 05_fit_by_priority_continuation.R has finished. This script does
# not refit the full models: independent refits use small, complete-list samples.
source("code/codex/school_preference_va/common.R")
suppressPackageStartupMessages(library(survival))

pooled_dir <- clean_dir
clean_dir <- file.path(pooled_dir, "by_priority_continuation")
stopifnot(dir.exists(clean_dir))
group_order <- c("ending_nonpriority", "ending_priority", "voluntary_nonpriority", "voluntary_priority")
expected_sample <- c(ending_nonpriority = "school_ending", ending_priority = "school_ending",
  voluntary_nonpriority = "voluntary", voluntary_priority = "voluntary")
expected_priority <- c(ending_nonpriority = 0L, ending_priority = 1L,
  voluntary_nonpriority = 0L, voluntary_priority = 1L)

est <- as.data.table(readRDS(file.path(clean_dir, "estimation_data.rds")))
pooled <- as.data.table(readRDS(file.path(pooled_dir, "estimation_data.rds")))
split_type_terms <- c("mixed_hc_tp", "tp_only")
check("Eight interaction specifications cover every type-by-VA combination",
  nrow(interaction_specs) == 8L && all(c("control", "va", "term") %in% names(interaction_specs)) &&
    !anyDuplicated(interaction_specs$term) && !anyDuplicated(interaction_specs[, .(control, va)]) &&
    setequal(paste(interaction_specs$control, interaction_specs$va),
      as.vector(outer(split_type_terms, va_terms, paste))))
for (input_name in c("est", "pooled")) {
  x <- get(input_name)
  for (k in seq_len(nrow(interaction_specs))) {
    term <- interaction_specs$term[k]
    if (term %in% names(x)) {
      check(paste(input_name, term, "stored input is the exact type-by-VA product"),
        identical(x[[term]], x[[interaction_specs$control[k]]] * x[[interaction_specs$va[k]]]))
    }
  }
  x <- add_va_interactions(x)
  for (k in seq_len(nrow(interaction_specs))) {
    check(paste(input_name, interaction_specs$term[k], "independent product reconstruction"),
      identical(x[[interaction_specs$term[k]]],
        x[[interaction_specs$control[k]]] * x[[interaction_specs$va[k]]]))
  }
  assign(input_name, x)
}
rm(x)
roster <- fread(file.path(clean_dir, "group_roster.csv"))
builder_roster <- fread(file.path(pooled_dir, "ranking_roster.csv"))
definitions <- fread(file.path(clean_dir, "group_definitions.csv"))
coefficients <- fread(file.path(clean_dir, "coefficients.csv"))
summaries <- fread(file.path(clean_dir, "model_summary.csv"))
diagnostics <- fread(file.path(clean_dir, "model_diagnostics.csv"))
slopes <- fread(file.path(clean_dir, "type_specific_va_slopes.csv"))
slope_type_controls <- c("Non-TP" = NA_character_, "Mixed HC/TP" = "mixed_hc_tp", "TP only" = "tp_only")
normalization <- fread(file.path(clean_dir, "va_normalization.csv"))
pooled_normalization <- fread(file.path(pooled_dir, "va_normalization.csv"))
shared <- names(pooled)
required <- unique(c(shared, "sample", "continuation_group", "prioritario", "group"))
check("Subgroup input contains pooled columns and all group flags", all(required %in% names(est)))
check("All four definitions supplied exactly once", !anyDuplicated(definitions$group) &&
  setequal(definitions$group, group_order))
check("Pooled school normalization file preserved byte for byte",
  identical(unname(tools::md5sum(file.path(clean_dir, "va_normalization.csv"))),
    unname(tools::md5sum(file.path(pooled_dir, "va_normalization.csv")))))
check("Pooled normalization values unchanged", identical(normalization, pooled_normalization))
check("All four groups and both models exported", nrow(summaries) == 8L &&
  !anyDuplicated(summaries[, .(group, model)]) && setequal(summaries$group, group_order) &&
  setequal(summaries$model, names(distance_vars)))
check("One diagnostic row per subgroup model", nrow(diagnostics) == 8L &&
  !anyDuplicated(diagnostics[, .(group, model)]))
expected_coefficient_rows <- length(group_order) * length(distance_vars) * (length(model_static_terms) + 1L)
check("One coefficient row per requested subgroup model term", nrow(coefficients) == expected_coefficient_rows &&
  !anyDuplicated(coefficients[, .(group, model, term)]))
check("Requested specification contains four VAs, two type indicators and eight interactions",
  identical(controls, split_type_terms) && identical(interaction_terms, interaction_specs$term) &&
    identical(model_static_terms, c(va_terms, split_type_terms, interaction_terms)) && length(model_static_terms) == 14L &&
    !"tp_school" %in% model_static_terms)
check("Single any-TP indicator absent from exported subgroup coefficients", !"tp_school" %in% coefficients$term)
check("Exactly twelve type-specific VA slopes per group and distance model",
  nrow(slopes) == length(group_order) * length(distance_vars) * nrow(specs) * length(slope_type_controls) &&
    !anyDuplicated(slopes[, .(group, model, school_type, outcome)]) &&
    setequal(slopes$group, group_order) && setequal(slopes$model, names(distance_vars)) &&
    setequal(slopes$school_type, names(slope_type_controls)) && setequal(slopes$outcome, specs$outcome))

# Reconstruct the classification from the existing paper sources, independently
# of the estimation script. This also verifies unresolved origins are preserved
# in the population roster rather than silently coded as voluntary applicants.
paper_roster <- as.data.table(readRDS(roster_path))[, .(mrun, sae_proceso, sample, continuation_group)]
priority_path <- file.path(root, "data/clean/empirical_bayes_school_va",
  "va_2017_2020__sae_2018_2020/prioritario_sae_2018_2020.csv")
priority <- fread(priority_path, select = c("mrun", "sae_proceso", "prioritario"))
check("Priority source has unique applicant-process keys", !anyDuplicated(priority[, .(mrun, sae_proceso)]))
check("Paper roster has unique applicants", nrow(paper_roster) == 294198L && !anyDuplicated(paper_roster$mrun))
check("Constructed roster preserves every original ranking", nrow(roster) == 294198L &&
  !anyDuplicated(roster$ranking_id) && setequal(roster$ranking_id, builder_roster$ranking_id))
source_groups <- merge(paper_roster, priority, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
source_groups[, expected_group := fcase(
  sample == "school_ending" & prioritario == 0L, "ending_nonpriority",
  sample == "school_ending" & prioritario == 1L, "ending_priority",
  sample == "voluntary" & prioritario == 0L, "voluntary_nonpriority",
  sample == "voluntary" & prioritario == 1L, "voluntary_priority",
  default = "unresolved_other")]
source_index <- match(paste(roster$mrun, roster$sae_proceso), paste(source_groups$mrun, source_groups$sae_proceso))
check("All roster applicants join original group sources", !anyNA(source_index))
same_including_na <- function(a, b) length(a) == length(b) &&
  all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
for (flag in c("sample", "continuation_group", "prioritario")) {
  check(paste("Roster", flag, "matches direct paper source"),
    same_including_na(roster[[flag]], source_groups[[flag]][source_index]))
}
check("Four-way membership matches independently reconstructed sources",
  identical(roster$group, source_groups$expected_group[source_index]))
check("Unresolved applicants remain outside four regression groups",
  all(roster[sample == "unresolved_other", group] == "unresolved_other"))
check("Missing priority is never coded as nonprioritario",
  all(roster[is.na(prioritario), group] == "unresolved_other"))

setorder(est, ranking_id, rank_school)
setorder(pooled, ranking_id, rank_school)
check("Unique school within every subgroup ranking", !anyDuplicated(est[, .(ranking_id, rbd)]))
check("Unique submitted rank within every subgroup ranking", !anyDuplicated(est[, .(ranking_id, rank_school)]))
check("Only the four requested groups enter estimation", setequal(est$group, group_order))
check("All regressors observed and finite", all(vapply(est[, c(model_static_terms, distance_vars), with = FALSE],
  function(x) all(is.finite(x)), logical(1))))
ranking_check <- est[, .(alternatives = .N, students = uniqueN(mrun), processes = uniqueN(sae_proceso),
  groups = uniqueN(group), priorities = uniqueN(prioritario), samples = uniqueN(sample),
  consecutive = identical(as.integer(rank_school), seq_len(.N))), by = ranking_id]
check("Complete consecutive rankings with at least two schools", all(ranking_check$alternatives >= 2L & ranking_check$consecutive))
check("Exactly one applicant process and group per ranking", all(ranking_check$students == 1L &
  ranking_check$processes == 1L & ranking_check$groups == 1L & ranking_check$priorities == 1L & ranking_check$samples == 1L))
check("Students are disjoint across the four groups", all(est[, uniqueN(group), by = mrun]$V1 == 1L))
index_roster <- match(est$ranking_id, roster$ranking_id)
check("Every estimation alternative matches roster membership", !anyNA(index_roster) &&
  identical(est$group, roster$group[index_roster]) &&
  same_including_na(est$prioritario, roster$prioritario[index_roster]) &&
  identical(est$sample, roster$sample[index_roster]) &&
  identical(est$continuation_group, roster$continuation_group[index_roster]))
eligible_ids <- intersect(pooled$ranking_id, roster[group %in% group_order, ranking_id])
check("All eligible pooled rankings retained in the four-group union", setequal(est$ranking_id, eligible_ids))
expected_rows <- pooled[ranking_id %in% eligible_ids]
check("Entire submitted lists and all pooled regressors preserved without changes",
  isTRUE(all.equal(est[, ..shared], expected_rows[, ..shared], check.attributes = FALSE, tolerance = 0)))
check("Roster sizes equal complete submitted list lengths", all(ranking_check$alternatives ==
  roster$n_schools[match(ranking_check$ranking_id, roster$ranking_id)]))
if ("tp_school" %in% names(est)) {
  check("Retained TP metadata remains an unstandardized observed binary", all(est$tp_school %in% 0:1))
  check("Split school-type indicators partition the retained any-TP metadata",
    all(est$mixed_hc_tp + est$tp_only == est$tp_school))
}
check("Both school-type indicators are observed unstandardized binaries",
  all(vapply(est[, ..split_type_terms], function(x) all(x %in% 0:1), logical(1))))
check("Mixed HC/TP and TP-only indicators are mutually exclusive",
  all(est$mixed_hc_tp + est$tp_only <= 1L))
check("Both school-type indicators are constant within destination school",
  all(est[, lapply(.SD, uniqueN), by = rbd, .SDcols = split_type_terms][,
    unlist(.SD), .SDcols = split_type_terms] == 1L))
type_source <- fread(file.path(pooled_dir, "school_type_classification.csv"))
check("School-type classification source contains unique destination schools",
  all(c("rbd", split_type_terms) %in% names(type_source)) && !anyDuplicated(type_source$rbd))
type_index <- match(est$rbd, type_source$rbd)
check("All subgroup schools match the school-type classification source", !anyNA(type_index))
for (type_term in split_type_terms) {
  check(paste(type_term, "matches saved Directorio school-type classification"),
    all(est[[type_term]] == type_source[[type_term]][type_index]))
}
est[, event1 := 1L]

direct_formula <- function(distance) as.formula(paste("Surv(rank_school, event1) ~",
  paste(c(model_static_terms, distance), collapse = " + "), "+ strata(ranking_id)"))
expanded_formula <- function(distance) as.formula(paste("chosen ~",
  paste(c(model_static_terms, distance), collapse = " + "), "+ strata(stage_id)"))

# Independent exact PL likelihood, preserving every remaining school at each
# submitted position. Each complete ranking is centered to prevent overflow.
pl_loglik <- function(d, beta, terms) {
  eta <- as.numeric(as.matrix(d[, ..terms]) %*% beta)
  work <- data.table(ranking_id = d$ranking_id, eta = eta)
  work[, {
    centered <- eta - max(eta)
    denominator <- rev(cumsum(rev(exp(centered))))
    if (any(denominator == 0)) {
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
control <- coxph.control(eps = 1e-10, iter.max = 100L)
for (group_name in group_order) {
  message("Independent group verification: ", group_name)
  d <- est[group == group_name]
  check(paste(group_name, "priority membership"), all(d$prioritario == expected_priority[[group_name]]))
  check(paste(group_name, "continuation membership"), all(d$sample == expected_sample[[group_name]]) &&
    all(d$continuation_group == if (expected_sample[[group_name]] == "school_ending")
      "Ends at 8 in SAE offerings" else "Offers grade 9"))
  ranks <- d[, .(alternatives = .N), by = ranking_id]
  regular_ids <- ranks$ranking_id[unique(as.integer(round(seq(1, nrow(ranks), length.out = min(100L, nrow(ranks))))))]
  long_ids <- ranks[order(-alternatives, ranking_id), head(ranking_id, 20L)]
  sample_ids <- unique(c(regular_ids, long_ids))
  sub <- copy(d[ranking_id %in% sample_ids])
  sub[, validation_ranking := match(ranking_id, unique(ranking_id))]
  check(paste(group_name, "validation sample preserves full rankings"),
    nrow(sub) == ranks[ranking_id %in% sample_ids, sum(alternatives)])
  expanded <- rbindlist(lapply(split(sub, by = "ranking_id", keep.by = TRUE), function(z) {
    rbindlist(lapply(seq_len(nrow(z) - 1L), function(r) {
      part <- copy(z[r:nrow(z)])
      part[, `:=`(chosen = as.integer(seq_len(.N) == 1L),
        stage_id = paste0(validation_ranking[1L], "_", r))]
      part
    }))
  }))
  check(paste(group_name, "one chosen school per expanded choice stage"),
    all(expanded[, sum(chosen), by = stage_id]$V1 == 1L))
  saved <- list()
  for (model_name in names(distance_vars)) {
    label <- paste(group_name, model_name)
    distance <- unname(distance_vars[model_name])
    terms <- c(model_static_terms, distance)
    fit <- readRDS(file.path(clean_dir, "models", paste0(group_name, "_", model_name, ".rds")))
    saved[[model_name]] <- fit
    check(paste(label, "saved coxph model with exactly requested terms"),
      inherits(fit, "coxph") && identical(names(coef(fit)), terms))
    check(paste(label, "single any-TP indicator absent from fitted coefficients and formula"),
      !"tp_school" %in% names(coef(fit)) && !"tp_school" %in% all.vars(fit$call$formula))
    check(paste(label, "student clustering and robust covariance"),
      !is.null(fit$naive.var) && identical(gsub("\\s", "", paste(deparse(fit$call$cluster), collapse = "")), "mrun"))
    check(paste(label, "finite coefficients and positive clustered variance"),
      all(is.finite(coef(fit))) && all(is.finite(vcov(fit))) && all(diag(vcov(fit)) > 0))
    check(paste(label, "sample size and events preserve every alternative"), fit$n == nrow(d) &&
      fit$nevent == nrow(d) && is.null(fit$na.action))
    check(paste(label, "event ranks equal complete common input"), !is.null(fit$y) &&
      identical(as.numeric(fit$y[, 1L]), as.numeric(d$rank_school)) && all(fit$y[, 2L] == 1))
    check(paste(label, "Breslow representation without rank ties"), identical(fit$method, "breslow"))
    formula_terms <- attr(terms(fit$call$formula), "term.labels")
    formula_terms <- setdiff(formula_terms, grep("^strata\\(", formula_terms, value = TRUE))
    check(paste(label, "stored formula uses requested model"), setequal(formula_terms, terms) &&
      any(grepl("strata\\(ranking_id\\)", deparse(fit$call$formula))))
    info <- solve(fit$naive.var)
    eig <- eigen((info + t(info)) / 2, symmetric = TRUE, only.values = TRUE)$values
    check(paste(label, "positive definite likelihood information"), all(is.finite(eig)) && min(eig) > 0)
    row_summary <- summaries[group == group_name & model == model_name]
    check(paste(label, "summary counts agree with exact subgroup input"), nrow(row_summary) == 1L &&
      row_summary$applicants == uniqueN(d$mrun) && row_summary$rankings == uniqueN(d$ranking_id) &&
      row_summary$applicant_school_pairs == nrow(d) && row_summary$schools == uniqueN(d$rbd))
    row_coef <- coefficients[group == group_name & model == model_name]
    row_coef <- row_coef[match(terms, term)]
    check(paste(label, "exported coefficients and standard errors equal saved model"),
      identical(row_coef$term, terms) && max(abs(row_coef$estimate - coef(fit))) < 1e-12 &&
        max(abs(row_coef$std_error - sqrt(diag(vcov(fit))))) < 1e-12)
    exported_cov <- fread(file.path(clean_dir, paste0("vcov_", group_name, "_", model_name, ".csv")))
    check(paste(label, "exported clustered covariance equals saved model"),
      identical(exported_cov$term, terms) && identical(setdiff(names(exported_cov), "term"), terms) &&
        max(abs(as.matrix(exported_cov[, ..terms]) - vcov(fit))) < 1e-12)
    for (school_type_name in names(slope_type_controls)) for (k in seq_len(nrow(specs))) {
      base_term <- specs$standardized[k]
      type_control <- slope_type_controls[[school_type_name]]
      contrast_terms <- base_term
      if (!is.na(type_control)) {
        interaction_term <- interaction_specs[control == type_control & va == base_term, term]
        check(paste(label, school_type_name, base_term, "unique interaction"), length(interaction_term) == 1L)
        contrast_terms <- c(contrast_terms, interaction_term)
      }
      a <- setNames(numeric(length(coef(fit))), names(coef(fit)))
      a[contrast_terms] <- 1
      expected_estimate <- sum(a * coef(fit))
      expected_se <- sqrt(as.numeric(crossprod(a, vcov(fit) %*% a)))
      row_slope <- slopes[group == group_name & model == model_name & school_type == school_type_name &
        outcome == specs$outcome[k]]
      check(paste(label, school_type_name, base_term, "slope estimate and student-clustered contrast SE"),
        nrow(row_slope) == 1L && row_slope$term == base_term &&
          abs(row_slope$estimate - expected_estimate) < 1e-12 &&
          abs(row_slope$std_error - expected_se) < 1e-12)
      expected_z <- expected_estimate / expected_se
      check(paste(label, school_type_name, base_term, "slope inference uses full clustered covariance"),
        abs(row_slope$z - expected_z) < 1e-10 &&
          abs(row_slope$p_value - 2 * pnorm(-abs(expected_z))) < 1e-12 &&
          abs(row_slope$lower95 - (expected_estimate - qnorm(.975) * expected_se)) < 1e-12 &&
          abs(row_slope$upper95 - (expected_estimate + qnorm(.975) * expected_se)) < 1e-12 &&
          setequal(strsplit(gsub("\\s", "", row_slope$contrast_terms), "+", fixed = TRUE)[[1L]], contrast_terms))
    }
    exact_ll <- pl_loglik(d, coef(fit), terms)
    stored_ll <- as.numeric(tail(fit$loglik, 1L))
    full_difference <- abs(exact_ll - stored_ll)
    check(paste(label, "exact full-sample Plackett-Luce likelihood"), full_difference < 1e-6,
      format(full_difference, digits = 10))
    check(paste(label, "exported likelihood equals saved model"),
      abs(row_summary$log_likelihood - stored_ll) < 1e-6)

    direct <- coxph(direct_formula(distance), data = sub, ties = "breslow", cluster = mrun,
      na.action = na.fail, control = control)
    exploded <- clogit(expanded_formula(distance), data = expanded, method = "breslow", cluster = mrun,
      na.action = na.fail, control = control)
    beta_difference <- max(abs(coef(direct) - coef(exploded)))
    vcov_difference <- max(abs(vcov(direct) - vcov(exploded)))
    likelihood_difference <- abs(tail(direct$loglik, 1L) - tail(exploded$loglik, 1L))
    check(paste(label, "expanded conditional-logit coefficient equivalence"), beta_difference < 1e-7,
      format(beta_difference, digits = 10))
    check(paste(label, "expanded student-cluster covariance equivalence"), vcov_difference < 1e-7,
      format(vcov_difference, digits = 10))
    check(paste(label, "expanded conditional-logit likelihood equivalence"), likelihood_difference < 1e-7,
      format(likelihood_difference, digits = 10))
    comparison[[paste(group_name, model_name, sep = "_")]] <- data.table(group = group_name, model = model_name,
      rankings = uniqueN(sub$ranking_id), student_clusters = uniqueN(sub$mrun), alternatives = nrow(sub),
      exploded_rows = nrow(expanded), max_abs_coefficient_difference = beta_difference,
      max_abs_student_cluster_vcov_difference = vcov_difference, abs_subsample_loglik_difference = likelihood_difference,
      independent_full_loglik = exact_ll, stored_full_loglik = stored_ll, abs_full_loglik_difference = full_difference)
  }
  check(paste(group_name, "both distance models use identical rankings and normalization"),
    identical(saved$main$y, saved$municipality$y) && saved$main$n == saved$municipality$n &&
      identical(attr(saved$main, "preference_model")$estimation_data,
        attr(saved$municipality, "preference_model")$estimation_data) &&
      identical(attr(saved$main, "preference_model")$normalization_file,
        attr(saved$municipality, "preference_model")$normalization_file))
  rm(d, sub, expanded, saved); invisible(gc())
}
fwrite(rbindlist(comparison), file.path(clean_dir, "validation_comparison.csv"))
save_checks("verification.csv")
message("All ", nrow(checks), " independent subgroup verification checks passed.")
