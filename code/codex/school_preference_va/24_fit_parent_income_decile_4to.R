# Add existing grade-4-SIMCE school income decile to parent-education model.
source("code/codex/school_preference_va/parent_income_common.R")
suppressPackageStartupMessages(library(survival))
d <- as.data.table(readRDS(parent_income_input_path))
d <- add_metric_interactions(d)
setorder(d, ranking_id, rank_school)
d[, event := 1L]
check("Extended model has seven metrics, two type indicators and fourteen interactions",
  length(metric_terms) == 7L && nrow(metric_interactions) == 14L && length(extended_static_terms) == 23L)
check("Matched baseline retains the parent-education specification",
  identical(variant_terms$matched_baseline, base_parent_education_terms) && length(base_parent_education_terms) == 20L)
check("One ranking per student and unique alternatives", uniqueN(d$mrun) == uniqueN(d$ranking_id) &&
  !anyDuplicated(d[, .(ranking_id, rbd)]))
check("All requested regressors and distances finite", all(vapply(d[, c(extended_static_terms,
  unname(distance_vars)), with = FALSE], function(z) all(is.finite(z)), logical(1))))
check("Every included list preserves at least two ranked schools", all(d[, .N, by = ranking_id]$N >= 2L))

coefficients <- summaries <- diagnostics <- slopes <- correlations <- list()
for (g in names(group_labels)) {
  x <- if (g == "pooled") copy(d) else copy(d[group == g]
  )
  check(paste(g, "sample is nonempty"), nrow(x) > 0L)
  ids <- unique(x$ranking_id)
  ranking_group <- match(x$ranking_id, ids)
  group_n <- tabulate(ranking_group)
  schools <- unique(x[, c("rbd", metric_terms), with = FALSE], by = "rbd")
  corr <- cor(as.matrix(schools[, ..metric_terms]))
  eig_c <- eigen(corr, symmetric = TRUE, only.values = TRUE)$values
  check(paste(g, "school attribute correlations have full rank"), min(eig_c) > 1e-10)
  correlations[[g]] <- data.table(group = g, term = metric_terms, vif = diag(solve(corr)), as.data.table(corr))

  for (variant in names(variant_terms)) for (model in names(distance_vars)) {
    key <- paste(g, variant, model, sep = "_")
    distance <- unname(distance_vars[model])
    terms <- c(variant_terms[[variant]], distance)
    message("Fitting ", key, ": ", length(ids), " rankings, ", nrow(x), " school pairs")
    fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "),
      "+ strata(ranking_id)"), env = .GlobalEnv)
    fitting_warnings <- character()
    elapsed <- system.time(fit <- withCallingHandlers(
      coxph(fml, data = x, cluster = mrun, ties = "breslow", singular.ok = FALSE,
        na.action = na.fail, model = FALSE, x = FALSE, y = TRUE,
        control = coxph.control(iter.max = 60L, eps = 1e-10)),
      warning = function(w) { fitting_warnings <<- c(fitting_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }))["elapsed"]
    check(paste(key, "fit has no warnings"), !length(fitting_warnings), paste(fitting_warnings, collapse = "; "))
    check(paste(key, "all requested coefficients retained"), identical(names(coef(fit)), terms))
    check(paste(key, "estimates and clustered covariance are finite"), all(is.finite(coef(fit))) && all(is.finite(vcov(fit))))
    check(paste(key, "no rows automatically deleted"), fit$n == nrow(x) && is.null(fit$na.action))
    check(paste(key, "converged within iteration limit"), fit$iter < 60L)
    check(paste(key, "likelihood improves on null"), fit$loglik[2L] >= fit$loglik[1L])
    exact_ll <- paes_pl_loglik(x, coef(fit), terms)
    check(paste(key, "Cox partial likelihood equals submitted-list Plackett-Luce likelihood"),
      abs(exact_ll - as.numeric(logLik(fit))) < 1e-5)
    score <- residuals(fit, type = "score")
    gradient <- colSums(score)
    check(paste(key, "score gradient is small per ranking"), max(abs(gradient)) / length(ids) < 1e-7)
    information <- solve(fit$naive.var)
    dimnames(information) <- list(terms, terms)
    eig_h <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
    check(paste(key, "likelihood information is positive definite"), min(eig_h) > 0)
    X <- as.matrix(x[, ..terms])
    means <- rowsum(X, ranking_group, reorder = FALSE) / group_n
    X <- X - means[ranking_group, , drop = FALSE]
    eig_x <- eigen(crossprod(X), symmetric = TRUE, only.values = TRUE)$values
    rank_x <- sum(eig_x > max(eig_x) * 1e-10)
    check(paste(key, "within-ranking design is full rank"), rank_x == length(terms))
    fwrite(data.table(term = terms, as.data.table(vcov(fit))), file.path(clean_dir, paste0("vcov_", key, ".csv")))
    fwrite(data.table(term = terms, as.data.table(information)), file.path(clean_dir, paste0("information_", key, ".csv")))
    fwrite(data.table(term = terms, score = gradient), file.path(clean_dir, paste0("score_", key, ".csv")))
    attr(fit, "preference_model") <- list(group = g, variant = variant, model = model,
      likelihood = "Exact conditional submitted-set Plackett-Luce", distance = distance,
      cluster = "mrun", estimation_data = parent_income_input_path,
      metric_terms = terms[terms %in% metric_terms],
      interaction_terms = terms[terms %in% metric_interactions$term],
      parent_survey = "Existing grade-4-SIMCE household income decile aggregated to school mean among 2017-2020 grade-8 attendees",
      temporal_interpretation = "School composition is retrospective for 2018-2020 applicants")
    fit$call$formula <- fml
    saveRDS(fit, paste0(parent_income_model_path(g, variant, model), ".rds"))
    slopes[[key]] <- metric_type_slopes(fit, g, variant, model)
    b <- coef(fit); se <- sqrt(diag(vcov(fit)))
    coefficients[[key]] <- data.table(group = g, variant = variant, model = model,
      term = names(b), estimate = as.numeric(b), std_error = as.numeric(se), z = as.numeric(b / se),
      p_value = 2 * pnorm(-abs(as.numeric(b / se))), lower95 = as.numeric(b - qnorm(.975) * se),
      upper95 = as.numeric(b + qnorm(.975) * se), conditional_odds_ratio = exp(as.numeric(b)))
    summaries[[key]] <- data.table(group = g, variant = variant, model = model,
      applicants = uniqueN(x$mrun), rankings = length(ids), applicant_school_pairs = nrow(x),
      schools = uniqueN(x$rbd), informative_rank_decisions = nrow(x) - length(ids),
      log_likelihood = as.numeric(logLik(fit)), null_log_likelihood = fit$loglik[1L],
      iterations = fit$iter, seconds = as.numeric(elapsed), distance = distance)
    diagnostics[[key]] <- data.table(group = g, variant = variant, model = model,
      within_ranking_rank = rank_x, within_ranking_gram_condition = max(eig_x) / min(eig_x),
      information_min_eigenvalue = min(eig_h), information_condition = max(eig_h) / min(eig_h),
      max_abs_score = max(abs(gradient)), max_abs_score_per_ranking = max(abs(gradient)) / length(ids))
    message(key, " completed in ", round(as.numeric(elapsed), 1), " seconds")
    rm(fit, score, X, means); invisible(gc())
  }
  rm(x); invisible(gc())
}
summary <- rbindlist(summaries)
for (g in names(group_labels)) for (model_name in names(distance_vars)) {
  pair <- summary[group == g & model == model_name]
  check(paste(g, model_name, "matched baseline and extended samples are identical"), nrow(pair) == 2L &&
    all(vapply(pair[, .(applicants, rankings, applicant_school_pairs, schools, null_log_likelihood)],
      function(z) uniqueN(z) == 1L, logical(1))))
  check(paste(g, model_name, "adding income decile weakly increases likelihood"),
    pair[variant == "extended", log_likelihood] >= pair[variant == "matched_baseline", log_likelihood] - 1e-7)
}
fwrite(rbindlist(coefficients), file.path(clean_dir, "coefficients.csv"))
fwrite(summary, file.path(clean_dir, "model_summary.csv"))
fwrite(rbindlist(diagnostics), file.path(clean_dir, "model_diagnostics.csv"))
fwrite(rbindlist(slopes), file.path(clean_dir, "type_specific_slopes.csv"))
fwrite(rbindlist(correlations), file.path(clean_dir, "metric_correlations.csv"))
save_checks("fit_verification.csv")
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
print(summary)
print(rbindlist(slopes)[group == "pooled" & variant == "extended" & model == "main" & suffix == "parent_income_decile_4to",
  .(school_type, estimate, std_error, p_value)])
