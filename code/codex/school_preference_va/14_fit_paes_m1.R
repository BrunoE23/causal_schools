# Add school-average regular PAES M1 and its type interactions to the joint ROL.
source("code/codex/school_preference_va/paes_common.R")
suppressPackageStartupMessages(library(survival))

message("Reading PAES extension input: ", round(file.info(paes_input_path)$size / 1e6, 1), " MB")
d <- as.data.table(readRDS(paes_input_path))
d <- add_metric_interactions(d)
setorder(d, ranking_id, rank_school)
d[, event := 1L]
check("extended model has five metrics, two types and ten interactions", length(extended_static_terms) == 17L &&
  length(metric_terms) == 5L && nrow(metric_interactions) == 10L)
check("one ranking per student", uniqueN(d$mrun) == uniqueN(d$ranking_id))
check("unique student-school pairs", !anyDuplicated(d[, .(ranking_id, rbd)]))
check("both distance definitions and extended regressors finite", all(vapply(d[,
  c(extended_static_terms, unname(distance_vars)), with = FALSE], function(z) all(is.finite(z)), logical(1))))
check("school types binary and mutually exclusive", all(d$mixed_hc_tp %in% 0:1) &&
  all(d$tp_only %in% 0:1) && all(d$mixed_hc_tp + d$tp_only <= 1L))
check("all retained lists have at least two distinct schools", all(d[, .N, by = ranking_id]$N >= 2L))

results <- summaries <- diagnostics <- slopes <- correlations <- list()
for (g in names(group_labels)) {
  x <- if (g == "pooled") copy(d) else copy(d[group == g])
  check(paste(g, "nonempty sample"), nrow(x) > 0L)
  ids <- unique(x$ranking_id)
  ranking_group <- match(x$ranking_id, ids)
  group_n <- tabulate(ranking_group)
  schools <- unique(x[, c("rbd", metric_terms), with = FALSE], by = "rbd")
  corr <- cor(as.matrix(schools[, ..metric_terms]))
  eig_c <- eigen(corr, symmetric = TRUE, only.values = TRUE)$values
  check(paste(g, "school metric correlations full rank"), min(eig_c) > 1e-10)
  correlations[[g]] <- data.table(group = g, term = metric_terms, vif = diag(solve(corr)), as.data.table(corr))

  for (variant_name in names(variant_terms)) for (model_name in names(distance_vars)) {
    key <- paste(g, variant_name, model_name, sep = "_")
    distance <- unname(distance_vars[model_name])
    terms <- c(variant_terms[[variant_name]], distance)
    message("Fitting ", key, ": ", length(ids), " rankings, ", nrow(x), " school pairs")
    fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "),
      "+ strata(ranking_id)"), env = .GlobalEnv)
    fitting_warnings <- character()
    elapsed <- system.time(fit <- withCallingHandlers(
      coxph(fml, data = x, cluster = mrun, ties = "breslow", singular.ok = FALSE,
        na.action = na.fail, model = FALSE, x = FALSE, y = TRUE,
        control = coxph.control(iter.max = 60L, eps = 1e-10)),
      warning = function(w) { fitting_warnings <<- c(fitting_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }))[["elapsed"]]
    check(paste(key, "no fitting warnings"), !length(fitting_warnings), paste(fitting_warnings, collapse = "; "))
    check(paste(key, "all requested coefficients retained"), identical(names(coef(fit)), terms))
    check(paste(key, "finite estimates and clustered covariance"), all(is.finite(coef(fit))) && all(is.finite(vcov(fit))))
    check(paste(key, "student-clustered covariance"), !is.null(fit$naive.var))
    check(paste(key, "no automatic row deletion"), fit$n == nrow(x) && is.null(fit$na.action))
    check(paste(key, "convergence within iteration limit"), fit$iter < 60L)
    check(paste(key, "likelihood improves on null"), fit$loglik[2L] >= fit$loglik[1L])

    score <- residuals(fit, type = "score")
    gradient <- colSums(score)
    check(paste(key, "small gradient per ranking"), max(abs(gradient)) / length(ids) < 1e-7)
    information <- solve(fit$naive.var)
    dimnames(information) <- list(terms, terms)
    eig_h <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
    check(paste(key, "positive definite likelihood information"), min(eig_h) > 0)
    X <- as.matrix(x[, ..terms])
    means <- rowsum(X, ranking_group, reorder = FALSE) / group_n
    X <- X - means[ranking_group, , drop = FALSE]
    eig_x <- eigen(crossprod(X), symmetric = TRUE, only.values = TRUE)$values
    rank_x <- sum(eig_x > max(eig_x) * 1e-10)
    check(paste(key, "within-ranking design full rank"), rank_x == length(terms))
    fwrite(data.table(term = terms, as.data.table(vcov(fit))), file.path(clean_dir, paste0("vcov_", key, ".csv")))
    fwrite(data.table(term = terms, as.data.table(information)), file.path(clean_dir, paste0("information_", key, ".csv")))
    fwrite(data.table(term = terms, score = gradient), file.path(clean_dir, paste0("score_", key, ".csv")))
    attr(fit, "preference_model") <- list(group = g, variant = variant_name, model = model_name,
      likelihood = "Exact conditional submitted-set Plackett-Luce", distance = distance, cluster = "mrun",
      estimation_data = paes_input_path, sample_filter = if (g == "pooled") "All rows" else paste0("group == ", g),
      metric_terms = terms[terms %in% metric_terms], controls = controls,
      interaction_terms = terms[terms %in% metric_interactions$term],
      va_normalization = file.path(clean_dir, "va_normalization.csv"),
      paes_normalization = file.path(clean_dir, "paes_m1_normalization.csv"),
      paes_source = "Admissions process 2025; 2024 graduates; MATE1_REG_ACTUAL mean among valid takers at graduation RBD",
      reference_school_type = "Non-TP: HC-only, artistic-only and other/no regular secondary in 2024",
      temporal_interpretation = "Later measured school performance, not necessarily observed by applicants in 2018-2020")
    fit$call$formula <- fml
    saveRDS(fit, paste0(paes_model_path(g, variant_name, model_name), ".rds"))
    slopes[[key]] <- metric_type_slopes(fit, g, variant_name, model_name)
    b <- coef(fit); se <- sqrt(diag(vcov(fit)))
    results[[key]] <- data.table(group = g, variant = variant_name, model = model_name,
      term = names(b), estimate = as.numeric(b), std_error = as.numeric(se), z = as.numeric(b / se),
      p_value = 2 * pnorm(-abs(as.numeric(b / se))), lower95 = as.numeric(b - qnorm(.975) * se),
      upper95 = as.numeric(b + qnorm(.975) * se), conditional_odds_ratio = exp(as.numeric(b)))
    summaries[[key]] <- data.table(group = g, variant = variant_name, model = model_name,
      applicants = uniqueN(x$mrun), rankings = length(ids), applicant_school_pairs = nrow(x), schools = uniqueN(x$rbd),
      informative_rank_decisions = nrow(x) - length(ids), log_likelihood = as.numeric(logLik(fit)),
      null_log_likelihood = fit$loglik[1L], iterations = fit$iter, seconds = elapsed, distance = distance)
    diagnostics[[key]] <- data.table(group = g, variant = variant_name, model = model_name,
      within_ranking_rank = rank_x, within_ranking_gram_condition = max(eig_x) / min(eig_x),
      information_min_eigenvalue = min(eig_h), information_condition = max(eig_h) / min(eig_h),
      max_abs_score = max(abs(gradient)), max_abs_score_per_ranking = max(abs(gradient)) / length(ids))
    message(key, " completed in ", round(elapsed, 1), " seconds")
    rm(fit, score, X, means); invisible(gc())
  }
  rm(x); invisible(gc())
}
summary <- rbindlist(summaries)
for (g in names(group_labels)) for (model_name in names(distance_vars)) {
  pair <- summary[group == g & model == model_name]
  check(paste(g, model_name, "baseline and extended samples identical"), nrow(pair) == 2L &&
    all(vapply(pair[, .(applicants, rankings, applicant_school_pairs, schools, null_log_likelihood)],
      function(z) uniqueN(z) == 1L, logical(1))))
  check(paste(g, model_name, "adding M1 weakly increases likelihood"),
    pair[variant == "extended", log_likelihood] >= pair[variant == "matched_baseline", log_likelihood] - 1e-7)
}
fwrite(rbindlist(results), file.path(clean_dir, "coefficients.csv"))
fwrite(summary, file.path(clean_dir, "model_summary.csv"))
fwrite(rbindlist(diagnostics), file.path(clean_dir, "model_diagnostics.csv"))
fwrite(rbindlist(slopes), file.path(clean_dir, "type_specific_slopes.csv"))
fwrite(rbindlist(correlations), file.path(clean_dir, "metric_correlations.csv"))
save_checks("fit_verification.csv")
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
print(summary)
print(rbindlist(slopes)[group == "pooled" & variant == "extended" & model == "main",
  .(school_type, label, estimate, std_error)])
