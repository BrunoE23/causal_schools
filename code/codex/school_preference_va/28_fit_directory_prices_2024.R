# Exact submitted-list Plackett-Luce with directory fee bands and clustered SEs.
source("code/codex/school_preference_va/directory_prices_common.R")
suppressPackageStartupMessages(library(survival))
d <- as.data.table(readRDS(price_input_path))
d <- add_metric_interactions(d); setorder(d, ranking_id, rank_school); d[, event := 1L]
original <- as.data.table(readRDS(prior_income_input_path))
setorder(original, ranking_id, rank_school)
same_sample <- identical(d[, .(ranking_id, rbd, rank_school)], original[, .(ranking_id, rbd, rank_school)])
check("Seven previous school attributes retain all interactions", length(metric_terms) == 7L && length(prior_income_static_terms) == 23L)
check("All requested attributes, prices and distances are finite", all(vapply(
  d[, c(prior_income_static_terms, fee_terms, unname(distance_vars)), with = FALSE],
  function(x) all(is.finite(x)), logical(1))))
check("Each included student has one ranking and unique alternatives", uniqueN(d$mrun) == uniqueN(d$ranking_id) && !anyDuplicated(d[, .(ranking_id, rbd)]))
rm(original); invisible(gc())
coefficients <- summaries <- diagnostics <- slopes <- supports <- joint_tests <- list()
for (g in names(group_labels)) {
  x <- if (g == "pooled") copy(d) else copy(d[group == g])
  ids <- unique(x$ranking_id); ranking_group <- match(x$ranking_id, ids); group_n <- tabulate(ranking_group)
  fee_sums <- rowsum(as.matrix(x[, ..fee_terms]), ranking_group, reorder = FALSE)
  within_ss <- colSums(as.matrix(x[, ..fee_terms])) - colSums(fee_sums^2 / group_n)
  support <- copy(fee_specs)
  support[, `:=`(group = g, n_schools = vapply(term, function(v) uniqueN(x[get(v) == 1L, rbd]), integer(1)),
    n_pairs = vapply(term, function(v) sum(x[[v]]), numeric(1)),
    n_rankings = vapply(term, function(v) uniqueN(x[get(v) == 1L, ranking_id]), integer(1)),
    within_ranking_ss = as.numeric(within_ss[term]))]
  support[, estimated := within_ranking_ss > 1e-9]
  support[, status := fifelse(estimated, "Estimated", "No within-ranking variation")]
  supports[[g]] <- support; active_fee_terms <- support[estimated == TRUE, term]
  rm(fee_sums)
  for (variant in names(variant_terms)) for (model in names(distance_vars)) {
    key <- paste(g, variant, model, sep = "_"); distance <- unname(distance_vars[model])
    terms <- c(prior_income_static_terms, if (variant == "extended") active_fee_terms else character(), distance)
    fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "), "+ strata(ranking_id)"), env = .GlobalEnv)
    fitting_warnings <- character()
    if (variant == "matched_baseline" && same_sample) {
      fit <- readRDS(prior_price_baseline_path(g, model)); elapsed <- 0
      fit_source <- "Reused identical prior seven-attribute fit"
    } else {
      message("Fitting ", key, ": ", length(ids), " rankings, ", length(terms), " coefficients")
      elapsed <- system.time(fit <- withCallingHandlers(coxph(fml, data = x, cluster = mrun, ties = "breslow", singular.ok = FALSE,
        na.action = na.fail, model = FALSE, x = FALSE, y = TRUE, control = coxph.control(iter.max = 60L, eps = 1e-10)),
        warning = function(w) { fitting_warnings <<- c(fitting_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }))["elapsed"]
      fit_source <- "New fit"
    }
    check(paste(key, "has no warnings and retains all identified terms"), !length(fitting_warnings) && identical(names(coef(fit)), terms))
    check(paste(key, "uses every row and converges"), fit$n == nrow(x) && is.null(fit$na.action) && fit$iter < 60L)
    b <- coef(fit); V <- vcov(fit); se <- sqrt(diag(V))
    check(paste(key, "finite estimates and clustered covariance"), all(is.finite(b)) && all(is.finite(V)) && all(se > 0))
    exact_ll <- paes_pl_loglik(x, b, terms)
    check(paste(key, "Cox likelihood equals submitted-list Plackett-Luce"), abs(exact_ll - as.numeric(logLik(fit))) < 1e-5)
    gradient <- colSums(residuals(fit, type = "score"))
    check(paste(key, "score per ranking is small"), max(abs(gradient)) / length(ids) < 1e-7)
    information <- solve(fit$naive.var); dimnames(information) <- list(terms, terms)
    eig_h <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
    X <- as.matrix(x[, ..terms]); means <- rowsum(X, ranking_group, reorder = FALSE) / group_n
    X <- X - means[ranking_group, , drop = FALSE]
    eig_x <- eigen(crossprod(X), symmetric = TRUE, only.values = TRUE)$values
    rank_x <- sum(eig_x > max(eig_x) * 1e-10)
    check(paste(key, "within-ranking design and information are full rank"), min(eig_h) > 0 && rank_x == length(terms))
    fwrite(data.table(term = terms, as.data.table(V)), file.path(clean_dir, paste0("vcov_", key, ".csv")))
    fwrite(data.table(term = terms, as.data.table(information)), file.path(clean_dir, paste0("information_", key, ".csv")))
    attr(fit, "directory_price_model") <- list(group = g, variant = variant, model = model, fee_terms = if (variant == "extended") active_fee_terms else character(),
      source_year = 2024L, free_reference = TRUE, cluster = "mrun", input = price_input_path, fit_source = fit_source)
    fit$call$formula <- fml
    saveRDS(fit, paste0(price_model_path(g, variant, model), ".rds"))
    coefficients[[key]] <- data.table(group = g, variant = variant, model = model, term = names(b), estimate = as.numeric(b), std_error = as.numeric(se),
      p_value = 2 * pnorm(-abs(as.numeric(b / se))), lower95 = as.numeric(b - qnorm(.975) * se), upper95 = as.numeric(b + qnorm(.975) * se), conditional_odds_ratio = exp(as.numeric(b)))
    slopes[[key]] <- metric_type_slopes(fit, g, variant, model)
    summaries[[key]] <- data.table(group = g, variant = variant, model = model, applicants = uniqueN(x$mrun), rankings = length(ids), applicant_school_pairs = nrow(x),
      schools = uniqueN(x$rbd), informative_rank_decisions = nrow(x) - length(ids), log_likelihood = as.numeric(logLik(fit)), null_log_likelihood = fit$loglik[1L],
      iterations = fit$iter, seconds = as.numeric(elapsed), distance = distance, fit_source = fit_source)
    diagnostics[[key]] <- data.table(group = g, variant = variant, model = model, within_ranking_rank = rank_x, coefficients = length(terms),
      within_ranking_gram_condition = max(eig_x) / min(eig_x), information_min_eigenvalue = min(eig_h), information_condition = max(eig_h) / min(eig_h), max_abs_score_per_ranking = max(abs(gradient)) / length(ids))
    if (variant == "extended") {
      W <- as.numeric(t(b[active_fee_terms]) %*% solve(V[active_fee_terms, active_fee_terms], b[active_fee_terms]))
      joint_tests[[key]] <- data.table(group = g, model = model, statistic = W, df = length(active_fee_terms), p_value = pchisq(W, df = length(active_fee_terms), lower.tail = FALSE))
    }
    message(key, " complete (", fit_source, ")")
    rm(fit, X, means, V); invisible(gc())
  }
  rm(x); invisible(gc())
}
summary <- rbindlist(summaries)
for (g in names(group_labels)) for (m in names(distance_vars)) {
  pair <- summary[group == g & model == m]
  check(paste(g, m, "before and after models use identical samples"), nrow(pair) == 2L && all(vapply(pair[, .(applicants, rankings, applicant_school_pairs, schools, null_log_likelihood)], function(z) uniqueN(z) == 1L, logical(1))))
  check(paste(g, m, "fee terms weakly improve likelihood"), pair[variant == "extended", log_likelihood] >= pair[variant == "matched_baseline", log_likelihood] - 1e-7)
}
fwrite(rbindlist(coefficients), file.path(clean_dir, "coefficients.csv")); fwrite(summary, file.path(clean_dir, "model_summary.csv"))
fwrite(rbindlist(slopes), file.path(clean_dir, "type_specific_slopes.csv")); fwrite(rbindlist(supports), file.path(clean_dir, "fee_support.csv"))
fwrite(rbindlist(diagnostics), file.path(clean_dir, "model_diagnostics.csv")); fwrite(rbindlist(joint_tests), file.path(clean_dir, "fee_joint_tests.csv"))
preservation <- fread(file.path(clean_dir, "prior_outputs_preservation.csv"))
check("All preceding data, models and reports remain unchanged", identical(unname(tools::md5sum(preservation$path)), preservation$md5_before))
save_checks("fit_verification.csv"); writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
print(summary); print(rbindlist(coefficients)[group == "pooled" & variant == "extended" & model == "main" & term %in% fee_terms])
