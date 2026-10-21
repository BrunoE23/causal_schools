# Fit A:distance+fees, B:A+observables, C:A+VA, D:B+VA on identical lists.
source("code/codex/school_preference_va/four_specifications_common.R")
suppressPackageStartupMessages(library(survival))
source_paths <- c(four_spec_input_path,
  file.path(source_price_dir, c("coefficients.csv", "model_summary.csv", "school_prices.csv", "fee_specs.csv")),
  list.files(file.path(source_price_dir, "models"), pattern = "[.]rds$", full.names = TRUE))
check("Existing source files are present and distinct", all(file.exists(source_paths)) && !anyDuplicated(source_paths))
fwrite(data.table(path = source_paths, md5_before = unname(tools::md5sum(source_paths))), file.path(clean_dir, "source_preservation.csv"))
fwrite(data.table(path = four_spec_input_path, bytes = file.info(four_spec_input_path)$size,
  md5 = unname(tools::md5sum(four_spec_input_path))), file.path(clean_dir, "source_manifest.csv"))
raw <- as.data.table(readRDS(four_spec_input_path))
needed <- unique(c("ranking_id", "mrun", "rbd", "rank_school", active_fee_terms, observable_terms, va_terms, unname(distance_vars)))
d <- copy(raw[, ..needed]); rm(raw); invisible(gc())
setorder(d, ranking_id, rank_school); d[, event := 1L]
check("Current common sample retains 273184 rankings and 969377 school pairs", uniqueN(d$ranking_id) == 273184L && nrow(d) == 969377L && uniqueN(d$rbd) == 2481L)
check("One complete unique ranking per student is preserved", uniqueN(d$mrun) == uniqueN(d$ranking_id) && !anyDuplicated(d[, .(ranking_id, rbd)]) &&
  all(d[, .(valid = .N >= 2L && identical(as.integer(rank_school), seq_len(.N))), by = ranking_id]$valid))
check("Every required regressor is finite on the common sample", all(vapply(
  d[, setdiff(needed, c("ranking_id", "mrun")), with = FALSE], function(x) all(is.finite(x)), logical(1))))
check("Specifications contain precisely the requested additive terms", identical(specification_terms$A, active_fee_terms) &&
  identical(specification_terms$B, c(active_fee_terms, observable_terms)) && identical(specification_terms$C, c(active_fee_terms, va_terms)) &&
  identical(specification_terms$D, c(active_fee_terms, observable_terms, va_terms)))
ids <- unique(d$ranking_id); ranking_group <- match(d$ranking_id, ids); group_n <- tabulate(ranking_group)
coefficients <- summaries <- diagnostics <- list()
for (model_name in names(distance_vars)) for (spec_name in names(specification_terms)) {
  terms <- c(unname(distance_vars[model_name]), specification_terms[[spec_name]])
  key <- paste(spec_name, model_name, sep = "_")
  message("Fitting ", key, ": ", length(terms), " coefficients, ", length(ids), " full rankings")
  fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "), "+ strata(ranking_id)"), env = .GlobalEnv)
  fitting_warnings <- character()
  elapsed <- system.time(fit <- withCallingHandlers(coxph(fml, data = d, cluster = mrun, ties = "breslow", singular.ok = FALSE,
    na.action = na.fail, model = FALSE, x = FALSE, y = TRUE, control = coxph.control(iter.max = 60L, eps = 1e-10)),
    warning = function(w) { fitting_warnings <<- c(fitting_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }))["elapsed"]
  check(paste(key, "converges without warnings and retains every term"), !length(fitting_warnings) && fit$iter < 60L && identical(names(coef(fit)), terms))
  check(paste(key, "uses the entire common sample"), fit$n == nrow(d) && is.null(fit$na.action))
  b <- coef(fit); V <- vcov(fit); se <- sqrt(diag(V))
  check(paste(key, "estimates and clustered covariance are finite"), all(is.finite(b)) && all(is.finite(V)) && all(se > 0))
  check(paste(key, "exact submitted-list likelihood equals Cox likelihood"), abs(paes_pl_loglik(d, b, terms) - as.numeric(logLik(fit))) < 1e-5)
  gradient <- colSums(residuals(fit, type = "score"))
  check(paste(key, "score per ranking is small"), max(abs(gradient)) / length(ids) < 1e-7)
  information <- solve(fit$naive.var); dimnames(information) <- list(terms, terms)
  eig_h <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
  X <- as.matrix(d[, ..terms]); means <- rowsum(X, ranking_group, reorder = FALSE) / group_n
  X <- X - means[ranking_group, , drop = FALSE]
  eig_x <- eigen(crossprod(X), symmetric = TRUE, only.values = TRUE)$values
  rank_x <- sum(eig_x > max(eig_x) * 1e-10)
  check(paste(key, "within-ranking information has full rank"), min(eig_h) > 0 && rank_x == length(terms))
  attr(fit, "four_specification_model") <- list(specification = spec_name, model = model_name, cluster = "mrun", input = four_spec_input_path,
    fee_reference = "Free for each fee; one shared unknown-fee indicator", normalization = "Previous pooled school normalizations unchanged")
  fit$call$formula <- fml
  saveRDS(fit, four_spec_model_path(spec_name, model_name))
  fwrite(data.table(term = terms, as.data.table(V)), file.path(clean_dir, paste0("vcov_", key, ".csv")))
  coefficients[[key]] <- data.table(specification = spec_name, model = model_name, term = terms, estimate = as.numeric(b), std_error = as.numeric(se),
    p_value = 2 * pnorm(-abs(as.numeric(b / se))), lower95 = as.numeric(b - qnorm(.975) * se), upper95 = as.numeric(b + qnorm(.975) * se), conditional_odds_ratio = exp(as.numeric(b)))
  summaries[[key]] <- data.table(specification = spec_name, model = model_name, applicants = uniqueN(d$mrun), rankings = length(ids), applicant_school_pairs = nrow(d),
    schools = uniqueN(d$rbd), informative_rank_decisions = nrow(d) - length(ids), log_likelihood = as.numeric(logLik(fit)), null_log_likelihood = fit$loglik[1L],
    iterations = fit$iter, seconds = as.numeric(elapsed), distance = unname(distance_vars[model_name]), parameters = length(terms))
  diagnostics[[key]] <- data.table(specification = spec_name, model = model_name, within_ranking_rank = rank_x, information_min_eigenvalue = min(eig_h),
    information_condition = max(eig_h) / min(eig_h), within_ranking_gram_condition = max(eig_x) / min(eig_x), max_abs_score_per_ranking = max(abs(gradient)))
  message(key, " completed in ", round(as.numeric(elapsed), 1), " seconds")
  rm(fit, X, means, V); invisible(gc())
}
summary <- rbindlist(summaries)
for (m in names(distance_vars)) {
  s <- summary[model == m]
  check(paste(m, "all four columns use identical samples and null likelihood"), all(vapply(s[, .(applicants, rankings, applicant_school_pairs, schools, null_log_likelihood)], function(x) uniqueN(x) == 1L, logical(1))))
  ll <- setNames(s$log_likelihood, s$specification)
  check(paste(m, "nested specification likelihoods weakly increase"), ll["B"] >= ll["A"] - 1e-7 && ll["C"] >= ll["A"] - 1e-7 && ll["D"] >= max(ll[c("B", "C")]) - 1e-7)
}
preservation <- fread(file.path(clean_dir, "source_preservation.csv"))
check("All existing data and previous fitted models remain unchanged", identical(unname(tools::md5sum(preservation$path)), preservation$md5_before))
fwrite(rbindlist(coefficients), file.path(clean_dir, "coefficients.csv")); fwrite(summary, file.path(clean_dir, "model_summary.csv"))
fwrite(rbindlist(diagnostics), file.path(clean_dir, "diagnostics.csv")); save_checks("fit_verification.csv")
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
print(summary)
