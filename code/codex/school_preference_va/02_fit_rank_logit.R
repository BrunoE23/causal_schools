# Exact submitted-set Plackett-Luce likelihood, represented as a stratified Cox model.
source("code/codex/school_preference_va/common.R")
suppressPackageStartupMessages(library(survival))
input <- file.path(clean_dir, "estimation_data.rds")
message("Reading minimal estimation data: ", round(file.info(input)$size / 1e6, 1), " MB on disk")
d <- as.data.table(readRDS(input))
d <- add_va_interactions(d)
setorder(d, ranking_id, rank_school)
d[, event := 1L]
check("informative rankings available", uniqueN(d$ranking_id) > 0L)
check("one ranking per paper pupil", uniqueN(d$mrun) == uniqueN(d$ranking_id))
check("unique student-school alternatives", !anyDuplicated(d[, .(ranking_id, rbd)]))
check("both distance definitions complete", all(is.finite(d$log1p_distance_main)) && all(is.finite(d$log1p_distance_comuna)))
check("all selected VA terms complete", all(is.finite(as.matrix(d[, ..va_terms]))))
check("requested model uses separate mixed and TP-only indicators", identical(controls, c("mixed_hc_tp", "tp_only")))
check("school-type indicators binary and mutually exclusive", all(d$mixed_hc_tp %in% 0:1) &&
  all(d$tp_only %in% 0:1) && all(d$mixed_hc_tp + d$tp_only <= 1L))

# One observation per unique destination school in this common estimation sample.
schools <- unique(d[, c("rbd", va_terms), with = FALSE], by = "rbd")
cor_va <- cor(as.matrix(schools[, ..va_terms]))
fwrite(data.table(term = rownames(cor_va), as.data.table(cor_va)), file.path(clean_dir, "va_correlations.csv"))
fwrite(data.table(term = va_terms, vif = diag(solve(cor_va))), file.path(clean_dir, "va_vif.csv"))
eig <- eigen(cor_va, symmetric = TRUE, only.values = TRUE)$values
check("selected school VA correlation matrix full rank", min(eig) > 1e-10)
fwrite(data.table(schools = nrow(schools), min_eigenvalue = min(eig), max_eigenvalue = max(eig),
  condition_number = max(eig) / min(eig)), file.path(clean_dir, "va_correlation_condition.csv"))

results <- summaries <- diagnostics <- slopes <- list()
ids <- unique(d$ranking_id)
group <- match(d$ranking_id, ids)
group_n <- tabulate(group)
for (name in names(distance_vars)) {
  dist <- distance_vars[[name]]
  terms <- c(model_static_terms, dist)
  message("Fitting ", name, ": ", nrow(d), " alternatives in ", length(ids), " rankings")
  fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "), "+ strata(ranking_id)"), env = .GlobalEnv)
  warnings <- character()
  elapsed <- system.time(fit <- withCallingHandlers(
    coxph(fml, data = d, cluster = mrun, ties = "breslow", singular.ok = FALSE,
      na.action = na.fail, model = FALSE, x = FALSE, y = TRUE,
      control = coxph.control(iter.max = 60L, eps = 1e-10)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }))[["elapsed"]]
  check(paste(name, "no fitting warnings"), !length(warnings), paste(warnings, collapse = "; "))
  check(paste(name, "all requested coefficients retained"), identical(names(coef(fit)), terms))
  check(paste(name, "finite coefficient and clustered covariance"), all(is.finite(coef(fit))) && all(is.finite(vcov(fit))))
  check(paste(name, "student clustered covariance supplied"), !is.null(fit$naive.var))
  check(paste(name, "no automatic row deletion"), fit$n == nrow(d) && is.null(fit$na.action))
  check(paste(name, "optimizer within iteration limit"), fit$iter < 60L)
  check(paste(name, "likelihood improved over zero"), fit$loglik[2L] >= fit$loglik[1L])

  # Cox score contributions aggregate to the score of the specified ranking likelihood.
  score <- residuals(fit, type = "score")
  gradient <- colSums(score)
  check(paste(name, "small likelihood gradient per ranking"), max(abs(gradient)) / length(ids) < 1e-7,
    format(max(abs(gradient)) / length(ids), scientific = TRUE))
  information <- solve(fit$naive.var)
  eig_h <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
  check(paste(name, "positive definite likelihood information"), min(eig_h) > 0)
  X <- as.matrix(d[, ..terms])
  means <- rowsum(X, group, reorder = FALSE) / group_n
  X <- X - means[group, , drop = FALSE]
  gram <- crossprod(X)
  eig_x <- eigen(gram, symmetric = TRUE, only.values = TRUE)$values
  rank_x <- sum(eig_x > max(eig_x) * 1e-10)
  check(paste(name, "within-ranking design has full requested rank"), rank_x == length(terms))
  fwrite(data.table(term = terms, score = gradient), file.path(clean_dir, paste0("score_", name, ".csv")))
  fwrite(data.table(term = terms, as.data.table(vcov(fit))), file.path(clean_dir, paste0("vcov_", name, ".csv")))
  fwrite(data.table(term = terms, as.data.table(information)), file.path(clean_dir, paste0("information_", name, ".csv")))
  attr(fit, "preference_model") <- list(model = name, distance = dist, likelihood = "conditional submitted-set Plackett-Luce",
    normalization = "equal-weight unique destination schools in common final sample", cluster = "mrun",
    estimation_data = input, normalization_file = file.path(clean_dir, "va_normalization.csv"),
    controls = controls, interaction_terms = interaction_terms,
    specification = "Four standardized EB VAs, mixed HC/TP, TP-only, eight type-by-VA interactions, logged distance",
    reference_school_type = "Non-TP: HC-only, artistic-only and other/no regular secondary in 2024")
  # Store the actual formula, not the mutable loop symbol fml, in the saved call.
  fit$call$formula <- fml
  saveRDS(fit, file.path(clean_dir, "models", paste0(name, ".rds")))
  slopes[[name]] <- type_specific_va_slopes(fit, name)
  b <- coef(fit)
  se <- sqrt(diag(vcov(fit)))
  results[[name]] <- data.table(model = name, term = names(b), estimate = as.numeric(b), std_error = se,
    z = as.numeric(b) / se, p_value = 2 * pnorm(-abs(as.numeric(b) / se)),
    lower95 = as.numeric(b) - qnorm(.975) * se, upper95 = as.numeric(b) + qnorm(.975) * se,
    conditional_odds_ratio = exp(as.numeric(b)))
  summaries[[name]] <- data.table(model = name, applicants = uniqueN(d$mrun), rankings = length(ids),
    applicant_school_pairs = nrow(d), schools = uniqueN(d$rbd), informative_rank_decisions = nrow(d) - length(ids),
    log_likelihood = as.numeric(logLik(fit)), null_log_likelihood = fit$loglik[1L], iterations = fit$iter,
    seconds = elapsed, distance = dist)
  diagnostics[[name]] <- data.table(model = name, within_ranking_rank = rank_x,
    within_ranking_gram_condition = max(eig_x) / min(eig_x), information_min_eigenvalue = min(eig_h),
    information_condition = max(eig_h) / min(eig_h), max_abs_score = max(abs(gradient)),
    max_abs_score_per_ranking = max(abs(gradient)) / length(ids))
  message(name, " completed in ", round(elapsed, 1), " seconds")
  rm(fit, score, X, means, gram); invisible(gc())
}
fwrite(rbindlist(results), file.path(clean_dir, "coefficients.csv"))
fwrite(rbindlist(summaries), file.path(clean_dir, "model_summary.csv"))
fwrite(rbindlist(diagnostics), file.path(clean_dir, "model_diagnostics.csv"))
fwrite(rbindlist(slopes), file.path(clean_dir, "type_specific_va_slopes.csv"))
save_checks("estimation_verification.csv")
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "session_info.txt"))
print(rbindlist(results))
print(rbindlist(summaries))
