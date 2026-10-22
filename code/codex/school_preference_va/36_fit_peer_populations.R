# D plus three alternative peer populations, on the user-approved common sample.
source("code/codex/school_preference_va/four_specifications_common.R")
suppressPackageStartupMessages(library(survival))
out <- file.path(base_clean_dir, "peer_populations")
dir.create(file.path(out, "models"), recursive = TRUE, showWarnings = FALSE)
peer_path <- file.path(root, "data/clean/applicant_composition_trends/group_averages/school_group_averages_2018_2020.csv")
input <- file.path(out, "top100_complete_rankings.rds")
protected <- c(input, peer_path, four_spec_model_path("D", "main"), four_spec_input_path)
hashes <- data.table(path = protected, md5_before = unname(tools::md5sum(protected)))
d <- as.data.table(readRDS(input)); setorder(d, ranking_id, rank_school)
check("Approved full-list sample is intact", nrow(d) == 36276L && uniqueN(d$ranking_id) == 13037L && uniqueN(d$rbd) == 99L &&
  !anyDuplicated(d[, .(ranking_id, rbd)]) && all(d[, identical(as.integer(rank_school), seq_len(.N)), by = ranking_id]$V1))
peer <- fread(peer_path)
metrics <- c("parents_education", "simce_baseline", "female", "income_decile_imputed")
peer_labels <- c(parents_education = "Peer parents' education", simce_baseline = "Peer baseline SIMCE",
  female = "Peer female share", income_decile_imputed = "Peer income decile")
group_map <- c(applicants = "weighted_applicants", offers = "first_round_offered", attendees = "actual_attendees")
ref <- peer[group == "actual_attendees" & rbd %in% d$rbd]
normalization <- rbindlist(lapply(metrics, function(v) data.table(metric = v,
  reference_mean = mean(ref[[paste0("mean_equal_cohort_", v)]]),
  reference_sd = sd(ref[[paste0("mean_equal_cohort_", v)]]), schools = nrow(ref))))
check("Four common peer normalization units exist", nrow(ref) == 99L && all(is.finite(normalization$reference_sd) & normalization$reference_sd > 0))
for (g in names(group_map)) {
  lookup <- peer[group == group_map[[g]], c("rbd", paste0("mean_equal_cohort_", metrics)), with = FALSE]
  check(paste(g, "peer join is unique"), !anyDuplicated(lookup$rbd))
  for (v in metrics) {
    n <- normalization[metric == v]
    lookup[, (paste0("peer_", g, "_", v)) := (get(paste0("mean_equal_cohort_", v)) - n$reference_mean) / n$reference_sd]
  }
  lookup <- lookup[, c("rbd", paste0("peer_", g, "_", metrics)), with = FALSE]
  d <- merge(d, lookup, by = "rbd", all.x = TRUE, sort = FALSE)
}
setorder(d, ranking_id, rank_school); d[, event := 1L]
base_terms <- c("log1p_distance_main", specification_terms$D)
constant <- base_terms[vapply(base_terms, function(v) uniqueN(d[[v]]) < 2L, logical(1))]
check("Only absent fee indicators are omitted from restricted D", all(constant %in% active_fee_terms))
base_terms <- setdiff(base_terms, constant)
peer_terms <- setNames(lapply(names(group_map), function(g) paste0("peer_", g, "_", metrics)), names(group_map))
check("Peer composition is complete on every original alternative", nrow(d) == 36276L && all(vapply(d[, unlist(peer_terms), with = FALSE], function(v) all(is.finite(v)), logical(1))))
fwrite(normalization, file.path(out, "peer_normalization.csv"))
fwrite(data.table(term = constant, reason = "Identically zero in the approved top-100 complete-list sample"), file.path(out, "omitted_constant_fees.csv"))
fwrite(peer[rbd %in% d$rbd], file.path(out, "source_peer_characteristics_used.csv"))
saveRDS(d, file.path(out, "estimation_data.rds"))

fits <- list(); coef_rows <- list(); summary_rows <- list(); diagnostic_rows <- list()
fit_one <- function(key, terms, auxiliary = FALSE) {
  message("Fitting ", key, ": ", length(terms), " coefficients, 13037 complete rankings")
  fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "), "+ strata(ranking_id)"), env = .GlobalEnv)
  warnings <- character()
  fit <- withCallingHandlers(coxph(fml, data = d, cluster = mrun, ties = "breslow", singular.ok = FALSE,
    na.action = na.fail, model = FALSE, x = FALSE, y = TRUE, control = coxph.control(eps = 1e-10, iter.max = 60L)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  b <- coef(fit); V <- vcov(fit)
  check(paste(key, "converged with every regressor and observation"), !length(warnings) && fit$iter < 60L &&
    identical(names(b), terms) && fit$n == nrow(d) && all(is.finite(b)) && all(is.finite(V)) && all(diag(V) > 0))
  check(paste(key, "matches the exact submitted-ranking likelihood"), abs(paes_pl_loglik(d, b, terms) - as.numeric(logLik(fit))) < 1e-6)
  score <- residuals(fit, type = "score")
  check(paste(key, "score equations hold"), max(abs(colSums(score))) / 13037 < 1e-7)
  H <- solve(fit$naive.var); ev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  check(paste(key, "information is identified"), min(ev) > 0)
  # Reconstruct the student-clustered sandwich independently from Cox scores.
  clustered_score <- rowsum(score, d$mrun, reorder = FALSE)
  robust <- fit$naive.var %*% crossprod(clustered_score) %*% fit$naive.var
  check(paste(key, "student-clustered covariance reproduces"), max(abs(robust - V)) < 1e-7)
  fit$call$formula <- fml
  saveRDS(fit, file.path(out, "models", paste0(key, ".rds")))
  fwrite(data.table(term = names(b), as.data.table(V)), file.path(out, paste0("vcov_", key, ".csv")))
  se <- sqrt(diag(V))
  coef_rows[[key]] <<- data.table(model = key, auxiliary = auxiliary, term = names(b), estimate = as.numeric(b),
    std_error = se, p_value = 2 * pnorm(-abs(b / se)), lower95 = b - qnorm(.975) * se, upper95 = b + qnorm(.975) * se)
  summary_rows[[key]] <<- data.table(model = key, auxiliary = auxiliary, applicants = uniqueN(d$mrun),
    rankings = uniqueN(d$ranking_id), school_pairs = nrow(d), schools = uniqueN(d$rbd),
    log_likelihood = as.numeric(logLik(fit)), null_log_likelihood = fit$loglik[1L], iterations = fit$iter)
  diagnostic_rows[[key]] <<- data.table(model = key, information_condition = max(ev) / min(ev), max_abs_score = max(abs(colSums(score))))
  fits[[key]] <<- fit
  invisible(fit)
}
fit_one("D", base_terms)
for (g in names(group_map)) fit_one(paste0("D_", g), c(base_terms, peer_terms[[g]]))
# These three nested fits support unit-invariant comparisons of incremental
# ranking fit; only the four requested models appear in the coefficient table.
for (g in names(group_map)) fit_one(paste0("noVA_", g), c(setdiff(base_terms, va_terms), peer_terms[[g]]), auxiliary = TRUE)
block_tests <- rbindlist(lapply(names(group_map), function(g) {
  fit <- fits[[paste0("D_", g)]]; b <- coef(fit); V <- vcov(fit)
  wald <- function(terms) { z <- b[terms]; as.numeric(t(z) %*% solve(V[terms, terms, drop = FALSE], z)) }
  wp <- wald(peer_terms[[g]]); wv <- wald(va_terms)
  gain_peer <- as.numeric(logLik(fit) - logLik(fits$D))
  gain_va <- as.numeric(logLik(fit) - logLik(fits[[paste0("noVA_", g)]]))
  check(paste(g, "nested likelihoods improve"), gain_peer >= -1e-7 && gain_va >= -1e-7)
  data.table(group = g, peer_wald_chisq = wp, peer_wald_p = pchisq(wp, 4L, lower.tail = FALSE),
    va_wald_chisq = wv, va_wald_p = pchisq(wv, 4L, lower.tail = FALSE),
    loglik_gain_peers_given_VA = gain_peer, loglik_gain_VA_given_peers = gain_va,
    peer_to_VA_fit_gain_ratio = gain_peer / gain_va)
}))
fwrite(rbindlist(coef_rows), file.path(out, "coefficients.csv"))
fwrite(rbindlist(summary_rows), file.path(out, "model_summary.csv"))
fwrite(rbindlist(diagnostic_rows), file.path(out, "diagnostics.csv"))
fwrite(block_tests, file.path(out, "block_comparisons.csv"))
hashes[, md5_after := unname(tools::md5sum(path))]
check("Supplied peers, ranking sources and original D are unchanged", all(hashes$md5_before == hashes$md5_after))
fwrite(hashes, file.path(out, "source_preservation.csv"))
fwrite(checks, file.path(out, "verification.csv"))
writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
print(rbindlist(coef_rows)[auxiliary == FALSE & (term %in% va_terms | grepl("^peer_", term))])
print(block_tests)
