# Separate the existing joint preference model by continuation and low-income priority.
source("code/codex/school_preference_va/common.R")
suppressPackageStartupMessages(library(survival))

subgroup_dir <- file.path(clean_dir, "by_priority_continuation")
dir.create(file.path(subgroup_dir, "models"), recursive = TRUE, showWarnings = FALSE)
input <- file.path(clean_dir, "estimation_data.rds")
ranking_path <- file.path(clean_dir, "ranking_roster.csv")
priority_path <- file.path(root, "data/clean/empirical_bayes_school_va",
  "va_2017_2020__sae_2018_2020", "prioritario_sae_2018_2020.csv")
normalization_path <- file.path(clean_dir, "va_normalization.csv")
definitions <- data.table(
  group = c("ending_nonpriority", "ending_priority", "voluntary_nonpriority", "voluntary_priority"),
  label = c("School ending, non-prioritario", "School ending, prioritario",
    "Voluntary, non-prioritario", "Voluntary, prioritario"),
  definition = c("Origin has no SAE offering above grade 8; prioritario = 0",
    "Origin has no SAE offering above grade 8; prioritario = 1",
    "Origin offers grade 9; prioritario = 0", "Origin offers grade 9; prioritario = 1"))
fwrite(definitions, file.path(subgroup_dir, "group_definitions.csv"))
paths <- c(input, ranking_path, roster_path, priority_path, normalization_path)
manifest <- manifest_rows(paths)
manifest[, md5 := as.character(tools::md5sum(paths))]
fwrite(manifest, file.path(subgroup_dir, "source_manifest.csv"))

# All inputs are small existing caches. No raw application or school VA rebuild.
u <- as.data.table(readRDS(roster_path))[, .(mrun, sae_proceso, sample, continuation_group)]
p <- fread(priority_path, select = c("mrun", "sae_proceso", "prioritario"))
check("continuation flags unique by pupil and process", !anyDuplicated(u[, .(mrun, sae_proceso)]))
check("priority flags unique by pupil and process", !anyDuplicated(p[, .(mrun, sae_proceso)]))
u <- merge(u, p, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
check("all original applicants have binary SAE low-income priority", all(u$prioritario %in% 0:1))
check("continuation flags use existing paper categories", all(u$sample %chin%
  c("school_ending", "voluntary", "unresolved_other")))
check("school-ending category has correct source label", all(u[sample == "school_ending",
  continuation_group] == "Ends at 8 in SAE offerings"))
check("voluntary category has correct source label", all(u[sample == "voluntary",
  continuation_group] == "Offers grade 9"))
u[, group := fcase(sample == "school_ending" & prioritario == 0L, "ending_nonpriority",
  sample == "school_ending" & prioritario == 1L, "ending_priority",
  sample == "voluntary" & prioritario == 0L, "voluntary_nonpriority",
  sample == "voluntary" & prioritario == 1L, "voluntary_priority", default = "unresolved_other")]
r <- fread(ranking_path)
check("pooled ranking roster unique by pupil and process", !anyDuplicated(r[, .(mrun, sae_proceso)]))
r <- merge(r, u, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
check("group join preserves entire original roster", nrow(r) == nrow(u) && !anyNA(r$group))
r[, group_eligible := estimation_eligible & group %chin% definitions$group]
fwrite(r, file.path(subgroup_dir, "group_roster.csv"))

# Mutually exclusive accounting: unclassified continuation first, then singletons,
# then missing model inputs. pooled_eligible counts only classified usable lists.
flow <- r[, .(original_applicants = .N, pooled_eligible = sum(group_eligible),
  excluded_singleton = sum(group != "unresolved_other" & !at_least_two_schools),
  excluded_missing_regressor = sum(group != "unresolved_other" & at_least_two_schools & !estimation_eligible),
  excluded_unclassified = if (group[1L] == "unresolved_other") .N else 0L), by = .(group, sae_proceso)]
totals <- flow[, lapply(.SD, sum), by = group, .SDcols = c("original_applicants", "pooled_eligible",
  "excluded_singleton", "excluded_missing_regressor", "excluded_unclassified")]
totals[, sae_proceso := "All"]
flow[, sae_proceso := as.character(sae_proceso)]
flow <- rbindlist(list(flow, totals), use.names = TRUE)
flow[, label := definitions$label[match(group, definitions$group)]]
flow[group == "unresolved_other", label := "Unresolved continuation (excluded)"]
check("exclusive subgroup attrition accounts for every original applicant", all(flow$original_applicants ==
  flow$pooled_eligible + flow$excluded_singleton + flow$excluded_missing_regressor + flow$excluded_unclassified))
setcolorder(flow, c("group", "label", "sae_proceso", "original_applicants", "pooled_eligible",
  "excluded_singleton", "excluded_missing_regressor", "excluded_unclassified"))
fwrite(flow, file.path(subgroup_dir, "sample_flow.csv"))

d <- as.data.table(readRDS(input))
pooled_n <- nrow(d)
d <- merge(d, u, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
check("flags preserve every pooled school alternative", nrow(d) == pooled_n && !anyNA(d$group))
check("four categories plus unresolved partition pooled applicants", uniqueN(d$ranking_id) ==
  sum(r$estimation_eligible))
d <- d[group %chin% definitions$group]
setorder(d, ranking_id, rank_school)
check("subgroup rows equal sum of intact eligible school lists", nrow(d) == r[group_eligible == TRUE, sum(n_schools)] &&
  uniqueN(d$ranking_id) == r[group_eligible == TRUE, .N])
check("one subgroup per full ranking", all(d[, uniqueN(group), by = ranking_id]$V1 == 1L))
check("one complete ranking per student", uniqueN(d$mrun) == uniqueN(d$ranking_id))
check("four groups all available", setequal(unique(d$group), definitions$group))
check("both distance definitions and static regressors finite", all(vapply(
  d[, c(model_static_terms, unname(distance_vars)), with = FALSE], function(z) all(is.finite(z)), logical(1))))
check("requested model uses separate mixed and TP-only indicators", identical(controls, c("mixed_hc_tp", "tp_only")))
check("school-type indicators binary and mutually exclusive", all(d$mixed_hc_tp %in% 0:1) &&
  all(d$tp_only %in% 0:1) && all(d$mixed_hc_tp + d$tp_only <= 1L))
saveRDS(d, file.path(subgroup_dir, "estimation_data.rds"))
check("copy pooled normalization parameters unchanged", file.copy(normalization_path,
  file.path(subgroup_dir, "va_normalization.csv"), overwrite = TRUE))
check("subgroup normalization file byte-identical to pooled", unname(tools::md5sum(normalization_path)) ==
  unname(tools::md5sum(file.path(subgroup_dir, "va_normalization.csv"))))

results <- summaries <- diagnostics <- correlations <- list()
for (g in definitions$group) {
  x <- copy(d[group == g])
  x[, event := 1L]
  ids <- unique(x$ranking_id)
  rank_group <- match(x$ranking_id, ids)
  rank_n <- tabulate(rank_group)
  schools <- unique(x[, c("rbd", va_terms), with = FALSE], by = "rbd")
  cor_va <- cor(as.matrix(schools[, ..va_terms]))
  check(paste(g, "school VA correlation matrix full rank"), min(eigen(cor_va,
    symmetric = TRUE, only.values = TRUE)$values) > 1e-10)
  correlations[[g]] <- data.table(group = g, term = rownames(cor_va),
    vif = diag(solve(cor_va)), as.data.table(cor_va))

  for (name in names(distance_vars)) {
    key <- paste(g, name, sep = "_")
    dist <- distance_vars[[name]]
    terms <- c(model_static_terms, dist)
    message("Fitting ", key, ": ", length(ids), " rankings, ", nrow(x), " alternatives")
    fml <- as.formula(paste("Surv(rank_school, event) ~", paste(terms, collapse = " + "),
      "+ strata(ranking_id)"), env = .GlobalEnv)
    warnings <- character()
    elapsed <- system.time(fit <- withCallingHandlers(coxph(fml, data = x, cluster = mrun,
      ties = "breslow", singular.ok = FALSE, na.action = na.fail, model = FALSE, x = FALSE, y = TRUE,
      control = coxph.control(iter.max = 60L, eps = 1e-10)),
      warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }))[["elapsed"]]
    check(paste(key, "no fitting warnings"), !length(warnings), paste(warnings, collapse = "; "))
    check(paste(key, "all requested coefficients retained"), identical(names(coef(fit)), terms))
    check(paste(key, "finite coefficients and student-cluster covariance"),
      all(is.finite(coef(fit))) && all(is.finite(vcov(fit))) && all(diag(vcov(fit)) > 0) && !is.null(fit$naive.var))
    check(paste(key, "no row deletion"), fit$n == nrow(x) && is.null(fit$na.action))
    check(paste(key, "optimizer converged within limit"), fit$iter < 60L)
    check(paste(key, "likelihood improved"), fit$loglik[2L] >= fit$loglik[1L])
    score <- residuals(fit, type = "score")
    gradient <- colSums(score)
    max_score <- max(abs(gradient)) / length(ids)
    check(paste(key, "small score per ranking"), max_score < 1e-7, format(max_score, scientific = TRUE))
    information <- solve(fit$naive.var)
    eig_h <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
    check(paste(key, "positive definite likelihood information"), min(eig_h) > 0)
    X <- as.matrix(x[, ..terms])
    means <- rowsum(X, rank_group, reorder = FALSE) / rank_n
    X <- X - means[rank_group, , drop = FALSE]
    eig_x <- eigen(crossprod(X), symmetric = TRUE, only.values = TRUE)$values
    rank_x <- sum(eig_x > max(eig_x) * 1e-10)
    check(paste(key, "all terms identified within rankings"), rank_x == length(terms))
    fwrite(data.table(term = terms, score = gradient), file.path(subgroup_dir, paste0("score_", key, ".csv")))
    fwrite(data.table(term = terms, as.data.table(vcov(fit))), file.path(subgroup_dir, paste0("vcov_", key, ".csv")))
    fwrite(data.table(term = terms, as.data.table(information)), file.path(subgroup_dir, paste0("information_", key, ".csv")))
    attr(fit, "preference_model") <- list(model = name, group = g, distance = dist,
      likelihood = "conditional submitted-set Plackett-Luce", cluster = "mrun",
      normalization = "unchanged pooled equal-weight school mean/SD; not restandardized by group",
      normalization_file = file.path(subgroup_dir, "va_normalization.csv"), controls = controls,
      estimation_data = file.path(subgroup_dir, "estimation_data.rds"),
      group_definitions = file.path(subgroup_dir, "group_definitions.csv"),
      specification = "Four standardized EB VAs, mixed HC/TP, TP-only, logged distance",
      reference_school_type = "Non-TP: HC-only, artistic-only and other/no regular secondary in 2024")
    fit$call$formula <- fml
    saveRDS(fit, file.path(subgroup_dir, "models", paste0(key, ".rds")))
    b <- as.numeric(coef(fit)); se <- sqrt(diag(vcov(fit)))
    results[[key]] <- data.table(group = g, model = name, term = terms, estimate = b, std_error = as.numeric(se),
      z = b / se, p_value = 2 * pnorm(-abs(b / se)), lower95 = b - qnorm(.975) * se,
      upper95 = b + qnorm(.975) * se, conditional_odds_ratio = exp(b))
    summaries[[key]] <- data.table(group = g, model = name, applicants = uniqueN(x$mrun), rankings = length(ids),
      applicant_school_pairs = nrow(x), schools = uniqueN(x$rbd), informative_rank_decisions = nrow(x) - length(ids),
      log_likelihood = as.numeric(logLik(fit)), null_log_likelihood = fit$loglik[1L], iterations = fit$iter,
      seconds = elapsed, distance = dist)
    diagnostics[[key]] <- data.table(group = g, model = name, within_ranking_rank = rank_x,
      within_ranking_gram_condition = max(eig_x) / min(eig_x), information_min_eigenvalue = min(eig_h),
      information_condition = max(eig_h) / min(eig_h), max_abs_score = max(abs(gradient)),
      max_abs_score_per_ranking = max_score)
    message(key, " completed in ", round(elapsed, 1), " seconds")
    rm(fit, score, X, means); invisible(gc())
  }
}
fwrite(rbindlist(results), file.path(subgroup_dir, "coefficients.csv"))
fwrite(rbindlist(summaries), file.path(subgroup_dir, "model_summary.csv"))
fwrite(rbindlist(diagnostics), file.path(subgroup_dir, "model_diagnostics.csv"))
fwrite(rbindlist(correlations), file.path(subgroup_dir, "va_correlations.csv"))
fwrite(checks, file.path(subgroup_dir, "fit_verification.csv"))
writeLines(capture.output(sessionInfo()), file.path(subgroup_dir, "session_info.txt"))
print(rbindlist(results)[model == "main", .(group, term, estimate, std_error)])
print(rbindlist(summaries))
