# Independently verify the directory fee join, identified fee bands and saved models.
source("code/codex/school_preference_va/directory_prices_common.R")
suppressPackageStartupMessages(library(survival))
same_num <- function(a, b, tol = 1e-9) {
  if (length(a) != length(b)) return(FALSE)
  a <- as.numeric(a); b <- as.numeric(b)
  all((is.na(a) & is.na(b)) | (is.finite(a) & is.finite(b) & abs(a - b) <= tol))
}
same_values <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) same_num(a, b, tol = 0) else identical(a, b)
}
raw_map <- c("SIN INFORMACION" = 0L, "GRATUITO" = 1L, "$1.000 A $10.000" = 2L,
  "$10.001 A $25.000" = 3L, "$25.001 A $50.000" = 4L, "$50.001 A $100.000" = 5L, "MAS DE $100.000" = 6L)
check("Fee mapping agrees with the documented source labels", identical(fee_map, raw_map))
manifest <- fread(file.path(clean_dir, "source_manifest.csv"))
check("All source inputs retain their recorded hashes", all(file.exists(manifest$path)) &&
  identical(unname(tools::md5sum(manifest$path)), manifest$md5))
preservation <- fread(file.path(clean_dir, "prior_outputs_preservation.csv"))
check("Prior estimation inputs, fitted objects and reports are unchanged", all(file.exists(preservation$path)) &&
  identical(unname(tools::md5sum(preservation$path)), preservation$md5_before))
directory_path <- list.files(file.path(data_root, "data/raw/school_directory/2024"), pattern = "[.]csv$", full.names = TRUE)
stopifnot(length(directory_path) == 1L)
raw <- fread(directory_path, sep = ";", encoding = "UTF-8", select = c("AGNO", "RBD", "PAGO_MATRICULA", "PAGO_MENSUAL"))
check("Raw 2024 directory has unique school identifiers", !anyDuplicated(raw$RBD) && all(raw$AGNO == 2024L))
school <- fread(file.path(clean_dir, "school_prices.csv"))
check("Saved school fee source preserves all directory rows", nrow(school) == nrow(raw) &&
  !anyDuplicated(school$rbd) && setequal(school$rbd, raw$RBD))
si <- match(school$rbd, raw$RBD)
for (field_name in c("PAGO_MATRICULA", "PAGO_MENSUAL")) {
  expected <- unname(raw_map[toupper(trimws(raw[[field_name]][si]))])
  check(paste(field_name, "saved codes reproduce raw category labels"), !anyNA(expected) &&
    identical(school[[field_name]], raw[[field_name]][si]) &&
    same_num(school[[paste0(field_name, "_CODE")]], expected, 0))
}
est <- as.data.table(readRDS(price_input_path))
prior <- as.data.table(readRDS(prior_income_input_path))
roster <- fread(file.path(clean_dir, "ranking_roster.csv"))
prior_roster <- fread(file.path(prior_income_dir, "ranking_roster.csv"))
check("Applicant roster preserves the entire original population", nrow(roster) == 294198L &&
  !anyDuplicated(roster$ranking_id) && setequal(roster$ranking_id, prior_roster$ranking_id))
ri <- match(roster$ranking_id, prior_roster$ranking_id)
check("Every pre-existing applicant flag and group is preserved", all(vapply(names(prior_roster), function(v)
  same_values(roster[[v]], prior_roster[[v]][ri]), logical(1))))
check("All previously eligible applicants retain their complete rankings", setequal(unique(est$ranking_id), unique(prior$ranking_id)) &&
  setequal(unique(est$ranking_id), roster[price_estimation_eligible == TRUE, ranking_id]) &&
  all(roster$price_estimation_eligible == roster$income_estimation_eligible))
check("Price sample has unique schools and ranks within each list", nrow(est) == nrow(prior) &&
  !anyDuplicated(est[, .(ranking_id, rbd)]) && !anyDuplicated(est[, .(ranking_id, rank_school)]) &&
  all(est[, .(ok = .N >= 2L && min(rank_school) == 1L && max(rank_school) == .N), by = ranking_id]$ok))
setorder(est, ranking_id, rank_school); setorder(prior, ranking_id, rank_school)
check("All prior applicant-school attributes and normalizations are unchanged", all(vapply(names(prior),
  function(v) same_values(est[[v]], prior[[v]]), logical(1))))
check("All destination schools match 2024 fee information", uniqueN(est$rbd) == 2481L &&
  all(est$price_directory_matched) && all(est$price_directory_year == 2024L))
ei <- match(est$rbd, raw$RBD)
for (field_name in c("PAGO_MATRICULA", "PAGO_MENSUAL")) {
  expected <- unname(raw_map[toupper(trimws(raw[[field_name]][ei]))])
  check(paste(field_name, "every estimation row matches its raw school fee band"), !anyNA(expected) &&
    same_num(est[[paste0(field_name, "_CODE")]], expected, 0))
}
check("Unknown fee information is shared by both source fields", identical(est$PAGO_MATRICULA_CODE == 0L,
  est$PAGO_MENSUAL_CODE == 0L) && same_num(est$fee_information_unknown, as.integer(est$PAGO_MENSUAL_CODE == 0L), 0))
check("The absent monthly CLP 1,000-10,000 band is not treated as free", !any(est$PAGO_MENSUAL_CODE == 2L) &&
  !any(est$PAGO_MENSUAL_CODE == 0L & est$fee_information_unknown == 0L))
for (k in seq_len(nrow(fee_specs))) {
  s <- fee_specs[k]
  expected <- if (s$field == "BOTH") as.integer(est$PAGO_MENSUAL_CODE == 0L) else
    as.integer(est[[paste0(s$field, "_CODE")]] == s$code)
  check(paste(s$term, "indicator matches its raw fee band"), same_num(est[[s$term]], expected, 0))
}
full <- as.data.table(readRDS(file.path(clean_dir, "rankings_with_directory_prices.rds")))
check("Full ranking file retains all submitted school alternatives", nrow(full) == 1007253L &&
  uniqueN(full$ranking_id) == 294198L && !anyDuplicated(full[, .(ranking_id, rbd)]))
list_coverage <- full[, .(expected = all(price_directory_matched)), by = ranking_id]
check("Full-list directory eligibility is reconstructed from every listed school", {
  ix <- match(roster$ranking_id, list_coverage$ranking_id)
  !anyNA(ix) && identical(roster$list_all_price_directory_matched, list_coverage$expected[ix])
})
rm(full, prior, prior_roster); invisible(gc())
est <- add_metric_interactions(est)
co <- fread(file.path(clean_dir, "coefficients.csv")); sl <- fread(file.path(clean_dir, "type_specific_slopes.csv"))
ms <- fread(file.path(clean_dir, "model_summary.csv")); support <- fread(file.path(clean_dir, "fee_support.csv"))
check("Twenty saved models cover every requested group, variant and distance", nrow(ms) == 20L &&
  !anyDuplicated(ms[, .(group, variant, model)]) && setequal(ms$group, names(group_labels)) &&
  setequal(ms$variant, names(variant_terms)) && setequal(ms$model, names(distance_vars)))
check("All reported model coefficients and clustered errors are finite", all(is.finite(co$estimate)) &&
  all(is.finite(co$std_error) & co$std_error > 0) && all(ms$iterations < 60L))
for (g in names(group_labels)) {
  x <- if (g == "pooled") est else est[group == g]
  x[, event := 1L]
  gs <- support[group == g]
  check(paste(g, "fee support lists every intended indicator once"), nrow(gs) == nrow(fee_specs) &&
    !anyDuplicated(gs$term) && setequal(gs$term, fee_specs$term))
  X <- as.matrix(x[, fee_terms, with = FALSE])
  group_id <- match(x$ranking_id, unique(x$ranking_id)); n_by_list <- tabulate(group_id)
  sum_by_list <- rowsum(X, group_id, reorder = FALSE)
  within_ss <- colSums(X * X) - colSums(sum_by_list * sum_by_list / n_by_list)
  for (term_name in fee_terms) {
    z <- x[get(term_name) == 1L]
    ss <- gs[term == term_name]
    check(paste(g, term_name, "support counts and within-list variation reproduce"), nrow(ss) == 1L &&
      ss$n_schools == uniqueN(z$rbd) && ss$n_pairs == nrow(z) && ss$n_rankings == uniqueN(z$ranking_id) &&
      abs(ss$within_ranking_ss - within_ss[term_name]) < 1e-7 &&
      isTRUE(ss$estimated) == isTRUE(within_ss[term_name] > 1e-10))
  }
  rm(X, sum_by_list)
  for (v in names(variant_terms)) for (m in names(distance_vars)) {
    key <- paste(g, v, m, sep = "_")
    fit <- readRDS(paste0(price_model_path(g, v, m), ".rds"))
    wanted <- c(prior_income_static_terms, if (v == "extended") gs[estimated == TRUE, term], unname(distance_vars[m]))
    b <- coef(fit); V <- vcov(fit); tab <- co[group == g & variant == v & model == m]
    check(paste(key, "saved terms exactly match identified specification"), identical(names(b), wanted) &&
      setequal(tab$term, wanted) && length(b) == nrow(tab))
    check(paste(key, "coefficient estimates and clustered errors match exports"), same_num(b, tab$estimate[match(wanted, tab$term)]) &&
      same_num(sqrt(diag(V)), tab$std_error[match(wanted, tab$term)]))
    vf <- fread(file.path(clean_dir, paste0("vcov_", key, ".csv")))
    check(paste(key, "saved covariance agrees with model object"), identical(vf$term, wanted) &&
      same_num(as.matrix(vf[, ..wanted]), as.vector(V)) && max(abs(V - t(V))) < 1e-10 && all(diag(V) > 0))
    check(paste(key, "model explicitly clusters covariance by student"),
      identical(as.character(fit$call$cluster), "mrun") && !is.null(fit$naive.var))
    if (g == "pooled" && v == "extended" && m == "main") {
      cluster_influence <- rowsum(residuals(fit, type = "dfbeta"), x$mrun, reorder = FALSE)
      check("Pooled main student-clustered sandwich reconstructs from influence scores",
        same_num(as.vector(crossprod(cluster_influence)), as.vector(V), 1e-9))
      rm(cluster_influence); invisible(gc())
    }
    row <- ms[group == g & variant == v & model == m]
    check(paste(key, "model uses complete requested sample with valid likelihood"), nrow(row) == 1L &&
      fit$n == nrow(x) && is.null(fit$na.action) && row$applicants == uniqueN(x$mrun) &&
      row$rankings == uniqueN(x$ranking_id) && row$applicant_school_pairs == nrow(x) && row$schools == uniqueN(x$rbd) &&
      abs(row$log_likelihood - as.numeric(logLik(fit))) < 1e-7 && fit$iter < 60L &&
      all(is.finite(V)) && min(eigen(fit$naive.var, symmetric = TRUE, only.values = TRUE)$values) > 0)
    slopes <- sl[group == g & variant == v & model == m]
    check(paste(key, "all school-type metric slopes are reported"), nrow(slopes) == 3L * nrow(metric_specs) &&
      !anyDuplicated(slopes[, .(school_type, term)]))
    reproduced <- lapply(seq_len(nrow(slopes)), function(k) {
      s <- slopes[k]; contrast <- setNames(numeric(length(b)), names(b)); contrast[s$term] <- 1
      if (s$school_type != "Non-TP") {
        type_control <- if (s$school_type == "Mixed HC/TP") "mixed_hc_tp" else "tp_only"
        interaction_term <- metric_interactions[control == type_control & metric == s$term, term]
        stopifnot(length(interaction_term) == 1L); contrast[interaction_term] <- 1
      }
      c(estimate = sum(contrast * b), std_error = sqrt(as.numeric(crossprod(contrast, V %*% contrast))))
    })
    reproduced <- do.call(rbind, reproduced)
    check(paste(key, "type-specific total slopes use covariance-aware contrast errors"),
      same_num(slopes$estimate, reproduced[, "estimate"]) && same_num(slopes$std_error, reproduced[, "std_error"]))
    if (v == "matched_baseline") {
      old <- readRDS(prior_price_baseline_path(g, m))
      check(paste(key, "same-sample baseline reproduces the preceding fitted model"),
        identical(names(coef(old)), names(b)) && same_num(coef(old), b, 1e-8) &&
        same_num(as.vector(vcov(old)), as.vector(V), 1e-8) && old$n == fit$n &&
        abs(as.numeric(logLik(old)) - as.numeric(logLik(fit))) < 1e-7)
    }
    rm(fit)
  }
}
check("Fee models weakly increase likelihood on identical samples", {
  pairs <- merge(ms[variant == "extended", .(group, model, ll1 = log_likelihood, n1 = applicant_school_pairs, r1 = rankings)],
    ms[variant == "matched_baseline", .(group, model, ll0 = log_likelihood, n0 = applicant_school_pairs, r0 = rankings)], by = c("group", "model"))
  nrow(pairs) == 10L && all(pairs$ll1 >= pairs$ll0 - 1e-7) && all(pairs$n1 == pairs$n0 & pairs$r1 == pairs$r0)
})
check("All previous outputs still match their original hashes after verification", identical(
  unname(tools::md5sum(preservation$path)), preservation$md5_before))
save_checks("verification.csv")
message("Independent directory-price verification: ", sum(checks$passed), "/", nrow(checks), " checks passed.")
