# Independent construction, exact-likelihood and covariance checks for the PAES
# extension. Run after 13 builds its inputs and 14 finishes all twenty models.
# Existing four-VA outputs are read-only throughout this verification.
source("code/codex/school_preference_va/paes_common.R")
suppressPackageStartupMessages(library(survival))

same_numeric <- function(a, b, tolerance = 0) {
  length(a) == length(b) && all((is.na(a) & is.na(b)) |
    (!is.na(a) & !is.na(b) & abs(a - b) <= tolerance))
}
same_table <- function(a, b, columns, tolerance = 0) {
  nrow(a) == nrow(b) && all(vapply(columns, function(column) {
    if (is.numeric(a[[column]]) && is.numeric(b[[column]])) {
      same_numeric(a[[column]], b[[column]], tolerance)
    } else identical(as.character(a[[column]]), as.character(b[[column]]))
  }, logical(1)))
}

check("Extended model has five metrics, two type intercepts and ten interactions",
  nrow(metric_specs) == 5L && length(controls) == 2L && nrow(metric_interactions) == 10L &&
    identical(controls, c("mixed_hc_tp", "tp_only")) &&
    identical(extended_static_terms, c(metric_terms, controls, metric_interactions$term)) &&
    length(extended_static_terms) == 17L && !anyDuplicated(extended_static_terms))
check("Interaction specification contains every type-by-metric combination once",
  !anyDuplicated(metric_interactions[, .(control, metric)]) &&
    setequal(paste(metric_interactions$control, metric_interactions$metric),
      as.vector(outer(controls, metric_terms, paste))))
check("Matched baseline retains exactly the original four-VA specification",
  identical(variant_terms$matched_baseline, base_static_terms) && length(base_static_terms) == 14L &&
    !"z_paes_m1" %in% base_static_terms && !"tp_school" %in% extended_static_terms)
check("Primary PAES extension uses admissions process 2025", identical(primary_paes_process, 2025L))

# The raw CSVs are approximately 230 MB each. Read only six numerical fields,
# rather than loading their many item-level response columns. This independently
# verifies the small cache and prevents MAX scores being substituted for regular.
individual <- as.data.table(readRDS(file.path(clean_dir, "paes_individual_scores.rds")))
required_individual <- c("mrun", "rbd", "paes_process", "graduation_year", "paes_m1_regular",
  "paes_m1_best", "is_current_graduate", "valid_rbd", "m1_regular_observed", "included")
check("Individual PAES cache has required raw-score and inclusion fields",
  all(required_individual %in% names(individual)))
check("Individual PAES cache is unique by student and process",
  !anyDuplicated(individual[, .(mrun, paes_process)]))
raw_paths <- c(
  `2024` = file.path(data_root, "data/raw/2024/PAES-2024-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PAES_2024_PUB_MRUN.csv"),
  `2025` = file.path(data_root, "data/raw/2025/PAES_2025_Inscritos_Puntajes/A_INSCRITOS_PUNTAJES_PAES_2025_PUB_MRUN.csv"))
check("Both declared raw PAES sources exist", all(file.exists(raw_paths)))
raw_parts <- lapply(names(raw_paths), function(process) {
  path <- raw_paths[[process]]
  message("Independent narrow raw PAES audit: ", process, " (", round(file.info(path)$size / 1e6), " MB source)")
  header <- names(fread(path, nrows = 0L, showProgress = FALSE))
  upper_header <- toupper(header)
  process_column <- intersect(c("ANYO_PROCESO", "ANYIO_PROCESO"), upper_header)
  check(paste(process, "exactly one raw admissions-process field"), length(process_column) == 1L)
  raw_columns <- c("MRUN", process_column, "ANYO_DE_EGRESO", "RBD", "MATE1_REG_ACTUAL", "MATE1_MAX")
  check(paste(process, "raw regular and best score fields both available"), all(raw_columns %in% upper_header))
  raw <- fread(path, select = header[match(raw_columns, upper_header)], sep = ";", dec = ",",
    na.strings = c("", "NA"), showProgress = FALSE)
  setnames(raw, c("mrun", "paes_process", "graduation_year", "rbd", "paes_m1_regular", "paes_m1_best"))
  for (column in names(raw)) set(raw, j = column, value = as.numeric(raw[[column]]))
  check(paste(process, "raw process year matches file"), all(raw$paes_process == as.integer(process)))
  check(paste(process, "raw student identifiers observed and unique"), !anyNA(raw$mrun) && !anyDuplicated(raw$mrun))
  raw
})
raw <- rbindlist(raw_parts)
setorder(raw, paes_process, mrun)
setorder(individual, paes_process, mrun)
check("Small individual cache preserves every source record and six original fields",
  same_table(individual, raw, names(raw)))
expected_current <- !is.na(individual$graduation_year) & individual$graduation_year == individual$paes_process - 1L
expected_rbd <- is.finite(individual$rbd) & individual$rbd > 0
expected_observed <- is.finite(individual$paes_m1_regular) &
  individual$paes_m1_regular >= 100 & individual$paes_m1_regular <= 1000
expected_included <- expected_current & expected_rbd & expected_observed
check("Current-graduate indicator uses process year minus one",
  identical(as.logical(individual$is_current_graduate), expected_current))
check("Valid-school indicator requires positive observed RBD",
  identical(as.logical(individual$valid_rbd), expected_rbd))
check("Observed M1 uses current regular score in the 100-1000 range",
  identical(as.logical(individual$m1_regular_observed), expected_observed))
check("School-mean sample has exactly the declared three restrictions",
  identical(as.logical(individual$included), expected_included))
independent_school <- individual[expected_included, .(graduation_year = unique(graduation_year),
  paes_m1_mean = mean(paes_m1_regular), n_m1_takers = .N, sd_m1 = sd(paes_m1_regular),
  min_m1 = min(paes_m1_regular), max_m1 = max(paes_m1_regular)), by = .(paes_process, rbd)]
school <- fread(file.path(clean_dir, "school_paes_m1.csv"))
check("School-mean file is unique by process and RBD", !anyDuplicated(school[, .(paes_process, rbd)]))
setorder(independent_school, paes_process, rbd)
setorder(school, paes_process, rbd)
check("Every school mean and taker count matches independent current-regular aggregation",
  same_table(school, independent_school, names(independent_school), tolerance = 1e-10))
check("School counts exhaust the independently defined taker sample",
  sum(school$n_m1_takers) == sum(expected_included))
rm(raw_parts, raw, individual); invisible(gc())

est <- as.data.table(readRDS(paes_input_path))
full <- as.data.table(readRDS(file.path(clean_dir, "ranking_paes_m1.rds")))
roster <- fread(file.path(clean_dir, "ranking_roster.csv"))
old_roster <- fread(file.path(base_clean_dir, "ranking_roster.csv"))
old_est <- as.data.table(readRDS(file.path(base_clean_dir, "estimation_data.rds")))
group_roster <- fread(file.path(base_clean_dir, "by_priority_continuation/group_roster.csv"))
base_columns <- names(old_est)
pair_columns <- c("ranking_id", "mrun", "sae_proceso", "rbd", "rank_school")
check("Full ranking data retain IDs, ranks and both annual score availability flags",
  all(c(pair_columns, "list_all_m1_observed", "paes_estimation_eligible",
    paste0("paes_m1_mean_", 2024:2025), paste0("n_m1_takers_", 2024:2025),
    paste0("school_m1_observed_", 2024:2025), paste0("list_all_m1_observed_", 2024:2025)) %in% names(full)))
check("Full data retain all original ranked alternatives", nrow(full) == 1007253L &&
  !anyDuplicated(full[, .(ranking_id, rbd)]) && !anyDuplicated(full[, .(ranking_id, rank_school)]))
check("Original applicant roster is preserved", nrow(roster) == nrow(old_roster) &&
  nrow(roster) == 294198L && !anyDuplicated(roster$ranking_id) &&
    setequal(roster$ranking_id, old_roster$ranking_id))
setorder(full, ranking_id, rank_school)
setorder(est, ranking_id, rank_school)
setorder(old_est, ranking_id, rank_school)
old_index <- match(roster$ranking_id, old_roster$ranking_id)
check("Roster applicant IDs and submitted list lengths remain unchanged",
  same_table(roster, old_roster[old_index], intersect(c("ranking_id", "mrun", "sae_proceso", "n_schools", "n_programs"),
    names(roster))))
check("Roster preserves original estimation eligibility",
  identical(as.logical(roster$estimation_eligible), as.logical(old_roster$estimation_eligible[old_index])))
original_pair_path <- file.path(base_clean_dir, "student_school_rankings.rds")
message("Comparing existing full school rankings (", round(file.info(original_pair_path)$size / 1e6),
  " MB RDS); discard all columns except identifiers and submitted ranks immediately.")
full_pair_columns <- c(pair_columns, "rank_program")
old_pairs <- as.data.table(readRDS(original_pair_path))[, ..full_pair_columns]
setorder(old_pairs, ranking_id, rank_school)
check("All original student-school pairs and original program and collapsed ranks are preserved",
  same_table(full, old_pairs, full_pair_columns))
rm(old_pairs); invisible(gc())

for (process in 2024:2025) {
  source <- school[paes_process == process]
  index <- match(full$rbd, source$rbd)
  expected_mean <- source$paes_m1_mean[index]
  expected_n <- source$n_m1_takers[index]
  mean_column <- paste0("paes_m1_mean_", process)
  count_column <- paste0("n_m1_takers_", process)
  observed_column <- paste0("school_m1_observed_", process)
  list_column <- paste0("list_all_m1_observed_", process)
  check(paste(process, "every ranked school joins the independently verified mean and taker count"),
    same_numeric(full[[mean_column]], expected_mean, 1e-10) &&
      same_numeric(full[[count_column]], expected_n))
  check(paste(process, "school-score availability is observed mean plus positive taker count"),
    identical(as.logical(full[[observed_column]]), is.finite(expected_mean) & is.finite(expected_n) & expected_n > 0))
  list_flags <- data.table(ranking_id = full$ranking_id, observed = full[[observed_column]])[,
    .(observed = all(observed)), by = ranking_id]
  check(paste(process, "list completeness requires every submitted school to have a mean"),
    identical(as.logical(full[[list_column]]), list_flags$observed[match(full$ranking_id, list_flags$ranking_id)]))
}
primary_flag <- paste0("list_all_m1_observed_", primary_paes_process)
check("Primary PAES list-completeness alias uses the declared process",
  identical(as.logical(full$list_all_m1_observed), as.logical(full[[primary_flag]])))
list_flags <- full[, .(list_all_m1_observed = all(get(primary_flag)), pairs = .N), by = ranking_id]
list_index <- match(roster$ranking_id, list_flags$ranking_id)
check("Roster M1 completeness agrees with every submitted school",
  identical(as.logical(roster$list_all_m1_observed), list_flags$list_all_m1_observed[list_index]))
expected_eligible <- as.logical(old_roster$estimation_eligible[old_index]) & roster$list_all_m1_observed
check("PAES sample adds only whole-list M1 completeness to original eligibility",
  identical(as.logical(roster$paes_estimation_eligible), expected_eligible))
check("PAES group eligibility adds only whole-list M1 completeness",
  identical(as.logical(roster$paes_group_eligible), as.logical(roster$group_eligible & roster$list_all_m1_observed)))
if ("base_estimation_eligible" %in% names(roster)) {
  check("Saved original eligibility matches the unchanged builder roster",
    identical(as.logical(roster$base_estimation_eligible), as.logical(old_roster$estimation_eligible[old_index])))
}
eligible_ids <- roster[paes_estimation_eligible == TRUE, ranking_id]
check("Every eligible complete ranking is retained and no other ranking enters",
  setequal(est$ranking_id, eligible_ids))
check("Full pair-level eligibility agrees with roster eligibility",
  identical(as.logical(full$paes_estimation_eligible),
    as.logical(roster$paes_estimation_eligible[match(full$ranking_id, roster$ranking_id)])))
check("Whole-list restriction retains original row contents and four-VA units",
  all(base_columns %in% names(est)) &&
    same_table(est, old_est[ranking_id %in% eligible_ids], base_columns))
check("Estimation sample equals full retained lists without shortening or reordering",
  same_table(est, full[ranking_id %in% eligible_ids], pair_columns))
check("Every retained ranking has at least two consecutive distinct schools",
  est[, .(ok = .N >= 2L && identical(as.integer(rank_school), seq_len(.N)) && !anyDuplicated(rbd)),
    by = ranking_id][, all(ok)])
check("One submitted ranking per student and no applicant-school duplicates",
  uniqueN(est$ranking_id) == uniqueN(est$mrun) && !anyDuplicated(est[, .(ranking_id, rbd)]))
for (flag in c("group", "prioritario", "sample", "continuation_group")) {
  if (flag %in% names(est)) {
    index <- match(est$ranking_id, group_roster$ranking_id)
    check(paste("PAES estimation", flag, "preserves original applicant grouping"),
      !anyNA(index) && same_table(est, group_roster[index], flag))
  }
}
check("Four reported groups are disjoint within the pooled sample",
  all(est[, uniqueN(group), by = ranking_id]$V1 == 1L) &&
    all(est[, uniqueN(group), by = mrun]$V1 == 1L))
check("Two type intercepts remain binary and mutually exclusive",
  all(est$mixed_hc_tp %in% 0:1) && all(est$tp_only %in% 0:1) && all(est$mixed_hc_tp + est$tp_only <= 1L))
flow <- fread(file.path(clean_dir, "sample_flow.csv"), colClasses = list(character = "sae_proceso"))
check("Twenty unique sample-flow group and year rows", nrow(flow) == 20L &&
  !anyDuplicated(flow[, .(group, sae_proceso)]))
for (k in seq_len(nrow(flow))) {
  group_name <- flow$group[k]
  year_name <- flow$sae_proceso[k]
  population <- if (group_name == "pooled") roster else roster[group == group_name]
  if (year_name != "All") population <- population[sae_proceso == as.integer(year_name)]
  check(paste(group_name, year_name, "sample-loss counts independently reconcile"),
    flow$original_applicants[k] == nrow(population) &&
      flow$baseline_eligible[k] == sum(population$estimation_eligible) &&
      flow$excluded_missing_m1[k] == sum(population$estimation_eligible & !population$list_all_m1_observed) &&
      flow$estimation_applicants[k] == sum(population$paes_estimation_eligible) &&
      flow$comparable_2024_complete_applicants[k] ==
        sum(population$estimation_eligible & population$list_all_m1_observed_2024) &&
      flow$missing_2025_despite_2024_coverage[k] ==
        sum(population$estimation_eligible & !population$list_all_m1_observed_2025 & population$list_all_m1_observed_2024))
}

check("Original four-VA normalization copied without any byte changes",
  identical(unname(tools::md5sum(file.path(clean_dir, "va_normalization.csv"))),
    unname(tools::md5sum(file.path(base_clean_dir, "va_normalization.csv")))))
m1_norm <- fread(file.path(clean_dir, "paes_m1_normalization.csv"))
check("One finite PAES normalization with positive sample SD",
  nrow(m1_norm) == 1L && is.finite(m1_norm$reference_mean) &&
    is.finite(m1_norm$reference_sd) && m1_norm$reference_sd > 0)
check("PAES normalization documents current-regular scores and correct process/calendar years",
  m1_norm$paes_process == primary_paes_process && m1_norm$graduation_year == primary_paes_process - 1L &&
    m1_norm$regular_sitting_calendar_year == primary_paes_process - 1L &&
    identical(m1_norm$score_column, "MATE1_REG_ACTUAL"))
primary_school <- school[paes_process == primary_paes_process]
index <- match(est$rbd, primary_school$rbd)
check("Model raw PAES mean is the primary-process current-regular mean",
  same_numeric(est$paes_m1_mean, primary_school$paes_m1_mean[index], 1e-10))
if ("n_m1_takers" %in% names(est)) {
  check("Model PAES taker count matches the primary-process source",
    same_numeric(est$n_m1_takers, primary_school$n_m1_takers[index]))
}
schools_z <- unique(est[, .(rbd, paes_m1_mean, z_paes_m1)], by = "rbd")
check("PAES metric and school type are constant by destination RBD",
  all(est[, lapply(.SD, uniqueN), by = rbd, .SDcols = c("paes_m1_mean", "z_paes_m1", controls)][,
    unlist(.SD), .SDcols = c("paes_m1_mean", "z_paes_m1", controls)] == 1L))
check("PAES normalization uses unweighted unique retained destination schools",
  m1_norm$n_reference_schools == nrow(schools_z) &&
    abs(mean(schools_z$paes_m1_mean) - m1_norm$reference_mean) < 1e-10 &&
    abs(sd(schools_z$paes_m1_mean) - m1_norm$reference_sd) < 1e-10)
check("Standardized PAES metric exactly uses saved normalization",
  max(abs(est$z_paes_m1 - (est$paes_m1_mean - m1_norm$reference_mean) / m1_norm$reference_sd)) < 1e-12 &&
    abs(mean(schools_z$z_paes_m1)) < 1e-12 && abs(sd(schools_z$z_paes_m1) - 1) < 1e-12)
for (k in seq_len(nrow(metric_interactions))) {
  term <- metric_interactions$term[k]
  expected <- est[[metric_interactions$control[k]]] * est[[metric_interactions$metric[k]]]
  if (term %in% names(est)) {
    check(paste(term, "stored interaction equals independent product"), identical(as.numeric(est[[term]]), as.numeric(expected)))
  }
  set(est, j = term, value = expected)
  check(paste(term, "independently constructed metric-by-type product"),
    identical(as.numeric(est[[term]]), as.numeric(est[[metric_interactions$control[k]]] * est[[metric_interactions$metric[k]]])))
}
check("All extended regressors and both distances are finite",
  all(vapply(est[, c(extended_static_terms, unname(distance_vars)), with = FALSE],
    function(x) all(is.finite(x)), logical(1))))
est[, event1 := 1L]
rm(full, old_est); invisible(gc())

# Direct reverse cumulative sums implement the submitted-list PL likelihood,
# independently of the fitter's log-add-exp implementation.
independent_pl <- function(x, beta, terms) {
  work <- data.table(ranking_id = x$ranking_id, eta = as.numeric(as.matrix(x[, ..terms]) %*% beta))
  work[, {
    centered <- eta - max(eta)
    denominators <- rev(cumsum(rev(exp(centered))))
    if (any(denominators == 0)) stop("Independent PL underflow: log-add-exp check required")
    .(ll = sum(centered - log(denominators)))
  }, by = ranking_id][, sum(ll)]
}
direct_formula <- function(terms) as.formula(paste("Surv(rank_school, event1) ~", paste(terms, collapse = " + "), "+ strata(ranking_id)"))
expanded_formula <- function(terms) as.formula(paste("chosen ~", paste(terms, collapse = " + "), "+ strata(stage_id)"))

coefficients <- fread(file.path(clean_dir, "coefficients.csv"))
summary <- fread(file.path(clean_dir, "model_summary.csv"))
diagnostics <- fread(file.path(clean_dir, "model_diagnostics.csv"))
slopes <- fread(file.path(clean_dir, "type_specific_slopes.csv"))
check("Twenty distinct group-variant-distance model summaries",
  nrow(summary) == 20L && !anyDuplicated(summary[, .(group, variant, model)]) &&
    setequal(summary$group, names(group_labels)) && setequal(summary$variant, names(variant_terms)) &&
    setequal(summary$model, names(distance_vars)))
check("All 330 requested coefficients are exported once",
  nrow(coefficients) == 330L && !anyDuplicated(coefficients[, .(group, variant, model, term)]))
check("All 270 type-specific metric slopes are exported once",
  nrow(slopes) == 270L && !anyDuplicated(slopes[, .(group, variant, model, school_type, outcome)]))
check("Twenty distinct model diagnostic rows", nrow(diagnostics) == 20L &&
  !anyDuplicated(diagnostics[, .(group, variant, model)]))
type_controls <- c("Non-TP" = NA_character_, "Mixed HC/TP" = "mixed_hc_tp", "TP only" = "tp_only")
comparison <- list()
control <- coxph.control(iter.max = 100L, eps = 1e-10)
for (group_name in names(group_labels)) {
  message("Independent PAES model validation: ", group_name)
  d <- if (group_name == "pooled") copy(est) else copy(est[group == group_name])
  check(paste(group_name, "has complete nonempty rankings"), nrow(d) > 0L &&
    all(d[, .N, by = ranking_id]$N >= 2L))
  ranks <- d[, .(alternatives = .N), by = ranking_id]
  regular_ids <- ranks$ranking_id[unique(as.integer(round(seq(1L, nrow(ranks),
    length.out = min(200L, nrow(ranks))))))]
  long_ids <- ranks[order(-alternatives, ranking_id), head(ranking_id, 40L)]
  sample_ids <- unique(c(regular_ids, long_ids))
  sub <- copy(d[ranking_id %in% sample_ids])
  sub[, validation_ranking := match(ranking_id, unique(ranking_id))]
  check(paste(group_name, "independent validation sample preserves entire submitted rankings"),
    nrow(sub) == ranks[ranking_id %in% sample_ids, sum(alternatives)])
  expanded <- rbindlist(lapply(split(sub, by = "ranking_id", keep.by = TRUE), function(z) {
    rbindlist(lapply(seq_len(nrow(z) - 1L), function(r) {
      part <- copy(z[r:nrow(z)])
      part[, `:=`(chosen = as.integer(seq_len(.N) == 1L), stage_id = paste0(validation_ranking[1L], "_", r))]
      part
    }))
  }))
  check(paste(group_name, "each expanded choice stage has one chosen school"),
    all(expanded[, sum(chosen), by = stage_id]$V1 == 1L))
  saved <- list()
  for (variant_name in names(variant_terms)) for (model_name in names(distance_vars)) {
    key <- paste(group_name, variant_name, model_name, sep = "_")
    terms <- c(variant_terms[[variant_name]], unname(distance_vars[model_name]))
    fit <- readRDS(paste0(paes_model_path(group_name, variant_name, model_name), ".rds"))
    saved[[paste(variant_name, model_name, sep = "_")]] <- fit
    check(paste(key, "all requested coefficients retained"), inherits(fit, "coxph") &&
      identical(names(coef(fit)), terms) && length(terms) == if (variant_name == "extended") 18L else 15L)
    check(paste(key, "finite coefficient and student-clustered covariance"),
      all(is.finite(coef(fit))) && all(is.finite(vcov(fit))) && all(diag(vcov(fit)) > 0) && !is.null(fit$naive.var))
    check(paste(key, "student clustering and unchanged event input"),
      identical(gsub("\\s", "", paste(deparse(fit$call$cluster), collapse = "")), "mrun") &&
        fit$n == nrow(d) && fit$nevent == nrow(d) && is.null(fit$na.action) &&
        identical(as.numeric(fit$y[, 1L]), as.numeric(d$rank_school)) && all(fit$y[, 2L] == 1))
    labels <- attr(stats::terms(fit$call$formula), "term.labels")
    check(paste(key, "stored formula exactly matches the requested variant"),
      setequal(setdiff(labels, grep("^strata\\(", labels, value = TRUE)), terms) &&
        "strata(ranking_id)" %in% labels && !"tp_school" %in% all.vars(fit$call$formula) &&
        identical(fit$method, "breslow"))
    metadata <- attr(fit, "preference_model")
    check(paste(key, "saved model metadata identify its sample and metric normalizations"),
      identical(metadata$group, group_name) && identical(metadata$variant, variant_name) &&
        identical(metadata$model, model_name) && identical(metadata$estimation_data, paes_input_path) &&
        identical(metadata$va_normalization, file.path(clean_dir, "va_normalization.csv")) &&
        identical(metadata$paes_normalization, file.path(clean_dir, "paes_m1_normalization.csv")))
    row_summary <- summary[group == group_name & variant == variant_name & model == model_name]
    check(paste(key, "exported sample sizes equal complete subgroup input"), nrow(row_summary) == 1L &&
      row_summary$applicants == uniqueN(d$mrun) && row_summary$rankings == uniqueN(d$ranking_id) &&
        row_summary$applicant_school_pairs == nrow(d) && row_summary$schools == uniqueN(d$rbd))
    row_coef <- coefficients[group == group_name & variant == variant_name & model == model_name]
    row_coef <- row_coef[match(terms, term)]
    check(paste(key, "exported coefficients and clustered standard errors equal model"),
      nrow(row_coef) == length(terms) && identical(row_coef$term, terms) &&
        max(abs(row_coef$estimate - coef(fit))) < 1e-12 &&
        max(abs(row_coef$std_error - sqrt(diag(vcov(fit))))) < 1e-12)
    covariance <- fread(file.path(clean_dir, paste0("vcov_", key, ".csv")))
    information_export <- fread(file.path(clean_dir, paste0("information_", key, ".csv")))
    information <- solve(fit$naive.var)
    check(paste(key, "exported covariance equals full clustered covariance"),
      identical(covariance$term, terms) && identical(setdiff(names(covariance), "term"), terms) &&
        max(abs(as.matrix(covariance[, ..terms]) - vcov(fit))) < 1e-12)
    check(paste(key, "exported information equals saved likelihood information"),
      identical(information_export$term, terms) && identical(setdiff(names(information_export), "term"), terms) &&
        max(abs(as.matrix(information_export[, ..terms]) - information)) < 1e-6)
    info_eigen <- eigen((information + t(information)) / 2, symmetric = TRUE, only.values = TRUE)$values
    check(paste(key, "likelihood information positive definite and optimizer converged"),
      all(is.finite(info_eigen)) && min(info_eigen) > 0 && fit$iter < 60L)

    active <- metric_specs[standardized %in% terms]
    check(paste(key, "requested number of school-type metric slopes"),
      slopes[group == group_name & variant == variant_name & model == model_name, .N] == 3L * nrow(active))
    for (type_name in names(type_controls)) for (k in seq_len(nrow(active))) {
      base_term <- active$standardized[k]
      contrast_terms <- base_term
      type_control <- type_controls[[type_name]]
      if (!is.na(type_control)) {
        interaction <- metric_interactions[control == type_control & metric == base_term, term]
        check(paste(key, type_name, base_term, "unique active metric interaction"), length(interaction) == 1L && interaction %in% terms)
        contrast_terms <- c(contrast_terms, interaction)
      }
      a <- setNames(numeric(length(terms)), terms)
      a[contrast_terms] <- 1
      expected_estimate <- sum(a * coef(fit))
      expected_se <- sqrt(as.numeric(crossprod(a, vcov(fit) %*% a)))
      row_slope <- slopes[group == group_name & variant == variant_name & model == model_name &
        school_type == type_name & outcome == active$outcome[k]]
      check(paste(key, type_name, base_term, "independent total slope and full clustered contrast SE"),
        nrow(row_slope) == 1L && row_slope$term == base_term && expected_se > 0 &&
          abs(row_slope$estimate - expected_estimate) < 1e-12 && abs(row_slope$std_error - expected_se) < 1e-12)
      expected_z <- expected_estimate / expected_se
      check(paste(key, type_name, base_term, "reported slope inference and contrast terms"),
        abs(row_slope$z - expected_z) < 1e-10 &&
          abs(row_slope$p_value - 2 * pnorm(-abs(expected_z))) < 1e-12 &&
          abs(row_slope$lower95 - (expected_estimate - qnorm(.975) * expected_se)) < 1e-12 &&
          abs(row_slope$upper95 - (expected_estimate + qnorm(.975) * expected_se)) < 1e-12 &&
          setequal(strsplit(gsub("\\s", "", row_slope$contrast_terms), "+", fixed = TRUE)[[1L]], contrast_terms))
    }
    exact_ll <- independent_pl(d, coef(fit), terms)
    stored_ll <- as.numeric(tail(fit$loglik, 1L))
    full_difference <- abs(exact_ll - stored_ll)
    check(paste(key, "independent full-sample submitted-list likelihood"), full_difference < 1e-6,
      format(full_difference, digits = 10))
    check(paste(key, "exported likelihood equals saved exact model"), abs(row_summary$log_likelihood - stored_ll) < 1e-6)

    # Check the full requested design on the validation sample before fitting;
    # no interaction or metric is dropped if a small sample lacks support.
    X <- as.matrix(sub[, ..terms])
    group_index <- match(sub$ranking_id, unique(sub$ranking_id))
    group_counts <- tabulate(group_index)
    centered <- X - (rowsum(X, group_index, reorder = FALSE) / group_counts)[group_index, , drop = FALSE]
    check(paste(key, "all requested regressors identified in independent validation sample"),
      qr(centered, tol = 1e-10)$rank == length(terms))
    direct <- coxph(direct_formula(terms), data = sub, cluster = mrun, ties = "breslow",
      singular.ok = FALSE, na.action = na.fail, control = control)
    exploded <- clogit(expanded_formula(terms), data = expanded, cluster = mrun, method = "breslow",
      singular.ok = FALSE, na.action = na.fail, control = control)
    beta_difference <- max(abs(coef(direct) - coef(exploded)))
    vcov_difference <- max(abs(vcov(direct) - vcov(exploded)))
    likelihood_difference <- abs(tail(direct$loglik, 1L) - tail(exploded$loglik, 1L))
    check(paste(key, "expanded conditional-logit coefficient equivalence"), beta_difference < 1e-7,
      format(beta_difference, digits = 10))
    check(paste(key, "expanded student-clustered covariance equivalence"), vcov_difference < 1e-7,
      format(vcov_difference, digits = 10))
    check(paste(key, "expanded conditional-logit likelihood equivalence"), likelihood_difference < 1e-7,
      format(likelihood_difference, digits = 10))
    comparison[[key]] <- data.table(group = group_name, variant = variant_name, model = model_name,
      rankings = uniqueN(sub$ranking_id), student_clusters = uniqueN(sub$mrun), alternatives = nrow(sub),
      exploded_rows = nrow(expanded), coefficients = length(terms), max_abs_coefficient_difference = beta_difference,
      max_abs_student_cluster_vcov_difference = vcov_difference, abs_subsample_loglik_difference = likelihood_difference,
      independent_full_loglik = exact_ll, stored_full_loglik = stored_ll, abs_full_loglik_difference = full_difference)
  }
  reference_y <- saved[[1L]]$y
  check(paste(group_name, "both variants and both distances use identical complete rankings"),
    all(vapply(saved, function(fit) identical(fit$y, reference_y) && fit$n == nrow(d), logical(1))))
  for (model_name in names(distance_vars)) {
    extended <- saved[[paste("extended", model_name, sep = "_")]]
    baseline <- saved[[paste("matched_baseline", model_name, sep = "_")]]
    check(paste(group_name, model_name, "nested extended likelihood weakly improves on matched baseline"),
      tail(extended$loglik, 1L) >= tail(baseline$loglik, 1L) - 1e-7)
  }
  rm(d, sub, expanded, saved, X, centered); invisible(gc())
}

preservation <- fread(file.path(clean_dir, "prior_outputs_preservation.csv"))
check("Pre-extension original-file hash manifest is nonempty and unique",
  nrow(preservation) > 0L && !anyDuplicated(preservation$path) && all(file.exists(preservation$path)))
after_hash <- unname(tools::md5sum(preservation$path))
check("Every preserved original input model table and report is byte-identical",
  identical(after_hash, preservation$md5_before) && all(file.info(preservation$path)$size == preservation$bytes))
fwrite(rbindlist(comparison), file.path(clean_dir, "validation_comparison.csv"))
save_checks("verification.csv")
message("All ", nrow(checks), " independent PAES-extension verification checks passed.")
