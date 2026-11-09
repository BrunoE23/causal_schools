# Exact premium-field decomposition, on the unchanged four-LPM estimation sample.
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setDTthreads(min(4L, getDTthreads()))
setFixest_nthreads(4L)
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
prior <- file.path(repo, "data/clean/premium_enrollment_scores_income")
clean <- file.path(repo, "data/clean/premium_field_groups_scores_income")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(prior, "analysis_data.rds")
prior_coef_path <- file.path(prior, "coefficients.csv")
input_hashes <- unname(tools::md5sum(c(cache, prior_coef_path)))
a <- as.data.table(readRDS(cache))
original_ids <- a$mrun
prior_summary <- fread(file.path(prior, "model_summary.csv"))
stopifnot(!anyNA(a$mrun), !anyDuplicated(a$mrun), all(prior_summary$N == nrow(a)))
manifest <- fread(file.path(repo, "data/clean/math_high_score_by_income/field_components/source_manifest.csv"),
                  colClasses = c(modified = "character"))
master <- manifest$path[1L]; source_info <- file.info(master)
keep <- c("mrun", "field_reclassified_m1", "f_science_m1", "f_law_m1", "f_eng_m1")
stopifnot(source_info$size == manifest$bytes[1L], as.character(source_info$mtime) == manifest$modified[1L],
          all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size, modified = as.character(source_info$mtime),
  selected_columns = paste(keep, collapse = ";"), strategy = "Read five columns and link to unchanged prior regression IDs"),
  file.path(clean, "master_source_manifest.csv"))
fwrite(data.table(path = c(cache, prior_coef_path), md5 = input_hashes), file.path(clean, "cache_source_manifest.csv"))
message("Reading five field-classification columns; retaining the exact previous regression sample...")
f <- fread(master, select = keep, na.strings = c("", "NA"))
f <- f[mrun %in% original_ids]
stopifnot(nrow(f) == nrow(a), !anyDuplicated(f$mrun))
ii <- match(a$mrun, f$mrun)
stopifnot(!anyNA(ii))
for (v in setdiff(keep, "mrun")) set(a, j = v, value = f[[v]][ii])
flags <- c("f_science_m1", "f_law_m1", "f_eng_m1")
stopifnot(all(unlist(a[, ..flags]) %in% c(0L, 1L, NA_integer_)))
# Identical positive-list rules to 04_field_components.R. Medicine+ is not all Medicine.
a[, `:=`(science = as.integer(enrolled & f_science_m1 %in% 1L),
         law = as.integer(enrolled & f_law_m1 %in% 1L),
         engineering = as.integer(enrolled & f_eng_m1 %in% 1L),
         medicine_plus = as.integer(enrolled & high_paying_field_m1 == 1L &
                                     field_reclassified_m1 %chin% "Medicine"))]
outcomes <- c("science", "law", "engineering", "medicine_plus")
labels <- c("Science", "Law", "Engineering / Manufacturing / Construction", "Medicine+")
a[, n_components := rowSums(.SD), .SDcols = outcomes]
stopifnot(identical(a$mrun, original_ids), !anyNA(a[, ..outcomes]), all(a$n_components <= 1L),
          all(a$n_components == a$high_paying_field_m1),
          all(a[enrolled == FALSE, n_components] == 0L),
          all(a[field_reclassified_m1 %chin% "Medicine", science + law + engineering] == 0L))
fwrite(a[, .(N = .N), by = .(field_reclassified_m1, high_paying_field_m1, science, law, engineering, medicine_plus)],
       file.path(clean, "component_classification_audit.csv"))
a[, `:=`(income_decile = factor(income_decile_imputed, levels = 1:10), exam_year = factor(psu_year))]
fwrite(a[, c(list(N = .N), lapply(.SD, sum)), keyby = psu_year, .SDcols = outcomes],
       file.path(clean, "outcome_counts_by_admission_year.csv"))
fwrite(a[, c(list(N = .N, n_premium_field = sum(high_paying_field_m1)), lapply(.SD, sum)),
         keyby = income_decile_imputed, .SDcols = outcomes], file.path(clean, "counts_by_income_decile.csv"))
fwrite(fread(file.path(prior, "sample_flow.csv")), file.path(clean, "sample_flow.csv"))

rhs <- c("z_math", "z_verbal", "income_decile", "exam_year")
X <- model.matrix(reformulate(rhs), data = a)
N <- nrow(X); K <- ncol(X); G <- uniqueN(a$most_time_RBD)
stopifnot(qr(X)$rank == K, all(prior_summary$n_schools == G), all(prior_summary$n_parameters == K))
cluster_index <- match(a$most_time_RBD, unique(a$most_time_RBD))
correction <- (G / (G - 1)) * ((N - 1) / (N - K))
bread <- solve(crossprod(X))
models <- list(); coefs <- list(); summaries <- list(); checks <- list(); covariances <- list()
ssc_rule <- ssc(K.adj = TRUE, G.adj = TRUE, G.df = "min", t.df = "min")
for (j in seq_along(outcomes)) {
  outcome <- outcomes[j]
  message("OLS field component: ", labels[j])
  fit <- feols(reformulate(rhs, response = outcome), data = a,
               vcov = ~most_time_RBD, ssc = ssc_rule, notes = FALSE)
  b <- coef(fit); V <- vcov(fit); e <- resid(fit)
  stopifnot(nobs(fit) == N, identical(names(b), colnames(X)), all(is.finite(b)),
            max(abs(crossprod(X, e))) / N < 1e-9)
  cluster_scores <- rowsum(X * as.numeric(e), cluster_index, reorder = FALSE)
  V_check <- correction * bread %*% crossprod(cluster_scores) %*% bread
  stopifnot(max(abs(V - V_check)) < 1e-9, all(diag(V) > 0))
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, names(ct)[2:5], c("estimate", "std_error", "t_statistic", "p_value"))
  ct[, `:=`(outcome = outcome, outcome_label = labels[j], df = G - 1L,
            lower95 = estimate - qt(.975, G - 1L) * std_error,
            upper95 = estimate + qt(.975, G - 1L) * std_error)]
  coefs[[outcome]] <- ct
  pred <- fitted(fit)
  summaries[[outcome]] <- data.table(outcome = outcome, outcome_label = labels[j], N = N,
    n_schools = G, n_parameters = K, mean_outcome = mean(a[[outcome]]),
    r_squared = 1 - sum(e^2) / sum((a[[outcome]] - mean(a[[outcome]]))^2),
    share_predictions_below_zero = mean(pred < 0), share_predictions_above_one = mean(pred > 1),
    n_non_enrollees = sum(!a$enrolled), n_admission_2026 = sum(a$psu_year == 2026L))
  checks[[outcome]] <- data.table(outcome = outcome, N = N, n_schools = G,
    max_ols_normal_equation = max(abs(crossprod(X, e))) / N,
    max_cluster_covariance_difference = max(abs(V - V_check)))
  models[[outcome]] <- fit; covariances[[outcome]] <- V
}
# Linearity of OLS on identical X: the four component coefficients add to the prior field coefficient.
prior_coefs <- fread(prior_coef_path)[outcome == "high_paying_field_m1"]
summed_coefs <- Reduce(`+`, lapply(models, coef))
comparison <- data.table(term = names(summed_coefs), sum_components = as.numeric(summed_coefs))
comparison <- merge(comparison, prior_coefs[, .(term, previous_field_estimate = estimate)], by = "term", all = TRUE)
comparison[, difference := sum_components - previous_field_estimate]
stopifnot(nrow(comparison) == K, !anyNA(comparison), max(abs(comparison$difference)) < 1e-9,
          abs(sum(vapply(summaries, function(s) s$mean_outcome, numeric(1))) -
                prior_summary[outcome == "high_paying_field_m1", mean_outcome]) < 1e-12)
fwrite(comparison, file.path(clean, "coefficient_additivity_verification.csv"))
fwrite(rbindlist(coefs), file.path(clean, "coefficients.csv"))
fwrite(rbindlist(summaries), file.path(clean, "model_summary.csv"))
fwrite(rbindlist(checks), file.path(clean, "regression_verification.csv"))
saveRDS(models, file.path(clean, "models.rds"))
saveRDS(covariances, file.path(clean, "covariance_matrices.rds"))
saveRDS(a[, c("mrun", "most_time_RBD", "cohort_gr8", "enrolled", "psu_year", "income_decile_imputed",
               "z_math", "z_verbal", "high_paying_field_m1", outcomes), with = FALSE],
        file.path(clean, "analysis_data.rds"))
stopifnot(identical(unname(tools::md5sum(c(cache, prior_coef_path))), input_hashes),
          file.info(master)$size == source_info$size, file.info(master)$mtime == source_info$mtime)
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))
print(rbindlist(summaries))
print(rbindlist(coefs)[term %in% c("z_math", "z_verbal", "income_decile10"),
  .(outcome_label, term, estimate_pp = 100 * estimate, se_pp = 100 * std_error)])
