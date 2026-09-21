# Run from repository root: Rscript code/codex/high_va_compliers/01_run.R
suppressPackageStartupMessages({library(data.table); library(fixest)})
source('code/codex/high_va_compliers/helpers.R')
setDTthreads(min(4L, getDTthreads()))
roots <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'),
           'C:/Users/brunem/Box/causal_schools', 'C:/Users/brunem/Dropbox/causal_schools')
roots <- roots[nzchar(roots) & file.exists(file.path(roots, 'data/clean/univ_gr8_df.csv'))]
if (!length(roots)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the project data root.')
clean <- file.path(roots[1], 'data/clean')
years <- as.integer(strsplit(Sys.getenv('COMPLIER_YEARS', '2018,2019,2020'), ',')[[1]])
stopifnot(length(years) > 0L, !anyNA(years), !anyDuplicated(years))
out <- 'data/clean/high_va_compliers'
tables <- 'output/tables/high_va_compliers'
percentile <- as.integer(Sys.getenv('HIGH_VA_PERCENTILE', '75'))
stopifnot(percentile %in% c(25L, 50L, 75L))
indicator_dir <- 'data/clean/high_va_cutoffs'
if (percentile == 25L) {
  out <- file.path(out, 'p25')
  tables <- file.path(tables, 'p25')
  indicator_dir <- file.path(indicator_dir, 'p25')
}
if (percentile == 50L) {
  out <- file.path(out, 'median')
  tables <- file.path(tables, 'median')
  indicator_dir <- file.path(indicator_dir, 'median')
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(tables, recursive = TRUE, showWarnings = FALSE)
keys <- c('math', 'language', 'highinst', 'highpay', 'program_income_full')
schools <- fread(file.path(indicator_dir, 'school_high_va.csv'), na.strings = 'NA')
stopifnot(!anyDuplicated(schools$school_rbd))
columns <- c('student_id', 'mrun', 'cohort_gr8', 'sae_proceso', 'timely_sae',
  'rbd_treated_1R', 'most_time_RBD', 'GEN_ALU', 'z_sim_mat_4to', 'z_sim_leng_4to',
  'income_decile_imputed', 'father_educ_years_imputed', 'mother_educ_years_imputed')
message('Reading only ', length(columns), ' selected student columns; no post-school outcomes.')
u <- fread(file.path(clean, 'univ_gr8_df.csv'), select = columns, na.strings = c('', 'NA'))
u <- u[timely_sae == 1 & sae_proceso %in% years & !is.na(student_id)]
stopifnot(!anyDuplicated(u[, .(student_id, sae_proceso)]), !anyDuplicated(u$mrun[!is.na(u$mrun)]))
u[, female := fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2), NA_real_)]
message('Timely applicants: ', nrow(u))
probabilities <- rbindlist(lapply(years, function(y) {
  message('Aggregating assignment probabilities for ', y)
  p <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
             select = c('student_id', 'school_id', 'prob'))
  p <- p[student_id %in% u[sae_proceso == y, student_id]]
  stopifnot(!anyDuplicated(p[, .(student_id, school_id)]))
  a <- aggregate_high_probability(p, schools, keys)
  a[, sae_proceso := y]
  a
}))
u <- merge(u, probabilities, by = c('student_id', 'sae_proceso'), all.x = TRUE, sort = FALSE)
characteristics <- c('female', 'z_sim_mat_4to', 'z_sim_leng_4to',
  'income_decile_imputed', 'father_educ_years_imputed', 'mother_educ_years_imputed')
results <- list(); diagnostics <- list(); student_frames <- list(); pos <- 0L
for (k in keys) {
  message('Estimating complier characteristics: ', k)
  d <- copy(u)
  h <- schools[[paste0('high_va_', k)]]
  d[, D := h[match(most_time_RBD, schools$school_rbd)]]
  d[, Z := h[match(rbd_treated_1R, schools$school_rbd)]]
  # Existing first-round convention: zero means no first-round school offer.
  d[!is.na(rbd_treated_1R) & rbd_treated_1R == 0, Z := 0L]
  d[, `:=`(q = get(paste0('q_', k)), unknown_mass = get(paste0('unknown_', k)))]
  # Apply these exclusions in order and retain the reason for auditing.
  d[, exclusion := fcase(is.na(total_mass), 'no_probability_record',
    abs(total_mass - 1) > 1e-6, 'probability_mass_not_one',
    unknown_mass > 1e-12, 'unclassified_assignment_option',
    q <= 1e-10 | q >= 1 - 1e-10, 'no_high_low_offer_risk',
    is.na(Z), 'unclassified_observed_offer', is.na(D), 'unclassified_attended_school',
    default = 'eligible')]
  diagnostics[[k]] <- d[, .N, by = exclusion][, va := k]
  student_frames[[k]] <- d[, c('student_id', 'sae_proceso', 'D', 'Z', 'q',
    'total_mass', 'unknown_mass', 'exclusion', characteristics), with = FALSE][, va := k]
  eligible <- d[exclusion == 'eligible']
  for (v in characteristics) {
    s <- eligible[is.finite(get(v))]
    x <- s[[v]]
    w <- s$Z - s$q
    # Assignment-variance-normalized first-stage moment; equals a weighted
    # complier share in population, but not necessarily bounded in finite samples.
    fs <- if (nrow(s)) sum(w * s$D) / sum(s$q * (1 - s$q)) else NA_real_
    for (state in 0:1) for (method in c('recentered', 'spline_df4')) {
      fun <- if (method == 'recentered') ratio_mean else fit_spline_mean
      r <- safe_fit(fun, x, s$D, s$Z, s$q, state)
      r[, `:=`(va = k, characteristic = v, state = state, method = method,
        n = nrow(s), n_eligible = nrow(eligible), n_missing_characteristic = nrow(eligible) - nrow(s),
        applicant_mean = if (nrow(s)) mean(x) else NA_real_,
        weighted_first_stage = fs,
        offer_balance_moment = if (nrow(s)) mean(w * x) else NA_real_)]
      pos <- pos + 1L; results[[pos]] <- r
    }
  }
}
result <- rbindlist(results, fill = TRUE)
result[, cutoff_percentile := percentile]
fwrite(result, file.path(out, 'complier_means.csv'), na = 'NA')
fwrite(rbindlist(diagnostics), file.path(out, 'sample_exclusions.csv'))
fwrite(rbindlist(student_frames), file.path(out, 'student_analysis.csv'), na = 'NA')
main <- result[method == 'recentered' & state == 1]
main[, cell := fifelse(status == 'ok', sprintf('%.3f (%.3f)', estimate, se), status)]
tab <- dcast(main, characteristic ~ va, value.var = 'cell')
fwrite(tab, file.path(tables, 'baseline_complier_means.csv'))
lines <- c('# Baseline characteristics of high-VA compliers', '',
  paste('High VA: strictly above the school-level P', percentile, '.', sep = ''), '',
  paste('Assignment years:', paste(years, collapse = ', ')), '',
  'Main estimates use (Z-q) moments with the treated-state equation. Parentheses contain heteroskedastic-robust standard errors.',
  'These are assignment-variance-weighted complier means, conditional on valid classification and observed baseline covariates.', '',
  paste('|', paste(names(tab), collapse = ' | '), '|'),
  paste('|', paste(rep('---', ncol(tab)), collapse = ' | '), '|'),
  apply(tab, 1, function(row) paste('|', paste(row, collapse = ' | '), '|')), '',
  'Income and parent education use existing baseline imputations. Female is a share; math/language are grade-4 standardized scores; parent education is in years.',
  'See data/clean/high_va_compliers/complier_means.csv for untreated-state estimates, spline sensitivity, applicant means, confidence intervals, sample sizes, first-stage and balance diagnostics.',
  'Standard errors condition on school VA classifications and simulated assignment probabilities; they do not propagate VA estimation or simulation uncertainty.',
  'No-first-round-offer (RBD 0) and simulated unmatched states have Z=0. Missing VA is never classified as low.',
  'Positive first stages do not establish individual monotonicity. Missing attendance and covariate exclusions may affect interpretation; inspect the saved exclusion counts.')
writeLines(lines, file.path(tables, 'baseline_complier_means.md'))
writeLines(capture.output(sessionInfo()), file.path(out, 'session_info.txt'))
print(tab)
if (any(result$status != 'ok')) warning('Some estimates failed; see status column.')
message('Completed ', nrow(result), ' estimates; all outputs are separate from existing analyses.')
