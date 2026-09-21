# All three cutoffs, one common timely-SAE applicant reference population.
# Uses full saved rosters, including students excluded from IV estimation.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
source('code/codex/high_va_compliers/helpers.R')
base <- 'data/clean/high_va_compliers'
table_base <- 'output/tables/high_va_compliers'
variables <- c('female', 'z_sim_mat_4to', 'z_sim_leng_4to',
  'income_decile_imputed', 'father_educ_years_imputed', 'mother_educ_years_imputed')
cols <- c('student_id', 'sae_proceso', 'va', 'exclusion', 'D', 'Z', 'q', variables)
all_results <- list()
population <- NULL
for (percentile in c(75L, 50L, 25L)) {
  suffix <- switch(as.character(percentile), '75' = '', '50' = 'median', '25' = 'p25')
  path <- file.path(base, suffix)
  tables <- file.path(table_base, suffix)
  message('Centering P', percentile, ' on the full saved timely-SAE roster.')
  students <- fread(file.path(path, 'student_analysis.csv'), select = cols)
  roster <- students[va == 'math', c('student_id', 'sae_proceso', variables), with = FALSE]
  setorder(roster, sae_proceso, student_id)
  stopifnot(!anyDuplicated(roster[, .(student_id, sae_proceso)]))
  if (is.null(population)) population <- copy(roster) else stopifnot(identical(roster, population))
  # Verify each VA contains precisely the same full roster and characteristics.
  for (k in unique(students$va)) {
    check <- students[va == k, c('student_id', 'sae_proceso', variables), with = FALSE]
    setorder(check, sae_proceso, student_id)
    stopifnot(identical(check, population))
  }
  results <- fread(file.path(path, 'complier_means.csv'))
  main <- results[method == 'recentered' & state == 1]
  centered <- rbindlist(lapply(seq_len(nrow(main)), function(i) {
    row <- main[i]
    v <- row$characteristic
    p <- population[is.finite(get(v))]
    s <- students[va == row$va & exclusion == 'eligible' & is.finite(get(v))]
    match_id <- match(paste(s$sae_proceso, s$student_id), paste(p$sae_proceso, p$student_id))
    stopifnot(!anyNA(match_id), !anyDuplicated(match_id))
    x <- s[[v]]
    fit <- ratio_mean(x, s$D, s$Z, s$q)
    stopifnot(length(x) == row$n, abs(fit$estimate - row$estimate) < 1e-9)
    mu <- mean(p[[v]])
    b <- (s$Z - s$q) * s$D
    # Joint sandwich over the common population, with zero complier-estimator
    # contributions outside the IV sample. Includes the overlapping-sample covariance.
    contributions <- -(p[[v]] - mu) / nrow(p)
    contributions[match_id] <- contributions[match_id] + b * (x - fit$estimate) / sum(b)
    se_difference <- sqrt(nrow(p) / (nrow(p)-1) * sum(contributions^2))
    difference <- fit$estimate - mu
    data.table(va = row$va, characteristic = v, cutoff_percentile = percentile,
      n_complier_estimation = length(x), n_population_observed = nrow(p),
      population_mean = mu, complier_mean = fit$estimate, difference = difference,
      difference_se = se_difference, ci_low = difference - 1.96*se_difference,
      ci_high = difference + 1.96*se_difference)
  }))
  stopifnot(nrow(centered) == 30, all(is.finite(centered$difference_se)), all(centered$difference_se > 0))
  fwrite(centered, file.path(path, 'complier_minus_sae_population.csv'))
  all_results[[as.character(percentile)]] <- centered
  centered[, cell := sprintf('%+.3f (%.3f)', difference, difference_se)]
  tab <- dcast(centered, characteristic ~ va, value.var = 'cell')
  fwrite(tab, file.path(tables, 'complier_minus_sae_population.csv'))
  writeLines(c('# Complier characteristics relative to the common SAE population', '',
    paste0('High VA: strictly above P', percentile, '.'),
    paste0('Reference: all ', nrow(population), ' timely SAE applicants in the saved ',
      paste(sort(unique(population$sae_proceso)), collapse = ', '), ' roster, with the characteristic observed.'),
    'Includes applicants excluded from IV estimation: no assignment risk, incomplete VA coverage, or missing attendance. The same characteristic-specific population mean is used for every VA and cutoff.', '',
    paste('|', paste(names(tab), collapse = ' | '), '|'),
    paste('|', paste(rep('---', ncol(tab)), collapse = ' | '), '|'),
    apply(tab, 1, function(row) paste('|', paste(row, collapse = ' | '), '|')), '',
    'Parentheses: joint heteroskedastic-robust SEs accounting for the shared observations in both estimators. Saved school VA and simulated probabilities are treated as fixed.',
    'Differences describe baseline composition, not causal effects. Female is in share units; achievement in SDs; income in deciles; parent education in years.'),
    file.path(tables, 'complier_minus_sae_population.md'))
}
combined <- rbindlist(all_results)
stopifnot(all(combined[, uniqueN(population_mean), by = characteristic]$V1 == 1L))
fwrite(combined, file.path(base, 'all_cutoffs_minus_sae_population.csv'))
benchmark <- unique(combined[, .(characteristic, n_population_observed, population_mean)])
fwrite(benchmark, file.path(base, 'sae_population_benchmarks.csv'))
small <- dcast(combined[va == 'program_income_full' & characteristic %in%
  c('income_decile_imputed', 'father_educ_years_imputed', 'mother_educ_years_imputed')],
  characteristic + population_mean ~ cutoff_percentile, value.var = 'difference')
print(small)
cat('Verified common reference for 90 centered estimates.\n')
