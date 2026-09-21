# Run from repository root after 01_run.R. Uses saved analysis inputs only.
# Main treated-state recentered estimator minus the same-sample applicant mean.
suppressPackageStartupMessages(library(data.table))
source('code/codex/high_va_compliers/helpers.R')
out <- 'data/clean/high_va_compliers'
tables <- 'output/tables/high_va_compliers'
percentile <- as.integer(Sys.getenv('HIGH_VA_PERCENTILE', '75'))
stopifnot(percentile %in% c(50L, 75L))
if (percentile == 50L) {
  out <- file.path(out, 'median')
  tables <- file.path(tables, 'median')
}
results <- fread(file.path(out, 'complier_means.csv'))
main <- results[method == 'recentered' & state == 1]
variables <- unique(main$characteristic)
students <- fread(file.path(out, 'student_analysis.csv'),
  select = c('va', 'exclusion', 'D', 'Z', 'q', variables))
students <- students[exclusion == 'eligible']
centered <- rbindlist(lapply(seq_len(nrow(main)), function(i) {
  row <- main[i]
  s <- students[va == row$va & is.finite(get(row$characteristic))]
  x <- s[[row$characteristic]]
  fit <- ratio_mean(x, s$D, s$Z, s$q)
  stopifnot(length(x) == row$n, abs(fit$estimate - row$estimate) < 1e-9,
            abs(mean(x) - row$applicant_mean) < 1e-9)
  b <- (s$Z - s$q) * s$D
  # Joint influence function includes the estimated mean and its covariance
  # with the complier mean. Do not reuse the uncentered estimator's SE.
  influence <- b * (x - fit$estimate) / mean(b) - (x - mean(x))
  n <- length(x)
  difference <- fit$estimate - mean(x)
  difference_se <- sqrt(sum(influence^2) / (n * (n - 1)))
  row[, .(va, characteristic, n, complier_mean = estimate, applicant_mean,
    difference = difference, difference_se = difference_se,
    ci_low = difference - 1.96 * difference_se,
    ci_high = difference + 1.96 * difference_se,
    reference_population = 'Same eligible applicants with this characteristic observed')]
}))
stopifnot(nrow(centered) == 30L, all(is.finite(centered$difference_se)),
          all(centered$difference_se > 0))
fwrite(centered, file.path(out, 'complier_minus_applicant_means.csv'))
centered[, cell := sprintf('%+.3f (%.3f)', difference, difference_se)]
tab <- dcast(centered, characteristic ~ va, value.var = 'cell')
fwrite(tab, file.path(tables, 'complier_minus_applicant_means.csv'))
writeLines(c('# Complier characteristics relative to eligible applicants', '',
  paste('High VA: strictly above the school-level P', percentile, '.', sep = ''), '',
  'Each entry is the complier mean minus the mean among the same eligible applicants with that characteristic observed.',
  'This is a difference in baseline composition, not a treatment effect. The reference is not the national student population or all SAE applicants.',
  'Complier means retain the original assignment-variance weighting; reference means give applicants equal weights. Reference populations differ across VA measures and characteristic availability.', '',
  paste('|', paste(names(tab), collapse = ' | '), '|'),
  paste('|', paste(rep('---', ncol(tab)), collapse = ' | '), '|'),
  apply(tab, 1, function(row) paste('|', paste(row, collapse = ' | '), '|')), '',
  'Parentheses contain joint heteroskedastic-robust SEs, accounting for estimation of both means and their covariance.',
  'Female differences are in share units (0.01 = one percentage point); baseline scores are in SD units, income in deciles, and parent education in years.',
  'School VA categories and assignment probabilities are treated as fixed, as in the original analysis.'),
  file.path(tables, 'complier_minus_applicant_means.md'))
print(tab)
