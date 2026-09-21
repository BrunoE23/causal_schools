# High VA = strictly above the school-weighted empirical 75th percentile.
# Run from repository root; original VA inputs remain unchanged.
suppressPackageStartupMessages(library(data.table))
input <- 'output/tables/empirical_bayes_school_va'
out <- 'data/clean/high_va_cutoffs'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
keys <- c('math', 'language', 'highinst', 'highpay', 'program_income_full')
values <- rbindlist(lapply(keys, function(k) {
  x <- fread(file.path(input, paste0('stata_eb_school_values_', k, '.csv')))
  x <- x[analysis_sample == 'All']
  stopifnot(nrow(x) > 0L, !anyNA(x$school_rbd), !anyDuplicated(x$school_rbd))
  valid <- is.finite(x$va_eb_centered)
  stopifnot(any(valid))
  # Type 1 matches the inverse empirical CDF used in the distribution plots.
  cutoff <- unname(quantile(x$va_eb_centered[valid], .75, type = 1))
  result <- data.table(school_rbd = x$school_rbd, outcome = x$outcome,
             va_eb_centered = x$va_eb_centered, cutoff_p75 = cutoff,
             high_va = fifelse(valid, as.integer(x$va_eb_centered > cutoff), NA_integer_))
  result[, key := k]
  result
}))
summary <- values[, .(cutoff_p75 = unique(cutoff_p75),
                      n_schools = .N, n_valid = sum(!is.na(high_va)),
                      n_missing = sum(is.na(high_va)), n_high = sum(high_va, na.rm = TRUE),
                      share_high = mean(high_va, na.rm = TRUE),
                      n_at_cutoff = sum(va_eb_centered == cutoff_p75, na.rm = TRUE)),
                  by = .(key, outcome)]
wide <- dcast(values, school_rbd ~ key, value.var = 'high_va')
setnames(wide, keys, paste0('high_va_', keys))
setorder(values, school_rbd, key)
setorder(wide, school_rbd)
stopifnot(!anyDuplicated(wide$school_rbd),
          all(is.na(values$high_va) | values$high_va %in% 0:1),
          all(summary$share_high <= .25 + 1e-10))
fwrite(values, file.path(out, 'school_high_va_long.csv'), na = 'NA')
fwrite(wide, file.path(out, 'school_high_va.csv'), na = 'NA')
fwrite(summary, file.path(out, 'high_va_cutoffs.csv'), na = 'NA')
# Verify saved indicators match source VA and preserve unavailable outcomes.
saved <- fread(file.path(out, 'school_high_va.csv'), na.strings = 'NA')
for (k in keys) {
  source <- values[key == k]
  actual <- saved[[paste0('high_va_', k)]][match(source$school_rbd, saved$school_rbd)]
  stopifnot(identical(actual, source$high_va),
            all(is.na(saved[[paste0('high_va_', k)]][!saved$school_rbd %in% source$school_rbd])))
}
print(summary)
cat('Saved and verified indicators for', nrow(wide), 'schools.\n')
