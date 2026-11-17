# Reuse verified Tables 4/5 estimates; do not refit or restrict the pooled sample.
lottery_dir <- file.path(root, 'data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation')
lottery <- fread(file.path(lottery_dir, 'results.csv'))
lottery_replication <- fread(file.path(lottery_dir, 'paper_replication.csv'))
lottery_checks <- fread(file.path(lottery_dir, 'verification.csv'))
stopifnot(nrow(lottery_replication) == 42L, all(lottery_replication$passed), all(lottery_checks$passed),
  nrow(lottery) == 21L, !anyDuplicated(lottery[, .(sample, spec)]))
pooled_values <- copy(lottery[sample == 'all'])
pooled_values[, n_obs := as.numeric(n_obs)]
current_pooled <- melt(pooled_values, id.vars = 'spec',
  measure.vars = c('beta', 'se', 'fs_beta', 'fs_se', 'fs_f', 'n_obs'), variable.name = 'statistic', value.name = 'current')
pooled_check <- merge(current_pooled, lottery_replication, by = c('spec', 'statistic'))
stopifnot(nrow(pooled_check) == 42L, max(abs(pooled_check$current - pooled_check$estimated)) < 1e-10)
lottery_groups <- c('all', 'school_ending', 'voluntary')
lottery_labels <- c(all = 'Pooled (paper)', school_ending = 'School ending', voluntary = 'Voluntary')
lottery_specs <- lottery[sample == 'all', .(spec, label)]
stopifnot(nrow(lottery_specs) == 7L, setequal(lottery$sample, lottery_groups))
for (g in lottery_groups) stopifnot(setequal(lottery[sample == g, spec], lottery_specs$spec))
for (field in c('beta', 'se', 'fs_beta', 'fs_se', 'fs_f', 'n_obs')) stopifnot(all(is.finite(lottery[[field]])))
stopifnot(all(lottery$se > 0 & lottery$fs_se > 0 & lottery$n_obs > 0))
lottery_stars <- function(p) ifelse(p < .01, '***', ifelse(p < .05, '**', ifelse(p < .10, '*', '')))
lottery_num <- function(x) format(x, big.mark = ',', trim = TRUE, scientific = FALSE)
lottery_table <- function(first_stage = TRUE) {
  body <- vapply(lottery_groups, function(g) {
    z <- lottery[sample == g]
    z <- z[match(lottery_specs$spec, z$spec)]
    b <- if (first_stage) z$fs_beta else z$beta
    se <- if (first_stage) z$fs_se else z$se
    p <- if (first_stage) z$fs_p_value else z$p_value_equal_zero
    cells <- paste0(sprintf('%.3f', b), lottery_stars(p),
      '<br><small>(', sprintf('%.3f', se), ')</small>',
      '<br><small>N = ', lottery_num(z$n_obs), '</small>',
      if (first_stage) paste0('<br><small>F = ', sprintf('%.1f', z$fs_f), '</small>') else '')
    paste0('<tr><th>', lottery_labels[g], '</th>', paste0('<td>', cells, '</td>', collapse = ''), '</tr>')
  }, character(1))
  paste0('<div class="table"><table class="lottery"><thead><tr><th>Sample</th>',
    paste0('<th>', esc(lottery_specs$label), '</th>', collapse = ''),
    '</tr></thead><tbody>', paste0(body, collapse = ''), '</tbody></table></div>')
}
lottery_html <- paste0('<section id="lottery-results"><h2>Lottery results</h2>',
  '<p>The pooled row reproduces the paper. The other rows estimate the same specifications separately for school-ending and voluntary applicants.</p>',
  '<h3>First stage: offered-school VA predicts attended-school VA</h3>', lottery_table(TRUE),
  '<h3>Second stage: lottery IV pass-through coefficient</h3>', lottery_table(FALSE),
  '<p class="notes">Parentheses contain the paper\'s heteroskedasticity-robust standard errors. N is the outcome-specific estimation sample; F is the single-instrument robust first-stage F statistic. Stars: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.10.</p>',
  '<p class="notes">Both stages use timely SAE applicants with assignment risk in 2018-2020 and the unchanged 2017-2020 EB school VAs. The second stage instruments attended-school VA with first-round offered-school VA. Both stages control for expected VA for the outcome, cohort indicators, grade-4 math and verbal scores, gender indicators and age indicators. Outcome-specific restrictions and no-offer instrument values match the paper.</p>',
  '<p class="notes">The pooled paper sample includes eligible applicants with unresolved/other origin offerings; these are absent from the two subgroup rows. The pooled row therefore need not equal a combination of those two groups. Differences in subgroup point estimates alone do not establish statistically significant differences between groups.</p>',
  '<p class="notes">All 42 pooled coefficient, standard-error, F and N checks reproduce the paper. ',
  '<a href="../empirical_bayes_school_va/main_by_continuation.html">Additional continuation-group lottery diagnostics</a>.</p></section>')
fwrite(lottery, file.path(clean, 'lottery_results.csv'))
fwrite(pooled_check, file.path(clean, 'lottery_paper_replication.csv'))
fwrite(data.table(check = c('Pooled paper statistics reproduce', 'Existing lottery estimation checks pass', 'Three samples by seven outcomes'),
  passed = c(all(lottery_replication$passed), all(lottery_checks$passed), nrow(lottery) == 21L)),
  file.path(clean, 'lottery_report_verification.csv'))
