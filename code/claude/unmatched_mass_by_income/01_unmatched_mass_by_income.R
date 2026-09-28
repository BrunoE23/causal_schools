# No-offer (unmatched) probability by income decile. Run from repository root.
# P(no offer)_i = 1 - sum_j p_ij = probability mass on the 'unmatched' row of DA_probs.
# Also reports how much this compresses E^m: E_uncond = (1 - P(no offer)) * E_cond.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})

repo <- 'C:/Users/brunem/Research/causal_schools'   # absolute paths; cwd irrelevant
va_file <- file.path(repo, 'output/tables/empirical_bayes_school_va/stata_eb_school_values_math.csv')
stopifnot(file.exists(va_file))

roots <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'),
  'C:/Users/brunem/Box/causal_schools', 'C:/Users/brunem/Dropbox/causal_schools')
roots <- roots[nzchar(roots) & file.exists(file.path(roots, 'data/clean/univ_gr8_df.csv'))]
if (!length(roots)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the data root.')
clean <- file.path(roots[1], 'data/clean')
out_dir <- file.path(repo, 'output/tables/unmatched_mass_by_income')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Baseline: timely 2018-2020 SAE applicants (same frame as expected_va_sorting)
u <- fread(file.path(clean, 'univ_gr8_df.csv'),
  select = c('student_id', 'sae_proceso', 'timely_sae', 'income_decile_imputed'),
  na.strings = c('', 'NA'))
u <- u[timely_sae == 1L & sae_proceso %in% 2018:2020 & !is.na(student_id)]
u[, `:=`(student_id = as.numeric(student_id), sae_proceso = as.integer(sae_proceso),
  income_decile = as.integer(round(income_decile_imputed)))]

# Math EB VA, to show the compression of E
va <- fread(va_file,
  select = c('school_rbd', 'analysis_sample', 'va_eb_centered'))[analysis_sample == 'All']
va <- va[, .(school_rbd = as.numeric(school_rbd), va_math = va_eb_centered)]

prob <- rbindlist(lapply(2018:2020, function(y) {
  p <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
    select = c('student_id', 'school_id', 'prob'))
  p[, `:=`(student_id = as.numeric(student_id), sae_proceso = y,
    unmatched = tolower(trimws(school_id)) == 'unmatched',
    school_rbd = suppressWarnings(as.numeric(sub('_.*$', '', school_id))))]
  p[student_id %in% u[sae_proceso == y, student_id]]
}))
prob <- merge(prob, va, by = 'school_rbd', all.x = TRUE, sort = FALSE)

s <- prob[, .(
  total_mass     = sum(prob),
  p_nooffer      = sum(prob * unmatched),
  max_prob       = max(prob),                        # at risk if < 1
  e_math_uncond  = sum(prob * fifelse(unmatched, 0, va_math), na.rm = TRUE)
), by = .(student_id, sae_proceso)]
s[, e_math_cond := fifelse(p_nooffer < 1, e_math_uncond / (1 - p_nooffer), NA_real_)]
s[, at_risk := max_prob < 1 - 1e-9]

dt <- merge(u, s, by = c('student_id', 'sae_proceso'))
dt <- dt[abs(total_mass - 1) < 1e-6 & !is.na(income_decile)]
cat('N applicants:', nrow(dt), ' | at risk:', dt[, sum(at_risk)], '\n')

summ <- function(d) d[, .(
  n                = .N,
  mean_p_nooffer   = mean(p_nooffer),
  se_p_nooffer     = sd(p_nooffer) / sqrt(.N),
  share_any_risk   = mean(p_nooffer > 0),
  share_sure_none  = mean(p_nooffer > 1 - 1e-9),
  mean_e_math      = mean(e_math_uncond),
  mean_e_math_cond = mean(e_math_cond, na.rm = TRUE)
), keyby = income_decile]

res <- rbind(
  cbind(sample = 'All timely SAE applicants', summ(dt)),
  cbind(sample = 'At-risk applicants', summ(dt[at_risk == TRUE])))
fwrite(res, file.path(out_dir, 'unmatched_mass_by_income.csv'))
print(res, digits = 3)

# Slope of P(no offer) on decile, with robust SE
for (smp in c(FALSE, TRUE)) {
  d <- if (smp) dt[at_risk == TRUE] else dt
  m <- lm(p_nooffer ~ income_decile + factor(sae_proceso), data = d)
  b <- coef(summary(m))['income_decile', ]
  cat(if (smp) 'At-risk' else 'All', ': slope per decile =',
    sprintf('%.4f (se %.4f)', b[1], b[2]), '\n')
}

g <- ggplot(res, aes(income_decile, mean_p_nooffer, colour = sample)) +
  geom_pointrange(aes(ymin = mean_p_nooffer - 1.96 * se_p_nooffer,
                      ymax = mean_p_nooffer + 1.96 * se_p_nooffer),
                  position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3)) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = 'Household income decile', y = expression(1 - sum(p[ij], j)),
       colour = NULL, title = 'Probability of no first-round SAE offer') +
  theme_minimal(base_size = 12) + theme(legend.position = 'bottom')
ggsave(file.path(out_dir, 'unmatched_mass_by_income.png'), g, width = 7, height = 4.5, dpi = 300)
