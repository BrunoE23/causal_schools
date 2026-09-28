# Descriptive: expected (E) and attended (A) school VA by household income decile,
# and by SAE low-income priority status (prioritario).
#   E_i^m = sum_j p_ij V_j^m  (no-offer probability gets VA 0, as in the IV)
#   A_i^m = V^m of the most-time high school after grade 8
# Timely 2018-2020 SAE grade-9 applicants; 7 main-table EB VA dimensions in own units.
# Run: source("C:/Users/brunem/Research/causal_schools/code/claude/expected_va_by_income/01_expected_va_by_income_decile.R")
suppressPackageStartupMessages({library(data.table); library(ggplot2)})

repo <- 'C:/Users/brunem/Research/causal_schools'
roots <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'),
  'C:/Users/brunem/Box/causal_schools', 'C:/Users/brunem/Dropbox/causal_schools')
roots <- roots[nzchar(roots) & file.exists(file.path(roots, 'data/clean/univ_gr8_df.csv'))]
if (!length(roots)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the data root.')
data_wd <- roots[1]; clean <- file.path(data_wd, 'data/clean')
out <- file.path(repo, 'output/tables/expected_va_by_income')
fig <- file.path(repo, 'output/figures/expected_va_by_income')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)

dims <- data.table(
  dkey = c('math', 'verbal', 'exam', 'hpinst', 'hpfield', 'income', 'benefits'),
  label = c('Math', 'Verbal', 'Exam taking', 'High-premium inst.',
            'High-premium field', 'Log projected income', 'Benefits/credit app.'),
  outcome = c('z_year_math_max', 'z_year_leng_max', 'admission_exam_taker',
    'high_inst_m1', 'high_paying_field_m1', 'log_program_income_clp_m1', 'any_postulacion'))

# ---- School VA (paper main table) ----
vr <- fread(file.path(clean, 'empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv'),
  na.strings = c('', 'NA'))
vr <- vr[analysis_sample == 'All' & outcome %chin% dims$outcome,
  .(school_rbd = as.numeric(school_rbd), outcome, v = controlled_value_added_eb_centered_student)]
V <- dcast(vr, school_rbd ~ outcome, value.var = 'v')
setnames(V, dims$outcome, paste0('v_', dims$dkey))
vcols <- paste0('v_', dims$dkey)

# ---- Applicants ----
u <- fread(file.path(clean, 'univ_gr8_df.csv'),
  select = c('student_id', 'mrun', 'sae_proceso', 'timely_sae', 'income_decile_imputed', 'most_time_RBD'),
  na.strings = c('', 'NA'))
u <- u[timely_sae == 1L & sae_proceso %in% 2018:2020 & !is.na(student_id) & !is.na(income_decile_imputed)]
u[, `:=`(student_id = as.numeric(student_id), mrun = as.numeric(mrun),
  sae_proceso = as.integer(sae_proceso), income_decile = as.integer(round(income_decile_imputed)),
  attended = as.numeric(most_time_RBD))]

# Prioritario flag from the public SAE applicant files (B1)
b1 <- rbindlist(lapply(2018:2020, function(y) {
  f <- Sys.glob(file.path(data_wd, 'data/raw', y, paste0('SAE_', y), 'B1_Postulantes_etapa_regular_*PUBL.csv'))
  if (!length(f)) return(NULL)
  x <- fread(f[1], select = c('mrun', 'cod_nivel', 'prioritario'), sep = ';', na.strings = c('', 'NA'))
  x[cod_nivel == 9, .(mrun = as.numeric(mrun), sae_proceso = y, prioritario = as.integer(prioritario))]
}))
has_prio <- nrow(b1) > 0
if (has_prio) {
  b1 <- unique(b1, by = c('mrun', 'sae_proceso'))
  u <- merge(u, b1, by = c('mrun', 'sae_proceso'), all.x = TRUE, sort = FALSE)
  message('Prioritario matched: ', u[, sum(!is.na(prioritario))], ' of ', nrow(u))
} else message('B1 files not found: skipping prioritario split.')

# ---- E from DA probabilities ----
prob <- rbindlist(lapply(2018:2020, function(y) {
  p <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
    select = c('student_id', 'school_id', 'prob'))
  p[, `:=`(student_id = as.numeric(student_id), sae_proceso = y,
    unmatched = tolower(trimws(school_id)) == 'unmatched',
    school_rbd = suppressWarnings(as.numeric(sub('_.*$', '', school_id))))]
  p[student_id %in% u[sae_proceso == y, student_id]]
}))
prob <- merge(prob, V, by = 'school_rbd', all.x = TRUE, sort = FALSE)
E <- prob[, c(list(total_mass = sum(prob),
    unknown_mass = sum(prob * (!unmatched & is.na(v_math)))),
  lapply(.SD, function(v) sum(prob * fifelse(unmatched, 0, v), na.rm = TRUE))),
  by = .(student_id, sae_proceso), .SDcols = vcols]
setnames(E, vcols, paste0('E_', dims$dkey))
rm(prob); gc(verbose = FALSE)

# ---- A: attended-school VA ----
A <- copy(V); setnames(A, vcols, paste0('A_', dims$dkey))
d <- merge(u, E, by = c('student_id', 'sae_proceso'))
d <- merge(d, A, by.x = 'attended', by.y = 'school_rbd', all.x = TRUE, sort = FALSE)
n0 <- nrow(d)
d <- d[abs(total_mass - 1) < 1e-6 & unknown_mass < 1e-12]
# Common sample: E and A observed for every dimension
d <- d[complete.cases(d[, c(paste0('E_', dims$dkey), paste0('A_', dims$dkey)), with = FALSE])]
message('Applicants with E: ', n0, ' | common E & A sample: ', format(nrow(d), big.mark = ','))

# ---- Long format and summaries ----
long <- melt(d, id.vars = intersect(c('student_id', 'income_decile', 'prioritario'), names(d)),
  measure.vars = c(paste0('E_', dims$dkey), paste0('A_', dims$dkey)), value.name = 'va')
long[, `:=`(series = fifelse(substr(variable, 1, 1) == 'E', 'Expected (offer)', 'Attended'),
            dkey = sub('^[EA]_', '', variable))]
long <- merge(long, dims[, .(dkey, label)], by = 'dkey')
long[, label := factor(label, levels = dims$label)]
summ <- function(dt, by) dt[, .(n = .N, mean = mean(va), se = sd(va) / sqrt(.N)), keyby = by]

by_dec <- summ(long, c('label', 'series', 'income_decile'))
fwrite(by_dec, file.path(out, 'va_by_income_decile.csv'))

# Wide table (E and A, D1..D10 and D10-D1)
w <- dcast(by_dec, series + income_decile ~ label, value.var = 'mean')
fwrite(w, file.path(out, 'va_by_income_decile_wide.csv'))

theme_set(theme_minimal(base_size = 11) + theme(legend.position = 'bottom',
  panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold')))
cols <- c('Expected (offer)' = '#1f77b4', 'Attended' = '#d62728')

# Pooled version (no prioritario split), kept as a backup
g1 <- ggplot(by_dec, aes(income_decile, mean, colour = series)) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey60') +
  geom_pointrange(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), size = 0.25,
                  position = position_dodge(0.5)) +
  facet_wrap(~label, scales = 'free_y', ncol = 4) +
  scale_x_continuous(breaks = 1:10) + scale_colour_manual(values = cols) +
  labs(x = 'Household income decile', y = 'Mean school VA (outcome units)', colour = NULL)
ggsave(file.path(fig, 'va_by_income_decile_pooled.png'), g1, width = 12, height = 6, dpi = 300)

# ---- Prioritario ----
if (has_prio && 'prioritario' %in% names(long)) {
  lp <- long[!is.na(prioritario)]
  lp[, prio := factor(fifelse(prioritario == 1, 'Prioritario', 'Not prioritario'),
                      levels = c('Not prioritario', 'Prioritario'))]
  share <- d[!is.na(prioritario), .(n = .N, share_prioritario = mean(prioritario)), keyby = income_decile]
  fwrite(share, file.path(out, 'prioritario_share_by_income_decile.csv'))

  by_prio <- summ(lp, c('label', 'series', 'prio'))
  fwrite(by_prio, file.path(out, 'va_by_prioritario.csv'))
  by_dec_prio <- summ(lp, c('label', 'series', 'income_decile', 'prio'))
  fwrite(by_dec_prio, file.path(out, 'va_by_income_decile_prioritario.csv'))

  # Main figure: per decile, E and A (colour) x prioritario (marker); 4 points per decile.
  # Cells with < 200 students dropped.
  pdat <- by_dec_prio[n >= 200]
  pdat[, grp := interaction(series, prio)]
  g2 <- ggplot(pdat, aes(income_decile, mean, colour = series, shape = prio, group = grp)) +
    geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey60') +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                  width = 0, linewidth = 0.3, position = position_dodge(0.6)) +
    geom_point(size = 1.8, stroke = 0.8, position = position_dodge(0.6)) +
    facet_wrap(~label, scales = 'free_y', ncol = 4) +
    scale_x_continuous(breaks = 1:10) + scale_colour_manual(values = cols) +
    scale_shape_manual(values = c('Not prioritario' = 16, 'Prioritario' = 2)) +
    labs(x = 'Household income decile', y = 'Mean school VA (outcome units)',
         colour = NULL, shape = NULL)
  ggsave(file.path(fig, 'va_by_income_decile.png'), g2, width = 12, height = 6.5, dpi = 300)
  print(share)
}
print(dcast(by_dec[series == 'Expected (offer)'], income_decile ~ label, value.var = 'mean'), digits = 3)
message('Outputs: ', out, ' and ', fig)
