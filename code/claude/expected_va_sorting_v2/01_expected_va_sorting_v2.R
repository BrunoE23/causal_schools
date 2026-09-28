# Expected-VA sorting, second pass.
# Descriptive: who seeks / is offered / attends high-VA schools, and how much is geography.
#
# DVs (per VA dimension m):  first-choice VA (demand), E^m = sum_j p_ij V_j^m (offer),
#                            attended VA (realized).
# Dimensions: the 7 main-table EB VAs + non-math dimensions residualized on math VA
#             (school-level, student-weighted).
# Specs: S1 baseline (as v1) | S2 + extra predetermined covariates
#        S3 S2 + comuna-by-cohort FE | S4 S2 + middle-school FE
# SEs clustered by middle school.
#
# Run: source("C:/Users/brunem/Research/causal_schools/code/claude/expected_va_sorting_v2/01_expected_va_sorting_v2.R")
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(0); setFixest_nthreads(0)

repo <- 'C:/Users/brunem/Research/causal_schools'
roots <- c(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD'),
  'C:/Users/brunem/Box/causal_schools', 'C:/Users/brunem/Dropbox/causal_schools')
roots <- roots[nzchar(roots) & file.exists(file.path(roots, 'data/clean/univ_gr8_df.csv'))]
if (!length(roots)) stop('Set CAUSAL_SCHOOLS_DATA_WD to the data root.')
data_wd <- roots[1]; clean <- file.path(data_wd, 'data/clean')
out <- file.path(repo, 'output/tables/expected_va_sorting_v2')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
log_lines <- character()
say <- function(...) { m <- paste0(...); message(m); log_lines <<- c(log_lines, m) }
pick <- function(have, cands) { x <- intersect(cands, have); if (length(x)) x[1] else NA_character_ }

# ---- 1. School VA (paper main table: VA cohorts 2017-2020) -------------------
va_path <- file.path(clean, 'empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv')
if (!file.exists(va_path)) va_path <- file.path(clean, 'empirical_bayes_school_va/eb_school_rbd_observational_values.csv')
stopifnot(file.exists(va_path)); say('VA file: ', va_path)
dims <- data.table(
  dkey = c('math', 'verbal', 'exam', 'hpinst', 'hpfield', 'income', 'benefits'),
  label = c('Math', 'Verbal', 'Exam taking', 'HP institution', 'HP field',
            'Proj. income', 'Benefits app.'),
  outcome = c('z_year_math_max', 'z_year_leng_max', 'admission_exam_taker',
    'high_inst_m1', 'high_paying_field_m1', 'log_program_income_clp_m1', 'any_postulacion'))
vr <- fread(va_path, na.strings = c('', 'NA'))
vr <- vr[analysis_sample == 'All' & outcome %chin% dims$outcome]
wcol <- pick(names(vr), c('n_students_regression', 'n_students'))
vr <- vr[, .(school_rbd = as.numeric(school_rbd), outcome,
  v = controlled_value_added_eb_centered_student,
  w = if (is.na(wcol)) 1 else get(wcol))]
V <- dcast(vr, school_rbd ~ outcome, value.var = 'v')
setnames(V, dims$outcome, paste0('v_', dims$dkey), skip_absent = TRUE)
W <- vr[, .(w = max(w, na.rm = TRUE)), by = school_rbd]
V <- merge(V, W, by = 'school_rbd')
V[!is.finite(w) | is.na(w), w := 1]
stopifnot(all(paste0('v_', dims$dkey) %in% names(V)))

# Residualize non-math dimensions on math VA (student-weighted, school level)
nonmath <- setdiff(dims$dkey, 'math')
for (k in nonmath) {
  ok <- V[, !is.na(get(paste0('v_', k))) & !is.na(v_math)]
  f <- lm(as.formula(paste0('v_', k, ' ~ v_math')), data = V[ok], weights = w)
  V[ok, (paste0('v_', k, '_perp')) := resid(f)]
  say(sprintf('School-level slope of %s VA on math VA: %.3f (R2 %.3f)', k,
    coef(f)[2], summary(f)$r.squared))
}
vkeys <- c(dims$dkey, paste0(nonmath, '_perp'))
vlabels <- c(dims$label, paste0(dims$label[-1], ' \u22a5 math'))
vcols <- paste0('v_', vkeys)

# ---- 2. Students ----------------------------------------------------------------
hdr <- names(fread(file.path(clean, 'univ_gr8_df.csv'), nrows = 0))
opt <- list(
  comuna = pick(hdr, c('COD_COM_ALU', 'cod_com_alu')),
  ms_rbd = pick(hdr, c('most_time_rbd_middle', 'most_time_RBD_middle')),
  ind_f  = pick(hdr, c('father_indigenous_imputed', 'father_indigenous')),
  ind_m  = pick(hdr, c('mother_indigenous_imputed', 'mother_indigenous')),
  gpa_ms = pick(hdr, c('z_gpa_middle_mean')), att_ms = pick(hdr, c('z_att_middle_mean')),
  rural  = pick(hdr, c('RURAL_RBD', 'rural_rbd')),
  attended = pick(hdr, c('most_time_RBD')))
say('Optional universe columns found: ',
  paste(names(opt)[!is.na(unlist(opt))], unlist(opt)[!is.na(unlist(opt))], sep = '=', collapse = ', '))
base_cols <- c('student_id', 'mrun', 'sae_proceso', 'timely_sae', 'GEN_ALU',
  'z_sim_mat_4to', 'z_sim_leng_4to', 'income_decile_imputed',
  'father_educ_years_imputed', 'mother_educ_years_imputed')
u <- fread(file.path(clean, 'univ_gr8_df.csv'),
  select = unique(c(base_cols, na.omit(unlist(opt)))), na.strings = c('', 'NA'))
u <- u[timely_sae == 1L & sae_proceso %in% 2018:2020 & !is.na(student_id)]
u[, `:=`(student_id = as.numeric(student_id), mrun = as.numeric(mrun),
  sae_proceso = as.integer(sae_proceso),
  female = fifelse(GEN_ALU %in% 1:2, as.numeric(GEN_ALU == 2), NA_real_))]
for (nm in names(opt)) if (!is.na(opt[[nm]])) setnames(u, opt[[nm]], nm)
if ('ind_f' %in% names(u) || 'ind_m' %in% names(u))
  u[, indigenous := as.numeric(pmax(if ('ind_f' %in% names(u)) ind_f else 0,
                                    if ('ind_m' %in% names(u)) ind_m else 0, na.rm = TRUE) > 0)]
stopifnot(!anyDuplicated(u[, .(student_id, sae_proceso)]))
say('Timely SAE applicants 2018-2020: ', format(nrow(u), big.mark = ','))

# SAE applicant file (B1): priority flags and comuna, if available
b1 <- rbindlist(lapply(2018:2020, function(y) {
  f <- Sys.glob(file.path(data_wd, 'data/raw', y, paste0('SAE_', y), 'B1_Postulantes_etapa_regular_*PUBL.csv'))
  if (!length(f)) return(NULL)
  h <- names(fread(f[1], nrows = 0, sep = ';'))
  keep <- intersect(c('mrun', 'cod_nivel', 'prioritario', 'alto_rendimiento', 'cod_com'), h)
  x <- fread(f[1], select = keep, sep = ';', dec = ',', na.strings = c('', 'NA'))
  x <- x[cod_nivel == 9]; x[, sae_proceso := y]; x[, cod_nivel := NULL]; x
}), fill = TRUE)
if (nrow(b1)) {
  b1[, mrun := as.numeric(mrun)]
  b1 <- unique(b1, by = c('mrun', 'sae_proceso'))
  u <- merge(u, b1, by = c('mrun', 'sae_proceso'), all.x = TRUE, sort = FALSE)
  if (!'comuna' %in% names(u) && 'cod_com' %in% names(u)) setnames(u, 'cod_com', 'comuna')
  say('B1 merged: prioritario non-missing for ', u[, sum(!is.na(prioritario))], ' applicants')
} else say('B1 applicant files not found; skipping prioritario / alto_rendimiento.')

# ---- 3. Application lists: first choice, list length ---------------------------
ae <- new.env(); load(file.path(clean, 'sae_grade9_unique_proceso.RData'), envir = ae)
apps <- as.data.table(ae$sae_apps_grade9)[sae_proceso %in% 2018:2020,
  .(mrun = as.numeric(mrun), sae_proceso = as.integer(sae_proceso),
    rank = as.integer(preferencia_postulante), rbd = as.numeric(rbd))]
apps <- unique(apps, by = c('mrun', 'sae_proceso', 'rank'))
lists <- apps[, .(list_len = uniqueN(rbd), first_rbd = rbd[which.min(rank)]),
  by = .(mrun, sae_proceso)]
apps_v <- merge(apps, V[, c('school_rbd', 'v_math'), with = FALSE],
  by.x = 'rbd', by.y = 'school_rbd', all.x = TRUE)
lists <- merge(lists, apps_v[, .(list_mean_math = mean(v_math, na.rm = TRUE),
  list_max_math = suppressWarnings(max(v_math, na.rm = TRUE))), by = .(mrun, sae_proceso)],
  by = c('mrun', 'sae_proceso'))
u <- merge(u, lists, by = c('mrun', 'sae_proceso'), all.x = TRUE, sort = FALSE)
if ('ms_rbd' %in% names(u)) u[, first_is_own_ms := as.numeric(first_rbd == ms_rbd)]

# ---- 4. DA probabilities -> E ----------------------------------------------------
prob <- rbindlist(lapply(2018:2020, function(y) {
  p <- fread(file.path(clean, 'DA_probs', paste0('DA_probs_', y, '.csv')),
    select = c('student_id', 'school_id', 'prob'))
  p[, `:=`(student_id = as.numeric(student_id), sae_proceso = y,
    unmatched = tolower(trimws(school_id)) == 'unmatched',
    school_rbd = suppressWarnings(as.numeric(sub('_.*$', '', school_id))))]
  p[student_id %in% u[sae_proceso == y, student_id]]
}))
prob <- merge(prob, V[, c('school_rbd', vcols), with = FALSE], by = 'school_rbd', all.x = TRUE, sort = FALSE)
agg <- prob[, c(list(total_mass = sum(prob), p_nooffer = sum(prob * unmatched),
  at_risk = as.numeric(max(prob) < 1 - 1e-9),
  unknown_mass = sum(prob * (!unmatched & is.na(v_math)))),
  lapply(.SD, function(v) sum(prob * fifelse(unmatched, 0, v), na.rm = TRUE))),
  by = .(student_id, sae_proceso), .SDcols = vcols]
setnames(agg, vcols, paste0('E_', vkeys))
rm(prob); gc(verbose = FALSE)
u <- merge(u, agg, by = c('student_id', 'sae_proceso'), all.x = TRUE, sort = FALSE)

# First-choice and attended VA
fc <- V[, c('school_rbd', vcols), with = FALSE]; setnames(fc, vcols, paste0('F_', vkeys))
u <- merge(u, fc, by.x = 'first_rbd', by.y = 'school_rbd', all.x = TRUE, sort = FALSE)
if ('attended' %in% names(u)) {
  at <- V[, c('school_rbd', vcols), with = FALSE]; setnames(at, vcols, paste0('A_', vkeys))
  u[, attended := as.numeric(attended)]
  u <- merge(u, at, by.x = 'attended', by.y = 'school_rbd', all.x = TRUE, sort = FALSE)
}
fams <- c(F = 'First-choice VA', E = 'Expected VA (E)', A = 'Attended VA')
fams <- fams[paste0(names(fams), '_math') %in% names(u)]

# ---- 5. Sample, scaling -----------------------------------------------------------
x_base <- c('z_sim_mat_4to', 'z_sim_leng_4to', 'income_decile_imputed',
  'mother_educ_years_imputed', 'father_educ_years_imputed', 'female')
x_extra <- intersect(c('indigenous', 'gpa_ms', 'att_ms'), names(u))
# SAE priority flags are mechanism inputs: they shift p_ij for a given list, so they
# are bad controls for the total SES gradient in E. Kept out of S1-S4; added in S5.
x_prio <- intersect(c('prioritario', 'alto_rendimiento'), names(u))
need <- c(x_base, x_extra, x_prio, 'comuna', 'ms_rbd',
  as.vector(outer(names(fams), vkeys, paste, sep = '_')))
need <- intersect(need, names(u))
d <- u[abs(total_mass - 1) < 1e-6 & unknown_mass < 1e-12]
say('With full probability mass and VA coverage: ', format(nrow(d), big.mark = ','))
d <- d[complete.cases(d[, ..need])]
say('Common complete sample (all DVs, covariates, geography): ', format(nrow(d), big.mark = ','))

d[, inc_dec_raw := round(income_decile_imputed)]
# Continuous covariates standardized; binary left as-is.
bin <- c('female', 'indigenous', 'prioritario', 'alto_rendimiento')
for (v in setdiff(c(x_base, x_extra, x_prio), bin)) d[, (v) := (get(v) - mean(get(v))) / sd(get(v))]
# DV scale: every family for dimension m divided by the SD of first-choice VA for m,
# so first-choice, E, and attended coefficients are in the same units and comparable.
for (k in vkeys) {
  s <- d[, sd(get(paste0('F_', k)))]
  for (f in names(fams)) d[, (paste0(f, '_', k)) := get(paste0(f, '_', k)) / s]
}
d[, coh_com := paste(sae_proceso, comuna)]

# ---- 6. Regressions -----------------------------------------------------------------
specs <- list(
  S1 = list(x = x_base, fe = 'sae_proceso', lab = 'Baseline'),
  S2 = list(x = c(x_base, x_extra), fe = 'sae_proceso', lab = '+ covariates'),
  S3 = list(x = c(x_base, x_extra), fe = 'coh_com', lab = '+ comuna x cohort FE'),
  S4 = list(x = c(x_base, x_extra), fe = 'sae_proceso + ms_rbd', lab = '+ middle-school FE'))
if (length(x_prio)) specs$S5 <- list(x = c(x_base, x_extra, x_prio), fe = 'coh_com',
  lab = 'S3 + SAE priority flags')
res <- list(); fit <- list()
for (f in names(fams)) for (s in names(specs)) {
  ys <- paste0(f, '_', vkeys)
  fml <- as.formula(paste0('c(', paste(ys, collapse = ','), ') ~ ',
    paste(specs[[s]]$x, collapse = ' + '), ' | ', specs[[s]]$fe))
  m <- feols(fml, data = d, cluster = ~ms_rbd, notes = FALSE)
  for (i in seq_along(ys)) {
    mi <- m[[i]]; ct <- coeftable(mi)
    res[[length(res) + 1]] <- data.table(family = f, spec = s, dim = vkeys[i],
      covariate = rownames(ct), beta = ct[, 1], se = ct[, 2], p = ct[, 4])
    fit[[length(fit) + 1]] <- data.table(family = f, spec = s, dim = vkeys[i],
      n = nobs(mi), r2 = r2(mi, 'r2'), within_r2 = r2(mi, 'wr2'))
  }
}
res <- rbindlist(res); fit <- rbindlist(fit)
fwrite(res, file.path(out, 'coefficients_long.csv')); fwrite(fit, file.path(out, 'fit_long.csv'))

# ---- 7. Diagnostics (not all for the table) -------------------------------------------
# 7a. Share of DV variance explained by geography alone
geo <- rbindlist(lapply(names(fams), function(f) rbindlist(lapply(vkeys, function(k) {
  y <- paste0(f, '_', k)
  data.table(family = f, dim = k,
    r2_comuna = r2(feols(as.formula(paste(y, '~ 1 | coh_com')), d, notes = FALSE), 'r2'),
    r2_ms     = r2(feols(as.formula(paste(y, '~ 1 | ms_rbd')), d, notes = FALSE), 'r2'),
    r2_covs   = r2(feols(as.formula(paste(y, '~', paste(c(x_base, x_extra), collapse = '+'), '| sae_proceso')), d, notes = FALSE), 'r2'))
}))))
fwrite(geo, file.path(out, 'variance_shares_geography.csv'))

# 7b. Attenuation of SES coefficients once geography is absorbed
ses <- c('income_decile_imputed', 'mother_educ_years_imputed', 'father_educ_years_imputed')
att <- dcast(res[covariate %in% c(ses, 'female', 'z_sim_mat_4to')],
  family + dim + covariate ~ spec, value.var = 'beta')
att[, `:=`(keep_S3 = S3 / S2, keep_S4 = S4 / S2)]
fwrite(att, file.path(out, 'geography_attenuation.csv'))

# 7c. Means by income decile (for the plot) and by gender
raw_dec <- u[d[, .(student_id, sae_proceso)], on = .(student_id, sae_proceso)]
dec <- d[, c(list(n = .N), lapply(.SD, mean)), keyby = .(income_decile = inc_dec_raw),
  .SDcols = as.vector(outer(names(fams), vkeys, paste, sep = '_'))]
fwrite(dec, file.path(out, 'means_by_income_decile.csv'))
gen <- d[, c(list(n = .N), lapply(.SD, mean)), keyby = female,
  .SDcols = as.vector(outer(names(fams), vkeys, paste, sep = '_'))]
fwrite(gen, file.path(out, 'means_by_gender.csv'))

# 7d. List features by decile
lf <- intersect(c('list_len', 'p_nooffer', 'at_risk', 'first_is_own_ms', 'list_mean_math',
  'list_max_math', 'prioritario'), names(raw_dec))
lstats <- raw_dec[, lapply(.SD, function(z) mean(as.numeric(z), na.rm = TRUE)),
  keyby = .(income_decile = round(income_decile_imputed)), .SDcols = lf]
fwrite(lstats, file.path(out, 'list_features_by_income_decile.csv'))

# 7e. How tightly does E track first choice? (student-level correlations)
cors <- rbindlist(lapply(vkeys, function(k) data.table(dim = k,
  cor_F_E = d[, cor(get(paste0('F_', k)), get(paste0('E_', k)))],
  cor_E_A = if ('A' %in% names(fams)) d[, cor(get(paste0('E_', k)), get(paste0('A_', k)))] else NA_real_,
  cor_F_A = if ('A' %in% names(fams)) d[, cor(get(paste0('F_', k)), get(paste0('A_', k)))] else NA_real_)))
fwrite(cors, file.path(out, 'dv_correlations.csv'))

# ---- 8. Main LaTeX table: panels F and E, spec S3, main dims (math, verbal, ⊥math others)
tab_dims <- c('math', 'verbal', 'exam_perp', 'hpinst_perp', 'hpfield_perp', 'income_perp', 'benefits_perp')
tab_labs <- vlabels[match(tab_dims, vkeys)]
rows_x <- intersect(c('income_decile_imputed', 'mother_educ_years_imputed', 'father_educ_years_imputed',
  'z_sim_mat_4to', 'z_sim_leng_4to', 'female', 'indigenous'), c(x_base, x_extra))
rlab <- c(income_decile_imputed = 'Income decile', mother_educ_years_imputed = "Mother's educ.",
  father_educ_years_imputed = "Father's educ.", z_sim_mat_4to = 'Grade-4 math',
  z_sim_leng_4to = 'Grade-4 verbal', female = 'Female', prioritario = 'SAE priority (low-income)',
  indigenous = 'Indigenous')
star <- function(p) fifelse(p < .01, '^{***}', fifelse(p < .05, '^{**}', fifelse(p < .1, '^{*}', '')))
panel <- function(f, title) {
  z <- res[family == f & spec == 'S3']
  c(sprintf('\\multicolumn{%d}{l}{\\textit{%s}} \\\\', length(tab_dims) + 1, title),
    unlist(lapply(rows_x, function(x) {
      r <- z[covariate == x][match(tab_dims, dim)]
      c(paste0(rlab[x], ' & ', paste0('$', sprintf('%.3f', r$beta), star(r$p), '$', collapse = ' & '), ' \\\\'),
        paste0(' & ', paste0('(', sprintf('%.3f', r$se), ')', collapse = ' & '), ' \\\\'))
    })),
    paste0('Within $R^2$ & ', paste(sprintf('%.3f', fit[family == f & spec == 'S3'][match(tab_dims, dim), within_r2]), collapse = ' & '), ' \\\\'))
}
tex <- c('\\begin{table}[!htbp]', '\\centering',
  '\\caption{Who seeks and who is offered high-value-added schools}',
  '\\label{tab:expected-va-sorting-v2}', '\\resizebox{\\textwidth}{!}{%',
  paste0('\\begin{tabular}{l', strrep('c', length(tab_dims)), '}'), '\\toprule',
  paste0(' & ', paste(tab_labs, collapse = ' & '), ' \\\\'), '\\midrule',
  panel('F', 'Panel A: VA of first-ranked school'), '\\midrule',
  panel('E', 'Panel B: Expected VA of the offer, $E_i^m$'),
  '\\midrule', paste0('N & \\multicolumn{', length(tab_dims), '}{c}{', format(nrow(d), big.mark = ','), '} \\\\'),
  '\\bottomrule', '\\end{tabular}', '}', '\\par\\medskip\\footnotesize',
  '\\begin{minipage}{\\textwidth}',
  paste0('Notes: Timely 2018--2020 SAE grade-9 applicants. Dependent variables are EB school VA ',
    'scaled by the SD of first-choice VA for that dimension, so Panels A and B are in common units. ',
    'Columns 3--7 use VA residualized on math VA across schools (student-weighted). Continuous ',
    'covariates are standardized. All columns include comuna-by-cohort fixed effects and the ',
    'additional covariates listed in the text; standard errors clustered by middle school. ',
    'Descriptive associations, not causal effects. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.'),
  '\\end{minipage}', '\\end{table}')
writeLines(tex, file.path(out, 'expected_va_sorting_v2.tex'))

# ---- 9. How much does SAE priority move the SES gradient? (S3 vs S5) -------------------
if ('S5' %in% names(specs)) {
  pc <- merge(
    res[spec == 'S3' & covariate %in% ses, .(family, dim, covariate, beta_S3 = beta, se_S3 = se)],
    res[spec == 'S5' & covariate %in% ses, .(family, dim, covariate, beta_S5 = beta, se_S5 = se)],
    by = c('family', 'dim', 'covariate'))
  pc[, share_via_priority := 1 - beta_S5 / beta_S3]
  pc <- merge(pc, dcast(res[spec == 'S5' & covariate %in% x_prio],
    family + dim ~ covariate, value.var = c('beta', 'se')), by = c('family', 'dim'), all.x = TRUE)
  fwrite(pc, file.path(out, 'priority_comparison.csv'))
  # Compact tex: income-decile coefficient without / with priority, and priority coefficient
  z <- pc[covariate == 'income_decile_imputed']
  prow <- function(f, lab) {
    r <- z[family == f][match(tab_dims, dim)]
    c(sprintf('\\multicolumn{%d}{l}{\\textit{%s}} \\\\', length(tab_dims) + 1, lab),
      paste0('Income decile, no priority control & ', paste(sprintf('%.3f', r$beta_S3), collapse = ' & '), ' \\\\'),
      paste0('Income decile, with priority & ', paste(sprintf('%.3f', r$beta_S5), collapse = ' & '), ' \\\\'),
      if ('beta_prioritario' %in% names(r))
        paste0('SAE priority (low-income) & ', paste(sprintf('%.3f', r$beta_prioritario), collapse = ' & '), ' \\\\'),
      if ('se_prioritario' %in% names(r))
        paste0(' & ', paste0('(', sprintf('%.3f', r$se_prioritario), ')', collapse = ' & '), ' \\\\'))
  }
  ptex <- c('\\begin{table}[!htbp]', '\\centering',
    '\\caption{SAE low-income priority and the income gradient in school VA}',
    '\\label{tab:expected-va-priority}', '\\resizebox{\\textwidth}{!}{%',
    paste0('\\begin{tabular}{l', strrep('c', length(tab_dims)), '}'), '\\toprule',
    paste0(' & ', paste(tab_labs, collapse = ' & '), ' \\\\'), '\\midrule',
    prow('F', 'Panel A: First-choice VA'), '\\midrule', prow('E', 'Panel B: Expected VA ($E_i^m$)'),
    '\\bottomrule', '\\end{tabular}', '}', '\\par\\medskip\\footnotesize',
    '\\begin{minipage}{\\textwidth}',
    'Notes: Same sample, scaling and controls as Table~\\ref{tab:expected-va-sorting-v2} (comuna-by-cohort FE). "With priority" adds the SAE low-income (prioritario) and high-achievement flags. Because priority shifts admission probabilities for a given list, the change in the income coefficient in Panel B measures how much of the income gradient in offered VA runs through the priority rules.',
    '\\end{minipage}', '\\end{table}')
  writeLines(ptex, file.path(out, 'expected_va_priority.tex'))
  say('Share of income gradient in E removed by priority controls (by dim): ',
    paste(z[family == 'E', sprintf('%s=%.2f', dim, share_via_priority)], collapse = ', '))
}
writeLines(log_lines, file.path(out, 'run_log.txt'))
say('Done. Outputs in ', out)
