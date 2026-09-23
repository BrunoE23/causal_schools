"""Compact staff/school characteristics vs school EB VA (8 focal rows).

Same estimator as code/codex/staff_ols_va/01_fit.R (unweighted school-level OLS,
median placeholders + missingness indicators, role absence/roster nuisance,
Y and X standardized by sample SD in the outcome sample, HC1 SEs), with a
different focal set and added dependency + region controls.
Run from repo root:  python code/claude/staff_va_compact/01_fit_compact_table.py
"""
import hashlib, json, os, sys
import numpy as np, pandas as pd

ROOT = os.getcwd()
# Main sample (Bruno, 2026-09-23): schools with >= 100 pooled VA-sample
# students (grade-8 cohorts 2017-2020). MIN_VA_STUDENTS=0 gives the all-schools
# robustness version.
MIN_VA = int(os.environ.get('MIN_VA_STUDENTS', '100'))
SUFFIX = '' if MIN_VA == 100 else ('_allschools' if MIN_VA == 0 else f'_minva{MIN_VA}')
OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_compact' + SUFFIX)
OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_compact' + SUFFIX)
os.makedirs(OUT_DATA, exist_ok=True); os.makedirs(OUT_TAB, exist_ok=True)

P = dict(
    predictors='data/clean/staff_lasso_va/school_predictors.csv',
    staff_idx='data/clean/staff_quality_va/school_staff_indices_and_measures.csv',
    lead_idx='data/clean/leadership_quality_va/leadership_indices_and_measures.csv',
    # Current paper VAs (Bruno, 2026-09-23): EB All-sample, grade-8 cohorts
    # 2017-2020, copied from Box data/clean/empirical_bayes_school_va/cohorts_2017_2020/.
    va='data/clean/staff_va_compact_inputs/eb_school_rbd_observational_values_cohorts_2017_2020.csv')
md5 = {k: hashlib.md5(open(v, 'rb').read()).hexdigest() for k, v in P.items()}

OUTCOMES = [('z_year_math_max', 'Math'), ('z_year_leng_max', 'Language'),
            ('admission_exam_taker', 'Exam taking'), ('any_postulacion', 'FUAS application'),
            ('high_paying_field_m1', 'High-premium field'), ('high_inst_m1', 'High-premium inst.'),
            ('log_program_income_clp_m1', 'Proj. income')]
FOCAL = [('teacher__per100_enrolled', 'Teachers per 100 students enrolled', 'Teachers'),
         ('teacher__balanced_index', 'Teacher qualification index', 'Teachers'),
         ('counselor__young_per100', 'Orientadores born 1977+ per 100 students', 'Orientadores'),
         ('counselor__young_trained_per100', 'Counseling-trained orientadores born 1977+ per 100', 'Orientadores'),
         ('counselor__old_per100', 'Orientadores born before 1977 per 100 students', 'Orientadores'),
         ('counselor__old_trained_per100', 'Counseling-trained orientadores born <1977 per 100', 'Orientadores'),
         ('leadership__per100_enrolled', 'Leaders per 100 students enrolled', 'Leadership'),
         ('leadership__balanced_index', 'Leadership qualification index', 'Leadership'),
         ('school__composition_math_mean', 'Peer grade-4 math (mean)', 'Size'),
         ('school__log_enrolled', 'Log students enrolled', 'Size'),
         ('school__has_tp', 'Technical-professional offering (0/1)', 'Track'),
         ('school__has_artistic', 'Artistic offering (0/1; 2 schools)', 'Track')]
# Track dummies are reported as 0/1 contrasts (Y SD units), not per predictor SD.
UNSTD = {'school__has_tp', 'school__has_artistic'}
feats = [f for f, _, _ in FOCAL]
STAFF = feats[:8]; SCHOOL = feats[8:10]; TRACK = feats[10:]

x = pd.read_csv(P['predictors']).sort_values('RBD').reset_index(drop=True)
assert len(x) == 3682 and x.RBD.is_unique
s = pd.read_csv(P['staff_idx'], usecols=['RBD', 'ROLE', 'balanced_index'])
t = s[s.ROLE == 'teacher'].set_index('RBD').balanced_index
l = pd.read_csv(P['lead_idx'], usecols=['RBD', 'balanced_index']).set_index('RBD').balanced_index
assert t.index.is_unique and l.index.is_unique
x['teacher__balanced_index'] = x.RBD.map(t)
x['leadership__balanced_index'] = x.RBD.map(l)
# Split HAS_TP_OR_ARTISTIC (ENS 410-810 or 910). The 2024 directory lists
# only RBD 320 and 8511 with 910; neither has a TP code (see inputs CSV).
art = pd.read_csv('data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv')
x['school__has_artistic'] = x.RBD.isin(art.RBD).astype(float)
x['school__has_tp'] = x.school__has_tp_or_artistic - x.school__has_artistic
assert set(x.school__has_tp.unique()) <= {0.0, 1.0}
# Staffing ratios: mean annual 2018-2024 headcount per 100 students enrolled,
# where enrollment = universe students (grade-8 cohorts 2017-2020) assigned to
# the school as most_time_RBD (4 cohorts ~ grades 9-12). Built by
# 00_hs_enrollment_from_universe.R. Schools absent from the universe file have
# missing ratios (handled by the missingness indicators).
enr_path = 'data/clean/staff_va_compact_inputs/hs_enrollment_universe_2017_2020.csv'
if not os.path.exists(enr_path):
    sys.exit('Run 00_hs_enrollment_from_universe.R first: ' + enr_path)
P['enrollment'] = enr_path
md5['enrollment'] = hashlib.md5(open(enr_path, 'rb').read()).hexdigest()
enr = pd.read_csv(enr_path).set_index('RBD').HS_ENROLLED_EST
assert enr.index.is_unique
x['hs_enrolled_est'] = x.RBD.map(enr)
for r in ('teacher', 'counselor', 'leadership'):
    den = x.hs_enrolled_est.where(x.hs_enrolled_est > 0)
    x[f'{r}__per100_enrolled'] = 100 * x[f'{r}__mean_headcount'] / den
    print(r, 'per 100 enrolled:', x[f'{r}__per100_enrolled'].describe(percentiles=[.01, .5, .99]).round(2).to_dict(),
          'skew', round(x[f'{r}__per100_enrolled'].skew(), 1))
# Orientador birth-cohort tiers (03_build_orientador_tiers.py). Schools with a
# known zero orientador headcount get zeros; unknown headcount stays missing.
tiers = pd.read_csv('data/clean/staff_va_compact_inputs/orientador_tiers_2018_2024.csv').set_index('RBD')
known0 = x.counselor__mean_headcount.notna()
for src, dst in (('counselor_young_headcount_mean', 'counselor__young_per100'),
                 ('counselor_young_trained_mean', 'counselor__young_trained_per100'),
                 ('counselor_old_headcount_mean', 'counselor__old_per100'),
                 ('counselor_old_trained_mean', 'counselor__old_trained_per100')):
    cnt = x.RBD.map(tiers[src]).where(known0 & x.RBD.isin(tiers.index), np.where(known0, 0.0, np.nan))
    x[dst] = 100 * cnt / x.hs_enrolled_est.where(x.hs_enrolled_est > 0)
tot = x.counselor__young_per100 + x.counselor__old_per100
assert np.allclose(tot.dropna(), x.counselor__per100_enrolled[tot.notna()])

# Peer composition is reported (Bruno, 2026-09-23): dropping it loads peer
# composition onto the staff, size and track rows. Log students enrolled
# (universe denominator) replaces log VA-sample size as the size measure.
x['school__log_enrolled'] = np.log(x.hs_enrolled_est.where(x.hs_enrolled_est > 0))
context = \
    [c for c in x.columns if c.startswith('school__dependency_') or c.startswith('school__region_')]
nuis = [f'{r}__{k}' for r in ('teacher', 'counselor', 'leadership') for k in ('absent_all_years', 'roster_incomplete')]
missing_cols = [c for c in feats + context + nuis if c not in x.columns]
assert not missing_cols, missing_cols

va = pd.read_csv(P['va'], usecols=['school_rbd', 'analysis_sample', 'outcome', 'controlled_value_added_centered_student',
                                   'controlled_value_added_se', 'controlled_value_added_eb_centered_student'])
va = va[va.analysis_sample == 'All']
assert not va.duplicated(['school_rbd', 'outcome']).any()


def ols_hc1(X, y):
    XtX_inv = np.linalg.inv(X.T @ X)
    b = XtX_inv @ X.T @ y
    e = y - X @ b
    n, k = X.shape
    V = n / (n - k) * XtX_inv @ (X * e[:, None]).T @ (X * e[:, None]) @ XtX_inv
    return b, np.sqrt(np.diag(V)), e


def drop_redundant(M, names, protect):
    keep = []
    for j in range(M.shape[1]):
        if np.linalg.matrix_rank(M[:, keep + [j]], tol=1e-9 * max(1, np.abs(M).max())) > len(keep):
            keep.append(j)
        elif names[j] in protect:
            sys.exit(f'focal not identified: {names[j]}')
    return keep


coef_rows, summ_rows = [], []
for o, olab in OUTCOMES:
    v = va[va.outcome == o].set_index('school_rbd').reindex(x.RBD)
    idx = np.isfinite(v.controlled_value_added_eb_centered_student.values)
    # Exclude Particular Pagado (COD_DEPE == 4; Bruno 2026-09-23): outside SAE
    # and public funding.
    idx &= (x.school__dependency_4.values != 1)
    if MIN_VA:
        idx &= (np.exp(x.school__log_va_students.values) >= MIN_VA - 1e-6)
    y_eb = v.controlled_value_added_eb_centered_student.values[idx]
    y_raw = v.controlled_value_added_centered_student.values[idx]
    se_raw = v.controlled_value_added_se.values[idx]
    xs = x.loc[idx]

    raw = xs[feats].to_numpy(float); raw[~np.isfinite(raw)] = np.nan
    mu, sig, med = np.nanmean(raw, 0), np.nanstd(raw, 0, ddof=1), np.nanmedian(raw, 0)
    miss = np.isnan(raw)
    for j in range(len(feats)): raw[miss[:, j], j] = med[j]
    unstd = np.array([f in UNSTD for f in feats])
    mu[unstd], sig[unstd] = 0.0, 1.0
    Z = (raw - mu) / sig
    Mi = miss.astype(float)
    N = xs[nuis].to_numpy(float); Nm = (~np.isfinite(N)).astype(float); N[~np.isfinite(N)] = 0
    C = xs[context].to_numpy(float); assert np.isfinite(C).all()

    blocks = dict(focal=(Z, feats), fmiss=(Mi, [f + '__missing' for f in feats]),
                  nuis=(N, nuis), nmiss=(Nm, [n + '__missing' for n in nuis]), ctx=(C, context))

    def design(focal_subset):
        cols, names = [np.ones((idx.sum(), 1))], ['(Intercept)']
        fi = [feats.index(f) for f in focal_subset]
        cols += [Z[:, fi], Mi[:, fi]]; names += focal_subset + [f + '__missing' for f in focal_subset]
        cols += [C, N, Nm]; names += context + nuis + [n + '__missing' for n in nuis]
        M = np.hstack(cols)
        keep = drop_redundant(M, names, set(focal_subset))
        return M[:, keep], [names[k] for k in keep]

    yz = (y_eb - y_eb.mean()) / y_eb.std(ddof=1)
    r2 = {}
    # Nested R2 (Bruno): controls only -> + size and track -> + staff -> + peers
    # (= full reported model).
    PEER = ['school__composition_math_mean']; SIZE = [f for f in SCHOOL if f not in PEER]
    for spec, fs in (('controls', []), ('sizetrack', SIZE + TRACK), ('staff', SIZE + TRACK + STAFF), ('full', feats)):
        X, names = design(fs)
        b, se, e = ols_hc1(X, yz)
        r2[spec] = 1 - (e @ e) / ((yz - yz.mean()) @ (yz - yz.mean()))
        if spec == 'full':
            for nm, bb, ss in zip(names, b, se):
                if nm in feats:
                    coef_rows.append(dict(OUTCOME=o, FEATURE=nm, BETA_SD=bb, SE_HC1=ss))
            Xfull = X
    summ_rows.append(dict(OUTCOME=o, LABEL=olab, N=int(idx.sum()), P_FULL=Xfull.shape[1],
                          R2_CONTROLS=r2['controls'], R2_SIZETRACK=r2['sizetrack'], R2_STAFF=r2['staff'], R2_FULL=r2['full']))

coef = pd.DataFrame(coef_rows); summ = pd.DataFrame(summ_rows)
coef.to_csv(os.path.join(OUT_DATA, 'coefficients.csv'), index=False)
summ.to_csv(os.path.join(OUT_DATA, 'model_summary.csv'), index=False)
cov = pd.DataFrame([dict(FEATURE=f, LABEL=lab, N_OBSERVED=int(np.isfinite(x[f]).sum())) for f, lab, _ in FOCAL])
cov.to_csv(os.path.join(OUT_DATA, 'focal_coverage.csv'), index=False)
assert md5 == {k: hashlib.md5(open(v, 'rb').read()).hexdigest() for k, v in P.items()}
json.dump(md5, open(os.path.join(OUT_DATA, 'source_md5.json'), 'w'), indent=1)

import math
def stars(b, se):
    # Two-sided normal p-value from the HC1 t-statistic (residual df > 2,000).
    p = math.erfc(abs(b / se) / math.sqrt(2))
    return '$^{***}$' if p < 0.01 else '$^{**}$' if p < 0.05 else '$^{*}$' if p < 0.1 else ''


# ---- LaTeX table (paper format; column order/labels follow the var/cov table) ----
PAPER_COLS = [('z_year_math_max', 'Math achievement'), ('z_year_leng_max', 'Verbal achievement'),
              ('high_inst_m1', 'High-premium institution'), ('high_paying_field_m1', 'High-premium field'),
              ('log_program_income_clp_m1', 'Log projected income'), ('admission_exam_taker', 'Admission exam taken'),
              ('any_postulacion', 'Any application (postulaci\\\'on)')]
PAPER_ROWS = {'teacher__per100_enrolled': 'Teachers per 100 students',
              'teacher__balanced_index': 'Teacher qualification index',
              'counselor__young_per100': 'Born 1977 or later, per 100 students',
              'counselor__young_trained_per100': '\\hspace{1em}Counseling-trained, born 1977 or later',
              'counselor__old_per100': 'Born before 1977, per 100 students',
              'counselor__old_trained_per100': '\\hspace{1em}Counseling-trained, born before 1977',
              'leadership__per100_enrolled': 'Leaders per 100 students',
              'leadership__balanced_index': 'Leadership qualification index',
              'school__composition_math_mean': 'Peer grade-4 math achievement',
              'school__log_enrolled': 'Log students enrolled',
              'school__has_tp': 'Technical-professional track (0/1)'}
BLOCKS = {'Teachers': 'Teachers', 'Orientadores': 'Counselors (orientadores)', 'Leadership': 'School leadership',
          'Size': 'Peers, size and track', 'Resources': None, 'Track': None}
SHORT = {'z_year_math_max': 'Math VA', 'z_year_leng_max': 'Verbal VA', 'high_inst_m1': 'HP inst. VA',
         'high_paying_field_m1': 'HP field VA', 'log_program_income_clp_m1': 'Income VA',
         'admission_exam_taker': 'Exam VA', 'any_postulacion': 'Fin. aid app. VA'}
nc = len(PAPER_COLS)
samp = ('public and private-subsidized schools with at least %d students in the value-added sample' % MIN_VA) if MIN_VA else 'all public and private-subsidized schools'
L = [r'\begin{table}[!htbp]', r'\centering', r'\caption{School staff, peers, size and track, and school value added}',
     r'\label{tab:staff-va' + ('' if MIN_VA == 100 else '-' + SUFFIX.strip('_')) + '}', r'\resizebox{\textwidth}{!}{%',
     r'\begin{tabular}{l' + 'c' * nc + '}', r'\toprule',
     ' & ' + ' & '.join(f'({i+1})' for i in range(nc)) + r' \\',
     ' & ' + ' & '.join(SHORT[o] for o, _ in PAPER_COLS) + r' \\', r'\midrule']
prev = None
for f, lab, blk in FOCAL:
    if f not in PAPER_ROWS:
        continue
    head = BLOCKS[blk]
    if head and head != prev:
        L.append(r'\multicolumn{' + str(nc + 1) + r'}{l}{\textit{' + head + r'}} \\'); prev = head
    c = coef[coef.FEATURE == f].set_index('OUTCOME')
    L.append(r'\quad ' + PAPER_ROWS[f] + ' & ' + ' & '.join(f'{c.BETA_SD[o]:.3f}' + stars(c.BETA_SD[o], c.SE_HC1[o]) for o, _ in PAPER_COLS) + r' \\')
    L.append(' & ' + ' & '.join(f'({c.SE_HC1[o]:.3f})' for o, _ in PAPER_COLS) + r' \\')
L.append(r'\midrule')
sm = summ.set_index('OUTCOME')
for col, lab in (('R2_CONTROLS', r'$R^2$: controls only'), ('R2_SIZETRACK', r'$R^2$: adding size and track'),
                 ('R2_STAFF', r'$R^2$: adding staff'), ('R2_FULL', r'$R^2$: adding peers (full model)')):
    L.append(lab + ' & ' + ' & '.join(f'{sm[col][o]:.3f}' for o, _ in PAPER_COLS) + r' \\')
L.append('Schools & ' + ' & '.join(f'{sm.N[o]:,}' for o, _ in PAPER_COLS) + r' \\')
L += [r'\bottomrule', r'\end{tabular}}', r'\par\medskip', r'\footnotesize', r'\begin{minipage}{\textwidth}',
      'Notes: Each column is a school-level OLS regression of the All-sample Empirical-Bayes school value added for the '
      'indicated outcome (grade-8 cohorts 2017--2020) on the listed characteristics, estimated on ' + samp + '. '
      'Both value added and continuous regressors are standardized by their sample standard deviations, so '
      'coefficients are in standard deviations of value added per standard deviation of the regressor; the '
      'technical-professional coefficient is the difference between schools with and without that track. '
      'Staff measures average 2018--2024 administrative staff records. Staffing ratios divide mean annual '
      'headcounts by the number of grade-8-cohort students assigned to the school. Qualification indices combine '
      'prior role experience, the current spell at the school, university qualification and teaching '
      'qualification. Counseling-trained counselors hold a counseling-specific postgraduate or post-degree '
      'certificate recorded in SIES graduation records by the staff year. All regressions control for '
      'dependency, region and an artistic-track indicator, and include indicators for missing '
      'regressors, absent roles and incomplete staff rosters. Heteroskedasticity-robust (HC1) standard errors '
      'in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$. (1) Math achievement; (2) Verbal achievement; (3) High-premium institution; '
      '(4) High-premium field; (5) Log projected income; (6) Admission exam taken; (7) Any application (postulaci\\\'on).',
      r'\end{minipage}', r'\end{table}']
open(os.path.join(OUT_TAB, 'staff_va_compact.tex'), 'w').write('\n'.join(L) + '\n')

wide = coef.assign(cell=lambda d: d.BETA_SD.map('{:.3f}'.format) + ' (' + d.SE_HC1.map('{:.3f}'.format) + ')') \
    .pivot(index='FEATURE', columns='OUTCOME', values='cell').loc[feats, [o for o, _ in OUTCOMES]]
wide.to_csv(os.path.join(OUT_TAB, 'staff_va_compact.csv'))
pd.set_option('display.width', 250)
print(wide.rename(columns=dict(OUTCOMES), index={f: l for f, l, _ in FOCAL}).to_string())
print(summ.round(3).to_string()); print(cov.to_string())
