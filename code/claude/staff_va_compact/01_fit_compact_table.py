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
# Optional sensitivity: drop schools with fewer than MIN_VA pooled VA-sample students.
MIN_VA = int(os.environ.get('MIN_VA_STUDENTS', '0'))
SUFFIX = '' if MIN_VA == 0 else f'_minva{MIN_VA}'
OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_compact' + SUFFIX)
OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_compact' + SUFFIX)
os.makedirs(OUT_DATA, exist_ok=True); os.makedirs(OUT_TAB, exist_ok=True)

P = dict(
    predictors='data/clean/staff_lasso_va/school_predictors.csv',
    staff_idx='data/clean/staff_quality_va/school_staff_indices_and_measures.csv',
    lead_idx='data/clean/leadership_quality_va/leadership_indices_and_measures.csv',
    va='output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv')
md5 = {k: hashlib.md5(open(v, 'rb').read()).hexdigest() for k, v in P.items()}

OUTCOMES = [('z_year_math_max', 'Math'), ('z_year_leng_max', 'Language'),
            ('admission_exam_taker', 'Exam taking'), ('higher_ed_enrolled_m1', 'HE enrollment'),
            ('stem_enrollment_m1', 'STEM'), ('high_inst_m1', 'High-premium inst.'),
            ('log_program_income_full_clp_m1', 'Proj. income')]
FOCAL = [('teacher__log1p_per1000', 'Log(1 + teachers per 1,000 students)', 'Teachers'),
         ('teacher__balanced_index', 'Teacher qualification index', 'Teachers'),
         ('counselor__log1p_per1000', 'Log(1 + orientadores per 1,000 students)', 'Orientadores'),
         ('counselor__ROLE_SPECIFIC_QUALIFICATION', 'Orientation-specific qualification', 'Orientadores'),
         ('leadership__log1p_per1000', 'Log(1 + leaders per 1,000 students)', 'Leadership'),
         ('leadership__balanced_index', 'Leadership qualification index', 'Leadership'),
         ('school__composition_math_mean', 'Peer grade-4 math (mean)', 'Peers'),
         ('school__log_public_funding_level', 'Log public funding per student', 'Resources')]
feats = [f for f, _, _ in FOCAL]
STAFF = feats[:6]; SCHOOL = feats[6:]

x = pd.read_csv(P['predictors']).sort_values('RBD').reset_index(drop=True)
assert len(x) == 3682 and x.RBD.is_unique
s = pd.read_csv(P['staff_idx'], usecols=['RBD', 'ROLE', 'balanced_index'])
t = s[s.ROLE == 'teacher'].set_index('RBD').balanced_index
l = pd.read_csv(P['lead_idx'], usecols=['RBD', 'balanced_index']).set_index('RBD').balanced_index
assert t.index.is_unique and l.index.is_unique
x['teacher__balanced_index'] = x.RBD.map(t)
x['leadership__balanced_index'] = x.RBD.map(l)
# Staffing ratios are extremely right-skewed (skew 6-24; driven by schools with
# very few VA-sample students), so they enter as log(1 + staff per 1,000).
for r in ('teacher', 'counselor', 'leadership'):
    x[f'{r}__log1p_per1000'] = np.log1p(x[f'{r}__staff_per1000_va_students'])

context = ['school__log_va_students', 'school__has_tp_or_artistic'] + \
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
    for spec, fs in (('controls', []), ('staff', STAFF), ('full', feats)):
        X, names = design(fs)
        b, se, e = ols_hc1(X, yz)
        r2[spec] = 1 - (e @ e) / ((yz - yz.mean()) @ (yz - yz.mean()))
        if spec == 'full':
            for nm, bb, ss in zip(names, b, se):
                if nm in feats:
                    coef_rows.append(dict(OUTCOME=o, FEATURE=nm, BETA_SD=bb, SE_HC1=ss))
            Xfull = X
    summ_rows.append(dict(OUTCOME=o, LABEL=olab, N=int(idx.sum()), P_FULL=Xfull.shape[1],
                          R2_CONTROLS=r2['controls'], R2_STAFF=r2['staff'], R2_FULL=r2['full']))

coef = pd.DataFrame(coef_rows); summ = pd.DataFrame(summ_rows)
coef.to_csv(os.path.join(OUT_DATA, 'coefficients.csv'), index=False)
summ.to_csv(os.path.join(OUT_DATA, 'model_summary.csv'), index=False)
cov = pd.DataFrame([dict(FEATURE=f, LABEL=lab, N_OBSERVED=int(np.isfinite(x[f]).sum())) for f, lab, _ in FOCAL])
cov.to_csv(os.path.join(OUT_DATA, 'focal_coverage.csv'), index=False)
assert md5 == {k: hashlib.md5(open(v, 'rb').read()).hexdigest() for k, v in P.items()}
json.dump(md5, open(os.path.join(OUT_DATA, 'source_md5.json'), 'w'), indent=1)

# ---- LaTeX table ----
L = [r'\begin{tabular}{l' + 'c' * len(OUTCOMES) + '}', r'\toprule',
     ' & ' + ' & '.join(f'({i+1})' for i in range(len(OUTCOMES))) + r' \\',
     ' & ' + ' & '.join(lab for _, lab in OUTCOMES) + r' \\', r'\midrule']
prev = None
for f, lab, blk in FOCAL:
    if blk != prev:
        L.append(r'\multicolumn{' + str(len(OUTCOMES) + 1) + r'}{l}{\textit{' + blk + r'}} \\'); prev = blk
    c = coef[coef.FEATURE == f].set_index('OUTCOME')
    L.append(r'\quad ' + lab.replace('&', r'\&') + ' & ' + ' & '.join(f'{c.BETA_SD[o]:.3f}' for o, _ in OUTCOMES) + r' \\')
    L.append(' & ' + ' & '.join(f'({c.SE_HC1[o]:.3f})' for o, _ in OUTCOMES) + r' \\')
L.append(r'\midrule')
sm = summ.set_index('OUTCOME')
for col, lab in (('R2_CONTROLS', r'$R^2$: controls only'), ('R2_STAFF', r'$R^2$: + staff'),
                 ('R2_FULL', r'$R^2$: + peers, funding')):
    L.append(lab + ' & ' + ' & '.join(f'{sm[col][o]:.3f}' for o, _ in OUTCOMES) + r' \\')
L.append('Schools & ' + ' & '.join(f'{sm.N[o]:,}' for o, _ in OUTCOMES) + r' \\')
L += [r'\bottomrule', r'\end{tabular}']
open(os.path.join(OUT_TAB, 'staff_va_compact.tex'), 'w').write('\n'.join(L) + '\n')

wide = coef.assign(cell=lambda d: d.BETA_SD.map('{:.3f}'.format) + ' (' + d.SE_HC1.map('{:.3f}'.format) + ')') \
    .pivot(index='FEATURE', columns='OUTCOME', values='cell').loc[feats, [o for o, _ in OUTCOMES]]
wide.to_csv(os.path.join(OUT_TAB, 'staff_va_compact.csv'))
pd.set_option('display.width', 250)
print(wide.rename(columns=dict(OUTCOMES), index={f: l for f, l, _ in FOCAL}).to_string())
print(summ.round(3).to_string()); print(cov.to_string())
