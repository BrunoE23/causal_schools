"""Check: the numpy estimator reproduces code/codex/staff_ols_va (R) coefficients/SEs
when given the same 14 focal variables and no dependency/region controls."""
import numpy as np, pandas as pd
feats = ['teacher__va_students_per_staff','teacher__UG_HIGH_PREMIUM','teacher__age_under35_share',
  'counselor__staff_per1000_va_students','counselor__ROLE_SPECIFIC_QUALIFICATION',
  'leadership__primary_headcount_mean','leadership__ANY_HIGH_PREMIUM','leadership__ROLE_SPECIFIC_MAGISTER',
  'school__composition_math_mean','school__composition_math_sd','school__composition_income_decile_mean',
  'school__log_public_funding_level','school__log_va_students','school__has_tp_or_artistic']
nuis = [f'{r}__{k}' for r in ('teacher','counselor','leadership') for k in ('absent_all_years','roster_incomplete')]
x = pd.read_csv('data/clean/staff_lasso_va/school_predictors.csv').sort_values('RBD').reset_index(drop=True)
va = pd.read_csv('output/tables/empirical_bayes_school_va/stata_eb_school_rbd_observational_values_for_iv.csv',
                 usecols=['school_rbd','analysis_sample','outcome','controlled_value_added_eb_centered_student'])
va = va[va.analysis_sample=='All']
ref = pd.read_csv('data/clean/staff_ols_va/coefficients.csv'); ref = ref[ref.FOCAL]
worst = 0
for o in ref.OUTCOME.unique():
    y = va[va.outcome==o].set_index('school_rbd').reindex(x.RBD).controlled_value_added_eb_centered_student.values
    idx = np.isfinite(y); y = y[idx]; xs = x.loc[idx]
    raw = xs[feats].to_numpy(float); raw[~np.isfinite(raw)] = np.nan
    mu, sig, med = np.nanmean(raw,0), np.nanstd(raw,0,ddof=1), np.nanmedian(raw,0)
    m = np.isnan(raw)
    for j in range(len(feats)): raw[m[:,j],j] = med[j]
    N = xs[nuis].to_numpy(float); Nm = (~np.isfinite(N)).astype(float); N[~np.isfinite(N)] = 0
    M = np.hstack([np.ones((len(y),1)), (raw-mu)/sig, m.astype(float), N, Nm])
    keep = []
    for j in range(M.shape[1]):
        if np.linalg.matrix_rank(M[:, keep+[j]], tol=1e-9*max(1,np.abs(M).max())) > len(keep): keep.append(j)
    X = M[:, keep]; yz = (y-y.mean())/y.std(ddof=1)
    A = np.linalg.inv(X.T@X); b = A@X.T@yz; e = yz-X@b
    V = len(yz)/(len(yz)-X.shape[1]) * A@(X*e[:,None]).T@(X*e[:,None])@A
    r = ref[ref.OUTCOME==o].set_index('FEATURE').loc[feats]
    worst = max(worst, np.abs(b[1:15]-r.BETA_SD.values).max(), np.abs(np.sqrt(np.diag(V))[1:15]-r.SE_HC1.values).max())
print('max abs diff vs R (coef & SE, 12 outcomes x 14 focal):', worst)
assert worst < 1e-6
