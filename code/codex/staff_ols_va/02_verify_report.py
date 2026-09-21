"""Independent least-squares/HC1 checks and one readable fixed-model table."""
from pathlib import Path
from datetime import date
import hashlib
import html
import json
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'data/clean/staff_ols_va'
REPORT = ROOT / 'output/reports'
manifest = pd.read_csv(OUT / 'source_manifest.csv')
for row in manifest.itertuples():
    with open(row.PATH, 'rb') as f:
        assert hashlib.file_digest(f, 'md5').hexdigest() == row.MD5
paths = dict(zip(manifest.SOURCE, manifest.PATH))
x = pd.read_csv(paths['predictors']).set_index('RBD').sort_index()
dictionary = pd.read_csv(OUT / 'focal_dictionary.csv')
outcomes = pd.read_csv(OUT / 'outcome_dictionary.csv')
coef = pd.read_csv(OUT / 'coefficients.csv')
summary = pd.read_csv(OUT / 'model_summary.csv').set_index('OUTCOME')
normal = pd.read_csv(OUT / 'normalization.csv')
saved = pd.read_csv(OUT / 'school_predictions.csv.gz')
va = pd.read_csv(paths['va'], usecols=['school_rbd', 'analysis_sample', 'outcome', 'controlled_value_added_eb_centered_student'])
va = va[va.analysis_sample.eq('All')]
features = dictionary.FEATURE.tolist()
nuisance = [r + '__' + n for r in ['teacher', 'counselor', 'leadership'] for n in ['absent_all_years', 'roster_incomplete']]
assert len(features) == 14 and len(set(features)) == 14

def close(a, b, atol=1e-9):
    np.testing.assert_allclose(np.asarray(a, float), np.asarray(b, float), rtol=1e-7, atol=atol, equal_nan=True)

max_beta_error = max_se_error = 0.0
for outcome in outcomes.OUTCOME:
    y = va[va.outcome.eq(outcome)].set_index('school_rbd').controlled_value_added_eb_centered_student.reindex(x.index)
    y = y[np.isfinite(y)]
    raw = x.loc[y.index, features].replace([np.inf, -np.inf], np.nan)
    mu, sig, med = raw.mean(), raw.std(ddof=1), raw.median()
    n = normal[normal.OUTCOME.eq(outcome)].set_index('FEATURE').loc[features]
    close(n.CENTER, mu); close(n.SCALE, sig); close(n.MEDIAN, med)
    close(n.N_OBSERVED, raw.notna().sum())
    controls = x.loc[y.index, nuisance].replace([np.inf, -np.inf], np.nan)
    full = pd.concat([pd.Series(1., index=y.index, name='(Intercept)'),
        (raw.fillna(med)-mu)/sig, raw.isna().astype(float).add_suffix('__missing'),
        controls.fillna(0), controls.isna().astype(float).add_suffix('__missing')], axis=1)
    c = coef[coef.OUTCOME.eq(outcome)].set_index('FEATURE')
    assert set(features).issubset(c.index) and c.FOCAL.sum() == 14
    a = full[c.index].to_numpy()
    assert np.linalg.matrix_rank(a) == a.shape[1]
    inverse = np.linalg.pinv(a)
    # Every discarded column lies in retained nuisance/focal span.
    close(a @ (inverse @ full.to_numpy()), full.to_numpy(), atol=1e-8)
    yy = ((y-y.mean())/y.std(ddof=1)).to_numpy()
    b = np.linalg.lstsq(a, yy, rcond=None)[0]
    residual = yy-a @ b
    # SVD generalized inverse gives the same HC1 sandwich, independently of R's Cholesky.
    influence = inverse * residual[None, :]
    cov = (len(y)/(len(y)-a.shape[1])) * influence @ influence.T
    se = np.sqrt(np.maximum(np.diag(cov), 0))
    close(c.BETA_SD, b); close(c.SE_HC1, se)
    max_beta_error = max(max_beta_error, float(np.max(np.abs(c.BETA_SD-b))))
    max_se_error = max(max_se_error, float(np.max(np.abs(c.SE_HC1-se))))
    s = summary.loc[outcome]
    assert s.N == len(y) and s.P == a.shape[1]
    close(s.Y_MEAN, y.mean()); close(s.Y_SD, y.std(ddof=1))
    close(s.R2, 1-residual @ residual / (yy @ yy))
    close(s.ADJ_R2, 1-(residual @ residual/(len(y)-a.shape[1]))/(yy @ yy/(len(y)-1)))
    h = np.sum(a * inverse.T, axis=1)
    close(s.MAX_LEVERAGE, h.max())
    assert s.N_UNIT_LEVERAGE == np.sum(h > 1-1e-8)
    prediction = saved[saved.OUTCOME.eq(outcome)].set_index('RBD').loc[y.index]
    close(prediction.Y, y); close(prediction.FITTED, y.mean()+y.std(ddof=1)*(a @ b))

verification = dict(status='passed', models=12, focal_coefficients=168,
    max_coefficient_error=max_beta_error, max_hc1_se_error=max_se_error,
    original_outcome_samples_preserved=True, redundant_control_span_verified=True)
(OUT / 'verification.json').write_text(json.dumps(verification, indent=2), encoding='utf-8')

labels = ['Students per HS teacher', 'UG at high-premium institution (share)', 'Younger than 35 (share)',
    'Orientadores per 1,000 VA students', 'Orientation-specific qualification (share)',
    'Primary leadership headcount', 'Any degree at high-premium institution (share)',
    'Leadership-specific magister (share)', 'Baseline math: mean', 'Baseline math: within-school SD',
    'Household income: mean decile', 'Log public funding per student', 'Log VA-sample student count', 'TP/artistic offering']
short = ['Math', 'Language', 'Exam taking', 'HE enrollment', 'STEM', 'HP field', 'HP institution',
    'Income: full', 'Income: field', 'Income: institution', 'Program accreditation', 'Institution accreditation']
roles = {'teacher': 'HS teachers', 'counselor': 'Orientadores', 'leadership': 'Leadership', 'school': 'School'}
order = outcomes.OUTCOME.tolist()
rows = []
md_rows = []
for i, f in enumerate(features):
    cc = coef[coef.FEATURE.eq(f)].set_index('OUTCOME').loc[order]
    label = roles[dictionary.iloc[i].ROLE] + ': ' + labels[i]
    cells = [f'{b:.3f}<span class="se">({s:.3f})</span>' for b, s in zip(cc.BETA_SD, cc.SE_HC1)]
    rows.append('<tr><th scope="row">' + html.escape(label) + '</th>' + ''.join('<td>'+c+'</td>' for c in cells) + '</tr>')
    md_rows.append('| ' + label + ' | ' + ' | '.join(f'{b:.3f} ({s:.3f})' for b,s in zip(cc.BETA_SD,cc.SE_HC1)) + ' |')
for label, field in [('Schools', 'N'), ('R-squared (in-sample)', 'R2')]:
    values = summary.loc[order, field]
    cells = [f'{int(v):,}' if field == 'N' else f'{v:.3f}' for v in values]
    rows.append('<tr class="summary"><th scope="row">' + label + '</th>' + ''.join('<td>'+v+'</td>' for v in cells) + '</tr>')
    md_rows.append('| ' + label + ' | ' + ' | '.join(cells) + ' |')
notes = [
    'Each column is one unweighted joint OLS regression of saved All-sample empirical-Bayes school VA on all 14 displayed measures. No Lasso, selection, penalty or cross-validation is used here. Region, dependency and fees are not added.',
    'Coefficients are VA standard deviations per one observed predictor standard deviation, calculated within each outcome sample. Binary variables also use SD units, not a 0-to-1 contrast. Parentheses contain HC1 heteroskedasticity-robust standard errors. No significance stars are shown.',
    'Missing focal values use observed medians with missingness indicators. Role-absence, roster-incompleteness and unknown-control indicators enter as nuisance controls, not displayed. Constant/redundant nuisance terms are omitted without changing the regressor span. No outcome is imputed; all outcome samples are preserved.',
    'HC1 uses the OLS sandwich covariance with N/(N-P). Singleton nuisance cells have unit leverage, making HC3 undefined. R-squared is in-sample and should not be compared directly with the previous held-out Lasso R-squared.',
    'The shortlist was informed by the previous full-sample analysis. Standard errors are conditional/descriptive and do not adjust for variable selection, multiple testing, estimated-VA uncertainty or cross-school dependence. These are partial associations, not causal staff/input effects.',
    'Staff measures use 2018-2024. Composition describes the exact 757,999-student broad VA sample (2017-2020 grade-8 cohorts), not the entire school. Public funding requires complete 2017-2021 histories and is measured in real 2021 CLP before taking logs; it is not total expenditure. Staffing ratios use pooled VA counts and average headcounts, not class size or annual caseload.',
    'Absent-role credentials remain undefined rather than zero. Linked credential shares also depend on qualification-database coverage. Baseline household income retains existing observed/imputed values.'
]
css = '''body{font:15px/1.5 system-ui,sans-serif;color:#23313f;background:#f7f8fa;margin:0;padding:24px}main{max-width:1900px;margin:auto}h1{font-size:27px;margin-bottom:8px}p,li{max-width:1150px}.wrap{overflow:auto;max-height:82vh;border:1px solid #cbd5df;background:white;border-radius:6px}table{border-collapse:separate;border-spacing:0;width:100%;font-size:13px;font-variant-numeric:tabular-nums}th,td{padding:10px 12px;border-bottom:1px solid #e2e8ee;text-align:right;min-width:110px}thead th{position:sticky;top:0;background:#243f50;color:white;z-index:3;text-align:center}tr th:first-child{position:sticky;left:0;min-width:310px;max-width:360px;text-align:left;background:white;z-index:2;font-weight:500}thead tr th:first-child{background:#243f50;z-index:4}tbody tr:nth-child(even) td,tbody tr:nth-child(even) th{background:#f0f4f7}.se{display:block;font-size:12px;color:#617284}.summary td,.summary th{font-weight:600;border-top:2px solid #9eadb9}details{margin-top:22px}summary{cursor:pointer;font-weight:600}li{margin-bottom:8px}a{color:#17648c}.sub{color:#526674;margin-bottom:18px}'''
table = '<div class="wrap"><table><thead><tr><th>Measure</th>' + ''.join('<th>'+html.escape(s)+'</th>' for s in short) + '</tr></thead><tbody>' + '\n'.join(rows) + '</tbody></table></div>'
page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Staff and school characteristics versus VA: OLS</title><style>'+css+'</style></head><body><main><h1>Staff and school characteristics versus VA</h1><p class="sub">Joint OLS · 14 fixed measures · Standardized coefficients, robust standard errors in parentheses</p><p>All measures enter together. Scroll horizontally for all 12 outcomes. These are descriptive partial associations, not causal effects.</p>'+table+'<details open><summary>Definitions and model notes</summary><ol>'+''.join('<li>'+html.escape(n)+'</li>' for n in notes)+'</ol></details><p class="sub">Verified '+date.today().isoformat()+'; 12 regressions and 168 focal coefficients checked independently.</p></main></body></html>'
(REPORT / 'staff_ols_va.html').write_text(page,encoding='utf-8')
md = '# Staff and school characteristics versus VA: joint OLS\n\nStandardized coefficients (HC1 robust standard errors).\n\n| Measure | '+' | '.join(short)+' |\n| --- | '+' | '.join(['---:']*12)+' |\n'+'\n'.join(md_rows)+'\n\n## Notes\n\n'+'\n'.join('- '+n for n in notes)+'\n'
(REPORT / 'staff_ols_va.md').write_text(md,encoding='utf-8')
print(json.dumps(verification,indent=2))
print(summary[['N','R2','N_UNIT_LEVERAGE']].to_string())
