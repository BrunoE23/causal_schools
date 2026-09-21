"""Independent prediction/aggregation checks and readable all-outcome tables."""
from pathlib import Path
import hashlib
import html
import json
import warnings

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'data/clean/staff_lasso_va'
TABLE = ROOT / 'output/tables/staff_lasso_va'
REPORT = ROOT / 'output/reports'
TABLE.mkdir(parents=True, exist_ok=True)
REPORT.mkdir(parents=True, exist_ok=True)


def close(a, b, atol=1e-9):
    np.testing.assert_allclose(np.asarray(a, dtype=float), np.asarray(b, dtype=float),
                               rtol=1e-8, atol=atol, equal_nan=True)


manifest = pd.read_csv(OUT / 'input_manifest.csv')
for row in manifest.itertuples():
    with open(row.PATH, 'rb') as source:
        assert hashlib.file_digest(source, 'md5').hexdigest() == row.MD5
paths = dict(zip(manifest.SOURCE, manifest.PATH))
x = pd.read_csv(OUT / 'school_predictors.csv').set_index('RBD')
dictionary = pd.read_csv(OUT / 'predictor_dictionary.csv').set_index('FEATURE')
outcomes = pd.read_csv(OUT / 'outcome_dictionary.csv')
performance = pd.read_csv(OUT / 'lasso_performance.csv')
coef = pd.read_csv(OUT / 'lasso_coefficients.csv')
outer_coef = pd.read_csv(OUT / 'lasso_outer_coefficients.csv.gz')
pred = pd.read_csv(OUT / 'lasso_oof_predictions.csv.gz')
analysis = pd.read_csv(OUT / 'school_va_analysis.csv.gz')
audit = pd.read_csv(OUT / 'lasso_fit_audit.csv')
curves = pd.read_csv(OUT / 'lasso_cv_curves.csv')
freq = pd.read_csv(OUT / 'lasso_selection_frequency.csv')
assert x.index.is_unique and len(x) == 3682 and len(dictionary) == 175
assert not pred.duplicated(['RBD', 'OUTCOME', 'SPEC', 'RULE']).any()
assert len(performance) == 48 and len(audit) == 288
assert (coef.SELECTED == coef.BETA_SD.ne(0)).all()
assert (coef.loc[coef.TRAIN_CONSTANT, 'BETA_SD'] == 0).all()

# Independently verify fee categories directly from the 2024 raw directory.
fee = pd.read_csv(paths['fee_directory'], sep=';', encoding='utf-8-sig',
                  usecols=['RBD', 'AGNO', 'PAGO_MATRICULA', 'PAGO_MENSUAL']).set_index('RBD')
fee = fee.loc[x.index]
assert fee.index.is_unique and fee.AGNO.eq(2024).all()
fee_categories = ['SIN INFORMACION', 'GRATUITO', '$1.000 A $10.000', '$10.001 A $25.000',
                  '$25.001 A $50.000', '$50.001 A $100.000', 'MAS DE $100.000']
for field, prefix in [('PAGO_MATRICULA', 'enrollment_fee'), ('PAGO_MENSUAL', 'monthly_fee')]:
    values = fee[field].str.strip().str.upper()
    assert values.isin(fee_categories).all()
    matrix = []
    for band in [2, 3, 4, 5, 6, 0]:
        key = f'school__{prefix}_' + ('unknown' if band == 0 else f'band_{band}')
        close(x[key], values.eq(fee_categories[band]).astype(int))
        matrix.append(x[key].to_numpy())
    close(np.array(matrix).sum(axis=0), values.ne('GRATUITO').astype(int))
print('Verified 12 fee indicators: free reference, distinct unknown category, raw directory matches.', flush=True)

# Verify all 24 credential aggregates and coverage features from person sources.
metrics = ['UG_HIGH_PREMIUM', 'ANY_POST_UG', 'POST_UG_HIGH_PREMIUM', 'ANY_HIGH_PREMIUM',
           'ROLE_SPECIFIC_QUALIFICATION', 'ANY_MAGISTER', 'MAGISTER_HIGH_PREMIUM',
           'ROLE_SPECIFIC_MAGISTER']
fields = metrics + ['MATCH_ANY_BY_STAFF_YEAR', 'MATCH_UNDERGRAD_BY_STAFF_YEAR', 'ANY_UNCOVERED_INSTITUTION']
members = pd.read_csv(paths['members'], dtype={'MRUN': str})
flags = pd.read_csv(paths['credentials'], usecols=['MRUN', 'ROLE', 'AGNO'] + fields, dtype={'MRUN': str})
sr = pd.read_csv(paths['staff_roster'])
lr = pd.read_csv(paths['leader_roster'])
for role, label, countcol in [('teacher', 'HS teachers', 'N_TEACHER'),
                              ('counselor', 'Orientadores', 'N_COUNSELOR'),
                              ('leadership', 'Leadership', 'N_ROLE')]:
    roster = (lr if role == 'leadership' else sr)[['RBD', 'AGNO', countcol]].rename(columns={countcol: 'N_ROLE'})
    m = members[members.ROLE.eq(label)]
    j = m.merge(flags[flags.ROLE.eq(label)], on=['MRUN', 'ROLE', 'AGNO'], how='left', validate='many_to_one')
    assert len(j) == len(m) and not j[fields].isna().any().any()
    ann = j.groupby(['RBD', 'AGNO'])[fields].mean().reset_index()
    ann = ann.merge(roster, on=['RBD', 'AGNO'], validate='one_to_one')
    ann = ann[ann.N_ROLE.gt(0) & ann.N_ROLE.notna()]
    per = ann.groupby('RBD')[fields].mean().reindex(x.index)
    nknown = roster.groupby('RBD').N_ROLE.count().reindex(x.index).fillna(0)
    per.loc[nknown.ne(7)] = np.nan
    for feature in fields:
        close(per[feature], x[f'{role}__{feature}'])
del members, flags, j
print('Verified all three roles: annual-to-period credentials and coverage.', flush=True)

# Original source VA must match exactly after its declared All-sample restriction.
source_va = pd.read_csv(paths['va'], usecols=['school_rbd', 'analysis_sample', 'outcome',
    'controlled_value_added_eb_centered_student'])
source_va = source_va[source_va.analysis_sample.eq('All')].rename(columns={
    'school_rbd': 'RBD', 'outcome': 'OUTCOME', 'controlled_value_added_eb_centered_student': 'SOURCE_Y'})
matched = analysis.merge(source_va[['RBD', 'OUTCOME', 'SOURCE_Y']], on=['RBD', 'OUTCOME'], validate='one_to_one')
close(matched.Y, matched.SOURCE_Y)


def preprocess(train, test):
    tr = train.to_numpy(float).copy(); te = test.to_numpy(float).copy()
    tr[~np.isfinite(tr)] = np.nan; te[~np.isfinite(te)] = np.nan
    trmiss = np.isnan(tr); temiss = np.isnan(te)
    with warnings.catch_warnings():
        warnings.simplefilter('ignore', RuntimeWarning)
        med = np.nanmedian(tr, axis=0)
    med[np.isnan(med)] = 0
    tr = np.where(trmiss, med, tr); te = np.where(temiss, med, te)
    tr = np.column_stack([tr, trmiss.astype(float)])
    te = np.column_stack([te, temiss.astype(float)])
    mu = tr.mean(axis=0); sd = tr.std(axis=0, ddof=0)
    keep = sd > 1e-12
    names = train.columns.tolist() + [v + '__missing' for v in train.columns]
    return (tr[:, keep] - mu[keep]) / sd[keep], (te[:, keep] - mu[keep]) / sd[keep], np.array(names)[keep]


max_prediction_error = 0.0
max_kkt = 0.0
for (outcome, spec), ca in coef.groupby(['OUTCOME', 'SPEC'], sort=False):
    a = analysis[analysis.OUTCOME.eq(outcome)].set_index('RBD').sort_index()
    features = dictionary.index[dictionary.ROLE.eq('school')].tolist() if spec == 'school_only' else dictionary.index.tolist()
    raw = x.loc[a.index, features]
    for fold in range(6):
        train = np.ones(len(a), dtype=bool) if fold == 0 else a.OUTER_FOLD.ne(fold).to_numpy()
        test = np.ones(len(a), dtype=bool) if fold == 0 else ~train
        ztr, zte, names = preprocess(raw.loc[train], raw.loc[test])
        yy = a.Y.to_numpy()[train]; ym = yy.mean(); ys = yy.std(ddof=0)
        yz = (yy - ym) / ys
        cc = ca if fold == 0 else outer_coef[outer_coef.OUTCOME.eq(outcome) & outer_coef.SPEC.eq(spec) & outer_coef.OUTER_FOLD.eq(fold)]
        for rule in ['one_se', 'minimum']:
            bet = cc[cc.RULE.eq(rule)].set_index('FEATURE').BETA_SD.reindex(names).to_numpy()
            fit_audit = audit[audit.OUTCOME.eq(outcome) & audit.SPEC.eq(spec) & audit.RULE.eq(rule) & audit.OUTER_FOLD.eq(fold)].iloc[0]
            # Both X and Y are train-centered, so the optimal intercept is zero.
            resid = yz - ztr @ bet
            grad = ztr.T @ resid / len(yy)
            violation = np.where(bet != 0, np.abs(grad - fit_audit.LAMBDA * np.sign(bet)),
                                 np.maximum(np.abs(grad) - fit_audit.LAMBDA, 0))
            kk = max(np.abs(resid.mean()), violation.max())
            max_kkt = max(max_kkt, kk)
            assert kk < 2e-4, (outcome, spec, fold, rule, kk)
            if fold:
                expected = ym + ys * (zte @ bet)
                saved = pred[pred.OUTCOME.eq(outcome) & pred.SPEC.eq(spec) & pred.RULE.eq(rule) & pred.OUTER_FOLD.eq(fold)].set_index('RBD').reindex(a.index[test])
                assert saved.PRED.notna().all()
                close(expected, saved.PRED, atol=1e-8)
                close(saved.Y, a.Y.loc[test]); close(saved.NULL_PRED, ym)
                max_prediction_error = max(max_prediction_error, float(np.max(np.abs(expected - saved.PRED))))
            curve = curves[curves.OUTCOME.eq(outcome) & curves.SPEC.eq(spec) & curves.OUTER_FOLD.eq(fold)].sort_values('INDEX')
            imin = curve.CV_MSE.idxmin()
            threshold = curve.loc[imin, 'CV_MSE'] + curve.loc[imin, 'CV_SE']
            chosen = curve.loc[imin] if rule == 'minimum' else curve[curve.CV_MSE.le(threshold)].iloc[0]
            close(chosen.LAMBDA, fit_audit.LAMBDA)
    print(f'Verified predictions, tuning rule and KKT: {outcome} / {spec}', flush=True)

for key, p in pred.groupby(['OUTCOME', 'SPEC', 'RULE']):
    row = performance[(performance[['OUTCOME', 'SPEC', 'RULE']] == pd.Series(key, index=['OUTCOME', 'SPEC', 'RULE'])).all(axis=1)].iloc[0]
    sse = ((p.Y - p.PRED) ** 2).sum()
    close(1 - sse / ((p.Y - p.Y.mean()) ** 2).sum(), row.R2_OOF)
    close(np.sqrt(sse / len(p)), row.RMSE_OOF)
    close(1 - sse / ((p.Y - p.NULL_PRED) ** 2).sum(), row.R2_VS_TRAIN_MEAN)
    assert row.N == len(p)
computed_freq = outer_coef.groupby(['OUTCOME', 'SPEC', 'RULE', 'FEATURE']).SELECTED.mean().rename('CHECK').reset_index()
fc = freq.merge(computed_freq, on=['OUTCOME', 'SPEC', 'RULE', 'FEATURE'], validate='one_to_one')
close(fc.SELECTION_FREQUENCY, fc.CHECK)
verification = dict(status='passed', schools=len(x), candidates=len(dictionary),
    performance_rows=len(performance), fitted_models_checked=len(audit),
    oof_prediction_rows=len(pred), max_prediction_reconstruction_error=max_prediction_error,
    max_independent_kkt_violation=max_kkt, source_hashes_unchanged=True)
(OUT / 'independent_verification.json').write_text(json.dumps(verification, indent=2), encoding='utf-8')

# Reader-facing tables. Coefficients are SD-per-SD; no post-selection significance tests.
role_names = {'teacher': 'HS teachers', 'counselor': 'Orientadores', 'leadership': 'Leadership', 'school': 'School context'}
ordered_outcomes = outcomes.OUTCOME.tolist()
short = ['Math', 'Language', 'Exam taking', 'HE enrollment', 'STEM', 'HP field', 'HP institution',
         'Income: full', 'Income: field', 'Income: institution', 'Program accreditation', 'Institution accreditation']
shortmap = dict(zip(ordered_outcomes, short))
rows = []
for feature in coef.FEATURE.unique():
    base = feature.removesuffix('__missing')
    info = dictionary.loc[base]
    missing = feature.endswith('__missing')
    label = info.LABEL + (' [missing/undefined]' if missing else '')
    if feature == 'school__dependency_4':
        label = 'Private paid school (dependency 4)'
    rows.append(dict(FEATURE=feature, LABEL=label, ROLE=info.ROLE,
                     BLOCK='missingness' if missing else info.BLOCK))
labels = pd.DataFrame(rows).set_index('FEATURE')
co = coef.merge(labels.reset_index(), on='FEATURE', validate='many_to_one')
co = co.merge(freq, on=['OUTCOME', 'SPEC', 'RULE', 'FEATURE'], validate='one_to_one')
co.to_csv(OUT / 'lasso_coefficients_labeled.csv', index=False)


def fmt(v):
    if v == 0:
        return '—'
    if abs(v) < .0005:
        return '+<0.001' if v > 0 else '−<0.001'
    return f'{v:+.3f}'.replace('-', '−')


def coefficient_table(rule, cols, group=None):
    sel = co[co.SPEC.eq('joint') & co.RULE.eq(rule) & co.OUTCOME.isin(cols)]
    selected = sel.groupby('FEATURE').SELECTED.any()
    sel = sel[sel.FEATURE.isin(selected[selected].index)]
    if group == 'substantive':
        sel = sel[~sel.BLOCK.isin(['coverage', 'missingness'])]
    elif group == 'coverage':
        sel = sel[sel.BLOCK.isin(['coverage', 'missingness'])]
    elif group in role_names:
        sel = sel[sel.ROLE.eq(group) & ~sel.BLOCK.isin(['coverage', 'missingness'])]
    if len(sel) == 0:
        return pd.DataFrame(columns=['Predictor'] + [shortmap[o] for o in cols])
    wide = sel.pivot(index='FEATURE', columns='OUTCOME', values='BETA_SD').reindex(columns=cols).fillna(0)
    order = {v: i for i, v in enumerate(dictionary.index)}
    wide = wide.loc[sorted(wide.index, key=lambda v: (['teacher', 'counselor', 'leadership', 'school'].index(labels.loc[v, 'ROLE']),
                                                     order[v.removesuffix('__missing')], v.endswith('__missing')))]
    pretty = wide.map(fmt)
    pretty.columns = [shortmap[o] for o in cols]
    pretty.insert(0, 'Predictor', [role_names[labels.loc[v, 'ROLE']] + ': ' + labels.loc[v, 'LABEL'] for v in wide.index])
    return pretty.reset_index(drop=True)


def md_table(frame):
    head = '| ' + ' | '.join(frame.columns) + ' |'
    line = '| ' + ' | '.join(['---'] + ['---:'] * (len(frame.columns) - 1)) + ' |'
    body = ['| ' + ' | '.join(str(v).replace('|', '/') for v in row) + ' |' for row in frame.itertuples(index=False, name=None)]
    return '\n'.join([head, line] + body)


def performance_table(rule):
    p = performance[performance.RULE.eq(rule)]
    wide = p.pivot(index='OUTCOME', columns='SPEC', values='R2_OOF').reindex(ordered_outcomes)
    j = p[p.SPEC.eq('joint')].set_index('OUTCOME').reindex(ordered_outcomes)
    return pd.DataFrame({'Outcome': [shortmap[v] for v in ordered_outcomes],
        'Schools': j.N.astype(int).map(lambda n: f'{n:,}').to_numpy(),
        'Selected': j.N_SELECTED.astype(int).to_numpy(),
        'School-only R²': wide.school_only.map(lambda n: f'{n:.3f}').to_numpy(),
        'Joint R²': wide.joint.map(lambda n: f'{n:.3f}').to_numpy(),
        'Gain in R²': (wide.joint-wide.school_only).map(lambda n: f'{n:+.3f}').to_numpy()})


notes = [
    'One Gaussian Lasso per saved All-sample EB VA outcome. All three staff groups enter jointly; schools receive equal weight.',
    'Main coefficients use the one-standard-error penalty. Each entry is outcome SD per predictor SD, conditional on all selected predictors. Dashes mean zero coefficients, not unavailable outcomes. No significance stars are used.',
    'Staff components and as-of credential shares refer to 2018–2024; observed career histories start in 2013. Teachers are HS-assigned classroom teachers. Orientadores and leaders can hold primary or secondary roles.',
    'The 175 candidates comprise 39 school, 43 teacher, 46 orientador and 47 leadership measures. Each also has a missingness indicator; training-constant columns are removed. Composite indices and alternative-history versions are excluded; broken post-2018 reported-tenure averages are excluded.',
    'School-only and joint models both include separate enrollment-fee and monthly-fee indicators from the 2024 MINEDUC directory. Free is the reference for each; paid bands are CLP 1,000–10,000, 10,001–25,000, 25,001–50,000, 50,001–100,000 and above 100,000. No information is a separate category. These self-reported bands are not exact prices; no midpoint, top-code amount or annual-cost scalar is assigned.',
    'Absent roles and incomplete rosters have explicit indicators. Undefined or missing characteristics use training-only median placeholders plus missingness indicators, not a claim of zero qualifications. Qualification database coverage and missing institution-premium coverage are separate predictors.',
    'Student/staff ratios use pooled VA-sample students and average annual staff headcount. They are not class sizes, annual HS enrollment ratios, FTE measures or counselor caseloads. No funding or 2024-only age measure is included.',
    'Prediction uses five outer school folds and five inner tuning folds. Every imputation, scaling step and penalty choice excludes the held-out outer schools. The school-only baseline is separately tuned on identical schools and folds. Its penalty range extends to zero after a boundary audit; joint-model minima were interior.',
    'R² is pooled out-of-fold 1 − SSE/SST; negative values are possible. Coefficients instead come from the separately tuned full-sample refit. The minimum-error penalty is a sensitivity; selection frequencies count the five outer fits, not independent replications.',
    'Lasso may select one of several correlated measures and omit another. A zero does not establish irrelevance; signs are conditional predictive associations, not causal hiring effects or validated staff quality.',
    'Validation treats the saved EB VA estimates and institution-premium definitions as fixed, without reestimating them in folds or propagating their uncertainty. Schools sharing staff are not grouped into common folds. These are held-out-school predictions of saved estimates, not forecasts validated on future cohorts.',
    'Independent checks reconstruct all role credential aggregates, all 240 outer-model predictions, all 288 fitted-model KKT conditions, tuning choices, selection frequencies and performance statistics. Raw inputs and source VA hashes are unchanged.'
]

md = ['# Staff characteristics and school VA: joint Lasso', '', '## Out-of-sample prediction', '',
      md_table(performance_table('one_se')), '', '## Main coefficients', '',
      'Standardized coefficients; one-standard-error penalty. Only predictors selected in at least one displayed outcome appear.', '']
for i in range(0, 12, 4):
    md += [f'### Outcomes {i+1}–{i+4}', '', md_table(coefficient_table('one_se', ordered_outcomes[i:i+4], 'substantive')), '']
md += ['## Coverage and missingness indicators', '', md_table(coefficient_table('one_se', ordered_outcomes, 'coverage')), '',
       '## Minimum-error penalty sensitivity', '', md_table(performance_table('minimum')), '',
       '## Definitions and interpretation', ''] + ['- ' + s for s in notes]
md += ['', 'Estimator documentation: [glmnet](https://glmnet.stanford.edu/articles/glmnet.html).', '']
(REPORT / 'staff_lasso_va.md').write_text('\n'.join(md), encoding='utf-8')

css = '''body{font:15px/1.5 system-ui,sans-serif;color:#23313f;background:#f7f8fa;margin:0;padding:30px}
main{max-width:1800px;margin:auto}h1{font-size:29px}h2{margin-top:38px}h3{margin-top:25px}
.tablewrap{overflow:auto;max-height:700px;border:1px solid #ccd4da;border-radius:6px;background:white;margin:15px 0 25px}
table{border-collapse:separate;border-spacing:0;font-variant-numeric:tabular-nums;width:100%;font-size:13px}
th,td{padding:9px 12px;border-bottom:1px solid #e1e7eb;text-align:right;white-space:nowrap}
thead th{position:sticky;top:0;background:#243f50;color:white;z-index:3;white-space:normal;min-width:92px}
td:first-child,th:first-child{text-align:left;position:sticky;left:0;min-width:280px;max-width:460px;white-space:normal}
td:first-child{background:white;z-index:1}thead th:first-child{z-index:4}tbody tr:nth-child(even) td{background:#f1f5f7}
.intro{max-width:1000px}.note{color:#526674}details{margin:18px 0}summary{cursor:pointer;font-weight:600}
li{margin-bottom:8px}nav a{margin-right:20px;color:#1b6274}a{color:#1b6274}p,li{max-width:1150px}
@media print{body{padding:0;background:white}.tablewrap{max-height:none;overflow:visible}th,td{font-size:9px;padding:4px}thead{display:table-header-group}details{display:block}}
'''


def ht(frame):
    return '<div class="tablewrap">' + frame.to_html(index=False, escape=True, border=0) + '</div>'


parts = ['<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
         '<title>Joint staff Lasso and school VA</title><style>' + css + '</style><main>',
         '<h1>Staff characteristics and school VA</h1>',
         '<p class="intro">Joint Lasso across 12 VA outcomes. Main specification: one-standard-error penalty. '
         'Teacher, orientador and leadership measures compete jointly with school characteristics. These are predictive associations, not causal effects.</p>',
         '<p class="intro">Both models include 2024 enrollment-fee and monthly-fee bands, with free as the reference and no information as a separate category. School-only excludes all staff measures.</p>',
         '<nav><a href="#prediction">Prediction</a><a href="#coefficients">Coefficients</a><a href="#sensitivity">Sensitivity</a><a href="#notes">Methods</a></nav>',
         '<h2 id="prediction">Out-of-sample prediction</h2>', ht(performance_table('one_se')),
         '<h2 id="coefficients">Selected coefficients</h2><p>Outcome SD per predictor SD. Dashes indicate not selected. '
         'Scroll horizontally to see all 12 outcomes; row labels and column headings stay visible.</p>']
for role, label in role_names.items():
    parts += ['<h3>' + html.escape(label) + '</h3>', ht(coefficient_table('one_se', ordered_outcomes, role))]
parts += ['<details><summary>Selected coverage and missingness indicators</summary>',
          ht(coefficient_table('one_se', ordered_outcomes, 'coverage')), '</details>',
          '<h2 id="sensitivity">Minimum-error penalty sensitivity</h2>', ht(performance_table('minimum')),
          '<details><summary>All selected minimum-error coefficients</summary>',
          ht(coefficient_table('minimum', ordered_outcomes)), '</details>',
          '<h2 id="notes">Definitions and interpretation</h2><ol>']
parts += ['<li>' + html.escape(s) + '</li>' for s in notes]
parts += ['</ol><p>Estimator: <a href="https://glmnet.stanford.edu/articles/glmnet.html">official glmnet documentation</a>.</p>',
          '<p class="note">Verified on 2026-09-20. Reproducible inputs, fold assignments, coefficients and predictions are saved separately.</p></main></html>']
(REPORT / 'staff_lasso_va.html').write_text('\n'.join(parts), encoding='utf-8')

# A full numeric selected-coefficient matrix and readable LaTeX panels.
for rule in ['one_se', 'minimum']:
    full = co[co.SPEC.eq('joint') & co.RULE.eq(rule)]
    wide = full.pivot(index='FEATURE', columns='OUTCOME', values='BETA_SD').reindex(columns=ordered_outcomes)
    wide = labels.join(wide).loc[wide.ne(0).any(axis=1)]
    wide.to_csv(OUT / f'joint_selected_coefficients_{rule}.csv')


def tex_escape(s):
    s = str(s).replace('—', '--').replace('−', '-').replace('²', '$^2$')
    for a, b in [('&', r'\&'), ('%', r'\%'), ('_', r'\_'), ('#', r'\#')]:
        s = s.replace(a, b)
    return s


tex = ['% Requires booktabs, longtable, array. Main one-SE standardized Lasso coefficients.',
       '% Rows are selected in at least one outcome of the panel. No inferential stars.']
for i in range(0, 12, 4):
    frame = coefficient_table('one_se', ordered_outcomes[i:i+4])
    tex += [r'\begin{small}', r'\begin{longtable}{p{6.5cm}*{4}{>{\raggedleft\arraybackslash}p{2cm}}}',
            r'\caption{Joint staff Lasso: outcomes ' + str(i+1) + '--' + str(i+4) + r'}\\', r'\toprule',
            ' & '.join(tex_escape(v) for v in frame.columns) + r' \\', r'\midrule\endhead']
    tex += [' & '.join(tex_escape(v) for v in row) + r' \\' for row in frame.itertuples(index=False, name=None)]
    tex += [r'\bottomrule', r'\end{longtable}', r'\end{small}', '']
(TABLE / 'staff_lasso_coefficients.tex').write_text('\n'.join(tex), encoding='utf-8')
print(json.dumps(verification, indent=2))
print(performance_table('one_se').to_string(index=False))
