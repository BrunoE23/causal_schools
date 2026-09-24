"""Administration-type differences in school Empirical-Bayes value added."""
from pathlib import Path
import math
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
PRED = ROOT / 'data/clean/staff_lasso_va/school_predictors.csv'
VA = ROOT / 'data/clean/staff_va_compact_inputs/eb_school_rbd_observational_values_cohorts_2017_2020.csv'
OUT = ROOT / 'output/tables/admin_type_va'
OUT.mkdir(parents=True, exist_ok=True)

OUTCOMES = [
    ('z_year_math_max', 'Math VA'),
    ('z_year_leng_max', 'Verbal VA'),
    ('high_inst_m1', 'HP inst. VA'),
    ('high_paying_field_m1', 'HP field VA'),
    ('log_program_income_clp_m1', 'Income VA'),
    ('admission_exam_taker', 'Exam VA'),
    ('any_postulacion', 'Fin. aid app. VA'),
]
GROUPS = [
    (2, 'Municipal'),
    (1, r'Corporaci\'on Municipal'),
    (3, r'Private subsidized'),
    (5, r'Delegated administration'),
    (6, r'SLEP'),
]

x = pd.read_csv(PRED).sort_values('RBD').reset_index(drop=True)
code = np.ones(len(x), dtype=int)
for k in (2, 3, 4, 5, 6):
    code[x[f'school__dependency_{k}'].eq(1).to_numpy()] = k
x['ADMIN'] = code

va = pd.read_csv(VA, usecols=['school_rbd', 'analysis_sample', 'outcome',
                              'controlled_value_added_eb_centered_student'])
va = va[va.analysis_sample.eq('All')]
rows = []
for outcome, _ in OUTCOMES:
    v = va[va.outcome.eq(outcome)].set_index('school_rbd').reindex(x.RBD)
    y = v.controlled_value_added_eb_centered_student.to_numpy()
    keep = np.isfinite(y) & (np.exp(x.school__log_va_students.to_numpy()) >= 100 - 1e-6) & (code != 4)
    yz = (y[keep] - np.mean(y[keep])) / np.std(y[keep], ddof=1)
    g = code[keep]
    assert len(yz) == 2058 and set(np.unique(g)) == {1, 2, 3, 5, 6}
    stats = {}
    for k, _ in GROUPS:
        a = yz[g == k]
        stats[k] = (len(a), np.mean(a), np.var(a, ddof=1))
    n0, m0, v0 = stats[2]
    for k, label in GROUPS:
        n, mean, var = stats[k]
        if k == 2:
            estimate, se = mean, math.sqrt(var / n)
        else:
            estimate, se = mean - m0, math.sqrt(var / n + v0 / n0)
        p = math.erfc(abs(estimate / se) / math.sqrt(2))
        star = '***' if p < .01 else '**' if p < .05 else '*' if p < .10 else ''
        rows.append(dict(outcome=outcome, admin=k, label=label, estimate=estimate,
                         se=se, p=p, stars=star, schools=n))

r = pd.DataFrame(rows)
r.to_csv(OUT / 'admin_type_va.csv', index=False)

def cell(d):
    stars = f"$^{{{d.stars}}}$" if d.stars else ''
    return f'{d.estimate:.3f}{stars}'

cols = [o for o, _ in OUTCOMES]
nc = len(cols)
lines = [
    r'\begin{table}[!htbp]', r'\centering',
    r'\caption{School value added by administration type}',
    r'\label{tab:admin-type-va}', r'\resizebox{\textwidth}{!}{%',
    r'\begin{tabular}{l' + 'c' * nc + 'r}', r'\toprule',
    ' & ' + ' & '.join(f'({j + 1})' for j in range(nc)) + r' & Schools \\',
    ' & ' + ' & '.join(label for _, label in OUTCOMES) + r' & \\', r'\midrule',
]
for k, label in GROUPS:
    if k == 1:
        lines.append(r'\multicolumn{' + str(nc + 2) +
                     r'}{l}{\textit{Differences relative to Municipal}} \\')
    z = r[r.admin.eq(k)].set_index('outcome').loc[cols]
    n = int(z.schools.iloc[0])
    lines.append(label + ' & ' + ' & '.join(cell(z.loc[o]) for o in cols) + f' & {n:,}' + r' \\')
    lines.append(' & ' + ' & '.join(f'({z.loc[o, "se"]:.3f})' for o in cols) + r' & \\')
lines += [
    r'\bottomrule', r'\end{tabular}}', r'\par\medskip', r'\footnotesize',
    r'\begin{minipage}{\textwidth}',
    'Notes: Municipal denotes the Municipal DAEM category. The first row reports its mean Empirical-Bayes value added. '
    'The remaining rows report differences relative to that group. Each value-added measure is standardized '
    'across the 2,058 public and private-subsidized schools with at least 100 students in the value-added sample. '
    'Standard errors in parentheses use the sampling variance of the corresponding group mean or difference in means. '
    'The table is descriptive and adds no controls beyond those used to construct value added; it does not adjust administration-type comparisons for region. '
    'Standard errors describe dispersion across schools and do not incorporate estimation error in school value added. '
    'Stars in the first row test equality to zero; stars in subsequent rows test equality to the Municipal mean. '
    r'* $p<0.10$, ** $p<0.05$, *** $p<0.01$.',
    r'\end{minipage}', r'\end{table}'
]
(OUT / 'admin_type_va.tex').write_text('\n'.join(lines) + '\n', encoding='utf-8')
print(r[['outcome', 'label', 'estimate', 'se', 'schools']].to_string(index=False))
