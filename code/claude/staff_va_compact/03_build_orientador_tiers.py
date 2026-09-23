"""Orientador headcounts and counseling-trained headcounts by birth-cohort tier.

Tiers (Bruno, 2026-09-23): born >= 1977 (counseling training essentially fully
observed in SIES, since it happens mostly after age 30 and records start 2007)
and born < 1977 (pre-2007 training invisible).
Counseling-trained = holds a counseling-specific NON-undergraduate award
(SIES postitulo or postgraduate with ORIENTADOR_RELEVANT == 1) with
ASOF_YEAR <= staff year. Undergraduate counseling awards (45 records) excluded.
Unmatched staff are observed zero (correct for post-2007 awards).
School measure = mean over 2018-2024 of annual person counts (missing school-
years count as 0, reproducing counselor__mean_headcount exactly; roster
incompleteness is handled by the existing nuisance controls).
Output: data/clean/staff_va_compact_inputs/orientador_tiers_2018_2024.csv
"""
import numpy as np, pandas as pd
L = 'data/clean/titulados_staff_linkage/'
CUT = 1977
m = pd.read_csv(L + 'staff_membership_person_school_year.csv.gz')
m = m[(m.ROLE == 'Orientadores') & m.AGNO.between(2018, 2024)]
assert not m.duplicated(['MRUN', 'RBD', 'AGNO']).any()
p = pd.read_csv(L + 'credentials/staff_credentials_person_role_year.csv.gz', usecols=['MRUN', 'ROLE', 'STAFF_BIRTH_YM'])
p = p[p.ROLE == 'Orientadores']
by = pd.to_numeric(p.STAFF_BIRTH_YM.astype(str).str[:4], errors='coerce').groupby(p.MRUN).agg(['min', 'max'])
assert (by['min'] == by['max']).all() and by['min'].notna().all()
m['BY'] = m.MRUN.map(by['min']); assert m.BY.notna().all()
e = pd.read_csv(L + 'credentials/credential_award_evidence.csv.gz', usecols=['MRUN', 'LEVEL', 'ORIENTADOR_RELEVANT', 'ASOF_YEAR'])
e = e[(e.ORIENTADOR_RELEVANT == 1) & (e.LEVEL != 'undergraduate')]
first = e.groupby('MRUN').ASOF_YEAR.min()          # earliest qualifying as-of year
m['TRAINED'] = (m.MRUN.map(first) <= m.AGNO).astype(int)
m['YOUNG'] = (m.BY >= CUT).astype(int)

rows = []
for (tier, flag) in (('young', 1), ('old', 0)):
    s = m[m.YOUNG == flag]
    n = s.groupby(['RBD', 'AGNO']).MRUN.nunique()
    t = s[s.TRAINED == 1].groupby(['RBD', 'AGNO']).MRUN.nunique()
    rows.append((tier, n, t))
rbd = sorted(m.RBD.unique())
out = pd.DataFrame({'RBD': rbd})
for tier, n, t in rows:
    for lab, ser in (('headcount', n), ('trained', t)):
        w = ser.unstack(fill_value=0).reindex(index=rbd, columns=range(2018, 2025), fill_value=0)
        out[f'counselor_{tier}_{lab}_mean'] = w.mean(1).values
out['counselor_total_headcount_mean'] = out.counselor_young_headcount_mean + out.counselor_old_headcount_mean
out.to_csv('data/clean/staff_va_compact_inputs/orientador_tiers_2018_2024.csv', index=False)
print('schools with any orientador:', len(out))
print('person-years young share:', m.YOUNG.mean().round(3), ' trained share young/old:',
      m.groupby('YOUNG').TRAINED.mean().round(3).to_dict())
print(out.describe().T[['mean', '50%', 'max']].round(3))
