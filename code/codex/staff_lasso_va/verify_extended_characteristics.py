"""Independent pandas checks of all added school characteristics."""
from pathlib import Path
import hashlib
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'data/clean/staff_lasso_va'

def close(a, b):
    np.testing.assert_allclose(np.asarray(a, float), np.asarray(b, float),
                               rtol=1e-9, atol=1e-8, equal_nan=True)

def verify():
    manifest = pd.read_csv(OUT / 'extended_source_manifest.csv')
    for row in manifest.itertuples():
        with open(row.PATH, 'rb') as f:
            assert hashlib.file_digest(f, 'md5').hexdigest() == row.MD5
    paths = dict(zip(manifest.SOURCE, manifest.PATH))
    x = pd.read_csv(OUT / 'extended_school_predictors.csv').set_index('RBD')
    u = pd.read_csv(OUT / 'composition_student_inputs.csv.gz')
    assert len(u) == 757999 and u.MRUN.is_unique
    counts = pd.read_csv(OUT / 'composition_sample_reconciliation.csv')
    close(counts.N_EXPECTED, counts.N_BUILT)
    g = u.groupby('RBD')
    close(g.size().reindex(x.index), counts.set_index('RBD').N_EXPECTED.reindex(x.index))
    sources = {'math': 'z_sim_mat_4to', 'language': 'z_sim_leng_4to'}
    expected = {}
    for name, source in sources.items():
        for stat in ['mean', 'std']:
            expected[name + ('_sd' if stat == 'std' else '_mean')] = g[source].agg(stat)
    for name, source in {'income_decile_mean': 'income_decile_imputed',
        'father_education': 'father_educ_years_imputed', 'mother_education': 'mother_educ_years_imputed',
        'grade8_age_mean': 'EDAD_ALU', 'income_imputed_share': 'income_mid_was_imputed',
        'father_education_imputed_share': 'father_educ_years_was_imputed',
        'mother_education_imputed_share': 'mother_educ_years_was_imputed'}.items():
        expected[name] = g[source].mean()
    expected['low_income_share'] = u.income_decile_imputed.le(2).groupby(u.RBD).mean()
    expected['high_income_share'] = u.income_decile_imputed.ge(9).groupby(u.RBD).mean()
    expected['female_share'] = u.GEN_ALU.eq(2).where(u.GEN_ALU.isin([1, 2])).groupby(u.RBD).mean()
    for name, values in expected.items():
        close(x['school__composition_' + name], values.reindex(x.index))
    del u

    fy = pd.read_csv(paths['funding_year'])
    fy = fy[fy.school_rbd.isin(x.index)].copy()
    fy['valid'] = (fy.n_months_observed.eq(12) & fy.n_month_rows.eq(12)
        & np.isfinite(fy.avg_monthly_enrollment) & fy.avg_monthly_enrollment.gt(0)
        & np.isfinite(fy.total_public_funding_components_2021_pesos)
        & fy.total_public_funding_components_2021_pesos.ge(0))
    fg = fy.groupby('school_rbd')
    valid = fg.valid.sum().reindex(x.index, fill_value=0)
    n = fg.size().reindex(x.index, fill_value=0)
    level = (fg.total_public_funding_components_2021_pesos.sum() / fg.avg_monthly_enrollment.sum()).reindex(x.index).where(valid.eq(5) & n.eq(5))
    change = (fy[fy.year.ge(2019)].groupby('school_rbd').public_funding_per_student_2021_pesos.mean()
        - fy[fy.year.le(2018)].groupby('school_rbd').public_funding_per_student_2021_pesos.mean()).reindex(x.index).where(valid.eq(5) & n.eq(5))
    for name, values in {'public_funding_level_millions': level / 1e6,
        'log_public_funding_level': np.log(level.where(level.gt(0))),
        'public_funding_change_millions': change / 1e6,
        'funding_record_year_share': n / 5, 'funding_full_year_share': valid / 5}.items():
        close(x['school__' + name], values)
    print('Verified exact school sample counts, 14 composition and five resource measures.', flush=True)

    ap = pd.read_csv(OUT / 'staff_age_person_role_school_year.csv.gz', dtype={'MRUN': str})
    assert not ap.duplicated(['MRUN', 'RBD', 'ROLE', 'AGNO']).any()
    # Re-read raw birthdates: check conflicts, sentinels and year-specific membership.
    for year in range(2018, 2025):
        a = ap[ap.AGNO.eq(year)]
        b = pd.read_csv(paths['birth_' + str(year)], sep=';', encoding='utf-8-sig',
                        usecols=['MRUN', 'DOC_FEC_NAC'], dtype=str)
        b = b[b.MRUN.isin(a.MRUN)].drop_duplicates()
        date = b.DOC_FEC_NAC
        yob = pd.to_numeric(date.str[:4], errors='coerce')
        month = pd.to_numeric(date.str[4:6], errors='coerce')
        good = date.str.fullmatch(r'\d{6}', na=False) & month.between(1, 12) & date.ne('190001') & (year-yob).between(18, 99)
        b['AGE'] = year-yob
        bg = b[good].groupby('MRUN')
        number = bg.DOC_FEC_NAC.nunique()
        ages = bg.AGE.first().where(number.eq(1))
        close(a.AGE, a.MRUN.map(ages))
        close(a.N_VALID_DATES, a.MRUN.map(number).fillna(0))
    annual = pd.read_csv(OUT / 'staff_age_school_year.csv').set_index(['ROLE', 'RBD', 'AGNO'])
    role_names = {'teacher': 'HS teachers', 'counselor': 'Orientadores', 'leadership': 'Leadership'}
    for role, label in role_names.items():
        a = ap[ap.ROLE.eq(label)].copy()
        a['age_under35_share'] = a.AGE.lt(35).where(a.AGE.notna())
        a['age_50plus_share'] = a.AGE.ge(50).where(a.AGE.notna())
        ag = a.groupby(['RBD', 'AGNO'])
        ar = annual.loc[role].copy()
        built = pd.DataFrame({'age_mean': ag.AGE.mean(), 'age_sd': ag.AGE.std(),
            'age_under35_share': ag.age_under35_share.mean(), 'age_50plus_share': ag.age_50plus_share.mean()}).reindex(ar.index)
        built['age_valid_share'] = ag.AGE.count().reindex(ar.index).div(ar.N_ROLE).where(ar.N_ROLE.gt(0))
        for col in built:
            close(ar[col], built[col])
        known = ar.N_ROLE.notna().groupby(level='RBD').sum()
        active = ar.N_ROLE.gt(0).groupby(level='RBD').sum()
        age_years = built.age_mean.notna().groupby(level='RBD').sum()
        period = built.groupby(level='RBD').mean().reindex(x.index)
        period.loc[~(known.eq(7) & active.gt(0)), :] = np.nan
        period.loc[age_years.lt(active), ['age_mean', 'age_sd', 'age_under35_share', 'age_50plus_share']] = np.nan
        period['age_year_coverage'] = age_years.div(active).where(known.eq(7) & active.gt(0))
        for col in period:
            close(x[role + '__' + col], period[col])
    print('Verified raw contemporaneous birthdates and all 18 staff age/coverage predictors.', flush=True)
    return True

if __name__ == '__main__':
    verify()
