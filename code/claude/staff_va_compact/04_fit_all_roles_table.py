"""Expanded, role-symmetric staff correlates of school EB VA.

This imports the verified compact-table estimator, but replaces its asymmetric
staff selection with the same seven measures for teachers, counselors, and
school leadership. Outputs are written separately so the compact paper table
is preserved while the larger candidate set is assessed.
"""
import os
from pathlib import Path

INCLUDE_PEERS = os.environ.get('INCLUDE_PEERS', '0') == '1'
VARIANT = 'with_peers' if INCLUDE_PEERS else 'no_peers'
SECTOR = os.environ.get('SCHOOL_SECTOR', 'all').lower()
if SECTOR not in {'all', 'public', 'private_subsidized'}:
    raise ValueError('SCHOOL_SECTOR must be all, public, or private_subsidized')
TAG = VARIANT if SECTOR == 'all' else f'{VARIANT}_{SECTOR}'

src = Path(__file__).with_name('01_fit_compact_table.py').read_text(encoding='utf-8')

src = src.replace(
    "OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_compact' + SUFFIX)\n"
    "OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_compact' + SUFFIX)",
    f"OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_all_roles_{TAG}' + SUFFIX)\n"
    f"OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_all_roles_{TAG}' + SUFFIX)")

start = src.index("FOCAL = [")
end = src.index("# Track dummies", start)
focal = r'''FOCAL = [
    ('teacher__age_under35_per100', 'Staff under age 35 per 100 students', 'Teachers'),
    ('teacher__age_35_49_per100', 'Staff ages 35--49 per 100 students', 'Teachers'),
    ('teacher__age_50plus_per100', 'Staff age 50+ per 100 students', 'Teachers'),
    ('teacher__UG_HIGH_PREMIUM', 'High-premium undergraduate degree share', 'Teachers'),
    ('teacher__ANY_HIGH_PREMIUM', 'Any high-premium credential share', 'Teachers'),
    ('teacher__ROLE_SPECIFIC_QUALIFICATION', 'Role-specific qualification share', 'Teachers'),
    ('teacher__ROLE_SPECIFIC_MAGISTER', 'Role-specific master degree share', 'Teachers'),
    ('teacher__balanced_index', 'Qualification index', 'Teachers'),
    ('counselor__age_under35_per100', 'Staff under age 35 per 100 students', 'Counselors'),
    ('counselor__age_35_49_per100', 'Staff ages 35--49 per 100 students', 'Counselors'),
    ('counselor__age_50plus_per100', 'Staff age 50+ per 100 students', 'Counselors'),
    ('counselor__UG_HIGH_PREMIUM', 'High-premium undergraduate degree share', 'Counselors'),
    ('counselor__ANY_HIGH_PREMIUM', 'Any high-premium credential share', 'Counselors'),
    ('counselor__ROLE_SPECIFIC_QUALIFICATION', 'Role-specific qualification share', 'Counselors'),
    ('counselor__ROLE_SPECIFIC_MAGISTER', 'Role-specific master degree share', 'Counselors'),
    ('counselor__balanced_index', 'Qualification index', 'Counselors'),
    ('leadership__age_under35_per100', 'Staff under age 35 per 100 students', 'Leadership'),
    ('leadership__age_35_49_per100', 'Staff ages 35--49 per 100 students', 'Leadership'),
    ('leadership__age_50plus_per100', 'Staff age 50+ per 100 students', 'Leadership'),
    ('leadership__UG_HIGH_PREMIUM', 'High-premium undergraduate degree share', 'Leadership'),
    ('leadership__ANY_HIGH_PREMIUM', 'Any high-premium credential share', 'Leadership'),
    ('leadership__ROLE_SPECIFIC_QUALIFICATION', 'Role-specific qualification share', 'Leadership'),
    ('leadership__ROLE_SPECIFIC_MAGISTER', 'Role-specific master degree share', 'Leadership'),
    ('leadership__balanced_index', 'Qualification index', 'Leadership'),
    ('school__composition_math_mean', 'Peer grade-4 math (mean)', 'Size'),
    ('school__log_enrolled', 'Log students enrolled', 'Size'),
    ('school__has_tp', 'Technical-professional offering (0/1)', 'Track'),
    ('school__has_artistic', 'Artistic offering (0/1; 2 schools)', 'Track')]
'''
if not INCLUDE_PEERS:
    focal = focal.replace(
        "    ('school__composition_math_mean', 'Peer grade-4 math (mean)', 'Size'),\n", "")
if SECTOR == 'private_subsidized':
    focal = focal.replace(
        "    ('school__has_artistic', 'Artistic offering (0/1; 2 schools)', 'Track')", "")
src = src[:start] + focal + src[end:]

if INCLUDE_PEERS:
    partition = "STAFF = feats[:24]; SCHOOL = feats[24:26]; TRACK = feats[26:]"
else:
    partition = "STAFF = feats[:24]; SCHOOL = feats[24:25]; TRACK = feats[25:]"
src = src.replace("STAFF = feats[:8]; SCHOOL = feats[8:10]; TRACK = feats[10:]", partition)
src = src.replace(
    "s = pd.read_csv(P['staff_idx'], usecols=['RBD', 'ROLE', 'balanced_index'])\n"
    "t = s[s.ROLE == 'teacher'].set_index('RBD').balanced_index\n"
    "l = pd.read_csv(P['lead_idx'], usecols=['RBD', 'balanced_index']).set_index('RBD').balanced_index\n"
    "assert t.index.is_unique and l.index.is_unique\n"
    "x['teacher__balanced_index'] = x.RBD.map(t)\n"
    "x['leadership__balanced_index'] = x.RBD.map(l)",
    "s = pd.read_csv(P['staff_idx'], usecols=['RBD', 'ROLE', 'balanced_index'])\n"
    "for role in ('teacher', 'counselor'):\n"
    "    z = s[s.ROLE == role].set_index('RBD').balanced_index\n"
    "    assert z.index.is_unique\n"
    "    x[f'{role}__balanced_index'] = x.RBD.map(z)\n"
    "l = pd.read_csv(P['lead_idx'], usecols=['RBD', 'balanced_index']).set_index('RBD').balanced_index\n"
    "assert l.index.is_unique\n"
    "x['leadership__balanced_index'] = x.RBD.map(l)")

# The counselor birth-cohort construction belongs to the compact specification,
# not to this matched-variable table.
tier_start = src.index("# Orientador birth-cohort tiers")
tier_end = src.index("# Peer composition", tier_start)
age_code = r'''# Mean annual staff counts in mutually exclusive age categories. Birth dates
# are observed for essentially all roster members; inactive school-years count
# as zero, while genuinely unknown roster years remain missing.
age_path = 'data/clean/staff_lasso_va/staff_age_school_year.csv'
P['age_year'] = age_path
md5['age_year'] = hashlib.md5(open(age_path, 'rb').read()).hexdigest()
age = pd.read_csv(age_path)
age_groups = [('under35', 'age_under35_share'), ('35_49', 'age_35_49_share'),
              ('50plus', 'age_50plus_share')]
for suffix, share in age_groups:
    age[suffix] = age.N_VALID_AGE * age[share]
    age.loc[age.N_VALID_AGE.eq(0), suffix] = 0.0
age_mean = age.groupby(['ROLE', 'RBD'])[[a for a, _ in age_groups]].mean()
for role in ('teacher', 'counselor', 'leadership'):
    den = x.hs_enrolled_est.where(x.hs_enrolled_est > 0)
    for suffix, _ in age_groups:
        counts = x.RBD.map(age_mean.loc[role, suffix])
        x[f'{role}__age_{suffix}_per100'] = 100 * counts / den

'''
src = src[:tier_start] + age_code + src[tier_end:]

rows_start = src.index("PAPER_ROWS = {")
rows_end = src.index("BLOCKS =", rows_start)
row_lines = ["PAPER_ROWS = {"]
labels = {
    'age_under35_per100': 'Under age 35 per 100 students',
    'age_35_49_per100': 'Ages 35--49 per 100 students',
    'age_50plus_per100': 'Age 50+ per 100 students',
    'UG_HIGH_PREMIUM': 'High-premium undergraduate degree share',
    'ANY_HIGH_PREMIUM': 'Any high-premium credential share',
    'ROLE_SPECIFIC_QUALIFICATION': 'Role-specific qualification share',
    'ROLE_SPECIFIC_MAGISTER': "Role-specific master's degree share",
    'balanced_index': 'Qualification index',
}
for role in ('teacher', 'counselor', 'leadership'):
    for suffix, label in labels.items():
        row_lines.append(f"    '{role}__{suffix}': \"{label}\",")
row_lines += [
    "    'school__composition_math_mean': 'Peer grade-4 math achievement',",
    "    'school__log_enrolled': 'Log students enrolled',",
    "    'school__has_tp': 'Technical-professional track (0/1)'", "}\n"]
src = src[:rows_start] + '\n'.join(row_lines) + src[rows_end:]
src = src.replace("'Orientadores': 'Counselors (orientadores)'", "'Counselors': 'Counselors (orientadores)'")
src = src.replace("School staff, peers, size and track, and school value added",
                  "Role-symmetric school staff correlates of school value added" +
                  (" (controlling for peers)" if INCLUDE_PEERS else "") +
                  ({'all': '', 'public': ': public schools',
                    'private_subsidized': ': private-subsidized schools'}[SECTOR]))
src = src.replace("tab:staff-va", "tab:staff-va-all-roles-" + TAG.replace('_', '-'))
src = src.replace("Qualification indices combine ", "The qualification index combines ")
src = src.replace(
    "Counseling-trained counselors hold a counseling-specific postgraduate or post-degree "
    "certificate recorded in SIES graduation records by the staff year. ", "")
src = src.replace(
    "Counseling-trained counselors hold a counseling-specific postgraduate or post-degree '",
    "'")
src = src.replace(
    "      'certificate recorded in SIES graduation records by the staff year. All regressions control for '",
    "      'All regressions control for '")
if not INCLUDE_PEERS:
    src = src.replace(
        "    PEER = ['school__composition_math_mean']; SIZE = [f for f in SCHOOL if f not in PEER]",
        "    PEER = []; SIZE = SCHOOL")
    src = src.replace(
        "                 ('R2_STAFF', r'$R^2$: adding staff'), ('R2_FULL', r'$R^2$: adding peers (full model)')):",
        "                 ('R2_FULL', r'$R^2$: adding staff (full model)')):")
    src = src.replace("All regressions control for dependency, region and an artistic-track indicator,",
                      "All regressions control for dependency, region and an artistic-track indicator,")
if SECTOR != 'all':
    sector_filter = {
        'public': "idx &= ((x.school__dependency_2.values == 1) | (x.school__dependency_5.values == 1) | (x.school__dependency_6.values == 1) | ((x[[c for c in x.columns if c.startswith('school__dependency_')]].sum(axis=1).values) == 0))",
        'private_subsidized': "idx &= (x.school__dependency_3.values == 1)",
    }[SECTOR]
    src = src.replace(
        "    idx &= (x.school__dependency_4.values != 1)",
        "    idx &= (x.school__dependency_4.values != 1)\n    " + sector_filter)
sector_sample = {
    'public': 'public schools (municipal, SLEP, or delegated-administration)',
    'private_subsidized': 'private-subsidized schools',
}.get(SECTOR)
if sector_sample:
    src = src.replace(
        "samp = ('public and private-subsidized schools with at least %d students in the value-added sample' % MIN_VA) if MIN_VA else 'all public and private-subsidized schools'",
        f"samp = ('{sector_sample} with at least %d students in the value-added sample' % MIN_VA) if MIN_VA else 'all {sector_sample}'")
if SECTOR == 'private_subsidized':
    src = src.replace("dependency, region and an artistic-track indicator",
                      "dependency and region")
src = src.replace("'staff_va_compact.tex'", f"'staff_va_all_roles_{TAG}.tex'")
src = src.replace("'staff_va_compact.csv'", f"'staff_va_all_roles_{TAG}.csv'")

exec(compile(src, str(Path(__file__).with_name('01_fit_compact_table.py')), 'exec'))
