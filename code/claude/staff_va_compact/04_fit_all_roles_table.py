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

src = Path(__file__).with_name('01_fit_compact_table.py').read_text(encoding='utf-8')

src = src.replace(
    "OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_compact' + SUFFIX)\n"
    "OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_compact' + SUFFIX)",
    f"OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_all_roles_{VARIANT}' + SUFFIX)\n"
    f"OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_all_roles_{VARIANT}' + SUFFIX)")

start = src.index("FOCAL = [")
end = src.index("# Track dummies", start)
focal = r'''FOCAL = [
    ('teacher__per100_enrolled', 'Staff per 100 students', 'Teachers'),
    ('teacher__age_under35_share', 'Share under age 35', 'Teachers'),
    ('teacher__UG_HIGH_PREMIUM', 'High-premium undergraduate degree share', 'Teachers'),
    ('teacher__ANY_HIGH_PREMIUM', 'Any high-premium credential share', 'Teachers'),
    ('teacher__ROLE_SPECIFIC_QUALIFICATION', 'Role-specific qualification share', 'Teachers'),
    ('teacher__ROLE_SPECIFIC_MAGISTER', 'Role-specific master degree share', 'Teachers'),
    ('teacher__balanced_index', 'Qualification index', 'Teachers'),
    ('counselor__per100_enrolled', 'Staff per 100 students', 'Counselors'),
    ('counselor__age_under35_share', 'Share under age 35', 'Counselors'),
    ('counselor__UG_HIGH_PREMIUM', 'High-premium undergraduate degree share', 'Counselors'),
    ('counselor__ANY_HIGH_PREMIUM', 'Any high-premium credential share', 'Counselors'),
    ('counselor__ROLE_SPECIFIC_QUALIFICATION', 'Role-specific qualification share', 'Counselors'),
    ('counselor__ROLE_SPECIFIC_MAGISTER', 'Role-specific master degree share', 'Counselors'),
    ('counselor__balanced_index', 'Qualification index', 'Counselors'),
    ('leadership__per100_enrolled', 'Staff per 100 students', 'Leadership'),
    ('leadership__age_under35_share', 'Share under age 35', 'Leadership'),
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
src = src[:start] + focal + src[end:]

if INCLUDE_PEERS:
    partition = "STAFF = feats[:21]; SCHOOL = feats[21:23]; TRACK = feats[23:]"
else:
    partition = "STAFF = feats[:21]; SCHOOL = feats[21:22]; TRACK = feats[22:]"
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
src = src[:tier_start] + src[tier_end:]

rows_start = src.index("PAPER_ROWS = {")
rows_end = src.index("BLOCKS =", rows_start)
row_lines = ["PAPER_ROWS = {"]
labels = {
    'per100_enrolled': 'Staff per 100 students',
    'age_under35_share': 'Share under age 35',
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
                  (" (controlling for peers)" if INCLUDE_PEERS else ""))
src = src.replace("tab:staff-va", "tab:staff-va-all-roles-" + VARIANT.replace('_', '-'))
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
src = src.replace("'staff_va_compact.tex'", f"'staff_va_all_roles_{VARIANT}.tex'")
src = src.replace("'staff_va_compact.csv'", f"'staff_va_all_roles_{VARIANT}.csv'")

exec(compile(src, str(Path(__file__).with_name('01_fit_compact_table.py')), 'exec'))
