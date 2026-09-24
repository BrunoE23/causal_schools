"""Staff-VA table separating roster and linked-credential measurement.

Directory qualifications are measured for staff of all ages. Measures that
require the SIES degree linkage are measured only among staff born in 1989 or
later, for whom the degree register beginning in 2007 covers the conventional
college-entry ages. The regression/sample machinery is inherited from the
paper's existing compact staff table.
"""
from pathlib import Path

base = Path('code/claude/staff_va_compact/01_fit_compact_table.py')
src = base.read_text(encoding='utf-8')

src = src.replace(
    "OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_compact' + SUFFIX)\n"
    "OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_compact' + SUFFIX)",
    "OUT_DATA = os.path.join(ROOT, 'data/clean/staff_va_age_coverage' + SUFFIX)\n"
    "OUT_TAB = os.path.join(ROOT, 'output/tables/staff_va_age_coverage' + SUFFIX)")

start = src.index('FOCAL = [')
end = src.index('# Track dummies', start)
focal = r'''FOCAL = [
    ('teacher__age_under35_per100', 'Teachers under age 35 per 100 students', 'Age'),
    ('teacher__age_35_49_per100', 'Teachers ages 35--49 per 100 students', 'Age'),
    ('teacher__age_50plus_per100', 'Teachers age 50+ per 100 students', 'Age'),
    ('counselor__age_under35_per100', 'Counselors under age 35 per 100 students', 'Age'),
    ('counselor__age_35_49_per100', 'Counselors ages 35--49 per 100 students', 'Age'),
    ('counselor__age_50plus_per100', 'Counselors age 50+ per 100 students', 'Age'),
    ('leadership__age_under35_per100', 'Leaders under age 35 per 100 students', 'Age'),
    ('leadership__age_35_49_per100', 'Leaders ages 35--49 per 100 students', 'Age'),
    ('leadership__age_50plus_per100', 'Leaders age 50+ per 100 students', 'Age'),
    ('teacher__directory_university', 'Teachers: university qualification share', 'Directory'),
    ('teacher__directory_teaching', 'Teachers: high-school teaching qualification share', 'Directory'),
    ('counselor__directory_university', 'Counselors: university qualification share', 'Directory'),
    ('counselor__directory_teaching', 'Counselors: teaching qualification share', 'Directory'),
    ('leadership__directory_university', 'Leaders: university qualification share', 'Directory'),
    ('leadership__directory_teaching', 'Leaders: teaching qualification share', 'Directory'),
    ('teacher__young_UG_HIGH_PREMIUM', 'Teachers: high-premium undergraduate share', 'SIES'),
    ('teacher__young_ANY_HIGH_PREMIUM', 'Teachers: any high-premium credential share', 'SIES'),
    ('teacher__young_ROLE_SPECIFIC_QUALIFICATION', 'Teachers: role-specific qualification share', 'SIES'),
    ('teacher__young_ROLE_SPECIFIC_MAGISTER', 'Teachers: role-specific master share', 'SIES'),
    ('counselor__young_UG_HIGH_PREMIUM', 'Counselors: high-premium undergraduate share', 'SIES'),
    ('counselor__young_ANY_HIGH_PREMIUM', 'Counselors: any high-premium credential share', 'SIES'),
    ('counselor__young_ROLE_SPECIFIC_QUALIFICATION', 'Counselors: role-specific qualification share', 'SIES'),
    ('counselor__young_ROLE_SPECIFIC_MAGISTER', 'Counselors: role-specific master share', 'SIES'),
    ('leadership__young_UG_HIGH_PREMIUM', 'Leaders: high-premium undergraduate share', 'SIES'),
    ('leadership__young_ANY_HIGH_PREMIUM', 'Leaders: any high-premium credential share', 'SIES'),
    ('leadership__young_ROLE_SPECIFIC_QUALIFICATION', 'Leaders: role-specific qualification share', 'SIES'),
    ('leadership__young_ROLE_SPECIFIC_MAGISTER', 'Leaders: role-specific master share', 'SIES')]
'''
src = src[:start] + focal + src[end:]
src = src.replace('STAFF = feats[:8]; SCHOOL = feats[8:10]; TRACK = feats[10:]',
                  'STAFF = feats; SCHOOL = []; TRACK = []')

# Replace the qualification-index merge with all-age directory measures.
old = """s = pd.read_csv(P['staff_idx'], usecols=['RBD', 'ROLE', 'balanced_index'])
t = s[s.ROLE == 'teacher'].set_index('RBD').balanced_index
l = pd.read_csv(P['lead_idx'], usecols=['RBD', 'balanced_index']).set_index('RBD').balanced_index
assert t.index.is_unique and l.index.is_unique
x['teacher__balanced_index'] = x.RBD.map(t)
x['leadership__balanced_index'] = x.RBD.map(l)"""
new = """s = pd.read_csv(P['staff_idx'], usecols=['RBD', 'ROLE', 'university_share',
                                                   'teaching_title_share', 'hs_teaching_title_share'])
for role in ('teacher', 'counselor'):
    z = s[s.ROLE == role].set_index('RBD')
    assert z.index.is_unique
    x[f'{role}__directory_university'] = x.RBD.map(z.university_share)
    teach = z.hs_teaching_title_share if role == 'teacher' else z.teaching_title_share
    x[f'{role}__directory_teaching'] = x.RBD.map(teach)
l = pd.read_csv(P['lead_idx'], usecols=['RBD', 'university_share', 'teaching_title_share']).set_index('RBD')
assert l.index.is_unique
x['leadership__directory_university'] = x.RBD.map(l.university_share)
x['leadership__directory_teaching'] = x.RBD.map(l.teaching_title_share)"""
assert old in src
src = src.replace(old, new)

# Replace the counselor cohort tiers with age counts for all roles and detailed
# linked-degree shares for staff born in 1989 or later.
tier_start = src.index('# Orientador birth-cohort tiers')
tier_end = src.index('# Peer composition', tier_start)
measurement = r'''# Mean annual staff counts in mutually exclusive age categories.
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

# SIES-linked credential shares among staff born in 1989 or later. We first
# form a share within school-role-year, then average the annual shares so that
# each observed staff year receives equal weight, as in the existing inputs.
member_path = 'data/clean/titulados_staff_linkage/staff_membership_person_school_year.csv.gz'
cred_path = 'data/clean/titulados_staff_linkage/credentials/staff_credentials_person_role_year.csv.gz'
P['membership'] = member_path; P['credentials'] = cred_path
md5['membership'] = hashlib.md5(open(member_path, 'rb').read()).hexdigest()
md5['credentials'] = hashlib.md5(open(cred_path, 'rb').read()).hexdigest()
flags = ['UG_HIGH_PREMIUM', 'ANY_HIGH_PREMIUM', 'ROLE_SPECIFIC_QUALIFICATION',
         'ROLE_SPECIFIC_MAGISTER']
m = pd.read_csv(member_path)
c = pd.read_csv(cred_path, usecols=['MRUN', 'ROLE', 'AGNO', 'STAFF_BIRTH_YM'] + flags)
j = m.merge(c, on=['MRUN', 'ROLE', 'AGNO'], how='left', validate='many_to_one')
j['BIRTH_YEAR'] = pd.to_numeric(j.STAFF_BIRTH_YM.astype(str).str[:4], errors='coerce')
j = j[j.BIRTH_YEAR >= 1989].copy()
role_map = {'HS teachers': 'teacher', 'Orientadores': 'counselor', 'Leadership': 'leadership'}
j = j[j.ROLE.isin(role_map)]
annual = j.groupby(['ROLE', 'RBD', 'AGNO'])[flags].mean()
period = annual.groupby(['ROLE', 'RBD'])[flags].mean()
for raw_role, role in role_map.items():
    z = period.loc[raw_role]
    for flag in flags:
        x[f'{role}__young_{flag}'] = x.RBD.map(z[flag])

'''
src = src[:tier_start] + measurement + src[tier_end:]

# Table 6 controls, suppressed from the displayed rows: administration type
# (Municipal DAEM omitted), school-size categories (100--249 omitted), and
# curricular tracks (no track omitted). Geography is deliberately excluded.
control_code = r'''x['school__size_under100'] = (x.hs_enrolled_est < 100).astype(float)
x['school__size_250_499'] = ((x.hs_enrolled_est >= 250) & (x.hs_enrolled_est < 500)).astype(float)
x['school__size_500_plus'] = (x.hs_enrolled_est >= 500).astype(float)
x['school__dependency_1'] = (x[[f'school__dependency_{k}' for k in (2, 3, 4, 5, 6)]].sum(axis=1) == 0).astype(float)
context = ['school__dependency_1', 'school__dependency_3', 'school__dependency_5',
           'school__dependency_6', 'school__size_under100', 'school__size_250_499',
           'school__size_500_plus', 'school__has_tp', 'school__has_artistic']
'''
old_context = "context = \\\n    [c for c in x.columns if c.startswith('school__dependency_') or c.startswith('school__region_')]"
assert old_context in src
src = src.replace(old_context, control_code.rstrip())

# No peer row in this version; the main paper currently omits peers.
src = src.replace("    PEER = ['school__composition_math_mean']; SIZE = [f for f in SCHOOL if f not in PEER]",
                  "    PEER = []; SIZE = SCHOOL")
src = src.replace("                 ('R2_STAFF', r'$R^2$: adding staff'), ('R2_FULL', r'$R^2$: adding peers (full model)')):",
                  "                 ('R2_FULL', r'$R^2$: adding staff (full model)')):")
src = src.replace("for spec, fs in (('controls', []), ('sizetrack', SIZE + TRACK), ('staff', SIZE + TRACK + STAFF), ('full', feats)):",
                  "for spec, fs in (('controls', []), ('sizetrack', []), ('staff', feats), ('full', feats)):")
src = src.replace("('R2_SIZETRACK', r'$R^2$: adding size and track'),\n                 ('R2_FULL', r'$R^2$: adding staff (full model)')):",
                  "('R2_FULL', r'$R^2$: adding staff (full model)')):")

rows_start = src.index('PAPER_ROWS = {')
rows_end = src.index('BLOCKS =', rows_start)
paper_rows = r'''PAPER_ROWS = {
    'teacher__age_under35_per100': 'Teachers: under age 35 per 100 students',
    'teacher__age_35_49_per100': 'Teachers: ages 35--49 per 100 students',
    'teacher__age_50plus_per100': 'Teachers: age 50+ per 100 students',
    'counselor__age_under35_per100': 'Counselors: under age 35 per 100 students',
    'counselor__age_35_49_per100': 'Counselors: ages 35--49 per 100 students',
    'counselor__age_50plus_per100': 'Counselors: age 50+ per 100 students',
    'leadership__age_under35_per100': 'Leadership: under age 35 per 100 students',
    'leadership__age_35_49_per100': 'Leadership: ages 35--49 per 100 students',
    'leadership__age_50plus_per100': 'Leadership: age 50+ per 100 students',
    'teacher__directory_university': 'Teachers: university qualification share',
    'teacher__directory_teaching': 'Teachers: high-school teaching qualification share',
    'counselor__directory_university': 'Counselors: university qualification share',
    'counselor__directory_teaching': 'Counselors: teaching qualification share',
    'leadership__directory_university': 'Leadership: university qualification share',
    'leadership__directory_teaching': 'Leadership: teaching qualification share',
    'teacher__young_UG_HIGH_PREMIUM': 'Teachers: high-premium undergraduate share',
    'teacher__young_ANY_HIGH_PREMIUM': 'Teachers: any high-premium credential share',
    'teacher__young_ROLE_SPECIFIC_QUALIFICATION': 'Teachers: role-specific qualification share',
    'teacher__young_ROLE_SPECIFIC_MAGISTER': "Teachers: role-specific master's share",
    'counselor__young_UG_HIGH_PREMIUM': 'Counselors: high-premium undergraduate share',
    'counselor__young_ANY_HIGH_PREMIUM': 'Counselors: any high-premium credential share',
    'counselor__young_ROLE_SPECIFIC_QUALIFICATION': 'Counselors: role-specific qualification share',
    'counselor__young_ROLE_SPECIFIC_MAGISTER': "Counselors: role-specific master's share",
    'leadership__young_UG_HIGH_PREMIUM': 'Leadership: high-premium undergraduate share',
    'leadership__young_ANY_HIGH_PREMIUM': 'Leadership: any high-premium credential share',
    'leadership__young_ROLE_SPECIFIC_QUALIFICATION': 'Leadership: role-specific qualification share',
    'leadership__young_ROLE_SPECIFIC_MAGISTER': "Leadership: role-specific master's share"}
'''
src = src[:rows_start] + paper_rows + src[rows_end:]
src = src.replace(
    "BLOCKS = {'Teachers': 'Teachers', 'Orientadores': 'Counselors (orientadores)', 'Leadership': 'School leadership',\n"
    "          'Size': 'Peers, size and track', 'Resources': None, 'Track': None}",
    "BLOCKS = {'Age': 'Staffing by age', 'Directory': 'Qualifications reported in the staff directory (all staff)',\n"
    "          'SIES': 'Detailed linked-degree measures (staff born in 1989 or later)'}")
src = src.replace('School staff, peers, size and track, and school value added',
                  'Staff measurement coverage and school value added')
src = src.replace('tab:staff-va', 'tab:staff-va-age-coverage')

note_start = "      'Staff measures average 2018--2024 administrative staff records. Staffing ratios divide mean annual '"
note_end = "      'regressors, absent roles and incomplete staff rosters. Heteroskedasticity-robust (HC1) standard errors '"
i = src.index(note_start)
j2 = src.index(note_end, i)
replacement = """      'Staff measures average 2018--2024 administrative records. Staffing ratios divide mean annual '
      'headcounts by the number of grade-8-cohort students assigned to the school. University and teaching '
      'qualifications are reported in the staff directory and cover staff of all ages. High-premium and '
      'role-specific credentials use linked SIES degree records and are shares among staff born in 1989 or '
      'later; the SIES register begins in 2007, when this cohort was at most 18 years old. Schools without '
      'younger staff in a role have a missing detailed-credential share and enter through the corresponding '
      'missing-value indicator. In the estimation sample, these detailed measures are observed for 2,045 schools '
      'for teachers, 214 for counselors, and 314 for leadership. All regressions include the administration-type, '
      'school-size-category, and curricular-track indicators reported in Table~\\\\ref{tab:admin-type-va}; these '
      'controls are not displayed. Geography is not included. The sample excludes fully private schools and '
      'schools with fewer than 100 students in the pooled value-added sample. Regressions also include indicators '
      'for missing regressors, absent roles and incomplete staff rosters. '
"""
src = src[:i] + replacement + src[j2 + len(note_end):]
src = src.replace("; the technical-professional coefficient is the difference between schools with and without that track. ", ". ")
src = src.replace("      'in parentheses. * $p<0.10$", 
                  "      'Heteroskedasticity-robust (HC1) standard errors in parentheses. * $p<0.10$")
src = src.replace("'staff_va_compact.tex'", "'staff_va_age_coverage.tex'")
src = src.replace("'staff_va_compact.csv'", "'staff_va_age_coverage.csv'")

exec(compile(src, str(base), 'exec'))

# This deliberately comprehensive version has many rows. Scale the tabular
# slightly below the text width so that the table and notes fit on one page.
tex_path = Path(OUT_TAB) / 'staff_va_age_coverage.tex'
table_tex = tex_path.read_text(encoding='utf-8')
table_tex = table_tex.replace(r'\resizebox{\textwidth}{!}{%',
                              r'\resizebox{0.78\textwidth}{!}{%')
table_tex = table_tex.replace(
    '; the technical-professional coefficient is the difference between schools with and without that track.',
    '.')
tex_path.write_text(table_tex, encoding='utf-8')

