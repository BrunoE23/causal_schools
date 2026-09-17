# School staff linkage coverage and VA

## User request and scope

Bruno requested the fraction of teachers, orientadores and leadership found in
the extensive titulados database per school, its distribution and VA correlations.
Use the existing 3,682 VA schools and 2018-2024 staff window. Teachers means
regular youth-HS teachers; do not mix the separately defined 2024 non-HS group.
Current orientador and leadership primary-or-secondary membership is preserved.
These are linkage-coverage measures, not the eight credential-characteristic
indicators and not staff-quality effects. The latter's VA correlations have not
been estimated by this task.

## Denominators and chronology

Main ANY_DB: exact-MRUN match to any qualification in the 2007-2025 reporting-
cohort files, including awards later than the particular staff observation.
This is intentional for a full-database linkage diagnostic, not evidence that
an earlier staff member already held a later qualification. UG_DB requires an
undergraduate match. ANY_ASOF / UG_ASOF impose the previous conservative award
and report-year cutoff for each staff year; all four are saved separately.

Annual share: matched people / distinct role members in the school-year.
Repeated appointments and qualifications cannot multiply a person within this
denominator. People can contribute to different schools and overlapping roles.
Reconcile every annual membership count with the original school-role roster.
Unknown role classifications imply an unknown denominator. A confirmed zero
headcount gives an undefined share, not zero matching.

The main school-period rate averages annual shares equally over active-role
years. Require known role counts in all seven years, as in the previous staff
analysis. Require at least one active year for coverage, not three as for staff
indices; a three-active-year sensitivity is supplied. Retain the observed-year
average for incomplete schools without treating it as the primary rate.
Save separate pooled-person-year rates and unique-ever-person rates (the latter
only for time-invariant ANY_DB/UG_DB). All 3,682 schools remain in each role's
wide output with support counts and status; do not silently discard absences.

## Associations

Use existing All-sample saved EB VA for all 12 outcomes. Pearson and Spearman
correlations weight schools equally, using outcome-specific complete cases.
Show broad-VA-student-weighted and unshrunk-VA correlations as sensitivities.
Rank correlations preserve ties at 10 significant digits, consistent with prior
staff reports. Pearson CIs/p-values are conventional conditional-on-VA results;
BH corrections are within role and coverage definition across 12 outcomes.
No adjusted regressions, causal claims, imputation or VA re-estimation.

## Findings and limitations

Schools with defined main rates: teachers 3,003, orientadores 2,111, leadership
3,550. Mean full-DB match shares: 71.7%, 61.8%, 56.7%; median: 72.4%, 85.7%,
59.5%. Among schools with the role, orientadores have zero matches at 519 and
complete matches at 1,013 schools, so coverage is very uneven.
UG-only means are 63.8%, 30.4%, 19.7%. Any-as-of means are 69.0%, 58.5%, 52.6%.

Many correlations are small, but it is incorrect to say none correlate with
higher-education VA. Teacher full-DB coverage has r=-0.124 with high-premium-
institution VA and +0.113 with higher-ed enrollment VA. Orientador coverage has
r=+0.061 with high-premium-field VA and +0.052 with projected-income VA; these
examples survive the stated within-role/definition BH adjustment. Leadership
links are weak. UG-only teacher coverage correlates -0.210 with high-premium-
institution VA (-0.218 for as-of UG), so the undergraduate coverage margin is
more selective. None of this establishes random missingness or credential effects.

## Outputs and verification

Scripts: code/codex/titulados_staff_linkage/08_school_match_coverage_va.R and
09_verify_school_coverage_report.py. Data: data/clean/titulados_staff_linkage/
school_coverage/. Merge-ready rates: school_staff_match_rates.csv. Report:
output/reports/school_staff_titulados_coverage_va.md. Distribution and correlation
PNGs: output/figures/titulados_school_coverage/.

Independent verification reconstructs annual rates from individual memberships
and match flags, checks original roster denominators, 309,288 annual and 44,184
period rows, distributions, 144 main and 108 robustness correlation rows, BH,
saved VA equality and unchanged source hashes. Figures were visually checked.
