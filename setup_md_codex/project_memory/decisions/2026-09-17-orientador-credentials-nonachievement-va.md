# Orientador credentials and non-achievement VA

## Request and prespecified construction

Bruno requested correlations of orientador credentials with non-achievement VA,
following the separate database-coverage diagnostic. Use all eight previously
defined credential binaries without selecting or changing their definitions
after inspecting VA. Membership is primary-or-secondary orientador at the
existing VA schools, one person per school-year. Read the verified dated
person-role-year indicators; no future qualification is attached to an earlier
staff year.

Annual share is the sum of each binary over all current orientadores divided by
their headcount. Main school share averages equally across active orientador
years in 2018-2024, requiring all seven role counts known and at least one
active year. No role means undefined, not zero. Preserve all 3,682 school rows;
2,111 have main shares. All zeros mean no qualifying observed award, not verified
lifetime absence; the 2007 start date and unsupported institution FE remain
measurement limitations.

## Outcomes and associations

Use the ten non-score outcomes from the existing twelve-outcome VA dictionary:
exam taking, higher-ed enrollment, STEM, high-premium field, high-premium
institution, projected income full/field/institution, program accreditation and
institution accreditation. Math/language scores are excluded. Use existing
All-sample school EB VA; do not re-estimate or recenter it. Outcomes have their
own pairwise complete sample, 2,037-2,111 schools for main results.

Report equal-school Pearson and Spearman, broad-VA-student-weighted correlations,
and unshrunk-VA counterparts. Preserve the ten-significant-digit rank-tie
convention. BH correction is across all 80 tests, separately per specification,
not separately per credential after observing results. Pearson inference is
conditional on estimated VA, not a full propagation of its uncertainty.

## Coverage and support sensitivities

1. Partial r: residualize both credential share and VA on an intercept, any-award
   matching share and UG-matching share in the same main complete sample. This is
   descriptive; no causal interpretation or claimed correction for selection.
2. Matched-only denominator: any observed award, except UG-high-premium uses
   observed UG awards. Each denominator must be nonempty in >=80% of active
   role-years; average the available matched-year shares. This changes both the
   denominator and the sample (1,153 schools for seven measures, 526 for UG at
   high-premium institutions in the full-income comparison). It is not an
   unbiased missing-data correction.
3. Require at least three active orientador years.

No school-context/age-adjusted regression or within-school identification was
requested or performed. Rare masters and short staff histories remain visible.

## Findings

Orientation-related qualification shares: r=0.080 with higher-ed enrollment,
0.075 with high-premium-field and 0.087 with projected-income-full VA. These pass
the 80-test BH correction. The income r remains 0.085 after partialling match
coverage and 0.103 among matched records. High-premium-institution training is
most associated with students' high-premium-institution VA (UG r=0.144, any award
r=0.133, non-UG r=0.107, Magister r=0.100). Any-Magister/full-income r=0.015 and
orientation-specific-Magister/full-income r=0.033 do not pass the main BH test.
These are modest associations, not causal effects of counselors or credentials.

## Outputs and verification

Code: code/codex/titulados_staff_linkage/10_orientador_credentials_va.R and
11_verify_orientador_credentials_report.py. Clean outputs:
data/clean/titulados_staff_linkage/orientador_credentials_va/. Report:
output/reports/orientador_credentials_nonachievement_va.md. Figure:
output/figures/orientador_credentials_va/orientador_credentials_nonachievement_va.png.

Independent Python checks reconstruct annual/period rates from person-school
memberships and person credentials, reconcile roster denominators, verify all
240 main/sensitivity rows and 80 partial correlations, BH, saved-VA equality and
unchanged source hashes. No source staff, credential or VA dataset is overwritten.
