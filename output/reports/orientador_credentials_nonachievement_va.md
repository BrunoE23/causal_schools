# Orientador credentials and non-achievement school VA

## Main results

These are correlations with the eight observed credential measures, not database-match rates. The clearest patterns concern training at high-premium institutions and orientation-related qualifications. Simply having a magister has little association with higher-education outcomes in these data.

| School share of orientadores with... | Higher-ed enrollment | High-premium field | High-premium institution | Projected income (full) |
| --- | --- | --- | --- | --- |
| UG at high-premium institution | 0.009 | 0.026 | 0.144* | 0.060* |
| Any non-UG qualification | 0.044 | 0.050 | 0.017 | 0.038 |
| Non-UG at high-premium institution | 0.037 | 0.043 | 0.107* | 0.075* |
| Any award at high-premium institution | 0.032 | 0.040 | 0.133* | 0.079* |
| Orientation-related qualification | 0.080* | 0.075* | 0.031 | 0.087* |
| Any magister | 0.032 | 0.009 | 0.015 | 0.015 |
| Magister at high-premium institution | 0.022 | -0.002 | 0.100* | 0.064* |
| Orientation-related magister | 0.045 | 0.004 | -0.008 | 0.033 |

* BH-adjusted p < 0.05 across all 80 main tests (eight credentials by ten non-score outcomes). There are 2,110 schools for higher-ed enrollment and 2,111 for field, institution and full-income VA. The complete outcome family includes exam taking, enrollment, STEM, high-premium field/institution, three projected-income measures and two accreditation outcomes; math/language scores are excluded.

![All correlations](C:/Users/brunem/Research/causal_schools/output/figures/orientador_credentials_va/orientador_credentials_nonachievement_va.png)

Orientation-related qualifications correlate 0.080 with enrollment VA, 0.075 with high-premium-field VA and 0.087 with full projected-income VA. An undergraduate qualification at a high-premium institution correlates 0.144 with high-premium-institution VA; any qualification at one correlates 0.133. These are modest associations, not estimated causal effects of counselor credentials.

## Construction and denominators

For each school-year, count each orientador once across appointments and divide each binary credential total by all current primary-or-secondary orientadores. Qualifications are those reported and obtained by that staff year, using the verified person-role-year indicators and the existing subject mapping. A zero means no qualifying award observed, not no lifetime qualification. Records begin in 2007. Average annual shares equally over active orientador years in 2018-2024. Require all seven role counts known and at least one active year. Schools with no orientador have undefined shares, not zeros. All 3,682 school rows remain in the underlying period data; 2,111 have main rates.

High premium is the preexisting centered MiFuturo institution FE >0.1. Non-undergraduate includes diplomados, postitulos, magisters, doctorates and specialties. A role-specific qualification explicitly concerns orientation/counseling/psychoeducation/family mediation under the reviewable mapping; generic psychology or education alone is insufficient. A relevant master's must itself satisfy the subject condition. No subject definitions or institution thresholds were changed after looking at VA.

| Credential | Schools | Mean share | Schools with positive share | Matched-only schools |
| --- | --- | --- | --- | --- |
| UG at high-premium institution | 2111 | 3.2% | 152 | 526 |
| Any non-UG qualification | 2111 | 45.2% | 1335 | 1153 |
| Non-UG at high-premium institution | 2111 | 18.4% | 649 | 1153 |
| Any award at high-premium institution | 2111 | 20.8% | 714 | 1153 |
| Orientation-related qualification | 2111 | 23.1% | 785 | 1153 |
| Any magister | 2111 | 21.8% | 762 | 1153 |
| Magister at high-premium institution | 2111 | 2.9% | 124 | 1153 |
| Orientation-related magister | 2111 | 5.0% | 181 | 1153 |

## Coverage and support checks: projected-income VA

| Credential | Main r | Coverage partial r | Matched-only r | >=3 active years r |
| --- | --- | --- | --- | --- |
| UG at high-premium institution | 0.060 | 0.053 | 0.115 | 0.056 |
| Any non-UG qualification | 0.038 | 0.036 | 0.026 | 0.044 |
| Non-UG at high-premium institution | 0.075 | 0.073 | 0.091 | 0.066 |
| Any award at high-premium institution | 0.079 | 0.075 | 0.097 | 0.071 |
| Orientation-related qualification | 0.087 | 0.085 | 0.103 | 0.090 |
| Any magister | 0.015 | 0.005 | 0.004 | 0.026 |
| Magister at high-premium institution | 0.064 | 0.062 | 0.074 | 0.050 |
| Orientation-related magister | 0.033 | 0.028 | 0.021 | 0.035 |

Coverage partial r residualizes both the credential share and VA on an intercept, any-award match share and undergraduate-match share, on the same complete main sample. It is a descriptive partial correlation, not a causal coefficient. The source does not support a claim that this corrects selective observation.

Matched-only shares divide by orientadores with at least one as-of qualification match; the undergraduate-high-premium share instead divides by orientadores with an as-of undergraduate match. A matched denominator must be nonempty in at least 80% of active years, and matched-only annual shares are averaged over those years. This changes both the denominator and the school sample: 1,153 schools for the seven any-award-based measures and 526 for undergraduate-high-premium in the full-income comparison. It is a sensitivity on selected observed records, not an unbiased correction. A separate check requires at least three active orientador years.

The orientation-related qualification/income association remains about 0.085 after controlling linearly for match coverage, and about 0.103 among matched records. Any-magister/income remains close to zero. Matched-only significance is corrected separately across its own 80 tests.

## Inference, files and verification

Primary correlations use equally weighted schools and existing All-sample EB VA, with outcome-specific complete cases. The CSV also provides Spearman, broad-VA-student-weighted and unshrunk-VA correlations, Pearson confidence intervals/p-values and per-specification BH corrections. Spearman ranks use ten significant digits to preserve ties. Inference is conditional on saved VA and does not propagate its estimation uncertainty. There are no student-level regressions, no re-estimation of VA and no age/school-context adjustment in this exercise.

- [Full correlation table](C:/Users/brunem/Research/causal_schools/data/clean/titulados_staff_linkage/orientador_credentials_va/orientador_credentials_va_correlations.csv)
- [School credential shares](C:/Users/brunem/Research/causal_schools/data/clean/titulados_staff_linkage/orientador_credentials_va/orientador_credentials_school_period.csv)
- [Role-specific definition review](C:/Users/brunem/Research/causal_schools/output/reports/staff_credential_specialization_review.md)

Independent checks reconstructed 206,192 annual and 29,456 period rows from staff memberships and person credentials, reconciled roster denominators, checked all 240 main/sensitivity correlation rows and 80 coverage partial correlations, all BH adjustments, saved-VA equality and unchanged input hashes. These verify implementation, not the substantive validity of the credential taxonomy.
