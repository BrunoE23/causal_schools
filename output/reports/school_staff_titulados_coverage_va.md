# School staff coverage in titulados and correlations with VA

## Definition

Main measure: for each school, role and year, the fraction of identified role members with at least one exact-MRUN qualification match anywhere in the 2007-2025 reporting-cohort database. Average those fractions equally over active-role years in 2018-2024. A person counts once per school-role-year, even with several appointments or awards. The same person can legitimately appear at different schools or in different roles. Teachers means regular youth-HS classroom teachers under the established definition; orientadores and leadership include primary or secondary functions.

This is database linkage coverage, not historical qualification possession or staff quality. A later award can establish a full-database match, but cannot establish an as-of credential. Separate ANY_ASOF and UG_ASOF measures enforce reporting and award years no later than the staff year. UG_DB requires an undergraduate match anywhere in the database.

Require known role counts in all seven staff years for the main period measure, consistent with the existing staff-VA analysis. At least one active-role year is sufficient for this coverage diagnostic; a three-active-year sensitivity is also reported. Zero staff means undefined coverage, never zero matching. Unknown role counts remain missing. All 3,682 schools are retained in the merge-ready file for each role, including missing rates and their status. The observed-years average is retained separately for partial rosters, but does not enter the main plots/correlations.

Alternative aggregation: pooled matched person-years divided by staff person-years, and matched unique people divided by all unique people ever at that school in the role. These are saved and used in sensitivity correlations; neither silently replaces the equal-year primary measure.

## Distribution across schools

| Role | Schools | Mean | P10 | Median | P90 | Zero coverage | Complete coverage |
| --- | --- | --- | --- | --- | --- | --- | --- |
| HS teachers | 3003 | 71.7% | 53.6% | 72.4% | 89.0% | 1 | 31 |
| Orientadores | 2111 | 61.8% | 0.0% | 85.7% | 100.0% | 519 | 1013 |
| Leadership | 3550 | 56.7% | 0.0% | 59.5% | 100.0% | 521 | 808 |

Each school receives equal weight. The orientador distribution is highly discrete: small role headcounts often produce zero or complete matching. A school with no observed orientador is excluded from this distribution rather than added to its zero bin.

![Distribution](C:/Users/brunem/Research/causal_schools/output/figures/titulados_school_coverage/school_staff_match_distribution.png)

### Coverage definitions

| Role | Any, entire DB | UG, entire DB | Any, by staff year | UG, by staff year |
| --- | --- | --- | --- | --- |
| HS teachers | 71.7% | 63.8% | 69.0% | 62.8% |
| Orientadores | 61.8% | 30.4% | 58.5% | 29.4% |
| Leadership | 56.7% | 19.7% | 52.6% | 19.2% |

All four definitions use the same eligible schools within role. The differences reflect award type and temporal coverage, not a changed school sample.

### School availability

| ROLE | STATUS | N_SCHOOLS |
| --- | --- | --- |
| HS teachers | observed | 3003 |
| Leadership | observed | 3550 |
| Orientadores | observed | 2111 |
| HS teachers | role_never_observed | 574 |
| Orientadores | role_never_observed | 1451 |
| HS teachers | incomplete_role_count_coverage | 105 |
| Leadership | incomplete_role_count_coverage | 119 |
| Orientadores | incomplete_role_count_coverage | 120 |
| Leadership | role_never_observed | 13 |

## Correlations with school VA

Unweighted Pearson correlations with the existing All-sample empirical-Bayes school VA estimates. Each outcome uses its own pairwise complete school sample; no imputation or VA re-estimation. These are unadjusted descriptive correlations, not causal effects, and do not establish that missing qualifications are random.

| VA outcome | HS teachers | Orientadores | Leadership |
| --- | --- | --- | --- |
| Math score | -0.103 | 0.009 | -0.005 |
| Language score | -0.066 | 0.017 | -0.017 |
| Admission-exam taking | 0.102 | 0.074 | 0.027 |
| Higher-ed enrollment | 0.113 | 0.059 | 0.024 |
| STEM enrollment | -0.014 | 0.057 | 0.041 |
| High-premium field | -0.017 | 0.061 | 0.015 |
| High-premium institution | -0.124 | 0.004 | 0.020 |
| Projected income (full) | -0.018 | 0.052 | 0.016 |
| Projected income (field) | -0.034 | 0.004 | 0.014 |
| Projected income (institution) | -0.025 | 0.008 | 0.021 |
| Program accreditation years | 0.015 | -0.007 | 0.010 |
| Institution accreditation years | 0.000 | 0.006 | 0.035 |

![VA correlations](C:/Users/brunem/Research/causal_schools/output/figures/titulados_school_coverage/school_staff_match_va_correlations.png)

Most correlations are small. Teacher coverage has modest negative Pearson associations with high-premium-institution and math VA. For math the teacher Spearman correlation is near zero, so the negative Pearson association is not a strong monotonic pattern throughout the distribution. Orientador and leadership coverage have weak associations with the main income and achievement VA measures.

Small does not mean statistically indistinguishable from zero: the teacher links to higher-ed enrollment and high-premium-institution VA, and the orientador links to high-premium-field and projected-income VA, survive the stated within-role/definition BH correction. Undergraduate-only teacher coverage has a stronger negative link to high-premium-institution VA: r=-0.210 for the entire DB, and r=-0.218 for as-of undergraduate coverage. These results concern matching coverage, not the eight credential characteristics; their associations with VA have not been computed in this analysis.

The full correlation CSV includes N, Pearson r and conventional 95% confidence intervals/p-values, Spearman r, student-weighted r, and unshrunk-VA r. Student weights are the fixed broad VA-school student counts, not annual enrollment. Spearman ranks use 10 significant digits to preserve intended ties. BH corrections are within role and coverage definition across the 12 outcomes. Inference is conditional on estimated VA and does not propagate VA estimation uncertainty. No adjusted regressions are claimed in this diagnostic.

## Files and verification

- [School-level rates](C:/Users/brunem/Research/causal_schools/data/clean/titulados_staff_linkage/school_coverage/school_staff_match_rates.csv)
- [Detailed period rates and alternative denominators](C:/Users/brunem/Research/causal_schools/data/clean/titulados_staff_linkage/school_coverage/school_staff_match_period_long.csv)
- [All correlations and sample sizes](C:/Users/brunem/Research/causal_schools/data/clean/titulados_staff_linkage/school_coverage/school_staff_match_va_correlations.csv)
- [Alternative aggregation correlations](C:/Users/brunem/Research/causal_schools/data/clean/titulados_staff_linkage/school_coverage/school_staff_match_va_robustness.csv)

Independent Python verification reconstructs active annual numerators/denominators from person-school memberships and match flags; checks all 309,288 annual and 44,184 period rows against roster/aggregation rules; checks all distribution summaries, 144 main and 108 sensitivity correlation rows, BH adjustments and source-VA equality. Source input hashes are unchanged. Records start in 2007, so missing matches can reflect older or foreign degrees rather than no qualifications.
