# Main results by school continuation

## assignment risk

| Group | All applicants | With assignment risk | Fraction with risk | Probability coverage |
| --- | --- | --- | --- | --- |
| School ending | 250,564 | 135,477 | 54.07% | 100.0% |
| Voluntary | 38,868 | 24,016 | 61.79% | 100.0% |

## first stage

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | 0.452*** (0.009) | 0.452*** (0.007) | 0.438*** (0.006) | 0.418*** (0.007) | 0.440*** (0.006) | 0.259*** (0.010) | 0.488*** (0.008) |
| Voluntary | 0.422*** (0.019) | 0.386*** (0.016) | 0.353*** (0.015) | 0.340*** (0.016) | 0.372*** (0.015) | 0.378*** (0.025) | 0.423*** (0.018) |

## pass through

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | 1.139*** (0.105) | 0.992*** (0.105) | 0.501*** (0.054) | 0.582*** (0.105) | 0.748*** (0.125) | 0.554*** (0.135) | 0.875*** (0.093) |
| Voluntary | 1.393*** (0.238) | 1.198*** (0.261) | 0.419** (0.167) | 0.518* (0.281) | 0.772** (0.339) | 0.953*** (0.236) | 0.751*** (0.238) |

## N

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | 73,700 | 75,040 | 111,952 | 111,952 | 111,952 | 75,470 | 75,470 |
| Voluntary | 16,461 | 16,719 | 21,196 | 21,196 | 21,196 | 16,789 | 16,789 |

## first stage F

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | 2815.8 | 3742.4 | 5445.1 | 3774.0 | 4819.7 | 666.6 | 3699.9 |
| Voluntary | 479.4 | 563.7 | 527.1 | 460.4 | 653.5 | 235.1 | 566.2 |

## expected all applicants

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | -0.108 | -0.087 | -4.652 | 0.522 | -1.126 | -3.246 | 0.433 |
| Voluntary | -0.089 | -0.065 | -1.233 | 1.518 | -0.173 | -2.460 | 0.413 |

## expected iv sample

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | -0.083 | -0.057 | -2.575 | 1.348 | -0.086 | -3.054 | 0.168 |
| Voluntary | -0.074 | -0.043 | 0.291 | 2.073 | 0.532 | -2.232 | 0.274 |

## covered probability

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | 91.4% | 91.4% | 91.4% | 91.4% | 91.4% | 91.4% | 91.4% |
| Voluntary | 98.1% | 98.1% | 98.2% | 98.2% | 98.2% | 98.1% | 98.1% |

## p psi zero

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | <0.001 | <0.001 | <0.001 | <0.001 | <0.001 | <0.001 | <0.001 |
| Voluntary | <0.001 | <0.001 | 0.012 | 0.065 | 0.023 | <0.001 | 0.002 |

## p psi one

| Group | Math | Verbal | Exam taking | Fin. aid app. | Higher-ed enrollment | High-premium inst. | High-premium field |
| --- | --- | --- | --- | --- | --- | --- | --- |
| School ending | 0.188 | 0.943 | <0.001 | <0.001 | 0.044 | <0.001 | 0.180 |
| Voluntary | 0.098 | 0.448 | <0.001 | 0.086 | 0.501 | 0.843 | 0.296 |

## Definitions and verification

- First-stage and pass-through specifications are the paper's Tables 4 and 5, estimated separately within each origin-school group. Both use timely applicants with assignment risk in 2018-2020; school VA is estimated on the original 2017-2020 sample and is held fixed across groups.
- The first stage regresses attended-school EB VA on first-round offered-school EB VA. The pass-through regression instruments attended VA with offered VA. Both control for expected VA in that outcome, cohort indicators, grade-4 math and verbal scores, gender indicators and age indicators. Outcome-specific missingness and exam-taking restrictions match the main paper. No-offer instrument values and all VA normalizations are unchanged.
- Parentheses contain heteroskedasticity-robust standard errors, as in the main paper. Stars use the original normal-reference tests: *** p<0.01, ** p<0.05, * p<0.10. First-stage F is the squared robust t statistic for the single excluded instrument. These subgroup point estimates alone do not establish a statistically significant difference between groups.
- Assignment-risk fractions use all applicants in each Table 2 subgroup before complete-control and outcome restrictions. School ending means the grade-8 origin has no SAE offering above grade 8; voluntary means it offers grade 9. These operational labels describe continuation availability, not observed motives or actual moves.
- 4,766 applicants have unresolved/other origin offerings, including 3,140 with assignment risk. They are excluded from both subgroup rows. The original pooled results include them when eligible.
- Expected VA is the mean of sum_j p_ij times the original EB VA_j, without recentering within either group. The main expected-VA table uses all subgroup applicants; the sensitivity uses the exact outcome-specific IV estimation samples. Math and verbal expected VA use outcome-SD units. Binary-outcome expected VA is multiplied by 100 and expressed in percentage points. Pass-through and first-stage coefficients are not rescaled.
- Expected VA reflects portfolios, priorities and assignment probabilities together; it is not a pure measure of ranked preferences. As in the main paper, unmatched outcomes and school-options without measured VA contribute zero, and probability mass is not renormalized. The coverage table shows the mean assignment probability on schools with measured VA, so differences in coverage can be assessed.
- Verification: all 42 original pooled coefficient/SE/F/N statistics reproduce the paper; all seven reconstructed expected-VA measures match the cached main inputs for 162,633 risk applicants. Subgroup counts exactly partition each original outcome-specific estimation sample, allowing for unresolved/other origins. Independent matrix calculations reproduce 2SLS and first-stage robust SEs for math in both groups.
