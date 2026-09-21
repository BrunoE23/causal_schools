# Decision: Describe baseline characteristics of high-VA compliers

**Date:** 2026-09-21
**Status:** active

Bruno requested code for Section 3.2.1 of hoee_methods, using the five selected
top-quartile EB school-VA indicators to describe gender, baseline achievement,
income decile, and parent education. The scope excludes subsequent outcomes
and Section 3.2.2 potential-outcome distributions.

Use D=high-VA most-attended school and Z=high-VA first-round offered school;
q=sum_j p_ij*high_j from unrounded saved simulated assignment probabilities.
The main estimator uses Z-q moments, with a four-df natural cubic spline in
both 2SLS stages as a sensitivity. Exact q itself suffices for recentering;
flexibility is not required to make a known propensity linear. Each VA defines
its own instrument, risk score, and complier population. The estimand is the
q(1-q)-weighted baseline complier mean under valid assignment and monotonicity.

Implementation default: timely applicants in assignment cohorts 2018-2020,
configurable by COMPLIER_YEARS. Gender is female share; achievement is grade-4
math and language separately; SES uses existing imputed income decile and
mother/father years of education separately. No new imputation is performed.
This window follows the recent three-cohort analysis and avoids the poor
2021 baseline coverage, while retaining the previously selected observational
EB school definitions (not RC-VAM estimates).

Require full classified probability support, probability mass one, interior q,
known observed treatment/instrument, and a nonmissing characteristic. Do not
set missing VA to low or renormalize probabilities. Simulated unmatched and
observed no-first-round offer (RBD 0) are Z=0, consistent with the current
project convention; the existing no-offer interpretation issue remains open.
All sample exclusions are saved. Missing attendance selection may affect IV
interpretation and is not justified by the probability control itself.

Use heteroskedastic moment-sandwich SEs for main ratios and regression-derived
robust SEs for spline IV. These condition on saved VA categories and simulated
q and do not propagate their uncertainty or model assignment dependence.
Do not report ordinary applicant averages as complier means. The script saves
applicant averages separately for comparison, plus untreated-state estimates.

Implementation and validation: `code/codex/high_va_compliers/`.

## Centered reporting

Bruno subsequently requested subtracting the population mean. Implementation
uses, as an explicitly stated default, the equally weighted mean of the same
eligible applicants with the characteristic observed, separately by VA and
characteristic. This is not a national mean or a mean of all timely applicants.
The main treated-state complier mean minus this reference is reported as a
baseline composition difference, not a causal gain. Both component means and
sample sizes remain available. Joint influence-function SEs include estimation
of the applicant mean and its covariance with the complier mean; uncentered
SEs are not reused. The original absolute-mean outputs are preserved.
