# High-math cubic OLS for the four main premium-enrollment outcomes

The user requested the preceding high-math analysis for the main four outcomes:
premium institution, premium field, either (inclusive union), and both.
Use the exact 93,423 high-math students from the preceding HE-enrollment model,
including 8,522 non-enrollees and 840 admission-2026 students. The cutoff remains
original `z_math >= 1`, with scores standardized within admission year in the
original broad panel. Do not condition on HE enrollment, renormalize scores,
re-rank household income, or add a school-VA-availability requirement.

All four OLS models include the full total-degree-three math/verbal polynomial,
nine household-income-decile indicators with D1 reference, and admission-year
indicators. Outcome definitions and non-enrollee zeros remain unchanged.
The main income contribution is `(R2_full - R2_without_income)/R2_full`, where
the reduced model drops only the income indicators on exactly the same sample.
Use ordinary unadjusted in-sample R2. This is income's incremental explanatory
power after scores/year, not a causal share or an allocation of their shared
explanatory power. Save partial R2 separately.

Retain attended-school-clustered CR1 covariance with 2,735 schools, K=24 full
parameters and K=15 reduced parameters, and t(G-1) inference. Report all income
decile coefficients and average local score derivatives in percentage points,
with SEs from the actual clustered regression covariance. Scores' average
derivatives include all cubic and interaction terms and are not finite one-SD
changes. Do not clip LPM predictions.

Extend scripts 09 and 10 in `code/codex/premium_enrollment_scores_income/` with
`--high-math-main`. Flagged runs fit only the four main outcomes and write to
`high_math_main_cubic_income_contribution`; default runs retain the previous
eight unrestricted outcome analysis. Preserve previous outputs. Verify exact
ID equality with the previous high-math HE sample, both models' normal equations
and actual clustered covariance, nested-model projection identities, the
coefficient identity either = institution + field - both, and source checksums.

All eight full/reduced fits pass verification, including exact sample equality
with the prior high-math HE-enrollment model. Full R2 for institution/field/
either/both is 0.1667378/0.0755618/0.1308117/0.0992086; reduced R2 is
0.1108203/0.0573408/0.1286892/0.0927328. Income's incremental share of explained
variation is 33.5362%/24.1140%/1.6226%/6.5274%. Decile 10 versus 1 coefficients
are +35.01/-17.24/+4.23/+13.54 percentage points, respectively. The source
checksums and all earlier result files remain unchanged.
