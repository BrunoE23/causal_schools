# High-math restriction for projected-income and HE-enrollment OLS

The user requested the preceding two-outcome analysis restricted to math above
one SD, as in the earlier descriptive analysis. Preserve that exact convention:
`z_math >= 1`, using the existing within-admission-year standardization in the
broad panel. Apply it to the preceding complete-score/family-income regression
sample; do not restandardize in the selected sample or re-rank income deciles.
The assistant stated the inclusive threshold before estimation.

Keep the full cubic math/verbal polynomial (total degree <= 3), nine income
indicators with D1 reference, and admission-year indicators. HE enrollment uses
all selected high-math students, including non-enrollees. Log projected income
uses only selected HE enrollees. Retain valid projected monthly CLP income at
first HE enrollment and audit that every enrollee has it. Admission-2026 records
remain in the enrollment OLS if they pass the cutoff; they have no recorded HE
enrollment. Neither model is restricted by school-VA availability.

Retain school-clustered CR1 covariance with sample-specific N, K and G and
t(G-1) inference. Full and no-income models use identical students within each
outcome. Income's reported fraction remains
`(R2_full - R2_without_income) / R2_full`, using ordinary unadjusted R2.
This is incremental explanatory power after scores/year, not a causal share
or an allocation of explanatory power shared by income and scores.

Extend scripts 11 and 12 in `code/codex/premium_enrollment_scores_income/` with
the explicit `--high-math` flag. Default runs retain the preceding unrestricted
analysis and output location. Flagged runs write separate outputs under
`high_math_income_enrollment_cubic_income_contribution`. Preserve earlier results.
Verify exact selected IDs, threshold and unchanged score units, coverage,
normal equations, actual clustered covariance, nested-model projection
identities, and unchanged source checksums. No plots are requested.

The cutoff selects 93,423 students (2,735 schools), of whom 84,901 are HE
enrollees (2,689 schools). All enrollees have valid projected income. There are
8,522 non-enrollees, including 840 admission-2026 records. No student is exactly
at z_math = 1, so strict versus inclusive cutoff yields the same selected IDs.

All four full/reduced fits pass the normal-equation, independently reconstructed
clustered-covariance, nested-R2/projection, and sample checks. For projected
income, full R2 is 0.1271576, reduced R2 is 0.1266258, and income's share is
0.4182%. For enrollment, full R2 is 0.1967888, reduced R2 is 0.1960322, and
income's share is 0.3845%. Decile 10 versus 1 is about +0.011 log points for
projected income and -2.25 percentage points for enrollment. Original inputs
and the previous unrestricted outputs remain unchanged.
