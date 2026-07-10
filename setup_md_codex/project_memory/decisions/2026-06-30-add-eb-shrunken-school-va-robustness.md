# Decision: Add EB-shrunken school value-added robustness path

**Date:** 2026-06-30
**Status:** active

## Context

The current paper-facing scalar-IV workflow uses adjusted observational school value added as the scalar school-quality index.
The IV then tests whether lottery-induced movement toward schools with higher observational value added improves the corresponding outcome, controlling for expected value added in the student's assignment portfolio.

Because school fixed effects are noisy, especially for schools with smaller regression samples or noisier outcomes, we decided to add an Empirical-Bayes robustness path.
The goal is to shrink stage-1 observational school value-added estimates before using them as the scalar treatment, instrument, and expected-risk control in the existing IV design.

## Decision

Add a separate EB-shrunken school-VA workflow without replacing the current main expected-VA specification.

The workflow is:

1. Rerun `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`.
2. Use the newly exported stage-1 uncertainty columns to construct EB-shrunken school values.
3. Rerun the expected-VA scalar IV using the EB-shrunken values.

The EB workflow lives in:

- `code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R`
- `code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R`
- `code/codex/empirical_bayes_school_va/README.md`

The current main workflow remains:

- `code/codex/scalar_school_value_iv/08_run_expected_va_scalar_iv.R`

## Implementation

The school-value constructor now exports:

- `controlled_value_added_se`
- `controlled_value_added_resid_sd`
- `controlled_value_added_se_method`

The SE is the regression-derived uncertainty of the student-weighted centered school effect:

`controlled_value_added_se = SE(controlled_value_added_centered_student)`

Because `fixef()` returns fixed-effect point estimates but not their SEs, the constructor runs an auxiliary regression with `school_rbd` entered as explicit dummies and the remaining fixed effects still absorbed.
The implementation uses `lfe::felm()` and `lfe::getfe(se = TRUE)` with a custom estimable function for the student-weighted centered school effect.
The regression uncertainty is therefore extracted for `school FE - student-weighted mean(school FE)`, not for a raw reference-normalized fixed-effect level.
The current method label is `lfe_getfe_school_rbd_centered_student_iid_bN100`.
`controlled_value_added_resid_sd` is kept only as a diagnostic.

Implementation-standard note:
do not replace this regression-derived SE with residual-spread-over-sqrt-n or other convenience proxies.
For research code in this project, no se toman atajos: if the needed stage-1 uncertainty is not directly returned by `fixef()`, it must be derived from an equivalent regression/VCOV object or discussed before implementation.

For each outcome, the EB constructor estimates:

`tau_m^2 = max(weighted Var(Vhat_s^m) - weighted mean(se_s^2), 0)`

where weights are `n_students_regression`.

The school-specific reliability is:

`lambda_s^m = tau_m^2 / (tau_m^2 + se_s^2)`

The EB school value is:

`V_EB_s^m = mean_m + lambda_s^m * (Vhat_s^m - mean_m)`

The output is recentered to preserve the student-weighted zero-mean convention.

The EB IV then defines:

- `A_i^EB = V_EB_{most_time_RBD}^m`
- `O_i^EB = V_EB_{rbd_treated_1R}^m`
- `E_i^EB = sum_s p_is V_EB_s^m`

and instruments `A_i^EB` with `O_i^EB`, controlling for `E_i^EB`.

## Rationale

This path follows the idea that when estimated school value added is used as a scalar right-hand-side index, shrinkage can improve the signal-to-noise ratio of that index.
It is conceptually distinct from hybrid causal EB or IV VAM.

The EB path does not estimate a separate causal effect for each school.
It asks whether an EB-shrunken observational school-value index has causal predictive content along the SAE lottery margin.

## Risks and Follow-Up

Because EB shrinkage compresses the scale of school values, coefficients using EB-shrunken VA should be interpreted together with the SD of the EB index.
The EB pass-through coefficient can be mechanically larger than the unshrunken coefficient if the same causal movement is measured against a more compressed scale.

If EB becomes the main specification rather than a robustness path, the project should revisit whether to use a cross-fitted/repeated-split reliability approach in addition to regression-derived uncertainty.

As of the program-income implementation pass, the Stata license is temporarily unavailable.
The EB stage-1, shrinkage, and expected-VA IV regressions therefore have R implementations and should be run in R until Stata is restored.

The orthogonal math/STEM value-added exercise should continue to use the EB-free cross-cohort covariance logic.
The covariance of EB posterior means should not be used as direct evidence about latent covariance between school effects.
