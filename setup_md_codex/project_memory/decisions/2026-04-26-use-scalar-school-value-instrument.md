# Decision: Use Scalar School-Value Offer Instrument for First-Pass IV

**Date:** 2026-04-26
**Status:** active

## Context

The project could treat school attendance as a high-dimensional treatment vector with one indicator for each school. In that setup, the instrument set could also be high-dimensional, with offer indicators for many schools.

The first-pass empirical strategy instead uses pre-specified observational school values to reduce school attendance into a scalar treatment:

`D_i = V_{S_i}`

where `V_s` is the observational value of school `s`.

## Options Considered

- Estimate a fully flexible vector of school effects using school indicators.
- Use a high-dimensional vector of school-specific offer instruments.
- Use a scalar school-value treatment and scalar offer instrument based on the value of the offered school.

## Decision

Use the scalar school-value approach for the first-pass IV.

The endogenous treatment is:

`D_i = V_{most_time_RBD}`

The scalar offer instrument is based on:

`Z_i = V_{rbd_treated_1R}`

with the current outside-support normalization documented separately.

## Rationale

The lottery design may shift students across schools, but it generally does not identify a separate causal effect for every school. The scalar index approach asks whether lottery-induced movement toward schools with higher pre-specified observational value improves student outcomes.

This makes the estimand interpretable as the causal return to moving along a specified school-value index, rather than the causal effect of attending any particular school.

## Implementation

The conceptual specification is documented in:

- `setup_md_codex/project_memory/empirical_methods.md`

The current construction and estimation code is in:

- `code/codex/scalar_school_value_iv/01_build_scalar_school_value_iv_df.R`
- `code/codex/scalar_school_value_iv/02_run_scalar_school_value_iv.do`

## Risks and Follow-Up

Results depend on the chosen school-value index. Math, verbal/language, STEM enrollment, adjusted value-added, unadjusted means, and gender-gap values each define a different scalar treatment.

This approach should not be described as estimating a causal effect of each individual school. It estimates the causal effect of lottery-induced movement along the chosen school-value index.
