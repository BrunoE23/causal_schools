# Decision: Use Cubic Baseline-Score Controls in VA

Date: 2026-04-26

## Context

The controlled observational value-added metrics use student-level controls to adjust school fixed effects for observed differences in student composition.

The previous specification included baseline SIMCE math and language scores linearly:

- `z_sim_mat_4to`
- `z_sim_leng_4to`

These baseline achievement measures may have nonlinear relationships with later score outcomes and enrollment outcomes. The working view is that flexible controls for baseline achievement are consistent with the way the relevant value-added literature is typically specified; citations still need to be added later.

## Decision

Use third-order polynomials in the baseline SIMCE controls for the school value-added regressions:

- `z_sim_mat_4to`
- `I(z_sim_mat_4to^2)`
- `I(z_sim_mat_4to^3)`
- `z_sim_leng_4to`
- `I(z_sim_leng_4to^2)`
- `I(z_sim_leng_4to^3)`

The rest of the main control set remains:

- `factor(cohort_gr8)`
- `factor(GEN_ALU)`
- `factor(EDAD_ALU)`
- `factor(COD_COM_ALU)`

For gender-gap value-added regressions, allow the baseline-score polynomial terms, cohort fixed effects, and grade-8 age fixed effects to vary flexibly by gender. Keep student comuna as the shared, non-interacted control because interacting comuna by gender would add many degrees of freedom.

## Rationale

The goal is to avoid forcing the adjustment for prior achievement to be linear. This is especially important because the school VA metrics are intended to summarize school-level observational performance after accounting for student baseline differences.

## Implementation

Implemented in:

- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`

For the current estimation-development pass, controlled VA is only run for `z_year_math_max`, `z_year_leng_max`, `z_year_leng_math_total`, and `stem_enrollment_m1`. Gender-gap VA is only run for `gender_gap__z_year_math_max` and `gender_gap__stem_enrollment_m1`.

The output school-value CSV should be regenerated after this change.
