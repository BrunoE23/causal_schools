# Decision: Add Grade-8 Age Control to School Value-Added Metrics

**Date:** 2026-04-26
**Status:** active

Update: the original non-interaction rule for `EDAD_ALU` in the gender-gap VA model was later changed. Current rule: include `factor(EDAD_ALU)` in the main VA model and interact `factor(EDAD_ALU)` with gender in the gender-gap VA model. The only shared, non-interacted gender-gap control is `factor(COD_COM_ALU)`, because comuna has many categories.

## Context

While reviewing the school-level observational value-added outputs, some schools that appear to serve adults or heavily over-age students were showing very poor value-added metrics.

That pattern raised an identification and interpretation concern. A very low school fixed effect in these schools may not reflect poor school performance in the usual sense. It may instead reflect strong negative selection into schools that serve students who are late in their schooling trajectory, including students who are one to three or more years behind the expected grade progression.

In the current project data, `EDAD_ALU` is interpreted as age in grade 8. That makes it a useful proxy for being over-age relative to the standard school progression and therefore a direct way to absorb part of this selection margin.

Separately, the upstream sample definition was also tightened to keep only `COD_ENSE == 110`, corresponding to regular 8th-grade students. That change is conceptually aligned with the same concern about adult or nonstandard schooling trajectories, even if in practice it may not materially change the sample very much.

## Options Considered

### Leave age out of the value-added regressions

- Pros: Simpler model and closer to the original first-pass specification.
- Cons: Risks attributing over-age selection into adult or nonstandard schools to school value added itself.
- Verdict: Rejected.

### Restrict the sample only and do not add an age control

- Pros: Removes extreme cases if the restriction is tight enough.
- Cons: Even within the retained sample, schools with more over-age students can still look artificially bad if age is not adjusted for directly.
- Verdict: Not sufficient on its own.

### Tighten the upstream sample to regular 8th-grade students

- Pros: Reduces contamination from adult or nonstandard education tracks before the observational-value step.
- Cons: By itself, it does not solve within-sample differences in over-age composition across schools.
- Verdict: Useful upstream restriction, but still not sufficient on its own.

### Add flexible age controls to the value-added regressions

- Pros: Directly adjusts for grade-8 age composition and reduces the risk of mistaking delayed schooling for poor school performance.
- Cons: Adds another high-dimensional categorical control and changes the interpretation of the adjusted school effects.
- Verdict: Selected.

## Decision

Add `factor(EDAD_ALU)` to the main school value-added regressions.

For the gender-gap value-added regressions, also include `factor(EDAD_ALU)` as a shared control, but do not interact it with gender.

At the sample-definition level, keep only `COD_ENSE == 110` upstream so the observational-value exercise is based on regular 8th-grade students.

Also restrict the sample inside the current OLS-style value-added code to the accepted grade-8 age range. In practice, the current value-added regressions are therefore estimated only on that age-restricted sample rather than on the full merged universe.

This decision is explicitly motivated by the adult-school pattern: part of the apparent poor performance of those schools is interpreted as selection of students who are late in the schooling pipeline rather than as a pure school effect.

## Rationale

The goal of the adjusted school metrics is to compare schools after accounting for observable student composition. Grade-8 age is a meaningful composition variable in this setting because it captures delayed progression through school, which is plausibly related to later test performance and enrollment outcomes.

Without this adjustment, adult schools or schools serving unusually over-age students can look mechanically bad in a way that is difficult to interpret as school performance. Including `EDAD_ALU` makes the adjusted metrics less likely to confuse delayed schooling status with school quality.

## Implementation

The current observational-value scripts now:

- include `factor(EDAD_ALU)` in the main school value-added control set,
- keep `factor(EDAD_ALU)` as a non-interacted shared control in the gender-gap value-added specification,
- continue to use the age restriction `12 <= EDAD_ALU <= 16` for the current school-value sample.

The `COD_ENSE == 110` restriction is an upstream sample rule rather than a direct filter inside `01_construct_school_rbd_values.R`. By the time data reaches `univ_gr8_df.csv`, the `COD_ENSE` field is no longer present, so the constructor inherits that restriction through the upstream build rather than reapplying it locally.

This means the relevant changes occurred in:

- `code/def_sample_frame.R`
- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`

with different roles:

- `def_sample_frame.R` is the place for the upstream `COD_ENSE == 110` sample definition
- `01_construct_school_rbd_values.R` is the place where the age restriction `12 <= EDAD_ALU <= 16` is currently imposed for the observational-value exercise

So the later OLS-style school-value regressions should be interpreted as operating on an upstream regular-8th-grade sample plus an additional VA-specific age restriction.

Relevant script:

- `code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R`

## Risks and Follow-Up

Including age improves comparability, but it may also absorb part of the effect of interest if school assignment itself influences delayed progression before grade 8 in some future design variant.

For the current observational exercise, that risk is acceptable. If age later becomes an outcome-relevant mechanism rather than a baseline composition variable, this decision should be revisited.

The practical effect of the `COD_ENSE == 110` restriction may be small if very few nonstandard cases survive into the current merged universe, but it is still worth preserving as an explicit sample-definition rule.
