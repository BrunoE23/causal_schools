# Decision: Current paper specification and draft conventions

**Date:** 2026-05-31
**Status:** active

## Context

The May draft was revised after the value-added, expected-VA IV, heterogeneity, and orthogonal-value-added exercises converged into a coherent paper specification.

The main goal was to make the manuscript, tables, figures, and project memory consistent before a final author pass.

## Options Considered

Several choices were discussed during this pass:

- whether to keep the older wide probability-control workflow or move the paper to the expected-VA scalar risk-control workflow
- whether to retain the k-support restriction in main estimates
- whether higher-education outcomes should be analyzed in the broad panel or in an admission-exam-taker sample
- whether heterogeneity should be based on grade-8 SIMCE or grade-4 SIMCE
- whether institutional quality should use the accreditation-years interaction or the institution-only measure
- whether to keep Male/Female value-added estimates active in the main workflow
- whether orthogonal STEM value added should use leave-one-out cohort measures or the four-cohort construction
- how much assignment-algorithm replication detail should appear in the paper

## Decision

The current paper-facing workflow uses All-sample school value added only.

Male/Female sample-specific value added and gender-gap value added are on hold unless explicitly requested.

The current main scalar-IV workflow uses expected value added as the scalar assignment-risk control:

`expected_VA_i = sum_s p_is * V_s`

The older wide `prob_<rbd>` and `iszero_<rbd>` probability-control workflow, including the k-support restriction, is retained only as a legacy or robustness path.

The paper-facing VA and IV samples restrict to students with an observed math or language national admission-test score.

Within this exam-taker sample, students without observed higher-education enrollment are coded as zero for unconditional higher-education outcomes, while genuinely missing accreditation inputs among observed enrollment records remain missing.

The four main paper outcomes are:

- math achievement
- language achievement
- STEM enrollment
- institutional quality, measured by institutional accreditation years

The current heterogeneity exercise uses grade-4 SIMCE math quintiles.

This keeps the heterogeneity variable pre-treatment and available for the full exam-taker analysis sample.

The heterogeneity figure should show points by quintile without connecting lines.

The orthogonal-value-added exercise is an additional analysis, not a replacement for the earlier VA code.

It uses the four available cohorts to construct math and STEM school value added and then residualizes STEM value added on math value added using the EB-free cross-cohort covariance logic in Rose-style orthogonal VA.

For now, the paper does not use leave-one-out cohort value added for this exercise because four cohorts would make the three-cohort leave-one-out measures noisier.

The paper should describe the SAE assignment-algorithm diagnostic succinctly as reproducing more than 99% of observed first-round assignments in each of the four cohorts.

The recomputed year-specific rates are:

- 2018: 99.4709%
- 2019: 99.2697%
- 2020: 99.1263%
- 2021: 99.3878%

The manuscript title is:

`Do High Schools Shape College Choices? Evidence from Centralized Assignment in Chile`

The abstract should remain under 250 words.

As of this decision, the abstract is 229 words.

Recommended keywords are:

`school value added, higher education, college major choice, centralized school assignment, Chile`

When editing manuscript prose, keep the `.tex` source formatted as one sentence per line.

## Rationale

The expected-VA workflow is simpler and closer to the current estimand because it controls for the scalar expected value of the same school-value metric used as the treatment and instrument.

It avoids carrying a large set of school-specific probability controls into every table and removes the need to restrict the main school sample to the k-support set.

Restricting paper-facing analyses to exam takers makes the achievement and higher-education outcome samples more comparable.

It also makes zero-coded higher-education outcomes easier to interpret as outcomes among students who reached the national admission-test margin.

Grade-4 SIMCE quintiles are preferred over grade-8 SIMCE for the main heterogeneity figure because they are pre-treatment and available across the paper's full analysis cohorts.

Institutional accreditation years are preferred for the fourth main outcome because they give a simple institution-quality measure and avoid adding the program-accreditation interaction to the main story.

The orthogonal VA result answers a distinct mechanism question: whether STEM enrollment value added is just math-achievement value added in another form.

The current result supports the interpretation that STEM value added captures a dimension of school value that is not mediated through the measured math value-added component.

## Implementation

Key current scripts and outputs include:

- `code/codex/scalar_school_value_iv/08_run_expected_va_scalar_iv.R`
- `code/codex/scalar_school_value_iv/12_run_expected_va_grade4_quintile_all_sample.R`
- `code/codex/results_section/01_make_results_section_outputs.R`
- `code/codex/orthogonal_school_va/01_construct_orthogonal_school_va.R`
- `code/codex/orthogonal_school_va/02_run_orthogonal_va_iv.R`
- `output/tables/results_section/main_four_va_metrics.tex`
- `output/figures/results_section/simce4_math_quintile_four_metrics.png`
- `output/figures/results_section/math_stem_va_correlation.png`
- `output/tables/orthogonal_school_va/orthogonal_va_iv_2x2.tex`
- `writing/may_draft/may_draft.tex`

The current manuscript uses:

- main four-VA table for math, language, STEM enrollment, and institutional quality
- grade-4 math quintile heterogeneity figure
- math/STEM VA correlation plot
- orthogonal 2x2 IV table

## Risks and Follow-Up

The admission-exam-taker restriction changes the interpretation of higher-education outcomes relative to a full-cohort unconditional enrollment outcome.

Future versions should decide whether to add a robustness table in the broader sample.

The orthogonal-value-added exercise is not leave-one-out by cohort.

With only four cohorts, this is currently a precision choice, but a future robustness exercise could compare leave-one-out or cross-fitted versions.

The assignment-algorithm replication mismatch is small but not zero.

If the paper adds an appendix on assignment replication, it should report the year-specific rates and discuss the remaining institutional details that are difficult to recover from public files.
