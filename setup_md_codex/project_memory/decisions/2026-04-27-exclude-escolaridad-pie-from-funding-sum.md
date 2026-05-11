# Exclude `escolaridad_pie` from the school funding sum

Date: 2026-04-27

## Decision

When constructing the school public funding per student measure from
`Subvenciones-a-EE`, exclude `escolaridad_pie` from the component sum.

## Reason

`escolaridad_pie` is already contained in `escolaridad`. Including both
components would double-count PIE-related schooling payments.

## Implementation

`code/codex/school_expenditure_values/01_construct_public_funding_per_student.R`
now keeps `escolaridad` and removes `escolaridad_pie` from
`component_candidates`.

## Consequence

Existing money-IV regression outputs that were produced before this correction
should be treated as stale until the funding data, money-IV dataframe, and Stata
money-IV regressions are rerun.
