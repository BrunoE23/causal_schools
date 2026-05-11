# Decision: Use Subvenciones-a-EE for first-pass school funding measure

Date: 2026-04-26

## Context

We want a school-level pesos-per-student measure that can be used as a scalar school characteristic in the same IV framework as the observational value-added measures.

The raw finance folder contains both `Subvenciones-a-EE` and `Reliquidacion-FICOM` files.

## Decision

Use `Subvenciones-a-EE` files for 2017-2021 to construct the first-pass measure of public funding per student.

Exclude `Reliquidacion-FICOM` from the first pass.

## Rationale

The current lottery-supported school set contains no `Particular Pagado` schools. FICOM is therefore not the relevant resource margin for the schools identified by the lottery design. The `Subvenciones-a-EE` files are better aligned with the public funding received by the schools in the SAE support.

## Current Measure

The first-pass school-year measure is:

`annual public funding per student = annual sum of selected monthly subsidy/payment components / average monthly enrollment`

Monetary values are converted to 2021 pesos using annual CPI index values for Chile from World Bank indicator `FP.CPI.TOTL`, sourced from IMF International Financial Statistics.

The first-pass 2017-2021 school scalar is:

`enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021`

## Caveats

- the measure is public funding received, not total school expenditure
- the component list should be reviewed before treating this as final
