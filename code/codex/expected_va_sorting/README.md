# Expected-VA sorting

This descriptive exercise uses the existing five primary EB-shrunken
observational VA measures. It constructs each timely 2018--2020 SAE applicant's
expected VA from simulated assignment probabilities and regresses standardized
expected VA on standardized baseline math, verbal, income decile, mother and
father education, a female indicator, and assignment-cohort fixed effects.

All five regressions use one common complete-case sample. Applicants must have
probability mass one and every positive-probability school option must have a VA
estimate in every dimension; simulated unmatched assignments have centered VA
zero. Robust SEs, R2, adjusted R2, and partial R2 for achievement and SES blocks
are reported. The exercise describes ex ante sorting into assignment
opportunities encoded by applications, priorities, and the mechanism. It is not
a causal effect and is distinct from realized sorting into attended schools.

Run `01_run_expected_va_sorting.R` from the repository root. Outputs are under
`data/clean/expected_va_sorting` and `output/tables/expected_va_sorting`.
