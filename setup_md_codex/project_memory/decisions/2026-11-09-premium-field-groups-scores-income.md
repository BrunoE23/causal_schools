# Splitting the premium-field regression into four groups

The user requested the same regression table, splitting fields into the four
groups used earlier: Science, Law, Engineering/Manufacturing/Construction,
and Medicine+. Run four OLS linear probability models using exactly the prior
premium-outcome regression sample, covariates, standardization and covariance.
This is an outcome decomposition, not a restriction to students in premium fields.

Use existing first-enrollment Science/Law/Engineering flags; Medicine+ equals
the original premium-field indicator intersected with the Medicine classification.
This preserves the six-program positive list (medicine, pharmacy, nursing,
midwifery, medical technology, dentistry), rather than expanding to all medicine
classifications. Non-enrollees and students in other fields are zero for each
group indicator. Verify mutually exclusive components summing exactly to the
old premium-field indicator for every student.

All four models use the same 565,040 students as before, with both standardized
math and verbal scores, nine family-income decile indicators (decile 1 reference),
admission-year indicators, and attended-school-clustered CR1 covariance with
t(G-1) inference. Retain the prior admission-2026 observations. No high-score,
HE-enrollment, or school-VA condition is added. Report full income-decile tables
in percentage points with school-clustered standard errors.

In addition to normal-equation and clustered covariance checks, verify that
the four coefficient vectors sum to the previous aggregate premium-field
coefficient vector. This identity follows from OLS linearity on the identical
design and sample. Standard errors do not add; use each outcome's covariance.

All checks passed on the unchanged 565,040 students and 3,372 school clusters.
All four models retain 17 parameters. The decile-10 coefficients (percentage
points relative to decile 1) are Science -2.6816, Law +2.5385, Engineering -3.8391,
and Medicine+ -2.7131, summing to the previous premium-field coefficient -6.6954.
All original source files and prior regression coefficients remain unchanged.

Scripts: `code/codex/premium_enrollment_scores_income/03_field_groups.R` and
`04_field_table.R`. New data, reports, and table folders use task name
`premium_field_groups_scores_income`; prior regression outputs are preserved.
