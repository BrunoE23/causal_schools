# Attended and expected EB school VA by prioritario status

Run `01_compare.R` from the repository root with R, data.table and fixest,
after the two baseline-by-VA-quintile scripts. It reuses the 294,198-student
full timely SAE-applicant input and its four validated current-main E values.
It reads only three columns from each 2018-2020 SAE B1 file (about 56 MB total),
retaining Grade 9 records. Priority is matched on mrun and application year.
Missing priority is not non-prioritario; conflicting statuses, if any, remain
missing and are audited. Inputs and source files are never modified.

Outcomes are current unified R All-sample EB math, verbal, high-premium field
and high-premium institution VA. Attendance is linked by most-time school RBD.
The current VA source must match that used to construct the cached E values.
For each outcome, use one common sample with known priority and finite
attended VA/E for raw attended means, adjusted attended means and E means.
No income, baseline-completeness, PAES-taking or at-risk-only restriction is
introduced. This differs from older income-decile descriptive tables, which
required income and common coverage across seven VA dimensions.

The requested adjustment is OLS `attended_va ~ prioritario + expected_va`,
with the corresponding outcome's E as the only additional control. The raw
model is `attended_va ~ prioritario`; E is also summarized by priority group.
Adjusted group means hold E at its pooled common-sample mean, with a common
linear E slope. No additional controls are silently added. Save regression-
derived covariance clustered by attended-school RBD, treating EB indices as
fixed; VA-estimation uncertainty is not propagated. Main displayed results
are descriptive point estimates, not causal priority effects.

Clean-data outputs under `data/clean/prioritario_attended_expected_va/`
contain group means/gaps, adjusted means, coefficients and cluster SEs,
sample/priority/source coverage, and E means for all known-priority applicants
without the attended-VA coverage restriction. The HTML table is under
`output/tables/prioritario_attended_expected_va/`. Math/verbal are in score SD;
field/institution are displayed in percentage points of school VA.
The HTML includes both the matched-sample E means and a separate all-applicant
E panel; the combined CSV also carries both sets of E means/gaps.

Checks verify unique roster keys, binary status, unchanged source values,
same regression Ns, raw-gap equality to group mean differences, exact
decomposition of the raw attended gap into the E-adjusted gap plus the E
slope times the E gap, and preservation of the overall attended mean by
standardized group predictions.

Priority status is observed for every applicant: 113,185 non-prioritario and
181,013 prioritario, with no conflicting status records. Per-outcome attended-
VA samples contain 292,671 math, 292,681 verbal and 292,682 field/institution
students. Raw prioritario-minus-non-prioritario attended gaps are -0.02152 SD,
-0.02819 SD, +0.13176 pp and -1.39139 pp respectively. After controlling only
for matching E, those gaps are -0.00790 SD, -0.00960 SD, +0.02250 pp and
-0.13100 pp. All-applicant E gaps are -0.01769 SD, -0.02351 SD, +0.14157 pp
and -1.32607 pp. Both raw/adjusted attended means and both E comparisons are
included in the report and combined CSV.
