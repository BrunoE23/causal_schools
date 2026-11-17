# Premium field and institution: parent/PAES versus full VA baseline controls

The user requested a comparison of parents' education plus PAES scores against
the full preceding baseline specification, for high-premium field and institution
outcomes. Fit four OLS linear probability regressions on the same 559,914 students
and 3,371 school clusters used in the preceding full-baseline enrollment model.
This keeps model comparisons separate from sample-selection changes.

Simple specification: intercept, father and mother schooling years, and linear
standardized entrance math and verbal. No grade-4 scores or other controls enter
this comparator. Full specification: linear PAES scores plus the exact preceding
observational-VA baseline control set, with separate cubic grade-4 scores, linear
income decile, parent/early-childhood variables, middle-school GPA and attendance,
and cohort/sex/age/comuna/middle-years-observed and middle-school indicators.
Parent education enters only once; destination high-school effects are excluded.

Attach the existing high_paying_field_m1 and high_inst_m1 from the broad-panel
cache, checking IDs, cohorts, attended schools, enrollment and both PAES scores.
Retain binary definitions and non-enrollee zeros; no HE-enrollment, high-math or
SAE-applicant restriction. Preserve current imputations, normalizations, and
recorded admission-year outcomes. No new raw-data reads or estimation of VA.

Use attended-school CR1/t inference, counting full baseline FE degrees of freedom
exactly. Independently verify the sandwich using actual residuals and either
original or within-transformed regressors. Report coefficients in percentage
points per parent schooling year or original score SD. Report overall R2,
including baseline fixed effects, and verify nested-model R2 improvement. These
are conditional associations. Task: premium_parent_paes_va_baseline_comparison;
script: 21_premium_outcomes_va_baseline_comparison.R.

Completed results: all four models retain 559,914 students/3,371 school clusters.
Field mean=38.9862%; R2 rises from 0.078329 to 0.144849. Institution mean=18.6825%;
R2 rises from 0.208457 to 0.345702. Field father/mother coefficients (pp/year)
change from -0.040013/-0.142977 to 0.144252/0.045670; institution coefficients
change from 1.249545/0.746455 to 0.165964/0.089308. Field PAES math/verbal
coefficients change from 12.757541/1.495810 to 11.087798/2.503607 pp/SD;
institution coefficients change from 11.297631/4.589501 to 7.661799/3.827801.
All four independent covariance and normal-equation checks pass, as do source
checksum, sample, binary-outcome and non-enrollee-zero checks. The HTML comparison
shows the four common regressors and links them to the complete raw coefficient
table in an expandable appendix.
