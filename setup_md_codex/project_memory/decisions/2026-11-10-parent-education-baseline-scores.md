# Add grade-4 math and verbal to the enrollment regression

The user requested baseline math and then baseline verbal in the current
HE-enrollment OLS. Add both existing standardized grade-4 SIMCE scores linearly
to continuous father/mother education and standardized entrance math/verbal.
There are seven parameters including the intercept. No income, region, year
effects, nonlinear terms, or high-math restriction are added.

Use `z_sim_mat_4to` and `z_sim_leng_4to` from the universe. The original cleaning
code scales these separately within SIMCE year; preserve that normalization,
and the prior admission-year entrance-score scales. Do not impute missing
baseline scores or restandardize in the estimation sample. Read four selected
columns from the preflighted 1.23 GB universe, then match the previous 569,681
students by ID and verify cohorts. Both baseline scores are available for
564,542 students, excluding 5,139; there are 3,372 attended-school clusters.
Refit the prior model on precisely this sample for comparison.

Final pp coefficients (school-clustered SE): father years 0.209684 (0.023598),
mother years 0.090223 (0.025565), entrance math 2.405336 (0.080730), entrance
verbal 3.136841 (0.088266), baseline math 0.849157 (0.088949), baseline verbal
0.258643 (0.081727). Scores are per original SD; parent education is per year.
R2 rises from 0.02615948 without baseline scores to 0.02652446 with both.
Enrollment is 82.7102%. Retain the original outcome data and 5,095 admission-2026
records. Interpret as conditional associations, not causal effects.

Use actual school-clustered CR1 covariance with all parameters counted and
t(G-1) inference. Normal-equation and independent sandwich checks pass for
both fits. Input-cache checksums and large-source size/mtime are unchanged.
Outputs are under `parent_education_baseline_scores_he_enrollment` in clean
data, reports and tables. Script: `19_add_baseline_scores.R` in the existing
`code/codex/premium_enrollment_scores_income/` task folder.
