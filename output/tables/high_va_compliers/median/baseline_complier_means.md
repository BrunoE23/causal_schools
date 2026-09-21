# Baseline characteristics of high-VA compliers

High VA: strictly above the school-level P50.

Assignment years: 2018, 2019, 2020

Main estimates use (Z-q) moments with the treated-state equation. Parentheses contain heteroskedastic-robust standard errors.
These are assignment-variance-weighted complier means, conditional on valid classification and observed baseline covariates.

| characteristic | highinst | highpay | language | math | program_income_full |
| --- | --- | --- | --- | --- | --- |
| father_educ_years_imputed | 11.237 (0.093) | 10.959 (0.050) | 11.146 (0.052) | 11.188 (0.051) | 10.936 (0.054) |
| female | 0.454 (0.016) | 0.479 (0.008) | 0.511 (0.008) | 0.512 (0.008) | 0.458 (0.009) |
| income_decile_imputed | 5.044 (0.086) | 4.746 (0.045) | 4.849 (0.047) | 4.942 (0.046) | 4.708 (0.049) |
| mother_educ_years_imputed | 11.369 (0.087) | 11.192 (0.048) | 11.367 (0.048) | 11.399 (0.047) | 11.212 (0.050) |
| z_sim_leng_4to | -0.186 (0.033) | -0.107 (0.017) | -0.009 (0.017) | -0.015 (0.017) | -0.098 (0.018) |
| z_sim_mat_4to | -0.224 (0.033) | -0.145 (0.017) | -0.085 (0.017) | -0.082 (0.016) | -0.153 (0.018) |

Income and parent education use existing baseline imputations. Female is a share; math/language are grade-4 standardized scores; parent education is in years.
See data/clean/high_va_compliers/complier_means.csv for untreated-state estimates, spline sensitivity, applicant means, confidence intervals, sample sizes, first-stage and balance diagnostics.
Standard errors condition on school VA classifications and simulated assignment probabilities; they do not propagate VA estimation or simulation uncertainty.
No-first-round-offer (RBD 0) and simulated unmatched states have Z=0. Missing VA is never classified as low.
Positive first stages do not establish individual monotonicity. Missing attendance and covariate exclusions may affect interpretation; inspect the saved exclusion counts.
