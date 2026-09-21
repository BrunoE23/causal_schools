# Baseline characteristics of high-VA compliers

High VA: strictly above the school-level P25.

Assignment years: 2018, 2019, 2020

Main estimates use (Z-q) moments with the treated-state equation. Parentheses contain heteroskedastic-robust standard errors.
These are assignment-variance-weighted complier means, conditional on valid classification and observed baseline covariates.

| characteristic | highinst | highpay | language | math | program_income_full |
| --- | --- | --- | --- | --- | --- |
| father_educ_years_imputed | 10.885 (0.135) | 10.817 (0.071) | 10.859 (0.061) | 10.917 (0.060) | 10.482 (0.089) |
| female | 0.441 (0.022) | 0.502 (0.012) | 0.489 (0.010) | 0.486 (0.009) | 0.497 (0.014) |
| income_decile_imputed | 4.372 (0.124) | 4.640 (0.064) | 4.626 (0.054) | 4.661 (0.053) | 4.192 (0.081) |
| mother_educ_years_imputed | 11.175 (0.127) | 11.093 (0.067) | 11.030 (0.058) | 11.104 (0.056) | 10.763 (0.084) |
| z_sim_leng_4to | -0.161 (0.045) | -0.118 (0.024) | -0.171 (0.021) | -0.140 (0.020) | -0.207 (0.030) |
| z_sim_mat_4to | -0.240 (0.045) | -0.180 (0.024) | -0.238 (0.020) | -0.207 (0.020) | -0.252 (0.030) |

Income and parent education use existing baseline imputations. Female is a share; math/language are grade-4 standardized scores; parent education is in years.
See data/clean/high_va_compliers/complier_means.csv for untreated-state estimates, spline sensitivity, applicant means, confidence intervals, sample sizes, first-stage and balance diagnostics.
Standard errors condition on school VA classifications and simulated assignment probabilities; they do not propagate VA estimation or simulation uncertainty.
No-first-round-offer (RBD 0) and simulated unmatched states have Z=0. Missing VA is never classified as low.
Positive first stages do not establish individual monotonicity. Missing attendance and covariate exclusions may affect interpretation; inspect the saved exclusion counts.
