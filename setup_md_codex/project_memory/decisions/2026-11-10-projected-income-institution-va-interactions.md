# Projected income with parent education x premium-institution VA

User requested the previous premium-institution VA interactions for log projected
income and income in levels. Fit both parents+linear-PAES and full-baseline OLS
specifications, with both parent-by-institution-VA interactions and all main
effects. Retain the exact baseline controls from the preceding analysis.

Use the 559,819-person VA-covered complete-baseline cache and attach existing
program_income_full_clp_m1 and log_program_income_full_clp_m1 from the small
broad-panel cache, verifying matched IDs, cohort, attended school, enrollment,
and PAES scores. Restrict both outcomes to HE enrollees with finite positive
projected income and a finite matching natural log. This matches the earlier
projected-income analysis scope; audit exclusions. No winsorization, trimming,
new imputation, high-math restriction, or changes to source score scaling.

Use existing All-sample 2017-2020 observational EB premium-institution VA for
both outcomes. Center this VA and both parents' years at the included enrollees'
means. A VA unit remains 0.10 probability units (10 pp). Fit the income-level
outcome in 100,000 CLP units for numerical stability, but restore monthly CLP
in reported estimates/SEs. Log coefficients are natural-log points. The raw
level and log income values and scaling constants are saved for reproducibility.

Use school-clustered CR1 with exact full baseline-FE degrees of freedom and
t(G-1); test the pair of interactions using F(2,G-1). Verify normal equations
and independently reconstruct the cluster sandwich. Parent gradients at VA
quartiles use full linear-combination covariance. Inference treats estimated
VA as fixed; VA uses overlapping cohorts and related outcomes, not cross-fitting.
Results are conditional associations among HE enrollees in projected monthly
income, not causal effects or realized earnings returns.

Script: code/codex/premium_enrollment_scores_income/23_projected_income_institution_va_interactions.R.
Output task: projected_income_parent_institution_va_interactions in clean data/reports.

Completed: all four models use 463,033 enrollees and 3,262 school clusters;
96,786 non-enrollees are excluded, and no enrolled student lacks valid income.
Mean projected income is 1,568,284 CLP/month. Father/mother centering means are
12.51505/12.57213 years; institution VA mean is 0.00394219 probability units.

Full-baseline log interactions are -0.000252835 (father, SE 0.000172979,
p=0.14393) and -0.000951937 (mother, SE 0.000191081, p=6.63e-7), with joint
p=8.20e-10. In levels, father is +332.04 CLP (SE 325.66, p=0.30801) and mother
-846.20 CLP (SE 367.68, p=0.02143), with joint p=0.07090. Interactions are per
parent year per +10 pp institution VA. Full R2: 0.35264 log, 0.34228 levels;
parents+PAES R2: 0.30500 log, 0.30290 levels. All four covariance/normal-equation
audits and source hash, log identity, common sample and matching checks pass.
