# Bivariate log projected income and parent schooling

User requested log projected income regressed on each parent's education years.
Run two separate OLS models, each containing only an intercept and the selected
parent's existing continuous schooling-years measure. No other parent, VA,
interactions, PAES, baseline controls, or instruments enters either model.

Use the same 463,033 HE enrollees as the earlier projected-income analysis for
comparability, rather than the immediately preceding lottery IV sample. State
this choice explicitly. The sample inherits the earlier complete-PAES/full-
baseline and school-VA coverage restrictions; these variables are not controls
here. No new sample restriction or imputation. Preserve existing income
projections, natural logs and parent-year imputations. Projected monthly income
is associated with first HE enrollment, not observed earnings.

Follow the previous OLS convention: attended-school clustered CR1 and t(G-1)
inference. Independently reconstruct covariance and verify normal equations,
sample equality, log identity and source checksum. Report log points per year
and 100*(exp(beta)-1) as the model-implied geometric-mean percentage difference.
Interpret as unadjusted associations, not causal education returns.

Script: code/codex/premium_enrollment_scores_income/26_log_income_parent_only.R.
Output task: log_projected_income_parent_only in clean data/reports.

Completed: N=463,033 and 3,262 school clusters in each regression. Father
coefficient 0.02902730 log points/year (SE 0.00055666), R2=0.067678;
mother 0.03033271 (0.00061770), R2=0.061657. Both p<0.001. Corresponding
geometric-mean differences are 2.94527% and 3.07974% per year. All sample,
normal-equation, independent CR1 covariance and source checks pass.
