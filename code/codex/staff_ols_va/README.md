# Fixed staff and school characteristics versus school VA

User-approved replacement of Lasso for the new summary table: unweighted joint
OLS of each of 12 saved All-sample EB school VAs on the same 14 focal predictors.
No penalty, selection, CV, stars or additional fee/dependency/region adjustment.
Previous Lasso files remain unchanged as historical exploratory results.

Run from the repository root with R 4.5.1 and bundled Python:

1. `Rscript --vanilla code/codex/staff_ols_va/01_fit.R`
2. `python code/codex/staff_ols_va/02_verify_report.py`

Add `--overwrite` to step 1 only for an intentional refresh of these outputs.
The input matrix and original VA source are read-only and hashed.

All 14 variables enter together with an intercept, focal-variable missingness
indicators, and staff-role absence/incomplete-roster indicators. Constant or
linearly dependent nuisance columns are dropped, preserving their span and all
14 focal coefficients. All schools with a finite outcome remain in its sample.
Predictor gaps use outcome-sample observed medians; missingness indicators prevent
treating these placeholders as observed measurements. No outcome is imputed.

X uses the observed nonmissing sample SD within each outcome sample; Y uses its
sample SD. Coefficients are outcome SD per observed predictor SD, including for
binary variables. HC1 covariance is the regression sandwich using squared
OLS residuals times N/(N-P). HC3 is undefined for singleton nuisance cells with
unit leverage, so HC1 is used consistently. Unknown absence indicators use zero
placeholders with explicit missingness controls, not assumed observed absence.
R2 is in-sample, not predictive held-out R2.
R fits OLS; Python independently checks coefficients, HC1, samples, scaling,
fitted values and R2. The chosen variables were informed by earlier full-sample
Lasso: standard errors are conditional descriptive uncertainty, not corrected
for selection, multiple outcomes, VA estimation error or cross-school dependence.

Outputs: reusable estimates/normalizations/audits in `data/clean/staff_ols_va/`;
one readable table in `output/reports/staff_ols_va.html` and Markdown equivalent.
Resource/student/staff definitions remain those in the existing predictor build.
In particular, VA-sample staffing ratios are not class size/caseload; public
funding is not total spending. Credential shares are undefined when a role is
absent, and linkage coverage can affect measured credential prevalence.
