# Fixed 14-characteristic OLS table

Bruno requested stopping Lasso and reporting all shortlisted metrics versus VA.
Interpretation stated before work: one joint OLS regression per VA outcome, all
14 shortlisted measures as rows, with standardized coefficients and robust SEs.
No new selection, penalty, CV or additional adjusted model. Preserve historical
Lasso outputs; publish the current fixed-model table separately as staff_ols_va.

Three HS-teacher variables: VA students per teacher, UG high-premium institution
share, under-35 share. Two orientador variables: staff per 1,000 VA students and
orientation-specific qualification share. Three leadership variables: primary
headcount, any high-premium institution degree share and role-specific magister
share. Six school variables: baseline math mean/SD, mean household income decile,
log public funding per student, log VA count and TP/artistic offering.

Fit unweighted across schools with observed saved All-sample EB VA. Preserve all
outcome-specific school counts. Median-placehold missing focal values with their
indicators, plus explicit role-absence and roster-incompleteness controls. Drop
only constant/collinear nuisance terms, auditing each; never drop a focal term.
Standardize each focal by its observed sample SD in the outcome sample, and Y
by its sample SD. This differs from Lasso's imputed-population-SD normalization
and is explicit. HC1 is computed from the actual design/residuals with N/(N-P), then
independently verified. Binary coefficients also represent one predictor SD.

R2 is in-sample. No significance stars: shortlist was data-informed, outcome
VAs are estimated and shrunk, and these descriptive SEs do not correct for
selection, VA uncertainty, multiple testing or cross-school dependence. Neither
coefficients nor R2 measure causal input effects. Staffing/resource timing and
denominator limitations carry forward from the documented source construction.

An initial HC3 check detected unit-leverage singleton nuisance cells. HC3 is
undefined there, so use conventional HC1 consistently; this change was stated
to Bruno before continuing. Retain all schools, audit unit-leverage counts,
and do not suppress data-coverage indicators to manufacture finite HC3 results.
Unknown role-absence indicators get zero placeholders plus explicit missingness
indicators; these are not treated as observed absence.

All 12 models retain 14 focal terms and 24 total independent coefficients.
Exactly one unit-leverage nuisance cell occurs in each outcome. Independent
SVD least squares and HC1 reconstruction match R to 4.33e-14 (coefficients)
and 4.90e-14 (SEs), including saved fitted values, sample sizes and R2.
Every discarded nuisance column is verified to lie in the retained design span.
All source hashes remain unchanged.
