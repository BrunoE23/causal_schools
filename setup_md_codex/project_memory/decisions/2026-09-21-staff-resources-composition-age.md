# Extend school characteristics with resources, composition and staff age

Bruno explicitly requested building these three groups and combining them with
all existing measures. Extend the same Lasso dataset/report, not a new IV model.
Preserve the existing school universe, 175 predictors, outcome samples, folds,
penalty grids and both penalty rules. Preserve the preceding performance table.

## Definitions

- Student composition: reconstruct the All-sample exam-taking VA estimation
  sample exactly (757,999 students, 3,682 schools, grade-8 cohorts 2017-2020,
  grade-8 ages 12-16, complete original regression controls and middle-school
  fixed effect). Assign by most_time_RBD, as in VA; this is not literally the
  entering school for every student. Pool students equally over cohorts.
  Use grade-4 math/language means and within-school sample SDs, household-income
  mean decile and bottom/top two-decile shares, mean parent education years,
  female share and mean grade-8 age. The income/education fields use the
  existing observed-plus-imputed baseline controls, with three imputation-share
  predictors. No HS achievement or higher-education outcome enters composition.
  These are baseline descriptors of the VA sample, not the entire school roster.
  Use the actual saved Stata analytic cache: the current CSV reconstruction
  yields 757,997 (one fewer each at RBD 609 and 10315), whereas the cache matches
  all original school counts exactly. Do not approximate or drop those records.
  Unknown sex (code 0) is excluded from the female-share denominator; save its
  known-sex count in the reusable school composition file.
- Resources: reuse the verified public-funding construction in Box, 2017-2021,
  in 2021 CLP. Require all five years with 12 unique monthly records, positive
  enrollment and finite nonnegative funding for the substantive measures.
  Level equals sum of real annual funding divided by sum of annual average
  enrollment. Add level in millions, log positive level, and change in annual
  per-student funding (mean 2019-2021 minus mean 2017-2018) in millions.
  Add record-year and full-year coverage shares. Missing funding is not zero.
  This is public funding, not total expenditure, fees, or a 2018-2024 measure.
- Staff age: same established HS-teacher, primary-or-secondary orientador and
  leadership rosters. Read DOC_FEC_NAC from each staff year's own 2018-2024 raw
  file, never backfill from 2024 survivors. Validate YYYYMM, month 1-12, exclude
  190001 and ages outside 18-99. Collapse identical records across appointments;
  conflicting valid birthdates within person-year imply missing age. Age is
  calendar year minus birth year (age attained that year, not age on census day).
  Count each person once per school-role-year. Compute mean age, within-year
  sample SD and shares <35, 35-49, >=50 among valid ages. Average annual summaries
  equally across active years, matching the credential aggregation. Require
  known role counts for all seven years and at least one valid age in every
  active year. SD is averaged only over years with at least two valid ages and
  remains missing if none qualify. Absent roles stay undefined. Keep valid-age
  share and active-year age coverage as separate predictors; retain the middle
  age share in reusable data but omit it from Lasso as an exact complement.

The extension adds 37 candidates: 14 composition (including 3 coverage), 5
resources (including 2 coverage), and 6 age/coverage per staff role. Resources
and composition are school predictors in both model sets; age is in the joint
staff model only. Total candidates: 212, comprising 58 school, 49 teacher,
52 orientador and 53 leadership. Generic missingness indicators remain.

## Validation and interpretation

Check exact school-by-school student counts against saved VA estimation counts,
resource aggregation against the preexisting five-year output, and age roster
counts against all three original annual rosters. Independently reconstruct
new summaries and rerun existing prediction, tuning, KKT and source-hash checks.
Current empirical work remains in R. Outcomes/old predictors are unchanged.
These are descriptive predictions of fixed saved EB VAs, not causal input
effects. Shared baseline data and contextual imputation are not regenerated
inside CV; timing differences and data coverage must be explicit in the report.

## Build and rerun

All 37 added aggregates pass an independent pandas reconstruction; staff ages
are checked against the raw contemporaneous files, not just the derived cache.
All 175 earlier predictor values and the fold assignments are unchanged.
Complete resource histories: 2,943 schools. Annual valid birthdates exceed 99.9%
of relevant staff; no conflicting valid person-year dates were found.
The R rerun preserves all outcome sample sizes and both tuning rules. Main
joint R2 increases from .465 to .554 for math and .458 to .484 for full income.
The updated school-only baseline is .539 and .470 respectively; gains from
adding staff are therefore modest for those outcomes. No causal attribution
to the added variable blocks is made.

Final verification passes all 288 fitted-model KKT checks and reconstructs
169,348 held-out predictions (maximum absolute difference 5.45e-14).
Maximum independent KKT violation is 1.28e-5, below tolerance 2e-4. Joint
minimum-CV choices span grid indices 35-53 of 70, all interior. The LaTeX
table compiles in draft mode without errors or overfull boxes. All source
hashes are unchanged. Reports are refreshed in place; no alternate versions.
