# Joint staff-characteristic Lasso and school VA

The design and results below describe the initial pre-fee specification.
The approved fee-band extension and current results are recorded in
`2026-09-20-staff-lasso-fee-bands.md`; the same report filenames now contain
that extended specification.

## Agreed design

Bruno approved one school-level Lasso for each of the 12 saved All-sample EB
VA outcomes, jointly entering HS-teacher, primary-or-secondary orientador and
leadership characteristics. This is descriptive prediction of saved estimates,
not causal effects of hiring staff, a new VA estimator, or individual quality.

Use the existing 3,682-school universe and 2018-2024 staff-period measures.
Drop only schools without the particular observed VA outcome. Schools receive
equal weight. No outcome is imputed; source VA and raw inputs remain unchanged.

Candidate features include the eight as-of-year linked credential shares for
each role; existing career, qualification, subject-match, staffing and baseline
tenure components; role availability and dated qualification matching coverage;
school log VA-sample size and its square; administrative dependency, region,
rural status and basic/TP-artistic offerings. Categorical reference codes are
dependency 1 and region 13, with unknown-code indicators. The 2018 reported
tenure measures are allowed; the broken post-2018 reported-tenure averages are
not. Composite indices and alternative-window indices are excluded because
their constituent measures already enter; secondary-only leadership is the
complement of primary leadership and is not entered twice. Other correlated
components are retained; selection of one over another need not be substantive.

Staff ratios explicitly use pooled VA-sample student counts divided by average
annual headcounts, not annual HS enrollment, FTEs, caseload or class size.
No true class-size measure is constructed without compatible section counts.
Funding and 2024-only age measures are not introduced in this specification.

Credential aggregation counts unique persons within role-school-year, uses
known annual roster denominators, and averages active-role-year shares equally.
Require seven known role counts; absent roles have undefined attributes, not
zero qualifications. Dated as-of credentials do not use later qualifications.
Add explicit absent-role and incomplete-roster indicators. Other nonfinite
predictors receive a training-fold median placeholder and a missingness
indicator. An all-missing training feature receives placeholder zero and is
constant-dropped; this does not assert zero quality. No preprocessing learns
from validation or outer-test schools, including imputation and scaling.

## Estimation and validation

Use Gaussian Lasso (alpha=1), penalizing all predictors, with an unpenalized
intercept. Standardize X and Y using each training sample's population SD;
present full-sample refit coefficients in outcome SD per predictor SD, including
standardized dummy/missingness indicators. Training-constant features are
excluded and documented; their coefficients are zero. No post-selection OLS
or conventional significance stars are attached to penalized coefficients.

Use a fixed, outcome-independent grid of 70 penalties from 1 to 0.0005 in
standardized units for the joint model. A boundary audit of the first run found
some school-only minima at the weakest penalty, whereas all joint minima were
interior. Before final reporting, extend every school-only grid with 20 weaker
penalties down to 0.000001 and the zero-penalty endpoint (91 points total), and
rerun its inner and outer tuning. This avoids disadvantaging the baseline via
a truncated range; it is a numerical tuning-support repair, not feature or
outcome selection. The upper end is sufficient for an intercept-only model
by Cauchy-Schwarz. Five-fold inner CV uses mean squared prediction error in the
original outcome units, with fold-specific preprocessing and outcome scaling.
The main penalty is the largest lambda within one fold-SE of minimum mean
fold MSE; minimum-error lambda is a sensitivity. The fold SE is SD of the five
fold MSEs divided by sqrt(5), used as a tuning heuristic, not inferential SE.

Five disjoint outer school folds evaluate the entire tuning procedure. Compare
joint models against separately tuned school-context-only Lasso on identical
schools/folds. Outer training labels never enter held-out predictions. Report
pooled out-of-fold R2 = 1 - SSE/SST using the evaluation outcome mean, RMSE,
and a train-fold-mean null benchmark. Negative out-of-sample R2 is retained.
Full-sample coefficients come from a separate five-fold tuning/refit, not an
average of outer coefficients. Also report how often each predictor is selected
in the five outer fits; this is descriptive split sensitivity, not a probability
of a true effect. Schools sharing staff are not grouped into common folds;
performance is for held-out schools, not necessarily wholly new labor markets.

Seed 20260920 determines stored common school folds. Evaluation treats source
VA estimates as fixed. Their estimation error/shrinkage is not propagated;
source EB VA and institution-premium definitions were not reestimated within
folds. Performance therefore describes these saved estimates, not independently
measured future school causal effects.

Official estimator reference: https://glmnet.stanford.edu/articles/glmnet.html
Code and outputs: code/codex/staff_lasso_va/ and data/clean/staff_lasso_va/.

## Completed checks and findings

Final inputs contain 163 raw candidates and an additional missingness indicator
for each. Full joint fits have 294 nonconstant design columns; school-only fits
have 25. Source and derived-join checks pass for all three staff groups, including
independent reconstruction of the 24 new credential aggregates. All 169,348
held-out prediction rows, 288 selected fitted-model KKT checks, 48 performance
rows, tuning-rule selections and five-outer-fit selection frequencies were
independently checked in Python. R synthetic tests also check training-only
median/scaling behavior and coefficient/prediction reconstruction. Source hashes
are unchanged. Expanding the baseline penalty range leaves the main one-SE
results unchanged; the minimum-error baseline was also fully rerun.

Out-of-fold joint versus school-only R2: math 0.446 vs 0.365; language 0.458 vs
0.412; exam taking 0.607 vs 0.450; higher-ed enrollment 0.332 vs 0.269; HP field
0.094 vs 0.095; HP institution 0.578 vs 0.476; full projected income 0.441 vs
0.398. Other outcome gains are small. The added block includes staffing and
data-coverage information as well as credentials and experience; its predictive
gain cannot be attributed wholly to substantive staff quality.

In the math full-sample joint fit, selected substantive features include mean
primary leadership headcount (+0.112), teacher magister share (+0.060), teacher
UG at high-premium institutions (+0.060), and teacher headcount (+0.054).
All are SD-per-SD penalized predictive coefficients, not isolated bivariate
correlations or causal slopes. Private-paid dependency remains a large school
context predictor (+0.381). Collinear alternative credentials and staffing
transformations must be interpreted jointly rather than ranked as independent
effects. Tables include all selected context and data-coverage indicators.
