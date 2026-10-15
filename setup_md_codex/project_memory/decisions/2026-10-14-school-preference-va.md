# Submitted school preferences and four-dimensional EB value added

Current user instruction: estimate math, exam-taking, high-premium-field and
high-premium-institution VA jointly with a TP-school indicator and
log(1+distance_km). Retain the two
distance versions: published student-to-campus distance and home-comuna-center
to the same campus. This supersedes the initially approved seven-VA model.
The initial seven-VA snapshot is retained only in the local ignored
data/clean/school_preference_va/archive/seven_va_initial directory and is not
the reported specification. No other preference extension or manuscript edit
is included.

## Population and submitted rankings

Use the Table 2 roster of 294198 timely grade-9 applicants in 2018-2020,
including those with and without assignment risk. The user explicitly chose
the paper's upstream age/linked-HS restrictions. Do not narrow to lottery risk
or outcome-specific IV samples. Reconstruct regular-round C1 lists, remove
auto-appended continuity entries, retain genuine priority choices, and exclude
complementary rounds. Old sae_apps drops genuine priority choices; the
unique-process cache lacks continuity flags. Neither can replace raw C1.

Preserve 1013909 genuine program rows. Collapse each RBD to its first submitted
program, retaining that program's campus, and renumber distinct-school ranks.
The full data have 1007253 school pairs. Keep flags for 4875 repeated-school
lists and 278 interleaved A/B/A program lists. The user chose school-level
aggregation rather than program alternatives; it does not recover every
program preference.

Preserve all original lists with coverage flags. A missing required regressor
at any listed school excludes the whole list; never silently remove only that
school. Single-school lists have likelihood one and provide no ordering signal.
Recalculate eligibility for the four selected VAs, rather than inheriting a
seven-VA complete-case restriction. This happens to retain the same sample:
276193 rankings, 985285 pairs and 2501 schools. Exclusive losses are 16720
single-school lists and 1285 informative lists missing at least one VA.
Before singleton restriction, 292887/294198 lists (99.55%) have all four VAs;
26 missing-VA lists are also singleton lists. Geography removes none.

## Existing indices and normalization

Use the canonical current paper file under Box data/clean/
empirical_bayes_school_va/cohorts_2017_2020, analysis_sample=All and
controlled_value_added_eb_centered_student. Outcomes, in report order, are
z_year_math_max, admission_exam_taker, high_paying_field_m1 and high_inst_m1.
Do not use legacy Stata exports, reconstruct VA, or invent year-specific VAs.
Math uses observed-score pupils; institution/field VAs use exam takers;
exam-taking VA uses the broader complete-control population. Preserve and
disclose those source definitions.

Normalize each VA using unweighted mean and sample SD (N-1) across unique
destination RBDs in the common final estimation sample, pooled across years.
Do not application-weight moments. Save parameters, retain raw values, and
apply exactly the same standardized indices in both models.

## Geography

All published B1 coordinate-quality types enter by explicit user choice,
including 59976 municipality-imputed pupils (20.4%). Coordinates describe
approximate declared locations and are privacy displaced. 2018 noise is
uniform +/-0.0013 degrees independently per coordinate; 2019-2020 uses a
uniform 50-300m radial displacement with random direction.

Use contemporaneous A1 campus coordinates joined by process/RBD/program;
campuses at a single RBD can differ by over20km. All coordinates match, so
neither directory fallback nor external geocoding is required. Robustness
replaces only student coordinates using grade-8 COD_COM_ALU from
universe_controls.csv and numeric centers from comuna_info.RDS. All applicants
match the345 unique codes. Join numeric codes, not lookup names: code1401
is mislabeled TOCOPILLA though its code/center/province identify Pozo Almonte;
actual Tocopilla is separately code2301. Retain official residence names and
document the source-label error without modifying original inputs. No old
84xx Nuble residence codes occur in this sample.

Use Haversine straight-line km with mean Earth radius6371.0088km, followed
by log1p. Do not standardize the distance term, trim genuine long distances,
or substitute travel distance. Schools retain actual campus points in both fits.

## Exact likelihood and inference

The user subsequently added a binary indicator for TP schools and pointed
to Directorio. It is a raw 0/1 regressor in both models, not standardized.
Construct it directly from ENS codes410,510,610,710,810 in the 2024 Directorio,
and verify equality to the paper's existing TP flag on their shared support.
Mixed HC/TP schools count1, pure artistic
schools0. This is school-level availability of a TP secondary track, not the
chosen student's track or a contemporaneous application-year classification.
All2501 current estimation schools are classified, with927TP (690 mixed).
No TP-based sample loss or VA renormalization occurs. All2524 schools in the
full submitted-ranking data receive a direct-directory classification,
including15 schools not covered by the older predictor cache. The final
estimation data are byte-identical whether using the verified cache or the
direct directory source, so the fitted parameters apply to the final data.

The estimand is ordering conditional on the submitted school set. Use
coxph(Surv(rank_school,1)~four_VAs+tp_school+distance+strata(ranking_id)), cluster=mrun,
ties=breslow. The event time is each school's unique rank; at rank r the risk
set is exactly the schools remaining in that submitted ranking. There are no
within-ranking ties, making this the exact specified Plackett-Luce likelihood.
The four normalized VAs, TP binary and distance term enter (six coefficients).
No intercept, school fixed effects, peer controls or unlisted alternatives
enter. Student-clustered sandwich SEs aggregate full ranking scores and
condition on supplied EB estimates without propagating VA estimation error.

Coefficients describe conditional ranking associations, not whether parents
observe these VA measures, their beliefs, the causal demand effect of VA
information, consideration-set inclusion, outside options or enrollment.

## Results and verification

Main/comuna point estimates with TP: math .042903/.041678;
exam .066261/.068796; high-premium field .058101/.056050;
high-premium institution -.017834/-.022832; TP .316737/.313501;
logged distance -.183724/-.063978. All four VA signs and the TP sign are
preserved across distance measures. TP has main conditional odds ratio1.3726;
the binary is not standardized. Adding TP changes the exam-VA coefficient
from -.018020 in the saved no-TP model to+.066261. All four main VA
coefficients are distinguishable from zero at5% with student-clustered
inference; these are conditional associations, not causal effects of school
type or providing VA information. The no-TP results are archived locally
under archive/four_va_without_tp and documented in commit58b9d71.

Both models converge in three iterations, have six identified regressors,
positive observed information and score per ranking below2e-14. Information
condition numbers are14.41/14.46; school-VA-only VIFs range1.03-1.17. Validation
compares independent full-sample PL likelihoods and expanded conditional
logit point estimates/student-clustered covariance on deterministic complete
lists, including long and interleaved-program examples. Detailed checks and
errors are saved with the results. Validation includes the raw TP binary in
both the direct and expanded likelihoods. The deterministic validation sample
contains1393 rankings (9207 alternatives,66199 expanded rows).
All56 independent model checks pass, with coefficient/covariance differences
below4e-16/5e-17 and full-sample likelihood differences below5e-7.

Code, derived data, tables and report use task name school_preference_va.
Student-level data and fitted models remain in ignored clean data; commit
only reproducible code, documentation and aggregate outputs.

## Requested priority-by-continuation models (2026-10-15)

The user subsequently requested four separate preference models: SAE
prioritario/non-prioritario crossed with voluntary application/school ending.
This authorizes this specific subgroup extension to the initial pooled scope.
Keep the four selected VAs, TP binary, log1p distance and exact submitted-set
likelihood unchanged. Carry both existing distance specifications into each
group, using identical group samples across geographic definitions.

Reuse sample/continuation_group in all_applicant_expected_va.rds and the
existing regular-round B1 prioritario_sae_2018_2020.csv cache, unique on
mrun/sae_proceso. Prioritario denotes low-income status, not a C1 sibling or
incumbent priority. The school-ending group has no origin SAE offering above
grade 8; voluntary origins offer grade 9. These are origin-continuation and
application categories, not proof of motive or a realized school move. Include
both lottery-risk and no-risk applicants. Exclude unresolved/other origins
rather than classifying them arbitrarily.

The four groups contain 289432 original applicants and 271597 pooled-eligible
rankings, respectively 87407/155028/15363/13799 in school-ending non-prioritario,
school-ending prioritario, voluntary non-prioritario and voluntary prioritario.
Unresolved/other continuation accounts for 4766 originals and 4596 otherwise
eligible rankings. Group and year accounting separates this exclusion from
singleton and incomplete-list losses. No additional alternative-level or
outcome-dependent restriction is imposed.

Keep the pooled normalization across the same 2501 unique destination schools
fixed in every subgroup; do not standardize on group support. Student-clustered
SEs and the fixed 2024 Directorio TP definition are unchanged. Separate-logit
coefficient comparisons can reflect differences in utility-error scale as
well as preferences. No cross-group equality tests or extra controls are added.

Scripts05/07/06 fit, independently verify and report these models under
school_preference_va; all subgroup outputs use by_priority_continuation and
leave the pooled outputs unchanged. Validation checks original membership,
whole-list preservation, identical input regressors, the full PL likelihood
and small-sample expanded conditional-logit coefficient/covariance equivalence
for each group and each distance definition.

All eight subgroup fits converge in three iterations without fitting
warnings, retain all six coefficients, and have information condition
numbers13.80-16.25. All177 independent checks pass. Expanded conditional-logit
coefficient/covariance maximum discrepancies are7.22e-16/5.83e-16; full-sample
independent PL likelihood maximum discrepancy is6.58e-8.

The main institution-VA coefficient is negative for both ending groups
(-.024988 non-prioritario, -.044411 prioritario), and positive for both
voluntary groups (.046082/.042474). Math, exam, field and TP coefficients are
positive in all four main models. All VA and TP signs persist with comuna
distances. Main distance slopes are negative throughout; comuna distance is
near zero and not distinguished from zero in the voluntary groups
(.015511, SE.025276; .019703, SE.028660). These are sensitivity results for
the existing location definitions, not evidence of a preference for travel.

## TP removal requested after subgroup results (2026-10-15)

The user asked to remove TP again. This supersedes the TP addition above:
all active pooled and four-group models now include only the four normalized
VAs and log1p distance, five coefficients in each model. Retain both distance
versions, exact submitted-list likelihood, full-list eligibility and
student-clustered covariance. TP is neither a regressor nor an eligibility
requirement. Retain its school classification in existing data as metadata.

Preserve the existing sample and pooled VA parameters unchanged. All original
destination schools already have a TP classification, so removing its old
completeness condition changes no list. Reuse the existing inputs instead
of rereading raw SAE files or reconstructing VAs. The builder is updated so
future reconstruction also omits TP from eligibility. Archive all prior TP
fits, their estimation inputs, normalization, covariance/diagnostics and
aggregate reports under data/clean/school_preference_va/archive/
four_va_with_tp_2026_10_15, with copy hashes and a manifest. Original sources
and individual submitted lists are unchanged.

The two pooled and eight subgroup fits are re-estimated; current reports
and tables replace their TP versions at the existing paths. Validation now
explicitly checks that TP is absent from fitted formulas and all coefficient
exports, alongside unchanged sample/normalization, exact PL likelihood and
expanded conditional-logit coefficients and clustered covariance.

All ten no-TP fits converge in three iterations. Both pooled and subgroup
estimation inputs and normalization are byte-identical to the archived TP
run; cached whole-list eligibility equals eligibility computed ignoring TP.
The pooled verifier passes59 checks and subgroup verifier187, including
explicit absence of TP in every model formula and coefficient set. Expanded
coefficient/covariance maximum errors are below1.1e-15; independent full PL
likelihood maximum errors are2.80e-7 pooled and4.82e-8 grouped.

Current pooled main estimates (student-clustered SEs): math.015557(.001824),
exam-.018020(.001923), field.057109(.001393), institution-.008186(.002951),
log distance-.181531(.003119). The main four-group math slopes are
.025291/-.006499/.045723/.023898, exam slopes.027173/-.044080/.016557/-.001987,
field slopes.061484/.054992/.056016/.054956, institution slopes
-.019587/-.028795/.045248/.049206 (ending non-prioritario, ending prioritario,
voluntary non-prioritario, voluntary prioritario). Exam-taking slopes are
not distinguished from zero at5% for either voluntary group. Institution VA
remains positive for both voluntary groups and negative for both ending
groups. Field slopes are positive throughout. These are conditional
associations; no cross-group equality test or causal interpretation is added.
