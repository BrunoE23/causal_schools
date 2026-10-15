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

## Separate mixed HC/TP and TP-only indicators (2026-10-15)

The user next requested separate mixed-HC/TP and TP-only indicators, first
describing the top10/20/50/100 most demanded schools and then refitting the
preference models. This supersedes the immediately preceding no-TP model.
Keep all four VAs, both distance versions, pooled normalization, original
rankings and student clustering. Enter mixed_hc_tp and tp_only as raw0/1
regressors:7coefficients per model. Do not enter the redundant any-TP binary.

Use the same2024 Directorio source. HC is regular youth ENS310; TP is any
of410/510/610/710/810. Mixed meansHC andTP, TP-only meansTP withoutHC; these
labels do not exclude lower grades. The reference is non-TP rather than pure
HC: among2501 estimation destinations,1557HC-only,1artistic-only and16other/
no-regular-secondary remain; mixed690 andTP-only237. All RBDs match. Preserve
the non-HC reference schools and do not add2024 activity restrictions or
change rankings to force a pure-HC reference. Classification is fixed2024,
not an application-year or individual-track measure.

The top-school analysis reuses the earlier published top100 ranking rather
than recomputing rankings with the new genuine-list cache. Counts(HC-only,
mixed,TP-only) are4/2/4(top10),10/6/4(top20),18/15/17(top50),41/31/28(top100).
School shares divide by eachN. Enrollment shares weight by2024 MAT_TOTAL
within those same schools, explicitly including all grades. They are not
track-specific or HS-only enrollment shares. All100 RBDs match uniquely.

school_types.R supplies the common map;08 generates the top-school summary
first,09 attaches the new columns to existing full and minimal ranking data
without altering pre-existing values.01 is updated for future reconstruction.
The no-TP models and inputs are saved in the ignored dated archive
four_va_no_tp_before_split_2026_10_15, with copy hashes. Pooled and subgroup
sample sizes and VA normalization remain fixed; all school-type inputs are
observed, so no new whole-list exclusion is necessary.

All ten split-type fits converge in three iterations.67 pooled and195
subgroup independent validation checks pass. Expanded conditional-logit
coefficient/covariance errors are below6e-16; maximum full PL likelihood
discrepancy is6.95e-8. A direct comparison to archived no-TP inputs confirms
all pre-existing columns and their values, group/pooled counts and
normalization parameters are unchanged. New indicators add no sample loss.

Pooled main estimates(SEs) are math.046228(.001866), exam.079093(.002327),
field.056336(.001399), institution-.022007(.002980), mixedHC/TP.289160(.004798),
TP-only.432256(.006465), log distance-.189412(.003147). Pooled comuna type
estimates are.291566(.004774) mixed and.405614(.006417) TP-only. Group main
mixed/TP-only estimates are.248659/.394008(ending non-prioritario),
.329599/.457516(ending prioritario),.225217/.355919(voluntary non-prioritario),
.254843/.500053(voluntary prioritario). Both type associations are positive,
with larger TP-only point estimates throughout. No formal cross-group
equality test is added; separate-logit scales and conditional-list scope
remain relevant to comparisons.

## Enrollment-weighted mean school VA by type (2026-10-15)

The user requested average VAs by school type and explicitly selected2024
school enrollment as weights, rather than SAE-sample students attending each
school. Use MAT_TOTAL from the same2024 Directorio. Primary population is
national functioning(ESTADO_ESTAB1), enrolled(MATRICULA1) establishments
offering regular secondary HC310, TP410/510/610/710/810 orartistic910,
including private-paid schools. MAT_TOTAL counts all grades, not just HS or
one track. This is distinct from the preference-model destination support.

Use current All-sample pooled2017-2020 EB VAs for math, exam, field and
institution. Require all four at each included school, with the same school
and enrollment denominators for every outcome. Do not impute missing VA.
Report raw means(math in student-scoreSD, others percentagepoints) and means
in the unchanged pooled-school-SD preference normalization; neither within-
type nor enrollment-based restandardization occurs. A separate appendix
restricts the same active schools to preference-model destination support.

Complete-school counts/enrollments areHC2050/1575818,mixed694/515262,
TP-only237/141705,artistic2/1525. Enrollment coverage relative to the full
national type groups is98.8817%,100%,99.9464%,100%. In model z-units,
HC means(math,exam,field,institution) are1.010143,.550119,.051795,.554430;
mixed-.385978,-.469236,-.030213,-.010917; TP-only-.627792,-1.075530,
.457084,.084110. Descriptive means agree with an independent calculation.
Original outcome-specific VA estimation populations remain unchanged;
institution and field estimates are among exam takers. These means are not
outcome prevalence rates or new causal comparisons of school types.

Code10_average_va_by_school_type.R writes clean results, coverage and source
manifest plus a readable report, without changing demand estimates or raw
inputs. The full national complete population has2983schools and2234310
all-grade pupils.

## Distribution plots by the four school types (2026-10-15)

The next request asks for four overlapping VA distributions per dimension.
Interpret the four groups as the preceding HC-only,mixedHC/TP,TP-only and
artistic-only table. Keep the user-selected2024 whole-school enrollment
weights and national2983-school/2234310-pupil four-VA-complete sample,
including private schools. Reuse the fixed pooled-school normalization.

Use exact weighted empirical CDFs rather than imposing a smooth density on
the two artistic schools. Aggregate tied VA values, accumulate positive
MAT_TOTALmass within type, and retain the full observed support. Zero/one
boundary segments extend plots without changing school-level jumps. Show
artistic jumps with markers; their mass shares are941/1525 and584/1525.
No bandwidth, tail trimming or interpolated quantile is introduced. Weighted
quantiles are the first VA where cumulative mass reaches the target fraction.

11_plot_va_distributions.R saves a combined2x2PNG/vectorPDF, individual PNGs,
exact CDF data, weighted quantiles and a report. Checks reproduce the previous
means and sample and establish16monotone,unit-mass curves. Visual inspection
checks labels, legend, all curves and tails. TP-only field VA has a strong
upper tail: weightedp90=3.5314 versusHC1.0855; weighted medians are-.0475
versusHC.0272, despite the higher TP-only mean. This describes distributions
of the saved school indices, not a new causal school-type comparison.

The independent source-data audit matches all11932school-dimension rows,
all11932CDFjump points and80quantiles to floating-point precision(maxCDF
error6.66e-16,maxquantile error4.88e-15). Combined and single-panel figures
are visually inspected; four curves, exact artistic jumps and full tails
remain visible. No preference regression or source VA is changed.

## User correction: densities and no artistic schools (2026-10-15)

The user clarified "not cdf" and then "drop artistic". Supersede the active
figure with three overlapping enrollment-weighted Gaussian kernel densities
per dimension:HC-only,mixedHC/TP,TP-only. Keep the prior input sample,
weights and normalization for those types,2981schools/2232785all-grade
pupils. This exclusion applies only to these plots; preference samples and
mean tables remain unchanged. Retain prior CDF files as historical outputs.

Normalize enrollment weights within type. Choose one common bandwidth per
dimension from all three types pooled: .9*min(weighted populationSD,
inverse-weighted-CDFIQR/1.34)*n_eff^(-1/5), n_eff=(sumw)^2/sum(w^2).
This is a graphical smoothing rule, not an inferential sample adjustment.
Bandwidths(math,exam,field,institution) are.228130787,.145071028,
.166287866,.206372531. Evaluate direct Gaussian mixtures on4096gridpoints
over the observed full support plus5bandwidths in each tail; do not trim
schools or use FFT/binning approximations. Verify nonnegative,unit-area
densities and shared bandwidths within panel, then inspect PNG/PDF exports.

12_plot_va_densities.R writes the new combined PDF/PNG and individual PNGs,
plus clean curve/bandwidth/diagnostic files. The active distribution report
at va_distributions_by_school_type.html now shows and links the density plot.

Independent audit matches all4bandwidths to5e-16 and selected direct-mixture
grid values to4.83e-15 across12curves/49152gridpoints. Numerical areas are
within2.36e-9of1 and agree with analytic finite-range Gaussian mass to1.36e-12.
All six automated plotting checks pass. The combined density image is
visually checked and the new densityPDF is shown through the app file panel.
