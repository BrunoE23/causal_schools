# SAE participation prediction: first administrative-data exercise

Date: 2026-10-05. Bruno authorized implementation after discussing predictors.

## Agreed target

Predict whether a student applies through SAE while in grade 8 for grade-9 entry.
Use grades and attendance in grades 5-8, their school-grade-year standardized
positions and squares, and differences between standardized grades and baseline
grade-4 SIMCE achievement relative to the same later peers (math and language).
Add parent evaluations of the school from SIMCE questionnaires. Include whether
the grade-8 school ends at grade 8 from the baseline specification. Defer all
distance/comuna work until the other blocks are complete. Actual school changing
is a distinct potential extension, not the primary outcome.

## Implementation choices

Use first-observed grade-8 cohorts 2017-2020 from the broad universe. Require
observed grade-8 origin and an origin region served by grade-9 SAE offerings in
that process. Keep nonapplicants, missing test scores and questionnaire nonresponse.
Rebuild explicit applications from C1/C2, rather than treating the first-process
legacy marker as a complete student-year outcome. Automatically appended continuity
rows alone do not count as an explicit choice. Preserve regular-round participation.

School directory ENS codes describe education levels with at least one enrolled
pupil (official dictionary footnote), not legal grade offerings. Use A1 offerings:
an observed grade-8 origin listed with no secondary offering is classified as
ending at grade 8 within SAE. Because some early rollout menus omit nonentry
grades, do not require the maximum listed level to equal 8. Schools absent from
A1 remain unresolved. This operational status cannot establish every private
school's authorized highest grade. Region-by-year coverage is sample definition,
not the deferred geographic analysis.

The 2018 directory is dated before Nuble became region 16, while annual tracking
uses the new code. Include region 16 in 2018 coverage because it belonged to the
covered Biobio region 8 at the directory date. This retains 6,604 otherwise
misclassified students. The final eligible sample is 660,426 of 944,510 universe
students. Applicability remains based on the origin region, so a small number of
cross-region applicants from outside covered origins are outside this estimand.

Compute inclusive school-grade-year sample-SD z-scores on complete observed
rosters before restricting to the target universe. SIMCE is first normalized by
subject and national test year and then relative to later schoolmates; scores
must precede the annual record. Undefined moments and unmatched scores stay
missing. Differences of z-scores are relative-achievement comparisons, not learning
gains. Retain all student-school records; the model uses one annual record chosen
by the documented project hierarchy and the latest observed record for each grade.
This leaves application-date origin ambiguous for within-year movers. Preserve
zero attendance as a literal percentage because the source dictionary does not
designate it as missing; this differs from older middle-school-control cleaning.

The 39-question/construct crosswalk was fixed using official dictionaries before
looking at outcome associations. Year-specific questions are not invented in other
years. Reverse the 2013 agreement scale and the 2016 mistreatment item; higher
answers indicate more favorable perceptions. Save exact wording/codes per year.
Include parent response/matching indicators and grade-4 versus grade-8 school
match. Compare a response-availability model with an answer-content model.

Use fixed nested unpenalized logits. Administrative numeric predictors enter with
squares; questionnaire ratings enter linearly. All models use a common sample,
training-only median imputation/scaling, and missingness indicators. Split schools
80/20 with seed 20261005 across all cohorts. Evaluate held-out AUC, Brier score,
log loss, calibration, and school-group/cohort performance. Brier R2 uses the
training-prevalence forecast as denominator. Report origin-school-clustered model
SEs for descriptive associations. No causal or selection-adjusted interpretation.

Full-year grade-8 records can postdate application: label the main analysis
retrospective. Also fit a model excluding grade-8 grades/attendance/gaps, without
claiming that the remaining annual origin identifies a strictly pre-application
information set. No post-grade-8 outcomes enter any predictor.

## Artifacts

Code and detailed run instructions: `code/codex/sae_participation/`.
New derived objects: repository-local `data/clean/sae_participation/`.
Source Box files are read-only. Figures and report use their ordinary output
locations. No unrelated existing databases or code are modified.

## Saved-model reproducibility

The installed fixest version rebuilds factor levels from original estimation data
at prediction time. A bare RDS fit loses the global training environment and can
silently use different reference categories if new data omit a training category.
The original in-session held-out predictions used the correct training context.
`common.R::restore_model` attaches the exact saved training columns from the shared
`model_frame.rds` before prediction, avoiding seven redundant copies of a large
training frame. Both the fits and shared frame are required model artifacts.
The verifier checks prediction reproduction in a fresh process on a small batch
and on the complete holdout.

## First results and verification

Training/test counts: 527,933 / 132,493, with disjoint origin schools. Held-out
AUC is .92238 for the continuation/context baseline, .93473 adding grades and
attendance, .93574 adding SIMCE gaps, .93587 adding parent availability/matching,
and .93591 adding parent answers. The answer content improves Brier loss only
.00003361 (about .04% relative); its incremental gain is concentrated among pupils
still attending the grade-4 school. The model excluding grade-8 annual measures
has AUC .93369. These are one-split predictive results, not causal findings.

The source-data and saved-output verifier passed 48 checks: student and round
identities, coverage, school splits, common model samples, independent rank-based
AUC and Brier calculations, training-only preprocessing, raw 2019 application
reconstruction, 2013/2014 parent scale coding, 20 independent full peer-roster
reconstructions in 2019, base-R logit agreement, and saved-model reproduction.
Report: `output/reports/sae_participation/report.html` (Markdown alongside).

## User refinement: beat the deterministic continuation rule

Bruno clarified that the model to beat predicts continuation for everyone except
pupils in schools without continuation. For the agreed application outcome,
operationalize this as no SAE application where grade 9 is offered, application
where the observed grade-8 origin has no secondary SAE offering. Nonapplication
does not establish actual staying, and application does not establish moving.

Evaluate the existing saved holdout probabilities with a fixed 0.5 cutoff and
equal costs for the two errors. No refit, test-set tuning, or selection of a
favorable cutoff. Restrict all comparisons to the same 120,943 held-out pupils in
the two resolved groups; exclude 11,531 unresolved pupils and 19 in Other SAE
offering. Report accuracy and corrections versus newly introduced mistakes,
overall and within each continuation group. For probability quality, separately
benchmark the group's training application rate, avoiding a comparison only to
overconfident zero/one probabilities. Hard-rule log loss is infinite with errors.

The rule achieves 89.7737% accuracy (12,368 mistakes). The full model achieves
90.0019% (12,092 mistakes), fixes 361 mistakes and introduces 85, a net 276 fewer
errors or 0.2282 percentage points. Of the net corrections, 275 are at schools
ending at grade 8 and one where grade 9 is offered. Excluding grade-8 annual
measures leaves only three net corrections (89.7762% accuracy). High overall AUC
does not establish useful classification of exceptions to school continuation.

An intercept-only regression of each pupil's change in correctness, with
origin-school-clustered SEs, gives a paired 95% CI of [0.1788, 0.2776] percentage
points for the full model's gain. This conditions on the fixed fitted models,
without training or selection uncertainty. Nine confusion-table, paired-count
and Brier identity checks pass. New specification or threshold development
belongs in training/validation; confirmatory future comparisons require a fresh
holdout because these test results have been examined.

Code: `06_continuation_benchmark.R` in the existing task directory. Primary
comparison report: `output/reports/sae_participation/continuation_benchmark.html`
(Markdown alongside). Counts, probabilities, paired intervals and checks are
saved under `data/clean/sae_participation/continuation_benchmark_*`.

Bruno also requested the applicant composition. Across the full analysis sample
(training plus holdout), 325,747 unique students apply: 274,819 from schools ending
at grade 8 (84.3658%), 45,947 from schools offering grade 9 (14.1051%), 4,849 with
unresolved offerings (1.4886%), and 132 with Other SAE offerings (0.0405%). Among
the two resolved groups, shares are 85.6759% school-ending and 14.3241% voluntary.
"Voluntary" describes applications when grade 9 is offered, not realized moves.
These shares cover our grade-8 cohort sample, not every raw SAE grade-9 applicant.
The benchmark report includes this table, saved as
`data/clean/sae_participation/applicant_composition_by_continuation.csv`.
