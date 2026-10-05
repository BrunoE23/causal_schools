# Predicting SAE participation in grade 8

This task implements the October 5, 2026 discussion with Bruno. The primary
outcome is an explicit grade-9 application while in the student's first observed
grade-8 year, in the regular or complementary SAE round. Geography/distance is
deliberately deferred. This is a retrospective prediction exercise, not a causal
estimate or a claim of forecasting with information available before applications.

## Run

From the repository root, run these scripts in order with R 4.5 or later:

1. `00_preflight.R` inventories inputs and extracts the official questionnaire dictionaries.
2. `01_build_admin.R` constructs the student panel and complete school-grade-year peer statistics.
3. `02_build_parents.R` joins and harmonizes the selected parent questions.
4. `03_fit.R` estimates the fixed logistic specifications and evaluates held-out schools.
5. `04_report.R` generates the report and diagnostic figures.
6. `05_verify.R` checks the resulting data and predictions.
7. `06_continuation_benchmark.R` compares the saved predictions with Bruno's
   deterministic continuation rule, without refitting or tuning on the holdout.
8. `07_voluntary_margin.R` refits the predictor blocks among origins offering
   grade 9, and compares two probability trees with complexity chosen in an
   inner school validation split. It requires the standard R package `rpart`.
9. `09_verify_voluntary.R` reproduces saved predictions in a fresh R session.
10. `08_voluntary_report.R` reports the voluntary application margin and risk deciles.

Code paths in this list are relative to `code/codex/sae_participation/`.
Required R packages: `data.table`, `readxl`, `fixest`, `ggplot2`.
No Stata, Python modeling, or additional package installation is required.
`inspect_parent_items.py` is a read-only helper for reviewing dictionary text.

To reload a fitted model, source `common.R` and use
`fit <- restore_model('plus_parent_answers')`, then `predict(fit, newdata=..., type='response')`.
New data must have the saved preprocessing columns. `restore_model` attaches the
exact training columns from `model_frame.rds`, which is shared by all seven fits.
Do not call `predict` on a bare `readRDS` fit in a fresh session: this fixest
version otherwise reconstructs factor reference categories from new data when
the original training environment is unavailable. The saved frame is an integral
part of the model artifact. Verification checks both a small prediction batch
and the complete holdout after restoring the model.

The source root is `CAUSAL_SCHOOLS_DATA_WD`, defaulting to
`C:/Users/brunem/Box/causal_schools`. Source data are read-only. All new derived
data, diagnostics and model objects go to this repository's
`data/clean/sae_participation/`; reports go to `output/reports/sae_participation/`
and figures to `output/figures/sae_participation/`.
The builder refuses to overwrite its final panel unless given `--overwrite`;
intermediate annual caches permit a failed build to resume. Rebuild caches with
`--overwrite` after changing record construction or peer normalization.

## Inputs and definitions

- Existing `universe.csv`, restricted to first-observed grade-8 cohorts 2017-2020,
  without conditioning on later high-school attendance, SIMCE response, or test taking.
- Annual Rendimiento records, 2013-2020, ordinary basic education (`COD_ENSE=110`),
  grades 5-8, observed final status, valid student and school IDs.
- Full raw grade-4 SIMCE student files for 2013-2016 and corresponding CPAD files.
  Earliest observed grade-4 record is selected; within-year duplicates prefer more
  available scores, then higher mean score, then student-survey ID, deterministically.
  Unlike the legacy clean SIMCE file, students missing both scores are retained.
- Raw C1 and C2 applications, reconstructed separately by process; an automatically
  added continuity school alone is not an explicit application. `applied_regular`
  is retained separately. The primary outcome covers the two observed official rounds,
  not subsequent off-platform or regularization activity.
- SAE A1 offerings and contemporaneous school directory. Analysis covers origin
  regions with listed grade-9 offerings that year. The directory is used only for
  school metadata and mapping SAE coverage, not for an invented legal maximum grade.

Within each school-grade-year, performance and attendance are standardized using
the full observed roster, including the student, with sample SD. Grade-4 SIMCE
math and language are first standardized within their own national test year;
each is then standardized among the current school-grade-year roster. Only
baseline tests strictly earlier than the annual record enter. The two gaps are
`z_gpa - z_baseline_subject`; they are not interpreted as learning gains.
Missing SIMCE pupils do not enter SIMCE peer moments. Peer sample sizes are saved.
Zero or undefined SD makes a relative measure missing, not zero.

All distinct student-school-grade-year records are retained in
`all_student_school_records.rds`. One annual model record follows the existing
project preference for positive attendance, then promotion, then highest attendance,
then smallest RBD; for a repeated grade, use its last observed year up to the cohort.
Earlier grades are never filled with later outcomes. This annual selection does
not identify exact school enrollment on the date of application. Origins with
multiple schools in grade 8 are flagged. Grade histories before 2013 remain missing.
Grades outside 1-7 and attendance outside 0-100 are missing. Literal zero attendance
is retained because the official dictionary does not identify zero as a missing code.

For a student observed in grade 8, `ends8_sae=1` means their origin school appears
in SAE A1 with no secondary grade offered. Early rollout menus list only entry
grades in some regions, so the maximum listed grade need not literally equal 8.
`offers_grade9` is direct evidence of grade-9 continuation, independent of vacancies.
Schools absent from A1 are unresolved, including many private schools; they are not
silently classified as ending in grade 8. Other secondary offerings form a separate
category. This is a statement about observed SAE offerings at the RBD level, not a
complete registry of every school's legally authorized grades or campus continuity.

`parent_crosswalk.csv` fixes 39 questions/constructs before inspecting outcome
associations. `parent_dictionary.csv` saves exact Spanish wording and original
coding by year. Similar questions are harmonized by stated construct; distinctive
questions remain year-specific. All valid answers are 1-4 or 1-7. The 2013 agreement
items and the 2016 mistreatment item are reversed so higher values are favorable.
Blank, double-mark, not-offered, and out-of-range answers are missing. Not-asked
years remain missing. These reports describe the grade-4 school, and whether that
RBD matches the grade-8 origin is included in the questionnaire block.

## Models and validation

A fixed seed (20261005) allocates 80% of origin schools to training and 20% to
testing, keeping a school in the same split across cohorts. All models use the
same pupils; missing predictors are imputed with the training median and retain
missingness indicators. Scaling also uses training observations only. Peer
statistics use observed administrative rosters but never application outcomes.

Unpenalized binomial logit models successively add school continuation, own and
peer-relative grades/attendance, SIMCE gaps, questionnaire availability, and parent
answers. Administrative continuous predictors enter linearly and quadratically;
parent ordinal responses enter linearly. Controls are cohort-by-region, school
sector, gender and age (age outside 9-25 is marked missing, with no sample exclusion).
The availability-only model distinguishes response/matching information from the
additional information in answers. The final sensitivity excludes grade-8 annual
grades/attendance/gaps; it is not a guaranteed pre-application forecast because
the school origin is still measured in annual records.

The primary classification benchmark predicts no application where grade 9 is
offered and an application where the school ends at grade 8 in SAE offerings.
Compare accuracy, mistakes corrected and introduced, and net errors avoided,
overall and separately by continuation group. Use a fixed 0.5 probability cutoff
and equal error costs. Restrict this comparison to the same held-out pupils in
the two resolved groups; exclude unresolved and Other SAE offerings explicitly.
The target remains application, not realized continuation. Paired accuracy-gain
intervals cluster by origin school and condition on the fitted models. For
probability scores, also compare with each group's training application rate.
The benchmark report is `output/reports/sae_participation/continuation_benchmark.html`.

Also report AUC, Brier score, log loss, calibration, and Brier R2 relative to the training
prevalence forecast; separately by cohort and school continuation group. No test
outcomes are used for feature selection or tuning. Model coefficient SEs cluster
at origin school. Parent associations are descriptive, jointly conditional,
correlated, and not causal effects. Missingness terms and collinearity drops are
saved. Geography and a separate actual-school-change outcome are not estimated.

Empirical rationale is also recorded in
`setup_md_codex/project_memory/decisions/2026-10-05-sae-participation-prediction.md`.

## Voluntary application margin

The follow-up restricts to 313,813 pupils with grade-9 SAE offerings at their
grade-8 origin. All 62,189 test pupils remain in the original school holdout.
The no-application rule is the primary classification benchmark. Six logits
re-estimate context, grades/attendance, SIMCE gaps, parent availability, parent
answers, and a full specification excluding grade-8 annual measures. Medians,
scaling and missingness terms are recomputed from voluntary training pupils.
Standalone saved fits carry their estimation data to preserve factor references.

Two CART regression alternatives use the binary outcome, so leaf means are
application probabilities and splits/pruning minimize squared error. They use
all predictors or exclude grade-8 annual measures,
with no class reweighting, depth at most 8 and leaves of at least 200 pupils.
Select complexity from {0.01, 0.005, 0.001, 0.0005, 0.0001} on an inner 20% school
validation split (seed 20261007): maximize accuracy at 0.5, break ties by Brier
loss, then prefer the larger complexity penalty. Inner imputation and scaling
use inner training only. Refit the selected complexity on all original training
pupils. No probability cutoff is tuned. No test outcome selects a model.

Report accuracy, detected applicants, false positives, paired school-clustered
accuracy-gain intervals, AUC, average precision, Brier loss, and the application
rate and recall in the top predicted decile. The original pooled predictions
provide a same-student comparison. Prior inspection of this holdout makes this
an exploratory follow-up. Grade-8 annual measures remain retrospective; neither
applying nor nonapplying establishes an actual move or continuation.

New objects are in `data/clean/sae_participation/voluntary/`; the report is
`output/reports/sae_participation/voluntary_margin.html` (Markdown alongside).
For repairs limited to serialization, tree specifications or scoring, the
`--reuse-logits` flag reloads the existing unchanged logit estimates. Do not use
this flag after changing input data, preprocessing, sample or logit formulas.
