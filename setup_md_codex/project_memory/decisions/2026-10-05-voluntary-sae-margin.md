# Predicting voluntary SAE applications

Date: 2026-10-05. Bruno requested prediction specifically at the voluntary margin.

## Target and sample

Restrict the existing eligible grade-8 cohorts to origins offering grade 9 in
SAE A1. The outcome remains an explicit regular or complementary grade-9 SAE
application during the first observed grade-8 year. "Voluntary" means continuation
is offered at the origin; it does not establish motivation or an actual move.
The sample has 313,813 pupils and 45,947 applicants. Preserve the original school
split: 251,624 training pupils and 62,189 test pupils (9,628 test applicants).
Do not redefine the target as applying conditional on having applied, and do not
restrict away questionnaire nonresponse or missing scores. Geography is deferred.

## Models and evaluation

Re-estimate the original predictor blocks solely among voluntary-margin pupils:
context; grades/attendance levels and peer z-scores with squares; SIMCE-relative
gaps with squares; parent availability/matching; parent responses. Also estimate
the full specification excluding annual grade-8 measures. School continuation is
constant and omitted. Re-estimate medians, scaling and missingness terms from
voluntary training observations. Parent answer content is compared to an otherwise
identical availability model. Context includes cohort-by-region, sector, gender,
and age/square/missingness. Use fixest logits with origin-school-clustered SEs.

Add two limited CART probability-tree alternatives (full and excluding grade 8),
using R rpart regression on the binary outcome. Leaf means are probabilities;
splits and pruning minimize squared error. Maximum depth 8, minimum leaf size
200, no class reweighting or surrogate splits. Pick complexity from .01, .005,
.001, .0005, .0001 using an inner 20% school validation split within original
training (seed 20261007). Select highest validation accuracy at .5, break ties by
lowest Brier loss, then larger penalty. Refit selected complexity on all original
training pupils. The inner imputation/scaling never uses validation observations.

The initial classification-tree implementation collapsed to a constant under
classification-error pruning because the minority outcome did not change leaf
majorities. Probability trees address this criterion issue; they are not chosen
by favorable test results. This is nevertheless an exploratory follow-up: the
test sample was already examined in the preceding exercise. Do not characterize
its results as an independent confirmatory test or select a winner on the test.

Primary benchmark: predict no application for everyone. All classifiers use .5
and equal error costs, without cutoff tuning. Report applicants detected, false
positives, net errors avoided, accuracy, and paired school-clustered gain CIs
conditional on fitted models. Probability benchmarks use the voluntary training
application rate. Also report AUC, average precision with tied-score grouping,
Brier loss, calibration and the share of applicants found in the highest-risk
10%. Outcome-independent student ID resolves ties when selecting that exact
top decile. Compare with original pooled predictions on identical test pupils.

## Implementation and reproducibility

Scripts `07_voluntary_margin.R`, `08_voluntary_report.R`, and
`09_verify_voluntary.R` live in `code/codex/sae_participation/`. Outputs are in
`data/clean/sae_participation/voluntary/` and
`output/reports/sae_participation/voluntary_margin.html` (Markdown alongside).
The cached administrative panel is read once and immediately filtered; no raw
data or prior model outputs are overwritten. The installed fixest drops saved
data with `lean=TRUE` even when `data.save=TRUE`, so exact required training
columns are explicitly attached to each saved fit. A separate R session verifies
complete-holdout reproduction for both full and no-grade-8 logits and trees.

Full-year grade-8 measures may postdate applications. Excluding them helps assess
the value of earlier information, but annual school origins still do not establish
application-date enrollment. No causal or realized-school-change interpretation.

## Results and verification

The 62,189 held-out pupils include 9,628 applicants (15.4818%), so predicting no
application yields 84.5182% accuracy. The full voluntary logit has AUC .65687
(pooled full model .64643), average precision .25489, and Brier .125421 versus
.130959 for the training-rate forecast. Its highest-risk 10% have 31.2269%
application prevalence and contain 20.1703% of applicants. Yet at .5 it identifies
only three applicants and falsely flags five nonapplicants: two additional errors
relative to the rule, with accuracy 84.5149%. Excluding grade-8 annual measures
gives AUC .64981 and the same net two additional errors. Parent answer content
changes Brier from .125517 to .125421, a small increment.

Both tree specifications select cp .0001 in inner validation. The full tree has
AUC .62278 and exactly the no-application classification. The tree excluding
grade-8 annual measures has AUC .61495 and detects 59 applicants with 34 false
positives: 25 fewer errors, accuracy 84.5584%, gain .0402 percentage points.
Its paired school-clustered 95% CI is [-.0389, .1193] percentage points. This
small gain is not statistically decisive and does not justify selecting the tree
as a confirmed winner. Probability ranking is modestly informative, but these
models barely improve the simple classification rule.

67 checks passed in the final cached-estimate run plus fresh-session verification.
They cover school separation, common samples, training references in saved fits,
complete-holdout prediction reproduction, convergence, independent confusion
counts and grouped-rank AUC, and exclusion of grade-8 terms in sensitivities.
The risk-decile figure was rendered and visually inspected.
