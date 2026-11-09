# High math scores by household income

Descriptive figure requested on 2026-11-09: the fraction scoring at least one
standard deviation above the mean on the PSU/PAES math exam, by household-income
decile. The user confirmed college entrance exams as the score of interest.

Run from the repository root in PowerShell:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/math_high_score_by_income/01_plot.R
```

Requires data.table and ggplot2. `CAUSAL_SCHOOLS_REPO_WD` can override the
repository directory; `CAUSAL_SCHOOLS_DATA_WD` can override the Box/Dropbox data
root containing `data/clean/univ_gr8_df.csv`.

## Definitions

- Broad panel: grade-8 cohorts 2017-2020, ages 12-16, positive observed
  `most_time_RBD`, matching the paper's score-normalization sample.
- Math: positive `math_max`, standardized using its mean and sample SD within
  `psu_year` in this broad panel, before restricting on income availability.
  When available, saved paper score diagnostics must match the calculation.
- High score: `z_math >= 1`. The cutoff is common across income groups within
  each admission year. No verbal-score or timely-exam restriction is added.
- Income: existing `income_decile_imputed`, based on grade-4 SIMCE household
  income with the paper's existing imputations. Students without this decile
  are excluded; income ranks are not recomputed within the plotted sample.
  Existing deciles split tied income midpoints and inherit the master's
  baseline-SIMCE eligibility requirement.
- The blue series divides observed high scores by observed positive math
  scores. The brown series divides the same numerator by all students with
  income data in the broad panel. Missing scores are not imputed; the brown
  series measures observed attainment, not latent achievement of non-takers.
- Fractions pool students with equal weight across cohorts. These are
  descriptive administrative-data shares; no regression or inference is used.

## Outputs and checks

- `output/figures/math_high_score_by_income/`: PNG and vector PDF.
- `data/clean/math_high_score_by_income/`: decile and cohort-decile counts and
  shares, score means/SDs/cutoffs, sample coverage, source manifest, and checks.

The script verifies unique students, standardization, agreement between raw
and standardized cutoffs, all ten deciles, count reconciliation, and
`share_high_all = share_math_observed * share_high_among_scored`.
The source master is read only. See the project-memory decision dated
2026-11-09 for the analysis definitions.

## Premium enrollment conditional on high math

Run `02_premium_enrollment.R` with the same Rscript command to calculate and
plot four first-enrollment shares within the exact previous high-math group.
Run `03_plot_premium_enrollment.R` to re-render from saved aggregate counts.
Set the environment variable `PREMIUM_LABEL_DECILE8=1` when running script 03
to save a separate `_labels_d8` PNG/PDF version with rates labeled at deciles
1, 8 and 10. This version uses the same saved proportions and leaves the
original figure files unchanged.

The four outcomes are the existing `high_paying_field_m1` (program),
`high_inst_m1` (institution), their inclusive union (either), and their
intersection (both). Program is the paper's high-paying field classification
(Science, Law, Engineering/Manufacturing/Construction, Medicine+), not a new
monetary cutoff for individual programs. Institution is the existing indicator
for a centered MiFuturo institution FE above 0.1 log points. The master retains
zeros for non-enrollees, and for institutions without an observed premium.

Every denominator is all high-math students with an income decile, including
non-enrollees. No additional enrollment, verbal-score or regression-control
condition is imposed. Missing outcomes cause a stop rather than changing
denominators. The prior score cutoffs are reused and all ten high-math counts
must match the previous figure exactly. Outcomes use first observed enrollment,
not current enrollment. These are raw conditional shares, without regression
adjustment within the high-math group.

New local inputs, counts and checks are in
`data/clean/math_high_score_by_income/premium_enrollment/`; the PNG/PDF are
`premium_enrollment_given_math_1sd_by_income_decile` in the same figure folder.

## Four high-paying field components

Run `04_field_components.R` to build and plot the exact decomposition; run
`05_plot_field_components.R` to re-render from saved aggregates. The sample
and denominators are read unchanged from the prior high-math student cache.

Science, Law, and Engineering/Manufacturing/Construction use their existing
first-enrollment flags. Medicine+ is the intersection of the existing
high-paying program flag with the Medicine classification. This preserves
the original six-program positive list (medicine, pharmacy, nursing,
midwifery, medical technology, dentistry), which is already encoded in the
combined high-paying flag. It does not use all Medicine classifications.
The script verifies mutual exclusivity and that the four components sum
exactly to the existing combined flag for every student and income decile.
Every component is zero for non-enrollees.

Counts, shares, classification audits and checks are in the local
`data/clean/math_high_score_by_income/field_components/` folder. The new figure
is `field_components_given_math_1sd_by_income_decile` in the figure folder.

## Mean math and verbal scores in the selected sample

Run `06_mean_scores.R` to calculate and plot means; `07_plot_mean_scores.R`
re-renders from the aggregate statistics. Retain the entire previous
high-math cache. Math uses its unchanged z-score. Verbal (`leng_max`) is
standardized separately within admission year among positive observed verbal
scores in the same broad panel as math, before income or high-math selection.
Its normalization must match the paper's saved score diagnostics.

Plot equal-student-weighted mean standardized scores by the existing income
deciles. Do not standardize within the selected sample or decile. Math retains
every high-math student; any missing verbal scores are excluded only from the
verbal mean and explicitly counted. The aggregate output also includes the
math mean among students with verbal scores to assess any sample difference.
No HE-enrollment or additional control restriction is applied.

Outputs are in `data/clean/math_high_score_by_income/mean_scores/` and figure
`mean_scores_given_math_1sd_by_income_decile` in the same figure folder.
