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
