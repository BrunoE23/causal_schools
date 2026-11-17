# Full-cohort outcomes by attended-school VA

Repeat the four-panel school-VA figures without the high-math condition or the
earlier household-income availability restriction. The prior conditional figures
remain in `output/figures/high_math_by_attended_va/`.

Run from the repository root with R 4.5.1 and data.table, ggplot2, ggrepel, gridExtra:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/all_students_by_attended_va/00_prepare.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/all_students_by_attended_va/01_build.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/all_students_by_attended_va/02_plot.R
```

`CAUSAL_SCHOOLS_REPO_WD` can override the repository root. The prior source
manifest locates the master and checks that its vintage is unchanged.
Preparation reads only 13 columns from the master and caches the broad panel;
subsequent aggregation and plotting use the small local cache and summaries.

## Sample and deciles

Use the paper's broad panel: grade-8 cohorts 2017-2020, ages 12-16, positive
observed `most_time_RBD`. Require attended-school VA in all three dimensions
to keep a common sample across figures. Retain students with missing scores,
scores below +1 SD, and missing household income. No outcome or enrollment
restriction enters sample selection or decile construction.

Use existing 2017-2020 All-sample EB-adjusted school estimates,
`controlled_value_added_eb_centered_student`, for `z_year_math_max`,
`high_paying_field_m1`, and `high_inst_m1`. Recompute weighted inverse-CDF
decile cutoffs separately for each VA measure, weighting each school by its
number of students in the full matched broad panel. Schools remain intact;
cutoff values enter the lower bin. These are new full-population deciles,
not the old deciles weighted by high-scoring students. All four panels use
these same bins, including the income and observed-score means.

## Outcomes

- Premium program, institution, either, both, and HE-enrollment shares use
  all matched students in each bin. Preserve existing flags and non-enrollee zeros.
- Mean math and verbal use observed positive scores, separately standardized
  within admission year in the original broad panel. Missing scores are never
  set to zero. Save each subject's denominator and missing count, plus diagnostic
  means among students with both scores.
- Mean natural-log projected monthly CLP income remains among HE enrollees,
  where program income is defined. Verify all enrollees have income; never
  silently drop them or assign income to non-enrollees.

The scripts verify the broad-panel counts and normalization against the earlier
analysis and reproduce the previous selected sample exactly as a subset.
They also check school-bin ordering, bounded size imbalance, counts, and
premium union/intersection identities. Figures are descriptive associations
with existing estimated VA.

The run matches 926,126 of 931,547 broad-panel students across 3,274 schools;
5,421 lack at least one VA measure. It includes 160,399 students without household
income and 280,478 without math scores. There are 624,730 HE enrollees, all with
projected income, 645,648 observed math scores, and 654,200 observed verbal scores.

Derived data and diagnostics: `data/clean/all_students_by_attended_va/`.
Figures: `output/figures/all_students_by_attended_va/`, with combined four-panel
PNG/PDF files and separate panels for each VA dimension. Labels remain at 1, 8, 10.
