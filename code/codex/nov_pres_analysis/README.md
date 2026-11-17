# November slide analysis

Run from the repository root with R 4.5 and packages data.table and fixest:

`Rscript code/codex/nov_pres_analysis/run.R`

Reads selected columns from the cached September EB regression dataframe under
`output/intermediate/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/`.
Reproduces all four main estimates, robust SEs, sample counts and first-stage F
statistics against the September main_results.csv before estimating subgroups.

Estimates separate IV regressions by grade-4 math rank quintile and gender for
math, verbal, high-premium institution and field. Quintiles use average ranks
among observed grade-4 math scores in the joint lottery dataframe, before
outcome restrictions, matching the legacy scripts. All four regressions require
exam taking, plus outcome-specific complete cases, and use the main expected-VA
and baseline controls. Gender indicators are constant and omitted within gender.
Uses fixest heteroskedasticity-robust (HC1) inference, treating estimated VA as
fixed, as in the main paper. Differences between disjoint groups use the sum of
their estimated variances; these are unadjusted exploratory comparisons.

The cross-outcome slide extracts the four-by-four direct-IV block from the
September paper's saved unshrunk cross-outcome results. It is not an EB rerun.
No RSS-implied projections or EB percentile gaps are substituted for direct IV.

Outputs: analysis CSVs and session info in `data/clean/nov_pres_analysis/`,
figures in `output/figures/nov_pres_analysis/`, and the cross-outcome LaTeX table
in `output/tables/nov_pres_analysis/`. Existing August and paper outputs remain
unchanged. The November deck includes these exports directly.
