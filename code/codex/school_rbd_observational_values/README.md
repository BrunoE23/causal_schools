# Observational school value added

Run `01_construct_school_rbd_values.R` with R from the repository root.
The script finds the data root using `CAUSAL_SCHOOLS_DATA_WD`, then the
configured Box/Dropbox candidates. Required packages are data.table, dplyr,
fixest, lfe, purrr, readr, and tidyr.

The primary input is `data/clean/univ_gr8_df.csv` under the data root.
Read only the required columns. Prefer consolidated middle-school controls
and full program-income outcomes. The lowercase `most_time_rbd_middle`
column is normalized to the existing internal name. Separate control and
income files are fallback inputs only when the required columns are absent.
The legacy `program_income_clp_m1` and `log_program_income_clp_m1` output names
remain aliases of the full program-income measures, matching the income
constructor. Field indicators and STEM are rebuilt from the current
`field_reclassified_m1` and `field_reclassified_ml` classifications.

The constructor estimates the existing outcome-specific school and
middle-school fixed-effect regressions with baseline controls. It retains
the existing age, complete-case, and outcome-specific exam-taking restrictions.
It does not add sector, SAE, lottery-risk, or explicit cohort restrictions.
Standard errors use the existing lfe bootstrap configuration (100 replications
by default); this can make a full run lengthy.

Outputs are regenerated under `data/clean/school_rbd_observational_values/`:
`school_rbd_observational_values.csv` and `score_scale_diagnostics_by_year.csv`.
The source universe is never modified. Empirical-Bayes shrinkage is a separate
downstream script in `../empirical_bayes_school_va/`.
