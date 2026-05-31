# SIMCE 8B Heterogeneity Inputs

This folder cleans the 2019 8th-grade SIMCE private files for the first
heterogeneous-effects pass.

The task is intentionally parallel to `code/clean_simce_survey.R`, which cleans
4th-grade SIMCE inputs already used in `univ_gr8_df`.

## Script

- `01_clean_simce8_2019.R`
- `02_append_to_univ_gr8_df.R`

Run from the repository or an R session with access to Dropbox:

```r
source("code/codex/simce8_heterogeneity/01_clean_simce8_2019.R")
```

If the Dropbox path is not one of the default local paths, set:

```r
Sys.setenv(CAUSAL_SCHOOLS_DATA_WD = "C:/path/to/causal_schools")
```

After creating the heterogeneity file, append the variables to the already-built
broad regression dataset with:

```r
source("code/codex/simce8_heterogeneity/02_append_to_univ_gr8_df.R")
```

Future full rebuilds through `code/universe_reg_df.R` also merge the same
SIMCE 8B variables directly.

## Inputs

- `data/raw/simce/Simce octavo básico 2019 - Versión privada/Archivos TXT (Planos)/simce8b2019_alu_privada_final-SEG.csv`
- `data/raw/simce/Simce octavo básico 2019 - Versión privada/Archivos TXT (Planos)/simce8b2019_cpad_final_SEG.csv`
- `data/clean/simce_4to.Rdata`

The grade-8 score variables are:

- `ptje_mate8b_alu`: SIMCE math score
- `ptje_lect8b_alu`: SIMCE reading/verbal score

The grade-8 CPAD income variable is:

- `cpad_p11`

The 2019 glosses define `cpad_p11` as the same 15-bin household-income question
used in the 4th-grade cleaner. Codes `0` and `99` are treated as missing.

## Outputs

All outputs are written under:

- `data/clean/simce8_heterogeneity/`

Files:

- `simce8_2019_clean.csv`
- `simce8_2019_clean.Rdata`
- `cohort_2019_math_heterogeneity.csv`
- `cohort_2019_math_movement_summary.csv`
- `simce4_to_simce8_overlap.csv`
- `simce4_to_simce8_prediction_summary.csv`
- `simce4_to_simce8_transition_matrices.csv`
- `simce8_2019_diagnostics.csv`

## Cleaning Rules

- Read the TXT/CSV planos rather than XLSX files for speed.
- Merge grade-8 ALU scores to grade-8 CPAD income by `idalumno` and `simce_year`.
- Keep one row per `mrun`, using the row with the highest observed average of
  math and reading scores when duplicate ALU rows exist.
- Preserve students with missing grade-8 scores if they have an ALU row and
  `mrun`; score deciles remain missing for those cases.
- Convert `cpad_p11` to `income_mid_8b` using the same midpoint map as the
  4th-grade SIMCE cleaner.
- Construct `z_sim_mat_8b`, `z_sim_leng_8b`, score deciles, average-score
  decile, and `income_decile_8b`.
- For the 2019 grade-8 cohort in `universe_controls.csv`, write one row per
  `mrun` to `cohort_2019_math_heterogeneity.csv`. Students without matched
  grade-8 or grade-4 SIMCE data are retained with missing heterogeneity fields.

## Math Heterogeneity Variables

The math-only heterogeneity file keeps the requested simple bins:

- `simce8_math_decile`: grade-8 SIMCE math decile
- `simce8_math_quintile`: grade-8 SIMCE math quintile
- `simce8_vs_4to_math_decile_change`: grade-8 math decile minus grade-4 math decile
- `simce8_math_improved_gt1_decile`: equals `1` if the student improved by more than one decile
- `simce8_math_within1_decile`: equals `1` if the grade-8 decile is within one decile of grade 4
- `simce8_math_worsened_gt1_decile`: equals `1` if the student worsened by more than one decile
- `simce8_math_decile_movement`: string version of those three mutually exclusive groups

The same variables are carried into `data/clean/univ_gr8_df.csv` and
`data/clean/univ_gr8_df.dta` by `02_append_to_univ_gr8_df.R` or by a future
rerun of `code/universe_reg_df.R`. They are also included in the compact scalar
school-value IV regression exports.

## Prediction Exercise

The script loads `simce_4to.Rdata`, constructs grade-4 math, reading, and
average-score deciles, and compares them with the corresponding 2019 grade-8
deciles. It also compares 4th-grade CPAD income decile with 8th-grade CPAD
income decile.

The prediction summary reports, for each measure:

- overlap sample size
- correlation
- simple linear slope and R-squared from grade-8 decile on grade-4 decile
- share in the same decile
- share within one decile
- mean absolute decile gap
