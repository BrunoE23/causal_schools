# Admission Probability Controls

This folder contains a Codex-owned version of the admission-probability export used for the IV estimation sample.

The script `01_export_probability_controls_k25.R` starts from the simulated DA probability files in Dropbox:

- `data/clean/DA_probs/DA_probs_2018.csv`
- `data/clean/DA_probs/DA_probs_2019.csv`
- `data/clean/DA_probs/DA_probs_2020.csv`
- `data/clean/DA_probs/DA_probs_2021.csv`

It then joins those probabilities to the broad grade-8 analysis universe in `data/clean/univ_gr8_df.csv`, keeping only students with `timely_sae == 1` and matching probability rows.

The practical probability-control support is defined among timely, at-risk SAE students. A school/probability option is retained if at least `25` at-risk students have positive simulated assignment probability for it. This reduces the number of wide probability-control columns while keeping the support relevant for the estimation sample.

Outputs are written to Dropbox under `data/clean/DA_probs/`:

- `probs_columns_wide_k25_timely_risk.csv`
- `probability_support_k25_timely_risk.csv`
- `probability_export_diagnostics_k25_timely_risk.csv`

The Stata `.dta` export is intentionally off by default because writing very wide `.dta` files can be slow. To create `probs_columns_wide_k25_timely_risk.dta`, set `write_stata_dta <- TRUE` at the top of the script.

The wide file contains one row per timely at-risk student with matching probability data, retained `prob_<rbd>` columns, matching `iszero_<rbd>` columns, and diagnostic fields such as `retained_probability_mass`.
