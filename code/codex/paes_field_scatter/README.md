# PAES math score and high-premium field enrollment

Run `01_plot_paes_math_field.R` from the repository root with R, data.table and
ggplot2. Uses selected original columns of the saved current main-IV frame
`data/clean/peer_lottery_variation/main_regression_df_with_peer_shocks.csv`.

The unit is an individual student, not a school VA estimate. Sample: timely
at-risk SAE applicants in processes 2018-2020 with observed
`z_year_math_max` and `high_paying_field_m1`. No IV-control or baseline-score
completeness restriction is imposed. Math is the existing project score
standardized within test year, pooling PAES/PDT score vintages. The first
recorded 2022-2025 high-premium field enrollment indicator includes zeros for
non-enrollees, consistent with the main outcome. It is not conditional on
college enrollment or restricted to college applicants.

The main plot shows mean math and the enrollment share in 50 rank bins,
keeping equal scores together. OLS is fitted to all individual observations
with equal student weights and no controls. Displayed R-squared and slope refer
to the individual-level model. No inferential band is added. The individual
binary-outcome scatter is also saved; no jitter is used. The relationship is
descriptive, with the existing unequal first-enrollment observation window.

PNG/PDF outputs: `output/figures/paes_field_scatter/`. Aggregate bins and fit
summary: `data/clean/paes_field_scatter/`. Checks verify unique students,
sample flags, valid binary outcomes, non-enrollee zero coding, exactly 50
nonempty bins, and preservation of sample size and the overall outcome mean.

## Broad-panel version

```powershell
$env:SCATTER_SAMPLE = 'broad_panel'
& 'C:/Program Files/R/R-4.4.1/bin/Rscript.exe' code/codex/paes_field_scatter/01_plot_paes_math_field.R
```

This uses the established 931,547-student broad panel: Grade 8 cohorts
2017-2020, Grade 8 age 12-16, positive observed most-time high-school RBD,
unique mrun, including private schools. No SAE or baseline-completeness
restriction is imposed. Score/outcome missingness is the only subsequent
filter. It reads selected universe columns and joins the existing small
`program_info_22-25.rds` by the unique program code to recover generic career
area. The exact `derive_high_paying_field` helper and dependencies are imported
from the existing person-outcome constructor without running its pipeline.
The reconstructed binary is checked against all 162,633 students in the
previous at-risk input; every value and missingness pattern must agree.

PAES/PDT math is standardized using the **broad panel's** within-test-year
mean/SD, saved in a separate audit. The earlier saved main-IV frame implicitly
uses an SAE-only score-standardization population; this reference-population
difference is explicit here. No raw scores are pooled across scales.
Outputs go under `broad_panel/` subdirectories, preserving the at-risk figures.
