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
