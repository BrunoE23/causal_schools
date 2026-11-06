# Lottery cross effects of EB math school VA

Run `01_estimate.R` from the repository root with R, data.table and fixest.
It re-estimates effects on high-premium field and high-premium institution
using the current main-IV sample and **shrunk** math VA throughout.

The saved 138 MB current main-IV frame is
`data/clean/peer_lottery_variation/main_regression_df_with_peer_shocks.csv`.
Only original main-IV columns are read; no peer variable enters a regression
or sample filter. The initial 162,633 unique students are timely at-risk SAE
applicants in processes 2018-2020. Each regression retains admission-exam takers
and complete cases for its outcome, treatment, instrument and controls.

Treatment is `d_math_adj_eb`, the EB math VA of `most_time_RBD`. Instrument is
`z_math_adj_eb`, the EB math VA of the first-round offered school. Include
`expected_math_adj_eb` as the matching assignment-risk control, plus cohort,
gender and Grade 8 age indicators and linear Grade 4 math and verbal scores.
No PAES math control or school fixed effects are added. Outcomes retain the
current main-IV binary definitions and non-enrollee zero coding.

VA is the current unified R/lfe All-sample
`controlled_value_added_eb_centered_student`, not the unshrunk counterpart.
Attended/offered values and missingness are verified against that exact source;
no-offer instruments are zero as in the main specification. This VA run is
distinct from the older saved Stata EB files used for the preceding simple
school-level correlations. The expected-VA variable is retained exactly from
the current main frame.

Estimate linear 2SLS with fixest `vcov="hetero"` and its default finite-sample
correction, preserving the main specification's SEs. Wald p-values and 95%
intervals use the same normal convention as the main tables. Verify the
same-outcome math, field and institution main betas, SEs and Ns before reporting
cross effects. For each model also check that reduced-form/first-stage equals
the IV coefficient on exactly the same student sample.

Results and source, sample and main-IV parity audits go to repository
`data/clean/math_va_cross_effects_eb/`. Coefficients for binary outcomes are
probability units per one unit of EB math VA, where math VA is measured in
math-score SD. No new scaling, probability-support filter, private-school
exclusion or selection correction is introduced.

The rerun gives math-EB-VA to field beta=-0.17060957, SE=0.05303222,
p=0.00129497, and math-EB-VA to institution beta=0.08343878, SE=0.03483369,
p=0.01660462. Both use N=94,487, with first-stage F=3,257.89. The negative field
and positive institution signs therefore also hold with the current shrunk
math VA, not just in the saved unshrunk cross-outcome table.
