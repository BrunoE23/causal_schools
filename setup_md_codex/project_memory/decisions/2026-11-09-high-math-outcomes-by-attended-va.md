# High-math descriptive outcomes by attended-school VA

The user requested repetition of the last two figures (premium program,
institution, either/both; and mean log projected income among HE enrollees
plus HE-enrollment rate) by deciles of attended-school math, high-premium-field
and high-premium-institution VA. The user explicitly chose equal numbers of
students rather than equal numbers of schools for the deciles.

Retain the earlier selected high-math cache, including its original household-
income availability requirement, and match `most_time_RBD` to the 2017-2020
All-sample EB school estimates. Use
`controlled_value_added_eb_centered_student` for `z_year_math_max`,
`high_paying_field_m1`, and `high_inst_m1`. Match all three dimensions on one
common sample: 93,466 of the previous 93,469 students; three lack school VA.

Deciles are student-weighted in this common selected sample: school weights
are the counts of these high-math students attending each school. At each
10% cumulative mass, choose the first VA attaining that mass (weighted inverse
empirical CDF). Assign boundary values to the lower bin and keep each school
intact. This produces roughly, not necessarily exactly, equal student counts.
Deciles are specific to each VA dimension. Do not weight by stage-1 regression
counts, split same-school students, or recompute bins for HE enrollees.

All enrollment probabilities use the full matched selected sample in each
bin. Log projected income is the mean existing natural-log monthly CLP measure
among observed HE enrollees only. Preserve existing premium program and
institution flags, including non-enrollee zeros. Label deciles 1, 8 and 10.
No new model or causal effect is estimated; the figures use the existing
pooled estimated VA and observed student outcomes.

Implementation: `code/codex/high_math_by_attended_va/`.
Derived data: `data/clean/high_math_by_attended_va/`.
Figures: `output/figures/high_math_by_attended_va/`.
