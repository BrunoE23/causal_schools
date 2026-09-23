# Student-weight RSS school-effect moments

Date: 2026-09-23

For comparisons between pooled school value added and the RSS cross-cohort
estimator, aggregate school-level RSS signals using the number of students in
the corresponding pooled VA regression. For outcome-specific variances, the
weight for school `j` is its total estimation-sample count across cohorts. For
cross-outcome covariances, use the number of students in the intersection of
the two outcome regression samples at that school so the covariance has one
symmetric, student-weighted target.

Let normalized weights be `w_j`, the cross-cohort own-product signal be `s_j`,
and the mean school-cohort residual-plus-FE component be `m_j`. Estimate the
weighted latent variance as

`sum_j w_j s_j - (sum_j w_j m_j)^2 + sum_j w_j^2 (m_j^2 - s_j)`.

The last term removes sampling noise from the weighted-mean correction. The
covariance uses the analogous cross-outcome expression. Schools require at
least two usable cohorts and finite school-level signals. Bootstrap inference
continues to resample schools while carrying their student-count weights.
