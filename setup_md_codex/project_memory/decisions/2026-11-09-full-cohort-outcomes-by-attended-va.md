# Full-cohort descriptive outcomes by attended-school VA

The user asked to redo the three four-panel VA figures "without conditioning at
all." Interpret this as removing the high-math threshold, score-observation
requirement, and household-income availability restriction carried over from
the earlier income-decile figures. Retain the paper's base cohort definition
(grade-8 cohorts 2017-2020, ages 12-16, attended high school observed) and common
availability of the three school VA measures needed for these figures.

Keep the user's preference for roughly equal student counts in each VA decile.
Recompute cutoffs using all students in this matched broad panel as school
weights, separately for math, high-premium-field, and high-premium-institution
VA. Use the same existing 2017-2020 All-sample EB-adjusted estimates and inverse
empirical CDF rule, keeping each school intact. Do not retain deciles weighted
by the previous high-scoring subsample or rebuild bins within each outcome.

Enrollment rates use every student in a bin, including those without test
scores or household income. Math and verbal means use observed positive scores
for each subject, with the unchanged admission-year broad-panel standardization.
Record observed and missing counts separately. Projected program income is
defined for HE enrollees, so its panel retains the mean natural-log monthly CLP
income among enrollees and states that denominator explicitly. This does not
condition the population or the other panels on enrollment.

Prepare a separate full-cohort cache and output directory; retain all conditional
figures. Check that the prior selected sample and its outcome definitions are
reproduced as a subset. No new regressions or causal effects are estimated.

The completed run uses 926,126 students in 3,274 schools from a broad panel of
931,547 (5,421 lack complete VA). This includes 160,399 without household income
and 280,478 without math scores. Income is observed for all 624,730 enrollees;
math and verbal means use 645,648 and 654,200 observations respectively.
Both score normalizations reproduce the earlier year-specific counts, means,
and SDs. The original 93,469-student selected sample is reproduced exactly
before the school-VA merge. Counts and union/intersection identities pass in
all three full-sample VA partitions and their cohort-specific summaries.

Code: `code/codex/all_students_by_attended_va/`.
Data: `data/clean/all_students_by_attended_va/`.
Figures: `output/figures/all_students_by_attended_va/`.
