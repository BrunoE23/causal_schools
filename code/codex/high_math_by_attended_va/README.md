# High-math outcomes by attended-school VA decile

Repeat the premium-enrollment and income/HE-enrollment figures from
`math_high_score_by_income`, replacing household-income deciles with attended
school VA deciles separately for math, high-premium field, and institution.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_math_by_attended_va/01_build.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/high_math_by_attended_va/02_plot.R
```

Requires data.table, ggplot2, ggrepel and gridExtra. `CAUSAL_SCHOOLS_REPO_WD` overrides
the repository root. The earlier source manifest locates the external VA data.

## Sample and measures

Reuse `data/clean/math_high_score_by_income/income_enrollment/high_math_with_income.rds`:
math z >= 1, grade-8 cohorts 2017-2020, ages 12-16, observed attended high school
and existing household-income decile. Retaining the original income availability
condition preserves comparability even though income no longer defines bins.
Restrict to students with school VA observed in all three dimensions; report
all exclusions. Join `most_time_RBD` to `school_rbd`.

VA comes from the current 2017-2020 EB school estimates, `analysis_sample == All`,
column `controlled_value_added_eb_centered_student`. The three outcome keys are
`z_year_math_max`, `high_paying_field_m1`, and `high_inst_m1`.

The user explicitly chose roughly equal numbers of students per decile.
Weight each school by its number of students in the common selected high-math
sample. For each VA dimension, use inverse empirical CDF thresholds at the
10th through 90th percentiles. Values equal to a cutoff enter the lower bin.
Keep all students at the same school together, so bin sizes need not be exactly
equal. Do not use regression sample counts or equal school weights. The same
bins apply to the income panel; do not redefine them among HE enrollees.

## Outcomes and outputs

Premium program, institution, inclusive either, both, and HE-enrollment shares
use all matched high-math students in the decile. Mean natural-log projected
monthly CLP income uses HE enrollees only. The original binary definitions,
income estimates and high-math normalization are unchanged. These are
descriptive associations with estimated school VA, not new causal estimates.

The added score panel joins the existing `mean_scores/high_math_with_verbal.rds`
cache by student ID after checking that IDs, admission years, and math scores
match the income cache. Math and verbal remain separately standardized within
admission year in the original broad panel. Math means use all matched students;
verbal means omit missing verbal scores only. Missing verbal scores never change
the selected sample, school weights, or VA-decile assignments. The aggregates
include subject-specific counts and a common-verbal-support math mean diagnostic.

Aggregates, school maps, cutoffs, sample coverage and verification are under
`data/clean/high_math_by_attended_va/`; figures under
`output/figures/high_math_by_attended_va/`. Each VA dimension has separate
premium, income/enrollment, and mean-score plots plus a combined four-panel version.
Labels show deciles 1, 8 and 10.
