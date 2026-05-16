# Middle-School Controls

This folder constructs student-level middle-school controls for the grade-8 universe.

Run:

```r
source("code/codex/middle_school_controls/01_construct_middle_school_controls.R")
```

Inputs:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/universe.csv`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/tracking_univ8gr.RData`
- `C:/Users/xd-br/Dropbox/causal_schools/data/raw/student_tracking/2014-2016/`

Outputs:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/middle_school_controls/middle_school_controls.csv`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/middle_school_controls/middle_school_controls_diagnostics.csv`

The script uses observed grade 5-8 enrollment rows from three years before through each student's first observed grade-8 cohort year. It uses the existing full-universe `tracking_univ8gr.RData` object for 2017-2020 and reads raw 2014-2016 enrollment files to complete the historical middle-school window for earlier grade-8 cohorts.

Controls constructed:

- `most_time_RBD_middle`: the RBD where the student spent the most observed middle-school years, breaking ties by latest year and then RBD.
- `z_gpa_middle_mean`: mean of annual GPA z-scores across observed middle-school years.
- `z_att_middle_mean`: mean of annual attendance z-scores across observed middle-school years.

For 2017-2020 records, the GPA and attendance z-scores reuse the school-grade means and standard deviations already stored in `tracking_univ8gr.RData`. For 2014-2016 historical records, the script computes the same school-grade moments from the raw year file before filtering to the grade-8 universe, matching the upstream tracking construction.
