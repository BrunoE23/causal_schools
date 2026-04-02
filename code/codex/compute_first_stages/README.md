# Compute First Stages

## Task

Create a school-level first-stage dataframe using the universe of `sample_students` in `samples.RData`.

For this task:
- Use only `FIRST ROUND` offers.
- `br_code` is the course-level school identifier, effectively combining `rbd_cod_nivel` and `cod_curso_proceso`.
- In the stacked applications data, `rbd` is the applied school, while students can appear multiple times because they are stacked once per school or course they applied to.

## Outcome Definition

Attendance is defined as an indicator that the student is present in the applied school in the school-tracking data:
- Year 1 after SAE: `in_target_school_t_0`
- Year 4 after SAE: `in_target_school_t_3`

These variables come from `tracking_clean_wide.RData`, where relative time is defined so that:
- `t_0` is the first school year after the SAE process
- `t_3` is the fourth school year after the SAE process

## Inputs

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/samples.RData`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/treatment_1R.RData`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/tracking_clean_wide.RData`

## Construction Notes

- The application universe starts from `sample_students`.
- First-round offers are merged using `mrun` and `br_code`.
- Because `sample_students` contains duplicated student-process-school rows at the `rbd` level, the script collapses to one row per `mrun`-`proceso`-`rbd_target` after merging offers.
- Within a student-process-school combination, the script defines first-round offer status as `1` if the student received any first-round offer in any associated `br_code` for that school.

## Output

The script writes:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/first_stage_school_offer_attendance.csv`

with columns:

- `rbd`
- `n_apps`
- `n_offers_1r`
- `prop_attend_y1_given_offer`
- `prop_attend_y4_given_offer`

## Script

- `compute_school_offer_attendance_first_stage.R`
