# Compute First Stages

## Task

Create a school-level first-stage dataframe using the universe of `sae_apps_grade9` in `sae_grade9_unique_proceso.RData`.

For this task:
- Use only `FIRST ROUND` offers.
- `br_code` is the course-level school identifier, effectively combining `rbd_cod_nivel` and `cod_curso_proceso`.
- In the stacked applications data, `rbd` is the applied school, while students can appear multiple times because they are stacked once per school or course they applied to.

## Outcome Definition

Attendance is defined as an indicator that the student is present in the applied school in the school summary file:
- Year 1 after SAE: `in_target_school_t_0`
- Year 4 after SAE: `in_target_school_t_3`

These variables are constructed from `rbd_universe.csv` as:
- `in_target_school_t_0 = 1(RBD_rel1 == rbd_target)`
- `in_target_school_t_3 = 1(RBD_rel4 == rbd_target)`

## Inputs

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/sae_grade9_unique_proceso.RData`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/treatment_1R.RData`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/rbd_universe.csv`

## Construction Notes

- The application universe starts from `sae_apps_grade9`.
- First-round offers are merged using `mrun` and `br_code`.
- Because `sae_apps_grade9` can contain multiple `br_code` rows within the same school, the script collapses to one row per `mrun`-`sae_proceso`-`rbd_target` after merging offers.
- Within a student-process-school combination, the script defines first-round offer status as `1` if the student received a first-round offer to that target school in any associated `br_code`.

## Output

The script writes:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/first_stage_school_offer_attendance.csv`

with columns:

- `rbd`
- `n_apps`
- `n_offers_target_school_1r`
- `prop_attend_y1_given_offer`
- `prop_attend_y4_given_offer`
- `prop_attend_y1_given_no_offer`
- `prop_attend_y4_given_no_offer`
- `first_stage_y1`
- `first_stage_y4`

## Script

- `compute_school_offer_attendance_first_stage.R`
