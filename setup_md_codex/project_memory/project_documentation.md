# Project Documentation

## `br_code`

`br_code` is a course-level school identifier, not just a school identifier. In practice, it behaves like the combination of school and specific track/course/process information, so multiple `br_code`s can sit inside the same `rbd`.

Operationally:
- `rbd` is the school the student applied to.
- `br_code` is the more granular school-course-process unit inside that school.
- In `sample_students`, students are stacked by application unit, so the same student can appear multiple times for the same `rbd` if that school has multiple relevant `br_code`s.

This matters for construction work. For the school-level first-stage exercise, first-round offers were merged at the `br_code` level and then collapsed back to one `mrun`-`proceso`-`rbd` row.

## Timing Convention in Tracking Databases

In the current tracking databases, relative time is defined as:

`year_rel_sae_change = AGNO - sae_proceso - 1`

So if a student applies in SAE process `2017`, they are assigned for the `2018` school year. Under this convention:

- `2018` is `t_0`
- `2019` is `t_1`
- `2020` is `t_2`
- `2021` is `t_3`

This means:

- "1 year after SAE" corresponds to `t_0`
- "4 years after SAE" corresponds to `t_3`

This convention may change later, but for now it is the convention used in the tracking databases.
