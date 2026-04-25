# Enzo’s Assignment

**I need to construct an average of PSU points and school funding on years 2016-2018.**

## PSU per school per year 2016-2018

1. Simple approach:
   Take all the people who signed up for the PSU. (Dropbox/causal_schools/data/raw/2016/PSU2016/
   Rinden_Admisión2016/ArchivoC_Adm2016.csv)
   Create a (long) datafile of the structure.

| rbd (school_code) | year | avg_psu | median_psu |
| --- | --- | --- | --- |
| 1 | 2016 | 500.50 |  |
| 1 | 2017 |  |  |
| 1 | 2018 |  |  |
| 5 | 2016 | etc | etc |

Outcomes to be tracked: <columns> average, median(p50), p10, p25, p75, p90.

Just take all students with valid score (higher than 0).

2. A bit more involved process.

Some people sign up for PSU and dont take it; or never sign up for it. We want to capture that also in the metric per school.

→ Start from universe of people enrolled for a given school on last year of schooling.

(Dropbox/causal_schools/data/raw/student_tracking/2016/Rendimiento-2016/… . csv

Recompute everything above, but just give 0s to those who dont show up on the PSU file.

Create a (long) datafile of the structure.

| rbd (school_code) | year | avg_psu_wz | median_psu_wz |
| --- | --- | --- | --- |
| 1 | 2016 | 490 | etc |
| 1 | 2017 |  |  |
| 1 | 2018 |  |  |
| 5 | 2016 | etc | etc |

Outcomes to be tracked: <columns> average, median(p50), p10, p25, p75, p90.

## School funding per year 2016-2018

use the Dropbox\causal_schools\data\raw\school_finances\2016\ … path.

Try to construct a public_funds from the Subvenciones-a-EE2016 file

The same for private funds from the

Create a (long) datafile of the structure.

| rbd (school_code) | year | public_funds | private_funds |
| --- | --- | --- | --- |
| 1 | 2016 |  |  |
| 1 | 2017 |  |  |
| 1 | 2018 |  |  |
| 5 | 2016 |  | etc |
