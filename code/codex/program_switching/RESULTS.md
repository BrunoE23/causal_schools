# Program Switching First-Pass Results

Run date: 2026-05-06

Input years:

- 2022
- 2023
- 2024
- 2025

Sample:

- Higher-ed matricula rows for `Carreras Profesionales` and `Carreras Tecnicas/Técnicas`
- Main unit: student (`MRUN`)
- Switch definition: a student is a program switcher if observed with more than one distinct program identifier across 2022-2025

## Headline Counts

- Students enrolled: 2,228,245
- Students observed in multiple years: 1,509,777
- Students ever observed in more than one program: 368,118
- Share of all enrolled students ever observed switching program: 16.5%
- Share of multi-year students ever observed switching program: 24.4%

## Switch Dimensions

Among all enrolled students:

- Any program switch: 368,118 students, 16.5%
- Any institution switch: 201,424 students, 9.0%
- Any `AREA_CARRERA_GENERICA` switch: 254,896 students, 11.4%
- Any broad reclassified field switch: 98,819 students, 4.4%

Among program switchers:

- Same institution only: 166,694 students, 45.3%
- Cross institution: 201,424 students, 54.7%
- Same `AREA_CARRERA_GENERICA` only: 113,243 students, 30.8%
- Cross `AREA_CARRERA_GENERICA`: 254,875 students, 69.2%
- Same broad reclassified field only: 269,299 students, 73.2%
- Cross broad reclassified field: 98,819 students, 26.8%
- Ever observed in a pipeline-like program: 28,406 students, 7.7%

## Interpretation Notes

The `AREA_CARRERA_GENERICA` results suggest that most observed program switching
is across generic career areas, but broad field switching is much less common.
Among program switchers, 69.2% cross `AREA_CARRERA_GENERICA`, while only 26.8%
cross the broader reclassified field. This is a potentially interesting fact:
mobility across program codes does not necessarily imply mobility across broad
fields, and `AREA_CARRERA_GENERICA` sits between those two levels of aggregation.

There is a small difference between students with any generic-major switch
(254,896) and program switchers with cross-generic-major switching (254,875).
That means 21 students are observed with multiple `AREA_CARRERA_GENERICA` labels
without being counted as program switchers, likely because the same program
identifier appears with different generic-major labels across years or rows.

The pipeline-like-program flag is heuristic and should be treated only as a
screen. It catches plausible gateway programs such as bachillerato, college, and
plan comun, but it also catches some false positives such as program names with
"primer ciclo basico" in education.

## Output Files

The current output CSVs live in:

- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/program_switching/program_switching_student_summary_2022_2025.csv`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/program_switching/program_switching_overall_summary_2022_2025.csv`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/program_switching/program_switching_dimension_summary_2022_2025.csv`
- `C:/Users/xd-br/Dropbox/causal_schools/data/clean/program_switching/potential_pipeline_programs_2022_2025.csv`
