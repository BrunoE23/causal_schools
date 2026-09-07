# Teacher and orientador characteristics

## Current cleaner (2026-09-07)

The current entry point is `01_clean_docentes_educacion.R`, with reusable
functions in `staff_cleaning_helpers.R`.
It reads all 13 Box annual files, 2013-2025, and prepares staff-level inputs
for school measures covering **2018-2024** (VA grade-8 cohorts 2017-2020).
It does not run PCA, choose school-period weights, or label national staff as
HS staff merely because their RBD also provides secondary education.

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/01_clean_docentes_educacion.R' --preflight
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/01_clean_docentes_educacion.R' --sample-rows=2000 --dry-run
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/tests/test_staff_cleaning.R'
# Full run, after reviewing the preflight:
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/01_clean_docentes_educacion.R'
```

Defaults are `C:/Users/brunem/Box/causal_schools/data/raw/docentes_educacion`
for input and `PROJECT/data/clean/docentes_educacion` for output.
Two positional arguments override these directories.
The cleaner refuses raw-tree output paths and refuses existing output files
unless `--overwrite` is explicitly supplied.
Sampled runs require `--dry-run` and cannot write population files.
The full run reads selected columns from approximately 1.3 GB of CSVs; expanded
tables and joins require several GB of RAM.

Outputs comprise the compatible main-position `*_clean.csv.gz`, an all-appointment
`*_appointments.csv.gz`, separate person-year and person-school-year histories,
`docentes_educacion_va_2018_2024_staff_features.csv.gz`, and aggregate audits.
The VA feature file contains **appointment rows**, keyed by
`AGNO/SOURCE_FILE/SOURCE_ROW`, not final school-period aggregates.
All individual outputs stay in the Git-ignored clean-data area.

Key definitions:

- All main functions are retained. Primary and secondary functions are separate
  from the `PERSONAS=1` main-appointment designation.
  `*_PRIMARY_NO_SECONDARY` describes the current assignment only.
- **Teacher sample = assigned to regular educacion media (HS).** A classroom
  teacher in either primary or secondary function qualifies at an appointment
  if `COD_ENS_1` or `COD_ENS_2` is one of `310, 410, 510, 610, 710, 810, 910`.
  These cover youth H-C, T-P and artistic secondary education (annual codebooks,
  Annex V, pp. 19-20). Adult-only media is outside the regular VA cohort scope.
  Mixed basic/HS assignments qualify, including non-main appointments.
  An HS title, a school offering HS, or an HS job at another RBD is insufficient.
  `NIVEL1/2` only checks consistency; it does not override/fill teaching codes.
  Missing/unmapped codes remain unknown and are audited, not coded as non-HS.
- Use `VA_TEACHER_ELIGIBLE == 1` for the teacher measure and the
  `TEACHER_HS_*` histories for HS-specific experience since 2013.
  All-level `TEACHER_*` histories remain supplemental career information.
  `VA_ORIENTADOR_ELIGIBLE` and orientador histories retain their existing
  function-based definition: orientadores need not have a classroom assignment.
  A basic-only teacher who is also an orientador can enter the combined VA file
  for the orientador measure only. Match to the final HS RBD support separately.
- `*_ANY_PRIOR_*` counts role experience across all appointments, once per person-year.
  `*_MAIN_PRIOR_*` uses the primary function of the main appointment.
  `*_AT_SCHOOL_PRIOR_*` counts a role once per person-school-year.
  These are different observable definitions, not interchangeable quality scores.
- The existing orientador spell measures are
  `ORIENTADOR_ANY_CONSECUTIVE_YEARS_TO_DATE` (person) and
  `ORIENTADOR_AT_SCHOOL_CONSECUTIVE_YEARS_TO_DATE` (person-RBD).
  They include primary or secondary orientador functions and the current year.
  A move alone resets the school-specific spell but not the person spell.
  Gaps, unknown role years and years out of the role break the relevant spell.
- `ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED` counts years from 2013
  through the row's year with `ID_IFP=9` in at least one appointment, once per
  person-year. This is the explicit cumulative primary-function measure.
  `ORIENTADOR_PRIMARY_PRIOR_YEARS_OBSERVED` excludes the current year.
  `ORIENTADOR_MAIN_CUMULATIVE_YEARS_OBSERVED` additionally requires
  `PERSONAS=1`: primary function and main appointment are distinct.
  `ORIENTADOR_ANY_CUMULATIVE_YEARS_OBSERVED` includes secondary functions too.
  Cumulative counts survive role changes, gaps and school moves; they do not
  fill unobserved years. `*_KNOWN_YEARS_TO_DATE` records their observed support.
- `PRIOR` uses years strictly before the row's year. Consecutive spells and
  `EXCLUSIVE_HISTORY_TO_DATE` and `CUMULATIVE_YEARS_OBSERVED` include the current year.
  Exclusive history means every observed year in the relevant role scope,
  requires at least two observed years, and remains missing with unknown role years.
  Gaps remain flagged; no history is not zero lifetime experience.
  Role-history construction starts in 2013, explicitly recorded in
  `HISTORY_WINDOW_START_YEAR`; prior experience in year t counts observed role
  years from 2013 through t-1, not reported system or school service.
- **Preferred years-at-school input:** `YEARS_AT_SCHOOL`, taken directly from
  the current row's `ANO_SERVICIO_EE` (also retained as
  `REPORTED_SERVICE_SCHOOL_YEARS`). These are aliases; do not put both in PCA.
  Reported tenure may exceed the observed panel length and is retained even on
  a person's first observed record at that school. Missing/negative reports stay
  missing; they are not filled using panel appearances.
  `SCHOOL_PRIOR_RECORD_YEARS_SINCE_2013` is a record-coverage diagnostic, not tenure.
  `*_AT_SCHOOL_PRIOR_*` remains a supplemental role-at-school history, not the
  baseline years-at-school measure. Reported system service is separate as well.
- Current credentials are never filled backward from later observations.
  `TIT_ID=0` is unknown, `TIT_ID=3` is explicitly not titled.
  Reported-title indicators summarize available slots, not all unreported qualifications.
  Noneducation title codes include nontertiary qualifications, so title status,
  tertiary qualification, and university training are kept distinct.
- Mentions are treated as applicable to basic-education titles
  (`TIT_ID=1`, `TIP_TIT_ID=13`), following the codebook's basic-title restriction.
  Positive flags outside this group are retained and audited, not discarded.
  Degree-specialty codes separately identify math and language specialties.
  `N_MENCIONES_TOTAL` preserves the old raw positive-flag count; it is not a
  complete specialization measure. Use the applicability/coverage fields.
- Institution categories are not ranks or actual institution identities.
  Qualification durations and hours retain fractional reported values.
  The pre-2015 teaching-hours unit and the 2015 director/encargado coding break
  are flagged; raw codes and recorded transitions remain available for review.
- Full-window `EVER_*` flags remain only in the compatible main-position output.
  They are excluded from the past-only histories and VA features.

School-period aggregation of experience/credentials, orientador coverage of HS
students, final quality-score support, PCA input selection and weights remain
subsequent steps. The staffing-ratio build below uses the broad VA RBD support.
The final intended unit is school over the VA window,
not school-year; the annual objects above are intermediate construction inputs.

## VA-sample students per orientador (implemented 2026-09-07)

`05_build_va_students_per_orientador.R` is a standalone, thin-column count build.
It does not require a full history-cleaner run and does not estimate quality/PCA.
The final output has one row per VA school; annual counts are an audit input.

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/05_build_va_students_per_orientador.R' --preflight
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/tests/test_orientador_ratios.R'
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' --vanilla 'code/codex/docentes_educacion/05_build_va_students_per_orientador.R'
```

The input/output directory defaults match the main cleaner. Optional positional
arguments override those directories; `--dry-run` suppresses exports and
`--overwrite` permits replacing only this build's four named output files.
Raw-tree output paths are rejected. The seven annual staff inputs are read with
only `AGNO`, `RBD`, `MRUN`, `ID_IFP`, and `ID_IFS`, then collapsed immediately.

The numerator is `n_students` from the saved broad All-sample exam-TAKING VA
input, `output/tables/empirical_bayes_school_va/stata_va_eb_input_exam.csv`:
757,999 students, grade-8 cohorts 2017-2020, across 3,682 `most_time_RBD` schools.
This is NOT a restriction to exam takers. School counts agree with the saved
STEM and full program-income VA inputs. The score-observed math sample is not
used, and the student count is not divided by four cohorts or seven staff years.

For each school and year, count distinct valid MRUNs across ALL appointments
at that RBD. There is no `PERSONAS=1` restriction and no teacher-assignment filter:

- PRIMARY: `ID_IFP=9` on at least one appointment at that school-year.
- ANY: `ID_IFP=9` or `ID_IFS=9` on at least one appointment there.

`MEAN_ORIENTADORES_PRIMARY/ANY` is the unweighted arithmetic mean of the
**seven annual headcounts, 2018-2024**. It includes confirmed zero years and
does not count all distinct people across the period as simultaneous staff.
`VA_STUDENTS_PER_AVG_ORIENTADOR_PRIMARY/ANY = N_VA_STUDENTS / MEAN_ORIENTADORES_PRIMARY/ANY`.
Both period-average denominator and numerator were explicitly agreed by Bruno.

Missing staff rosters are not zero headcounts. Missing/unmapped function codes
are unknown unless a positive function at another appointment for that same
person-school-year resolves their role. Unidentified potential orientador rows
also make the corresponding headcount unknown. All seven counts must be known
for the full-period mean and ratio. An observed-years-only mean is kept under
`MEAN_ORIENTADORES_OBSERVED_YEARS_*` solely as a coverage diagnostic, not a fallback.
`N_YEARS_KNOWN_*`, `N_YEARS_ZERO_*`, and `N_YEARS_ROSTER_OBSERVED` expose support.
The ratio is NA with status `incomplete_staff_coverage` when the denominator is
unknown, or `zero_orientadores_full_period` when the full-period mean is zero.
`NO_ORIENTADOR_FULL_PERIOD_*` separately identifies a confirmed absence.

Outputs in `data/clean/docentes_educacion/`:

- `va_students_per_orientador_2018_2024.csv`: final school-period ratios and coverage.
- `va_schools_orientador_headcounts_2018_2024.csv`: school-year counts and role/ID audits.
- `va_students_per_orientador_2018_2024_diagnostics.csv`: coverage and unweighted school-ratio quantiles.
- `va_students_per_orientador_2018_2024_input_manifest.csv`: source paths, sizes, dates, row counts, and VA count-source hash.

These are **VA-sample staffing ratios**, not full HS enrollment, annual caseload,
hours/FTE-adjusted counselor capacity, or student-specific staff exposures.
An orientador at a mixed-level school may also serve students outside HS.
The script neither reallocates that person's time nor assumes VA students
attended `most_time_RBD` in every year.

The first full run found 1,890 schools with a primary-function ratio available
(median 284.83) and 2,111 with an any-function ratio available (median 251).
Full-period zero-orientador counts are 1,698 and 1,451 respectively; incomplete
staff coverage affects 94 and 120 schools. Medians exclude zero/unknown denominators.

## Earlier planning notes (historical)

The notes below preserve the earlier plan.
Their 2018-2025 staffing window, Dropbox default, school-year final-output
proposal and unresolved teacher-assignment filter are superseded by the current
cleaner, HS teaching restriction and 2018-2024 VA-window agreement.

Status: proposed implementation plan, discussed on 2026-09-06.
The cohort flags and career reports below exist. School-year characteristics
and student exposure links have not yet been constructed.

## Objective and agreed scope

Construct school-level staff characteristics first, emphasizing two blocks:

1. Observed career trajectories, including persistence in a function and school.
2. Academic credentials and subject/orientation specializations.

The intended final outputs are two separate scores for each school-year:

- A teacher score combining teacher trajectories and academic credentials.
- An orientador score combining orientador trajectories and academic credentials.

Retain the underlying components alongside both scores. Their transformations,
direction, weights, and normalization remain to be chosen after coverage and
validation; no equal weighting, credential hierarchy, or outcome-based weighting
has been approved. These are school-level staff-characteristic scores, not
estimated individual causal contributions.

## Existing inputs and conventions

- Raw directory: `C:/Users/brunem/Dropbox/causal_schools/data/raw/docentes_educacion`.
- Eight annual files cover 2018-2025. Raw files must remain unchanged.
- Current cleaned file:
  `C:/Users/brunem/Research/causal_schools/output/data/docentes_educacion/docentes_educacion_2018_2025_clean.csv.gz`.
- The current cleaner keeps `PERSONAS == 1` across every `ID_IFP` function:
  one main position per person-year, not all simultaneous appointments.
- `EVER_TEACHER` identifies anyone observed with `ID_IFP == 1` at least once;
  `EVER_ORIENTADOR` identifies anyone observed with `ID_IFP == 9` at least once.
  Both flags follow the person across all their observed years and may overlap.
- Current reports cover the national directory, not an HS-only staff universe.
- The broad student analysis file is
  `C:/Users/brunem/Dropbox/causal_schools/data/clean/univ_gr8_df.csv`.
  Its header includes `MRUN`, `mrun`, `cohort_gr8`, `RBD_rel1`, `RBD_rel4`,
  `most_time_RBD`, and tracking-coverage fields. Project documentation reports
  grade-8 cohorts 2017-2021, whose usual four post-grade-8 years span 2018-2025.
  Verify actual cohort/year coverage before building links.
- `tracking_univ8gr.RData` and the school-tracking scripts are candidate sources
  for actual student RBD-year attendance. Endpoint and modal-school fields alone
  do not describe exposure in every high-school year.
- Rename staff and student identifiers explicitly within linking code.
  Link the two populations through school-year, not through their MRUN values.

## 1. Audit the HS population and staffing coverage

Build a small coverage report before processing the full student file:

- Read only school/cohort identifiers and the required tracking columns.
- Establish the HS RBD-year universe and verified education-level/grade codes.
  An RBD can offer both primary and secondary education; staff at that RBD are
  not automatically HS staff. Likewise, a post-grade-8 calendar observation
  need not imply that the student has advanced to HS.
- Check availability and meaning of staff `NIVEL1/2`, `COD_ENS_1/2`,
  `SECTOR1/2`, `SUBSECTOR1/2`, grade indicators, and hours fields in each year.
  Subject credentials alone do not prove which subject someone currently teaches.
- Keep the existing main-position person panel for career histories. Compare
  HS staffing coverage under `PERSONAS == 1` with all reported appointments;
  quantify secondary-school appointments omitted by the main-position rule.
  Decide the school-roster definition explicitly after this audit, without
  silently changing the existing person-level cohort definition.
- Distinguish a missing school-year roster from a covered roster with no person
  in a particular main function. Absence of a main-position orientador is not
  proof that the school has no counseling services.

Deliverable: coverage by year and HS type, with the proposed roster definition,
match rates, missingness, and counts affected by alternative position rules.

## 2. Construct staff-year trajectory variables

Retain current `ID_IFP`, both ever-role flags, and complete observed histories.

Candidate measures:

- Always versus sometimes observed in the role, with the number of observed
  years and a separate single-observation category.
- Prior observed years in the current role and share of prior observed years
  spent in that role.
- Consecutive observed tenure in the current role and RBD; changes in role,
  RBD, or both; gaps in observation.
- Entry into an orientador role from classroom teaching or another function;
  retention, departures, and replacements among each school's current staff.

Produce separate timing versions:

- Retrospective career descriptions use the full 2018-2025 window, as in the
  existing reports. A non-target role can occur before or after the target role;
  it is not automatically an exit or promotion from that role.
- School-year explanatory variables use information available by that year.
  Prior-history variables use years through t-1; current role and credentials
  use the year-t record. Full-window persistence flags remain explicitly
  retrospective, rather than being treated as predetermined characteristics.

Because the panel begins in 2018, observed tenure is a lower bound on career
experience. No prior observations means unknown history, not zero experience.
Gaps interrupt an observed consecutive spell but do not establish an exit.

## 3. Construct credential variables

Audit the annual dictionaries, then preserve and harmonize:

- Existing `TIP_INSTI_ID_1/2`, `ANO_TITULACION_1/2`,
  `DURACION_CARRERA_1/2`, and `MODALIDAD_ESTUDIO_1/2`.
- Degree, title-type, and specialty fields `TIT_ID_1/2`, `TIP_TIT_ID_1/2`,
  and `ESP_ID_1/2`, which are present in the inspected raw header but dropped
  by the current cleaner.
- The six requested math, language, and orientation mention flags, plus the
  total mention count. Audit applicability of a mention to each title type.

Create indicators for any relevant credential across the two titles, so a
person is counted once in a school share. Preserve title-level details; do not
silently sum or rank qualifications. Distinguish missing, no degree, and
not-applicable codes. Distinguish years since graduation from actual experience.

Institution type is not institution identity or institutional selectivity.
Do not infer a training-university quality measure from `TIP_INSTI_ID`.
Career-stage/biennium variables can be optional later checks for 2024-2025;
they do not provide a consistent baseline block for all eight years.

## 4. Aggregate to school-year measures by current function

Produce one row per RBD-year, with separate teacher and orientador blocks.
Use the current role to determine inclusion in each block. Someone who was an
orientador in another year contributes to the current teacher block when their
current role is classroom teacher; retain their history as a characteristic.

| Block | Candidate school-year measures |
|---|---|
| Trajectories | Mean prior years in role; role-persistence shares; mean observed RBD tenure; retention/replacement rates; role-change shares |
| Credentials | Shares with relevant degrees/specializations; university-trained share; any math/language/orientation credential; degree duration and study-modality distributions |
| Coverage/context | Staff counts; current orientador presence; valid-history and valid-credential denominators; roster coverage and HS-attribution flags |

Start with headcount-weighted means/shares and explicit valid-data denominators.
Only use hours weights if their interpretation and coverage pass the audit.
Count variables can be zero in a confirmed covered roster; qualifications and
mean tenure for an absent role are undefined, not zero. Keep missingness visible.
Do not label a particular trajectory or credential category better quality by
construction.

## 5. Attach school-year measures to the broad student panel

- Use the broad population, without introducing lottery participation or
  admission-exam-taking restrictions when creating staffing characteristics.
- Preserve one student-year observation under the established tracking rules,
  then join school-year characteristics on the actual RBD and calendar year.
- Keep first-HS-year and final-HS-year exposures separately. Construct a
  documented equal-observed-year exposure average, recording observed versus
  expected years and partial coverage; retain the annual links for alternatives.
- Do not assign later staff records to earlier cohorts or assume students stayed
  in `most_time_RBD` throughout HS.
- Flag unmatched observations rather than dropping students from the broad panel.

Deliverables: unique school-year characteristics; a student-year exposure link;
and a student-level exposure file joinable to `univ_gr8_df` without changing its
row count. These reusable objects belong in the project's clean-data area.
Leave the existing broad student database intact during this first construction.

## 6. Validate and assess what can reasonably be called quality

Check joins, counts, plausible support, structural missingness, denominators,
subject/level attribution, and time consistency. Report distributions and
within-school versus between-school variation for both characteristic blocks.
Confirm how many HS schools and cohorts have observable orientadores.

Construct the two scores after documenting the retained components,
transformations, weights, missing-component rule, reference population, and
normalization. Keep scales comparable across years and report score coverage.
A school without an observed orientador has a separate presence indicator and
an undefined orientador-quality score, rather than an automatic low-quality score.

Examine associations of the two scores and their components with the paper's
student outcomes and school VA. Use explicit outcome samples and controls;
qualifications or stable employment are not causal quality measures on their own.
If weights are learned from outcomes, define the target and use held-out cohorts
for validation before using those scores downstream. The weighting approach
remains an open design choice.

The immediate next implementation is steps 1-3 and a compact variable dictionary.
Their coverage report determines the final school-roster and aggregation rules.

## Existing scripts

- `01_clean_docentes_educacion.R`: all-function main-position cleaner and ever-role flags.
- `02_report_teacher_school_mobility.R`: movement conditional on retaining a selected role.
- `03_report_main_function_transitions.R`: all-function transitions and ever-orientador careers.
- `04_report_ever_function_cohort.R`: cohort histories and transitions for a selected ever-role.

These scripts currently write several derived objects under `output/`. Future
reusable school-year and exposure objects should follow the clean-data convention;
existing files should not be relocated silently as part of implementing this plan.
