# Plan: teacher and orientador characteristics in the broad HS panel

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
