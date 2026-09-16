# Graduation records linked to school staff

Purpose: clean all SIES graduation files for 2007-2025 and establish what
fraction of staff in the current school-VA analysis can be linked to observed
qualifications, including institution and program names. This is linkage and
coverage analysis, not a new staff-quality index or causal estimate.

## Inputs and scope

- Raw: `C:/Users/brunem/Box/causal_schools/data/raw/titulados/`.
- MINEDUC/SIES `ER titulados Ed.Superior 2007 - 2025, WEB.pdf`, six pages:
  row counts p.1; MRUN and birth month p.2; award date, institution and levels
  p.3; program fields p.4; nonreporting institutions p.5; level labels p.6.
- Staff: existing `staff_quality_va` and `leadership_quality_va` person-school-
  year caches and school universe. Main denominator: unique people per role in
  2024 at VA schools, exactly matching the prior age comparisons. HS teachers,
  orientadores and leadership also have the 2018-2024 panel. Non-HS teachers are
  the separately defined 2024 comparison group, not a national teacher sample.
- Additional raw staff fields: MRUN, birth month, sex and reported title years,
  read one annual file at a time for 2018-2024. For non-HS eligibility, check
  nationwide classroom appointments in 2024; no regular youth-HS assignment
  anywhere, known code slots, actual other-level assignment at a VA school.
  Adult education can enter this group. Groups other than the two teacher
  groups can overlap; never sum role denominators as distinct people.

## Cleaning and identity rules

There are 20 CSV copies but 19 graduation cohorts. The 2018 folder contains a
byte-identical extra copy of 2017. Verify its hash, exclude that copy, and
preserve both raw files. Canonical files match the year in their parent folder.
Read UTF-8 and validate complete strings; truncating a byte sample mid-character
must not trigger an erroneous Latin-1 interpretation. Source counts must agree
with the codebook. Work one cohort at a time, preserving all 40 original fields
alongside normalized analytical fields. The CSV parser trims surrounding field
whitespace and recognizes its default NA token; it does not otherwise rewrite
institution/program names. Original MRUN is retained as MRUN_RAW.
Remove only exact all-source-field duplicate rows after parsing within a cohort. Retain
different qualifications for one person. Records are award reports, not unique
people, and not necessarily unique lifetime graduation events across cohorts.
Record IDs are source year plus original row, with source path/hash manifests.

Normalize identifiers as positive integer strings (trim spaces/leading zeros).
Blank, zero, noninteger or malformed identifiers never match. No name, birth-
date-only or fuzzy matching. Link directly on MRUN; validate using birth month
and sex without silently replacing keys or dropping conflicting matches.
Keep conflicting staff birth/sex reports missing and expose counts.

Parse birth YYYYMM (also accept YYYYMMDD by retaining its month) and valid
calendar award dates YYYYMMDD. Treat 190001/19000101 as no birth information;
19000101 is the missing award-date sentinel. Original strings remain available.
Keep undergraduate, postgraduate and postitulo flags separate. Education means
the broad AREA_CONOCIMIENTO classification, not an inferred teaching license.
Institution/program names are preserved as reported, not ranked or fuzzy-merged.
Codebook lists CODIGO_UNICO but actual files do not contain it; do not invent a
globally stable program key from COD_CARRERA alone.

## Temporal and denominator rules

`MATCH_ANY_2007_2025` means any exact-ID award record in any supplied cohort,
even a qualification later than the staff observation. It is coverage only.
The preferred dated match requires both report cohort and valid award year to
be no later than staff year. When the award date is missing, use the report
year and expose ASOF_BASIS=report_year_only. This is a conservative retrospective
qualification chronology, not a claim that the current revised files were
published/available in real time. Do not attach 2025 qualifications to 2024
staff attributes. Earliest years mean first observed in these data, not lifetime.

Show any award, undergraduate, Education-undergraduate, postgraduate, postitulo
and named-institution coverage separately. An undergraduate match need not be
the person's original teaching degree. A postgraduate/diploma match cannot
stand in for that original degree. Staff reported title-year cohorts are based
on valid 1901-through-staff-year entries with a declared title in either slot.
The `All reported title years before 2007` group refers only to reported slots.

Main rates count people once per role-year, including unmatched people in the
denominator. Period unique-person rates count each ever-member once per role;
dated coverage stops at that person's last observed staff year in the role.
School-year output counts people once per RBD-role-year. No match means no
observed record here, not no qualification. Pre-2007 awards, foreign degrees and
nonreporting institutions can be absent. Codebook nonreporting: U. Gabriela
Mistral 2009-2010; CFT INFOMED 2009-2011 (the 2011 university cell says Si).

## Run and outputs

From the repository root, with R/data.table and Python/pandas/numpy:

```powershell
Rscript --vanilla code/codex/titulados_staff_linkage/01_preflight.R
Rscript --vanilla code/codex/titulados_staff_linkage/test_linkage.R
Rscript --vanilla code/codex/titulados_staff_linkage/02_clean_and_link.R
Rscript --vanilla code/codex/titulados_staff_linkage/03_summarize_matches.R
Rscript --vanilla code/codex/titulados_staff_linkage/04_audit_clean_files.R
python code/codex/titulados_staff_linkage/verify_linkage.py
python code/codex/titulados_staff_linkage/05_write_match_report.py
```

The full build refuses populated outputs unless `--overwrite` is explicit.
No raw data or prior staff datasets are changed. Outputs stay in Git-ignored
`data/clean/titulados_staff_linkage/`:

- `annual/titulados_YEAR.rds`: all cleaned records, including unmatched people.
- `staff_linked_awards.rds`: all fields for linked award reports; `.csv.gz`
  counterpart retains the analytical identity/date/institution/program fields.
- `staff_person_role_year.csv.gz` and membership file: original denominators.
- `staff_match_person_role_year.csv.gz`: dated person-role matching flags.
- `staff_match_2024_summary.csv`: main current-staff coverage table.
- `staff_match_2024_by_age.csv` and `...by_reported_title_year.csv`: coverage
  by observed age and reported qualification-year window.
- Annual, unique-period-person and school-year match coverage; degree levels,
  institution counts, identity-validation tables and source/cleaning audits.
- `independent_verification.json`: independent source-to-link, chronological
  flag, aggregate, school support and source-preservation checks.

Institution counts allow multiple institutions per person; do not sum them as
the number of matched people. Person identifiers and individual qualifications
must remain outside Git. This task does not modify the previous staff scores.

The reader-facing report is `output/reports/titulados_staff_match_report.md`.
The compact `staff_match_2024_overview.csv` has explicit count and percentage
columns. No individual records appear in the report or committed outputs.

## Agreed credential-indicator extension (2026-09-16; not yet computed)

The requested person-role-year indicators are:

1. `UG_HIGH_PREMIUM`: any observed undergraduate qualification at a
   high-premium institution.
2. `ANY_POST_UG`: any observed non-undergraduate qualification, including
   diplomados, postitulos, magisters and doctorates.
3. `POST_UG_HIGH_PREMIUM`: any such non-undergraduate qualification at a
   high-premium institution.
4. `ANY_HIGH_PREMIUM`: any observed qualification at a high-premium institution.
5. `ROLE_SPECIFIC_QUALIFICATION`: any observed qualification relevant to the
   person's current role.
6. `ANY_MAGISTER`: any observed qualification classified as Magister in
   NIVEL_CARRERA_1; a doctorate alone does not qualify.
7. `MAGISTER_HIGH_PREMIUM`: an observed Magister awarded by a high-premium
   institution. Separate degrees cannot jointly satisfy this condition.
8. `ROLE_SPECIFIC_MAGISTER`: an observed Magister whose own subject matches
   the person's current role; a general Magister plus a relevant non-master's
   qualification does not qualify.

Agreed subject domains: education, curriculum, teaching methods and assessment
for teachers; orientation, vocational counseling, psychoeducation and family
counseling for orientadores; management, educational administration and school
leadership for leadership. The exact program-level mapping still needs to be
constructed and reviewed, not inferred from an unrestricted keyword match.
Reuse the project's existing high-premium institution definition after checking
its institution-code crosswalk; do not invent a new ranking.

Apply the existing as-of staff-year rule to each qualifying award. Retain degree
level and coverage flags. Zero means no qualifying award observed in the covered
records, not verified lifetime absence. These are agreed definitions only; the
current output files do not yet contain these eight indicators.
