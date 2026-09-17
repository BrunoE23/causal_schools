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

## Credential-indicator extension (computed 2026-09-16)

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
leadership for leadership. The first explicit rule-based program/title/degree
mapping is implemented in `credential_helpers.R` and exported for review.
Teacher rules include the SIES Education area and explicit teaching subjects.
Orientador rules require orientation/counseling/psychoeducation/family mediation
or advising, not generic psychology/psychopedagogy alone. Leadership requires
management/administration/direction/leadership plus education context in the same
field, not a generic MBA. These are reviewable first definitions, not a manually
validated taxonomy or a measure of causal staff quality.

Where a program lists several mentions but the recorded title/degree specifies
a different mention and neither award field supports that role, exclude the
program-menu inference. Program-only matches without a contradictory awarded
mention are retained with explicit review flags. `credential_subject_overrides.csv`
supports exact program/title/degree/area overrides for each role, with a required
reason. Blank role override cells preserve defaults; blank source fields are
missing values, not wildcards. Case/accents/punctuation are normalized. Duplicate
or unknown signatures fail; the baseline override file is empty.

High premium reuses the existing MiFuturo institution-plus-field model's centered
institution FE strictly greater than 0.1. Map by SIES institution code, the first
component of the existing unique model key. No refitting or fuzzy-name matching.
The fixed FE snapshot is not a historical or postgraduate-specific premium.
Absent FE estimates give observed zero as in `high_inst_m1`; separate uncovered-
institution flags preserve this limitation. University/IP/CFT status is not an
additional restriction on the existing high-premium definition.

Apply the existing as-of staff-year rule to each qualifying award. Retain degree
level and coverage flags. Zero means no qualifying award observed in the covered
records, not verified lifetime absence. The extension covers 614,435 person-role-
years and 186,990 distinct people. The 2024 snapshot has 147,136 role memberships
and 145,819 distinct people; overlaps across roles are retained. Non-HS remains
2024 only. Existing staff indices, raw data and prior linkage outputs are intact.

Run with R/data.table and the project's Python/pandas/numpy runtime:

```powershell
Rscript --vanilla code/codex/titulados_staff_linkage/test_credentials.R
Rscript --vanilla code/codex/titulados_staff_linkage/06_build_credential_indicators.R
python code/codex/titulados_staff_linkage/07_write_credential_review.py
```

The builder refuses populated credential outputs unless `--overwrite` is supplied.
After editing rules/overrides, rerun both the builder with `--overwrite` and the
Python review generator; the latter also independently verifies all eight saved
indicators. `--review-only` builds only the subject/institution review mappings.

Outputs under `data/clean/titulados_staff_linkage/credentials/`:

- `staff_credentials_person_role_year.csv.gz`: full panel and coverage/level flags.
- `staff_credentials_2024.csv.gz`: current snapshot, one row per MRUN-role.
- `staff_credentials_summary.csv`: role-year counts/shares, including zeros.
- `credential_award_evidence.csv.gz`: private source-award evidence and rules.
- `credential_subject_mapping.csv`: full historical program/title/degree mapping.
- `role_specific_review_2024.csv`: all included and excluded 2024 qualification
  signatures with role-specific counts, rules and priority-review flags.
- `credential_institution_mapping.csv`: exact code-to-FE mapping and coverage.
- `credential_dictionary.csv`, `credential_input_manifest.csv`,
  `credential_verification.csv`, `credential_independent_verification.json`.

The readable review document is
`output/reports/staff_credential_specialization_review.md`. It lists all included
2024 program names by role and all program-only/conflicting-mention priority
cases for orientadores and leadership. It explains consequential boundaries
and how to change them without editing raw data. Full historical signatures
(including future awards excluded from earlier indicators) remain in the CSV.

Verification: targeted synthetic tests; independent direct role-year set checks
of 10,445,395 R binary cells including coverage; Python reconstruction of the six
non-subject conditions from source fields and checks of all 4,915,480 core binary
cells; exact prior-coverage reconciliation; snapshot equality; input hash checks.
Subject aggregation is verified, but substantive taxonomy choices remain open
to review. Individual records remain Git-ignored.

## School matching coverage and VA (2026-09-17)

`08_school_match_coverage_va.R` constructs school-level coverage for HS teachers,
orientadores and leadership, using existing exact-ID flags and memberships.
Main `ANY_DB` is any match in the 2007-2025 reporting-cohort database; it is a
linkage diagnostic, not a time-t credential. `UG_DB` restricts to undergraduate
records. `ANY_ASOF` and `UG_ASOF` require awards reported/obtained by staff year.
Do not confuse these matching measures with the eight credential indicators.

Annual denominator: distinct role members at each school. Average annual shares
equally over active-role years, 2018-2024. Require all seven role counts known
for the main period share. Absent roles have undefined rates, not zero; schools
with partial count coverage retain an observed-years sensitivity but no main
rate. All 3,682 schools remain in the wide output for all three roles. Non-HS
comparison teachers are not silently combined with HS teachers.

The primary school file is
`data/clean/titulados_staff_linkage/school_coverage/school_staff_match_rates.csv`.
Long annual/period files retain matched counts, denominator counts, years of
support, partial-roster status, pooled person-year and unique-person alternative
rates. Main coverage means are 71.7% / 61.8% / 56.7% for teachers/orientadores/
leadership, over 3,003 / 2,111 / 3,550 schools respectively.

Use the existing 12 All-sample saved EB VA outcomes, equally weighted schools,
and outcome-specific complete-case samples. Export Pearson (with conventional
CI/p-values), Spearman (10 significant-digit tie convention), broad-VA-student-
weighted and unshrunk-VA correlations. BH is within role and coverage definition.
Sensitivity checks vary aggregation to pooled staff person-years or unique
people, and require three active staff years. No adjusted regressions or causal
interpretation; no re-estimation of VA or staff-quality scores.

```powershell
Rscript --vanilla code/codex/titulados_staff_linkage/08_school_match_coverage_va.R
python code/codex/titulados_staff_linkage/09_verify_school_coverage_report.py
```

The R build refuses existing owned outputs unless `--overwrite` is specified.
Python independently verifies 309,288 annual rows, 44,184 period rows, all
distribution summaries, 144 correlation rows and 108 sensitivity rows, plus
original VA equality, denominator reconciliation, BH adjustments and source
hashes. It writes `output/reports/school_staff_titulados_coverage_va.md`.
Two checked PNGs are in `output/figures/titulados_school_coverage/`. All new
analysis CSVs remain under `data/clean/titulados_staff_linkage/school_coverage/`.
