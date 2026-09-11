# Graduation records linked to school staff

## Request and sources

Bruno requested cleaning the newly extracted titulados files and establishing
what fraction of the staff under discussion can be matched to their titulacion.
The 19 RARs cover graduation reporting cohorts 2007-2025, with 20 CSV copies:
the 2018 archive also carries a byte-identical 2017 CSV. Use one canonical
file per year, verify the redundant copy by hash, and preserve every raw file.
The MINEDUC/SIES six-page codebook supplies annual counts and field definitions.
CAT_PERIODO is the reporting/academic cohort; actual award dates may fall in
the next calendar year. These sources include undergraduate, postgraduate and
postitulo awards, not merely original undergraduate teaching qualifications.

## Cleaning decisions

Use exact positive MRUN integer strings, consistently trimming leading zeros
on both sides; missing/zero/malformed IDs never match. No fuzzy matching.
Keep invalid-ID records in the cleaned universe but never in staff links.
Validate UTF-8 on complete strings. A preflight byte slice truncated in the
middle of a UTF-8 character is not proof of a Latin-1 source.
Retain all 40 source fields after CSV parsing plus normalized analytical fields;
surrounding CSV field whitespace/default NA tokens follow the declared parser.
Original MRUN is MRUN_RAW. Remove only exact duplicate parsed source rows per
cohort; retain distinct awards for one person and source year/row identifiers.
Award reports are not necessarily unique people or lifetime degree events.
Do not synthesize CODIGO_UNICO: it appears in the codebook but not the files.
Institution and program names are observed and preserved, not ranked or merged
by fuzzy names; COD_CARRERA alone is not a globally stable program identifier.

Parse birth month YYYYMM and calendar award date YYYYMMDD, preserving original
strings and missing/invalid flags. Sentinels 190001/19000101 do not denote a
real birth in 1900; award sentinel 19000101 is missing. Undergraduate,
postgraduate and postitulo flags are separate. Education-undergraduate means
AREA_CONOCIMIENTO=Educacion and NIVEL_GLOBAL=Pregrado, not a validated teaching
license or necessarily the original teaching degree.

## Denominators, dates and validation

Main denominator is unique 2024 people per role at the existing VA schools:
72,854 HS teachers, 64,604 non-HS comparison teachers, 1,986 orientadores and
7,692 leaders. The two teacher groups are disjoint; other roles may overlap.
Non-HS follows the previous age comparison: confirmed other-level classroom
assignment at a VA school, no regular youth-HS classroom assignment anywhere
nationally, and no unknown assignment slots. Adult education may qualify.
This is not a national all-teacher denominator. HS teachers, orientadores and
leaders additionally use their full 2018-2024 staff panel; non-HS is 2024 only.
School-year coverage counts each person once per school-role-year.
Unique period-member coverage counts each ever-member once per role and stops
the dated match at the last year actually observed in that role.

MATCH_ANY_2007_2025 is an exact-ID match anywhere in the supplied files, even
to qualifications after the staff observation. For substantive dated coverage,
require report cohort AND actual award year <= staff year (by year-end, not
the census day). A missing award date would use report year with an explicit
basis flag; no missing/invalid award dates were found. These revised historical
files support retrospective chronology, not real-time publication availability.
No 2025/2026 qualification enters 2024 attributes. Earliest means first observed
in these files, never first lifetime degree.

Keep age and reported-title-year coverage strata. The latter use a declared
title in either staff slot with a valid year >1900 and <=staff year; a reported
post-2007 title does not guarantee it was an undergraduate teaching title.
Check birth month and sex for exact-ID links, retain disagreement diagnostics,
and do not silently substitute probabilistic links or discard disagreements.

## Completed results

By 2024, any-award / undergraduate / Education-undergraduate coverage is:

| Role | Any award | Undergraduate | Education undergraduate |
|---|---:|---:|---:|
| HS teachers | 75.3% | 68.6% | 60.6% |
| Non-HS teachers | 74.5% | 65.8% | 64.4% |
| Orientadores | 69.5% | 36.4% | 32.1% |
| Leadership | 62.9% | 29.9% | 27.1% |

Counts matched to any award by 2024: 54,825 / 48,152 / 1,381 / 4,842,
respectively. Undergraduate counts: 49,981 / 42,489 / 722 / 2,302.
Using all supplied files, including later qualifications, any-award coverage is
76.0% / 75.1% / 70.3% / 63.9%. Among staff aged 30-39, undergraduate matches are
94.6% / 94.3% / 96.4% / 94.8%. The much lower overall undergraduate matching
for orientadores/leaders is consistent with older age distributions and the
2007 starting cohort; their later postgraduate/postitulo records still match.

At least one birth-month agreement among comparable matched people is
99.980% / 99.971% / 100.000% / 99.938%. Some people have both agreeing and
disagreeing records; retain those diagnostics. Do not claim every record agrees.
No-match is not no-degree: pre-2007/foreign qualifications and institutional
nonreporting remain limitations. The codebook flags Universidad Gabriela
Mistral 2009-2010 and CFT INFOMED 2009-2011 as nonreporting.

## Verification and outputs

4,021,401 source rows match the codebook; 426 exact parsed-row duplicates
removed; 4,020,975 retained records. 7,369 retained records lack valid MRUN.
All institution IDs/names and program names are present. Each cohort's award
dates fall in its reporting year or the following calendar year; later dates
must not be treated as already awarded in the reporting year.
All 19 complete cleaned RDS files pass schema/ID/date/duplicate/source-row
checks. Ten synthetic tests cover sentinel, duplicate, multiple-award and
future-date handling. Independent Python traces all 209,792 linked award
records to the raw files and verifies 4,301,045 person-role-year flags, 511
aggregate cells and 60,327 school-role-years, plus 2024 identity diagnostics.
Raw and prior staff input hashes are unchanged. No staff index is altered.

Code: code/codex/titulados_staff_linkage/. All cleaned and person-level linked
data: data/clean/titulados_staff_linkage/ (Git-ignored). The compact table is
staff_match_2024_overview.csv; detailed files retain year, age, title-cohort,
degree-level, institution and identity-validation information. Reader-facing
aggregate report: output/reports/titulados_staff_match_report.md. Individual
identifiers and degree records must not be committed or placed in the report.
