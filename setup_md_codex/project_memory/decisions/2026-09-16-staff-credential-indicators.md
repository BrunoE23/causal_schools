# Eight observed staff credential indicators

## Request and unit

On 2026-09-16 Bruno requested the eight agreed credential binaries for every
individual in the existing staff sample and a reviewable document specifying
role-specific matches. Unit is person-role-year, not school-year, because the
same person can hold overlapping roles with different relevance definitions.
Keep all 614,435 original person-role-years and 186,990 distinct people. The
2024 snapshot retains 147,136 role memberships and 145,819 people. HS teachers,
orientadores and leaders cover 2018-2024; non-HS comparison teachers cover 2024.

## Definitions and time

The eight binaries are undergraduate at a high-premium institution, any
non-undergraduate award, non-undergraduate at a high-premium institution, any
award at a high-premium institution, role-specific qualification, any Magister,
Magister at a high-premium institution and role-specific Magister. Non-UG
includes the source postgraduate and postitulo categories (including diplomas,
strict postitulos, masters, doctorates and the rare medical/dental specialties).
Magister requires the source NIVEL_CARRERA_1 label, not merely duration or a
program-name guess; doctorate alone does not imply an observed Magister.
Both institution and subject conditions must apply to the same master's award.

Require ASOF_YEAR <= staff AGNO: both report cohort and valid actual award year
must be no later than that year. Missing award dates use the prior linkage's
explicit report-year fallback. Earliest qualifying award is computed separately
for each criterion. Unmatched people are retained with observed zeros and
coverage flags, not declared unqualified. Never backdate a later credential.

## High-premium institution

Reuse output/tables/mifuturo_matricula_income/
mifuturo_income_fe_institution_effects.csv from the existing institution-plus-
generic-field model. Strict centered effect >0.1, as in high_inst_m1. Model
institution codes are unique and link to the identical SIES COD_INST in awards.
No new institutional ranking, model estimation, university-only restriction,
or fuzzy name match. This fixed model snapshot is descriptive, not a ranking
observed at the staff member's graduation date and not a postgraduate-specific
institution effect. An absent FE yields observed zero as in the student measure,
with any/undergraduate/non-UG/Magister uncovered-institution flags retained.

## Reviewable subject rules

- Teachers: SIES Education area or explicit education/teaching, curriculum,
  didactics, psychopedagogy or neuroeducation subjects. Original teaching degrees
  and generic education masters count; this is not necessarily extra training.
  Early-childhood, special education, higher education and educational management
  remain included in this first broad rule. No individual teaching-subject match.
- Orientadores: explicit orientation (educational, vocational, professional,
  family/personal-relations), corresponding counseling, psychoeducation, family
  mediation/advising. Generic psychology, psychopedagogy, family law/science,
  education or management alone are excluded. Psychoeducational inclusion counts.
- Leadership: management, administration, direction, leadership/directiva or
  gerencia AND education/school/teaching context in the same text field. Generic
  MBAs/business management alone are excluded. Curriculum and convivencia
  management qualify in this first specification.

Search program/title/degree fields, not institution names. If a program lists
several potential mentions but the specific awarded title/degree names a
different mention and neither awarded field supports the role, exclude that
program-menu inference. With generic/missing award labels, program evidence
can qualify but has an explicit PROGRAM_ONLY review flag. MENTION_CONFLICT
flags expose exclusions. These rules do not certify that every optional program
mention was completed; the substantive mapping remains reviewable.

credential_subject_overrides.csv supports per-role 0/1 overrides on the exact
normalized program/title/degree/area signature, with a required reason. Empty
override cells preserve defaults. Blank source fields are literal missing values,
not wildcards. Duplicate/unknown keys stop the build. Baseline overrides are empty.

## Deliverables and checks

Code: code/codex/titulados_staff_linkage/06_build_credential_indicators.R,
credential_helpers.R, test_credentials.R and 07_write_credential_review.py.
Private outputs: data/clean/titulados_staff_linkage/credentials/. The Markdown
review document is output/reports/staff_credential_specialization_review.md;
full included/excluded current signatures and historical mappings are CSVs.
Existing linkage/staff indices and raw data are not overwritten.

R checks 10,445,395 binary cells (core plus auxiliary flags) via independent
dated sets, reconciles original coverage and verifies unchanged source hashes.
Python independently reconstructs six non-subject award conditions, checks all
4,915,480 core binary cells including subject aggregation and verifies snapshot
equality. Synthetic tests exercise subject boundaries, mention conflicts,
strict cutoff, no-FE cases, future awards, overlapping roles, same-award
requirements and exact overrides. These verify implementation, not validity of
the taxonomy as a quality measure.

Current 2024 any-role-specific / role-specific-Magister counts: HS teachers
49,121 / 7,066; non-HS teachers 47,234 / 5,766; orientadores 606 / 104;
leadership 2,420 / 1,789. Counts may change after a reviewed mapping revision.
