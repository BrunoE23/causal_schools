# Project Issues Log

## Open Issues

### Duplicate student-school rows in `sample_students`

- Status: Open
- Date noted: 2026-04-02
- Context: While constructing the school-level first-stage table, `sample_students` was found to contain duplicate `mrun`-`proceso`-`rbd` rows.
- What was found: There were 2,676 duplicated student-process-school combinations, with a maximum multiplicity of 3.
- Why it matters: School-level constructions can overcount applications or offers if they treat `sample_students` as unique at the `mrun`-`proceso`-`rbd` level.
- Current workaround: For the first-stage exercise, offers were merged at the `br_code` level and then collapsed back to one `mrun`-`proceso`-`rbd` row using `any(offered_spot_1R == 1)`.
- Follow-up: Clarify whether duplicates reflect true multiple course-level applications within school, a feature of `br_code`, or a data construction issue that should be cleaned upstream.

### RBD metadata joins can duplicate school-year summaries

- Status: Open
- Date noted: 2026-04-24
- Related decision: `decisions/2026-04-24-rbd-metadata-handling.md`
- Context: While reviewing `code/Enzo/task1`, the original notebook summarized PSU outcomes by `RBD` and then joined back `distinct(RBD, metadata...)`.
- What was found: The existing 2012-2020 output files contained 60 duplicated `RBD`-`year` pairs after this join pattern.
- Why it matters: Metadata variation can make it look like `RBD`s are unstable, but the immediate problem is the use of non-unique metadata in a post-collapse join. This breaks the intended one-row-per-school-year structure.
- Current workaround: In the revised scripts, metadata is summarized inside the same `RBD`-year aggregation using an explicit first-non-missing rule, and separate diagnostics flag metadata changes.
- Follow-up: For production work, consider merging to a cleaned school directory or choosing a modal metadata rule, especially if region/comuna/dependency are analytically important.

### Income decile control is not ready for value-added work

- Status: Open
- Date noted: 2026-04-24
- Context: While planning school/RBD observational value-added measures, `income_decile` was flagged as messy and not yet reliable enough to use as a student-level control.
- Why it matters: Including a noisy or inconsistently constructed income control could distort controlled school/RBD observational values and make interpretation harder.
- Current workaround: The `school_rbd_observational_values` task excludes `income_decile` from the control set. It uses cohort, student gender, student comuna (`COD_COM_ALU`), and prior SIMCE math/language instead.
- Follow-up: Clean and validate the income measure before adding it to controlled value-added specifications.

### Consider score-threshold STEM outcome

- Status: Open
- Date noted: 2026-04-24
- Context: For STEM enrollment outcomes, one possible measure would define STEM only among students above a specified score threshold and assign zero to everyone else.
- Why it matters: This would shift the estimand toward a high-achievement STEM outcome rather than an unconditional STEM enrollment rate. It may better capture STEM preparation among students with enough academic preparation, but it also builds achievement directly into the outcome definition.
- Current workaround: The `school_rbd_observational_values` task currently uses unconditional enrollment and field indicators, including STEM indicators based on the existing binary field specs.
- Follow-up: Decide the score threshold, which score to use, and whether the thresholded STEM measure should supplement or replace the unconditional STEM outcome.
