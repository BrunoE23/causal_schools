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

- Status: Resolved for current VA pass
- Date noted: 2026-04-24
- Date resolved: 2026-05-13
- Related decision: `decisions/2026-05-13-use-imputed-simce-income-control-in-va.md`
- Context: While planning school/RBD observational value-added measures, `income_decile` was flagged as messy and not yet reliable enough to use as a student-level control.
- Why it matters: Including a noisy or inconsistently constructed income control could distort controlled school/RBD observational values and make interpretation harder.
- Current rule: `universe_reg_df.R` now constructs `income_mid_imputed` first. It keeps observed SIMCE parent-survey income midpoint values and fills missing values only for students with both baseline SIMCE score controls using median income midpoint from baseline-context cells with at least `15` donor students. It then recomputes `income_decile_imputed` from the observed-plus-imputed midpoint distribution.
- Current VA use: The `school_rbd_observational_values` task includes `income_decile_imputed` in the controlled VA regressions.
- Follow-up: Parent-education controls from the CPAD files may be useful later, but they were not added in this task.

### Consider score-threshold STEM outcome

- Status: Open
- Date noted: 2026-04-24
- Context: For STEM enrollment outcomes, one possible measure would define STEM only among students above a specified score threshold and assign zero to everyone else.
- Why it matters: This would shift the estimand toward a high-achievement STEM outcome rather than an unconditional STEM enrollment rate. It may better capture STEM preparation among students with enough academic preparation, but it also builds achievement directly into the outcome definition.
- Current workaround: The `school_rbd_observational_values` task currently uses unconditional enrollment and field indicators, including STEM indicators based on the existing binary field specs.
- Follow-up: Decide the score threshold, which score to use, and whether the thresholded STEM measure should supplement or replace the unconditional STEM outcome.

### Validate first-offer merge in `universe_reg_df`

- Status: Open
- Date noted: 2026-04-26
- Context: While adding first-round offer information to `universe_reg_df`, joining raw `treatment_1R_v2.RData` by `mrun` expanded the broad universe from 944,360 rows to 1,778,389 rows because `all_treatments` is application-level.
- Current working rule: Collapse offers to `mrun` x `sae_proceso`, define `rbd_treated_1R` as the nonzero first-round offered RBD if one exists, then keep the earliest `sae_proceso` per student before merging to the broad universe.
- Why it matters: The broad-universe file should remain one row per student. Application-level offer rows should not duplicate student rows.
- Follow-up: After rebuilding `univ_gr8_df`, verify one row per `mrun`, verify `offers_1R_first` is one row per `mrun`, and check that no `mrun` x `sae_proceso` has multiple distinct nonzero first-round offered RBDs.

### `stem_enrollment_m1` source-of-truth in scalar IV construction

- Status: Open
- Date noted: 2026-04-26
- Context: The scalar IV construction script starts from `univ_gr8_df`. If `stem_enrollment_m1` is missing, the script currently reconstructs it as `f_science_m1 == 1 | f_eng_m1 == 1`, with missing field indicators treated as zero.
- Why it matters: This is intended to mirror the school-value construction, but the denominator/source-of-truth should be explicit. Reconstructing the outcome inside the scalar IV construction could add zero cases for students without observed higher-education enrollment if the upstream data do not already define the outcome.
- Current workaround: Leave the guard as-is for now; if `stem_enrollment_m1` exists upstream, the script does not overwrite it.
- Follow-up: Decide whether `stem_enrollment_m1` should be created upstream in `universe_reg_df` and required by the scalar IV script, or whether reconstruction in the scalar IV construction is acceptable with explicit denominator documentation.

### 2017 timely-SAE entrants lack probability rows

- Status: Open
- Date noted: 2026-04-27
- Context: Before applying any supported-school-set threshold, the broad grade-8 universe contains 318,813 timely SAE entrants, but only 296,173 are matched to assignment-probability rows.
- What was found: The full gap of 22,640 students comes from `cohort_gr8 == 2017` and `sae_proceso == 2017`. The current probability files used by the support and scalar-IV scripts start at `DA_probs_2018.csv`, so these 2017 process entrants have no probability rows.
- Why it matters: The lottery-risk sample is currently defined only among entrants with probability rows. If 2017 process entrants are valid lottery participants, excluding them reduces the sample. If they are not part of the grade-9 assignment window we can instrument, then `timely_sae` or the scalar-IV sample documentation should make this exclusion explicit.
- Current workaround: Risk and support calculations use the matched probability files for 2018-2021. This yields 296,173 entrants with probability rows and 163,600 at-risk entrants before school-support trimming.
- Follow-up: Check whether 2017 SAE probability simulations exist or whether `sae_proceso == 2017` should be excluded/reclassified for the grade-8 cohort design. Then update the sample definition in `univ_gr8_df`/`universe_reg_df` documentation and the scalar-IV construction scripts if needed.
