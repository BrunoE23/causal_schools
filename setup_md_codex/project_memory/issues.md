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
