# Decision: Handle RBD Metadata Inside School-Year Aggregations

**Date:** 2026-04-24
**Status:** active

## Context

While reviewing `code/Enzo/task1`, the original notebook summarized PSU outcomes by `RBD` and then joined back `distinct(RBD, metadata...)`. This produced duplicated `RBD`-`year` rows when the raw file contained more than one metadata combination for the same `RBD`.

The discussion clarified that the issue is not that `RBD` is unusable as a school identifier. The more precise issue is that metadata attached to an `RBD` may vary within or across years, and treating that metadata as uniquely implied by `RBD` can break school-year summaries.

## Options Considered

### Join distinct metadata after aggregation

- Pros: Simple and close to the original notebook.
- Cons: Duplicates already-collapsed school-year rows when metadata is non-unique.
- Verdict: Rejected.

### Summarize metadata inside the same school-year aggregation

- Pros: Preserves one row per `RBD`-`year` and makes the metadata rule explicit.
- Cons: Requires choosing a rule for metadata conflicts.
- Verdict: Selected for current task scripts.

### Merge to a separately cleaned school directory

- Pros: Likely best for production if region, comuna, or dependency are analytically important.
- Cons: Requires a cleaned directory and harmonization work beyond the immediate task.
- Verdict: Deferred.

## Decision

For school-year summary construction, summarize outcomes and metadata in the same `group_by(RBD, year)` or equivalent aggregation. Do not collapse outcomes by `RBD` and then join back non-unique `distinct(RBD, metadata...)`.

For the current task scripts, use the first non-missing metadata value within each `RBD`-year group and write separate diagnostics for metadata conflicts.

## Rationale

This preserves the intended one-row-per-school-year structure and avoids mistaking metadata variation for evidence that `RBD` itself is unstable. It also keeps unresolved metadata questions visible through diagnostics rather than hiding them in duplicated output rows.

## Implementation

The revised scripts in `code/Enzo/task1` summarize metadata inside the grouped aggregation and write diagnostic CSVs for:

- cross-year `RBD` overlap,
- `RBD`s with metadata changes across years,
- coarse metadata combinations associated with multiple `RBD`s.

## Risks and Follow-Up

The first-non-missing metadata rule is acceptable for task-level summaries but may not be the final production rule. If metadata fields are used as controls, sample restrictions, or substantive outcomes, prefer a cleaned school directory or a modal metadata rule after inspecting conflicts.
