# Table 2 with applicant continuation groups

Run from the repository root:

1. `01_continuation_groups.R` constructs group summaries, verifies the original
   paper columns, and renders the new table.
2. `02_report_continuation.R` rerenders from the small saved summaries alone.

Use R with data.table. The original Table 2 source is
`code/claude/balance_table/01_build_balance_table.R`; the comparison reference
is `output/tables/balance_table/balance_table.csv`. Its existing outputs are
not overwritten. The variant is
`output/tables/balance_table/balance_table_continuation.tex`, with an HTML view
at `output/reports/balance_table/table2_continuation.html`.

The first three columns retain Table 2's broad panel, timely SAE applicants and
assignment-risk applicants. The two new columns restrict the SAE-applicant
group to grade-8 origins classified in the existing participation cache as
ending at grade 8 or offering grade 9. They do not use the broader application
indicator from the participation exercise. Unresolved/other origins enter
neither new column. Means, complete-block match rates, N and percent of broad
panel are shown; difference columns and tests are omitted.

Inputs are read-only: selected fields of Box `data/clean/univ_gr8_df.csv`,
2018-2020 long DA probability files, and the local SAE-participation
`admin_panel.rds`. Preflight sizes are recorded. The 1.23 GB CSV is read with
column selection; the 160 MB RDS is immediately reduced to four fields.
Outputs and replication/coverage diagnostics are local under
`data/clean/balance_table_continuation/`.

Verification compares all 45 original means, group sizes and per-variable
match rates to the paper CSV at tolerance 1e-10. It also checks unique student
joins, complete linkage of origin classifications, mutually exclusive new
groups, and complete accounting of all SAE applicants.
See `setup_md_codex/project_memory/decisions/2026-10-12-table2-continuation-groups.md`.
