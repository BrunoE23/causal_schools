# First-round offer compliance

Run `01_make_offer_take_up_tables.R` after the two cohort-paired EB scalar-IV
regression data files have been generated. The script reports the positive
first-round offer rate, the share of the full lottery-risk sample attending the
offered school, and attendance at the offered school conditional on receiving
an offer. Attendance is measured using the most-time high school after grade 8.

The tables report overall compliance, compliance by SAE cohort, gender,
baseline grade-4 math quintile, baseline income quintile, and the exact sample
used for each of the seven main-table outcomes. Outputs are written to
`output/tables/compliance/`.

Run `02_make_application_rank_take_up_table.R` to compare students with and
without a recorded first-round offer. For each group, it reports the fraction
whose eventual most-time high school is their first, second, third, or any of
their top three SAE choices. Rank-specific denominators require that the student
submitted a school at that rank.
