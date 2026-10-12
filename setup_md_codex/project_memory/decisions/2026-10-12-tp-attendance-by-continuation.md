# TP attendance by origin-school continuation

Use the same 294,198 Table 2 SAE applicants and origin continuation labels as
the main IV subgroup exercise. Match their actual most-attended school
(`most_time_RBD`) to the exact TP flag in `admin_type_va/01_build_table.py`:
`school__has_tp_or_artistic` minus artistic-school membership. This is a
2024 directory-based school offering classification and includes mixed schools;
it is not individual TP-track enrollment or a cohort-contemporaneous track flag.

Among classified attended schools, TP shares are 68.0664% for school-ending
applicants (170,473/250,451) and 47.2418% for voluntary applicants
(18,361/38,866). Within assignment risk the shares are 60.2961%
(81,662/135,435) and 41.7323% (10,022/24,015), respectively.
Unclassified counts are 113 and 2 for all applicants, and 42 and 1 within risk.
Coverage exceeds 99.95% in both full groups; missing schools are not coded zero.

Reproduce with `code/codex/empirical_bayes_school_va/20_tp_attendance_by_continuation.R`.
Counts, coverage, and missing-classification bounds are saved in
`data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/tp_attendance_by_continuation.csv`.
The existing open balance-table document is unchanged.
