# Directory-only preschool school entry and status changes

User correction: ignore SAE years and use school directories. The new script
code/codex/kinder_capacity/04_directory_preschool_changes.R reads only annual
raw directories for 2015-2025. No SAE geography, process years, offerings, or
SAE-dependent cache restricts this national exercise. Earlier SAE calculations
are retained as historical outputs but are not the answer to this request.

The directories do not contain capacity/vacancy fields. MATRICULA is binary;
MAT_TOTAL starts in 2022 and measures all-school enrollment; MAT_ENS_1..8
appear only in 2025, with MAT_ENS_1 measuring preschool enrollment. Thus a
consistent series of preschool/kinder capacity changes greater than 10 seats
cannot be recovered from directory files alone. We explicitly tell the user
this limitation rather than substitute schoolwide enrollment for capacity.

Count active preschool schools using ESTADO_ESTAB=1, MATRICULA=1, and any
ENS teaching code=10. Preschool covers all educacion parvularia levels, including
nursery/middle levels, pre-kinder and kinder. It cannot isolate kinder. Check all ENS
slots (nine in 2015-2018, eleven in 2019-2025). Directory years denote their
annual April snapshots directly. 2015 is a left-censored baseline with no
opening count. First observed school operation in 2016-2025 is a candidate
opening rather than a verified founding date. RBD changes and operation before
2015 remain unobserved. Distinguish schools newly recorded in their first
operating year from those previously recorded but never observed operating.

Exact annual change in active preschool school numbers decomposes into:
first school operation with preschool + continuing operating schools adding
preschool + previously operating schools returning with preschool - continuing
operating schools dropping preschool - preschool schools no longer observed
operating. Continuing requires operation in both adjacent years. Additions can
reintroduce earlier preschool provision; first-ever-in-history additions are
retained separately. Return requires observed prior operation before the
preceding inactive year. Directory absence is treated as not observed operating,
not proof of permanent closure. Every annual accounting residual is zero.

The active preschool stock falls from 6133 in 2015 to 5841 in 2025. The 2025
raw directory has 5843 ENS10 RBDs, but one is in recess and one closed, so
5841 satisfy the operation/enrollment condition. MAT_ENS_1>0 agrees exactly
with ENS10 across all 16768 raw 2025 records. Do not use the previously
suggested, unreplicated 5924 figure. Registry counts or total teaching flags
must not substitute for active stocks.

Outputs are under data/clean/school_directory_preschool/ and the directory-only
report is output/reports/kinder_capacity/directory_only_preschool_changes.html.
PNG/PDF plots are under output/figures/kinder_capacity/. Source manifests and
field-availability audits accompany the data; raw directories are unchanged.
