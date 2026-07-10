# PAES 2026 Integration Audit

- Raw 2026 file families present: 5 / 5.
- Clean score rows for 2026: 215142
- Clean application rows for 2026: 1694081
- Main universe cohort range: 2017-2021.
- Main universe records with `psu_year == 2026`: 200024.
- `stem_outcome.RData` first-application rows in 2026: 141178.
- `program_info_22-25.rds` rows: 15826.
- PAES oferta 2026 rows matched to `program_info_22-25.rds`: 2060.

## Remaining Scope Notes

- Grade-8 cohort 2021 is present in the main universe audit.
- Student tracking 2025 is present, so the four-year post-grade-8 `most_time_RBD` window can be reproduced for grade-8 cohort 2021.
- Full SIES `Matricula-Ed-Superior-2026` is still absent; PAES matricula 2026 is admissions-process matricula and is not a replacement for full SIES coverage.
- PAES matricula 2026 has been mapped to oferta/COD_SIES for diagnostics, but it remains a separately labeled PAES-only source.
