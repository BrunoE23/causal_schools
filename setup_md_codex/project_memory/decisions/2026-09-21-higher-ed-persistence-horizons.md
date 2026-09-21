# Higher-education persistence horizons

For persistence outcomes based on the full 2022--2025 SIES matricula panel,
use grade-8 cohorts 2017--2020. Expected higher-education entry is grade-8
cohort plus five years, as confirmed by observed PSU and first-enrollment years.
Cohort 2017 has three follow-up years, cohort 2018 has two, cohort 2019 has one,
and cohort 2020 has entry-year status but no full-SIES follow-up through 2025.

Keep horizon names explicit (`_y1`, `_y2`, and `_continuous_through_y2`) rather
than pooling unequal follow-up windows. Define both conditional persistence
among entry-year enrollees and unconditional entry-and-persistence outcomes.
For multiple valid annual enrollments, use set overlap for same-program,
same-generic-area, and same-institution retention.

High-premium field and institution definitions must match the existing first-
enrollment outcomes. The institution threshold remains a centered MiFuturo
institution fixed effect above 0.1.

High-premium field is a positive-list binary. Agriculture, Services, and any
other observed enrollment outside Science, Law, Engineering, and Medicine+ are
coded zero even if the broader `field_reclassified` taxonomy is missing.

Do not treat the PAES 2026 admissions-matricula file as full SIES persistence
for the 2020 cohort. It has different institutional coverage and is retained
only as a separately labeled admissions outcome.
