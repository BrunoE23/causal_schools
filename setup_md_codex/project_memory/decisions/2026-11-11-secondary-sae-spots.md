# Secondary school spots in SAE for every year

The user asked about schools closing, downsizing, and expanding since SAE,
then clarified secondary schools, spots listed in SAE, and every year.
The resulting exercise uses the initial regular-round A1 offers for processes
2016-2025 (entry 2017-2026), the full series currently published by MINEDUC.
It supersedes the initial exploratory all-school directory interpretation
for this request; the older preschool analysis is a separate exercise.

Code: `code/codex/secondary_sae_spots/01_analyze.R`. The companion fetch script
downloads missing 2022-2025 archives and extracts only school offers and
documentation to a new task cache, preserving existing raw files.

Measure both declared capacity (`cupos_totales`) and advertised available
places (`vacantes`). Sum unique A1 programs to RBD-grade-year and then RBD-year
over secondary cod_nivel 9-12. Any positive/negative capacity change defines
expansion/downsizing among positive-capacity continuers; a strict 10-seat
threshold is provided separately, not imposed on the main analysis.

The regional rollout also phased in grades. Grade 9 is nationally covered
from process 2019, but grades 10-12 in Metropolitana enter only in 2020.
Therefore full-secondary national stock comparisons begin in process 2020,
with national adjacent-year changes from 2021. For every earlier transition,
intersect the observed region-grade coverage of the adjacent files before
summing to RBD; this prevents regional/grade rollout from becoming school
expansion. Early rows have different regional/grade coverage and are not
comparable national counts. Separate grade-9 comparisons are also provided.
Nuble is mapped to Biobio. Directories supply geography/names only.

An exit means positive capacity becomes zero or disappears in the compared
secondary coverage. Zero vacancies do not define an exit. This is not a
verified permanent/legal closure or necessarily an exit from all SAE grades.
Actual listing disappearance, continued zero-capacity listing, and absence
from all SAE are separately flagged. No causal effect of SAE is estimated.
The 2024 archive includes an A3 waitlist revision; preserve A1 in all years
to keep the measurement stage consistent.

National declared secondary capacity rises from 985972 in process 2020 to
1043233 in 2025 (+57261, +5.81%). Positive-capacity RBDs rise from 2504 to
2552. In 2025, 1003 continuers expand, 520 downsize, and 1002 are unchanged;
27 positive-capacity entries and 11 exits yield net +16 schools. Of the
11 exits, 8 disappear from all SAE grades, while 3 retain other-grade listings.
Advertised secondary vacancies are 274677 in 2020 and 276448 in 2025, with
substantial intervening variation. These stocks describe SAE-participating
secondary schools, not all Chilean schools or private paid schools.

All key/range/geography and exact school/seat-accounting checks pass. Raw-file
annual totals and national transition counts were separately reproduced in
Python. Data and source manifests are under `data/clean/secondary_sae_spots/`;
the HTML report and plot are under their matching output folders.
