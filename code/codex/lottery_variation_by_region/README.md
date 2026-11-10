# School lottery variation by region

Reconstruct the user-confirmed always / sometimes / never classification of
lottery variation across years, for the paper's SAE 2018-2020 window. The earlier
tally could not be found; this does not claim to reproduce its denominator.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/lottery_variation_by_region/01_build_report.R
```

Requires only data.table. Read-only source root defaults to
`C:/Users/brunem/Box/causal_schools`; override CAUSAL_SCHOOLS_DATA_WD if needed.
Read three columns from each 8-12 MB DA probability file, grade-9 offerings from
the three A1 files, three school metadata columns from each directory, and the
18 MB cached paper roster. No full master or wide probability matrix is read.

The main school universe is every RBD with a grade-9 regular-round SAE offering
in at least one year, including zero reported vacancies. Sum program/campus
assignment probabilities within student-year-RBD. A school-year has variation
if any simulated applicant has 0 < p < 1, with 1e-10 endpoint roundoff tolerance.
There is no minimum-count support threshold. Deterministic probabilities that
differ across students do not establish lottery variation.

Classify each RBD across its observed offering years: always (all), sometimes
(some but not all), or never (none). Do not count pre-rollout or non-offering
years as zero. Report years observed, plus a common balanced 2019-2020 window
and a supplement excluding schools observed only once. Region is the RBD's
latest 2018-2020 directory region, preserving Nuble's new regional code. Each
school counts equally; percentages are conditional on region. This uses school
location, not a student's PAES domicile.

Main evidence uses all students in the saved grade-9 simulations. A separate
table restricts evidence to the unchanged 294,198 paper-roster applicants and
keeps the school denominator fixed. No high-math or HE selection is imposed.
Never means no interior probability observed in the saved finite simulations.

Derived data and audits: `data/clean/lottery_variation_by_region/`.
HTML: `output/reports/lottery_variation_by_region/report.html`.
LaTeX fragment: `output/tables/lottery_variation_by_region/region_distribution.tex`.
Verification covers probability mass, program-to-school aggregation, unique
school-year keys, regional coverage, category identities, totals, paper-roster
coverage, and unchanged input checksums. All raw and master inputs are preserved.

Completed totals: 2,529 schools, of which 2,211 always (87.43%), 299 sometimes
(11.82%), and 19 never (0.75%). All schools match the 2020 directory. Observation
lengths are 37 schools with one year, 831 with two, and 1,661 with three. The
paper-roster supplement changes five schools from always to sometimes, yielding
2,206 / 304 / 19. The balanced 2019-2020 universe has 2,482 schools and category
counts 2,216 / 225 / 41 (89.28% / 9.07% / 1.65%). All verification checks pass;
no probability school falls outside the corresponding grade-9 A1 offerings.
