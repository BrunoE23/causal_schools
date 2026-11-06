# SAE school applications inside versus outside home region

Run `01_tabulate.R` with R, data.table and sf from the repository root, after
the higher-ed-geography home-region construction. Uses its validated roster
of 294,198 timely Grade 9 SAE applicants in 2018-2020 and their 162,633 at-risk
subset, without PAES-taking or baseline-completeness restrictions.

This describes the regions of schools actually applied to, not merely the
regional distribution of applicants. Primary choices come from regular-round
C1 records with `agregada_por_continuidad == 0`, excluding system-added
continuity options while retaining all other priority categories. Collapse
multiple programs at the same RBD to one student-school choice per process.
A separate sensitivity includes all C1 choices, including automatic continuity.
No restriction to positive simulated admission probability is imposed.

Home region uses the existing SAE B1 home-coordinate spatial match. School
region uses same-year A1 Grade 9 school coordinates, matched by exact polygon
containment to the same current 16-region boundaries. Multiple valid school
locations at one RBD/year must agree on region; ambiguous or entirely unknown
schools remain missing. No historical-code mismatch for Nuble, nearest-region
assignment or Grade 8 fallback is introduced. The region audit retains missing
locations and conflicting/multiple-region evidence.

Choice shares use known home-school geography. Applicant profiles partition
fully classified lists into home-only, both, and outside-only; unknown lists
and no-choice applicants are reported separately. A separate any-known-outside
count retains evidence of an outside choice on partially observed lists.
Counts and denominators are explicit; applicants and school choices must not
be conflated. The initial roster, student keys, probability-risk definition
and historical geocoding quality are unchanged.

Read five selected C1 columns from files of about 48/87/78 MB, filter to the
roster before joins, and read only needed A1 coordinate columns. Raw files and
existing geography data are never modified. Derived school geography audits,
row counts, choice/application summaries, student profiles and matched choice
rows are under `data/clean/sae_application_geography/`. A readable HTML table
is under `output/tables/sae_application_geography/`.

Checks cover sample totals and unique student/process keys, unique school/year
geography, valid continuity flags, known school links, common region codes,
choice count conservation and complete applicant-profile partitions.

Submitted unique-school choices: 995,525 home and 9,259 outside (99.0785% /
0.9215%) among all SAE applicants; 621,540 home and 4,853 outside (99.2252% /
0.7748%) among at risk. Geography is missing for 2,469 and 1,273 choices,
respectively, excluded from those share denominators.

There are 293,361 fully classified SAE applicant lists and 162,278 at-risk
lists. Their home-only/both/outside-only counts are 288,718/3,026/1,617 and
160,162/1,338/778. Thus 4,643 (1.5827%) and 2,116 (1.3039%) have at least one
outside choice among fully classified lists. There are 837 and 355 applicants
with unknown list geography; every applicant has at least one submitted
school choice. Two additional all-SAE applicants have a known outside choice
on a partially observed list, giving an any-known-outside count of 4,645.
Including automatic continuity schools gives outside-choice shares 1.0142%
and 0.8795%, respectively. These are school-choice shares, not migration rates.
