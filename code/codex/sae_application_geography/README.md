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

## Q1 school exposure anywhere, top three, and first

Run `02_q1_exposure.R` after the baseline-by-VA-quintile construction. This
uses the same 294,198 SAE applicants and 162,633 at-risk students and actual
C1 submitted choices, without geography, PAES, or baseline-completeness
restrictions. Automatic continuity options are excluded; all other priority
categories remain. Each applicant has exactly one submitted rank-one school.

Use the four separate current unified R EB school rankings: math, verbal,
high-premium field and high-premium institution. Q1 is the lowest quintile,
with the same fixed equal-school-weight cutoffs over all finite-VA schools,
including private schools, as the preceding baseline tables. Verify the
saved assignments against the current EB source. Do not rerank schools within
SAE, regions, lists or risk groups.

Multiple programs at one RBD inherit the school's best original preference
rank. Top three means original C1 rank <=3, and top one means rank one;
distinct RBDs are not reranked after programs/automatic options are removed.
Each cell counts students with at least one Q1 school in that list segment,
not the fraction of school choices in Q1. The full applicant denominator is
retained for every outcome/window.

An event is confirmed if any school in the segment is known Q1, ruled out if
all schools are classified and none is Q1, and otherwise unknown. Main CSVs
give confirmed shares over all students, upper bounds allowing unknown events
to be positive, and classifiable-event shares with their own denominators.
Thus missing school VA is never silently treated as a known non-Q1 school.
The largest unknown-event share is 0.344 pp (all-applicant field, anywhere).
Student-level positive exposure is checked to nest: first implies top three,
which implies anywhere.

Outputs are in `q1_exposure/` subdirectories of the preceding clean-data and
table folders. They include all 24 outcome/group/window results, cutoffs,
rank audits, a compact student-exposure RDS, bounds and classifiable-share
tables, and HTML with counts and uncertainty. Confirmed anywhere/top3/top1
shares for all SAE applicants are math 55.85/49.78/27.70%, verbal
58.41/53.17/31.98%, field 54.22/48.37/24.22%, institution
49.27/47.66/34.10%. At-risk counterparts are math 55.65/47.00/23.82%, verbal
55.16/47.59/24.83%, field 55.48/47.17/20.98%, institution
49.34/47.37/37.60%. These are confirmed lower shares; full bounds are saved.

## Q1 assignment probability by income decile

Run `03_q1_probability_by_income.R` from the repository root, with the same
R environment used above. Inputs are the cached SAE applicant/E roster,
the saved school quintiles verified against the current R EB output, and
the original 2018-2020 DA probability files. `CAUSAL_SCHOOLS_DATA_WD` can
override the default Dropbox input root.

For each student and each separate VA ranking, sum SAE assignment
probabilities over Q1 schools. Average these probabilities within each
existing imputed family-income decile, separately for all SAE applicants
and the at-risk subset. These are assignment probabilities, not subsequent
attendance rates. Include automatic continuity options and retain the
original distribution without renormalizing; unmatched contributes zero.
Q1 is the lowest quintile, with exactly the preceding national school
cutoffs, including private schools.

Probability on real schools without VA is recorded separately. Confirmed
Q1 probability is the lower bound; adding unknown school mass gives the
upper bound. These are coverage bounds, not confidence intervals. Income
is available for 240,719/294,198 applicants and 135,668/162,633 at risk.
Missing income is reported separately and remains in the overall rows;
income deciles are neither recomputed nor newly imputed.

The script validates complete probability partitions, reproduces risk
flags, and reconstructs continuous E and unknown-VA mass against the saved
roster (maximum absolute discrepancy below 4e-16). Student probabilities,
summary CSVs, income coverage, and validation audits are under
`data/clean/sae_application_geography/q1_probability_by_income/`. The HTML
tables and four-panel PNG are in the corresponding `output/tables/` and
`output/figures/` subdirectories. At-risk decile-1/decile-10 probabilities
are math 28.50/13.75%, verbal 32.42/11.79%, field 24.70/15.59%, and
institution 40.07/17.84%.
