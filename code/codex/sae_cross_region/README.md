# SAE applications across regions

Run from the project root with Rscript `code/codex/sae_cross_region/01_count.R`.
Requires data.table and access to the Box data root, optionally overridden with
`CAUSAL_SCHOOLS_DATA_WD`.

Counts applicants listing schools in two or more destination regions within
the same regular-round SAE process, 2018-2020. Uses C1 submitted choices and
same-year Directorio RBD region codes. Excludes only system-added continuity
choices (`agregada_por_continuidad == 1`); retains applicant priority choices.
Repeated programs/schools/regions do not multiply the applicant count.

Reports all grades, grade 9, and the existing 294,198-person timely grade-9
paper roster. Pooled annual counts are applicant-years; a separate file counts
unique people who ever submitted a multi-region list in a single year.
Geography uses contemporary directory codes; a sensitivity combines Nuble
(16) with Biobio (8). This is distinct from applying outside the home region.

Inputs are read-only. Aggregate outputs are in `data/clean/sae_cross_region/`.
Checks require complete and unique school-region mappings, full paper-roster
coverage, and agreement with the existing grade-9 annual applicant totals.

## Results (November 14, 2026)

All grades: 1,224/274,990 in 2018; 2,118/483,070 in 2019;
1,812/454,415 in 2020. Across years, 5,065 distinct people ever submitted a
multi-region list, out of 1,076,945 distinct applicants (0.4703%). The annual
sum is 5,154/1,212,475 applicant-years (0.4251%).

Paper grade-9 sample: 804/75,837 in 2018; 1,189/110,423 in 2019;
942/107,938 in 2020; pooled 2,935/294,198 (0.9976%). All grade-9 applicants,
before paper-roster restrictions: 3,007 distinct people out of 302,589 (0.9938%).

Every submitted destination has a same-year directory region. Combining
Nuble with Biobio reduces the paper-sample count from 2,935 to 2,737.

## Assignment risk follow-up

Run `code/codex/sae_cross_region/02_count_at_risk.R` to intersect cross-region
choices with the paper roster's existing `any_risk` flag. This flag is
`max(prob) < 1` over simulated program/unmatched outcomes, as constructed in
`code/codex/empirical_bayes_school_va/18_expected_va_risk_by_continuation.R`.
It is not redefined at region or RBD level. The script uses local cached
submitted choices and same-year directory geography, reproduces all three
previous annual multi-region counts, and verifies the total at risk is 162,633.

Of 2,935 multi-region paper applicants, 1,295 (44.1227%) are at risk: 357 in
2018, 534 in 2019, and 404 in 2020. These are 0.7963% of all 162,633 at-risk
paper applicants. Saved in `paper_multi_region_assignment_risk.csv`.
