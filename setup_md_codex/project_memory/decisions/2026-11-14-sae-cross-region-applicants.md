# SAE applicants listing schools in multiple regions

The requested diagnostic is interpreted as applying to schools in at least two
destination regions within the same regular-round SAE process. Count people,
not application rows. Use 2018-2020, the paper's SAE process window, and show
all grades, grade 9, and the existing 294,198-person timely grade-9 paper roster.

Use the C1 application files and same-process-year official school-directory
region codes joined by RBD. Remove automatically added continuity choices;
retain submitted choices with any priority. Deduplicate destination regions
within person and process. Do not restrict to VA support or lottery variation.
Require complete geography and coverage of the entire paper roster.

Sum annual counts only under the label applicant-years. Separately deduplicate
people across years, counting whether a person's list spanned regions in any
single observed year. Applying in different regions in different years alone
does not qualify. This also differs from applying outside one's home region.

The main definition follows contemporary official geography. A separate
historical-geography sensitivity combines Nuble (16) with Biobio (8), because
the region boundary changed around the beginning of this window.

Code: `code/codex/sae_cross_region/01_count.R`.
Aggregate results: `data/clean/sae_cross_region/`.

Result: 5,065 of 1,076,945 distinct all-grade applicants ever submitted a
multi-region list (0.4703%); the annual sum is 5,154 of 1,212,475 applicant-years
(0.4251%). In the paper roster, 2,935 of 294,198 students (0.9976%) submitted
multi-region lists. All destination-school region matches are complete and
annual grade-9 denominators reproduce the existing all-grade tabulations.

Follow-up: "at risk" uses the paper roster's existing `any_risk`, defined by
`max(prob) < 1` over simulated program/unmatched outcomes. Preserve this
definition instead of aggregating to school or region. Among the 2,935
multi-region paper applicants, 1,295 (44.1227%) have risk; these represent
0.7963% of the 162,633 at-risk paper applicants. Code:
`code/codex/sae_cross_region/02_count_at_risk.R`. Annual submitted-choice
counts and the pooled risk denominator reproduce the existing saved results.
