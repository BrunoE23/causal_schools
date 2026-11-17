# Secondary capacity growth and saved school VA

Follow-up request: check whether changes in secondary capacity over 2021-2025
correlate with a VA measure. Use SAE process years, preserving the preceding
task's cupos_totales definition across grades 9-12 and initial A1 offers.

Code is `code/codex/secondary_sae_spots/02_correlate_va.R`. The endpoint panel
starts with all 2,511 schools having positive capacity in 2021. Capacity absent
or zero in 2025 is zero listed capacity, not a claim of permanent closure;
24 schools have this endpoint exit. New listings with no positive 2021 capacity
are excluded because baseline percentage growth is undefined.

Main growth is 100*(C_2025/C_2021-1). Match by RBD to the current All-sample EB
student-centered school VA file for grade-8 cohorts 2017-2020 under the Box
`empirical_bayes_school_va/cohorts_2017_2020/` clean folder. Primary dimensions
were chosen before calculating correlations from the existing preference
analysis: math, exam taking, high-premium field, high-premium institution.
Supplementary saved outcomes are language, higher-ed enrollment, financial aid
application, and log predicted program income. These are adjusted observational
VA estimates, not individual-school lottery IV effects.

Correlations weight schools equally. Save Pearson and tied-rank Spearman for
percentage growth, absolute seat change, log growth among continuers, and
percentage growth with baseline capacity >=100. Unshrunk VA is an additional
sensitivity. No winsorization, controls, inferential p-values, or measurement-error
correction are used. Annual correlations use each prior year's positive-capacity
schools, for changes ending in 2022-2025; they are not a balanced panel.

Main matched Ns are 2,503 for math/field/institution/language/income and 2,506
for exam/enrollment/aid (99.7-99.8% baseline coverage). All 24 endpoint exits
have every VA measure. The two most extreme raw growth cases, RBD 16970
(1 to 140 seats) and 16780 (35 to 245), lack every VA and do not affect any
reported correlation. The largest matched growth is 354.3% at RBD 16919.
Retain these unmatched records in diagnostics; do not describe them as trimmed.

Pearson correlations with percent growth are math .053285, exam .019773,
field .001627, institution .021909, language .050837, enrollment -.006335,
aid -.015191, and log income .040126 (use saved CSV for exact values).
Math remains about .05-.06 in rank, log-growth, and baseline-100-plus checks;
field and institution remain close to zero. These are small descriptive
associations. VA outcomes overlap the capacity period, preventing an
interpretation as forecasts based only on information available in 2021.

Endpoint/matched data, coverage, unmatched schools, all 40 sensitivity rows,
annual correlations and source manifests are under
`data/clean/secondary_sae_spots/va_correlations/`. The HTML report and four-panel
scatter figure are under the existing task's output folders. Independent Python
source joins reproduce all 40 sample counts, Pearson and Spearman correlations
to 1e-12, and the figure is visually checked.
