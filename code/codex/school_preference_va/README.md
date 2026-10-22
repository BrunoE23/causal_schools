# School preferences and four-dimensional EB value added

## Full-school peer extension (2026-10-22)

The user superseded the top-100 restriction and requested the whole school
population. Run from the project root, using Rscript:

1. `code/codex/applicant_composition_trends/11_extend_all_school_group_averages.R`
2. `code/codex/applicant_composition_trends/10_verify_group_averages.R --all-schools`
3. `code/codex/school_preference_va/38_all_school_peer_preflight.R`
4. `code/codex/school_preference_va/36_fit_peer_populations.R --all-schools`
5. `code/codex/school_preference_va/37_report_peer_populations.R --all-schools`

Peer construction now includes all 2,524 genuine SAE destinations plus all
positive offered and most-attended-school RBDs in the 2018-2020 universe:
3,964 schools and 11,892 primary school-group rows. Retain the same three
population definitions, four measures and equal-available-cohort means.
The original 100-school measures reproduce exactly to numerical precision.

All 2,481 original D schools have attendee measures; applicant and offer
measures are complete at 2,456 and 2,469 schools respectively. Complete-list
intersection retains 267,937 rankings (98.08%), 940,605 pairs and 2,454
destinations. Entire rankings with any missing peer regressor are excluded,
never trimmed. Re-estimate D on this common sample with all ten original
fee indicators. The four main columns have identical samples and controls.

Peer normalization is common across populations, using attendee-school
means/SDs across the 2,454 retained destinations. Original VA units remain
unchanged. Models, covariances, normalization, eligibility, coverage by year,
diagnostics and block comparisons use the new `peer_populations_all_schools`
clean-data subfolder. The main report is
`output/reports/school_preference_va/with_peer_populations_all_schools.html`;
table companions use the corresponding output/tables subfolder. Earlier
top-100 inputs, reports and model objects remain unchanged.

## D plus three peer populations (2026-10-22)

Run `35_peer_composition_preflight.R`, `36_fit_peer_populations.R`, then
`37_report_peer_populations.R` from the project root. This new report uses
the supplied 100-school composition table in
`data/clean/applicant_composition_trends/group_averages/` directly.
The user selected the 13,037 complete lists entirely inside its coverage:
36,276 pairs and 99 destinations, shared by all four columns. Do not trim
rankings. D is re-estimated on this restricted sample; five identically
zero fee indicators are omitted from every model.

Add parent education, baseline SIMCE, female share and income decile
jointly, separately for weighted applicants, first-round offered students,
and all most-attended-school attendees. Use equal-cohort means over
available 2018-2020 cohorts. All peer variables share attendee-school mean/SD
normalization over the 99 estimation destinations; existing VA units stay
unchanged. Retain student-clustered covariance and the original distance.

The new report is `output/reports/school_preference_va/with_peer_populations.html`.
Models, covariance matrices, source preservation, diagnostics and peer
normalizations use `data/clean/school_preference_va/peer_populations/`.
The four-column table and sample/fit companions are under the matching
`output/tables` subfolder. Three auxiliary no-VA fits support incremental
fit comparisons without adding columns to the main table. Source data,
the full-sample D model and all previous reports remain unchanged.
See the decision record `2026-10-22-peer-populations-preferences.md`.

## Current report: additive specifications (2026-10-21)

The current report follows the requested comparison:

- A: distance and directory fee bands.
- A + averages: A plus the four observed school outcome averages, with no
  school-type indicators. This column is inserted between A and B.
- B: A plus mixed HC/TP, TP-only and four observed school outcome averages:
  PAES M1 mean, exam-taking share, high-premium-field enrollment share and
  high-premium-institution enrollment share.
- C: A plus the four standardized EB VAs.
- D: C plus mixed HC/TP and TP-only; PAES M1 is excluded.

All five specifications use the same 273,184 full submitted lists, 969,377
school alternatives and 2,481 destination schools from the preceding fee
extension. School-attribute normalizations and the ten identified fee
indicators are unchanged. School-type effects use Non-TP as reference.
All slopes are additive and common across types. Parent education, parent
income and type-by-attribute interactions do not enter these specifications.

Run 34_build_school_outcome_means.R first to attach the three unadjusted
binary-outcome means to the existing directory_prices_2024 ranking data.
Then run 31_fit_four_specifications.R, 33_verify_four_specifications.R and
32_report_four_specifications.R from the project root. Fit all columns with published student
coordinates and again with home-comuna distances. Source/model hashes,
new fitted objects, clustered covariance matrices and checks are under
`data/clean/school_preference_va/directory_prices_2024/four_specifications/`;
two aggregate companion tables use the same output/tables subfolder.

The current report remains
`output/reports/school_preference_va/with_directory_prices_2024.html`.
It displays one comparison table, with the municipality version in a collapsed
section. The earlier comprehensive report is preserved as
`with_directory_prices_2024_full.html`; script 29 now renders that archived
view so rerunning it does not overwrite the simplified current report.

The user subsequently removed PAES M1 from D only; B retains it. To update
only D, pass `--specifications=D` to script 31; unselected fitted objects and
coefficient tables are preserved and checked. The default refits all five
specifications. Then run scripts 33 and 32 as usual.

The next B-only extension adds the other three observed school outcome
averages as unadjusted counterparts to the VAs. These are existing `raw_mean`
values from the precursor school-VA file for grade-8 cohorts 2017-2020,
without adjustment or EB shrinkage. Exam taking uses all eligible school
attendees; field and institution enrollment use exam takers, matching the
outcome definitions in the VA constructor. New shares are standardized
across the same unique destination schools with equal school weights; the
existing PAES normalization is retained. Run script 31 with
`--specifications=B` to update B alone after script 34; A, C and D are
preserved. Source means, denominators, normalization and provenance are in
`data/clean/school_preference_va/directory_prices_2024/four_specifications/school_outcome_means/`.

Current B coefficients per school SD (published student coordinates) are
PAES M1 0.0517 (SE 0.0031), exam-taking share 0.1233 (0.0033), field share
0.0699 (0.0018) and institution share 0.0402 (0.0036). All 273,184 applicants
remain in the sample. Construction, partial-fit, independent and report
checks pass (22, 26, 124 and 10 respectively).

The original eight models converge in three iterations. In C versus the current D
under published student coordinates, math VA changes from 0.0180 to 0.0491
and exam-taking VA from -0.0184 to 0.0778 when school type alone is added.
The current report shows their student-clustered standard errors. B and D
are not nested: B uses the four observed outcome averages; D uses the four VAs.

To add/update just the intermediate column, run script 31 with
`--specifications=A_means`, then scripts 33 and 32. Its visible label is
`A + averages`. It retains A's distance and fee controls and adds the four
school outcome averages, isolating that block before adding school type
in B. Existing A, B, C and D objects and exports are preserved. There are
now ten model objects including the comuna-distance counterparts; the
legacy `four_specifications` folder and script names are retained.
Both new fits converge in three iterations and the expanded independent
verification passes 142 checks. Under published student coordinates, the
four average coefficients are PAES M1 0.0306, exam taking -0.0302, field
0.0743 and institution 0.0506 per school SD; the report includes their
student-clustered standard errors.

## Directorio fee-band preference extension (2026-10-21)

Add 2024 Directorio PAGO_MATRICULA and PAGO_MENSUAL as categorical fee levels
to the latest joint model with four EB VAs, PAES M1, parent education and
grade-4-SIMCE income composition. Keep existing type interactions, both
distance definitions and all five applicant groups. This source contains
reported ranges, so free is the reference and paid bands are separate raw
indicators. The lowest monthly paid band has no observations. Missing fee
information occurs for the same schools in both fields and enters through
one shared indicator. Ten identified fee controls are added; there are no
new fee interactions or midpoint-based continuous prices.

All 2,481 estimation destination schools match, preserving 273,184 rankings
and 969,377 school alternatives. Previous attributes and normalization are
unchanged. Reuse saved baseline models when the full sample is identical;
fit only the ten new fee specifications. Hashes check that prior inputs,
models and reports remain unchanged.

Run from the project root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/27_build_directory_prices_2024.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/28_fit_directory_prices_2024.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/30_verify_directory_prices_2024.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/29_report_directory_prices_2024.R
```

Derived inputs, models, covariance matrices, support and checks are in
`data/clean/school_preference_va/directory_prices_2024/`. Aggregate tables
are in `output/tables/school_preference_va/directory_prices_2024/`, with
report `output/reports/school_preference_va/with_directory_prices_2024_full.html`.
Coefficients compare reported fee bands with free schools conditional on
other attributes and the submitted list. The 2024 snapshot follows choices
in 2018-2020 and need not measure each household's net payment; do not
interpret these as causal price elasticities or peso-valued willingness to
pay. Full specification rationale is in the project-memory decision dated
2026-10-21.

Current pooled monthly-band coefficients (relative to free) are -0.2223
(SE 0.0185), -0.0895 (0.0083), -0.0590 (0.0078) and -0.1622 (0.0115) for
CLP 10,001-25,000, 25,001-50,000, 50,001-100,000 and above 100,000.
These categorical associations are not monotone in the fee range. The four
VA patterns remain similar: the largest absolute total-slope movement across
school types is 0.0069; non-TP exam-taking VA moves from 0.1110 to 0.1081.
All ten new fits converge in three or four iterations. All 144 estimation
checks and 245 independent checks pass, including the student-clustered
covariance reconstruction and unchanged prior-output hashes.

## PAES M1 school-average extension (2026-10-15)

The subsequent user request adds a constructed school-average PAES M1
performance measure alongside the four existing VAs. The separate extension
uses paes_common.R and scripts13/14/16/15 below; the previous four-VA models,
inputs, tables and reports remain available unchanged.

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/13_build_paes_m1.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/14_fit_paes_m1.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/16_verify_paes_m1.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/15_report_paes_m1.R
```

Construct school means directly from the raw2024 and2025 Inscritos-Puntajes
files, reading selected columns only after checking their230/234MB sizes.
Use current graduates (ANYO_DE_EGRESO=ANYO_PROCESO-1), positive graduation RBD
and a valid100-1000 MATE1_REG_ACTUAL score. Each student contributes once.
Do not restrict to our SAE/VA sample or impose a GPA requirement. This is the
current regular-sitting mean among M1 takers, not MATE1_MAX and not the mean
for every pupil enrolled at a school. Retain taker counts and score SDs.

The primary regressor uses admission process2025: graduates of2024 taking
the regular exam in late2024. Process2024 is constructed for a coverage
comparison only, with no cross-year fallback or pooling. These are later
school-performance measures relative to2018-2020 SAE applications; their
associations cannot establish that parents knew these exact future scores.

Retain the original four VA normalization parameters unchanged. Center the
new M1 mean and divide by its N-1 SD across unique destination RBDs in the
final pooled complete-ranking sample, giving every school equal weight.
Use this M1 scaling in every group and both distance specifications. If any
listed school lacks the primary M1 mean, exclude its entire ranking. Do not
shorten lists or impute scores. Apply no arbitrary minimum-taker restriction.

The extended utility includes all five standardized metrics, two type main
effects, all ten type-by-metric products and logged distance:18 coefficients.
Report15 total metric slopes across the three school types using exact
student-clustered contrast SEs. Type intercepts refer to all five metrics=0.
For comparison, refit the previous15-coefficient specification on exactly
the same retained lists (matched_baseline). Both variants use published
coordinates and home-comuna centers in the pooled and four applicant-group
samples,20 fitted objects total. These are associations conditional on the
submitted school set. Inference treats school means and EB VAs as fixed.

Derived inputs, school means, source manifests, normalization, sample flow,
models and diagnostics are under data/clean/school_preference_va/paes_m1.
New table fragments/companion CSVs are under output/tables/school_preference_va/
paes_m1. The new report is output/reports/school_preference_va/with_paes_m1.html.
Independent validation reconstructs the school means, list exclusions,
products, full-covariance total slopes and exact likelihood; expanded
conditional logits check the18/15-term models on complete validation lists.
prior_outputs_preservation.csv records hashes of26 prior artifacts.

Primary construction retains273516 applicants,970924 school pairs and2483
schools;2677 baseline rankings(0.969%) lose at least one school's2025 M1
mean and are excluded intact. The four group sizes are86353/153798/15153/
13646. New M1 normalization is585.7118 points with school SD60.02695.
Its pooled main total slopes are non-TP.1541(.0038),mixed.2066(.0060),
TP-only.1213(.0099). Math-VA slopes are negative conditional on M1; the
report compares them with the same-sample baseline. All30 build checks pass.
All20 fits converge in three iterations and pass236 fit checks. Independent
verification passes1187 checks, including fresh raw-score extraction,
330 coefficient rows,270 total metric slopes/full-covariance SEs and
expanded conditional-logit equivalence. Maximum coefficient/covariance
differences are below6e-15; full PL likelihood discrepancies are below3e-7.
All26 protected original inputs/models/tables/reports remain byte-identical.
The report generation passes162 additional consistency checks and exports
the pooled and subgroup total slopes, raw coefficients and matched changes.

## SIMCE parent-education preference extension (2026-10-15)

Add school-average parent education from the 2022 second-year-high-school
SIMCE parent questionnaire to the PAES-M1 joint preference model. This cohort
provides direct destination-school RBDs and covers nearly all schools in the
2018-2020 SAE destination support. The measure is observed after those choices,
so its coefficient is a descriptive association and does not establish what
families knew at application time.

The parent file has one record per SIMCE student. `cpad_p04` reports mother (or
father's partner) education and `cpad_p05` reports father (or mother's partner)
education. Valid codes1-20 are converted to approximate years following the
2022 questionnaire categories; empty,0,21 (don't know),99 (double mark) and
other values are missing. For each family, average the available parent
reports; then take the mean of family averages at each RBD. A family with one
valid parent report contributes that one report. The school measure is
standardized with equal weight per unique destination RBD in the final pooled
sample. If any school in a submitted ranking lacks a mean, exclude the whole
ranking; never shorten the submitted list.

Run the build from the SIMCE2022 source directory because its Box path has an
accented folder name that older R installations may not resolve from the
project root. Adjust the two local paths below if the project or Box location
differs:

```powershell
Push-Location -LiteralPath 'C:/Users/brunem/Box/causal_schools/data/raw/simce/Simce segundo medio 2022 - Versión privada'
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' 'C:/Users/brunem/Research/causal_schools/code/codex/school_preference_va/19_build_parent_education_2022.R'
Pop-Location
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/20_fit_parent_education_2022.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/22_verify_parent_education_2022.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/21_report_parent_education_2022.R
```

The extension adds six school attributes (four EB VAs, PAES M1 and parent
education), both school-type indicators, all12 type-by-attribute interactions,
and distance. Fit the extended model and a five-attribute model on the same
sample, for the pooled sample and all four priority-by-continuation groups,
using both student-coordinate and comuna-centroid distances. Inference clusters
by student. Derived data, normalization, source manifest,20 fitted objects,
covariances, sample flow, tables and checks are under
`data/clean/school_preference_va/parent_education_2022/` and
`output/tables/school_preference_va/parent_education_2022/`. The report is
`output/reports/school_preference_va/parent_education_2022.html`.

Current run: SIMCE2022 provides a valid mean for2,481 of2,483 PAES-M1
destination schools. Whole-list filtering retains273,184 of273,516 applicants
(969,377 applicant-school pairs). The school-level mean is11.6998 years and
its equal-school standard deviation is1.4187 years. In the pooled model with
published student coordinates, total parent-education slopes are
-0.0390 (0.0046) for non-TP,-0.1075 (0.0072) for mixed HC/TP and-0.0679
(0.0117) for TP-only schools. Average parent education correlates0.710 with
PAES M1 across destination schools; the model includes both jointly. All20 fits
converged in three iterations; independent verification and report checks
passed. The main and comuna results, subgroup slopes and changes in existing
attribute slopes are in the report and companion tables.

## Existing grade-4-SIMCE parent-income-decile extension (2026-10-15)

The subsequent request specifies reusing the previously constructed
`income_decile_imputed` measure based on 4th-grade SIMCE parent-survey data.
Use the existing school composition table
`data/clean/staff_lasso_va/school_student_composition.csv`, not a new 2022
2nd-year-high-school income response. Its `income_decile_mean` is the average
of the existing observed/imputed student deciles for 2017-2020 grade-8
students attending each RBD. The school composition measure is retrospective
for the SAE 2018-2020 applicant cohort and should be interpreted descriptively.

The new model jointly includes the four EB VAs, PAES M1, parent education,
parent income decile, two school-type indicators and all 14 attribute-by-type
interactions, plus distance. Compare it with the existing six-attribute
parent-education model on identical full submitted lists, for pooled and four
priority-by-continuation groups under published coordinates and comuna
centroids. The 273,184 parent-education-complete lists all have the income
measure; no additional applicants are lost. The pooled estimation sample has
969,377 applicant-school pairs and 2,481 destination schools.

Run from the project root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/23_build_parent_income_decile_4to.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/24_fit_parent_income_decile_4to.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/26_verify_parent_income_decile_4to.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/25_report_parent_income_decile_4to.R
```

Outputs are under `data/clean/school_preference_va/parent_income_decile_4to/`
and `output/tables/school_preference_va/parent_income_decile_4to/`; the report
is `output/reports/school_preference_va/parent_income_decile_4to.html`. The
school-income-decile total slopes in the pooled published-coordinate model
are -0.0796 (SE 0.0076) for non-TP schools, 0.0176 (0.0117) for mixed HC/TP,
and -0.0262 (0.0153) for TP-only. These conditional associations include
parent education and PAES M1. All 20 fits converge in 3 iterations; the
independent model and data checks pass.

## Original four-VA specification and construction

The preceding user specification is math, exam-taking, high-premium-field and
high-premium-institution VA, entered jointly with mixed-HC/TP and TP-only
school indicators, all eight school-type-by-VA interactions and
log(1+distance in km). The latest 2026-10-15 request allows all four VA slopes
to differ across non-TP, mixed-HC/TP and TP-only schools.
Estimate two versions: published student coordinates and home-comuna centers.
School-campus coordinates, ranking sample and VA normalization are identical
between the two models. The requested priority-by-continuation splits are
documented below; no other demand specification is included.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/01_build_rankings.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/08_top_school_types.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/09_attach_school_types.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/02_fit_rank_logit.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/04_verify.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/03_report.R
```

Dependencies: data.table and survival. Set CAUSAL_SCHOOLS_DATA_WD to override
the default Box data root. Raw data, existing clean source files and the
paper manuscript are read-only. New person-level files stay under the
Git-ignored data/clean/school_preference_va directory.

For the existing constructed data, skip01: run08 first for the requested
top-school distributions, then09 to append the new school-type columns to
all ranking objects without rebuilding raw applications or VAs. Process the
71MB school and69MB program RDS files sequentially; only the minimal27MB
estimation input is needed for fitting. Existing values and ranks are kept.
For the interaction update on already typed data, start directly at02; the
eight product columns are formed in memory by add_va_interactions(). No raw
application or VA reconstruction is necessary.05 saves the product columns
alongside the unchanged subgroup inputs.

## Rankings and sample

Use the existing Table 2 roster of 294,198 timely grade-9 applicants in
2018-2020, with and without assignment risk. Reconstruct regular-round C1
submitted lists. Remove auto-appended continuity alternatives; retain genuine
priority applications. The old cleaned sae_apps object drops priority choices
and the unique-process cache lacks continuity flags, so neither can replace C1.

Preserve all 1,013,909 original program submissions. Collapse an RBD to its
first submitted program and renumber distinct-school ranks, retaining that
program's campus. The full data have 1,007,253 student-school pairs. Flag the
4,875 lists with repeated schools and the 278 with interleaved A/B/A programs.
This school-level aggregation was explicitly selected by the user.

Keep all original lists in the full data with coverage and eligibility flags.
Estimate only whole lists with all four selected VAs, both observed school-type
indicators, both finite distances,
and at least two distinct schools. A missing alternative excludes the entire
list. Single-school lists have likelihood one and provide no ordering signal.
Coverage and normalization are recomputed for the selected four VAs; they
do not inherit the initial seven-VA sample restriction.

## Existing value added and normalization

Use the current paper's All-sample, pooled grade-8-cohort 2017-2020 EB file
under Box/data/clean/empirical_bayes_school_va/cohorts_2017_2020. Its
controlled_value_added_eb_centered_student column supplies school indices for
z_year_math_max, admission_exam_taker, high_paying_field_m1 and high_inst_m1.
Do not substitute legacy Stata exports or reconstruct VA estimates.

The source estimation populations differ: math uses observed exam scores;
institution/field VA uses exam takers; exam-taking VA uses the broader
complete-control sample. Retain these existing outcome definitions.

Demand regressors subtract the mean and divide by sample SD (N-1) across
unique destination RBDs in the final common estimation support, pooled across
years. Each school has equal normalization weight; list length/application
frequency do not determine moments. Save means/SDs and reuse them unchanged
in both models. Preserve raw EB values alongside normalized regressors.

## Secondary school-type indicators

tp_school is a raw, unstandardized 0/1 school-level indicator constructed from
any ENS code in {410,510,610,710,810} in the raw 2024 Directorio. The builder
also verifies exact agreement with the paper's school__has_tp_or_artistic
flag minus the separately identified artistic-school indicator. All inputs
are recorded in the manifest. Mixed HC/TP schools receive 1; pure artistic
schools receive 0.
This means the school offers a regular TP secondary track, not that the
student's particular grade-9 program or eventual track is TP. It is a fixed
2024 classification, consistent with the prior paper tables, rather than a
claim about every school's offerings in its application year.

The single tp_school flag remains metadata. Current models enter two raw0/1
indicators: mixed_hc_tp means both HC(ENS310) and TP; tp_only means TP without
HC. No active-status filter is applied to historical SAE destination schools.
All original schools match the2024 source, preserving the sample and pooled
VA normalization. The omitted category is non-TP, not exclusively HC-only:
1557HC-only,1artistic-only and16other/no-regular-secondary schools among2501
estimation destinations. Mixed and TP-only counts are690 and237. The latter
labels concern secondary offerings and allow lower grades in the school.
school_types.R defines the classification shared by construction, attachment
and top-school reporting. school_type_classification.csv retains every RBD;
school_type_coverage.csv records destination and alternative counts.
tp_coverage.csv records descriptive coverage of the older any-TP flag; the direct
directory comparison is saved as tp_directory_verification.csv. Missing TP
values are never implicitly zero-coded. In the existing estimation sample,
all 2501 schools are classified, 927 are TP, and 690 of those are mixed HC/TP.

## Geography

All published SAE student geography types enter, including quality4 municipality
imputations (59,976 original applicants, 20.4%). Published points are privacy
displaced: in 2018 each coordinate receives uniform +/-0.0013-degree noise;
in 2019-2020 a random-direction displacement of 50-300m is used. These are
approximate declared locations, not exact home addresses.

Attach school coordinates by application year/RBD/program using A1 regular
offers. Campuses can differ by over20km, so do not use an arbitrary RBD row.
The robustness replaces only the home point with the grade-8 residence-comuna
center from COD_COM_ALU in universe_controls.csv and comuna_info.RDS. All
applicants match numeric codes. Code1401 is wrongly labeled TOCOPILLA in the
lookup although its code/center identify Pozo Almonte; actual Tocopilla is2301.
Join numeric codes, retain official residence names, and document the label
error rather than changing raw/existing clean data. No directory fallback or
external geocoding is needed.

Use Haversine straight-line km, mean Earth radius6371.0088km, then log1p.
Keep zero and genuine long distances; these are not travel distances.

## Exact model and inference

The likelihood conditions on schools in the submitted list, with iid Type-I
extreme-value utility shocks. Use survival::coxph with rank_school as event
time, event=1 for every school, strata(ranking_id), and cluster=mrun. At rank r
the risk set is exactly the remaining listed schools. Unique ranks imply no
within-stratum event-time ties, so ties='breslow' is the exact specified
Plackett-Luce likelihood. The final singleton factor is one.

Four normalized VAs, two school-type indicators, their eight pairwise
type-by-VA products and one distance term enter: fifteen coefficients,
no intercept, school fixed effects, peer controls or unlisted alternatives.
Writing M and T for mixed and TP-only indicators and z for the four VAs,
utility is beta'z + alpha_M M + alpha_T T + delta_M'(M*z) +
delta_T'(T*z) + gamma*log1p(distance). The beta slopes apply to non-TP;
mixed slopes are beta+delta_M and TP-only slopes are beta+delta_T. Report
these totals with SEs from the full student-clustered covariance, including
the covariance of each main coefficient and its interaction. Type intercepts
apply only at z=0 in all four dimensions; the type premium varies with VA.
The sandwich covariance clusters full ranking scores by student. Inference
conditions on existing EB estimates and does not propagate their estimation
uncertainty. Interpret slopes as conditional ranking associations, not causal
VA information effects, beliefs, consideration-set inclusion or enrollment.

## Outputs and checks

- program_rankings.rds preserves each submitted program, original rank,
  priority, campus, geography, raw/normalized selected VAs and list flags.
- student_school_rankings.rds contains every collapsed school alternative,
  including excluded lists. rank_school is consecutive; rank_program retains
  the original first program position. eligible_estimation and
  estimation_eligible are identical list-level flags.
- estimation_data.rds is the common minimal model dataset; ranking_roster.csv
  contains one row per original list with coverage and exclusion reasons.
- CSVs retain sources, sample flow/attrition, VA normalization/correlation,
  geographic precision/campus differences, coefficients/covariances and
  numerical diagnostics. models/main.rds and models/municipality.rds are
  bare coxph fits with their actual formulas and preference-model metadata.
- type_specific_va_slopes.csv saves all12 total slopes per distance model
  with full-covariance SEs, confidence intervals and contrast definitions.
- The report is output/reports/school_preference_va/joint_rank_ordered_logit.html;
  the two-column table is output/tables/school_preference_va/
  joint_rank_ordered_logit.tex (fragment) and .csv. A companion
  type_specific_va_slopes.tex/.csv reports total slopes. No manuscript is edited.

Validation checks complete-list integrity, all joins, shared samples, saved
normalization, within-ranking design rank and likelihood information, final
scores and convergence. An independent full-sample PL likelihood and an
expanded conditional logit on deterministic complete validation lists check
point estimates and student-clustered covariance. The initial seven-VA fit was
superseded by the user's four-VA request; its local snapshot is in the ignored
clean-data archive/seven_va_initial folder and is not the reported model.

Methods decision: setup_md_codex/project_memory/decisions/
2026-10-14-school-preference-va.md.

## Current type-by-VA interaction results

All ten fits converge in three iterations with all15 coefficients identified.
Samples and pooled VA normalization are unchanged. Pooled main total slopes
(student-clustered SEs), in math/exam/field/institution order, are:

- Non-TP: .0510(.0021), .1535(.0043), .0114(.0031), -.0083(.0031).
- Mixed HC/TP: .0576(.0050), .0729(.0032), .0626(.0021), -.0573(.0041).
- TP-only: -.0063(.0087), .0256(.0047), .0578(.0024), -.0229(.0044).

Exam-taking VA has the largest pooled slope among non-TP schools; field VA
has substantially larger slopes among mixed and TP-only schools. The
TP-only math slope is close to zero. These remain conditional associations.
At all four VAs equal to their pooled-school means, mixed/TP-only intercepts
are .3387/.3850; logged distance is -.1912. Comuna-based location robustness
is reported beside the main fit. The archival additive split-type results
are in archive/split_types_no_va_interactions_2026_10_15, with79 hash-verified
files and the prior inputs, fits and aggregate reports.

All141 pooled and477 subgroup validation checks pass. These include all
eight products, all15 coefficients, independent PL likelihood, expanded
conditional-logit equivalence and120 total-slope contrasts including their
full student-clustered covariance. Expanded-fit coefficient and covariance
differences are below7e-15; the maximum full-sample PL discrepancy is2.73e-7.
Eight archive-comparison checks also confirm unchanged input values, row
ordering, samples and normalization. Saved checks:
interaction_update_preservation_checks.csv.

## Archived additive split-type results (superseded)

All ten fits converge in three iterations. The pooled main coefficients
(student-clustered SEs) are math0.0462(0.0019), exam0.0791(0.0023),
field0.0563(0.0014), institution-0.0220(0.0030), mixedHC/TP0.2892(0.0048),
TP-only0.4323(0.0065), logged distance-0.1894(0.0031). Both type indicators
have positive coefficients, with a larger TP-only point estimate in each
group. The pooled comuna estimates are mixed0.2916(0.0048) and
TP-only0.4056(0.0064). These are conditional ranking associations.

Pooled independent validation passes67 checks; subgroup validation195.
Expanded conditional-logit coefficients and student-clustered covariances
agree to machine precision, and full-sample likelihood discrepancies are
below7e-8. Archive comparisons confirm every pre-existing input value,
sample count and VA normalization parameter is preserved.

## Archived four-VA run without TP (superseded)

This previous specification is saved under
archive/four_va_no_tp_before_split_2026_10_15, with model inputs and copy hashes.

Removing TP preserves the estimation data and normalization byte for byte.
All ten pooled/subgroup fits converge in three iterations with five terms.
The pooled main slopes (student-clustered SEs) are math0.0156(0.0018),
exam taking-0.0180(0.0019), field0.0571(0.0014), institution-0.0082(0.0030)
and log distance-0.1815(0.0031). The pooled sample remains276193 rankings,
985285 school pairs and2501 schools. Pooled validation passes59 checks;
subgroup validation passes187, including explicit absence of TP from all
formulas and coefficients. Expanded-logit coefficients and clustered
covariances agree to machine precision; full likelihood errors are below3e-7.

The school-ending prioritario subgroup has negative math/exam slopes;
exam-taking VA is not distinguished from zero at5% in either voluntary
group. Institution VA remains negative for ending groups and positive for
voluntary groups; field VA remains positive in all four main fits. These
are conditional ordering associations.

## Archived four-VA plus TP run (superseded)

The following historical results are preserved in the ignored clean-data
archive/four_va_with_tp_2026_10_15 folder, with a manifest, fitted models,
their estimation inputs and aggregate reports. Active outputs instead use
separate mixed/TP-only indicators and their VA interactions.

Both distance versions use 276,193 rankings, 985,285 school pairs and 2,501
destination schools. Recomputed four-VA coverage retains 292,887 of 294,198
original lists (99.55%); the informative sample happens to match the earlier
seven-VA sample. Exclusive attrition is 16,720 singleton lists and 1,285
informative lists missing VA, with no geographic loss.

Main coefficients (student-clustered SEs): math 0.0429 (0.0019), exam taking
0.0663 (0.0023), high-premium field 0.0581 (0.0014), high-premium institution
-0.0178 (0.0030), TP 0.3167 (0.0047), log distance -0.1837 (0.0031).
The comuna-distance fit preserves all VA and TP signs; TP is 0.3135 (0.0047)
and logged distance -0.0640 (0.0057). Relative to the saved no-TP model,
exam-taking VA changes from -0.0180 to +0.0663. These remain conditional
within-submitted-set associations. TP's stage-choice odds ratio is 1.3726,
conditional on the four VAs and distance; it is not an enrollment effect.

Both full fits converge in three iterations; all 56 independent model checks
pass. On 1,393 validation rankings
(9,207 alternatives and 66,199 expanded
rows), direct Cox and expanded conditional-logit coefficients/covariances
agree to machine precision. Independent full-sample log likelihood errors
are below 5e-7. The report also includes all data-build and convergence checks.

## Prioritario by continuation groups (2026-10-15)

The subsequent request fits the same fifteen-regressor joint model separately
for prioritario/non-prioritario crossed with school-ending/voluntary
application. Run after the pooled data construction:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/05_fit_by_priority_continuation.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/07_verify_groups.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/06_report_groups.R
```

Use the existing paper flags in all_applicant_expected_va.rds and the
prioritario_sae_2018_2020.csv cache, joined uniquely by mrun/sae_proceso.
Prioritario is the regular-round B1 low-income-priority flag, not sibling or
incumbent application priority. School ending means no SAE offering above
grade 8 at the year-end grade-8 origin; voluntary means that origin offers
grade 9. These categories do not require an observed school move. Both
assignment-risk statuses enter.

The four groups cover 289,432 original applicants and 271,597 informative,
complete rankings. Respectively, final counts are 87,407 school-ending
non-prioritario, 155,028 school-ending prioritario, 15,363 voluntary
non-prioritario and 13,799 voluntary prioritario. Exclude the unresolved/other
continuation category: 4,766 original applicants, including 4,596 otherwise
eligible rankings. Do not assign this category to a substantive group.
Singleton and missing-regressor losses are recorded by group and year.

Reuse the pooled four-VA standardized columns without restandardizing on
group-specific school support. Save an identical copy of the pooled mean/SD
parameters. Whole-list eligibility, both geographic definitions,
and student-clustered inference are unchanged. Estimate both distances
within every group, with identical applicants and lists. Separate logits
normalize unobserved utility errors separately, so cross-group coefficient
differences can reflect scale as well as preferences.

Derived data, membership roster, manifests, diagnostics, model objects and
covariance matrices are saved under data/clean/school_preference_va/
by_priority_continuation. Existing pooled data/results are unchanged.
Independent verification checks membership and complete-list preservation,
fixed normalization, exact PL likelihood, and expanded conditional-logit
coefficient/student-clustered-covariance equivalence on small validation
samples from every group and for both distance specifications.

Readable report: output/reports/school_preference_va/by_priority_continuation.html.
Four-column table fragments: output/tables/school_preference_va/
by_priority_continuation_main.tex and by_priority_continuation_municipality.tex;
the combined numeric/display table is by_priority_continuation.csv.

Every current model includes mixed_hc_tp and tp_only and their eight VA
interactions, with non-TP as the reference. The previous additive, any-TP and
no-TP subgroup fits are preserved in their dated archives. Current inputs
retain every pre-existing value, row and normalization parameter. Companion
by_priority_continuation_type_slopes CSV and main/municipality
LaTeX panels report12 total slopes within each of the four applicant groups.
Current estimates and validation results are generated in the active tables
and report.

## School types among most demanded destinations

08_top_school_types.R uses the exact previously reported top100 ranking from
output/tables/applicant_priority_comparison/top_100_sae_destination_schools.csv;
it does not rerank using the newly reconstructed preference lists. Attach the
same2024 directory classification by unique numeric RBD. School shares use
N=10,20,50,100. Enrollment shares use MAT_TOTAL within each selected topN;
this is whole-establishment enrollment across all grades, not HS enrollment
or enrollment in a particular track. No school is dropped or recategorized
using2024 activity status. All100 schools match.

HC-only/mixed/TP-only counts are4/2/4(top10),10/6/4(top20),18/15/17(top50),
41/31/28(top100). No artistic/other schools appear in these sets. Readable
count, school-share and enrollment-share tables are in
output/reports/school_preference_va/top_school_types.html; CSVs and manifests
are in the task clean directory, with a companion aggregate table CSV in
output/tables/school_preference_va/top_school_type_distribution.csv.

## Enrollment-weighted average VA by type

10_average_va_by_school_type.R implements the subsequent descriptive request.
The user explicitly chose2024 Directorio enrollment weights, not the number
of SAE applicants attending each school. Primary means use national active,
enrolled regular secondary schools, including private-paid schools, with
all four selected EB indices observed. MAT_TOTAL includes every grade in a
school, not secondary-only or track-specific pupils. Use the same common
four-VA-complete population for every outcome, and show coverage by type.

Raw means retain original units; also report unchanged pooled preference-
school-SD units using va_normalization.csv. Do not restandardize within type
or using enrollment weights. A separate appendix restricts the same schools
to preference-model destination RBDs; it is not the primary national result.
There are2050HC-only,694mixed,237TP-only and2artistic schools in the primary
complete population, covering2234310pupils across all grades. The type-level
means agree with an independent computation. No regression is refitted.

Run Rscript code/codex/school_preference_va/10_average_va_by_school_type.R.
Report:output/reports/school_preference_va/enrollment_weighted_va_by_school_type.html.
Clean-data CSVs retain normalized/raw means, model-support comparison,
coverage, source manifest and verification. The aggregate table companion
CSV is in the task output/tables folder.

## Four overlapping VA distributions

11_plot_va_distributions.R draws one enrollment-weighted empirical CDF per
school type and dimension, using the exact same national complete-school
sample,2024MAT_TOTALweights and pooled VA units as10. Tied VAs aggregate
enrollment first; the CDF is cumulative mass at or below each value. No
smoothing, bandwidth, interpolation or tail trimming is used. Artistic-only
has two schools, so its two discrete jumps are shown explicitly with markers.

The combined2x2figure and four individual PNGs are saved under
output/figures/school_preference_va/, with a combined vector PDF. The main
stem is enrollment_weighted_va_cdfs_by_school_type. The report is
output/reports/school_preference_va/va_distributions_by_school_type.html.
Exact curve points, inverse-CDFp10/p25/p50/p75/p90, school inputs, source
manifest and checks are under the task clean-data folder. Validation
reconstructs the preceding means/denominators and verifies all16curves
are monotone with unit total enrollment mass. Figures are visually checked.
An independent reconstruction matches all11932school-dimension records,
16CDFs and80weighted quantiles to numerical precision.

## Current density plots, excluding artistic schools

The user corrected the requested figure to densities and dropped artistic
schools. Run12_plot_va_densities.R after11 to reuse the saved school inputs.
The active distribution report now displays three overlapping Gaussian KDEs
per dimension. Retain HC-only,mixedHC/TP andTP-only:2981schools/2232785
all-grade pupils. Nothing changes in the preference models or mean tables.

Enrollment weights normalize to one within each type. A single bandwidth
per dimension is shared by all three types, using the weighted Silverman
rule .9*min(weightedSD,weightedIQR/1.34)*n_eff^(-1/5), where
n_eff=(sumw)^2/sum(w^2). Shared bandwidths(math,exam,field,institution) are
.228131,.145071,.166288,.206373. Direct Gaussian mixtures use4096grid
points over the full observed support plus5bandwidths in each tail.
Each curve integrates numerically to one; no data trimming occurs.

Current PNG/PDF stem:output/figures/school_preference_va/
enrollment_weighted_va_densities_by_school_type. Four individual PNGs use
enrollment_weighted_va_density_<suffix>.png. Clean CSVs retain density
points, bandwidths, diagnostics, manifest and checks. Earlier CDF exports
remain available as historical outputs; the active report links the densityPDF.
