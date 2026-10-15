# School preferences and four-dimensional EB value added

The current user specification is math, exam-taking, high-premium-field and
high-premium-institution VA, entered jointly with log(1+distance in km).
The 2026-10-15 request removes TP from the pooled and subgroup regressions.
Estimate two versions: published student coordinates and home-comuna centers.
School-campus coordinates, ranking sample and VA normalization are identical
between the two models. The requested priority-by-continuation splits are
documented below; no other demand specification is included.

Run from the repository root:

```powershell
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/01_build_rankings.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/02_fit_rank_logit.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/04_verify.R
& 'C:/Program Files/R/R-4.5.1/bin/Rscript.exe' code/codex/school_preference_va/03_report.R
```

Dependencies: data.table and survival. Set CAUSAL_SCHOOLS_DATA_WD to override
the default Box data root. Raw data, existing clean source files and the
paper manuscript are read-only. New person-level files stay under the
Git-ignored data/clean/school_preference_va directory.

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
Estimate only whole lists with all four selected VAs, both finite distances,
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

## Retained Tecnico Profesional metadata

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

The indicator is retained as metadata but no longer enters any current model
or defines eligibility. All original schools are classified, so removing the
TP regressor preserves the estimation sample and VA normalization exactly.
tp_coverage.csv records descriptive coverage; the direct
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

Four normalized VAs and one distance term enter: five coefficients,
no intercept, school fixed effects, peer controls or unlisted alternatives.
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
- The report is output/reports/school_preference_va/joint_rank_ordered_logit.html;
  the two-column table is output/tables/school_preference_va/
  joint_rank_ordered_logit.tex (fragment) and .csv. No manuscript is edited.

Validation checks complete-list integrity, all joins, shared samples, saved
normalization, within-ranking design rank and likelihood information, final
scores and convergence. An independent full-sample PL likelihood and an
expanded conditional logit on deterministic complete validation lists check
point estimates and student-clustered covariance. The initial seven-VA fit was
superseded by the user's four-VA request; its local snapshot is in the ignored
clean-data archive/seven_va_initial folder and is not the reported model.

Methods decision: setup_md_codex/project_memory/decisions/
2026-10-14-school-preference-va.md.

## Current four-VA run without TP

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
their estimation inputs and aggregate reports. Active outputs omit TP.

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

The subsequent request fits the same five-regressor joint model separately
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

Every current model omits TP. The previous subgroup models with TP are
preserved in the same dated archive as the pooled models. The current
estimation inputs and fixed normalization are identical to that archived run;
only the regression specification changes. Current estimates and validation
results are generated in the active tables and report.
