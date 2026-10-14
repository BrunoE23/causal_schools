# School preferences and four-dimensional EB value added

The current user specification is math, exam-taking, high-premium-field and
high-premium-institution VA, entered jointly with log(1+distance in km).
Estimate two versions: published student coordinates and home-comuna centers.
School-campus coordinates, ranking sample and VA normalization are identical
between the two models. No other demand specification is included.

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

Only four normalized VAs and one distance term enter: five coefficients,
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

## Completed four-VA run

Both distance versions use 276,193 rankings, 985,285 school pairs and 2,501
destination schools. Recomputed four-VA coverage retains 292,887 of 294,198
original lists (99.55%); the informative sample happens to match the earlier
seven-VA sample. Exclusive attrition is 16,720 singleton lists and 1,285
informative lists missing VA, with no geographic loss.

Main coefficients (student-clustered SEs): math 0.0156 (0.0018), exam taking
-0.0180 (0.0019), high-premium field 0.0571 (0.0014), high-premium institution
-0.0082 (0.0030), log distance -0.1815 (0.0031). The comuna-distance fit
preserves all four VA signs. Its distance coefficient is -0.0589 (0.0057).
Field is the strongest positive conditional association; one SD corresponds
to a stage-choice odds ratio of 1.0588, holding other attributes fixed.

Both full fits converge in three iterations. All 54 independent model checks
pass. On 1,393 validation rankings (9,207 alternatives and 66,199 expanded
rows), direct Cox and expanded conditional-logit coefficients/covariances
agree to machine precision. Independent full-sample log likelihood errors
are below 3e-7. The report also includes all data-build and convergence checks.
