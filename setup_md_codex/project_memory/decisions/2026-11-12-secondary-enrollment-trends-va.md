# Secondary enrollment trends before and after SAE by VA

## Question and scope

The user asked whether the growth trajectory precedes SAE and whether its
trend changes for high-VA schools, permitting student counts. This follows
the 2021-2025 declared-capacity/VA exercise. Observed enrollment is a distinct
quantity: it permits a pre-SAE history but is not capacity or vacancies.

## Data and population

- Use official MINEDUC Resumen de matricula por curso archives, 2010-2025,
  annual April-30 SIGE snapshots. Classroom aggregates avoid scanning roughly
  8 GB of student-level records. All new sources/outputs stay in the task's
  clean-data directory; existing raw data remain untouched.
- Sum N_ALU for regular youth-secondary teaching codes 310,410,510,610,710,
  810,910. Exclude adult/special education. Use active schools: before 2015
  the release is active-only; thereafter require ESTADO_ESTAB=1. Simple
  classrooms have grades 1-4; combined classrooms use N_ALU_GRADO1-4.
- Assert unique full classroom keys, nonnegative counts, grade reconciliation,
  and gender reconciliation within the target population. N_ALU_NB appears
  from 2024 and must enter gender totals. Missing 2014 gender detail outside
  secondary is audited, not imputed or used to replace official N_ALU.
- Universe: 2,621 RBDs with positive secondary capacity in any 2016-2025 A1.
  Require positive secondary enrollment in each of the three pre-entry years:
  2,419 incumbents. Fixed sample and VA groups prevent mechanical composition
  changes after adoption. Missing later regular-secondary records are zero
  reported enrollment, not legal closures. Post-baseline openings are omitted;
  conditioning on eventual SAE participation excludes some pre-SAE exits.
- Match current All-sample student-centered observational EB VA from grade-8
  cohorts 2017-2020. Math, field, institution N=2,398; exam-taking N=2,404.
  These estimates are retrospective and overlap SAE; they are neither a
  pre-SAE information set nor school-specific lottery-IV effects.

## Timing, normalization, and specification

- Use first grade-9 SAE entry cohort: Magallanes 2017; Tarapaca, Coquimbo,
  O'Higgins, Los Lagos 2018; other non-Metropolitana regions 2019; Metropolitana
  2020. Process year is one earlier. Assert against first positive-capacity
  regional A1 process. Later full-grade rollout is not a different grade-9
  treatment date. Merge Nuble into historical Biobio (15 regional clusters).
- School index: 100*N_st / mean(N_s,-3:N_s,-1). Retains exits/zeros and
  avoids the survivor selection inherent in logs. Units are percentage points
  of baseline enrollment, not compound percentage growth rates.
- High VA: >= 75th percentile among matched incumbent schools, separately
  by outcome, with equal school weights. Keep classifications fixed in every
  year and robustness sample. Continuous VA uses its school-level SD in the
  same initial matched sample.
- Common event window -5 through +5. Regress the index on high*event_time,
  high*post, high*max(event_time,0), school FE and region-calendar-year FE.
  High*event_time is the differential pre-slope; adding high*hinge gives the
  post-slope. A separate high*post term permits a level jump.
- Main OLS gives each school equal weight. Cluster actual regression scores
  by region; fixest default small-sample correction, df.K=4 (three slopes and
  one intercept; FE are nested within clusters). CIs/p-values use t(14).
  Post-slope SE uses its exact linear-combination covariance. Save actual
  fitted regression objects, not approximated standard errors.
- Also estimate high*event-year coefficients with -1 reference and the same
  FE/clustering. Plots of raw normalized group means are distinct from these
  adjusted estimates. Calendar-year series retain 2010-2025 coverage.
- Grade 9 is a separate outcome and requires positive grade-9 counts in all
  three baseline years. It keeps the same VA cutoffs from the secondary sample.
- Sensitivities: exclude 2020/2021; baseline secondary >=100; baseline-enrollment
  weighted WLS; continuous VA; positive 2021 SAE capacity; positive enrollment
  in all five pre-SAE years. Four-outcome BH p-values are supplied within each
  contrast/metric/specification, not used to choose the reported specification.

## Results and limits

Main high-minus-other differential slopes (index points/year):

| VA | Before | After | Change | Change 95% CI |
|---|---:|---:|---:|---|
| Math | 1.854 | 0.282 | -1.571 | [-2.399,-0.744] |
| Exam taking | 3.114 | -0.515 | -3.630 | [-4.687,-2.572] |
| High-premium field | 0.246 | -0.559 | -0.805 | [-1.535,-0.076] |
| High-premium institution | -0.027 | -0.425 | -0.398 | [-1.605,0.810] |

Math/exam growth advantages predate SAE and weaken afterward, robust to all
listed sensitivities. Both groups' raw normalized enrollment increases after
rollout; the relative advantage narrows. Grade-9 slope changes are -2.430 and
-4.265, respectively. Field evidence is less robust (continuous VA and some
weighted/pandemic checks include zero); institution has no clear break.
The five-pre-year incumbent check gives math change -1.209 and exam -3.358,
both with negative CIs, so recent openings do not account for these findings.

These comparisons cannot identify a causal SAE response. VA is retrospective;
pre-trends differ, calendar/demographic shocks may differ by VA even within
regions, and only 15 clusters support inference. The national all-school
enrollment series declines from 991,716 in 2010 to 896,547 in 2017, then reaches
1,009,664 in 2025; this broader population is not the fixed eventual-SAE sample.

## Reproduction and validation

Code 03-06 in code/codex/secondary_sae_spots downloads, constructs, analyzes,
and independently checks the result. Source URLs, archive SHA256, input MD5,
cohort selection and analysis CSVs stay under enrollment_trends. R workers run
one VA outcome at a time with explicit low thread counts, no automatic
data.table indices, and staging-object cleanup to bound memory. Compact
fixest objects retain fitted coefficients and actual clustered covariance.

Independent Python streaming aggregation matched all five student counts
for all 47,317 source school-years. An exact FWL transformation (balanced
support within region; weights fixed within school) and cluster sandwich
reproduced 56 regressions/224 contrasts. Maximum coefficient discrepancy
1.63e-11; maximum SE discrepancy 2.49e-12. Counts, sample sizes, clusters,
and all t-based contrasts were checked. Three figures were visually reviewed.

Report: output/reports/secondary_sae_spots/enrollment_trends_before_after_sae_va.html.
