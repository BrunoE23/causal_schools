# Admission-probability-weighted applicant composition proxy

## Authorization and estimand

Bruno requested a version weighting applicant characteristics by each student's
probability of admission, explicitly accepting it as an imperfect proxy. Keep
the original top 100, 2018-2020 applicant population and genuine submitted pairs.
For each school/cohort/outcome use sum(p_is X_i) / sum(p_is) among observed X,
where p_is is the existing simulated assignment probability to that RBD, summing
across programs. This is a ratio of expected totals, not in general the expected
realized composition of a random admitted group; it also does not model take-up.

## Probability support

Read existing DA_probs files for 2018-2020. All 115,880 distinct applicants in the
original top-school panel have distributions summing to one including unmatched.
After that check, absent destination rows get zero recorded simulation weight.
Do not add 346 simulated top-school pairs absent from the original submitted
choices (combined probability mass 221.010). Do not reinterpret a missing student
distribution as zero. Original panel: 212,608 pairs; 134,426 have positive weight.

Nine populated school-cohort cells have zero total probability mass. A further
parent-education cell has zero observed probability mass. Leave these weighted
means missing. Three schools (RBD 20266, 9071 and 31103) have only one positive-mass
cohort, so no slope is identified. Keep the fixed school set and report support.
Effective N = (sum p)^2 / sum(p^2) is diagnostic; no minimum-N trimming is imposed.

## Inference

Use the same equal-cohort linear trends and unadjusted p < .05. Primary cohort
OLS has ordinary lm covariance and t(G - 2); two supported cohorts allow a slope
but no residual-based test. Supplementary student WLS uses p_is / sum(p_is) within
school/cohort/outcome and therefore gives exactly the same slope as cohort OLS.
Use actual regression HC2 covariance and t(N_positive - 2), holding estimated
probabilities fixed. This ignores simulation uncertainty, assignment dependence,
cohort shocks and origin-school dependence. Constant or undefined cases get no
artificial p-values. No multiplicity correction, reranking, reimputation or
probability truncation is introduced.

## Results

Primary cohort OLS: 80 schools have a test for at least one primary outcome;
9 have at least one significant trend, all exactly one. None have two or three.
Independent-applicant HC2: 97 have a test; 68 have at least one, 21 at least two,
and 1 all three. Exact 1/2/3 counts are 47/20/1. RBD 17788 (Colegio Amanecer) is
the all-three HC2 school. All-three p-values exist for 77 cohort-OLS schools and
91 HC2 schools; a missing test is not evidence of no trend.

Compared with unweighted counts 8/86 and 81/100 with at least one trend, the
weighted version changes both composition and supported time windows. Always
show eligible denominators. Some cells have effective N near one; their results
are retained and explicitly flagged in the report, not silently trimmed.

## Reproduction and verification

Scripts 06/02/08/07 under `code/codex/applicant_composition_trends/` build, fit,
verify and report the weighted version. Set APPLICANT_COMPOSITION_DIR to the
probability_weighted clean-data subfolder only when running the weighted fitter.
The default fitter still reproduces all 1,000 original records and summary counts.
The weighted audit passed 1,828 checks, including explicit matrix verification of
all 964 estimable HC2 regressions. The report's ten outcome tables, school search,
and local links also pass. Original unweighted outputs remain intact.

Report: `output/reports/applicant_composition_trends/probability_weighted.html`.
Data: `data/clean/applicant_composition_trends/probability_weighted/`.
