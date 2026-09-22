# Decision: Paper cohort windows for observational VA and SAE lottery analyses

**Date:** 2026-09-21
**Status:** active

## Decision

Exclude the 2021 grade-8 cohort from current paper analyses. Although a small
complete-case subset exists, baseline data are unavailable for most of that
cohort, so retaining it would select a different type of student into the VA
estimation.

Estimate two observational school-value-added samples:

- grade-8 cohorts 2017--2020, using all currently usable observational cohorts;
- grade-8 cohorts 2017--2018, paired with a non-overlapping SAE analysis.

Use two corresponding SAE lottery windows:

- assignment cohorts 2018--2020, using all currently usable lottery cohorts;
- assignment cohorts 2019--2020, paired with 2017--2018 VA so that the students
  used to estimate VA come from different cohorts than the students used for
  lottery validation.

Output paths must encode the cohort window to prevent one run from overwriting
another. The 2017--2020 observational VA and 2018--2020 SAE samples overlap in
cohort years by design; this is the maximum-data specification. The 2017--2018
observational VA and 2019--2020 SAE samples are the no-overlap specification.

The requested paper-facing VA outcomes are standardized admission-test math,
standardized admission-test language, admission-exam taking, log projected
program income, high-premium-field enrollment, high-premium-institution
enrollment, and any FUAS benefits/credit application. Exam taking and benefits
application use the broad complete-control sample; postsecondary outcomes use
the admission-exam-taker sample.
