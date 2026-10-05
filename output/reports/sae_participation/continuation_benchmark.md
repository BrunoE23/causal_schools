# Beating the continuation rule

Your rule predicts no SAE application where grade 9 is offered, and an application where the school ends at grade 8 in SAE offerings. It correctly classifies 108,575 of 120,943 held-out students (89.774%).

At a fixed probability threshold of 0.5, the full model reaches 90.002% accuracy: +0.228 percentage points. It fixes 361 of the rule's mistakes and introduces 85 new mistakes, avoiding 276 errors net (2.23% of the rule's errors).

Of the 276 net corrections, 275 are at schools ending at grade 8 and 1 at schools offering grade 9. The model barely improves binary classification of applications where staying is possible.

Without grade-8 annual grades, attendance and SIMCE gaps, accuracy is 89.776%: only 3 net errors avoided. The retrospective grade-8 information supplies almost all of the classification gain.

## Same-student comparison

| Model | Accuracy (%) | Errors | Fixed versus your rule | Introduced | Net avoided | Gain (pp) |
| --- | --- | --- | --- | --- | --- | --- |
| Your continuation rule | 89.774 | 12,368 | 0 | 0 | 0 | 0.000 |
| Existing continuation + context logit | 89.787 | 12,352 | 35 | 19 | 16 | 0.013 |
| + Grades and attendance, grades 5-8 | 89.995 | 12,100 | 357 | 89 | 268 | 0.222 |
| + SIMCE-relative achievement gaps | 90.001 | 12,093 | 361 | 86 | 275 | 0.227 |
| + Parent availability and school match | 90.001 | 12,093 | 358 | 83 | 275 | 0.227 |
| + Parent answers (full model) | 90.002 | 12,092 | 361 | 85 | 276 | 0.228 |
| Full model excluding grade-8 annual measures | 89.776 | 12,365 | 66 | 63 | 3 | 0.002 |

## Where the full model helps

| Group | Students | Your rule (%) | Full model (%) | Fixed | Introduced | Net avoided | Exceptions detected (%) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Ends at 8 in SAE offerings | 58,754 | 95.336 | 95.805 | 358 | 83 | 275 | 13.07 |
| Offers grade 9 | 62,189 | 84.518 | 84.520 | 3 | 2 | 1 | 0.03 |

## Where applicants come from (full analysis sample)

Across the full training and held-out analysis sample, 325,747 students applied for grade 9 in the 2017-2020 grade-8 cohorts. 84.4% come from schools ending at grade 8, 14.1% from schools offering grade 9 ("voluntary" applications), and 1.5% from unresolved/other offerings. Restricting to the two resolved groups gives 85.7% school-ending and 14.3% voluntary. These are unique students with explicit regular or complementary applications, not confirmed school changes; the figures cover the defined grade-8 cohort sample in SAE-covered origin regions, not every raw SAE grade-9 applicant.

| Group | Applicants | Share of all applicants (%) | Share within resolved groups (%) |
| --- | --- | --- | --- |
| Ends at 8 in SAE offerings | 274,819 | 84.37 | 85.68 |
| Offers grade 9 | 45,947 | 14.11 | 14.32 |
| Not resolved in SAE offerings | 4,849 | 1.49 | -- |
| Other SAE offering | 132 | 0.04 | -- |

## Definitions and interpretation

- The comparison uses the same 120,943 held-out students with either confirmed grade-9 SAE offerings or an observed grade-8 origin with no secondary SAE offering. It excludes 11,531 students with unresolved offerings and 19 in the small Other SAE offering group. These pupils cannot be assigned your rule unambiguously from the present classification.
- The target remains SAE application, not actual subsequent school continuation. A nonapplication is not proof that a student stays, and an application is not proof that a student moves.
- All learned models and predictions are unchanged. Classification uses a prespecified 0.5 cutoff and equal costs for false positives and false negatives. No cutoff or model was chosen to maximize accuracy on this already-inspected holdout.
- The paired full-model accuracy gain has an origin-school-clustered 95% confidence interval of [0.179, 0.278] percentage points, conditional on the fixed trained models. This comes from an intercept-only regression of each pupil's change in correctness. It does not include training-sample or model-selection uncertainty.
- For probability quality, a fairer school-only comparator assigns each continuation group its training-set application rate. It makes the same binary predictions as your rule at 0.5, but avoids treating application probabilities as literally zero or one. Probability-score comparisons are saved separately; the hard rule has infinite log loss when it makes a mistake.
- The previous overall AUC mostly rewarded separating schools with and without continuation. It did not establish accurate identification of exceptions to this rule. The primary comparison here is held-out accuracy and net mistakes avoided, with the two school groups reported separately.
- This is still retrospective when grade-8 annual measures are included. Geography remains deferred. Any new model or threshold development should use training/validation data; further confirmatory claims will need a fresh holdout because the current test results have now been examined.
