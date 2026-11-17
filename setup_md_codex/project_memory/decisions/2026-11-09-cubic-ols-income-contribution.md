# Income's incremental contribution to eight cubic-score OLS models

The user returned to OLS with flexible math/verbal controls and requested the
four main premium-enrollment outcomes, the four field groups, and the fraction
of the model explained by income. Retain the full total-degree-three basis
M, V, M^2, V^2, MV, M^3, V^3, M^2V, MV^2; income-decile indicators (D1 reference);
and admission-year indicators. There are no score-income interactions.

Restore the original OLS sample of 565,040 students and 3,372 attended-school
clusters, including 97,717 HE non-enrollees and the 5,105 admission-2026 records.
Their recorded premium outcomes are zero; their inclusion preserves the prior
OLS sample and is not evidence about future enrollment. Unlike logit, OLS can
retain this year cell. Preserve normalization, income ranks, and all outcomes.
Join the two existing small analysis caches by unique student ID, checking every
shared covariate exactly. Do not reread or modify the master data.

For each of the eight outcomes estimate the full model and a reduced model
dropping only the nine income indicators, on the identical sample. The main
requested quantity is (R2_full - R2_without_income) / R2_full. It is the share
of full-model explained variation uniquely added by income after flexible
scores and year effects. It is order-dependent and does not allocate explained
variation shared by income and scores. It is descriptive, not a causal share.
Use ordinary unadjusted in-sample R2 throughout. Separately save income's
partial R2 = (R2_full - R2_without_income) / (1 - R2_without_income); this has a
different denominator and must not be mislabeled as the requested fraction.

Retain school-clustered CR1 covariance, with G/(G-1)*(N-1)/(N-K), K=24 in the
full models and K=15 in the reduced models. Coefficient and average score
derivative inference uses t(G-1). Income coefficients are constant conditional
probability differences in these additive OLS models. Score summaries are
average local derivatives including every polynomial term; they are linear
functions of the full coefficient vector and use its actual clustered covariance.

Verify both nested fits' normal equations and clustered covariance, R2 nesting,
the projection identity equating the SSE gain to squared fitted-value changes,
both outcome coefficient identities, and exact replication of prior cubic OLS
field coefficients and SEs. No bootstrap or proxy uncertainty is assigned to
the descriptive R2 fractions. Keep all prior results.

Scripts: `code/codex/premium_enrollment_scores_income/09_cubic_ols_income_contribution.R`
and `10_cubic_ols_income_tables.R`. Outputs use the task name
`premium_cubic_income_contribution` in clean data, reports, and tables.

All sixteen nested OLS fits pass the checks; the four full field models
reproduce the earlier coefficients and SEs exactly. In institution/field/
either/both order, full R2 is 0.24930/0.09784/0.17287/0.17827 and income's
incremental share of explained variation is 12.54%/3.19%/1.34%/3.46%.
For Science/Law/Engineering/Medicine+, full R2 is
0.01635/0.03469/0.04404/0.04216 and income's incremental share is
11.38%/4.85%/3.22%/2.75%. Input-cache checksums remain unchanged.
