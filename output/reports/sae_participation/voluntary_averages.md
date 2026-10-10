# Voluntary SAE: average academic histories

Six academic summaries replace the 24 grade-specific measures. Each summary averages the available observations in grades 5-8, then enters with its square. The outcome remains voluntary SAE application: 45,947 applicants among 313,813 students (14.64%) whose grade-8 school offers grade 9.

## One measure at a time

Coefficients and school-clustered standard errors are in percentage points. Each row below is a separate regression with that academic average and its square, plus the same context and survey-PC1 controls. An average is standardized using its mean and SD among the original training pupils. The linear coefficient is the slope at that reference mean; with squares, a change from the mean to one SD above it is the sum of the linear and squared coefficients.

| Average across grades 5-8 | Linear coefficient (SE) | Squared term (SE) |
| --- | --- | --- |
| GPA | -1.617 (0.141) | -0.322 (0.062) |
| Attendance | -2.111 (0.194) | -0.233 (0.019) |
| GPA relative to peers | 0.023 (0.102) | -0.382 (0.058) |
| Attendance relative to peers | 0.193 (0.113) | -0.058 (0.024) |
| GPA minus baseline SIMCE math | -0.709 (0.074) | 0.008 (0.045) |
| GPA minus baseline SIMCE language | -0.522 (0.070) | 0.080 (0.049) |

## What the averages show

Peer-relative GPA has a near-zero linear coefficient (+0.023), but a negative squared term (-0.382). Its fitted association is therefore an inverted U, not no association: moving from the mean to one SD above or below it changes fitted application by -0.36 or -0.41 pp, respectively.

The average GPA-minus-SIMCE-math gap has a negative linear coefficient (-0.709 pp), while its squared coefficient is close to zero.

More favorable parent-survey PC1 scores remain associated with 0.67-1.23 pp less application per SD across survey years, controlling jointly for the six academic averages.

Averaging can hide timing: when grades 5-6 and 7-8 peer-relative GPA averages enter together, their linear coefficients are -1.74 and +1.66 pp. The compact average is useful for presentation, but the two-period check retains this distinction.

## Parent-survey PCA with all six academic averages

| Survey year | Complete questionnaires | Variance explained (%) | PC1 coefficient (SE) |
| --- | --- | --- | --- |
| 2013 | 18,294 | 47.0 | -1.090 (0.337) |
| 2014 | 49,701 | 51.7 | -1.226 (0.195) |
| 2015 | 76,778 | 53.5 | -1.044 (0.160) |
| 2016 | 73,468 | 48.9 | -0.673 (0.135) |

## All six academic averages entered together

| Average across grades 5-8 | Linear coefficient (SE) | Squared term (SE) |
| --- | --- | --- |
| GPA | -7.829 (0.619) | -1.158 (0.127) |
| Attendance | -4.075 (0.447) | -0.503 (0.057) |
| GPA relative to peers | 7.267 (0.564) | 0.710 (0.109) |
| Attendance relative to peers | 2.427 (0.305) | 0.143 (0.030) |
| GPA minus baseline SIMCE math | -0.678 (0.080) | -0.054 (0.046) |
| GPA minus baseline SIMCE language | -0.245 (0.075) | 0.042 (0.050) |

## Grades 5-6 versus 7-8

Each row enters the two period averages and squares together; no other academic measure enters.

| Measure | Grades 5-6: linear (SE) | Grades 7-8: linear (SE) | Grades 5-6: squared (SE) | Grades 7-8: squared (SE) |
| --- | --- | --- | --- | --- |
| GPA | -2.160 (0.202) | 0.294 (0.199) | -0.272 (0.087) | -0.080 (0.069) |
| Attendance | -0.353 (0.168) | -2.516 (0.289) | -0.063 (0.018) | -0.322 (0.034) |
| GPA relative to peers | -1.742 (0.202) | 1.664 (0.180) | -0.676 (0.087) | 0.138 (0.068) |
| Attendance relative to peers | 0.039 (0.098) | 0.117 (0.137) | -0.102 (0.020) | -0.009 (0.023) |
| GPA minus baseline SIMCE math | -0.213 (0.139) | -0.525 (0.136) | 0.160 (0.062) | -0.129 (0.066) |
| GPA minus baseline SIMCE language | -0.398 (0.139) | -0.148 (0.137) | 0.034 (0.064) | 0.072 (0.068) |

## GPA and math gap on identical observed grades

These three regressions use the same 275,638 pupils and average GPA and its math gap over exactly the same observed grades. This preserves D = G - M, where M is baseline math relative to later peers. In a joint linear specification aG + bD = (a+b)G - bM. A larger gap holding GPA fixed means lower baseline math; a higher GPA holding the gap fixed also means higher baseline math. The table uses separately standardized averages, so undo their SDs before combining coefficients. Squared terms make the slopes local. Main-table averages can use different available grades; the exact identity applies to this matched-grade comparison.

| Measure | Alone: linear (SE) | Together: linear (SE) | Alone: squared (SE) | Together: squared (SE) |
| --- | --- | --- | --- | --- |
| GPA relative to peers | -0.032 (0.106) | 0.304 (0.111) | -0.404 (0.064) | -0.458 (0.064) |
| GPA minus baseline SIMCE math | -0.712 (0.075) | -0.825 (0.075) | 0.003 (0.045) | 0.056 (0.045) |

## Model fit

The fit comparison is in-sample R-squared, not held-out predictive performance. All three fits use the same pupils and context/PC1 controls, with squares. Their academic coverage controls differ, so this is not a formal nested-model test.

| Specification | Academic measures | R-squared |
| --- | --- | --- |
| One average, grades 5-8 |  6 | 0.0282 |
| Two averages, grades 5-6 and 7-8 | 12 | 0.0327 |
| Separate grades 5, 6, 7, 8 | 24 | 0.0333 |

## Coverage and scaling

| Measure | All 4 grades | 1-3 grades | No observed grades | SD of average |
| --- | --- | --- | --- | --- |
| GPA | 301,232 | 12,357 | 224 | 0.502 |
| Attendance | 303,713 | 10,100 | 0 | 6.423 |
| GPA relative to peers | 300,450 | 13,139 | 224 | 0.844 |
| Attendance relative to peers | 280,010 | 33,707 | 96 | 0.403 |
| GPA minus baseline SIMCE math | 272,532 | 3,106 | 38,175 | 0.894 |
| GPA minus baseline SIMCE language | 271,486 | 3,113 | 39,214 | 0.903 |

## Definitions and verification

- All main regressions use the same 313,813 pupils and 2,000 origin schools. Applicants and nonapplicants are included. Offering grade 9 defines the voluntary margin operationally; applications do not establish actual moves or motives.
- For each measure, take the arithmetic mean of the available grade-level values, weighting observed grades equally. GPA/attendance use their original units; relative measures use the previously constructed school-grade-year peer z-scores. Gaps average the existing peer-GPA z minus baseline grade-4 SIMCE peer z; they are not learning gains.
- Normalize each new average with its own observed original-training mean and SD. If all grades are missing, impute the training median. Every included average has categorical controls for its number of observed grades (0-4, or 0-2 in the two-period check). These controls retain all pupils but do not eliminate all selection concerns. The quadratic term is the square of the standardized average, not the average of grade-specific squares.
- Common controls: cohort-by-region, school sector, gender, age and its square/missingness, grade-4 versus grade-8 school match/missingness, parent-record indicator, survey year, fraction of applicable questions answered, and year-specific complete-survey indicators and PC1 scores. One-at-a-time models contain no other academic measure.
- Survey PC1 is reused unchanged from the previous analysis: correlation PCA separately for 2013-2016 on 19, 29, 20 and 16 harmonized questions among complete respondents, with higher values indicating favorable answers. Scores have within-year mean zero and SD one; zero scores plus completeness controls retain incomplete respondents. The 218,241 complete questionnaires cover 69.5% of the sample. Scores concern the grade-4 school, which may differ from the grade-8 origin.
- All fits are OLS linear probability models with standard errors clustered at origin school. Inference conditions on the estimated PCA scores. These are descriptive associations; annual grade-8 inputs may postdate application. Geography remains deferred.
- The fit comparison is in-sample R-squared, not held-out predictive performance. All three fits use the same pupils and context/PC1 controls, with squares. Their academic coverage controls differ, so this is not a formal nested-model test.
- Verification: 43 checks passed, including sample counts, observed-grade averaging, the matched-grade gap identity, model composition, OLS normal equations, and an independent reconstruction of the compact joint model school-clustered covariance.

[Grade-specific results and PCA loadings](voluntary_separate_pca.html)
