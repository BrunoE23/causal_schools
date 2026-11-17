# Voluntary SAE application: one joint regression

[Latest compact table: grades 5-8 averages and survey PCA](voluntary_averages.html)

[October 10 update: separate predictors and parent-survey PCA](voluntary_separate_pca.html)

One joint linear probability regression. N = 313,813 students; 2,000 origin schools; 45,947 applicants (14.64%); R-squared = 0.0343. Each cell is coefficient (school-clustered standard error), in percentage points. All constructed predictors are entered together, including quadratic and missingness terms.

## Grades, attendance and SIMCE-relative gaps

| Variable | Term | Grade 5 | Grade 6 | Grade 7 | Grade 8 |
| --- | --- | --- | --- | --- | --- |
| GPA | Linear | -1.891 (0.467) | -2.788 (0.468) | -1.886 (0.582) | -2.596 (0.396) |
| GPA | Quadratic | -0.304 (0.125) | -0.375 (0.129) | 0.008 (0.141) | -0.614 (0.079) |
| Attendance | Linear | -0.695 (0.250) | -0.682 (0.263) | -2.117 (0.384) | -2.533 (0.376) |
| Attendance | Quadratic | -0.079 (0.039) | -0.087 (0.039) | -0.272 (0.047) | -0.345 (0.061) |
| GPA relative to peers | Linear | -0.307 (0.445) | -0.789 (0.510) | 3.749 (0.568) | 5.878 (0.428) |
| GPA relative to peers | Quadratic | 0.245 (0.114) | -0.311 (0.125) | 0.302 (0.125) | 0.519 (0.087) |
| Attendance relative to peers | Linear | 0.511 (0.181) | 0.869 (0.187) | 1.495 (0.261) | 0.724 (0.208) |
| Attendance relative to peers | Quadratic | -0.032 (0.025) | 0.059 (0.027) | 0.109 (0.029) | 0.068 (0.025) |
| GPA minus baseline SIMCE math (relative z-scores) | Linear | 1.624 (0.376) | 0.653 (0.449) | -0.492 (0.488) | -2.967 (0.468) |
| GPA minus baseline SIMCE math (relative z-scores) | Quadratic | 0.063 (0.073) | 0.159 (0.094) | -0.114 (0.092) | -0.155 (0.074) |
| GPA minus baseline SIMCE language (relative z-scores) | Linear | 0.342 (0.407) | 1.635 (0.513) | -1.466 (0.559) | -0.993 (0.486) |
| GPA minus baseline SIMCE language (relative z-scores) | Quadratic | -0.179 (0.079) | 0.159 (0.095) | 0.055 (0.090) | 0.076 (0.074) |

## Parent questionnaire answers

| Variable | Coefficient (SE) |
| --- | --- |
| Would enroll child here again | -0.105 (0.303) |
| Would recommend school | -0.429 (0.321) |
| Child likes attending school | -0.179 (0.195) |
| School is a good place to learn | -0.293 (0.218) |
| Child goes calmly to school | -0.236 (0.220) |
| Child feels accepted by peers (2013) | 0.207 (0.350) |
| Child feels accepted by teachers (2013) | -0.403 (0.341) |
| Child feels protected with peers (2014-15) | -0.463 (0.137) |
| Child feels protected with teachers (2014-15) | 0.055 (0.139) |
| Head teacher concern for students | 0.346 (0.187) |
| School discipline and compliance with rules rating | -0.330 (0.135) |
| Work with students with behavior problems | 0.393 (0.132) |
| Work to prevent bullying | -0.143 (0.132) |
| Care and cleanliness rating (2013-14) | -0.250 (0.217) |
| Quality of school facilities | -0.014 (0.229) |
| Values formation | -0.822 (0.161) |
| Teachers encourage students to help one another | 0.925 (0.155) |
| Students commitment to school | -0.345 (0.138) |
| Support to parents for educating children | 0.040 (0.149) |
| Extracurricular activities offered | -0.059 (0.147) |
| Students level of learning | -1.025 (0.173) |
| Quality of teaching (2014) | 0.896 (0.241) |
| Math teaching quality (2013) | -0.469 (0.399) |
| Language teaching quality (2013) | 0.220 (0.371) |
| Study and homework requirements | -0.002 (0.142) |
| School motivates interest in learning | 0.498 (0.179) |
| Communication of school values | -0.325 (0.156) |
| Communication of school rules | -0.205 (0.155) |
| Frequency of informing parents about grades | -0.192 (0.123) |
| Communication about student problems | 0.196 (0.127) |
| Frequency of head teacher interviews with parents | -0.016 (0.130) |
| Head teacher willingness to address concerns | -0.078 (0.122) |
| Willingness to listen to parent concerns | 0.085 (0.129) |
| All students equally important to teachers (2016) | -0.096 (0.163) |
| Equal opportunities to learn (2016) | 0.218 (0.164) |
| Absence of teacher mistreatment (2016; reversed) | -0.096 (0.125) |
| Child feels safe with peers (2016) | -0.237 (0.139) |
| School rules applied fairly (2016) | 0.063 (0.143) |
| Cleanliness and order (2016) | -0.074 (0.146) |

## Age and questionnaire matching

| Variable | Coefficient (SE) |
| --- | --- |
| Age: linear | 0.026 (0.117) |
| Age: quadratic | -0.380 (0.035) |
| Same grade-4 and grade-8 school | 1.004 (0.180) |
| Parent questionnaire record linked | 0.089 (0.203) |

## Missingness indicators

| Variable | Coefficient (SE) |
| --- | --- |
| Age | -1.581 (5.740) |
| GPA, grade 5 | 14.080 (6.551) |
| GPA, grade 6 | 2.464 (5.251) |
| GPA, grade 7 | 25.135 (9.451) |
| GPA, grade 8 | 8.639 (9.927) |
| Attendance, grade 5 | -17.585 (7.487) |
| Attendance, grade 6 | -8.147 (5.626) |
| Attendance, grade 7 | -20.525 (6.221) |
| Attendance, grade 8 | -18.106 (3.592) |
| GPA relative to peers, grade 5 | -0.138 (2.765) |
| GPA relative to peers, grade 6 | 1.106 (2.457) |
| GPA relative to peers, grade 7 | -0.449 (8.253) |
| GPA relative to peers, grade 8 | -6.131 (9.706) |
| Attendance relative to peers, grade 5 | 1.122 (1.683) |
| Attendance relative to peers, grade 6 | -0.665 (1.524) |
| Attendance relative to peers, grade 7 | -3.001 (3.977) |
| Attendance relative to peers, grade 8 | -1.134 (0.619) |
| GPA minus baseline SIMCE math (relative z-scores), grade 5 | -0.809 (4.291) |
| GPA minus baseline SIMCE math (relative z-scores), grade 6 | -1.061 (3.188) |
| GPA minus baseline SIMCE math (relative z-scores), grade 7 | -12.684 (4.353) |
| GPA minus baseline SIMCE math (relative z-scores), grade 8 | 13.418 (3.775) |
| GPA minus baseline SIMCE language (relative z-scores), grade 5 | 1.413 (4.317) |
| GPA minus baseline SIMCE language (relative z-scores), grade 6 | 1.610 (3.343) |
| GPA minus baseline SIMCE language (relative z-scores), grade 7 | 2.506 (4.228) |
| GPA minus baseline SIMCE language (relative z-scores), grade 8 | -6.023 (3.793) |
| Same grade-4 and grade-8 school | -1.486 (0.793) |
| Would enroll child here again | 0.576 (1.926) |
| Would recommend school | 0.559 (1.806) |
| Child likes attending school | -3.769 (1.685) |
| School is a good place to learn | -0.533 (1.675) |
| Child goes calmly to school | 2.674 (1.738) |
| Child feels accepted by peers (2013) | -2.259 (3.070) |
| Child feels accepted by teachers (2013) | 3.576 (3.046) |
| Child feels protected with peers (2014-15) | 0.875 (0.753) |
| Child feels protected with teachers (2014-15) | -0.397 (0.817) |
| Head teacher concern for students | 0.890 (1.117) |
| School discipline and compliance with rules rating | -0.079 (0.583) |
| Work with students with behavior problems | -0.671 (0.613) |
| Work to prevent bullying | -0.259 (0.730) |
| Care and cleanliness rating (2013-14) | 2.626 (1.441) |
| Quality of school facilities | -2.082 (1.418) |
| Values formation | -0.978 (1.086) |
| Teachers encourage students to help one another | 0.550 (1.079) |
| Students commitment to school | 0.670 (1.060) |
| Support to parents for educating children | 0.548 (1.030) |
| Extracurricular activities offered | -0.100 (1.074) |
| Students level of learning | 0.690 (1.145) |
| Quality of teaching (2014) | -0.671 (1.490) |
| Math teaching quality (2013) | 1.600 (2.452) |
| Language teaching quality (2013) | -1.466 (2.442) |
| Study and homework requirements | -0.613 (1.071) |
| School motivates interest in learning | 1.333 (0.979) |
| Communication of school values | -0.832 (0.868) |
| Communication of school rules | -0.692 (0.821) |
| Frequency of informing parents about grades | 0.321 (0.883) |
| Communication about student problems | 0.427 (0.850) |
| Frequency of head teacher interviews with parents | 0.487 (0.809) |
| Head teacher willingness to address concerns | 0.560 (0.843) |
| Willingness to listen to parent concerns | -0.028 (0.771) |
| All students equally important to teachers (2016) | 1.468 (0.658) |
| Equal opportunities to learn (2016) | 0.150 (0.711) |
| Absence of teacher mistreatment (2016; reversed) | -0.601 (0.589) |
| Child feels safe with peers (2016) | -0.296 (0.708) |
| School rules applied fairly (2016) | -1.050 (0.692) |
| Cleanliness and order (2016) | 2.006 (0.879) |

## Context controls and intercept

| Variable | Coefficient (SE) |
| --- | --- |
| (Intercept) | 92.968 (4.282) |
| factor(cohort_gr8)2018 | 1.914 (1.962) |
| factor(cohort_gr8)2019 | 2.478 (1.811) |
| factor(cohort_gr8)2020 | -0.991 (1.967) |
| factor(origin_region)2 | -0.648 (2.272) |
| factor(origin_region)3 | 0.908 (3.260) |
| factor(origin_region)4 | -0.673 (2.389) |
| factor(origin_region)5 | -1.798 (1.610) |
| factor(origin_region)6 | 6.341 (3.133) |
| factor(origin_region)7 | -0.748 (1.924) |
| factor(origin_region)8 | -1.667 (1.794) |
| factor(origin_region)9 | -0.333 (1.892) |
| factor(origin_region)10 | -1.721 (2.352) |
| factor(origin_region)11 | -5.509 (2.184) |
| factor(origin_region)12 | -0.672 (4.629) |
| factor(origin_region)13 | -1.686 (1.604) |
| factor(origin_region)14 | 0.119 (2.340) |
| factor(origin_region)15 | -1.497 (2.037) |
| factor(origin_region)16 | -0.943 (2.174) |
| factor(origin_sector)2 | 0.127 (1.587) |
| factor(origin_sector)3 | -3.083 (1.374) |
| factor(origin_sector)5 | -9.327 (2.497) |
| factor(origin_sector)6 | -0.438 (2.060) |
| factor(gender)1 | -78.707 (3.059) |
| factor(gender)2 | -75.946 (3.053) |
| factor(cohort_gr8)2018:factor(origin_region)2 | 1.085 (2.215) |
| factor(cohort_gr8)2019:factor(origin_region)2 | 1.043 (2.059) |
| factor(cohort_gr8)2018:factor(origin_region)3 | 2.240 (3.393) |
| factor(cohort_gr8)2019:factor(origin_region)3 | -0.633 (2.553) |
| factor(cohort_gr8)2018:factor(origin_region)4 | -3.324 (2.155) |
| factor(cohort_gr8)2019:factor(origin_region)4 | -3.461 (2.046) |
| factor(cohort_gr8)2020:factor(origin_region)4 | -3.706 (2.183) |
| factor(cohort_gr8)2018:factor(origin_region)5 | 0.939 (1.929) |
| factor(cohort_gr8)2019:factor(origin_region)5 | 1.038 (1.758) |
| factor(cohort_gr8)2018:factor(origin_region)6 | -5.873 (2.312) |
| factor(cohort_gr8)2019:factor(origin_region)6 | -5.900 (2.356) |
| factor(cohort_gr8)2020:factor(origin_region)6 | -8.007 (2.520) |
| factor(cohort_gr8)2018:factor(origin_region)7 | 0.618 (2.162) |
| factor(cohort_gr8)2019:factor(origin_region)7 | -1.295 (1.869) |
| factor(cohort_gr8)2018:factor(origin_region)8 | 0.219 (2.012) |
| factor(cohort_gr8)2019:factor(origin_region)8 | -1.940 (1.759) |
| factor(cohort_gr8)2018:factor(origin_region)9 | 1.256 (2.150) |
| factor(cohort_gr8)2019:factor(origin_region)9 | -0.613 (1.892) |
| factor(cohort_gr8)2018:factor(origin_region)10 | -3.061 (2.241) |
| factor(cohort_gr8)2019:factor(origin_region)10 | -1.892 (2.261) |
| factor(cohort_gr8)2020:factor(origin_region)10 | -1.795 (2.427) |
| factor(cohort_gr8)2018:factor(origin_region)11 | -2.880 (2.270) |
| factor(cohort_gr8)2019:factor(origin_region)11 | -2.193 (2.415) |
| factor(cohort_gr8)2018:factor(origin_region)12 | -0.104 (2.621) |
| factor(cohort_gr8)2019:factor(origin_region)12 | 2.365 (3.599) |
| factor(cohort_gr8)2020:factor(origin_region)12 | -0.096 (3.233) |
| factor(cohort_gr8)2019:factor(origin_region)13 | 1.811 (1.706) |
| factor(cohort_gr8)2018:factor(origin_region)14 | 1.205 (2.398) |
| factor(cohort_gr8)2019:factor(origin_region)14 | -1.559 (2.041) |
| factor(cohort_gr8)2018:factor(origin_region)15 | 1.574 (2.869) |
| factor(cohort_gr8)2019:factor(origin_region)15 | -0.880 (2.138) |
| factor(cohort_gr8)2018:factor(origin_region)16 | 2.438 (2.561) |
| factor(cohort_gr8)2019:factor(origin_region)16 | 0.559 (2.390) |

## Notes

- Outcome: explicit grade-9 SAE application during grade 8, in either regular or complementary rounds. Sample: all eligible students whose grade-8 origin offers grade 9. All displayed coefficients come from the same joint OLS regression.
- Coefficients and standard errors are in percentage points. Numeric measures use the existing voluntary-training mean and SD; missing values retain the existing training-median imputation and missingness indicators. This preserves the constructed regressors while fitting the regression on the full sample.
- For grades, attendance, gaps and age, the linear row is the coefficient on the standardized value, and the quadratic row is the coefficient on its square. With a quadratic term, a one-SD increase starting at the mean has association equal to the linear plus quadratic coefficients. Parent items enter linearly, so their coefficients are percentage points per one training-sample SD.
- Same-school and parent-record indicators also retain their original standardization. Their coefficients are per one SD, not a full zero-to-one change. Their original SDs are saved in the preprocessing file.
- Higher parent ratings indicate more favorable reports after the documented scale reversals. They refer to the grade-4 school; same-school and response/missingness indicators are included jointly.
- Controls include cohort-by-region, school sector, gender, age and age squared. Standard errors cluster by grade-8 origin school. Omitted collinear terms are listed below. School-ending status is constant in this sample. Geography remains deferred.
- These are conditional descriptive associations. Grade-8 annual measures can postdate application. The model is an in-sample linear probability regression; its R-squared is not held-out prediction performance.
- Verification passed 7 checks, including independent reconstruction of school-clustered standard errors, OLS normal equations, and R-squared.

## Omitted collinear terms

- `factor(cohort_gr8)2020:factor(origin_region)2`
- `factor(cohort_gr8)2020:factor(origin_region)3`
- `factor(cohort_gr8)2020:factor(origin_region)5`
- `factor(cohort_gr8)2020:factor(origin_region)7`
- `factor(cohort_gr8)2020:factor(origin_region)8`
- `factor(cohort_gr8)2020:factor(origin_region)9`
- `factor(cohort_gr8)2020:factor(origin_region)11`
- `factor(cohort_gr8)2018:factor(origin_region)13`
- `factor(cohort_gr8)2020:factor(origin_region)13`
- `factor(cohort_gr8)2020:factor(origin_region)14`
- `factor(cohort_gr8)2020:factor(origin_region)15`
- `factor(cohort_gr8)2020:factor(origin_region)16`
