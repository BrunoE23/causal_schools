# Staff characteristics and school VA: joint Lasso

## New characteristics: coverage

| New characteristic | Group | Schools observed (of 3,682) |
| --- | ---: | ---: |
| Mean pre-HS grade-4 math (SD units) | school | 3682 |
| Within-school SD of pre-HS grade-4 math | school | 3499 |
| Mean pre-HS grade-4 language (SD units) | school | 3682 |
| Within-school SD of pre-HS grade-4 language | school | 3499 |
| Mean baseline household income decile (observed/imputed) | school | 3682 |
| Baseline income bottom-two-decile share (observed/imputed) | school | 3682 |
| Baseline income top-two-decile share (observed/imputed) | school | 3682 |
| Mean father education years (observed/imputed) | school | 3682 |
| Mean mother education years (observed/imputed) | school | 3682 |
| Female student share among known sex | school | 3682 |
| Mean student age in grade 8 | school | 3682 |
| Baseline household income imputed share | school | 3682 |
| Father education imputed share | school | 3682 |
| Mother education imputed share | school | 3682 |
| Public funding per student, 2017-2021 (million 2021 CLP) | school | 2943 |
| Log public funding per student, 2017-2021 | school | 2943 |
| Public funding change: 2019-2021 vs 2017-2018 (million 2021 CLP/student) | school | 2943 |
| Share of 2017-2021 years with funding records | school | 3682 |
| Share of 2017-2021 years with complete funding records | school | 3682 |
| Mean age attained in staff year | teacher | 3003 |
| Mean within-year age SD (years with 2+ valid ages) | teacher | 2999 |
| Staff younger than 35 share | teacher | 3003 |
| Staff age 50+ share | teacher | 3003 |
| Valid birthdate share, active-year average | teacher | 3003 |
| Share of active staff years with any valid age | teacher | 3003 |
| Mean age attained in staff year | counselor | 2111 |
| Mean within-year age SD (years with 2+ valid ages) | counselor | 589 |
| Staff younger than 35 share | counselor | 2111 |
| Staff age 50+ share | counselor | 2111 |
| Valid birthdate share, active-year average | counselor | 2111 |
| Share of active staff years with any valid age | counselor | 2111 |
| Mean age attained in staff year | leadership | 3547 |
| Mean within-year age SD (years with 2+ valid ages) | leadership | 2632 |
| Staff younger than 35 share | leadership | 3547 |
| Staff age 50+ share | leadership | 3547 |
| Valid birthdate share, active-year average | leadership | 3550 |
| Share of active staff years with any valid age | leadership | 3550 |

## Out-of-sample prediction

| Outcome | Schools | Selected | School-only R² | Joint R² | Gain in R² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Math | 3,373 | 33 | 0.539 | 0.554 | +0.015 |
| Language | 3,381 | 31 | 0.546 | 0.547 | +0.001 |
| Exam taking | 3,682 | 93 | 0.565 | 0.637 | +0.072 |
| HE enrollment | 3,675 | 68 | 0.328 | 0.351 | +0.023 |
| STEM | 3,682 | 32 | 0.064 | 0.069 | +0.005 |
| HP field | 3,680 | 80 | 0.128 | 0.121 | -0.007 |
| HP institution | 3,682 | 72 | 0.578 | 0.634 | +0.056 |
| Income: full | 3,682 | 84 | 0.470 | 0.484 | +0.014 |
| Income: field | 3,375 | 23 | 0.143 | 0.135 | -0.008 |
| Income: institution | 3,375 | 28 | 0.284 | 0.287 | +0.003 |
| Program accreditation | 3,375 | 10 | 0.101 | 0.102 | +0.000 |
| Institution accreditation | 3,375 | 25 | 0.147 | 0.142 | -0.005 |

## Main coefficients

Standardized coefficients; one-standard-error penalty. Only predictors selected in at least one displayed outcome appear.

### Outcomes 1–4

| Predictor | Math | Language | Exam taking | HE enrollment |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Cumulative observed years in role | — | — | +0.006 | — |
| HS teachers: Primary-function share | +0.014 | — | — | −0.001 |
| HS teachers: University tertiary qualification | +0.012 | +0.041 | — | — |
| HS teachers: Teaching qualification | — | — | +0.009 | — |
| HS teachers: HS teaching qualification | — | +0.014 | +0.031 | — |
| HS teachers: Tertiary qualification | +0.023 | — | — | — |
| HS teachers: Recorded title specialty | — | — | +0.050 | +0.015 |
| HS teachers: CFT-trained share | −0.001 | −0.007 | −0.033 | −0.015 |
| HS teachers: Other institution type share | — | — | −0.033 | −0.041 |
| HS teachers: Longest reported degree (semesters) | — | — | +0.021 | +0.017 |
| HS teachers: Math credentials among assigned HS math teachers | — | +0.005 | — | — |
| HS teachers: Language credentials among assigned HS language teachers | — | — | −0.001 | — |
| HS teachers: Mean annual staff headcount | — | — | +0.018 | +0.070 |
| HS teachers: Share of years with staff present | −0.015 | −0.009 | +0.252 | +0.092 |
| HS teachers: VA-sample students per average staff member | −0.043 | −0.037 | — | +0.049 |
| HS teachers: Log VA-sample students per average staff member | — | — | +0.110 | +0.044 |
| HS teachers: Reported school tenure in 2018 | — | — | +<0.001 | — |
| HS teachers: Reported system tenure in 2018 | — | — | +0.028 | — |
| HS teachers: Role absent throughout 2018-2024 | — | — | −0.088 | −0.015 |
| HS teachers: UG degree at high-premium institution | +0.011 | — | — | — |
| HS teachers: Any non-UG qualification | +0.027 | +0.020 | — | — |
| HS teachers: Role-specific qualification | — | — | +0.003 | — |
| HS teachers: Any magister | +0.016 | +0.017 | — | — |
| HS teachers: Magister at high-premium institution | — | — | — | −0.004 |
| HS teachers: Mean age attained in staff year | — | — | −0.055 | −0.057 |
| HS teachers: Mean within-year age SD (years with 2+ valid ages) | −<0.001 | — | — | — |
| HS teachers: Staff younger than 35 share | — | — | +0.038 | +0.031 |
| Orientadores: Only primary role across school appointments | — | — | — | −0.010 |
| Orientadores: Prior main-function switching rate | — | — | +0.007 | — |
| Orientadores: Tertiary qualification | — | — | +0.017 | +0.014 |
| Orientadores: Other institution type share | — | — | +<0.001 | — |
| Orientadores: Years since earliest reported title | — | — | −0.014 | — |
| Orientadores: Share of years with staff present | −0.007 | — | — | — |
| Orientadores: Staff per 1,000 VA-sample students | +0.012 | +0.003 | −0.008 | — |
| Orientadores: Reported school tenure in 2018 | — | — | +0.003 | — |
| Orientadores: VA students per average primary counselor | — | — | −0.008 | — |
| Orientadores: Log VA students per average primary counselor | — | −0.006 | — | — |
| Orientadores: Role absent throughout 2018-2024 | — | — | — | −0.009 |
| Orientadores: UG degree at high-premium institution | — | — | −0.004 | — |
| Orientadores: Role-specific qualification | — | — | — | +0.001 |
| Leadership: Current school-leadership spell | — | — | — | +0.009 |
| Leadership: Prior detailed main-function change rate | — | — | −0.001 | — |
| Leadership: Teaching qualification | — | — | +0.013 | +0.006 |
| Leadership: Recorded title specialty | — | — | −0.003 | −0.007 |
| Leadership: CFT-trained share | — | — | +0.005 | — |
| Leadership: Director (4) share | — | — | +0.015 | — |
| Leadership: Subdirector (15) share | — | — | −0.001 | — |
| Leadership: Mean primary leadership headcount | +0.050 | +0.015 | — | — |
| Leadership: Share of years with leadership present | — | — | −0.013 | −0.001 |
| Leadership: Leaders per 1,000 VA-sample students | — | — | +0.080 | +0.045 |
| Leadership: VA-sample students per average leader | — | — | +0.030 | — |
| Leadership: Log VA-sample students per average leader | — | −0.015 | −0.099 | −0.030 |
| Leadership: Role absent throughout 2018-2024 | — | — | +0.030 | +0.016 |
| Leadership: Any non-UG qualification | — | — | — | +0.010 |
| Leadership: Non-UG qualification at high-premium institution | — | +0.002 | — | — |
| Leadership: Any degree at high-premium institution | +0.016 | — | — | — |
| Leadership: Role-specific qualification | — | — | +0.001 | — |
| Leadership: Any magister | — | — | +0.014 | — |
| Leadership: Magister at high-premium institution | +0.008 | — | — | — |
| Leadership: Role-specific magister | — | — | — | +0.006 |
| School characteristics: Log VA-sample student count | — | — | −0.037 | −0.035 |
| School characteristics: Technical-professional/artistic offering | −0.031 | −0.130 | −0.152 | −0.139 |
| School characteristics: Basic-education offering | — | — | +0.155 | +0.125 |
| School characteristics: dependency 2 | — | — | +0.014 | +0.007 |
| School characteristics: dependency 3 | — | — | — | +0.026 |
| School characteristics: dependency 5 | — | −0.001 | −0.002 | — |
| School characteristics: region 1 | — | −0.005 | −0.009 | — |
| School characteristics: region 2 | — | −<0.001 | −0.010 | +0.022 |
| School characteristics: region 3 | — | — | — | +0.023 |
| School characteristics: region 4 | — | — | +0.010 | +0.045 |
| School characteristics: region 5 | — | — | +0.022 | +0.042 |
| School characteristics: region 6 | — | — | −0.013 | −0.040 |
| School characteristics: region 7 | — | — | +0.004 | — |
| School characteristics: region 8 | — | — | −0.007 | +0.008 |
| School characteristics: region 9 | +0.026 | +0.047 | +0.003 | — |
| School characteristics: region 10 | +<0.001 | +0.012 | −0.014 | — |
| School characteristics: region 11 | — | +<0.001 | — | — |
| School characteristics: region 12 | — | — | −0.006 | +0.013 |
| School characteristics: region 14 | — | +0.003 | +0.007 | +0.018 |
| School characteristics: region 15 | — | — | +0.003 | +0.006 |
| School characteristics: region 16 | +0.003 | — | +0.003 | +0.027 |
| School characteristics: Enrollment fee: CLP 1,000-10,000 | — | — | — | +0.007 |
| School characteristics: Enrollment fee: Above CLP 100,000 | +0.148 | +0.073 | — | — |
| School characteristics: Monthly fee: CLP 1,000-10,000 | — | — | −0.004 | −0.006 |
| School characteristics: Monthly fee: CLP 25,001-50,000 | — | — | +0.013 | +0.022 |
| School characteristics: Monthly fee: CLP 50,001-100,000 | −0.027 | — | +0.027 | +0.027 |
| School characteristics: Monthly fee: Above CLP 100,000 | — | +0.032 | — | — |
| School characteristics: Mean pre-HS grade-4 math (SD units) | +0.338 | +0.238 | +0.034 | +0.030 |
| School characteristics: Within-school SD of pre-HS grade-4 math | −0.069 | −0.042 | −0.004 | −0.006 |
| School characteristics: Mean pre-HS grade-4 language (SD units) | +0.058 | +0.164 | +0.107 | +0.104 |
| School characteristics: Within-school SD of pre-HS grade-4 language | −0.060 | −0.042 | — | — |
| School characteristics: Baseline income bottom-two-decile share (observed/imputed) | +0.045 | — | −0.023 | −0.027 |
| School characteristics: Baseline income top-two-decile share (observed/imputed) | +0.175 | +0.128 | — | — |
| School characteristics: Mean mother education years (observed/imputed) | — | — | +0.113 | +0.053 |
| School characteristics: Female student share among known sex | — | — | +0.004 | — |
| School characteristics: Mean student age in grade 8 | +0.160 | +0.084 | −0.032 | −0.036 |
| School characteristics: Public funding per student, 2017-2021 (million 2021 CLP) | — | — | −0.009 | — |
| School characteristics: Log public funding per student, 2017-2021 | — | — | +0.057 | +0.118 |

### Outcomes 5–8

| Predictor | STEM | HP field | HP institution | Income: full |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Prior role-years at this school | — | — | −<0.001 | — |
| HS teachers: Primary-function share | — | — | +0.006 | — |
| HS teachers: Only primary role across school appointments | — | — | +0.002 | — |
| HS teachers: University tertiary qualification | — | — | — | +0.016 |
| HS teachers: Teaching qualification | −0.051 | −0.005 | — | — |
| HS teachers: HS teaching qualification | — | — | — | +0.030 |
| HS teachers: IP-trained share | +0.013 | — | — | — |
| HS teachers: CFT-trained share | — | — | −0.006 | −0.034 |
| HS teachers: Normal-school-trained share | −0.001 | −0.015 | — | — |
| HS teachers: Other institution type share | +0.024 | +0.007 | — | — |
| HS teachers: Longest reported degree (semesters) | −0.002 | — | — | +0.005 |
| HS teachers: Years since earliest reported title | — | — | +0.011 | — |
| HS teachers: Math credentials among assigned HS math teachers | −0.004 | −0.008 | −0.007 | −0.008 |
| HS teachers: Mean annual staff headcount | +0.113 | +0.068 | +0.005 | +0.059 |
| HS teachers: Share of years with staff present | — | — | −0.017 | — |
| HS teachers: VA-sample students per average staff member | — | — | −0.012 | +0.010 |
| HS teachers: Log VA-sample students per average staff member | — | — | — | +0.057 |
| HS teachers: Reported school tenure in 2018 | — | +0.017 | — | — |
| HS teachers: UG degree at high-premium institution | — | −0.015 | +0.231 | +0.018 |
| HS teachers: Any non-UG qualification | — | — | — | +0.001 |
| HS teachers: Non-UG qualification at high-premium institution | — | — | +0.046 | — |
| HS teachers: Any degree at high-premium institution | — | — | +0.083 | +0.049 |
| HS teachers: Any magister | — | — | — | +0.002 |
| HS teachers: Magister at high-premium institution | — | −0.014 | +0.017 | — |
| HS teachers: Role-specific magister | — | +0.004 | — | — |
| HS teachers: Mean age attained in staff year | — | −0.012 | — | — |
| HS teachers: Mean within-year age SD (years with 2+ valid ages) | — | — | −<0.001 | — |
| HS teachers: Staff younger than 35 share | — | — | −0.087 | — |
| Orientadores: University tertiary qualification | — | +0.010 | −0.005 | — |
| Orientadores: Teaching qualification | — | +<0.001 | — | — |
| Orientadores: Tertiary qualification | — | — | — | +0.004 |
| Orientadores: Recorded title specialty | — | +0.007 | — | — |
| Orientadores: CFT-trained share | — | — | +0.003 | — |
| Orientadores: Normal-school-trained share | — | — | +0.001 | — |
| Orientadores: Other institution type share | — | — | — | +<0.001 |
| Orientadores: Orientation mention among applicable titles | — | +<0.001 | — | — |
| Orientadores: VA-sample students per average staff member | — | — | −0.013 | — |
| Orientadores: Reported school tenure in 2018 | −<0.001 | −0.007 | — | — |
| Orientadores: VA students per average primary counselor | +0.011 | +0.007 | −0.004 | — |
| Orientadores: Any non-UG qualification | +<0.001 | — | — | — |
| Orientadores: Role-specific qualification | +0.015 | +0.017 | — | +0.001 |
| Orientadores: Mean age attained in staff year | — | −0.004 | — | — |
| Orientadores: Mean within-year age SD (years with 2+ valid ages) | — | −0.008 | — | — |
| Orientadores: Staff age 50+ share | — | −0.004 | — | −0.004 |
| Leadership: Prior primary-or-secondary leadership years | — | −0.001 | — | — |
| Leadership: Primary-function leadership share | — | +<0.001 | — | — |
| Leadership: Prior detailed main-function change rate | — | — | −0.005 | −<0.001 |
| Leadership: Prior main-function leadership entry/exit rate | — | +0.006 | — | — |
| Leadership: University tertiary qualification | — | — | −0.001 | — |
| Leadership: Teaching qualification | — | — | — | +0.005 |
| Leadership: Recorded title specialty | — | −0.001 | — | −0.007 |
| Leadership: CFT-trained share | — | −0.010 | — | — |
| Leadership: Longest degree (semesters) | — | −0.001 | — | — |
| Leadership: Planta Directiva (3) share | — | — | +0.004 | — |
| Leadership: Directiva (10) share | +0.008 | +0.007 | — | — |
| Leadership: Mean annual leadership headcount | — | — | +0.022 | — |
| Leadership: Mean primary leadership headcount | +0.032 | +0.040 | +0.034 | +0.028 |
| Leadership: Leaders per 1,000 VA-sample students | — | +0.019 | — | +0.019 |
| Leadership: Log VA-sample students per average leader | — | — | — | −0.071 |
| Leadership: Role absent throughout 2018-2024 | — | — | — | +0.002 |
| Leadership: UG degree at high-premium institution | +0.011 | +0.001 | — | — |
| Leadership: Any non-UG qualification | — | +0.004 | — | — |
| Leadership: Non-UG qualification at high-premium institution | — | +0.003 | — | +0.023 |
| Leadership: Any degree at high-premium institution | — | +0.003 | +0.006 | — |
| Leadership: Role-specific qualification | — | — | +0.010 | +0.009 |
| Leadership: Magister at high-premium institution | — | +0.002 | +0.007 | — |
| Leadership: Role-specific magister | +0.003 | +0.029 | — | +0.014 |
| Leadership: Mean within-year age SD (years with 2+ valid ages) | — | — | −0.002 | — |
| Leadership: Staff younger than 35 share | — | — | −0.002 | — |
| School characteristics: Log VA-sample student count | — | −0.014 | — | −0.003 |
| School characteristics: Rural school | — | — | +0.012 | — |
| School characteristics: Technical-professional/artistic offering | — | −0.042 | −0.009 | −0.103 |
| School characteristics: Basic-education offering | — | +0.062 | — | +0.117 |
| School characteristics: dependency 2 | −0.008 | −0.002 | — | — |
| School characteristics: dependency 3 | — | +0.009 | −0.022 | — |
| School characteristics: dependency 5 | +0.085 | +0.042 | — | — |
| School characteristics: region 1 | +0.019 | +0.039 | −0.022 | +0.004 |
| School characteristics: region 2 | +0.035 | +0.068 | +0.122 | +0.057 |
| School characteristics: region 3 | +0.037 | +0.069 | +0.121 | +0.055 |
| School characteristics: region 4 | +0.040 | +0.064 | +0.016 | +0.059 |
| School characteristics: region 5 | +0.084 | +0.097 | — | +0.061 |
| School characteristics: region 6 | −0.032 | −0.013 | −0.054 | −0.027 |
| School characteristics: region 7 | — | — | −0.048 | −0.007 |
| School characteristics: region 8 | — | +0.035 | −0.061 | — |
| School characteristics: region 9 | — | — | −0.068 | — |
| School characteristics: region 10 | — | +0.039 | −0.060 | +0.003 |
| School characteristics: region 11 | — | +0.018 | — | — |
| School characteristics: region 12 | — | +0.009 | −0.044 | — |
| School characteristics: region 14 | — | +0.065 | −0.060 | +0.026 |
| School characteristics: region 15 | — | +0.020 | −0.006 | +0.003 |
| School characteristics: region 16 | — | — | −0.027 | — |
| School characteristics: Enrollment fee: CLP 1,000-10,000 | — | — | — | +0.012 |
| School characteristics: Enrollment fee: CLP 10,001-25,000 | — | — | −0.010 | — |
| School characteristics: Enrollment fee: Above CLP 100,000 | — | — | +0.155 | +0.036 |
| School characteristics: Monthly fee: CLP 1,000-10,000 | — | — | — | −0.005 |
| School characteristics: Monthly fee: CLP 25,001-50,000 | — | +0.009 | — | +0.018 |
| School characteristics: Monthly fee: CLP 50,001-100,000 | — | +0.032 | −0.003 | +0.037 |
| School characteristics: Monthly fee: Above CLP 100,000 | — | +0.038 | — | +0.064 |
| School characteristics: Mean pre-HS grade-4 math (SD units) | — | +0.062 | +0.015 | +0.099 |
| School characteristics: Within-school SD of pre-HS grade-4 math | — | −0.022 | −0.024 | −0.041 |
| School characteristics: Mean pre-HS grade-4 language (SD units) | — | +0.050 | — | +0.087 |
| School characteristics: Within-school SD of pre-HS grade-4 language | — | −0.034 | −0.054 | −0.033 |
| School characteristics: Mean baseline household income decile (observed/imputed) | — | +0.049 | — | +0.097 |
| School characteristics: Baseline income bottom-two-decile share (observed/imputed) | — | — | +0.063 | — |
| School characteristics: Baseline income top-two-decile share (observed/imputed) | — | — | +0.283 | +0.019 |
| School characteristics: Mean father education years (observed/imputed) | — | +0.023 | — | +0.026 |
| School characteristics: Mean mother education years (observed/imputed) | — | — | — | +0.041 |
| School characteristics: Female student share among known sex | −0.095 | −0.122 | −0.001 | −0.002 |
| School characteristics: Mean student age in grade 8 | — | — | +0.034 | — |
| School characteristics: Log public funding per student, 2017-2021 | +0.003 | +0.040 | — | +0.046 |

### Outcomes 9–12

| Predictor | Income: field | Income: institution | Program accreditation | Institution accreditation |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Only primary role across school appointments | — | — | — | +0.006 |
| HS teachers: University tertiary qualification | +0.005 | — | — | — |
| HS teachers: Teaching qualification | — | — | +0.129 | — |
| HS teachers: IP-trained share | −0.027 | −0.001 | — | — |
| HS teachers: CFT-trained share | −0.008 | — | — | — |
| HS teachers: Mean annual staff headcount | — | — | — | +0.008 |
| HS teachers: VA-sample students per average staff member | — | — | −0.018 | −0.004 |
| HS teachers: UG degree at high-premium institution | — | +0.005 | — | — |
| HS teachers: Any non-UG qualification | — | — | — | +0.027 |
| HS teachers: Non-UG qualification at high-premium institution | — | — | — | +0.004 |
| HS teachers: Any degree at high-premium institution | — | +0.095 | — | — |
| Orientadores: VA-sample students per average staff member | — | −0.006 | — | — |
| Leadership: Mean primary leadership headcount | — | +0.012 | — | +0.037 |
| Leadership: VA-sample students per average leader | — | −0.013 | −0.090 | — |
| Leadership: Log VA-sample students per average leader | −0.029 | −0.022 | — | — |
| Leadership: Any degree at high-premium institution | — | +0.009 | — | +0.009 |
| Leadership: Role-specific qualification | — | +0.007 | — | — |
| Leadership: Role-specific magister | — | — | — | +0.007 |
| Leadership: Mean age attained in staff year | — | — | — | −<0.001 |
| School characteristics: Technical-professional/artistic offering | −0.027 | −0.064 | — | — |
| School characteristics: dependency 2 | — | — | +0.012 | — |
| School characteristics: dependency 3 | — | −0.001 | — | −0.013 |
| School characteristics: dependency 5 | — | — | — | +0.002 |
| School characteristics: region 1 | +0.029 | — | −0.030 | — |
| School characteristics: region 2 | +0.067 | +0.147 | — | — |
| School characteristics: region 3 | +0.035 | +0.121 | −0.012 | −0.018 |
| School characteristics: region 4 | +0.038 | +0.091 | −0.028 | −0.079 |
| School characteristics: region 5 | +0.037 | +0.105 | — | +0.148 |
| School characteristics: region 6 | −0.014 | −0.083 | −0.011 | −0.255 |
| School characteristics: region 7 | — | −0.087 | — | −0.049 |
| School characteristics: region 8 | — | — | — | +0.002 |
| School characteristics: region 9 | — | −0.112 | +0.003 | −0.030 |
| School characteristics: region 10 | +0.029 | −0.033 | — | +0.013 |
| School characteristics: region 12 | — | +0.016 | — | — |
| School characteristics: region 14 | +0.001 | — | — | +0.019 |
| School characteristics: region 15 | — | +0.045 | — | — |
| School characteristics: Enrollment fee: CLP 1,000-10,000 | — | — | — | −0.003 |
| School characteristics: Enrollment fee: Above CLP 100,000 | — | +0.036 | — | — |
| School characteristics: Monthly fee: Above CLP 100,000 | +0.055 | +0.008 | — | — |
| School characteristics: Mean pre-HS grade-4 math (SD units) | +0.144 | +0.073 | — | — |
| School characteristics: Within-school SD of pre-HS grade-4 math | −0.014 | −0.008 | — | — |
| School characteristics: Mean pre-HS grade-4 language (SD units) | +0.011 | — | +0.042 | — |
| School characteristics: Within-school SD of pre-HS grade-4 language | −0.009 | — | — | −0.010 |
| School characteristics: Mean baseline household income decile (observed/imputed) | +0.049 | +0.030 | — | — |
| School characteristics: Baseline income top-two-decile share (observed/imputed) | +0.018 | +0.081 | — | — |
| School characteristics: Female student share among known sex | −0.024 | −0.002 | — | −0.065 |

## Coverage and missingness indicators

| Predictor | Math | Language | Exam taking | HE enrollment | STEM | HP field | HP institution | Income: full | Income: field | Income: institution | Program accreditation | Institution accreditation |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| HS teachers: Prior observed years in role [missing/undefined] | — | — | −0.058 | −0.045 | −0.013 | −0.009 | — | −0.009 | — | — | — | — |
| HS teachers: Cumulative observed years in role [missing/undefined] | — | — | — | — | — | — | +0.026 | — | — | — | — | — |
| HS teachers: Prior role-years at this school [missing/undefined] | — | — | −0.004 | — | — | — | — | — | — | — | — | — |
| HS teachers: Current observed role spell (years) [missing/undefined] | — | — | — | — | — | — | +0.003 | — | — | — | — | — |
| HS teachers: Current role-at-school spell (years) [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Share of prior known years in role [missing/undefined] | — | — | −<0.001 | −<0.001 | −<0.001 | −<0.001 | — | — | — | — | — | — |
| HS teachers: Always in role in observed history [missing/undefined] | — | — | — | — | −<0.001 | −0.016 | — | — | — | — | — | — |
| HS teachers: Primary-function share [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Only primary role across school appointments [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Prior main-function switching rate [missing/undefined] | −0.001 | — | −0.013 | — | — | −0.009 | — | −0.008 | — | — | — | — |
| HS teachers: Math credentials among assigned HS math teachers [missing/undefined] | — | — | −0.097 | −0.084 | — | — | — | −0.130 | — | — | — | — |
| HS teachers: Language credentials among assigned HS language teachers [missing/undefined] | — | — | −0.050 | — | — | — | — | — | — | — | — | — |
| HS teachers: VA-sample students per average staff member [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Log VA-sample students per average staff member [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Reported school tenure in 2018 [missing/undefined] | +0.011 | — | −0.005 | — | — | — | — | — | +0.009 | — | — | — |
| HS teachers: Reported system tenure in 2018 [missing/undefined] | +<0.001 | — | — | — | — | — | — | — | — | — | — | — |
| HS teachers: UG degree at high-premium institution [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Any non-UG qualification [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: As-of undergraduate database match | — | — | — | — | — | +0.004 | −0.018 | — | — | — | — | — |
| HS teachers: Observed degree with institution premium unavailable | −0.008 | −0.021 | −0.003 | −0.009 | −0.026 | −0.059 | — | −0.014 | −0.012 | — | — | −0.016 |
| Orientadores: Recorded title specialty [missing/undefined] | — | — | — | −0.010 | — | — | — | — | — | — | — | — |
| Orientadores: Longest reported degree (semesters) [missing/undefined] | — | — | — | −<0.001 | — | — | — | — | — | — | — | — |
| Orientadores: Years since earliest reported title [missing/undefined] | — | — | — | −<0.001 | — | — | — | — | — | — | — | — |
| Orientadores: VA students per average primary counselor [missing/undefined] | — | — | −0.033 | — | — | — | — | — | — | — | — | — |
| Orientadores: Log VA students per average primary counselor [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| Orientadores: As-of undergraduate database match | — | — | — | — | — | +0.005 | — | — | — | — | — | — |
| Orientadores: Observed degree with institution premium unavailable | — | — | — | +0.015 | — | +0.007 | −0.005 | +0.001 | — | — | — | — |
| Orientadores: Mean within-year age SD (years with 2+ valid ages) [missing/undefined] | — | — | — | — | — | −0.011 | — | −0.002 | — | — | — | — |
| Leadership: Cumulative primary-function leadership years [missing/undefined] | — | — | — | — | — | — | — | +0.032 | — | — | — | — |
| Leadership: Prior leadership years at this school [missing/undefined] | — | — | +0.011 | — | — | +<0.001 | — | +0.003 | — | — | — | — |
| Leadership: Current leadership spell [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Current school-leadership spell [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Primary-function leadership share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Prior detailed main-function change rate [missing/undefined] | — | — | +0.001 | — | — | — | — | — | — | — | — | — |
| Leadership: Prior main-function leadership entry/exit rate [missing/undefined] | — | — | +0.003 | — | — | — | — | — | — | — | — | — |
| Leadership: Planta Directiva (3) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Director (4) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Directiva (10) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Subdirector (15) share [missing/undefined] | — | — | — | — | — | — | — | +0.002 | — | — | — | — |
| Leadership: Only primary leadership across school appointments [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: VA-sample students per average leader [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Log VA-sample students per average leader [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Reported school tenure in 2018 [missing/undefined] | — | — | −0.011 | — | — | — | — | — | — | — | — | — |
| Leadership: UG degree at high-premium institution [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Any as-of qualification database match | — | — | +0.003 | — | +0.011 | +0.005 | +0.001 | +0.007 | — | — | — | — |
| Leadership: Observed degree with institution premium unavailable | — | — | — | −0.004 | — | — | — | — | — | — | — | — |
| Leadership: Valid birthdate share, active-year average | −0.025 | — | — | +<0.001 | — | — | — | — | — | — | — | — |
| Leadership: Valid birthdate share, active-year average [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Share of active staff years with any valid age | — | — | — | +0.005 | +0.006 | +0.010 | — | +0.013 | — | — | — | — |
| Leadership: Share of active staff years with any valid age [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| School characteristics: Enrollment fee: No information | — | — | −0.002 | — | — | −0.004 | — | −0.001 | — | — | — | — |
| School characteristics: Within-school SD of pre-HS grade-4 math [missing/undefined] | — | — | +0.100 | +0.030 | +0.027 | +0.029 | +0.002 | +0.072 | — | — | — | — |
| School characteristics: Within-school SD of pre-HS grade-4 language [missing/undefined] | — | — | +0.017 | +0.016 | — | +<0.001 | +0.003 | — | — | — | — | — |
| School characteristics: Baseline household income imputed share | — | +0.003 | −0.010 | −0.005 | — | — | — | −0.003 | — | — | — | — |
| School characteristics: Father education imputed share | — | — | −0.019 | −0.017 | — | — | +0.003 | −0.012 | — | — | — | — |
| School characteristics: Mother education imputed share | — | — | −0.024 | −0.032 | — | −0.028 | +0.017 | −0.017 | — | — | — | — |
| School characteristics: Public funding per student, 2017-2021 (million 2021 CLP) [missing/undefined] | — | — | −0.013 | — | — | — | — | — | — | — | — | — |
| School characteristics: Log public funding per student, 2017-2021 [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| School characteristics: Public funding change: 2019-2021 vs 2017-2018 (million 2021 CLP/student) [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| School characteristics: Share of 2017-2021 years with funding records | — | — | +0.030 | — | — | — | — | −0.005 | — | — | — | −0.029 |

## Minimum-error penalty sensitivity

| Outcome | Schools | Selected | School-only R² | Joint R² | Gain in R² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Math | 3,373 | 77 | 0.559 | 0.576 | +0.017 |
| Language | 3,381 | 73 | 0.555 | 0.563 | +0.009 |
| Exam taking | 3,682 | 152 | 0.574 | 0.648 | +0.074 |
| HE enrollment | 3,675 | 126 | 0.346 | 0.371 | +0.025 |
| STEM | 3,682 | 122 | 0.099 | 0.099 | +0.000 |
| HP field | 3,680 | 149 | 0.156 | 0.148 | -0.007 |
| HP institution | 3,682 | 143 | 0.591 | 0.645 | +0.054 |
| Income: full | 3,682 | 138 | 0.485 | 0.500 | +0.014 |
| Income: field | 3,375 | 79 | 0.174 | 0.165 | -0.009 |
| Income: institution | 3,375 | 46 | 0.299 | 0.302 | +0.002 |
| Program accreditation | 3,375 | 61 | 0.125 | 0.126 | +0.001 |
| Institution accreditation | 3,375 | 45 | 0.170 | 0.164 | -0.005 |

## Definitions and interpretation

- One Gaussian Lasso per saved All-sample EB VA outcome. All three staff groups enter jointly; schools receive equal weight.
- Main coefficients use the one-standard-error penalty. Each entry is outcome SD per predictor SD, conditional on all selected predictors. Dashes mean zero coefficients, not unavailable outcomes. No significance stars are used.
- Staff components and as-of credential shares refer to 2018–2024; observed career histories start in 2013. Teachers are HS-assigned classroom teachers. Orientadores and leaders can hold primary or secondary roles.
- The 212 candidates comprise 58 school, 49 teacher, 52 orientador and 53 leadership measures. Each also has a missingness indicator; training-constant columns are removed. Composite indices and alternative-history versions are excluded; broken post-2018 reported-tenure averages are excluded.
- School-only and joint models both include separate enrollment-fee and monthly-fee indicators from the 2024 MINEDUC directory. Free is the reference for each; paid bands are CLP 1,000–10,000, 10,001–25,000, 25,001–50,000, 50,001–100,000 and above 100,000. No information is a separate category. These self-reported bands are not exact prices; no midpoint, top-code amount or annual-cost scalar is assigned.
- Absent roles and incomplete rosters have explicit indicators. Undefined or missing characteristics use training-only median placeholders plus missingness indicators, not a claim of zero qualifications. Qualification database coverage and missing institution-premium coverage are separate predictors.
- Student/staff ratios use pooled VA-sample students and average annual staff headcount. They are not class sizes, annual HS enrollment ratios, FTE measures or counselor caseloads.
- Resources enter both models: public funding per student in 2021 pesos, its log, and change from 2017-2018 to 2019-2021. Levels use total funding divided by summed annual average enrollment. All five complete years are required; missing funding is not zero. This is public funding, not total spending or an HS-specific budget. Funding coverage enters separately.
- Composition enters both models and pools the 757,999 students in the saved broad VA estimation sample (2017-2020 grade-8 cohorts): pre-HS grade-4 math/language means and SDs, household income decile and low/high-income shares, parental education, sex and grade-8 age. Existing baseline imputations are retained with their imputation shares; they are not reestimated within Lasso folds. Assignment is to the VA school, not necessarily the first HS attended.
- Staff ages enter the joint model: mean, within-year SD, under-35 and 50+ shares for each role. Ages are attained during each staff year, 2018-2024, using that year’s birthdate records. Active-year summaries are equally weighted; incomplete rosters and years without any valid ages do not silently disappear. Coverage enters separately. These overlapping/post-entry windows support descriptive prediction, not causal input effects.
- Prediction uses five outer school folds and five inner tuning folds. Every imputation, scaling step and penalty choice excludes the held-out outer schools. The school-only baseline is separately tuned on identical schools and folds. Its penalty range extends to zero after a boundary audit; joint-model minima were interior.
- R² is pooled out-of-fold 1 − SSE/SST; negative values are possible. Coefficients instead come from the separately tuned full-sample refit. The minimum-error penalty is a sensitivity; selection frequencies count the five outer fits, not independent replications.
- Lasso may select one of several correlated measures and omit another. A zero does not establish irrelevance; signs are conditional predictive associations, not causal hiring effects or validated staff quality.
- Validation treats the saved EB VA estimates and institution-premium definitions as fixed, without reestimating them in folds or propagating their uncertainty. Schools sharing staff are not grouped into common folds. These are held-out-school predictions of saved estimates, not forecasts validated on future cohorts.
- Independent checks reconstruct all new composition/resource/age aggregates, including ages directly from raw annual birthdates, all role credential aggregates, all 240 outer-model predictions, all 288 fitted-model KKT conditions, tuning choices, selection frequencies and performance statistics. Raw inputs and source VA hashes are unchanged.

Estimator documentation: [glmnet](https://glmnet.stanford.edu/articles/glmnet.html).
