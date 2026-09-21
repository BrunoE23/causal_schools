# Staff characteristics and school VA: joint Lasso

## Out-of-sample prediction

| Outcome | Schools | Selected | School-only R² | Joint R² | Gain in R² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Math | 3,373 | 45 | 0.401 | 0.465 | +0.065 |
| Language | 3,381 | 35 | 0.442 | 0.477 | +0.035 |
| Exam taking | 3,682 | 76 | 0.462 | 0.609 | +0.147 |
| HE enrollment | 3,675 | 71 | 0.277 | 0.334 | +0.057 |
| STEM | 3,682 | 21 | 0.045 | 0.055 | +0.010 |
| HP field | 3,680 | 86 | 0.106 | 0.100 | -0.005 |
| HP institution | 3,682 | 42 | 0.510 | 0.598 | +0.087 |
| Income: full | 3,682 | 76 | 0.424 | 0.458 | +0.034 |
| Income: field | 3,375 | 21 | 0.110 | 0.116 | +0.006 |
| Income: institution | 3,375 | 21 | 0.268 | 0.278 | +0.010 |
| Program accreditation | 3,375 | 11 | 0.092 | 0.097 | +0.004 |
| Institution accreditation | 3,375 | 21 | 0.133 | 0.135 | +0.002 |

## Main coefficients

Standardized coefficients; one-standard-error penalty. Only predictors selected in at least one displayed outcome appear.

### Outcomes 1–4

| Predictor | Math | Language | Exam taking | HE enrollment |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Cumulative observed years in role | +0.012 | — | — | — |
| HS teachers: Primary-function share | +0.001 | — | — | −0.007 |
| HS teachers: Prior main-function switching rate | −0.016 | — | — | — |
| HS teachers: University tertiary qualification | +0.021 | +0.049 | — | +0.001 |
| HS teachers: HS teaching qualification | +0.016 | +0.071 | +0.046 | — |
| HS teachers: Tertiary qualification | +0.036 | — | — | — |
| HS teachers: Recorded title specialty | — | — | +0.044 | +0.021 |
| HS teachers: CFT-trained share | — | — | −0.039 | −0.029 |
| HS teachers: Other institution type share | — | — | −0.034 | −0.049 |
| HS teachers: Longest reported degree (semesters) | — | — | +0.024 | +0.019 |
| HS teachers: Years since earliest reported title | +0.015 | — | — | −0.033 |
| HS teachers: Math credentials among assigned HS math teachers | — | +0.008 | +0.007 | — |
| HS teachers: Language credentials among assigned HS language teachers | +0.020 | +0.012 | — | — |
| HS teachers: Mean annual staff headcount | +0.031 | +0.029 | +0.077 | +0.122 |
| HS teachers: Share of years with staff present | −0.026 | — | +0.285 | +0.170 |
| HS teachers: Staff per 1,000 VA-sample students | −0.006 | — | +0.011 | +0.003 |
| HS teachers: VA-sample students per average staff member | −0.002 | — | +0.001 | +0.050 |
| HS teachers: Log VA-sample students per average staff member | — | — | +0.165 | +0.094 |
| HS teachers: Reported school tenure in 2018 | — | +0.005 | +0.017 | — |
| HS teachers: Role absent throughout 2018-2024 | — | — | −0.144 | −0.067 |
| HS teachers: UG degree at high-premium institution | +0.044 | +0.007 | — | — |
| HS teachers: Any non-UG qualification | +0.043 | +0.027 | +0.004 | — |
| HS teachers: Non-UG qualification at high-premium institution | +0.014 | +0.022 | — | +0.008 |
| HS teachers: Any degree at high-premium institution | — | +0.011 | +<0.001 | — |
| HS teachers: Role-specific qualification | — | — | +0.060 | — |
| HS teachers: Any magister | +0.043 | +0.045 | — | — |
| HS teachers: Magister at high-premium institution | +0.018 | +0.012 | — | — |
| Orientadores: Only primary role across school appointments | — | — | — | −0.017 |
| Orientadores: Prior main-function switching rate | — | — | +0.004 | +<0.001 |
| Orientadores: Tertiary qualification | — | — | +0.020 | +0.022 |
| Orientadores: IP-trained share | — | — | −0.003 | −0.005 |
| Orientadores: Other institution type share | — | — | +<0.001 | — |
| Orientadores: Longest reported degree (semesters) | +0.002 | — | — | — |
| Orientadores: Years since earliest reported title | — | — | −0.019 | −0.003 |
| Orientadores: Log VA-sample students per average staff member | — | — | +<0.001 | — |
| Orientadores: Reported school tenure in 2018 | — | — | +0.004 | — |
| Orientadores: VA students per average primary counselor | — | — | −0.011 | — |
| Orientadores: Log VA students per average primary counselor | −0.001 | — | — | +0.011 |
| Orientadores: Role absent throughout 2018-2024 | — | — | — | −0.015 |
| Orientadores: UG degree at high-premium institution | — | — | −0.002 | — |
| Orientadores: Non-UG qualification at high-premium institution | — | — | +0.002 | — |
| Orientadores: Role-specific qualification | — | — | — | +0.004 |
| Leadership: Current school-leadership spell | — | +0.003 | — | +0.017 |
| Leadership: Prior detailed main-function change rate | −0.004 | −<0.001 | −0.004 | −0.001 |
| Leadership: University tertiary qualification | — | — | — | +0.001 |
| Leadership: Teaching qualification | — | — | +0.010 | +0.006 |
| Leadership: Tertiary qualification | — | — | — | +0.002 |
| Leadership: Recorded title specialty | — | — | — | −0.007 |
| Leadership: CFT-trained share | — | — | +0.007 | — |
| Leadership: Director (4) share | — | — | +0.018 | — |
| Leadership: Subdirector (15) share | — | +0.008 | — | — |
| Leadership: Mean primary leadership headcount | +0.095 | +0.056 | — | — |
| Leadership: Share of years with leadership present | — | — | −0.037 | −0.022 |
| Leadership: Leaders per 1,000 VA-sample students | — | — | +0.153 | +0.072 |
| Leadership: VA-sample students per average leader | — | — | +0.015 | +0.008 |
| Leadership: Log VA-sample students per average leader | — | — | −0.088 | −0.093 |
| Leadership: Reported school tenure in 2018 | — | — | −0.002 | — |
| Leadership: Role absent throughout 2018-2024 | — | — | +0.022 | +0.013 |
| Leadership: Any non-UG qualification | — | — | — | +0.020 |
| Leadership: Non-UG qualification at high-premium institution | — | +0.019 | +<0.001 | +0.006 |
| Leadership: Any degree at high-premium institution | +0.041 | — | — | — |
| Leadership: Any magister | — | — | +0.018 | — |
| Leadership: Magister at high-premium institution | +0.007 | — | — | — |
| Leadership: Role-specific magister | — | — | — | +0.008 |
| School context: Log VA-sample student count | — | — | −0.095 | −0.081 |
| School context: Technical-professional/artistic offering | −0.151 | −0.232 | −0.211 | −0.183 |
| School context: Basic-education offering | +0.023 | +0.063 | +0.184 | +0.150 |
| School context: dependency 2 | −0.002 | −0.001 | — | — |
| School context: dependency 3 | — | — | — | +0.015 |
| School context: Private paid school (dependency 4) | +0.103 | +0.098 | +0.012 | +0.051 |
| School context: dependency 5 | −0.003 | −0.003 | −0.022 | — |
| School context: region 1 | — | −0.004 | — | +0.018 |
| School context: region 2 | — | — | — | +0.049 |
| School context: region 3 | — | — | +0.002 | +0.037 |
| School context: region 4 | — | — | +0.026 | +0.079 |
| School context: region 5 | −0.006 | — | +0.025 | +0.060 |
| School context: region 6 | — | — | −0.012 | −0.030 |
| School context: region 7 | +0.027 | +0.018 | +0.018 | +0.028 |
| School context: region 8 | — | — | — | +0.047 |
| School context: region 9 | +0.036 | +0.052 | +0.013 | +0.027 |
| School context: region 10 | +0.030 | +0.037 | −0.001 | +0.037 |
| School context: region 11 | — | +0.005 | — | +0.011 |
| School context: region 12 | — | — | — | +0.056 |
| School context: region 14 | — | +0.012 | +0.015 | +0.039 |
| School context: region 15 | — | — | +0.009 | +0.027 |
| School context: region 16 | +0.027 | — | +0.013 | +0.058 |
| School context: Enrollment fee: CLP 1,000-10,000 | — | +0.013 | — | +0.012 |
| School context: Enrollment fee: CLP 50,001-100,000 | — | — | — | +<0.001 |
| School context: Enrollment fee: Above CLP 100,000 | +0.215 | +0.115 | — | — |
| School context: Monthly fee: CLP 1,000-10,000 | — | — | −0.002 | −0.006 |
| School context: Monthly fee: CLP 25,001-50,000 | — | — | +0.028 | +0.025 |
| School context: Monthly fee: CLP 50,001-100,000 | — | — | +0.055 | +0.032 |
| School context: Monthly fee: Above CLP 100,000 | +0.142 | +0.171 | +0.055 | — |

### Outcomes 5–8

| Predictor | STEM | HP field | HP institution | Income: full |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Cumulative observed years in role | — | — | +0.015 | — |
| HS teachers: Prior role-years at this school | — | +0.001 | — | — |
| HS teachers: Primary-function share | — | — | — | −0.001 |
| HS teachers: Only primary role across school appointments | — | — | +0.004 | — |
| HS teachers: Prior main-function switching rate | — | −0.004 | — | — |
| HS teachers: University tertiary qualification | — | — | — | +0.020 |
| HS teachers: Teaching qualification | −0.059 | −0.017 | — | — |
| HS teachers: HS teaching qualification | — | — | — | +0.052 |
| HS teachers: IP-trained share | +0.011 | +0.003 | — | — |
| HS teachers: CFT-trained share | — | — | — | −0.035 |
| HS teachers: Normal-school-trained share | — | −0.016 | — | — |
| HS teachers: Other institution type share | +0.029 | +0.019 | — | — |
| HS teachers: Longest reported degree (semesters) | −0.003 | −0.006 | — | +0.009 |
| HS teachers: Years since earliest reported title | — | — | +0.056 | — |
| HS teachers: Math credentials among assigned HS math teachers | −0.001 | −0.010 | — | −0.002 |
| HS teachers: Language credentials among assigned HS language teachers | — | — | — | +0.001 |
| HS teachers: Mean annual staff headcount | +0.105 | +0.114 | — | +0.116 |
| HS teachers: Share of years with staff present | — | — | −0.014 | — |
| HS teachers: Staff per 1,000 VA-sample students | — | — | — | +0.006 |
| HS teachers: VA-sample students per average staff member | — | — | −0.007 | +0.008 |
| HS teachers: Log VA-sample students per average staff member | — | +0.027 | — | +0.107 |
| HS teachers: Reported school tenure in 2018 | — | +0.030 | — | — |
| HS teachers: Reported system tenure in 2018 | — | — | — | +0.004 |
| HS teachers: Role absent throughout 2018-2024 | — | −0.022 | — | −0.038 |
| HS teachers: UG degree at high-premium institution | — | −0.018 | +0.202 | +0.008 |
| HS teachers: Any non-UG qualification | — | +0.008 | — | +0.012 |
| HS teachers: Non-UG qualification at high-premium institution | — | — | +0.028 | — |
| HS teachers: Any degree at high-premium institution | — | — | +0.152 | +0.078 |
| HS teachers: Any magister | — | — | — | +0.012 |
| HS teachers: Magister at high-premium institution | — | −0.007 | +0.028 | — |
| HS teachers: Role-specific magister | — | +0.011 | — | +0.002 |
| Orientadores: Only primary role across school appointments | — | — | — | −0.002 |
| Orientadores: University tertiary qualification | — | +0.016 | — | — |
| Orientadores: Tertiary qualification | — | — | — | +0.008 |
| Orientadores: Recorded title specialty | — | +0.014 | — | — |
| Orientadores: CFT-trained share | — | −<0.001 | — | — |
| Orientadores: Other institution type share | — | +0.006 | — | +0.004 |
| Orientadores: Longest reported degree (semesters) | — | +0.001 | — | +<0.001 |
| Orientadores: Years since earliest reported title | — | −<0.001 | — | −0.005 |
| Orientadores: Orientation mention among applicable titles | — | +0.002 | — | — |
| Orientadores: VA-sample students per average staff member | — | — | −0.017 | −0.008 |
| Orientadores: Reported school tenure in 2018 | — | −0.014 | — | — |
| Orientadores: VA students per average primary counselor | +0.008 | +0.005 | — | — |
| Orientadores: Role absent throughout 2018-2024 | — | — | — | −0.001 |
| Orientadores: Non-UG qualification at high-premium institution | — | +0.001 | — | — |
| Orientadores: Role-specific qualification | +0.009 | +0.017 | — | +0.004 |
| Leadership: Prior primary-or-secondary leadership years | — | −0.008 | — | — |
| Leadership: Primary-function leadership share | — | +0.006 | — | — |
| Leadership: Prior detailed main-function change rate | — | — | — | −0.005 |
| Leadership: Prior main-function leadership entry/exit rate | — | +0.006 | — | — |
| Leadership: Recorded title specialty | — | −0.010 | — | −0.004 |
| Leadership: CFT-trained share | — | −0.008 | — | — |
| Leadership: Other institution type share | — | +0.001 | — | — |
| Leadership: Longest degree (semesters) | — | −0.003 | — | — |
| Leadership: Directiva (10) share | +0.005 | +0.012 | — | — |
| Leadership: Only primary leadership across school appointments | — | −0.007 | — | — |
| Leadership: Mean annual leadership headcount | — | — | +0.032 | — |
| Leadership: Mean primary leadership headcount | +0.027 | +0.052 | +0.033 | +0.042 |
| Leadership: Share of years with leadership present | — | — | — | −0.011 |
| Leadership: Leaders per 1,000 VA-sample students | +0.016 | +0.024 | — | +0.060 |
| Leadership: VA-sample students per average leader | — | — | −0.002 | — |
| Leadership: Log VA-sample students per average leader | — | — | — | −0.057 |
| Leadership: Reported system tenure in 2018 | — | +0.002 | — | — |
| Leadership: Role absent throughout 2018-2024 | — | +0.006 | — | — |
| Leadership: UG degree at high-premium institution | +0.010 | +0.007 | — | — |
| Leadership: Any non-UG qualification | — | +0.004 | — | — |
| Leadership: Non-UG qualification at high-premium institution | — | +0.017 | +0.007 | +0.034 |
| Leadership: Any degree at high-premium institution | — | — | — | +0.002 |
| Leadership: Role-specific qualification | — | — | — | +0.001 |
| Leadership: Any magister | — | +0.001 | — | +0.004 |
| Leadership: Magister at high-premium institution | — | +0.001 | +0.011 | — |
| Leadership: Role-specific magister | — | +0.031 | — | +0.020 |
| School context: Log VA-sample student count | — | −0.063 | — | −0.058 |
| School context: Rural school | — | — | +0.010 | — |
| School context: Technical-professional/artistic offering | — | −0.089 | −0.036 | −0.166 |
| School context: Basic-education offering | — | +0.087 | +<0.001 | +0.150 |
| School context: dependency 2 | — | −0.017 | — | −0.003 |
| School context: dependency 3 | — | — | −0.016 | +0.011 |
| School context: Private paid school (dependency 4) | — | — | +0.066 | +0.091 |
| School context: dependency 5 | +0.087 | +0.048 | — | — |
| School context: region 1 | +0.013 | +0.048 | −0.010 | +0.014 |
| School context: region 2 | +0.028 | +0.078 | +0.114 | +0.068 |
| School context: region 3 | +0.031 | +0.073 | +0.113 | +0.061 |
| School context: region 4 | +0.034 | +0.081 | +0.018 | +0.074 |
| School context: region 5 | +0.079 | +0.106 | — | +0.059 |
| School context: region 6 | −0.027 | −0.004 | −0.048 | −0.029 |
| School context: region 7 | — | +0.016 | −0.039 | — |
| School context: region 8 | — | +0.055 | −0.049 | — |
| School context: region 9 | — | +0.015 | −0.050 | — |
| School context: region 10 | — | +0.058 | −0.044 | +0.016 |
| School context: region 11 | — | +0.034 | — | — |
| School context: region 12 | — | +0.026 | −0.040 | +0.016 |
| School context: region 14 | — | +0.078 | −0.039 | +0.035 |
| School context: region 15 | — | +0.031 | — | +0.011 |
| School context: region 16 | — | +0.003 | −0.016 | +0.001 |
| School context: Enrollment fee: CLP 1,000-10,000 | — | — | — | +0.011 |
| School context: Enrollment fee: CLP 10,001-25,000 | — | — | −0.002 | — |
| School context: Enrollment fee: CLP 25,001-50,000 | — | −0.002 | — | −0.001 |
| School context: Enrollment fee: Above CLP 100,000 | — | +0.009 | +0.212 | +0.052 |
| School context: Monthly fee: CLP 1,000-10,000 | — | — | — | −0.005 |
| School context: Monthly fee: CLP 10,001-25,000 | — | +0.002 | — | +0.001 |
| School context: Monthly fee: CLP 25,001-50,000 | — | +0.026 | — | +0.043 |
| School context: Monthly fee: CLP 50,001-100,000 | — | +0.063 | — | +0.086 |
| School context: Monthly fee: Above CLP 100,000 | — | +0.118 | +0.110 | +0.170 |

### Outcomes 9–12

| Predictor | Income: field | Income: institution | Program accreditation | Institution accreditation |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Only primary role across school appointments | — | — | — | +0.005 |
| HS teachers: University tertiary qualification | +0.017 | — | — | — |
| HS teachers: Teaching qualification | — | — | +0.142 | — |
| HS teachers: IP-trained share | −0.027 | −0.007 | — | — |
| HS teachers: CFT-trained share | −0.006 | — | — | — |
| HS teachers: VA-sample students per average staff member | — | — | −0.014 | −0.001 |
| HS teachers: Any non-UG qualification | +0.005 | — | — | +0.024 |
| HS teachers: Non-UG qualification at high-premium institution | — | — | — | +0.003 |
| HS teachers: Any degree at high-premium institution | — | +0.117 | — | — |
| HS teachers: Any magister | +0.003 | — | — | — |
| Orientadores: VA-sample students per average staff member | — | −0.005 | — | — |
| Leadership: Mean primary leadership headcount | +0.025 | +0.033 | — | +0.036 |
| Leadership: VA-sample students per average leader | — | — | −0.084 | — |
| Leadership: Any degree at high-premium institution | +0.012 | +0.011 | — | +0.007 |
| Leadership: Role-specific magister | — | — | — | +<0.001 |
| School context: Technical-professional/artistic offering | −0.075 | −0.102 | — | — |
| School context: Basic-education offering | +0.025 | +0.020 | — | — |
| School context: dependency 2 | — | — | +0.004 | — |
| School context: dependency 3 | — | — | — | −0.021 |
| School context: Private paid school (dependency 4) | +0.009 | +0.022 | — | — |
| School context: dependency 5 | — | — | −0.001 | +0.013 |
| School context: region 1 | +0.027 | — | −0.030 | — |
| School context: region 2 | +0.070 | +0.143 | — | — |
| School context: region 3 | +0.033 | +0.114 | −0.012 | −0.016 |
| School context: region 4 | +0.036 | +0.087 | −0.028 | −0.077 |
| School context: region 5 | +0.025 | +0.099 | — | +0.143 |
| School context: region 6 | −0.015 | −0.082 | −0.012 | −0.251 |
| School context: region 7 | — | −0.083 | — | −0.046 |
| School context: region 8 | — | — | — | +0.001 |
| School context: region 9 | — | −0.112 | +0.003 | −0.027 |
| School context: region 10 | +0.031 | −0.028 | — | +0.009 |
| School context: region 12 | — | +0.012 | — | — |
| School context: region 14 | — | — | — | +0.014 |
| School context: region 15 | — | +0.043 | — | — |
| School context: Enrollment fee: CLP 1,000-10,000 | — | — | — | −0.004 |
| School context: Enrollment fee: Above CLP 100,000 | +0.025 | +0.068 | +0.008 | +0.019 |
| School context: Monthly fee: CLP 50,001-100,000 | +0.019 | — | — | — |
| School context: Monthly fee: Above CLP 100,000 | +0.143 | +0.075 | — | — |

## Coverage and missingness indicators

| Predictor | Math | Language | Exam taking | HE enrollment | STEM | HP field | HP institution | Income: full | Income: field | Income: institution | Program accreditation | Institution accreditation |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| HS teachers: Prior observed years in role [missing/undefined] | — | — | −0.059 | −0.053 | — | −0.021 | — | −0.021 | — | — | — | — |
| HS teachers: Cumulative observed years in role [missing/undefined] | — | — | — | — | — | — | +0.041 | — | — | — | — | — |
| HS teachers: Prior role-years at this school [missing/undefined] | −0.015 | −0.019 | −0.008 | — | — | — | — | — | — | — | — | — |
| HS teachers: Current observed role spell (years) [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Current role-at-school spell (years) [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Share of prior known years in role [missing/undefined] | — | — | −0.001 | −<0.001 | — | −<0.001 | — | −0.009 | — | — | — | — |
| HS teachers: Always in role in observed history [missing/undefined] | — | — | — | — | — | −0.021 | — | — | — | — | — | — |
| HS teachers: Primary-function share [missing/undefined] | — | — | — | — | — | — | +0.006 | — | — | — | — | — |
| HS teachers: Only primary role across school appointments [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Prior main-function switching rate [missing/undefined] | −0.017 | −0.014 | −<0.001 | — | — | −0.016 | — | −0.015 | — | — | — | — |
| HS teachers: Recorded title specialty [missing/undefined] | — | — | — | — | — | −0.002 | — | — | — | — | — | — |
| HS teachers: Years since earliest reported title [missing/undefined] | — | — | — | — | — | −<0.001 | — | — | — | — | — | — |
| HS teachers: Math credentials among assigned HS math teachers [missing/undefined] | — | — | −0.119 | −0.125 | — | −<0.001 | — | −0.190 | — | — | — | — |
| HS teachers: Language credentials among assigned HS language teachers [missing/undefined] | — | — | −0.080 | −0.019 | — | — | — | — | — | — | — | — |
| HS teachers: Reported school tenure in 2018 [missing/undefined] | +0.016 | — | −0.016 | — | — | — | — | — | — | — | — | — |
| HS teachers: Reported system tenure in 2018 [missing/undefined] | +<0.001 | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| HS teachers: As-of undergraduate database match | — | — | — | +0.028 | — | +0.025 | −0.056 | — | — | — | — | — |
| HS teachers: Observed degree with institution premium unavailable | −0.050 | −0.052 | −0.022 | −0.026 | −0.024 | −0.078 | — | −0.036 | −0.032 | −0.001 | — | −0.018 |
| Orientadores: Prior observed years in role [missing/undefined] | +0.012 | — | — | — | — | — | — | — | — | — | — | — |
| Orientadores: Prior role-years at this school [missing/undefined] | — | — | — | — | — | +0.004 | — | — | — | — | — | — |
| Orientadores: Share of prior known years in role [missing/undefined] | +<0.001 | — | — | — | — | — | — | — | — | — | — | — |
| Orientadores: Always in role in observed history [missing/undefined] | +<0.001 | — | — | — | — | — | — | — | — | — | — | — |
| Orientadores: Orientation mention among applicable titles [missing/undefined] | +0.003 | +0.003 | — | — | — | — | — | — | — | — | — | — |
| Orientadores: VA students per average primary counselor [missing/undefined] | +<0.001 | — | −0.031 | −0.003 | — | — | — | — | — | — | — | — |
| Orientadores: Log VA students per average primary counselor [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| Orientadores: As-of undergraduate database match | — | — | +<0.001 | — | — | +0.013 | — | — | — | — | — | — |
| Orientadores: Observed degree with institution premium unavailable | — | — | — | +0.016 | — | +0.010 | −<0.001 | — | — | — | — | — |
| Leadership: Cumulative primary-function leadership years [missing/undefined] | — | — | — | — | — | +<0.001 | — | +0.052 | — | — | — | — |
| Leadership: Prior leadership years at this school [missing/undefined] | — | — | +0.001 | — | — | — | — | — | — | — | — | — |
| Leadership: Current leadership spell [missing/undefined] | — | — | — | — | — | +<0.001 | — | +<0.001 | — | — | — | — |
| Leadership: Current school-leadership spell [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Primary-function leadership share [missing/undefined] | — | — | — | — | — | — | — | +0.002 | — | — | — | — |
| Leadership: Prior detailed main-function change rate [missing/undefined] | — | — | +0.010 | — | — | — | — | — | — | — | — | — |
| Leadership: Prior main-function leadership entry/exit rate [missing/undefined] | — | — | +<0.001 | — | — | — | — | — | — | — | — | — |
| Leadership: Planta Directiva (3) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Director (4) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Directiva (10) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Reported school tenure in 2018 [missing/undefined] | — | — | −0.015 | — | — | — | — | — | — | — | — | — |
| Leadership: Reported system tenure in 2018 [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| Leadership: Any as-of qualification database match | — | — | +0.010 | +0.004 | +0.006 | — | — | +0.009 | — | — | — | — |
| Leadership: Observed degree with institution premium unavailable | — | — | −0.003 | −0.015 | — | — | — | −0.001 | — | — | — | — |

## Minimum-error penalty sensitivity

| Outcome | Schools | Selected | School-only R² | Joint R² | Gain in R² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Math | 3,373 | 117 | 0.421 | 0.488 | +0.067 |
| Language | 3,381 | 79 | 0.459 | 0.495 | +0.036 |
| Exam taking | 3,682 | 169 | 0.474 | 0.620 | +0.145 |
| HE enrollment | 3,675 | 111 | 0.292 | 0.352 | +0.060 |
| STEM | 3,682 | 97 | 0.080 | 0.090 | +0.010 |
| HP field | 3,680 | 140 | 0.134 | 0.133 | -0.001 |
| HP institution | 3,682 | 137 | 0.531 | 0.612 | +0.081 |
| Income: full | 3,682 | 138 | 0.442 | 0.479 | +0.037 |
| Income: field | 3,375 | 84 | 0.146 | 0.144 | -0.002 |
| Income: institution | 3,375 | 40 | 0.283 | 0.293 | +0.010 |
| Program accreditation | 3,375 | 44 | 0.114 | 0.119 | +0.005 |
| Institution accreditation | 3,375 | 43 | 0.159 | 0.156 | -0.003 |

## Definitions and interpretation

- One Gaussian Lasso per saved All-sample EB VA outcome. All three staff groups enter jointly; schools receive equal weight.
- Main coefficients use the one-standard-error penalty. Each entry is outcome SD per predictor SD, conditional on all selected predictors. Dashes mean zero coefficients, not unavailable outcomes. No significance stars are used.
- Staff components and as-of credential shares refer to 2018–2024; observed career histories start in 2013. Teachers are HS-assigned classroom teachers. Orientadores and leaders can hold primary or secondary roles.
- The 175 candidates comprise 39 school, 43 teacher, 46 orientador and 47 leadership measures. Each also has a missingness indicator; training-constant columns are removed. Composite indices and alternative-history versions are excluded; broken post-2018 reported-tenure averages are excluded.
- School-only and joint models both include separate enrollment-fee and monthly-fee indicators from the 2024 MINEDUC directory. Free is the reference for each; paid bands are CLP 1,000–10,000, 10,001–25,000, 25,001–50,000, 50,001–100,000 and above 100,000. No information is a separate category. These self-reported bands are not exact prices; no midpoint, top-code amount or annual-cost scalar is assigned.
- Absent roles and incomplete rosters have explicit indicators. Undefined or missing characteristics use training-only median placeholders plus missingness indicators, not a claim of zero qualifications. Qualification database coverage and missing institution-premium coverage are separate predictors.
- Student/staff ratios use pooled VA-sample students and average annual staff headcount. They are not class sizes, annual HS enrollment ratios, FTE measures or counselor caseloads. No funding or 2024-only age measure is included.
- Prediction uses five outer school folds and five inner tuning folds. Every imputation, scaling step and penalty choice excludes the held-out outer schools. The school-only baseline is separately tuned on identical schools and folds. Its penalty range extends to zero after a boundary audit; joint-model minima were interior.
- R² is pooled out-of-fold 1 − SSE/SST; negative values are possible. Coefficients instead come from the separately tuned full-sample refit. The minimum-error penalty is a sensitivity; selection frequencies count the five outer fits, not independent replications.
- Lasso may select one of several correlated measures and omit another. A zero does not establish irrelevance; signs are conditional predictive associations, not causal hiring effects or validated staff quality.
- Validation treats the saved EB VA estimates and institution-premium definitions as fixed, without reestimating them in folds or propagating their uncertainty. Schools sharing staff are not grouped into common folds. These are held-out-school predictions of saved estimates, not forecasts validated on future cohorts.
- Independent checks reconstruct all role credential aggregates, all 240 outer-model predictions, all 288 fitted-model KKT conditions, tuning choices, selection frequencies and performance statistics. Raw inputs and source VA hashes are unchanged.

Estimator documentation: [glmnet](https://glmnet.stanford.edu/articles/glmnet.html).
