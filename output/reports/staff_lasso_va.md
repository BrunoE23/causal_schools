# Staff characteristics and school VA: joint Lasso

## Out-of-sample prediction

| Outcome | Schools | Selected | School-only R² | Joint R² | Gain in R² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Math | 3,373 | 47 | 0.365 | 0.446 | +0.081 |
| Language | 3,381 | 27 | 0.412 | 0.458 | +0.046 |
| Exam taking | 3,682 | 85 | 0.450 | 0.607 | +0.157 |
| HE enrollment | 3,675 | 67 | 0.269 | 0.332 | +0.063 |
| STEM | 3,682 | 21 | 0.047 | 0.055 | +0.008 |
| HP field | 3,680 | 85 | 0.095 | 0.094 | -0.001 |
| HP institution | 3,682 | 42 | 0.476 | 0.578 | +0.103 |
| Income: full | 3,682 | 66 | 0.398 | 0.441 | +0.043 |
| Income: field | 3,375 | 17 | 0.098 | 0.105 | +0.007 |
| Income: institution | 3,375 | 20 | 0.264 | 0.274 | +0.010 |
| Program accreditation | 3,375 | 10 | 0.092 | 0.094 | +0.003 |
| Institution accreditation | 3,375 | 18 | 0.131 | 0.133 | +0.003 |

## Main coefficients

Standardized coefficients; one-standard-error penalty. Only predictors selected in at least one displayed outcome appear.

### Outcomes 1–4

| Predictor | Math | Language | Exam taking | HE enrollment |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Cumulative observed years in role | +0.010 | — | — | — |
| HS teachers: Always in role in observed history | — | — | −0.002 | −<0.001 |
| HS teachers: Primary-function share | +0.008 | — | −0.001 | −0.008 |
| HS teachers: Prior main-function switching rate | −0.024 | — | — | — |
| HS teachers: University tertiary qualification | +0.018 | +0.049 | — | +0.001 |
| HS teachers: HS teaching qualification | +0.023 | +0.081 | +0.055 | — |
| HS teachers: Tertiary qualification | +0.037 | — | −0.006 | — |
| HS teachers: Recorded title specialty | — | — | +0.038 | +0.023 |
| HS teachers: CFT-trained share | — | — | −0.042 | −0.030 |
| HS teachers: Other institution type share | — | — | −0.037 | −0.050 |
| HS teachers: Longest reported degree (semesters) | — | — | +0.027 | +0.021 |
| HS teachers: Years since earliest reported title | +0.033 | — | −0.007 | −0.031 |
| HS teachers: Math credentials among assigned HS math teachers | — | +0.006 | +0.011 | — |
| HS teachers: Language credentials among assigned HS language teachers | +0.027 | +0.013 | — | — |
| HS teachers: Mean annual staff headcount | +0.054 | +0.030 | +0.079 | +0.120 |
| HS teachers: Share of years with staff present | −0.039 | — | +0.314 | +0.180 |
| HS teachers: Staff per 1,000 VA-sample students | −0.012 | — | +0.019 | +0.004 |
| HS teachers: VA-sample students per average staff member | −0.020 | — | +0.003 | +0.055 |
| HS teachers: Log VA-sample students per average staff member | — | — | +0.185 | +0.095 |
| HS teachers: Reported school tenure in 2018 | — | +0.010 | +0.025 | — |
| HS teachers: Role absent throughout 2018-2024 | — | — | −0.130 | −0.062 |
| HS teachers: UG degree at high-premium institution | +0.060 | — | — | — |
| HS teachers: Any non-UG qualification | +0.041 | +0.021 | +0.006 | — |
| HS teachers: Non-UG qualification at high-premium institution | +0.016 | +0.015 | — | +0.008 |
| HS teachers: Any degree at high-premium institution | — | +0.022 | +0.005 | — |
| HS teachers: Role-specific qualification | — | — | +0.057 | — |
| HS teachers: Any magister | +0.060 | +0.059 | — | — |
| HS teachers: Magister at high-premium institution | +0.021 | +0.020 | — | — |
| Orientadores: Only primary role across school appointments | — | — | — | −0.017 |
| Orientadores: Prior main-function switching rate | — | — | +0.004 | — |
| Orientadores: Teaching qualification | — | — | −0.002 | — |
| Orientadores: Tertiary qualification | — | — | +0.023 | +0.022 |
| Orientadores: IP-trained share | — | — | −0.003 | −0.004 |
| Orientadores: Normal-school-trained share | — | — | +0.001 | — |
| Orientadores: Other institution type share | — | — | +0.001 | — |
| Orientadores: Longest reported degree (semesters) | +0.003 | — | — | — |
| Orientadores: Years since earliest reported title | — | — | −0.020 | −0.003 |
| Orientadores: Mean annual staff headcount | −0.001 | — | — | — |
| Orientadores: Share of years with staff present | −0.014 | — | — | — |
| Orientadores: Log VA-sample students per average staff member | — | — | +0.004 | — |
| Orientadores: Reported school tenure in 2018 | — | — | +0.005 | — |
| Orientadores: VA students per average primary counselor | — | — | −0.013 | — |
| Orientadores: Log VA students per average primary counselor | −0.016 | — | — | +0.011 |
| Orientadores: Role absent throughout 2018-2024 | — | — | −0.003 | −0.013 |
| Orientadores: UG degree at high-premium institution | — | — | −0.005 | — |
| Orientadores: Non-UG qualification at high-premium institution | — | — | +0.003 | — |
| Orientadores: Role-specific qualification | — | — | — | +0.003 |
| Orientadores: Any magister | −0.002 | — | — | — |
| Leadership: Current school-leadership spell | +0.003 | +0.007 | — | +0.017 |
| Leadership: Primary-function leadership share | — | — | +0.002 | +0.001 |
| Leadership: Prior detailed main-function change rate | −0.009 | — | −0.005 | −<0.001 |
| Leadership: University tertiary qualification | — | — | — | +0.002 |
| Leadership: Teaching qualification | −0.004 | — | +0.010 | +0.006 |
| Leadership: Tertiary qualification | — | — | — | +0.001 |
| Leadership: Recorded title specialty | — | — | — | −0.008 |
| Leadership: CFT-trained share | — | — | +0.008 | — |
| Leadership: Normal-school-trained share | — | — | −0.002 | — |
| Leadership: Director (4) share | — | — | +0.026 | — |
| Leadership: Mean primary leadership headcount | +0.112 | +0.067 | — | — |
| Leadership: Share of years with leadership present | — | — | −0.044 | −0.024 |
| Leadership: Leaders per 1,000 VA-sample students | — | — | +0.148 | +0.074 |
| Leadership: VA-sample students per average leader | — | — | +0.013 | +0.007 |
| Leadership: Log VA-sample students per average leader | — | — | −0.113 | −0.088 |
| Leadership: Reported school tenure in 2018 | — | — | −0.002 | — |
| Leadership: Role absent throughout 2018-2024 | — | — | +0.021 | +0.011 |
| Leadership: Any non-UG qualification | — | — | — | +0.021 |
| Leadership: Non-UG qualification at high-premium institution | +0.003 | +0.021 | +<0.001 | +0.005 |
| Leadership: Any degree at high-premium institution | +0.045 | — | +<0.001 | — |
| Leadership: Any magister | — | — | +0.019 | — |
| Leadership: Magister at high-premium institution | +0.007 | — | — | — |
| Leadership: Role-specific magister | — | — | — | +0.008 |
| School context: Log VA-sample student count | — | — | −0.092 | −0.082 |
| School context: Technical-professional/artistic offering | −0.166 | −0.237 | −0.226 | −0.190 |
| School context: Basic-education offering | +0.036 | +0.072 | +0.183 | +0.150 |
| School context: dependency 2 | −0.016 | −0.003 | — | — |
| School context: dependency 3 | — | — | +0.003 | +0.026 |
| School context: Private paid school (dependency 4) | +0.381 | +0.315 | +0.044 | +0.049 |
| School context: dependency 5 | −0.009 | −0.001 | −0.026 | — |
| School context: region 1 | — | — | — | +0.017 |
| School context: region 2 | −0.012 | — | — | +0.051 |
| School context: region 3 | — | — | +0.006 | +0.038 |
| School context: region 4 | — | — | +0.029 | +0.079 |
| School context: region 5 | −0.010 | — | +0.028 | +0.061 |
| School context: region 6 | — | — | −0.012 | −0.030 |
| School context: region 7 | +0.039 | +0.015 | +0.019 | +0.027 |
| School context: region 8 | +0.001 | — | — | +0.046 |
| School context: region 9 | +0.046 | +0.047 | +0.013 | +0.024 |
| School context: region 10 | +0.042 | +0.036 | −0.002 | +0.035 |
| School context: region 11 | — | +0.001 | — | +0.010 |
| School context: region 12 | — | — | — | +0.055 |
| School context: region 14 | — | +0.008 | +0.015 | +0.038 |
| School context: region 15 | — | — | +0.008 | +0.024 |
| School context: region 16 | +0.033 | — | +0.013 | +0.057 |

### Outcomes 5–8

| Predictor | STEM | HP field | HP institution | Income: full |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Cumulative observed years in role | — | — | +0.026 | — |
| HS teachers: Prior role-years at this school | — | +0.002 | −0.002 | — |
| HS teachers: Primary-function share | — | — | — | −0.005 |
| HS teachers: Only primary role across school appointments | — | — | +0.005 | +0.007 |
| HS teachers: Prior main-function switching rate | — | −0.011 | — | −0.008 |
| HS teachers: University tertiary qualification | — | — | — | +0.015 |
| HS teachers: Teaching qualification | −0.059 | −0.024 | — | — |
| HS teachers: HS teaching qualification | — | +0.004 | — | +0.060 |
| HS teachers: IP-trained share | +0.011 | +0.006 | — | — |
| HS teachers: CFT-trained share | — | — | — | −0.039 |
| HS teachers: Normal-school-trained share | — | −0.017 | — | −0.002 |
| HS teachers: Other institution type share | +0.029 | +0.018 | — | −0.005 |
| HS teachers: Longest reported degree (semesters) | −0.003 | −0.004 | — | +0.014 |
| HS teachers: Years since earliest reported title | — | — | +0.061 | — |
| HS teachers: Math credentials among assigned HS math teachers | −0.001 | −0.008 | — | −0.002 |
| HS teachers: Language credentials among assigned HS language teachers | — | — | — | +0.010 |
| HS teachers: Mean annual staff headcount | +0.105 | +0.115 | — | +0.117 |
| HS teachers: Share of years with staff present | — | — | −0.021 | +0.061 |
| HS teachers: Staff per 1,000 VA-sample students | — | — | — | +0.012 |
| HS teachers: VA-sample students per average staff member | — | — | −0.015 | +0.008 |
| HS teachers: Log VA-sample students per average staff member | — | +0.034 | — | +0.119 |
| HS teachers: Reported school tenure in 2018 | — | +0.036 | — | — |
| HS teachers: Reported system tenure in 2018 | — | — | — | +0.020 |
| HS teachers: Role absent throughout 2018-2024 | — | −0.029 | — | — |
| HS teachers: UG degree at high-premium institution | — | −0.015 | +0.200 | +0.012 |
| HS teachers: Any non-UG qualification | — | +0.009 | — | +0.007 |
| HS teachers: Non-UG qualification at high-premium institution | — | — | +0.023 | — |
| HS teachers: Any degree at high-premium institution | — | — | +0.162 | +0.083 |
| HS teachers: Any magister | — | +0.009 | +0.003 | +0.030 |
| HS teachers: Magister at high-premium institution | — | −0.008 | +0.038 | — |
| HS teachers: Role-specific magister | — | +0.010 | — | — |
| Orientadores: Only primary role across school appointments | — | — | — | −0.003 |
| Orientadores: University tertiary qualification | — | +0.015 | — | — |
| Orientadores: Tertiary qualification | — | — | — | +0.011 |
| Orientadores: Recorded title specialty | — | +0.013 | — | — |
| Orientadores: CFT-trained share | — | −0.001 | — | — |
| Orientadores: Normal-school-trained share | — | +<0.001 | — | +0.005 |
| Orientadores: Other institution type share | — | +0.007 | — | +0.003 |
| Orientadores: Longest reported degree (semesters) | — | +0.001 | — | — |
| Orientadores: Years since earliest reported title | — | — | — | −0.006 |
| Orientadores: Orientation mention among applicable titles | — | +0.003 | — | — |
| Orientadores: Mean annual staff headcount | — | −0.002 | — | — |
| Orientadores: VA-sample students per average staff member | — | — | −0.014 | −0.011 |
| Orientadores: Reported school tenure in 2018 | — | −0.017 | — | — |
| Orientadores: VA students per average primary counselor | +0.008 | +0.006 | −0.004 | — |
| Orientadores: Role absent throughout 2018-2024 | — | — | — | −0.005 |
| Orientadores: Non-UG qualification at high-premium institution | — | +0.002 | — | — |
| Orientadores: Role-specific qualification | +0.009 | +0.016 | — | +0.001 |
| Leadership: Prior primary-or-secondary leadership years | — | −0.009 | — | — |
| Leadership: Primary-function leadership share | — | +0.010 | — | — |
| Leadership: Prior detailed main-function change rate | — | — | −0.002 | −0.006 |
| Leadership: Prior main-function leadership entry/exit rate | — | +0.008 | — | — |
| Leadership: Recorded title specialty | — | −0.011 | — | −0.006 |
| Leadership: CFT-trained share | — | −0.010 | — | — |
| Leadership: Normal-school-trained share | — | −<0.001 | — | −0.004 |
| Leadership: Other institution type share | — | +0.003 | — | — |
| Leadership: Longest degree (semesters) | — | −0.003 | — | — |
| Leadership: Planta Directiva (3) share | — | — | +0.001 | — |
| Leadership: Directiva (10) share | +0.005 | +0.013 | — | — |
| Leadership: Subdirector (15) share | — | −0.001 | — | — |
| Leadership: Only primary leadership across school appointments | — | −0.011 | — | — |
| Leadership: Mean annual leadership headcount | — | — | +0.030 | — |
| Leadership: Mean primary leadership headcount | +0.027 | +0.059 | +0.048 | +0.047 |
| Leadership: Share of years with leadership present | — | −0.002 | — | −0.019 |
| Leadership: Leaders per 1,000 VA-sample students | +0.016 | +0.026 | — | +0.062 |
| Leadership: Log VA-sample students per average leader | — | — | — | −0.065 |
| Leadership: Reported system tenure in 2018 | — | +0.006 | — | — |
| Leadership: Role absent throughout 2018-2024 | — | +0.006 | — | — |
| Leadership: UG degree at high-premium institution | +0.010 | +0.008 | — | — |
| Leadership: Non-UG qualification at high-premium institution | — | +0.020 | +0.012 | +0.040 |
| Leadership: Any magister | — | +0.003 | — | +0.006 |
| Leadership: Magister at high-premium institution | — | +0.001 | +0.009 | — |
| Leadership: Role-specific magister | — | +0.032 | — | +0.023 |
| School context: Log VA-sample student count | — | −0.068 | — | −0.062 |
| School context: Rural school | — | — | +0.009 | — |
| School context: Technical-professional/artistic offering | — | −0.116 | −0.049 | −0.191 |
| School context: Basic-education offering | — | +0.089 | +0.004 | +0.150 |
| School context: dependency 2 | — | −0.018 | — | — |
| School context: dependency 3 | — | +0.020 | −0.006 | +0.049 |
| School context: Private paid school (dependency 4) | — | +0.088 | +0.331 | +0.256 |
| School context: dependency 5 | +0.087 | +0.049 | — | — |
| School context: region 1 | +0.013 | +0.051 | −0.007 | +0.018 |
| School context: region 2 | +0.028 | +0.086 | +0.109 | +0.077 |
| School context: region 3 | +0.031 | +0.081 | +0.117 | +0.072 |
| School context: region 4 | +0.034 | +0.084 | +0.015 | +0.077 |
| School context: region 5 | +0.079 | +0.111 | — | +0.064 |
| School context: region 6 | −0.027 | −0.003 | −0.047 | −0.028 |
| School context: region 7 | — | +0.019 | −0.034 | — |
| School context: region 8 | — | +0.058 | −0.045 | — |
| School context: region 9 | — | +0.016 | −0.047 | — |
| School context: region 10 | — | +0.061 | −0.040 | +0.021 |
| School context: region 11 | — | +0.035 | — | — |
| School context: region 12 | — | +0.033 | −0.033 | +0.026 |
| School context: region 14 | — | +0.079 | −0.038 | +0.035 |
| School context: region 15 | — | +0.032 | — | +0.009 |
| School context: region 16 | — | +0.004 | −0.014 | +0.002 |

### Outcomes 9–12

| Predictor | Income: field | Income: institution | Program accreditation | Institution accreditation |
| --- | ---: | ---: | ---: | ---: |
| HS teachers: Only primary role across school appointments | — | — | — | +0.005 |
| HS teachers: University tertiary qualification | +0.017 | — | — | — |
| HS teachers: Teaching qualification | — | — | +0.143 | — |
| HS teachers: HS teaching qualification | — | +0.004 | — | — |
| HS teachers: IP-trained share | −0.027 | −0.007 | — | — |
| HS teachers: CFT-trained share | −0.002 | — | — | — |
| HS teachers: VA-sample students per average staff member | — | — | −0.016 | −0.003 |
| HS teachers: Any non-UG qualification | — | — | — | +0.026 |
| HS teachers: Non-UG qualification at high-premium institution | — | — | — | +0.004 |
| HS teachers: Any degree at high-premium institution | — | +0.119 | — | — |
| HS teachers: Any magister | +0.014 | — | — | — |
| Orientadores: VA-sample students per average staff member | — | −0.004 | — | — |
| Leadership: Mean primary leadership headcount | +0.032 | +0.040 | — | +0.039 |
| Leadership: VA-sample students per average leader | — | — | −0.084 | — |
| Leadership: Any degree at high-premium institution | +0.011 | +0.014 | — | +0.007 |
| School context: Technical-professional/artistic offering | −0.084 | −0.104 | — | — |
| School context: Basic-education offering | +0.032 | +0.026 | — | — |
| School context: dependency 2 | — | — | +0.003 | — |
| School context: dependency 3 | — | — | — | −0.029 |
| School context: Private paid school (dependency 4) | +0.131 | +0.138 | — | +0.001 |
| School context: dependency 5 | — | — | −0.001 | +0.011 |
| School context: region 1 | +0.020 | — | −0.030 | — |
| School context: region 2 | +0.067 | +0.142 | — | — |
| School context: region 3 | +0.032 | +0.117 | −0.012 | −0.016 |
| School context: region 4 | +0.027 | +0.085 | −0.028 | −0.077 |
| School context: region 5 | +0.016 | +0.098 | — | +0.142 |
| School context: region 6 | −0.012 | −0.082 | −0.012 | −0.251 |
| School context: region 7 | — | −0.081 | — | −0.047 |
| School context: region 9 | — | −0.111 | +0.003 | −0.028 |
| School context: region 10 | +0.026 | −0.026 | — | +0.008 |
| School context: region 12 | — | +0.016 | — | — |
| School context: region 14 | — | — | — | +0.013 |
| School context: region 15 | — | +0.044 | — | — |

## Coverage and missingness indicators

| Predictor | Math | Language | Exam taking | HE enrollment | STEM | HP field | HP institution | Income: full | Income: field | Income: institution | Program accreditation | Institution accreditation |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| HS teachers: Prior observed years in role [missing/undefined] | — | — | −0.061 | −0.055 | — | −0.027 | — | −0.036 | — | — | — | — |
| HS teachers: Cumulative observed years in role [missing/undefined] | — | — | — | — | — | — | +0.022 | — | — | — | — | — |
| HS teachers: Prior role-years at this school [missing/undefined] | −0.021 | −0.024 | −0.006 | — | — | — | — | — | — | — | — | — |
| HS teachers: Current observed role spell (years) [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Current role-at-school spell (years) [missing/undefined] | — | — | — | — | — | — | +0.002 | — | — | — | — | — |
| HS teachers: Share of prior known years in role [missing/undefined] | — | — | −0.001 | −<0.001 | — | −<0.001 | — | — | — | — | — | — |
| HS teachers: Always in role in observed history [missing/undefined] | — | — | — | — | — | −0.023 | — | — | — | — | — | — |
| HS teachers: Primary-function share [missing/undefined] | — | — | — | — | — | — | +<0.001 | — | — | — | — | — |
| HS teachers: Prior main-function switching rate [missing/undefined] | −0.022 | −0.019 | −0.003 | — | — | −0.022 | — | −0.023 | — | — | — | — |
| HS teachers: Recorded title specialty [missing/undefined] | — | — | — | — | — | −0.006 | — | — | — | — | — | — |
| HS teachers: Math credentials among assigned HS math teachers [missing/undefined] | — | — | −0.126 | −0.126 | — | −0.014 | — | −0.207 | — | — | — | — |
| HS teachers: Language credentials among assigned HS language teachers [missing/undefined] | — | — | −0.081 | −0.020 | — | — | — | — | — | — | — | — |
| HS teachers: Reported school tenure in 2018 [missing/undefined] | — | — | −0.018 | — | — | — | — | — | — | — | — | — |
| HS teachers: Reported system tenure in 2018 [missing/undefined] | — | — | −0.007 | — | — | — | — | — | — | — | — | — |
| HS teachers: As-of undergraduate database match | — | — | — | +0.027 | — | +0.021 | −0.064 | — | — | — | — | — |
| HS teachers: Observed degree with institution premium unavailable | −0.055 | −0.055 | −0.025 | −0.027 | −0.024 | −0.085 | — | −0.044 | −0.035 | −0.003 | — | −0.021 |
| Orientadores: Prior role-years at this school [missing/undefined] | — | — | — | — | — | +0.004 | — | — | — | — | — | — |
| Orientadores: Recorded title specialty [missing/undefined] | — | — | — | −0.003 | — | — | — | — | — | — | — | — |
| Orientadores: Orientation mention among applicable titles [missing/undefined] | +0.005 | — | — | — | — | — | — | — | — | — | — | — |
| Orientadores: Reported school tenure in 2018 [missing/undefined] | — | — | −0.002 | — | — | — | — | — | — | — | — | — |
| Orientadores: Reported system tenure in 2018 [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| Orientadores: VA students per average primary counselor [missing/undefined] | +0.004 | — | −0.024 | −0.004 | — | — | — | — | — | — | — | — |
| Orientadores: Log VA students per average primary counselor [missing/undefined] | +0.001 | — | −0.004 | — | — | — | — | — | — | — | — | — |
| Orientadores: As-of undergraduate database match | — | — | +0.001 | — | — | +0.013 | — | — | — | — | — | — |
| Orientadores: Observed degree with institution premium unavailable | — | — | −<0.001 | +0.014 | — | +0.010 | −0.001 | — | — | — | — | — |
| Leadership: Cumulative primary-function leadership years [missing/undefined] | — | — | — | — | — | — | — | +0.043 | — | — | — | — |
| Leadership: Prior leadership years at this school [missing/undefined] | — | — | +0.001 | — | — | — | — | — | — | — | — | — |
| Leadership: Current leadership spell [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Current school-leadership spell [missing/undefined] | — | — | — | — | — | — | — | +0.002 | — | — | — | — |
| Leadership: Primary-function leadership share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Prior detailed main-function change rate [missing/undefined] | — | — | +0.013 | — | — | — | — | — | — | — | — | — |
| Leadership: Planta Directiva (3) share [missing/undefined] | — | — | — | — | — | — | — | +<0.001 | — | — | — | — |
| Leadership: Reported school tenure in 2018 [missing/undefined] | — | — | −0.020 | — | — | — | — | — | — | — | — | — |
| Leadership: Reported system tenure in 2018 [missing/undefined] | — | — | −<0.001 | — | — | — | — | — | — | — | — | — |
| Leadership: Any as-of qualification database match | — | — | +0.007 | — | +0.006 | — | — | +0.003 | — | — | — | — |
| Leadership: As-of undergraduate database match | — | — | — | — | — | −0.001 | — | — | — | — | — | — |
| Leadership: Observed degree with institution premium unavailable | — | — | −0.004 | −0.015 | — | — | — | −<0.001 | — | — | — | — |

## Minimum-error penalty sensitivity

| Outcome | Schools | Selected | School-only R² | Joint R² | Gain in R² |
| --- | ---: | ---: | ---: | ---: | ---: |
| Math | 3,373 | 114 | 0.391 | 0.471 | +0.079 |
| Language | 3,381 | 61 | 0.433 | 0.480 | +0.047 |
| Exam taking | 3,682 | 173 | 0.459 | 0.617 | +0.158 |
| HE enrollment | 3,675 | 131 | 0.285 | 0.349 | +0.064 |
| STEM | 3,682 | 99 | 0.082 | 0.090 | +0.008 |
| HP field | 3,680 | 139 | 0.124 | 0.127 | +0.003 |
| HP institution | 3,682 | 112 | 0.500 | 0.594 | +0.094 |
| Income: full | 3,682 | 125 | 0.416 | 0.464 | +0.048 |
| Income: field | 3,375 | 54 | 0.132 | 0.134 | +0.002 |
| Income: institution | 3,375 | 38 | 0.278 | 0.290 | +0.012 |
| Program accreditation | 3,375 | 50 | 0.115 | 0.118 | +0.003 |
| Institution accreditation | 3,375 | 34 | 0.158 | 0.156 | -0.002 |

## Definitions and interpretation

- One Gaussian Lasso per saved All-sample EB VA outcome. All three staff groups enter jointly; schools receive equal weight.
- Main coefficients use the one-standard-error penalty. Each entry is outcome SD per predictor SD, conditional on all selected predictors. Dashes mean zero coefficients, not unavailable outcomes. No significance stars are used.
- Staff components and as-of credential shares refer to 2018–2024; observed career histories start in 2013. Teachers are HS-assigned classroom teachers. Orientadores and leaders can hold primary or secondary roles.
- The 163 candidates comprise 27 school, 43 teacher, 46 orientador and 47 leadership measures. Each also has a missingness indicator; training-constant columns are removed. Composite indices and alternative-history versions are excluded; broken post-2018 reported-tenure averages are excluded.
- Absent roles and incomplete rosters have explicit indicators. Undefined or missing characteristics use training-only median placeholders plus missingness indicators, not a claim of zero qualifications. Qualification database coverage and missing institution-premium coverage are separate predictors.
- Student/staff ratios use pooled VA-sample students and average annual staff headcount. They are not class sizes, annual HS enrollment ratios, FTE measures or counselor caseloads. No funding or 2024-only age measure is included.
- Prediction uses five outer school folds and five inner tuning folds. Every imputation, scaling step and penalty choice excludes the held-out outer schools. The school-only baseline is separately tuned on identical schools and folds. Its penalty range extends to zero after a boundary audit; joint-model minima were interior.
- R² is pooled out-of-fold 1 − SSE/SST; negative values are possible. Coefficients instead come from the separately tuned full-sample refit. The minimum-error penalty is a sensitivity; selection frequencies count the five outer fits, not independent replications.
- Lasso may select one of several correlated measures and omit another. A zero does not establish irrelevance; signs are conditional predictive associations, not causal hiring effects or validated staff quality.
- Validation treats the saved EB VA estimates and institution-premium definitions as fixed, without reestimating them in folds or propagating their uncertainty. Schools sharing staff are not grouped into common folds. These are held-out-school predictions of saved estimates, not forecasts validated on future cohorts.
- Independent checks reconstruct all role credential aggregates, all 240 outer-model predictions, all 288 fitted-model KKT conditions, tuning choices, selection frequencies and performance statistics. Raw inputs and source VA hashes are unchanged.

Estimator documentation: [glmnet](https://glmnet.stanford.edu/articles/glmnet.html).
