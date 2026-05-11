# School Public Funding Per Student

This folder constructs a first-pass school-level public funding measure from the
MINEDUC `Subvenciones-a-EE` files.

The current measure is not total school expenditure. It is public funding received
through the subsidy/payment file.

Main script:

- `01_construct_public_funding_per_student.R`

Outputs:

- `data/clean/school_expenditure_values/school_public_funding_per_student_year.csv`
- `data/clean/school_expenditure_values/school_public_funding_per_student_2017_2021.csv`
- `data/clean/school_expenditure_values/school_public_funding_per_student_diagnostics.csv`
- `data/clean/scalar_school_value_iv_money/scalar_school_money_iv_k100_timely_risk.csv`

First-pass definition:

`annual public funding per student = annual sum of selected monthly subsidy components / average monthly enrollment`

`escolaridad_pie` is excluded from the component sum because it is already
contained in `escolaridad`; including both would double-count PIE-related
schooling payments.

Monetary values are converted to 2021 pesos using annual CPI index values for
Chile from World Bank indicator `FP.CPI.TOTL`, sourced from IMF International
Financial Statistics. Nominal values are retained, but the real 2021-peso value
should be preferred.

The 2017-2021 school-level value used as a candidate scalar school characteristic is:

`enrollment_weighted_public_funding_per_student_2021_pesos_2017_2021`

This is the total selected public funding over observed years divided by the sum of
annual average enrollment across observed years, after converting each year's
funding to 2021 pesos.

The script also constructs a growth/change version using real 2021-peso annual
funding per student:

- early period: 2017-2018
- late period: 2019-2021

Candidate growth variables:

- `change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018`
- `pct_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018`
- `log_change_public_funding_per_student_2021_pesos_2019_2021_vs_2017_2018`

The level measure and the growth measure answer different questions. The level
measure asks about moves toward higher-funded schools; the growth measure asks
about moves toward schools where real public funding per student increased more
between the early and later period.

The IV regression scripts are:

- `02_build_money_scalar_iv_df.R`
- `03_run_money_scalar_iv.do`

The money IV dataframe scales pesos into millions of 2021 pesos. Therefore, the
Stata coefficient on `d_money_level_millions` is the effect of an additional
million 2021 pesos per student in average public funding, and the coefficient on
`d_money_growth_millions` is the effect of an additional million 2021 pesos per
student in funding growth from 2017-2018 to 2019-2021.

`Reliquidacion-FICOM` files are intentionally not included in this first pass. The
current IV lottery support contains no `Particular Pagado` schools, and FICOM is
not the relevant resource margin for the supported lottery schools.

Current caveats:

- the measure is based on public subsidy/payment components, not total school expenditure
- component definitions should be revisited before treating this as a final spending variable
