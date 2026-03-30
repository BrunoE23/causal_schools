cd "C:/Users/brunem/Dropbox/causal_schools"

set maxvar 20000


import delimited "./data/clean/univ_gr8_df.csv", clear
save "./data/clean/univ_gr8_df_comp.dta", replace



import delimited "./data/clean/DA_probs/probs_columns_wide.csv", clear
save "./data/clean/DA_probs/probs_columns_wide_comp.dta", replace
