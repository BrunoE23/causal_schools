version 19
clear all
set more off

log using "tmp/stata_probe.log", replace text

display "Stata probe running"
display c(stata_version)

capture confirm file "C:/Users/brunem/Dropbox/causal_schools/data/clean/univ_gr8_df.dta"
if _rc {
    display as error "Missing univ_gr8_df.dta"
    exit 601
}

use "C:/Users/brunem/Dropbox/causal_schools/data/clean/univ_gr8_df.dta", clear
capture which reghdfe
display "which reghdfe rc: " _rc
capture which ftools
display "which ftools rc: " _rc
capture which parmest
display "which parmest rc: " _rc

display "Rows: " _N
foreach v in MRUN mrun student_id cohort_gr8 most_time_RBD GEN_ALU EDAD_ALU COD_COM_ALU ///
    z_sim_mat_4to z_sim_leng_4to income_decile_imputed father_educ_years_imputed ///
    mother_educ_years_imputed father_indigenous_imputed mother_indigenous_imputed ///
    sala_cuna_imputed jardin_imputed prekinder_imputed kinder_imputed ///
    math_max leng_max psu_year f_science_m1 f_eng_m1 ///
    ACREDITADA_CARR_m1 ACREDITADA_INST_m1 ACRE_INST_ANIO_m1 ///
    program_certified_years_m1 institution_accredited_m1 {
    capture confirm variable `v'
    if _rc display as error "missing `v'"
    else describe `v', fullnames
}

ds *middle* *gpa* *att*, has(type numeric)
display "`r(varlist)'"

log close
