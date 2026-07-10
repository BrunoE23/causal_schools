version 19
clear all
set more off
set varabbrev off
set maxvar 30000

global data_wd "C:/Users/brunem/Dropbox/causal_schools"
global repo_wd "C:/Users/brunem/Research/causal_schools"
global clean_dir "$data_wd/data/clean"

log using "$repo_wd/tmp/stata_areg_test.log", replace text

tempfile middle analytic

import delimited using "$clean_dir/middle_school_controls/middle_school_controls.csv", clear varnames(1) case(preserve)
keep MRUN most_time_RBD_middle middle_years_observed z_gpa_middle_mean z_att_middle_mean
duplicates drop MRUN, force
save `middle', replace

use "$clean_dir/univ_gr8_df.dta", clear
merge 1:1 MRUN using `middle', nogen keep(master match)

gen double school_rbd = most_time_RBD
keep if !missing(school_rbd) & school_rbd > 0
keep if !missing(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16
replace math_max = . if missing(math_max) | math_max <= 0
replace leng_max = . if missing(leng_max) | leng_max <= 0
bysort psu_year: egen z_year_math_max = std(math_max)
gen byte admission_exam_taker = !missing(math_max) | !missing(leng_max)
keep if admission_exam_taker

gen double z_sim_mat_4to_sq = z_sim_mat_4to^2
gen double z_sim_mat_4to_cu = z_sim_mat_4to^3
gen double z_sim_leng_4to_sq = z_sim_leng_4to^2
gen double z_sim_leng_4to_cu = z_sim_leng_4to^3

compress

local controls ///
    i.cohort_gr8 ///
    i.GEN_ALU ///
    i.EDAD_ALU ///
    i.COD_COM_ALU ///
    income_decile_imputed ///
    father_educ_years_imputed ///
    mother_educ_years_imputed ///
    father_indigenous_imputed ///
    mother_indigenous_imputed ///
    sala_cuna_imputed ///
    jardin_imputed ///
    prekinder_imputed ///
    kinder_imputed ///
    z_gpa_middle_mean ///
    z_att_middle_mean ///
    i.middle_years_observed ///
    z_sim_mat_4to z_sim_mat_4to_sq z_sim_mat_4to_cu ///
    z_sim_leng_4to z_sim_leng_4to_sq z_sim_leng_4to_cu

timer clear
timer on 1
areg z_year_math_max ib(first).school_rbd `controls', absorb(most_time_RBD_middle)
timer off 1
timer list 1

display "N = " e(N)
display "df_r = " e(df_r)

log close
