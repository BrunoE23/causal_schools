*******************************************************
* Append gender-specific scalar school-value IV results
*
* Runs selected STEM specifications separately for boys and girls,
* then appends those rows to the existing results file.
*******************************************************

clear all
set more off
set varabbrev off

global data_wd "C:/Users/xd-br/Dropbox/causal_schools"
global repo_wd "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

global clean_dir "$data_wd/data/clean"
global scalar_iv_dir "$clean_dir/scalar_school_value_iv"
global support_stub "k100_timely_risk"
global controls_stub "prob_iszero"
global regression_dta "$scalar_iv_dir/scalar_school_value_iv_$support_stub.dta"
global output_dir "$repo_wd/output/tables/scalar_school_value_iv"
global results_csv "$output_dir/scalar_school_value_iv_results_${support_stub}_${controls_stub}.csv"
global results_dta "$output_dir/scalar_school_value_iv_results_${support_stub}_${controls_stub}.dta"

* Chilean admin-data convention used here: 1 = male, 2 = female.
local male_code 1
local female_code 2

use "$regression_dta", clear

local include_probability_controls 1
local prob_controls
if `include_probability_controls' {
    local prob_controls prob_* iszero_*
}

local baseline_controls
capture confirm variable cohort_gr8
if !_rc local baseline_controls `baseline_controls' i.cohort_gr8
capture confirm variable z_sim_mat_4to
if !_rc local baseline_controls `baseline_controls' z_sim_mat_4to
capture confirm variable z_sim_leng_4to
if !_rc local baseline_controls `baseline_controls' z_sim_leng_4to
capture confirm variable EDAD_ALU
if !_rc local baseline_controls `baseline_controls' i.EDAD_ALU

local controls `baseline_controls' `prob_controls'

tempname handle
tempfile new_results old_results combined_results

postfile `handle' str32 spec str16 group str32 outcome str32 treatment str32 instrument ///
    double beta se zstat p_value n_obs fs_beta fs_se fs_f iv_seconds fs_seconds int rc using `new_results', replace

local n_specs 2

local spec_1 "stem_adj"
local y_1 "stem_enrollment_m1"
local d_1 "d_stem_adj"
local z_1 "z_stem_adj"

local spec_2 "stem_gap_adj"
local y_2 "stem_enrollment_m1"
local d_2 "d_stem_gap_adj"
local z_2 "z_stem_gap_adj"

local group_1 "boys"
local if_1 "GEN_ALU == `male_code'"

local group_2 "girls"
local if_2 "GEN_ALU == `female_code'"

timer clear

forvalues s = 1/`n_specs' {
    local spec "`spec_`s''"
    local y "`y_`s''"
    local d "`d_`s''"
    local z "`z_`s''"

    forvalues i = 1/2 {
        local group "`group_`i''"
        local ifcond "`if_`i''"

        di as txt "Running spec: `spec' | group: `group'"
        di as txt "Outcome: `y' | Treatment: `d' | Instrument: `z' | If: `ifcond'"

        timer clear 1
        timer clear 2
        timer on 1
        capture quietly ivregress 2sls `y' `controls' (`d' = `z') if `ifcond', vce(robust)
        timer off 1
        quietly timer list 1
        scalar iv_seconds = r(t1)
        local rc = _rc

        if `rc' == 0 {
            scalar beta = _b[`d']
            scalar se = _se[`d']
            scalar zstat = beta / se
            scalar p_value = 2 * normal(-abs(zstat))
            scalar n_obs_scalar = e(N)

            timer on 2
            capture quietly regress `d' `z' `controls' if e(sample), vce(robust)
            timer off 2
            quietly timer list 2
            scalar fs_seconds = r(t2)

            if _rc == 0 {
                scalar fs_beta = _b[`z']
                scalar fs_se = _se[`z']
                capture quietly test `z'
                if _rc == 0 scalar fs_f = r(F)
                else scalar fs_f = .
            }
            else {
                scalar fs_beta = .
                scalar fs_se = .
                scalar fs_f = .
            }
        }
        else {
            di as error "Spec `spec' for `group' failed with return code `rc'."
            scalar beta = .
            scalar se = .
            scalar zstat = .
            scalar p_value = .
            scalar n_obs_scalar = .
            scalar fs_beta = .
            scalar fs_se = .
            scalar fs_f = .
            scalar fs_seconds = .
        }

        di as txt "Spec `spec' group `group' IV seconds: " %9.2f iv_seconds ///
            " | First-stage seconds: " %9.2f fs_seconds ///
            " | First-stage F: " %9.2f fs_f ///
            " | rc: `rc'"

        scalar rc_scalar = `rc'
        post `handle' (`"`spec'"') (`"`group'"') (`"`y'"') (`"`d'"') (`"`z'"') ///
            (beta) (se) (zstat) (p_value) (n_obs_scalar) (fs_beta) (fs_se) (fs_f) ///
            (iv_seconds) (fs_seconds) (rc_scalar)
    }
}

postclose `handle'

use `new_results', clear
format beta se zstat p_value fs_beta fs_se fs_f %9.4f
format iv_seconds fs_seconds %9.2f
format n_obs %12.0fc
save `new_results', replace

capture confirm file "$results_dta"
if _rc {
    di as error "Existing results file not found: $results_dta"
    di as error "Saving only new gender-specific rows."
    save "$results_dta", replace
    export delimited using "$results_csv", replace
}
else {
    use "$results_dta", clear

    capture confirm variable group
    if _rc {
        gen str16 group = "all"
    }
    else {
        replace group = "all" if missing(group) | group == ""
    }

    * Avoid duplicating prior gender-specific rows if this append script is rerun.
    drop if inlist(spec, "stem_adj", "stem_gap_adj") & inlist(group, "boys", "girls")

    append using `new_results'
    order spec group outcome treatment instrument beta se zstat p_value n_obs fs_beta fs_se fs_f iv_seconds fs_seconds rc
    save "$results_dta", replace
    export delimited using "$results_csv", replace
}

list if inlist(spec, "stem_adj", "stem_gap_adj"), abbreviate(24)

di as res "Appended gender-specific STEM rows to $results_csv"
