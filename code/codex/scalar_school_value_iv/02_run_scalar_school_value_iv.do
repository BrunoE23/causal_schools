*******************************************************
* Scalar school-value IV regressions
*
* Estimation-only Stata layer. All construction is done in R by
* 01_build_scalar_school_value_iv_df.R.
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
global regression_csv "$scalar_iv_dir/scalar_school_value_iv_$support_stub.csv"
global regression_dta "$scalar_iv_dir/scalar_school_value_iv_$support_stub.dta"
global output_dir "$repo_wd/output/tables/scalar_school_value_iv"

cap mkdir "$repo_wd/output"
cap mkdir "$repo_wd/output/tables"
cap mkdir "$output_dir"

* Set to 1 only after rerunning the R construction script and needing to
* refresh the Stata copy. Keep at 0 for faster repeated regressions.
local rebuild_regression_dta 0

capture confirm file "$regression_dta"
if _rc | `rebuild_regression_dta' {
    di as txt "Importing R-created scalar IV dataframe from CSV."
    import delimited "$regression_csv", clear varnames(1) case(preserve)
    compress
    save "$regression_dta", replace
}

use "$regression_dta", clear

* Keep probability controls as wildcard tokens rather than expanding them into
* thousands of variable names. Expanding the varlist can exceed Stata's command
* or macro limits and produce unhelpful "invalid syntax" errors.
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
capture confirm variable GEN_ALU
if !_rc local baseline_controls `baseline_controls' i.GEN_ALU
capture confirm variable EDAD_ALU
if !_rc local baseline_controls `baseline_controls' i.EDAD_ALU

local controls `baseline_controls' `prob_controls'

timer clear
timer on 99

tempname handle
tempfile results

postfile `handle' str32 spec str32 outcome str32 treatment str32 instrument ///
    double beta se zstat p_value n_obs fs_beta fs_se fs_f iv_seconds fs_seconds int rc using `results', replace

local n_specs 7

local spec_1 "math_unadj"
local y_1 "z_year_math_max"
local d_1 "d_math_unadj"
local z_1 "z_math_unadj"

local spec_2 "math_adj"
local y_2 "z_year_math_max"
local d_2 "d_math_adj"
local z_2 "z_math_adj"

local spec_3 "leng_unadj"
local y_3 "z_year_leng_max"
local d_3 "d_leng_unadj"
local z_3 "z_leng_unadj"

local spec_4 "leng_adj"
local y_4 "z_year_leng_max"
local d_4 "d_leng_adj"
local z_4 "z_leng_adj"

local spec_5 "stem_unadj"
local y_5 "stem_enrollment_m1"
local d_5 "d_stem_unadj"
local z_5 "z_stem_unadj"

local spec_6 "stem_adj"
local y_6 "stem_enrollment_m1"
local d_6 "d_stem_adj"
local z_6 "z_stem_adj"

local spec_7 "stem_gap_adj"
local y_7 "stem_enrollment_m1"
local d_7 "d_stem_gap_adj"
local z_7 "z_stem_gap_adj"

forvalues i = 1/`n_specs' {
    local spec "`spec_`i''"
    local y "`y_`i''"
    local d "`d_`i''"
    local z "`z_`i''"

    di as txt "Running spec: `spec'"
    di as txt "Outcome: `y' | Treatment: `d' | Instrument: `z'"

    timer clear 1
    timer clear 2
    timer on 1
    capture quietly ivregress 2sls `y' `controls' (`d' = `z'), vce(robust)
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
        di as error "Spec `spec' failed with return code `rc'."
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

    di as txt "Spec `spec' IV seconds: " %9.2f iv_seconds ///
        " | First-stage seconds: " %9.2f fs_seconds ///
        " | First-stage F: " %9.2f fs_f ///
        " | rc: `rc'"

    scalar rc_scalar = `rc'
    post `handle' (`"`spec'"') (`"`y'"') (`"`d'"') (`"`z'"') ///
        (beta) (se) (zstat) (p_value) (n_obs_scalar) (fs_beta) (fs_se) (fs_f) ///
        (iv_seconds) (fs_seconds) (rc_scalar)
}

postclose `handle'
timer off 99
timer list 99

use `results', clear
format beta se zstat p_value fs_beta fs_se fs_f %9.4f
format iv_seconds fs_seconds %9.2f
format n_obs %12.0fc

export delimited using "$output_dir/scalar_school_value_iv_results_${support_stub}_${controls_stub}.csv", replace
save "$output_dir/scalar_school_value_iv_results_${support_stub}_${controls_stub}.dta", replace

list, abbreviate(24)

di as res "Wrote $output_dir/scalar_school_value_iv_results_${support_stub}_${controls_stub}.csv"
