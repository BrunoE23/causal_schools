*******************************************************
* FINAL SCRIPT — Build a full dataset of school effects
* - Runs reghdfe with br_code absorbed & clustered
* - Extracts ONLY i.rbd_admitido coefficients via parmest
* - Produces a clean dataset: school_id, estimate, stderr, t, p, sig
* - Safely computes mean/SD and (optionally) draws a histogram
*******************************************************

set more off

cd "C:/Users/brunem/Dropbox/causal_schools/"
use "./data/clean/final_data.dta", replace 

* --- 0) One-time setup ---
cap which reghdfe
if _rc ssc install reghdfe

cap which parmest
if _rc ssc install parmest


cap which ftoools
if _rc ssc install ftools


* --- 1) Censor variables  ---

foreach v in school_grade_avg_GPA_t_0 school_grade_avg_GPA_t_1 school_grade_avg_GPA_t_2 school_grade_avg_GPA_t_3 ///
			 school_grade_avg_ATT_t_0 school_grade_avg_ATT_t_1 school_grade_avg_ATT_t_2 school_grade_avg_ATT_t_3 ///
			 z_ATT_t_min4 z_ATT_t_min3 z_ATT_t_min2 z_ATT_t_min1 ///
			 z_GPA_t_min4 z_GPA_t_min3 z_GPA_t_min2 z_GPA_t_min1 	{
   
   summarize `v', detail
    local p1  = r(p1)
    local p99 = r(p99)

    gen `v'_c = `v'
    replace `v'_c = `p1'  if `v'_c < `p1'
    replace `v'_c = `p99' if `v'_c > `p99'
}





* --- 1) Define outcomes and controls ---
global outcomes  female  n_years_rbd_target_post n_school_changes_post ///
				 school_grade_avg_GPA_t_0_c school_grade_avg_GPA_t_1_c school_grade_avg_GPA_t_2_c school_grade_avg_GPA_t_3_c ///
				 school_grade_avg_ATT_t_0 school_grade_avg_ATT_t_1 school_grade_avg_ATT_t_2 school_grade_avg_ATT_t_3 ///
				 graduated_hs registered_psu completed_psu ///
				 math_max leng_max  took_science took_history took_both avg_stem_share
				 
				 
global controls  z_ATT_t_min4_c z_ATT_t_min3_c z_ATT_t_min2_c z_ATT_t_min1_c ///
				z_GPA_t_min4_c z_GPA_t_min3_c z_GPA_t_min2_c z_GPA_t_min1_c 
				
				
global year_fixed_effects i.year_1st_app

				*pctl_school_grade_t_min4 pctl_school_grade_t_min3 pctl_school_grade_t_min2 pctl_school_grade_t_min1 ///
				
				
* --- 2) Loop, estimate, extract, and append ---
tempfile accum
local first = 1


foreach y in $outcomes {

preserve 

    di as txt "---- Estimating for outcome: `y' ----"

    quietly reghdfe `y' ///
        i.rbd_admitido ///
        $controls $year_fixed_effects, ///
        absorb(br_code) vce(cluster br_code)

    * Optional diagnostics (works even if macros are missing)
    capture noisily di as text "Obs: " %9.0f e(N) "  |  Clusters (br_code): " %9.0f e(N_clust1)
    capture noisily di as text "Dropped (collinearity): `e(droppedvars)'"  // reghdfe stores this; safe under capture

    * Extract coefficients into a dataset
    quietly parmest, norestore

    * Keep ONLY level dummies for rbd_admitido (no interactions)
    keep if strpos(parm, ".rbd_admitido") & !strpos(parm, "#")

    * Drop missing (base/collinear)
    drop if missing(estimate)

    * Parse the numeric level before the first dot: "123.rbd_admitido" -> 123
    gen str level_str = substr(parm, 1, strpos(parm, ".")-1)
    destring level_str, gen(school_id) force
    drop level_str

    * Significance flag (5%)
    gen byte sig = abs(t) > 1.96

    * Tag the outcome
    gen str32 outcome = "`y'"


    * Append into accumulator
    if `first' {
        save `accum'
        local first = 0
    }
    else {
        append using `accum'
        save `accum', replace
    }


restore 
	}


* --- 3) Load the final long dataset ---
use `accum', clear
compress
di as res "Done. Tidy results are in memory: one row per {outcome, rbd_admitido level}."


    * Nice labels
    label var school_id "rbd_admitido level (numeric)"
    label var beta  "Coefficient on i.rbd_admitido"
    label var se    "Cluster-robust SE (cluster = br_code)"
    label var t         "t-statistic"
    label var p         "p-value"
    label var sig       "Significant at 5% (|t|>1.96)"
    label var outcome   "Outcome variable"

    * Keep tidy columns
    keep outcome school_id parm estimate stderr t p sig
    order outcome school_id parm
    sort  outcome school_id


 save "data/clean/effects_schools_long.dta", replace
 export delimited using "data/clean/effects_schools_long.csv", replace

