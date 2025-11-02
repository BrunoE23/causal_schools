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


egen pre_GPA = rowmean(z_GPA_t_min4_c z_GPA_t_min3_c z_GPA_t_min2_c z_GPA_t_min1_c)
egen pre_ATT = rowmean(z_ATT_t_min4_c z_ATT_t_min3_c z_ATT_t_min2_c z_ATT_t_min1_c)

summ pre_GPA, d 
	local GPA_median = r(p50)

gen         above_median_GPA_pre = 1  if 	pre_GPA >= `GPA_median'
	replace above_median_GPA_pre = 0  if    pre_GPA <  `GPA_median'
 
	
	
* --- 1) Define outcomes and controls ---
global outcomes_tracking   female n_years_rbd_target_post n_school_changes_post ///
                           school_grade_avg_GPA_t_min4 school_grade_avg_GPA_t_min1  school_grade_avg_GPA_t_0 school_grade_avg_GPA_t_3
				 
	///			 school_grade_avg_GPA_t_min4 school_grade_avg_GPA_t_min3 school_grade_avg_GPA_t_min2 school_grade_avg_GPA_t_min1 ///
	///			 school_grade_avg_GPA_t_0    school_grade_avg_GPA_t_1    school_grade_avg_GPA_t_2    school_grade_avg_GPA_t_3   ///
	///			 school_grade_avg_ATT_t_0    school_grade_avg_ATT_t_1    school_grade_avg_ATT_t_2    school_grade_avg_ATT_t_3 
				

					
			
global outcomes_app			graduated_hs	  /// registered_psu
							completed_psu ///
				          math_max leng_max  took_only_science took_only_history took_both ///
						   avg_stem_share   /// 
						   prop_*
				 
				 
global controls pre_ATT  ///
				pre_GPA  


bysort br_code: gen n_per_br = _N

* Drop groups with fewer than 100 obs
drop if n_per_br < 100

* (Optional) clean up the helper variable
drop n_per_br
				
				
				
* --- 2) Loop, estimate, extract, and append ---
tempfile accum
local first = 1


* foreach y in $outcomes_tracking $outcomes_app {
* foreach group in "all" "female" "male" "ab_med" "be_med" {	
foreach y in n_years_rbd_target_post $outcomes_app {
	 foreach group in "all"  {	
preserve 

    di as txt "---- Estimating for outcome: `y' ----"
    di as txt "---- Estimating for group: `group' ----"

     // build the if-condition
        local ifcond
        if "`group'" == "female"       local ifcond if female==1
        else if "`group'" == "male"    local ifcond if female==0
        else if "`group'" == "ab_med"  local ifcond if above_median_GPA_pre==1
        else if "`group'" == "be_med"  local ifcond if above_median_GPA_pre==0
        // for "all" -> ifcond remains empty
	
   qui  areg `y' ///
        i.rbd_admitido ///
        $controls `ifcond', ///
        absorb(rbd_target) vce(cluster rbd_target)

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

	
	* Tag the group
    gen str32 group = "`group'"


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
}

* --- 3) Load the final long dataset ---
use `accum', clear
compress
di as res "Done. Tidy results are in memory: one row per {outcome, rbd_admitido level}."

	rename estimate beta
	rename stderr se 
	
    * Nice labels
    label var school_id "rbd_admitido level (numeric)"
    label var beta  "Coefficient on i.rbd_admitido"
    label var se    "Cluster-robust SE (cluster = br_code)"
    label var t         "t-statistic"
    label var p         "p-value"
    label var sig       "Significant at 5% (|t|>1.96)"
    label var outcome   "Outcome variable"
    label var group     "Sample choice"

    * Keep tidy columns
    keep outcome school_id  beta se t p sig group
    order outcome school_id group 
    sort  outcome school_id


 save "data/clean/effects_schools_long_v2.dta", replace
 export delimited using "data/clean/effects_schools_long_v2.csv", replace


 
 
 