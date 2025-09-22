*******************************************************
* FINAL SCRIPT — Build a full dataset of school effects
* - Runs reghdfe with br_code absorbed & clustered
* - Extracts ONLY i.rbd_admitido coefficients via parmest
* - Produces a clean dataset: school_id, estimate, stderr, t, p, sig
* - Safely computes mean/SD and (optionally) draws a histogram
*******************************************************

version 17
set more off

* 0) Install needed packages (once)
cap which reghdfe
if _rc ssc install reghdfe

cap which parmest
if _rc ssc install parmest

* 1) Load data
use "final_data.dta", clear

* 2) Ensure rbd_admitido is numeric for factor notation
capture confirm numeric variable rbd_admitido
if _rc {
    encode rbd_admitido, gen(rbd_id)
    drop rbd_admitido
    rename rbd_id rbd_admitido
}

* 3) Run model: absorb br_code, cluster on br_code, estimate i.rbd_admitido dummies
reghdfe avg_stem_share ///
    i.rbd_admitido ///
    PROM_NOTAS PTJE_RANKING math_max leng_max year_1st_app, ///
    absorb(br_code) vce(cluster br_code)

* Optional diagnostics
capture noisily di as text "Obs: " %9.0f e(N) "  |  Clusters (br_code): " %9.0f e(N_clust1)
capture noisily di as text "Dropped (collinearity): `e(droppedvarlist)'"

* 4) Extract all parameters to a dataset, then keep ONLY the rbd dummies (no interactions)
parmest, norestore
keep if strpos(parm, ".rbd_admitido") & !strpos(parm, "#")

* 5) Drop missing estimates (some levels may be omitted as base/collinear)
drop if missing(estimate)

* 6) Create a numeric school_id from the parm name (e.g., "123.rbd_admitido" -> 123)
gen str level_str = substr(parm, 1, strpos(parm, ".")-1)
destring level_str, gen(school_id) force
drop level_str
order school_id parm, first

* 7) Significance flag (5% two-sided)
gen byte sig = abs(t) > 1.96
label var school_id "rbd_admitido level (numeric)"
label var estimate  "Coefficient on i.rbd_admitido"
label var stderr    "Cluster-robust SE (cluster = br_code)"
label var t         "t-statistic"
label var p         "p-value"
label var sig       "Significant at 5% (|t|>1.96)"

* 8) Keep tidy columns (full dataset is now ready to use)
keep school_id parm estimate stderr t p sig
sort school_id
compress

* 9) Summary stats (avoid meanonly so r(sd) is populated)
quietly count
local k = r(N)

quietly summarize estimate   // <-- no meanonly
local hte_mean = r(mean)
local hte_sd   = r(sd)

di as res "School effects (non-missing): `k'"
di as res "Mean of effects: " %9.4f `hte_mean'

if `k' >= 2 & !missing(`hte_sd') {
    di as res "SD of effects:   " %9.4f `hte_sd'
}
else {
    di as res "SD of effects:   n/a (needs >=2 non-missing)"
}

* 10) Optional: Histogram with ±1 SD lines when SD is defined
capture graph drop _all
local addlines ""
local notetxt = "Mean = " + string(`hte_mean',"%9.3f")
if `k' >= 2 & !missing(`hte_sd') {
    local addlines "xline(`=`hte_mean' - `hte_sd'') xline(`=`hte_mean' + `hte_sd'')"
    local notetxt = "`notetxt'" + ", SD = " + string(`hte_sd',"%9.3f")
}

histogram estimate, bin(30) ///
    title("Histogram of school (rbd_admitido) effects on STEM share") ///
    xtitle("Estimated effect (coef. on i.rbd_admitido)") ///
    `addlines' ///
    note("`notetxt'") ///
    name(h_b, replace)
	
	
	
	* 11) Optional: save outputs
 save "school_effects_rbd_admitido.dta", replace
 export delimited using "school_effects_rbd_admitido.csv", replace
 graph export "school_effects_hist.png", replace width(2000)
