********************************************************************************
* Risk-Controlled Value-Added (RC VAM) -- Stata port -- RIDGE spec
*
* Angrist, Hull, Pathak & Walters (2024, "Credible School Value-Added with
* Undersubscribed School Lotteries"), equation (4):
*
*   Y_i = alpha_0 + sum_j alpha_j D_ij + X_i'Gamma + g(p_i) + eta_i
*
* g(p_i) = linear terms in each student's simulated assignment-probability
* vector p_i, plus a dummy for each school where p_ij == 0.
*
* REWRITTEN 2026-09-21: this script used to build g(p_i) itself (stack
* DA_probs, reshape wide, merge into the rich-X/outcomes dataset) in Steps
* 1-3. That's gone. `code/claude/rc_vam/00_build_rc_vam_analysis_sample.R`
* now does ALL of that in one R script -- data.table::dcast for the wide
* pivot, a data.table merge for the final join -- and writes ONE file,
* `rc_vam_stata_analysis_sample.dta`, that this script (and its siblings
* _lasso.do/_noreg.do) just `use` directly. Nothing here reshapes or does a
* wide merge anymore; that was the slow/hanging part in Stata. Run the R
* script first (Rscript 00_build_rc_vam_analysis_sample.R), then this.
*
* SAMPLE: ALL timely SAE applicants (timely_sae == 1), NOT restricted to
* any_risk == 1. RC VAM identifies off conditioning on p_i via g(p_i), not off
* offer-instrumented risk variation, so degenerate-risk students are still
* validly included.
*
* COHORTS: sae_proceso restricted to 2018-2020 (baked into the cached .dta
* by the R script) -- 2021 is excluded because z_sim_mat_4to (a baseline
* SIMCE control every outcome needs) is missing for 93.9% of timely 2021
* applicants, versus 15.6-22.8% for 2017-2020. 2017 is not included: g(p_i)
* can't be built for it (DA_probs_2017.csv doesn't exist) -- see
* setup_md_codex/project_memory/decisions/
* 2026-09-21-rc-vam-restrict-to-3-cohorts-2018-2020.md.
*
* ESTIMATION: `regress` includes every school as an explicit dummy in ONE
* fit and reads estimates + SEs straight off that fit's VCE via parmest. No
* bootstrap, no refitting -- see CENTERING note below for why this differs
* slightly from R's absorb-plus-bootstrap approach, and why that's fine.
*
* CENTERING AND ITS SE (decided 2026-09-20): the point estimate is recentered
* on the enrollment-weighted population mean across schools -- a relabeling
* for interpretability ("this school vs. the average school"), not a
* reference-school choice. The reported SE is each school's OWN regression
* SE, NOT adjusted for the fact that the mean being subtracted is itself
* estimated from every school's correlated coefficient -- an approximation,
* chosen for simplicity over exactness (see project chat log for the exact
* delta-method version if ever wanted).
*
* REGULARIZATION (decided 2026-09-20, revised same day: Ridge, not Lasso) --
* the g(p_i) nuisance block gets L2-shrunk; school dummies and X_i controls
* (x_terms) are never penalized. Ridge (shrinks but never zeroes out a term)
* was chosen over Lasso (can zero out a term) specifically because AHPW's
* theory keeps every g(p_i) term unconditionally -- each term's job is
* satisfying conditional independence for its OWN school, not predicting the
* outcome well, so dropping one (as Lasso can) risks losing that school's
* risk control. See setup_md_codex/project_memory/decisions/
* 2026-09-21-ridge-regularize-gp-block-stata-rc-vam.md for the full
* reasoning, including why this is implemented via the "ridge regression =
* OLS on augmented data" identity (one pseudo-observation per g(p_i) term,
* outcome 0, sqrt(ridge_lambda) in that term's own column, 0 everywhere
* else) rather than Stata's `elasticnet` or a hand-derived matrix closed
* form -- both were judged riskier to get right without a way to test them.
*
* Because a pseudo-row needs to represent "no school," which `ibn.school_rbd`
* cannot encode for a real observation, school_rbd is expanded into literal
* dummies for this fit -- see Step 2 below for the position -> school_rbd
* lookup this requires.
*
* REQUIRES: ssc install parmest (only dependency).
********************************************************************************

version 16
clear all
set more off

* ------------------------- Path resolution -----------------------------------
local data_wd ""
foreach cand in "C:/Users/brunem/Box/causal_schools" "C:/Users/brunem/Dropbox/causal_schools" "C:/Users/xd-br/Dropbox/causal_schools" {
    capture confirm file "`cand'/data/clean/univ_gr8_df.csv"
    if _rc == 0 & "`data_wd'" == "" local data_wd "`cand'"
}
if "`data_wd'" == "" {
    display as error "Could not find data_wd -- edit the candidate list above."
    exit 601
}
display as txt "Using data_wd = `data_wd'"

local clean_dir "`data_wd'/data/clean"
local cached_sample "`clean_dir'/rc_vam_stata_analysis_sample.dta"

local output_dir      "`clean_dir'/rc_vam_school_values"
capture mkdir "`output_dir'"
local output_path       "`output_dir'/rc_vam_school_values_stata_ridge.csv"
local diagnostics_path  "`output_dir'/rc_vam_school_values_diagnostics_stata_ridge.csv"

set maxvar 32000

capture which parmest
if _rc {
    display as txt "Installing parmest..."
    ssc install parmest
}

* ------------------------- Step 1: load the pre-built analysis sample --------
* Built by code/claude/rc_vam/00_build_rc_vam_analysis_sample.R -- g(p_i)
* (prob_*/iszero_*), rich X controls, and outcomes are all already merged
* into one dataset, one row per (student_id, sae_proceso). No reshape, no
* wide merge here at all.
capture confirm file "`cached_sample'"
if _rc {
    display as error "Could not find `cached_sample'."
    display as error "Run code/claude/rc_vam/00_build_rc_vam_analysis_sample.R first (Rscript 00_build_rc_vam_analysis_sample.R) -- it builds g(p_i), the rich X controls/outcomes, and merges them, all in R, and writes this file."
    exit 601
}
display as txt "Loading cached estimation sample: `cached_sample'"
use "`cached_sample'", clear
compress

ds prob_*
local prob_vars `r(varlist)'
ds iszero_*
local iszero_vars `r(varlist)'
display as txt "g(p_i) control columns: " (wordcount("`prob_vars'")) " prob_* + " (wordcount("`iszero_vars'")) " iszero_*"
count
display as txt "RC VAM estimation sample: " r(N) " students, " (wordcount("`prob_vars'")) " supported schools."

* Saved once here (not preserve/restore) so each outcome's loop iteration
* below can start from a guaranteed-clean copy via `use`. `parmest` does its
* own internal preserve when extracting results -- if the loop instead used
* `preserve` at its top (kept open for the whole iteration), that collides
* with parmest's internal one (Stata does not support nested preserves:
* r(621) "already preserved"). `use `full_sample'' sidesteps this entirely.
tempfile full_sample
save `full_sample', replace

* ------------------------- Step 2: control vector -----------------------------
* Same rich X_i as the R script's x_control_terms / x_control_vars.
local x_terms ///
    i.cohort_gr8 i.gen_alu i.edad_alu i.cod_com_alu ///
    income_decile_imputed ///
    father_educ_years_imputed mother_educ_years_imputed ///
    father_indigenous_imputed mother_indigenous_imputed ///
    sala_cuna_imputed jardin_imputed prekinder_imputed kinder_imputed ///
    z_gpa_middle_mean z_att_middle_mean i.middle_years_observed ///
    z_sim_mat_4to z_sim_mat_4to_2 z_sim_mat_4to_3 ///
    z_sim_leng_4to z_sim_leng_4to_2 z_sim_leng_4to_3

local x_vars_needed ///
    cohort_gr8 gen_alu edad_alu cod_com_alu income_decile_imputed ///
    father_educ_years_imputed mother_educ_years_imputed ///
    father_indigenous_imputed mother_indigenous_imputed ///
    sala_cuna_imputed jardin_imputed prekinder_imputed kinder_imputed ///
    middle_years_observed z_gpa_middle_mean z_att_middle_mean ///
    z_sim_mat_4to z_sim_leng_4to

local outcomes ///
    z_year_math_max z_year_leng_max admission_exam_taker higher_ed_enrolled_m1 ///
    stem_enrollment_m1 log_program_income_clp_m1 program_certified_years_m1 ///
    inst_certified_years_m1

local zero_variance_tol = 1e-8

* Ridge penalty for the g(p_i) regularization step (Step 3) -- a single,
* hand-set lambda (not cross-validated, see REGULARIZATION note in header).
* Larger = more shrinkage on the prob_/iszero_ terms. gp_used terms are all
* roughly unit-scale already (probabilities in [0,1] and 0/1 dummies), so no
* separate standardization is applied before penalizing.
local ridge_lambda = 100

* CAVEAT: if you're on Stata < 16, matsize caps out at 11,000. Total
* regressors here are roughly (num supported schools) x 1 [school dummies] +
* (num supported schools) x up to 2 [gp_used prob_/iszero_] + ~50 [x_terms].
* At k=100 (~1,600 schools) that's comfortably under 11,000.

tempfile results_accum
local results_first = 1

* ------------------------- Step 3: run RC VAM per outcome ---------------------
foreach y of local outcomes {
    display as txt "---- RC VAM outcome: `y' ----"
    use `full_sample', clear

    if !inlist("`y'", "admission_exam_taker", "higher_ed_enrolled_m1") {
        keep if admission_exam_taker
    }

    * Drop rows with missing outcome / school / needed controls.
    egen byte miss_needed = rowmiss(school_rbd `y' `x_vars_needed')
    keep if miss_needed == 0
    drop miss_needed

    count
    if r(N) == 0 {
        display as error "No observations left for outcome `y' -- skipping."
        continue
    }

    * Drop g(p_i) columns with (near) zero variance in THIS outcome's
    * estimation sample -- avoids regress erroring on collinear/constant cols.
    local gp_used
    foreach v of local prob_vars {
        quietly sum `v'
        if r(sd) > `zero_variance_tol' & !missing(r(sd)) local gp_used `gp_used' `v'
    }
    foreach v of local iszero_vars {
        quietly sum `v'
        if r(sd) > `zero_variance_tol' & !missing(r(sd)) local gp_used `gp_used' `v'
    }
    display as txt "  g(p_i) terms with usable variance: " (wordcount("`gp_used'"))

    quietly levelsof school_rbd, local(school_levels)
    if wordcount("`school_levels'") < 2 {
        display as error "Fewer than 2 distinct schools for outcome `y' -- skipping."
        continue
    }

    * --- Ridge-regularize ONLY the g(p_i) block (see REGULARIZATION note in
    * the header for why this is implemented via augmented-data OLS rather
    * than `elasticnet` or a hand-derived matrix closed form).
    *
    * school_rbd is expanded into literal dummies (sch_dum_1, sch_dum_2, ...
    * in sorted-level order, matching `school_levels') because a pseudo-row
    * needs to represent "no school," which `ibn.school_rbd` cannot encode
    * for a real observation. `i.cohort_gr8'/`i.gen_alu'/`i.edad_alu'/
    * `i.cod_com_alu'/`i.middle_years_observed' stay as ordinary factor
    * syntax in `x_terms' -- pseudo-rows are pinned to each one's own base
    * (min) level below, which correctly zeroes out all of that factor's
    * non-base dummies for those rows, same as a real base-category
    * observation would. Plain numeric x_terms are pinned to literal 0.
    quietly tabulate school_rbd, generate(sch_dum_)
    quietly ds sch_dum_*
    local school_dums `r(varlist)'
    local n_school_dums : word count `school_dums'

    local x_factor_vars cohort_gr8 gen_alu edad_alu cod_com_alu middle_years_observed
    local x_plain_vars income_decile_imputed father_educ_years_imputed ///
        mother_educ_years_imputed father_indigenous_imputed mother_indigenous_imputed ///
        sala_cuna_imputed jardin_imputed prekinder_imputed kinder_imputed ///
        z_gpa_middle_mean z_att_middle_mean ///
        z_sim_mat_4to z_sim_mat_4to_2 z_sim_mat_4to_3 ///
        z_sim_leng_4to z_sim_leng_4to_2 z_sim_leng_4to_3
    foreach fv of local x_factor_vars {
        quietly summarize `fv'
        local base_`fv' = r(min)
    }

    local n_real = _N
    local n_gp : word count `gp_used'
    display as txt "  Ridge-regularizing g(p_i) (" `n_gp' " terms; lambda = `ridge_lambda'; school dummies + X controls unpenalized) via augmented-data OLS..."
    quietly set obs `=`n_real' + `n_gp''

    * Vectorized zero-out / base-pin for the whole pseudo-row block at once
    * (cheap: O(number of variables) replace-if calls, not per-row loops).
    quietly replace `y' = 0 if _n > `n_real'
    foreach sd of local school_dums {
        quietly replace `sd' = 0 if _n > `n_real'
    }
    foreach xn of local x_plain_vars {
        quietly replace `xn' = 0 if _n > `n_real'
    }
    foreach fv of local x_factor_vars {
        quietly replace `fv' = `base_`fv'' if _n > `n_real'
    }
    foreach g of local gp_used {
        quietly replace `g' = 0 if _n > `n_real'
    }

    * One sqrt(lambda) spike per gp_used term -- this single-row-at-a-time
    * loop is O(n_gp), not O(n_gp^2): every column was already zeroed above,
    * so each iteration only has to set its own row's own column.
    local row = `n_real'
    foreach v of local gp_used {
        local ++row
        quietly replace `v' = sqrt(`ridge_lambda') in `row'
    }

    * Fit on the augmented data. school_rbd itself is left missing on
    * pseudo-rows (never touched above) -- harmless, since it is not a
    * regressor in this fit; `school_dums' stands in for it for exactly this
    * reason. Do not sort the data between here and the extraction below --
    * "_n <= `n_real'" is how real rows are told apart from pseudo-rows.
    quietly regress `y' `school_dums' `x_terms' `gp_used', noconstant

    * Small position -> school_rbd lookup (sch_dum_`k' corresponds to the
    * k-th entry of `school_levels', which `levelsof' returns sorted) built
    * in a scratch frame so it doesn't disturb the analysis sample.
    tempfile school_lookup_file
    capture frame drop school_lookup
    frame create school_lookup
    frame school_lookup {
        clear
        quietly set obs `n_school_dums'
        gen school_dum_pos = _n
        gen double school_rbd = .
        forvalues k = 1/`n_school_dums' {
            local this_school : word `k' of `school_levels'
            quietly replace school_rbd = `this_school' in `k'
        }
        save `school_lookup_file', replace
    }
    frame drop school_lookup

    drop sch_dum_*

    preserve
        keep if e(sample) & _n <= `n_real'
        contract school_rbd, freq(n_students_regression)
        tempfile school_counts
        save `school_counts', replace
    restore

    * Bulk-extract every school dummy's coefficient + SE, then map the
    * position-based sch_dum_# name back to its real school_rbd value.
    parmest, norestore level(95)
    keep if regexm(parm, "^sch_dum_[0-9]+$")
    gen str32 pos_str = subinstr(parm, "sch_dum_", "", .)
    destring pos_str, gen(school_dum_pos) force
    drop pos_str parm

    merge 1:1 school_dum_pos using `school_lookup_file', keep(match) nogenerate
    drop school_dum_pos

    rename estimate controlled_value_added
    rename stderr controlled_value_added_se

    merge 1:1 school_rbd using `school_counts', keep(match) nogenerate

    * Recenter the POINT ESTIMATE on the enrollment-weighted population mean.
    quietly sum n_students_regression
    local n_total = r(sum)
    gen double _w = n_students_regression / `n_total'
    quietly sum controlled_value_added [aw=_w]
    local weighted_mean = r(mean)
    gen controlled_value_added_centered_student = controlled_value_added - `weighted_mean'
    drop _w

    gen str32 outcome = "`y'"
    gen n_students_regression_total = `n_total'
    * Ridge never drops a term -- candidate and used counts are always equal
    * here; both columns are kept for schema consistency with the sibling
    * lasso/noreg scripts' diagnostics.
    gen n_gp_terms_candidate = `n_gp'
    gen n_gp_terms_used = `n_gp'
    gen double ridge_lambda_used = `ridge_lambda'
    gen str48 controlled_value_added_se_method = "stata_regress_dummy_simple_se_ridge_gp_augmented"

    if `results_first' {
        save `results_accum', replace
        local results_first = 0
    }
    else {
        append using `results_accum'
        save `results_accum', replace
    }
}

* ------------------------- Step 4: write outputs ------------------------------
use `results_accum', clear
order outcome school_rbd controlled_value_added controlled_value_added_se ///
    controlled_value_added_centered_student n_students_regression ///
    n_students_regression_total n_gp_terms_candidate n_gp_terms_used
gen str32 analysis_sample = "SAE_timely_RC_VAM_stata"

export delimited using "`output_path'", replace

preserve
    collapse (count) n_schools = school_rbd (sum) n_students_regression_tot = n_students_regression ///
        (first) n_gp_terms_candidate n_gp_terms_used ridge_lambda_used ///
        (sd) sd_rc_vam = controlled_value_added_centered_student, by(outcome)
    export delimited using "`diagnostics_path'", replace
restore

display as txt "Done."
display as txt "Wrote: `output_path'"
display as txt "Wrote: `diagnostics_path'"
