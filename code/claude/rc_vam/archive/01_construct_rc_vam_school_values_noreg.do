********************************************************************************
* Risk-Controlled Value-Added (RC VAM) -- Stata port -- NO REGULARIZATION
* (robustness-check sibling of 01_construct_rc_vam_school_values.do)
*
* Same estimation sample, same X_i controls, same outcomes, same g(p_i)
* construction as 01_construct_rc_vam_school_values.do (the "ridge" version)
* and 01_construct_rc_vam_school_values_lasso.do -- this script is the
* baseline for a 3-way robustness check across regularization choices for
* the g(p_i) nuisance block: none (this file), Ridge (main file), Lasso
* (_lasso.do). See setup_md_codex/project_memory/decisions/2026-09-21-ridge-
* regularize-gp-block-stata-rc-vam.md and the superseded 2026-09-20-lasso-...
* doc for why Ridge/Lasso were tried in the first place.
*
* Y_i = alpha_0 + sum_j alpha_j D_ij + X_i'Gamma + g(p_i) + eta_i
* (Angrist, Hull, Pathak & Walters 2024, eq. 4)
*
* This version uses g(p_i) exactly as AHPW specify it, unregularized: every
* prob_/iszero_ term (at k=100, ~3,000 of them) enters the single OLS fit
* alongside the school dummies and X controls, all unpenalized. If Ridge/
* Lasso give materially different school-level estimates than this, that is
* itself informative (regularization is doing real work, not just cosmetic
* shrinkage of noise) -- comparing against this baseline is the point.
*
* DEPENDENCY: this script does NOT rebuild g(p_i)/X/outcomes from scratch --
* it reuses `clean_dir'/rc_vam_stata_analysis_sample.dta, which is built by
* code/claude/rc_vam/00_build_rc_vam_analysis_sample.R (Rscript
* 00_build_rc_vam_analysis_sample.R). Run that R script at least once first;
* this one will error out with a clear message if the cached file isn't
* there yet.
*
* STATUS: written and reviewed but NOT executed (no Stata license/binary
* available in the environment this was written in). Estimation mechanics
* (ibn.school_rbd + noconstant, single regress, parmest extraction,
* enrollment-weighted-mean recentering with "simple" unadjusted SEs) are
* identical to the pre-regularization design already validated in the main
* script's own header/chat history -- this file just drops the
* regularization block entirely.
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
local output_path       "`output_dir'/rc_vam_school_values_stata_noreg.csv"
local diagnostics_path  "`output_dir'/rc_vam_school_values_diagnostics_stata_noreg.csv"

set maxvar 32000

capture which parmest
if _rc {
    display as txt "Installing parmest..."
    ssc install parmest
}

* ------------------------- Step 0: load the shared cached sample -------------
capture confirm file "`cached_sample'"
if _rc {
    display as error "Could not find `cached_sample'."
    display as error "Run 01_construct_rc_vam_school_values.do (the ridge version) first -- it builds and saves this file in its Step 3. This script reuses it so all three regularization variants share an identical estimation sample."
    exit 601
}
display as txt "Loading cached estimation sample: `cached_sample'"
use "`cached_sample'", clear

ds prob_*
local prob_vars `r(varlist)'
ds iszero_*
local iszero_vars `r(varlist)'
display as txt "g(p_i) control columns: " (wordcount("`prob_vars'")) " prob_* + " (wordcount("`iszero_vars'")) " iszero_*"

* ------------------------- Step 4 (renumbered to match main script) ----------
* Same rich X_i as the main script's x_control_terms / x_control_vars.
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

* No bootstrap needed -- see main script's header for the full rationale.
* ibn.school_rbd + noconstant gives every school its own absolute-level
* coefficient directly, one regress per outcome, analytical SEs off that
* fit's own VCE.
*
* CAVEAT 2: if you're on Stata < 16, matsize caps out at 11,000. Total
* regressors here are roughly (num supported schools) x 1 [ibn dummies] +
* (num supported schools) x up to 2 [gp_used prob_/iszero_] + ~50 [x_terms].
* At k=100 (~1,600 schools) that's comfortably under 11,000.

tempfile results_accum
local results_first = 1

* Saved once here (not preserve/restore) so each outcome's loop iteration
* below can start from a guaranteed-clean copy via `use`. `parmest` does its
* own internal preserve when extracting results -- if the loop instead used
* `preserve` at its top (kept open for the whole iteration), that collides
* with parmest's internal one (Stata does not support nested preserves:
* r(621) "already preserved"). `use `full_sample'' sidesteps this entirely.
tempfile full_sample
save `full_sample', replace

* ------------------------- Step 5: run RC VAM per outcome (NO regularization) -
foreach y of local outcomes {
    display as txt "---- RC VAM outcome: `y' ----"
    use `full_sample', clear

    if !inlist("`y'", "admission_exam_taker", "higher_ed_enrolled_m1") {
        keep if admission_exam_taker
    }

    egen byte miss_needed = rowmiss(school_rbd `y' `x_vars_needed')
    keep if miss_needed == 0
    drop miss_needed

    count
    if r(N) == 0 {
        display as error "No observations left for outcome `y' -- skipping."
        continue
    }

    * Drop g(p_i) columns with (near) zero variance in THIS outcome's
    * estimation sample -- same filter as the ridge/lasso versions, just no
    * further regularization on top of it.
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

    quietly levelsof school_rbd
    if r(r) < 2 {
        display as error "Fewer than 2 distinct schools for outcome `y' -- skipping."
        continue
    }
    display as txt "  Fitting single regression with " r(r) " school dummies (ibn., no reference category), g(p_i) unregularized..."

    * Single OLS fit, every g(p_i) term unpenalized -- the AHPW baseline.
    quietly regress `y' ibn.school_rbd `x_terms' `gp_used', noconstant

    preserve
        keep if e(sample)
        contract school_rbd, freq(n_students_regression)
        tempfile school_counts
        save `school_counts', replace
    restore

    parmest, norestore level(95)
    keep if strpos(parm, ".school_rbd")
    gen str32 school_str = regexr(parm, "\.school_rbd$", "")
    destring school_str, gen(school_rbd) force
    drop if missing(school_rbd)
    drop school_str parm

    rename estimate controlled_value_added
    rename stderr controlled_value_added_se

    merge 1:1 school_rbd using `school_counts', keep(match) nogenerate

    quietly sum n_students_regression
    local n_total = r(sum)
    gen double _w = n_students_regression / `n_total'
    quietly sum controlled_value_added [aw=_w]
    local weighted_mean = r(mean)
    gen controlled_value_added_centered_student = controlled_value_added - `weighted_mean'
    drop _w

    gen str32 outcome = "`y'"
    gen n_students_regression_total = `n_total'
    gen n_gp_terms_candidate = wordcount("`gp_used'")
    gen n_gp_terms_used = wordcount("`gp_used'")
    gen str48 controlled_value_added_se_method = "stata_regress_ibn_dummy_simple_se_no_regularization"

    if `results_first' {
        save `results_accum', replace
        local results_first = 0
    }
    else {
        append using `results_accum'
        save `results_accum', replace
    }
}

* ------------------------- Step 6: write outputs ------------------------------
use `results_accum', clear
order outcome school_rbd controlled_value_added controlled_value_added_se ///
    controlled_value_added_centered_student n_students_regression ///
    n_students_regression_total n_gp_terms_candidate n_gp_terms_used
gen str32 analysis_sample = "SAE_timely_RC_VAM_stata"

export delimited using "`output_path'", replace

preserve
    collapse (count) n_schools = school_rbd (sum) n_students_regression_tot = n_students_regression ///
        (first) n_gp_terms_candidate n_gp_terms_used (sd) sd_rc_vam = controlled_value_added_centered_student, by(outcome)
    export delimited using "`diagnostics_path'", replace
restore

display as txt "Done."
display as txt "Wrote: `output_path'"
display as txt "Wrote: `diagnostics_path'"
