********************************************************************************
* Risk-Controlled Value-Added (RC VAM) -- Stata port -- LASSO ON g(p_i)
* (robustness-check sibling of 01_construct_rc_vam_school_values.do)
*
* Same estimation sample, same X_i controls, same outcomes, same g(p_i)
* construction as 01_construct_rc_vam_school_values.do (the "ridge" version)
* and 01_construct_rc_vam_school_values_noreg.do -- this script is the Lasso
* leg of a 3-way robustness check across regularization choices for the
* g(p_i) nuisance block: none (_noreg.do), Ridge (main file), Lasso (this
* file). See setup_md_codex/project_memory/decisions/2026-09-20-lasso-
* regularize-gp-block-stata-rc-vam.md (superseded, but documents this exact
* mechanism) and 2026-09-21-ridge-regularize-gp-block-stata-rc-vam.md (why
* Ridge became the primary spec) for the full history.
*
* Y_i = alpha_0 + sum_j alpha_j D_ij + X_i'Gamma + g(p_i) + eta_i
* (Angrist, Hull, Pathak & Walters 2024, eq. 4)
*
* REGULARIZATION: the ~3,000 prob_/iszero_ nuisance terms (at k=100) get
* L1-penalized/selected; the school dummies and X_i controls are "always
* included" and never penalized, so the reported school-level point
* estimates are not shrunk toward zero by the lasso step. Mechanism: ONE
* `lasso linear` fit per outcome with school dummies + X forced in, gp_used
* as the only penalized candidate pool, then a plain post-lasso OLS refit
* (ibn.school_rbd + noconstant, same design as the other two scripts) using
* only the lasso-selected gp terms.
*
* CAVEAT (why this is the secondary spec, not the primary one): AHPW's
* theory keeps every g(p_i) term unconditionally -- each term's job is
* satisfying conditional independence for its own school, not predicting the
* outcome well. Lasso selects on outcome-predictive power, so it can
* legitimately drop a CIA-relevant-but-outcome-weak term for some school.
* Ridge (main script) shrinks every term but never drops any, which is why
* it's the primary spec -- this script exists to CHECK whether that
* distinction actually matters empirically: if Lasso and Ridge give similar
* school-level estimates, the choice didn't matter much in practice; if they
* diverge, especially for schools where n_gp_terms_used << n_gp_terms_candidate
* in the diagnostics file, that is a real signal worth investigating.
*
* Deliberately NOT Stata's dsregress/poregress: those commands run a
* *separate* lasso for every "variable of interest," fine for a handful of
* policy variables but not for ~1,600+ school dummies.
*
* DEPENDENCY: this script does NOT rebuild g(p_i)/X/outcomes from scratch --
* it reuses `clean_dir'/rc_vam_stata_analysis_sample.dta, which is built by
* code/claude/rc_vam/00_build_rc_vam_analysis_sample.R (Rscript
* 00_build_rc_vam_analysis_sample.R). Run that R script at least once first;
* this one will error out with a clear message if the cached file isn't
* there yet.
*
* STATUS: written and reviewed but NOT executed (no Stata license/binary
* available in the environment this was written in). Two Stata mechanics
* below could not be verified: whether `e(allvars_sel)` is the right stored
* result for the lasso-selected variable list (falls back to `lassocoef,
* matrix()`, and degrades to keeping every gp_used term with a loud warning
* if both fail, rather than crashing), and whether `lasso linear`'s
* always-included-parentheses syntax accepts `noconstant` cleanly alongside
* a full (non-omitted) school dummy set this wide.
*
* REQUIRES: ssc install parmest. Stata's `lasso`/`lassocoef` suite is native
* to Stata 16+ (no separate install).
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
local output_path       "`output_dir'/rc_vam_school_values_stata_lasso.csv"
local diagnostics_path  "`output_dir'/rc_vam_school_values_diagnostics_stata_lasso.csv"

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

* Lasso lambda-selection method. "cv" (10-fold cross-validation) is the most
* data-driven but refits ~10x per candidate lambda; switch to "bic" (single
* fit, faster, slightly less data-driven) if cv is too slow given the
* ~3,000-term candidate pool.
local lasso_selection "cv"

* CAVEAT 2: if you're on Stata < 16, matsize caps out at 11,000. Total
* regressors here are roughly (num supported schools) x 1 [ibn/dummies] +
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

* ------------------------- Step 5: run RC VAM per outcome (Lasso on g(p_i)) --
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
    * estimation sample -- same filter as the other two scripts.
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

    * --- Lasso-regularize ONLY the g(p_i) block. School dummies + X
    * controls are forced in as "always included" (never penalized); gp_used
    * is the only candidate pool subject to the L1 penalty. `lasso`'s
    * support for ~1,600-level factor variables inside its always-included
    * list is untested here, so school_rbd is expanded into literal dummies
    * first rather than trusting ibn.school_rbd to work correctly inside
    * lasso's syntax. `noconstant` matters here too: the dummy set already
    * spans every school with no omitted level, so an added intercept would
    * be exactly collinear with it.
    quietly tabulate school_rbd, generate(sch_dum_)
    quietly ds sch_dum_*
    local school_dums `r(varlist)'

    display as txt "  Lasso-selecting g(p_i) terms (" (wordcount("`gp_used'")) " candidates; school dummies + X controls always included, unpenalized)..."
    quietly lasso linear `y' (`school_dums' `x_terms') `gp_used', selection(`lasso_selection') noconstant

    * Extract which gp_used terms survived selection (school_dums/x_terms are
    * always in, by construction, so we only need to filter gp_used against
    * whatever lasso reports as selected). e(allvars_sel) is the primary,
    * documented route; lassocoef's matrix() output is the fallback in case
    * the exact ereturn name differs across Stata versions/updates. If both
    * fail, degrade to keeping every gp_used term (i.e. no regularization for
    * that outcome) rather than crashing the whole loop -- and say so loudly.
    local selected_all
    capture local selected_all = e(allvars_sel)
    if _rc | "`selected_all'" == "" {
        capture lassocoef, matrix(B)
        if _rc == 0 {
            local selected_all : rownames B
        }
    }
    local gp_selected
    if "`selected_all'" != "" {
        foreach v of local gp_used {
            if strpos(" `selected_all' ", " `v' ") > 0 local gp_selected `gp_selected' `v'
        }
    }
    if "`selected_all'" == "" {
        display as error "  Could not extract lasso-selected variable list (check e(allvars_sel)/lassocoef syntax for this Stata version) -- keeping ALL g(p_i) terms unregularized for this outcome so the script doesn't crash."
        local gp_selected `gp_used'
    }
    display as txt "  -> " (wordcount("`gp_selected'")) " of " (wordcount("`gp_used'")) " g(p_i) terms survived lasso selection."

    display as txt "  Refitting plain OLS with " r(r) " school dummies (ibn., no reference category) + lasso-selected g(p_i) terms..."

    * Post-lasso OLS refit: identical ibn./noconstant design as the other two
    * scripts, but gp_used is now narrowed to gp_selected. This step is what
    * actually produces the reported, UNpenalized point estimates + SEs --
    * the lasso step above only decided which gp terms are worth keeping.
    quietly regress `y' ibn.school_rbd `x_terms' `gp_selected', noconstant

    drop sch_dum_*

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
    gen n_gp_terms_used = wordcount("`gp_selected'")
    gen str48 controlled_value_added_se_method = "stata_regress_ibn_dummy_simple_se_postlasso_gp"

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
