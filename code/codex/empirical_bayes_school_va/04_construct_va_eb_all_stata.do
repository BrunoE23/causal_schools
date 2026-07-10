/***********************************************************************
 Stata construction of school VA, regression SEs, and empirical Bayes

 This script rebuilds the school value-added inputs used by the EB IV path,
 but keeps the VA and EB stages entirely in Stata.

 The SE exported here is the SE of the student-weighted centered school
 effect, not the raw reference-normalized fixed effect:

   VA_s = alpha_s - sum_j w_j alpha_j

 It is computed from e(V) after each regression as a linear combination of
 the explicit school dummy coefficients.
***********************************************************************/

version 19
clear all
set more off
set varabbrev off
set maxvar 30000

global data_wd "C:/Users/brunem/Dropbox/causal_schools"
global repo_wd "C:/Users/brunem/Research/causal_schools"

global clean_dir "$data_wd/data/clean"
global output_dir "$repo_wd/output/tables/empirical_bayes_school_va"

cap mkdir "$repo_wd/output"
cap mkdir "$repo_wd/output/tables"
cap mkdir "$output_dir"

global universe_dta "$clean_dir/univ_gr8_df.dta"
global middle_csv "$clean_dir/middle_school_controls/middle_school_controls.csv"
global program_income_csv "$repo_wd/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv"

global va_csv "$output_dir/stata_school_rbd_observational_values_eb_inputs.csv"
global eb_csv "$output_dir/stata_eb_school_rbd_observational_values.csv"
global eb_diag_csv "$output_dir/stata_eb_school_rbd_observational_values_diagnostics.csv"
global log_path "$output_dir/stata_construct_va_eb_all.log"

log using "$log_path", replace text

mata:
real scalar _parse_stata_factor_level(string scalar s)
{
    real scalar i
    string scalar ch, out
    out = ""
    for (i = 1; i <= strlen(s); i++) {
        ch = substr(s, i, 1)
        if ((ch >= "0" & ch <= "9") | ch == "-" | ch == ".") {
            out = out + ch
        }
    }
    return(strtoreal(out))
}

void _write_centered_school_fe(
    string scalar outcome,
    string scalar schoolvar,
    string scalar tousevar,
    string scalar outfile,
    string scalar mode,
    real scalar n_total
)
{
    real rowvector b, school_b, l
    real matrix V, Vschool, stripe, info
    string matrix cstripe
    string vector names
    real vector pos, coef_ids, sid, ids, counts, w_ids, w_coef
    real vector centered, se, rawcoef
    real scalar i, j, k, dot, idx, mean_fe, fh, ncoef, nids
    string scalar nm, prefix, line

    b = st_matrix("e(b)")
    V = st_matrix("e(V)")
    cstripe = st_matrixcolstripe("e(b)")
    names = cstripe[, 2]

    pos = J(0, 1, .)
    coef_ids = J(0, 1, .)
    for (i = 1; i <= cols(b); i++) {
        nm = names[i]
        if (strpos(nm, "." + schoolvar) > 0 & strpos(nm, "o.") == 0 & strpos(nm, "b.") == 0) {
            dot = strpos(nm, ".")
            prefix = substr(nm, 1, dot - 1)
            coef_ids = coef_ids \ _parse_stata_factor_level(prefix)
            pos = pos \ i
        }
    }

    if (rows(pos) == 0) {
        errprintf("No explicit school coefficients found for %s\n", outcome)
        exit(498)
    }

    sid = st_data(., schoolvar, tousevar)
    sid = select(sid, sid :< .)
    sid = sort(sid, 1)
    info = panelsetup(sid, 1)
    nids = rows(info)
    ids = J(nids, 1, .)
    counts = J(nids, 1, .)
    for (i = 1; i <= nids; i++) {
        ids[i] = sid[info[i, 1]]
        counts[i] = info[i, 2] - info[i, 1] + 1
    }
    w_ids = counts :/ sum(counts)

    ncoef = rows(pos)
    school_b = J(1, ncoef, .)
    for (j = 1; j <= ncoef; j++) {
        school_b[j] = b[1, pos[j]]
    }
    Vschool = V[pos, pos]

    w_coef = J(ncoef, 1, 0)
    for (j = 1; j <= ncoef; j++) {
        idx = selectindex(ids :== coef_ids[j])
        if (rows(idx) == 1) {
            w_coef[j] = w_ids[idx[1]]
        }
    }
    mean_fe = school_b * w_coef

    rawcoef = J(nids, 1, 0)
    centered = J(nids, 1, .)
    se = J(nids, 1, .)
    for (i = 1; i <= nids; i++) {
        idx = selectindex(coef_ids :== ids[i])
        l = -w_coef'
        if (rows(idx) == 1) {
            rawcoef[i] = school_b[idx[1]]
            l[idx[1]] = l[idx[1]] + 1
        }
        centered[i] = rawcoef[i] - mean_fe
        se[i] = sqrt(l * Vschool * l')
    }

    fh = fopen(outfile, mode)
    if (mode == "w") {
        fput(fh, "school_rbd,analysis_sample,outcome,controlled_value_added,controlled_value_added_centered_student,controlled_value_added_se,controlled_value_added_se_method,n_students_regression,n_students_regression_total")
    }
    for (i = 1; i <= nids; i++) {
        line = strofreal(ids[i], "%20.0f") + "," + ///
            "All," + outcome + "," + ///
            strofreal(rawcoef[i], "%21.15g") + "," + ///
            strofreal(centered[i], "%21.15g") + "," + ///
            strofreal(se[i], "%21.15g") + "," + ///
            "stata_areg_centered_student_vce_unadjusted," + ///
            strofreal(counts[i], "%20.0f") + "," + ///
            strofreal(n_total, "%20.0f")
        fput(fh, line)
    }
    fclose(fh)
}
end

tempfile middle_controls program_income analytic

di as txt "Importing middle-school controls."
import delimited using "$middle_csv", clear varnames(1) case(preserve)
keep MRUN most_time_RBD_middle middle_years_observed z_gpa_middle_mean z_att_middle_mean
duplicates drop MRUN, force
compress
save `middle_controls', replace

di as txt "Importing program-income outcomes."
import delimited using "$program_income_csv", clear varnames(1) case(preserve)
keep MRUN program_income_clp_m1 log_program_income_clp_m1 program_income_source_m1 program_income_missing_m1
duplicates drop MRUN, force
compress
save `program_income', replace

di as txt "Preparing analytic student file."
use "$universe_dta", clear
merge 1:1 MRUN using `middle_controls', nogen keep(master match)
merge 1:1 MRUN using `program_income', nogen keep(master match)

gen double school_rbd = most_time_RBD
keep if !missing(school_rbd) & school_rbd > 0
keep if !missing(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16

foreach v in math_max leng_max {
    replace `v' = . if missing(`v') | `v' <= 0
}

bysort psu_year: egen z_year_math_max = std(math_max)
bysort psu_year: egen z_year_leng_max = std(leng_max)

gen byte admission_exam_taker = !missing(math_max) | !missing(leng_max)
keep if admission_exam_taker

replace f_science_m1 = 0 if missing(f_science_m1)
replace f_eng_m1 = 0 if missing(f_eng_m1)
gen byte stem_enrollment_m1 = (f_science_m1 == 1 | f_eng_m1 == 1)

gen byte observed_matricula_m1 = !missing(ACREDITADA_CARR_m1) | !missing(ACREDITADA_INST_m1) | !missing(ACRE_INST_ANIO_m1)
replace program_certified_years_m1 = 0 if missing(program_certified_years_m1) & !observed_matricula_m1

gen double inst_certified_years_m1 = .
replace inst_certified_years_m1 = ACRE_INST_ANIO_m1 if institution_accredited_m1 == 1
replace inst_certified_years_m1 = 0 if institution_accredited_m1 == 0
replace inst_certified_years_m1 = 0 if missing(institution_accredited_m1) & !observed_matricula_m1

gen double z_sim_mat_4to_sq = z_sim_mat_4to^2
gen double z_sim_mat_4to_cu = z_sim_mat_4to^3
gen double z_sim_leng_4to_sq = z_sim_leng_4to^2
gen double z_sim_leng_4to_cu = z_sim_leng_4to^3

compress
save `analytic', replace

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

local outcomes ///
    z_year_math_max ///
    z_year_leng_max ///
    stem_enrollment_m1 ///
    log_program_income_clp_m1 ///
    program_certified_years_m1 ///
    inst_certified_years_m1

local write_mode "w"
foreach y of local outcomes {
    use `analytic', clear
    di as txt "Estimating school VA in Stata for outcome: `y'"
    quietly areg `y' ib(first).school_rbd `controls', absorb(most_time_RBD_middle)
    gen byte __esample = e(sample)
    mata: _write_centered_school_fe("`y'", "school_rbd", "__esample", "$va_csv", "`write_mode'", st_numscalar("e(N)"))
    local write_mode "a"
}

di as txt "Constructing EB shrinkage in Stata."
import delimited using "$va_csv", clear varnames(1) case(preserve)
destring school_rbd controlled_value_added controlled_value_added_centered_student ///
    controlled_value_added_se n_students_regression n_students_regression_total, replace force

gen double eb_noise_variance = controlled_value_added_se^2
gen double eb_tau2 = .
gen double eb_tau = .
gen double eb_reliability = .
gen double controlled_value_added_eb = .
gen double controlled_value_added_eb_centered_student = .
gen double eb_center_student = .
gen double eb_variance_observed_student = .
gen double eb_mean_noise_variance_student = .

levelsof outcome, local(eb_outcomes)
foreach y of local eb_outcomes {
    quietly summarize controlled_value_added_centered_student [aw=n_students_regression] if outcome == "`y'", meanonly
    scalar center_student = r(mean)

    gen double __dev2 = (controlled_value_added_centered_student - center_student)^2 if outcome == "`y'"
    quietly summarize __dev2 [aw=n_students_regression] if outcome == "`y'", meanonly
    scalar variance_observed_student = r(mean)
    drop __dev2

    quietly summarize eb_noise_variance [aw=n_students_regression] if outcome == "`y'", meanonly
    scalar mean_noise_variance_student = r(mean)
    scalar tau2 = max(variance_observed_student - mean_noise_variance_student, 0)

    replace eb_tau2 = tau2 if outcome == "`y'"
    replace eb_tau = sqrt(tau2) if outcome == "`y'"
    replace eb_center_student = center_student if outcome == "`y'"
    replace eb_variance_observed_student = variance_observed_student if outcome == "`y'"
    replace eb_mean_noise_variance_student = mean_noise_variance_student if outcome == "`y'"
    replace eb_reliability = cond(tau2 == 0, 0, tau2 / (tau2 + eb_noise_variance)) if outcome == "`y'"
    replace controlled_value_added_eb = center_student + eb_reliability * (controlled_value_added_centered_student - center_student) if outcome == "`y'"

    quietly summarize controlled_value_added_eb [aw=n_students_regression] if outcome == "`y'", meanonly
    scalar eb_center = r(mean)
    replace controlled_value_added_eb_centered_student = controlled_value_added_eb - eb_center if outcome == "`y'"
}

gen str60 eb_method = "stata_normal_normal_weighted_by_n_students_regression"
export delimited using "$eb_csv", replace

preserve
collapse ///
    (count) n_schools=school_rbd ///
    (sum) n_students_regression=n_students_regression ///
    (first) tau2=eb_tau2 tau=eb_tau ///
    (mean) mean_reliability_school_unweighted=eb_reliability, by(outcome)
export delimited using "$eb_diag_csv", replace
restore

di as result "Wrote VA inputs: $va_csv"
di as result "Wrote EB values: $eb_csv"
di as result "Wrote EB diagnostics: $eb_diag_csv"

log close
