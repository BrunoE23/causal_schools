/***********************************************************************
 Stata VA + EB for one outcome at a time

 Usage from Stata:
   do code/codex/empirical_bayes_school_va/05_construct_va_eb_one_outcome_stata.do math

Outcome keys:
   math, language, exam, enrolled, stem,
   highpay,
   program_income_area, program_income_institution, program_income_full,
   program_income, progcert, instcert

 The script caches the prepared analytic student file, then estimates only
 one school value-added regression. It records preparation, VA, and EB times.
***********************************************************************/

version 19
clear all
set more off
set varabbrev off
set maxvar 30000

args outcome_key rebuild_cache
if "`outcome_key'" == "" {
    di as error "Pass an outcome key: math, language, exam, enrolled, stem, highpay, program_income_area, program_income_institution, program_income_full, program_income, progcert, instcert"
    exit 198
}
if "`rebuild_cache'" == "" {
    local rebuild_cache "0"
}

global data_wd "C:/Users/brunem/Dropbox/causal_schools"
global repo_wd "C:/Users/brunem/Research/causal_schools"
global clean_dir "$data_wd/data/clean"
global output_dir "$repo_wd/output/tables/empirical_bayes_school_va"

cap mkdir "$repo_wd/output"
cap mkdir "$repo_wd/output/tables"
cap mkdir "$output_dir"

global universe_dta "$clean_dir/univ_gr8_df.dta"
global middle_csv "$clean_dir/middle_school_controls/middle_school_controls.csv"
global program_income_csv "$repo_wd/output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes_stata_va.csv"
global analytic_dta "$output_dir/stata_va_analytic_cache.dta"

local outcome_var ""
local outcome_label ""
if "`outcome_key'" == "math" local outcome_var "z_year_math_max"
if "`outcome_key'" == "language" local outcome_var "z_year_leng_max"
if "`outcome_key'" == "exam" local outcome_var "admission_exam_taker"
if "`outcome_key'" == "enrolled" local outcome_var "higher_ed_enrolled_m1"
if "`outcome_key'" == "stem" local outcome_var "stem_enrollment_m1"
if "`outcome_key'" == "highpay" {
    local outcome_var "highpay_field_m1"
    local outcome_label "high_paying_field_m1"
}
if "`outcome_key'" == "highpay_field" {
    local outcome_var "highpay_field_m1"
    local outcome_label "high_paying_field_m1"
}
if "`outcome_key'" == "program_income_area" {
    local outcome_var "log_proginc_area_clp_m1"
    local outcome_label "log_program_income_area_clp_m1"
}
if "`outcome_key'" == "program_income_institution" {
    local outcome_var "log_proginc_inst_clp_m1"
    local outcome_label "log_program_income_institution_clp_m1"
}
if "`outcome_key'" == "program_income_full" {
    local outcome_var "log_proginc_full_clp_m1"
    local outcome_label "log_program_income_full_clp_m1"
}
if "`outcome_key'" == "program_income" {
    local outcome_var "log_proginc_full_clp_m1"
    local outcome_label "log_program_income_clp_m1"
}
if "`outcome_key'" == "progcert" local outcome_var "program_certified_years_m1"
if "`outcome_key'" == "instcert" local outcome_var "inst_certified_years_m1"
if "`outcome_label'" == "" local outcome_label "`outcome_var'"

if "`outcome_var'" == "" {
    di as error "Unknown outcome key: `outcome_key'"
    exit 198
}

global va_csv "$output_dir/stata_va_eb_input_`outcome_key'.csv"
global eb_csv "$output_dir/stata_eb_school_values_`outcome_key'.csv"
global eb_diag_csv "$output_dir/stata_eb_diagnostics_`outcome_key'.csv"
global timing_csv "$output_dir/stata_va_eb_timing_`outcome_key'.csv"
global log_path "$output_dir/stata_va_eb_`outcome_key'.log"

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

real scalar _sample_sd(real vector x)
{
    real scalar n, mu
    x = select(x, x :< .)
    n = rows(x)
    if (n <= 1) return(.)
    mu = mean(x)
    return(sqrt(colsum((x :- mu):^2) / (n - 1)))
}

void _write_centered_school_fe_short(
    string scalar outcome,
    string scalar schoolvar,
    string scalar tousevar,
    string scalar residvar,
    string scalar outfile,
    real scalar n_total
)
{
    real rowvector b, school_b, l
    real matrix V, Vschool, info
    string matrix cstripe
    string vector names
    real matrix sample_data
    real vector pos, coef_ids, sid, resid, ids, counts, w_ids, w_coef, Vw
    real vector centered, se, rawcoef, resid_sd
    real scalar i, j, dot, idx, mean_fe, fh, ncoef, nids, wVw, se2
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

    sample_data = st_data(., (schoolvar, residvar), tousevar)
    sample_data = select(sample_data, sample_data[, 1] :< .)
    sample_data = sort(sample_data, 1)
    sid = sample_data[, 1]
    resid = sample_data[, 2]
    info = panelsetup(sid, 1)
    nids = rows(info)
    ids = J(nids, 1, .)
    counts = J(nids, 1, .)
    resid_sd = J(nids, 1, .)
    for (i = 1; i <= nids; i++) {
        ids[i] = sid[info[i, 1]]
        counts[i] = info[i, 2] - info[i, 1] + 1
        resid_sd[i] = _sample_sd(resid[|info[i, 1] \ info[i, 2]|])
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
    Vw = Vschool * w_coef
    wVw = w_coef' * Vw

    rawcoef = J(nids, 1, 0)
    centered = J(nids, 1, .)
    se = J(nids, 1, .)
    for (i = 1; i <= nids; i++) {
        idx = selectindex(coef_ids :== ids[i])
        if (rows(idx) == 1) {
            rawcoef[i] = school_b[idx[1]]
            se2 = Vschool[idx[1], idx[1]] - 2 * Vw[idx[1]] + wVw
        }
        else {
            se2 = wVw
        }
        centered[i] = rawcoef[i] - mean_fe
        se[i] = sqrt(max((se2, 0)))
    }

    fh = fopen(outfile, "w")
    fput(fh, "school_rbd,analysis_sample,outcome,va_raw,va_centered,va_se,va_resid_sd,va_se_method,n_students,n_total")
    for (i = 1; i <= nids; i++) {
        line = strofreal(ids[i], "%20.0f") + "," + ///
            "All," + outcome + "," + ///
            strofreal(rawcoef[i], "%21.15g") + "," + ///
            strofreal(centered[i], "%21.15g") + "," + ///
            strofreal(se[i], "%21.15g") + "," + ///
            strofreal(resid_sd[i], "%21.15g") + "," + ///
            "stata_areg_centered_student_vce_unadjusted," + ///
            strofreal(counts[i], "%20.0f") + "," + ///
            strofreal(n_total, "%20.0f")
        fput(fh, line)
    }
    fclose(fh)
}
end

timer clear
timer on 90

capture confirm file "$analytic_dta"
if _rc | "`rebuild_cache'" == "1" {
    di as txt "Preparing analytic cache: $analytic_dta"
    timer on 1

    tempfile middle_controls program_income

    import delimited using "$middle_csv", clear varnames(1) case(preserve)
    keep MRUN most_time_RBD_middle middle_years_observed z_gpa_middle_mean z_att_middle_mean
    duplicates drop MRUN, force
    compress
    save `middle_controls', replace

    import delimited using "$program_income_csv", clear varnames(1) case(preserve)
    keep MRUN ///
        proginc_area_clp_m1 log_proginc_area_clp_m1 proginc_area_src_m1 proginc_area_miss_m1 ///
        proginc_inst_clp_m1 log_proginc_inst_clp_m1 proginc_inst_src_m1 proginc_inst_miss_m1 ///
        proginc_full_clp_m1 log_proginc_full_clp_m1 proginc_full_src_m1 proginc_full_miss_m1 ///
        program_income_clp_m1 log_program_income_clp_m1 program_income_source_m1 program_income_missing_m1 ///
        highpay_field_m1
    duplicates drop MRUN, force
    compress
    save `program_income', replace

    use "$universe_dta", clear
    merge 1:1 MRUN using `middle_controls', nogen keep(master match)
    merge 1:1 MRUN using `program_income', nogen keep(master match)

    gen double school_rbd = most_time_RBD
    keep if !missing(school_rbd) & school_rbd > 0
    keep if !missing(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16
    keep if cohort_gr8 >= 2017 & cohort_gr8 <= 2020

    foreach v in math_max leng_max {
        replace `v' = . if missing(`v') | `v' <= 0
    }

    bysort psu_year: egen z_year_math_max = std(math_max)
    bysort psu_year: egen z_year_leng_max = std(leng_max)

    gen byte admission_exam_taker = !missing(math_max) | !missing(leng_max)

    gen byte higher_ed_enrolled_m1 = !missing(COD_SIES_m1)
    gen byte sies_m1_followup_observed = cohort_gr8 <= 2020
    replace higher_ed_enrolled_m1 = . if !sies_m1_followup_observed

    replace f_science_m1 = 0 if missing(f_science_m1) & sies_m1_followup_observed
    replace f_eng_m1 = 0 if missing(f_eng_m1) & sies_m1_followup_observed
    gen byte stem_enrollment_m1 = .
    replace stem_enrollment_m1 = (f_science_m1 == 1 | f_eng_m1 == 1) if sies_m1_followup_observed

    gen byte observed_matricula_m1 = !missing(ACREDITADA_CARR_m1) | !missing(ACREDITADA_INST_m1) | !missing(ACRE_INST_ANIO_m1)
    replace program_certified_years_m1 = 0 if missing(program_certified_years_m1) & !observed_matricula_m1 & sies_m1_followup_observed
    replace program_certified_years_m1 = . if !sies_m1_followup_observed

    gen double inst_certified_years_m1 = .
    replace inst_certified_years_m1 = ACRE_INST_ANIO_m1 if institution_accredited_m1 == 1
    replace inst_certified_years_m1 = 0 if institution_accredited_m1 == 0
    replace inst_certified_years_m1 = 0 if missing(institution_accredited_m1) & !observed_matricula_m1 & sies_m1_followup_observed
    replace inst_certified_years_m1 = . if !sies_m1_followup_observed

    foreach v in ///
        log_proginc_area_clp_m1 ///
        log_proginc_inst_clp_m1 ///
        log_proginc_full_clp_m1 ///
        log_program_income_clp_m1 ///
        highpay_field_m1 {
        capture confirm variable `v'
        if !_rc {
            replace `v' = . if !sies_m1_followup_observed
        }
    }

    gen double z_sim_mat_4to_sq = z_sim_mat_4to^2
    gen double z_sim_mat_4to_cu = z_sim_mat_4to^3
    gen double z_sim_leng_4to_sq = z_sim_leng_4to^2
    gen double z_sim_leng_4to_cu = z_sim_leng_4to^3

    compress
    save "$analytic_dta", replace

    timer off 1
}
else {
    di as txt "Using existing analytic cache: $analytic_dta"
    timer clear 1
}

use "$analytic_dta", clear
keep if cohort_gr8 >= 2017 & cohort_gr8 <= 2020
capture confirm variable higher_ed_enrolled_m1
if _rc {
    capture confirm variable COD_SIES_m1
    if _rc {
        di as error "Analytic cache lacks COD_SIES_m1; rerun with rebuild_cache=1."
        exit 111
    }
    gen byte higher_ed_enrolled_m1 = !missing(COD_SIES_m1)
}

capture confirm variable `outcome_var'
if _rc {
    di as error "Analytic cache lacks `outcome_var'; rerun with rebuild_cache=1 after regenerating MiFuturo outcomes."
    exit 111
}

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

di as txt "Estimating one-outcome school VA: `outcome_key' -> `outcome_label' (Stata variable: `outcome_var')"
capture erase "$va_csv"
capture erase "$eb_csv"
capture erase "$eb_diag_csv"
capture erase "$timing_csv"
timer on 2
quietly areg `outcome_var' ib(first).school_rbd `controls', absorb(most_time_RBD_middle)
gen byte __esample = e(sample)
predict double __resid if __esample, resid
local n_obs = e(N)
mata: _write_centered_school_fe_short("`outcome_label'", "school_rbd", "__esample", "__resid", "$va_csv", st_numscalar("e(N)"))
timer off 2

di as txt "Constructing one-outcome EB shrinkage: `outcome_key'"
timer on 3
import delimited using "$va_csv", clear varnames(1) case(preserve)
destring school_rbd va_raw va_centered va_se va_resid_sd n_students n_total, replace force

gen double noise_var = va_se^2
quietly summarize va_centered [aw=n_students], meanonly
scalar center_student = r(mean)
gen double dev2 = (va_centered - center_student)^2
quietly summarize dev2 [aw=n_students], meanonly
scalar var_observed = r(mean)
quietly summarize noise_var [aw=n_students], meanonly
scalar mean_noise = r(mean)
scalar tau2 = max(var_observed - mean_noise, 0)

gen double eb_tau2 = tau2
gen double eb_tau = sqrt(tau2)
gen double eb_reliability = cond(tau2 == 0, 0, tau2 / (tau2 + noise_var))
gen double va_eb = center_student + eb_reliability * (va_centered - center_student)
quietly summarize va_eb [aw=n_students], meanonly
scalar eb_center = r(mean)
gen double va_eb_centered = va_eb - eb_center
gen str60 eb_method = "stata_normal_normal_weighted_by_n_students_regression"

export delimited using "$eb_csv", replace

preserve
collapse ///
    (count) n_schools=school_rbd ///
    (sum) n_students_regression=n_students ///
    (first) tau2=eb_tau2 tau=eb_tau ///
    (mean) mean_rel_school=eb_reliability, by(outcome)
export delimited using "$eb_diag_csv", replace
restore
timer off 3
timer off 90

local prep_seconds = 0
capture quietly timer list 1
if !_rc {
    if r(t1) < . local prep_seconds = r(t1)
}
quietly timer list 2
local va_seconds = r(t2)
quietly timer list 3
local eb_seconds = r(t3)
quietly timer list 90
local total_seconds = r(t90)

quietly count
local n_schools = r(N)

file open timing using "$timing_csv", write replace text
file write timing "outcome_key,outcome,prep_seconds,va_seconds,eb_seconds,total_seconds,n_obs,n_schools" _n
file write timing "`outcome_key',`outcome_label',`prep_seconds',`va_seconds',`eb_seconds',`total_seconds',`n_obs',`n_schools'" _n
file close timing

di as result "Wrote VA input: $va_csv"
di as result "Wrote EB values: $eb_csv"
di as result "Wrote EB diagnostics: $eb_diag_csv"
di as result "Wrote timing: $timing_csv"
di as result "Outcome `outcome_key' seconds: prep=`prep_seconds', VA=`va_seconds', EB=`eb_seconds', total=`total_seconds'"

log close
