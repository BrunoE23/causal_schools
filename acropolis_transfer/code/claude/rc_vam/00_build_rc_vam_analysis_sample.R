####################################
# RC VAM analysis sample -- SINGLE R data-prep script ("Step 0")
#
# Consolidates what used to be spread across three places --
#   00_export_probability_controls_all_timely_sae.R (probs/roster/support),
#   00b_build_gp_wide_dta.R (wide g(p_i) pivot), and
#   Steps 1-3 of 01_construct_rc_vam_school_values.do (reshape + rich X/
#     outcomes construction + the wide merge)
# -- into ONE script. Output: ONE file,
#   data/clean/rc_vam_stata_analysis_sample.dta
# that every downstream Stata script (01_construct_rc_vam_school_values.do
# [ridge], _lasso.do, _noreg.do) just `use`s directly. Stata's job is now
# ONLY: load this file, define x_terms/outcomes, run regressions, export
# results -- no reshape, no wide merge, nothing that was making Stata slow
# or appear stuck.
#
# WHY: 01_construct_rc_vam_school_values.do's Step 1 (reshape/greshape) and
# Step 3 (merging a ~220-column dataset against a ~3,000+ column g(p_i)
# matrix) were both slow or appeared to hang in Stata. data.table::dcast
# (wide pivot) and a data.table key-merge (analytic + g(p_i) join) are both
# dramatically faster for exactly this "many columns" case -- this script
# does both in R, once, in memory (no intermediate long-format CSV
# round-trip like the two scripts it replaces used), and hands Stata a
# single ready-to-use .dta.
#
# COHORTS: sae_proceso restricted to 2018-2020 -- see decision doc
# setup_md_codex/project_memory/decisions/
# 2026-09-21-rc-vam-restrict-to-3-cohorts-2018-2020.md. 2021 is excluded
# because z_sim_mat_4to (baseline 4th-grade SIMCE, a required X control) is
# missing for 93.9% of timely 2021 applicants, vs. 15.6-22.8% for 2017-2020.
# 2017 is not included: DA_probs_2017.csv doesn't exist, so g(p_i) can't be
# built for it (usable only for a separate g(p_i)-free plain-VAM check).
#
# The two scripts this replaces (00_export_probability_controls_all_timely_
# sae.R, 00b_build_gp_wide_dta.R) are left in place, untouched, for
# provenance -- this script is what should actually be run going forward.
####################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

# Lowercase column names (matching Stata's default import delimited
# behavior, which every .do file in this pipeline assumes) AND resolve any
# resulting duplicates -- e.g. a raw CSV with both "mrun" and "MRUN" as
# genuinely separate columns (case-distinct, so fread() keeps them as two
# columns) collapses to two identical "mrun" columns once lowercased. Stata
# handles this at import time by silently auto-renaming the second one; this
# does the R equivalent -- keeps the FIRST occurrence, drops later
# duplicates, and says so loudly, since haven::write_dta() will hard-error
# on a genuine duplicate name much later (and less informatively) otherwise.
lowercase_dedupe_names <- function(dt, label = deparse(substitute(dt))) {
  new_names <- tolower(names(dt))
  dupes <- duplicated(new_names)
  if (any(dupes)) {
    dropped <- names(dt)[dupes]
    warning(
      label, ": dropping ", sum(dupes), " duplicate column(s) after ",
      "lowercasing (case-distinct in the raw file, identical once ",
      "lowercased): ", paste(dropped, collapse = ", "),
      ". Keeping the first occurrence of each."
    )
    dt <- dt[, !dupes, with = FALSE]
    new_names <- new_names[!dupes]
  }
  setnames(dt, new_names)
  dt
}

.script_start_time <- Sys.time()
log_stage <- function(label, start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message(sprintf("  [timing] %s: %.1f sec", label, elapsed))
  invisible(elapsed)
}

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }
  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- "C:/Users/brunem/Research/causal_schools"

rc_vam_min_year <- 2018L
rc_vam_max_year <- 2020L
support_k <- as.integer(Sys.getenv("RC_VAM_MIN_SUPPORT", unset = "100"))
probability_digits <- 2L

probs_dir <- file.path(data_wd, "data", "clean", "DA_probs")
universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")
middle_path <- file.path(data_wd, "data", "clean", "middle_school_controls", "middle_school_controls.csv")
income_path <- file.path(data_wd, "data", "clean", "mifuturo_person_level_income_outcomes.csv")
output_path <- file.path(data_wd, "data", "clean", "rc_vam_stata_analysis_sample.dta")

message("data_wd = ", data_wd)
message("Cohorts: ", rc_vam_min_year, "-", rc_vam_max_year, ", support_k = ", support_k)

# ------------------------- Step A: read univ_gr8_df.csv ONCE ------------------
# Read the full, wide file once and lowercase every column name immediately,
# matching exactly what Stata's default import delimited does (no
# case(preserve) anywhere in this project) -- every downstream reference
# below uses lowercase names, so this keeps this script and the .do files
# it feeds using the same names throughout. Reading once (rather than the
# .do file's old pattern of importing univ_gr8_df.csv twice, separately, for
# the roster vs. the rich controls) also removes the exact bug class that
# bit the Stata version (rbd_treated_1r ending up a different type in two
# separately-read copies of the same column).
.t0 <- Sys.time()
message("Reading univ_gr8_df.csv (this is the big one): ", universe_path)
univ_raw <- fread(universe_path, na.strings = c("NA", ""))
univ_raw <- lowercase_dedupe_names(univ_raw, "univ_gr8_df.csv")
log_stage("read_univ_gr8_df", .t0)
message("  -> ", nrow(univ_raw), " rows, ", ncol(univ_raw), " columns.")

for (v in c("mrun", "student_id", "cohort_gr8", "sae_proceso", "timely_sae", "rbd_treated_1r")) {
  if (v %in% names(univ_raw)) univ_raw[[v]] <- suppressWarnings(as.numeric(univ_raw[[v]]))
}

# ------------------------- Step B: roster (timely SAE, 3 cohorts) -----------
.t0 <- Sys.time()
roster_base <- univ_raw[
  timely_sae == 1L & sae_proceso >= rc_vam_min_year & sae_proceso <= rc_vam_max_year,
  .(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1r)
]
roster_base <- unique(roster_base, by = "student_id")
log_stage("build_roster_base", .t0)
message("  -> ", nrow(roster_base), " timely SAE students in ", rc_vam_min_year, "-", rc_vam_max_year, ".")

# ------------------------- Step C: stack DA_probs, build g(p_i) --------------
read_da_prob_file <- function(year) {
  path <- file.path(probs_dir, paste0("DA_probs_", year, ".csv"))
  dt <- fread(path, na.strings = c("NA", ""))
  dt <- lowercase_dedupe_names(dt, paste0("DA_probs_", year, ".csv"))
  dt[, student_id := suppressWarnings(as.numeric(student_id))]
  dt[, prob := suppressWarnings(as.numeric(prob))]
  dt[, school_id2 := ifelse(school_id == "unmatched", paste0("unmatched_", year), school_id)]
  dt[, rbd_prob := trimws(sub("_.*", "", school_id2))]
  dt[, sae_proceso := year]
  dt[, .(student_id, sae_proceso, rbd_prob, prob)]
}

.t0 <- Sys.time()
years <- seq(rc_vam_min_year, rc_vam_max_year)
missing_years <- years[!file.exists(file.path(probs_dir, paste0("DA_probs_", years, ".csv")))]
if (length(missing_years) > 0) {
  warning("Missing DA_probs files for: ", paste(missing_years, collapse = ", "))
}
years <- setdiff(years, missing_years)
probs_all <- rbindlist(lapply(years, read_da_prob_file))
log_stage("read_da_probs_all_years", .t0)
message("  -> ", nrow(probs_all), " raw probability rows across ", length(years), " years.")

.t0 <- Sys.time()
probs_estimation <- merge(
  probs_all,
  roster_base[, .(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1r)],
  by = c("student_id", "sae_proceso")
)
probs_estimation <- probs_estimation[
  , .(prob_raw = sum(prob, na.rm = TRUE)),
  by = .(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1r, rbd_prob)
]
probs_estimation[, prob_r := round(prob_raw, probability_digits)]
log_stage("build_probs_estimation", .t0)

.t0 <- Sys.time()
risk_flags <- probs_estimation[
  , .(
    any_risk = as.integer(max(prob_raw, na.rm = TRUE) < 1),
    total_probability_mass = sum(prob_raw, na.rm = TRUE),
    n_positive_probability_options = sum(prob_raw > 0, na.rm = TRUE)
  ),
  by = .(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1r)
]
log_stage("build_risk_flags", .t0)

.t0 <- Sys.time()
message("Defining practical school support (k = ", support_k, ") over ALL timely SAE applicants in ",
        rc_vam_min_year, "-", rc_vam_max_year, ".")
school_support <- probs_estimation[prob_raw > 0]
school_support <- school_support[, .(n_students_positive_prob = uniqueN(student_id)), by = rbd_prob]
support_rbd <- school_support[n_students_positive_prob >= support_k, rbd_prob]
log_stage("compute_school_support", .t0)
message("  -> ", length(support_rbd), " supported schools at k = ", support_k, ".")

.t0 <- Sys.time()
probs_supported <- probs_estimation[rbd_prob %in% support_rbd & prob_r != 0, .(student_id, sae_proceso, rbd_prob, prob_r)]
log_stage("filter_probs_supported", .t0)
message("  -> ", nrow(probs_supported), " nonzero (student, school) triples.")

# ------------------------- Step D: pivot wide (data.table::dcast) -----------
# This is the step that was slow/hanging as Stata `reshape`/`greshape` --
# dcast is a purpose-built, C-level pivot for exactly this "many output
# columns" case and is typically much faster.
.t0 <- Sys.time()
message("Pivoting g(p_i) wide with data.table::dcast...")
prob_wide <- dcast(probs_supported, student_id + sae_proceso ~ rbd_prob, value.var = "prob_r", fill = 0)
elapsed_pivot <- log_stage("dcast_pivot", .t0)
message("  -> ", ncol(prob_wide) - 2, " probability columns. (", round(elapsed_pivot, 1), " sec)")

prob_cols_raw <- setdiff(names(prob_wide), c("student_id", "sae_proceso"))
setnames(prob_wide, prob_cols_raw, paste0("prob_", prob_cols_raw))
prob_cols <- paste0("prob_", prob_cols_raw)

.t0 <- Sys.time()
gp_wide <- merge(
  roster_base[, .(student_id, mrun, cohort_gr8, sae_proceso, timely_sae, rbd_treated_1r)],
  prob_wide,
  by = c("student_id", "sae_proceso"),
  all.x = TRUE
)
for (col in prob_cols) gp_wide[is.na(get(col)), (col) := 0]
iszero_cols <- sub("^prob_", "iszero_", prob_cols)
for (i in seq_along(prob_cols)) gp_wide[, (iszero_cols[i]) := as.integer(get(prob_cols[i]) == 0)]

# mrun/cohort_gr8/timely_sae/rbd_treated_1r came along for the ride from
# roster_base, but `analytic` (built from univ_raw below) already carries
# its own authoritative copies of all four -- keeping both here would make
# Step F's merge collide on every one of them (haven::write_dta's duplicate-
# name check is what actually surfaces this, but it would have been true of
# any of the four, not just mrun). Drop them so gp_wide contributes only
# what's actually new: prob_*/iszero_*.
gp_wide[, c("mrun", "cohort_gr8", "timely_sae", "rbd_treated_1r") := NULL]
log_stage("build_gp_wide", .t0)
message("  -> gp_wide: ", nrow(gp_wide), " rows, ", length(prob_cols), " prob_* + ", length(iszero_cols), " iszero_* columns.")

# ------------------------- Step E: rich X_i controls + outcomes --------------
# Mirrors Step 2 of 01_construct_rc_vam_school_values.do exactly (same
# variable names, same transformations), just in R instead of Stata, and
# reusing univ_raw instead of re-importing univ_gr8_df.csv a second time.
.t0 <- Sys.time()
analytic <- copy(univ_raw)
num_vars <- c(
  "mrun", "student_id", "cohort_gr8", "sae_proceso", "timely_sae",
  "most_time_rbd", "gen_alu", "edad_alu", "cod_com_alu",
  "income_decile_imputed", "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed", "sala_cuna_imputed",
  "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "math_max", "leng_max", "psu_year",
  "f_science_m1", "f_eng_m1", "program_certified_years_m1", "acre_inst_anio_m1",
  "institution_accredited_m1", "rbd_treated_1r",
  "z_sim_mat_4to", "z_sim_leng_4to"
)
for (v in num_vars) {
  if (v %in% names(analytic)) analytic[[v]] <- suppressWarnings(as.numeric(analytic[[v]]))
}

analytic <- analytic[sae_proceso >= rc_vam_min_year & sae_proceso <= rc_vam_max_year]

analytic[math_max <= 0, math_max := NA]
analytic[leng_max <= 0, leng_max := NA]
analytic[psu_year %in% c(2021, 2022), math_max := math_max * (1000 / 850)]
analytic[psu_year %in% c(2021, 2022), leng_max := leng_max * (1000 / 850)]

std_by_group <- function(x, g) {
  ave_x <- ave(x, g, FUN = function(z) mean(z, na.rm = TRUE))
  sd_x <- ave(x, g, FUN = function(z) sd(z, na.rm = TRUE))
  (x - ave_x) / sd_x
}
analytic[, z_year_math_max := std_by_group(math_max, psu_year)]
analytic[, z_year_leng_max := std_by_group(leng_max, psu_year)]

analytic[, admission_exam_taker := as.integer(!is.na(math_max) | !is.na(leng_max))]

cod_sies_str <- trimws(as.character(analytic[["cod_sies_m1"]]))
analytic[, higher_ed_enrolled_m1 := as.integer(!is.na(cod_sies_m1) & cod_sies_str != "" & cod_sies_str != "NA")]

analytic[is.na(f_science_m1), f_science_m1 := 0]
analytic[is.na(f_eng_m1), f_eng_m1 := 0]
analytic[, stem_enrollment_m1 := as.integer(f_science_m1 == 1 | f_eng_m1 == 1)]

observed_matricula_m1 <- !is.na(analytic[["acreditada_carr_m1"]]) |
  !is.na(analytic[["acreditada_inst_m1"]]) | !is.na(analytic[["acre_inst_anio_m1"]])
analytic[is.na(program_certified_years_m1) & !observed_matricula_m1, program_certified_years_m1 := 0]

analytic[, inst_certified_years_m1 := NA_real_]
analytic[institution_accredited_m1 == 1, inst_certified_years_m1 := acre_inst_anio_m1]
analytic[institution_accredited_m1 == 0, inst_certified_years_m1 := 0]
analytic[is.na(institution_accredited_m1) & !observed_matricula_m1, inst_certified_years_m1 := 0]

setnames(analytic, "most_time_rbd", "school_rbd")
analytic <- analytic[!is.na(school_rbd) & school_rbd > 0]
analytic <- analytic[!is.na(edad_alu) & edad_alu >= 12 & edad_alu <= 16]
analytic <- unique(analytic, by = "student_id")
log_stage("build_rich_x_and_outcomes", .t0)
message("  -> analytic: ", nrow(analytic), " rows after school_rbd/edad_alu/dedup filters.")

# Benefits/credits (becas/creditos) outcomes, joined on mrun.
# Mirrors code/codex/becas_creditos/01_run_becas_creditos_va_iv.R's outcome
# construction (same postulaciones.dta/asignaciones.dta source, same
# definitions) -- not part of the paper, added per Bruno's request to get
# RC VAM estimates for these too, matching the more recent codex analysis.
.t0 <- Sys.time()
postulaciones_path <- file.path(data_wd, "data", "clean", "postulaciones.dta")
asignaciones_path <- file.path(data_wd, "data", "clean", "asignaciones.dta")
post_ids <- unique(suppressWarnings(as.numeric(read_dta(postulaciones_path, col_select = "mrun")$mrun)))
assignment_source <- as.data.table(read_dta(asignaciones_path, col_select = c("mrun", "beneficio_beca_fscu")))
assignment_source[, mrun := suppressWarnings(as.numeric(mrun))]
assignment_ids <- unique(assignment_source$mrun)
gratuidad_ids <- unique(assignment_source[toupper(trimws(beneficio_beca_fscu)) == "GRATUIDAD", mrun])
analytic[, any_postulacion := as.integer(mrun %in% post_ids)]
analytic[, any_asignacion := as.integer(mrun %in% assignment_ids)]
analytic[, grat_asignacion := as.integer(mrun %in% gratuidad_ids)]
log_stage("build_becas_creditos_outcomes", .t0)
message(
  "  -> any_postulacion: ", sum(analytic$any_postulacion),
  "; any_asignacion: ", sum(analytic$any_asignacion),
  "; grat_asignacion: ", sum(analytic$grat_asignacion),
  " (of ", nrow(analytic), ")."
)

# Program income outcome + high-premium institution/field flags, joined on
# mrun. high_inst_m1 / high_paying_field_m1 added 2026-09-2X per Bruno's
# request -- same source CSV as the income outcome, already built by
# code/codex/mifuturo_matricula_income/03_construct_person_level_income_
# outcomes.R (derive_high_institution / derive_high_paying_field). Not part
# of the paper's original outcome set, added for the more recent analysis.
.t0 <- Sys.time()
income_outcomes <- fread(income_path, na.strings = c("NA", ""))
income_outcomes <- lowercase_dedupe_names(income_outcomes, "mifuturo_person_level_income_outcomes.csv")
income_outcomes <- income_outcomes[, .(mrun, program_income_clp_m1, log_program_income_clp_m1,
                                        high_inst_m1, high_paying_field_m1)]
income_outcomes[, mrun := suppressWarnings(as.numeric(mrun))]
income_outcomes[, program_income_clp_m1 := suppressWarnings(as.numeric(program_income_clp_m1))]
income_outcomes[, log_program_income_clp_m1 := suppressWarnings(as.numeric(log_program_income_clp_m1))]
income_outcomes[, high_inst_m1 := suppressWarnings(as.numeric(high_inst_m1))]
income_outcomes[, high_paying_field_m1 := suppressWarnings(as.numeric(high_paying_field_m1))]
income_outcomes <- unique(income_outcomes, by = "mrun")
analytic <- merge(analytic, income_outcomes, by = "mrun", all.x = TRUE)
log_stage("merge_income_outcomes", .t0)

# Middle-school controls, joined on mrun.
.t0 <- Sys.time()
middle_controls <- fread(middle_path, na.strings = c("NA", ""))
middle_controls <- lowercase_dedupe_names(middle_controls, "middle_school_controls.csv")
middle_controls <- middle_controls[, .(mrun, most_time_rbd_middle, middle_years_observed, z_gpa_middle_mean, z_att_middle_mean)]
for (v in c("mrun", "most_time_rbd_middle", "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean")) {
  middle_controls[[v]] <- suppressWarnings(as.numeric(middle_controls[[v]]))
}
middle_controls <- unique(middle_controls, by = "mrun")
analytic <- merge(analytic, middle_controls, by = "mrun", all.x = TRUE)
log_stage("merge_middle_controls", .t0)

# Baseline SIMCE cubic polynomial terms.
analytic[, z_sim_mat_4to_2 := z_sim_mat_4to^2]
analytic[, z_sim_mat_4to_3 := z_sim_mat_4to^3]
analytic[, z_sim_leng_4to_2 := z_sim_leng_4to^2]
analytic[, z_sim_leng_4to_3 := z_sim_leng_4to^3]

# ------------------------- Step F: merge analytic with g(p_i) ---------------
# INNER join (Stata's Step 3 used keep(match)) -- a student needs both a
# valid rich-X/outcomes row AND a g(p_i) row to enter the regression sample.
.t0 <- Sys.time()
rc_vam_sample <- merge(analytic, gp_wide, by = c("student_id", "sae_proceso"))
log_stage("merge_analytic_with_gp_wide", .t0)
message("RC VAM estimation sample: ", nrow(rc_vam_sample), " students, ", length(prob_cols), " supported schools.")

# ------------------------- Step G: trim to what's actually needed -----------
# `analytic` still carries every one of univ_gr8_df.csv's ~220 columns
# (read once, in full, in Step A) -- including several imputation-diagnostic
# columns (the "_missing_after_impute" suite that comes with every
# "_imputed" covariate) whose names run past Stata's 32-character variable
# name limit, e.g. "father_indigenous_missing_after_impute" (38 chars).
# `write_dta()` correctly refuses these. Rather than rename them (they
# aren't used by any downstream .do script anyway -- x_terms/x_vars_needed/
# outcomes only ever reference the "_imputed" columns themselves, not their
# missingness flags), just keep the explicit set of columns the three
# regression scripts actually reference, plus join keys/provenance columns.
keep_cols <- c(
  "student_id", "sae_proceso", "mrun", "school_rbd", "cohort_gr8",
  "gen_alu", "edad_alu", "cod_com_alu", "timely_sae", "rbd_treated_1r",
  "income_decile_imputed", "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "z_gpa_middle_mean", "z_att_middle_mean", "middle_years_observed",
  "z_sim_mat_4to", "z_sim_mat_4to_2", "z_sim_mat_4to_3",
  "z_sim_leng_4to", "z_sim_leng_4to_2", "z_sim_leng_4to_3",
  "z_year_math_max", "z_year_leng_max", "admission_exam_taker",
  "higher_ed_enrolled_m1", "stem_enrollment_m1", "log_program_income_clp_m1",
  "program_certified_years_m1", "inst_certified_years_m1",
  "any_postulacion", "any_asignacion", "grat_asignacion",
  "high_inst_m1", "high_paying_field_m1",
  prob_cols, iszero_cols
)
missing_keep_cols <- setdiff(keep_cols, names(rc_vam_sample))
if (length(missing_keep_cols) > 0) {
  stop("Expected columns missing from rc_vam_sample before write_dta(): ", paste(missing_keep_cols, collapse = ", "))
}
rc_vam_sample <- rc_vam_sample[, ..keep_cols]
message("Trimmed to ", ncol(rc_vam_sample), " columns (from ", ncol(analytic) + length(prob_cols) + length(iszero_cols), ") before writing.")

# ------------------------- Step H: write output ------------------------------
# Reverted 2026-09-21: tried readstata13::save.dta13(..., compress = TRUE)
# here to shrink the .dta before writing (byte/float instead of long/double
# per column), but compress=TRUE requires an extra full scan of every
# column's value range to pick its type, and readstata13 itself is slower
# than haven regardless -- net effect was a much slower R-side write for a
# smaller file, which made things worse, not better. Back to plain
# haven::write_dta(): faster write, larger file, and the `compress` call
# already in each .do script right after `use` does the type-shrinking
# instead -- that's a cheap, in-memory, already-loaded-data operation, not
# the slow part (the slow part was always the read/reshape/merge, not
# compress itself).
.t0 <- Sys.time()
write_dta(rc_vam_sample, output_path)
log_stage("write_dta", .t0)

.total_elapsed <- as.numeric(difftime(Sys.time(), .script_start_time, units = "secs"))
message("Done.")
message("Wrote: ", output_path)
message(sprintf("Total elapsed: %.1f sec (%.1f min)", .total_elapsed, .total_elapsed / 60))
