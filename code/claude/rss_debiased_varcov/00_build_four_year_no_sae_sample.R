###############################################################################
# Build the cached "four_year_no_sae" analysis sample for the RSS debiased
# Var/Cov estimator (01_construct_rss_debiased_varcov.R, RSS_VARCOV_SPEC=
# four_year_no_sae). Mirrors code/claude/rc_vam/00_build_rc_vam_analysis_
# sample.R's Step E (rich X + outcomes), but for cohorts 2017-2020 (4 years)
# and WITHOUT any g(p_i)/SAE join or restriction -- 2017 is includable here
# specifically BECAUSE no g(p_i) is needed (DA_probs_2017.csv doesn't exist,
# see 00_build_rc_vam_analysis_sample.R's header comment).
#
# WHY THIS EXISTS: 01_construct_rss_debiased_varcov.R originally rebuilt this
# sample from raw sources (univ_gr8_df.csv, middle-school controls, income
# CSV, postulaciones/asignaciones) INSIDE itself, every single run -- that
# meant re-reading the big univ_gr8_df.csv every time, and meant Acropolis
# would need that whole raw-source set transferred just to run this one
# spec. Splitting data construction into its own "00" script that writes ONE
# cached .dta -- exactly the same pattern 00_build_rc_vam_analysis_sample.R
# already uses for the three_year_sae_gpi spec's rc_vam_stata_analysis_
# sample.dta -- means: (1) only ONE file needs transferring to Acropolis for
# this spec, (2) rerunning 01_construct_rss_debiased_varcov.R after a code
# change to the estimation logic doesn't require re-reading univ_gr8_df.csv
# again, and (3) this can be rebuilt locally and re-uploaded the same way
# the RC VAM .dta already is.
#
# INPUT (all under CAUSAL_SCHOOLS_DATA_WD/data/clean/, except the income CSV
# which is under the repo's output/tables/):
#   univ_gr8_df.csv, middle_school_controls/middle_school_controls.csv,
#   postulaciones.dta, asignaciones.dta,
#   output/tables/mifuturo_matricula_income/mifuturo_person_level_income_
#     outcomes.csv
#
# OUTPUT: data/clean/rss_debiased_varcov/four_year_no_sae_sample.dta
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

log_ts <- function(...) {
  message(format(Sys.time(), "[%H:%M:%S] "), ...)
  flush(stderr())
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

lowercase_dedupe_names <- function(dt, label = deparse(substitute(dt))) {
  new_names <- tolower(names(dt))
  dupes <- duplicated(new_names)
  if (any(dupes)) {
    warning(
      label, ": dropping ", sum(dupes), " duplicate column(s) after lowercasing."
    )
    dt <- dt[, !dupes, with = FALSE]
    new_names <- new_names[!dupes]
  }
  setnames(dt, new_names)
  dt
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
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
clean_dir <- file.path(data_wd, "data", "clean")

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
# middle_path / income_path removed 2026-09-21 -- univ_gr8_df.csv now carries
# those columns natively; see the comment further down where they're used.
# REVISED 2026-09-21: read codex's already-built becas_creditos_outcomes.csv
# (any_postulacion/any_asignacion/grat_asignacion pre-computed over the full
# universe) instead of postulaciones.dta + asignaciones.dta directly -- see
# the matching change in code/claude/rc_vam/00_build_rc_vam_analysis_
# sample.R for the full rationale.
becas_path <- file.path(clean_dir, "becas_creditos", "becas_creditos_outcomes.csv")

output_dir <- file.path(clean_dir, "rss_debiased_varcov")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_path <- file.path(output_dir, "four_year_no_sae_sample.dta")

log_ts("Reading univ_gr8_df.csv: ", universe_path)
analytic <- fread(universe_path, na.strings = c("NA", ""))
analytic <- lowercase_dedupe_names(analytic, "univ_gr8_df.csv")
log_ts("  -> ", nrow(analytic), " rows, ", ncol(analytic), " cols.")

for (v in c("mrun", "student_id", "cohort_gr8", "most_time_rbd", "edad_alu")) {
  if (v %in% names(analytic)) analytic[[v]] <- suppressWarnings(as.numeric(analytic[[v]]))
}
setnames(analytic, "most_time_rbd", "school_rbd")

analytic <- analytic[cohort_gr8 %in% 2017:2020]
analytic <- analytic[!is.na(school_rbd) & school_rbd > 0]
analytic <- analytic[!is.na(edad_alu) & edad_alu >= 12 & edad_alu <= 16]
if ("student_id" %in% names(analytic)) analytic <- unique(analytic, by = "student_id")
log_ts("  -> ", nrow(analytic), " rows after cohort/school_rbd/edad_alu/dedup filters.")

analytic[, admission_exam_taker := as.integer(
  (!is.na(math_max) & math_max > 0) | (!is.na(leng_max) & leng_max > 0)
)]
analytic[!is.na(psu_year) & !is.na(math_max) & math_max > 0,
         z_year_math_max := (math_max - mean(math_max, na.rm = TRUE)) / stats::sd(math_max, na.rm = TRUE),
         by = psu_year]
analytic[!is.na(psu_year) & !is.na(leng_max) & leng_max > 0,
         z_year_leng_max := (leng_max - mean(leng_max, na.rm = TRUE)) / stats::sd(leng_max, na.rm = TRUE),
         by = psu_year]
analytic[, higher_ed_enrolled_m1 := as.integer(
  !is.na(cod_sies_m1) & as.character(cod_sies_m1) != "" & as.character(cod_sies_m1) != "NA"
)]
analytic[, stem_enrollment_m1 := as.integer(
  fifelse(is.na(f_science_m1), 0, as.numeric(f_science_m1)) == 1 |
    fifelse(is.na(f_eng_m1), 0, as.numeric(f_eng_m1)) == 1
)]

log_ts("Merging becas/creditos outcomes: ", becas_path)
becas_outcomes <- fread(becas_path, na.strings = c("NA", ""))
becas_outcomes <- lowercase_dedupe_names(becas_outcomes, "becas_creditos_outcomes.csv")
becas_outcomes[, mrun := suppressWarnings(as.numeric(mrun_key))]
becas_outcomes <- unique(becas_outcomes[, .(mrun, any_postulacion, any_asignacion, grat_asignacion)], by = "mrun")
analytic <- merge(analytic, becas_outcomes, by = "mrun", all.x = TRUE)

# REVISED 2026-09-21: univ_gr8_df.csv (the broad panel) now carries
# most_time_rbd_middle/middle_years_observed/z_gpa_middle_mean/
# z_att_middle_mean and high_inst_m1/high_paying_field_m1 NATIVELY -- these
# used to need separate merges from middle_school_controls.csv and the
# mifuturo income CSV, but re-merging them now (a) is redundant and (b)
# actively broke the build: merge() silently suffixes same-named non-key
# columns as .x/.y on collision instead of erroring, so the plain column
# names disappeared entirely and write_dta()'s own column check caught it.
# log_program_income_clp_m1 is the one exception -- the panel only has the
# renamed log_program_income_full_clp_m1, which
# code/codex/mifuturo_matricula_income/03_construct_person_level_income_
# outcomes.R documents as the same thing (a "backward-compatible alias to
# program_income_full" for older scripts), so alias it back here.
log_ts("Using native panel columns for middle-school controls + high-premium/income outcomes (no separate merge needed).")
analytic[, log_program_income_clp_m1 := suppressWarnings(as.numeric(log_program_income_full_clp_m1))]
analytic[, high_inst_m1 := suppressWarnings(as.numeric(high_inst_m1))]
analytic[, high_paying_field_m1 := suppressWarnings(as.numeric(high_paying_field_m1))]
for (v in c("most_time_rbd_middle", "middle_years_observed", "z_gpa_middle_mean", "z_att_middle_mean")) {
  analytic[[v]] <- suppressWarnings(as.numeric(analytic[[v]]))
}

analytic[, z_sim_mat_4to_2 := z_sim_mat_4to^2]
analytic[, z_sim_mat_4to_3 := z_sim_mat_4to^3]
analytic[, z_sim_leng_4to_2 := z_sim_leng_4to^2]
analytic[, z_sim_leng_4to_3 := z_sim_leng_4to^3]

# Trim to just the columns 01_construct_rss_debiased_varcov.R actually needs
# -- school_rbd/cohort_gr8/fixed effects, every outcome it might estimate,
# and every X control it uses. Keeps the cached .dta much smaller than the
# full univ_gr8_df.csv it was built from.
keep_cols <- c(
  "mrun", "student_id", "school_rbd", "most_time_rbd_middle", "cohort_gr8",
  "gen_alu", "edad_alu", "cod_com_alu",
  "income_decile_imputed",
  "father_educ_years_imputed", "mother_educ_years_imputed",
  "father_indigenous_imputed", "mother_indigenous_imputed",
  "sala_cuna_imputed", "jardin_imputed", "prekinder_imputed", "kinder_imputed",
  "z_gpa_middle_mean", "z_att_middle_mean", "middle_years_observed",
  "z_sim_mat_4to", "z_sim_mat_4to_2", "z_sim_mat_4to_3",
  "z_sim_leng_4to", "z_sim_leng_4to_2", "z_sim_leng_4to_3",
  "admission_exam_taker", "z_year_math_max", "z_year_leng_max",
  "higher_ed_enrolled_m1", "stem_enrollment_m1", "log_program_income_clp_m1",
  "high_inst_m1", "high_paying_field_m1",
  "any_postulacion", "any_asignacion", "grat_asignacion"
)
missing_keep_cols <- setdiff(keep_cols, names(analytic))
if (length(missing_keep_cols) > 0) {
  stop("Expected columns missing before write_dta(): ", paste(missing_keep_cols, collapse = ", "))
}
analytic <- analytic[, ..keep_cols]

log_ts("Writing: ", output_path, " (", nrow(analytic), " rows x ", ncol(analytic), " cols)")
write_dta(analytic, output_path)
log_ts("Done.")
