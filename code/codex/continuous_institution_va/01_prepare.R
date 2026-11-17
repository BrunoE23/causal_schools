# Prepare continuous two-way MiFuturo institution premiums for school VA.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
clean <- file.path(repo, "data/clean/continuous_institution_va")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
va_code <- file.path(repo, "code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R")
# Import configuration and functions only, without executing the constructor.
va_lines <- readLines(va_code, warn = FALSE)
va_env <- new.env(parent = globalenv())
eval(parse(text = va_lines[seq_len(grep("^# ---.*Load and prepare data", va_lines) - 1L)]), envir = va_env)
source_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
premium_path <- file.path(repo, "output/tables/mifuturo_matricula_income/mifuturo_income_fe_institution_effects.csv")
person_path <- file.path(repo, "output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv")
paths <- c(source_path, premium_path, person_path, va_code)
before <- file.info(paths)
stopifnot(all(file.exists(paths)))
cols <- unique(c("MRUN", "most_time_RBD", "math_max", "leng_max", "COD_SIES_m1",
  "COD_INST_m1", "TIPO_INST_1_m1", "NOMB_INST_m1", "high_inst_m1",
  va_env$control_vars, "most_time_rbd_middle"))
stopifnot(all(cols %in% names(fread(source_path, nrows = 0L))))
message("Reading selected universe columns; source bytes=", before$size[1])
d <- fread(source_path, select = cols, na.strings = c("", "NA"), showProgress = FALSE)
setnames(d, "most_time_rbd_middle", "most_time_RBD_middle")
stopifnot(!anyDuplicated(d$MRUN))
flow <- data.table(stage = "universe", students = nrow(d))
d <- d[cohort_gr8 %in% 2017:2020 & !is.na(most_time_RBD) & most_time_RBD > 0 & EDAD_ALU >= 12 & EDAD_ALU <= 16]
d[, school_rbd := as.numeric(most_time_RBD)]
flow <- rbind(flow, data.table(stage = "cohort_age_school", students = nrow(d)))
d <- d[!is.na(math_max) | !is.na(leng_max)]
flow <- rbind(flow, data.table(stage = "exam_takers", students = nrow(d)))
required <- c(va_env$control_vars, va_env$fixed_effect_vars)
fwrite(data.table(variable = required, missing = vapply(d[, ..required], function(x) sum(is.na(x)), integer(1))),
  file.path(clean, "missing_controls.csv"))
d <- d[complete.cases(d[, ..required])]
flow <- rbind(flow, data.table(stage = "complete_va_controls", students = nrow(d)))
normalize_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  trimws(gsub("\\s+", " ", gsub("[^A-Z0-9]+", " ", toupper(x))))
}
inst <- fread(premium_path)[effect_type == "institution"]
stopifnot(!anyDuplicated(inst$level), all(is.finite(inst$effect_log_clp_centered)),
  abs(mean(inst$effect_log_clp_centered)) < 1e-12,
  all(inst$model_name == "institution_plus_carrera_generica"))
d[, institution_key := paste(as.character(as.integer(COD_INST_m1)), normalize_text(TIPO_INST_1_m1),
  normalize_text(NOMB_INST_m1), sep = " || ")]
d[, enrolled := !is.na(COD_SIES_m1) & nzchar(trimws(as.character(COD_SIES_m1))) & as.character(COD_SIES_m1) != "NA"]
d[, institution_fe := inst$effect_log_clp_centered[match(institution_key, inst$level)]]
stopifnot(d[enrolled == FALSE, all(is.na(institution_fe))])
# Validate the join against the FE already attached by the original constructor.
# Read only six columns from the preflighted 2.78 GB file.
message("Validating against selected person-outcome columns")
p <- fread(person_path, select = c("MRUN", "cohort_gr8", "COD_SIES_m1", "matriculated_m1", "high_inst_m1",
  "mifuturo_institution_fe_log_clp_centered_m1"), na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(!anyDuplicated(p$MRUN))
p <- p[match(d$MRUN, MRUN)]
same <- function(x, y) all((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y))
stopifnot(same(d$MRUN, p$MRUN), same(d$cohort_gr8, p$cohort_gr8), same(d$COD_SIES_m1, p$COD_SIES_m1),
  same(d$enrolled, p$matriculated_m1), same(d$high_inst_m1, p$high_inst_m1),
  same(d$institution_fe, p$mifuturo_institution_fe_log_clp_centered_m1))
d[, binary_reconstructed := as.integer(enrolled & is.finite(institution_fe) & institution_fe > 0.1)]
stopifnot(same(d$binary_reconstructed, d$high_inst_m1))
# The legacy three-part key has type/name discrepancies. Institution codes
# are unique in this FE table and are the official SIES institution identifier.
inst[, COD_INST := as.integer(sub(" \\|\\|.*$", "", level))]
stopifnot(!anyNA(inst$COD_INST), !anyDuplicated(inst$COD_INST))
d[, institution_fe_legacy_join := institution_fe]
d[, institution_fe := inst$effect_log_clp_centered[match(as.integer(COD_INST_m1), inst$COD_INST)]]
d[, matched_fe_key := inst$level[match(as.integer(COD_INST_m1), inst$COD_INST)]]
stopifnot(d[is.finite(institution_fe_legacy_join), all(institution_fe == institution_fe_legacy_join)],
  d[enrolled == FALSE, all(is.na(institution_fe))])
d[, recovered_by_code := enrolled & is.na(institution_fe_legacy_join) & is.finite(institution_fe)]
crosswalk <- d[enrolled == TRUE, .(students = .N),
  by = .(COD_INST_m1, institution_key, matched_fe_key, institution_fe, recovered_by_code)]
fwrite(crosswalk, file.path(clean, "institution_join_audit.csv"))
# The user explicitly requested NA for non-enrollees. There is no zero-filled outcome.
d[, institution_fe_enrolled_m1 := fifelse(enrolled, institution_fe, NA_real_)]
d[, fe_source := fcase(!enrolled, "not_enrolled_missing", recovered_by_code, "observed_fe_recovered_by_code",
  is.finite(institution_fe), "observed_fe_legacy_key",
  default = "enrolled_missing_institution_fe")]
coverage <- d[, .(students = .N, schools = uniqueN(school_rbd)), by = .(cohort_gr8, fe_source)]
missing <- d[enrolled & !is.finite(institution_fe), .(students = .N),
  by = .(COD_INST_m1, TIPO_INST_1_m1, NOMB_INST_m1, institution_key)][order(-students)]
fwrite(coverage, file.path(clean, "fe_coverage.csv"))
fwrite(missing, file.path(clean, "unsupported_institutions.csv"))
fwrite(flow, file.path(clean, "sample_flow.csv"))
fwrite(inst, file.path(clean, "institution_effects.csv"))
saveRDS(d, file.path(clean, "analysis_data.rds"))
saveRDS(list(control_terms = va_env$control_terms, control_vars = va_env$control_vars,
  fixed_effect_vars = va_env$fixed_effect_vars, source_code = va_code, cohorts = 2017:2020,
  bootstrap_reps = 100L, seed = 20261110L, premium_column = "effect_log_clp_centered",
  premium_center = "unweighted mean across MiFuturo institutions", fe_model = "institution_plus_carrera_generica",
  institution_join = "unique official COD_INST; legacy nonmissing values verified identical",
  non_enrollees = "missing, as explicitly requested by user", unsupported_enrollees = "missing"),
  file.path(clean, "specification.rds"))
after <- file.info(paths)
stopifnot(identical(before$size, after$size), identical(before$mtime, after$mtime))
fwrite(data.table(path = normalizePath(paths, winslash = "/"), bytes = before$size,
  modified = as.character(before$mtime), md5 = c(NA_character_, unname(tools::md5sum(premium_path)),
    NA_character_, unname(tools::md5sum(va_code)))), file.path(clean, "input_manifest.csv"))
fwrite(data.table(check = c("person_ids_cohorts_programs_enrollment", "exact_person_fe_match", "binary_reconstruction", "source_integrity"),
  passed = TRUE), file.path(clean, "preparation_verification.csv"))
print(flow); print(coverage); print(missing)
