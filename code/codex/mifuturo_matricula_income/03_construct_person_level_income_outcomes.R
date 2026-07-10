###############################################################################
# Construct person-level MiFuturo income outcomes
#
# Rule:
# - Matriculated students receive MiFuturo model predictions under three
#   explicit rules: area-only FE, institution-only FE, and the full hierarchy.
#   The full hierarchy is the selected two-way FE model
#   log(income) ~ institution + AREA_CARRERA_GENERICA, then AREA_CARRERA_GENERICA
#   FE, institution FE, and global MiFuturo mean.
# - Students with no observed matriculation receive a configurable raw
#   wage-floor proxy. The default is the current conservative minimum-wage
#   proxy selected for the paper.
# - The non-matriculation floor is never used for matriculated students.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

# ------------------------- Configuration -------------------------

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
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

output_dir <- file.path(repo_wd, "output", "tables", "mifuturo_matricula_income")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

non_matriculated_floor_clp <- as.numeric(Sys.getenv("MIFUTURO_NON_MATRICULATED_FLOOR_CLP", "553553"))
non_matriculated_floor_label <- Sys.getenv(
  "MIFUTURO_NON_MATRICULATED_FLOOR_LABEL",
  "current_minimum_wage_proxy_553553"
)
non_matriculated_floor_note <- Sys.getenv(
  "MIFUTURO_NON_MATRICULATED_FLOOR_NOTE",
  "Raw 553,553 CLP proxy for non-matriculated students, using the current minimum-wage value selected as the conservative floor."
)

universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")
mat_dir <- if (dir.exists(file.path(data_wd, "data", "clean", "mat_ingresos_22-25"))) {
  file.path(data_wd, "data", "clean", "mat_ingresos_22-25")
} else {
  file.path(data_wd, "data", "clean", "mat_ingresos_22-24")
}
mat_first_path <- file.path(mat_dir, "mat_1st_ing.csv")
mat_last_path <- file.path(mat_dir, "mat_last_ing.csv")
program_info_path <- if (file.exists(file.path(data_wd, "data", "clean", "program_info_22-25.rds"))) {
  file.path(data_wd, "data", "clean", "program_info_22-25.rds")
} else {
  file.path(data_wd, "data", "clean", "program_info_22-24.rds")
}
model_artifact_path <- file.path(output_dir, "mifuturo_income_fe_model_artifact.rds")

person_outcome_path <- file.path(output_dir, "mifuturo_person_level_income_outcomes.csv")
stata_va_outcome_path <- file.path(output_dir, "mifuturo_person_level_income_outcomes_stata_va.csv")
source_summary_path <- file.path(output_dir, "mifuturo_person_level_income_source_summary.csv")
program_summary_path <- file.path(output_dir, "mifuturo_enrolled_program_income_summary.csv")
unsupported_programs_path <- file.path(output_dir, "mifuturo_enrolled_income_unsupported_programs.csv")
highpay_summary_path <- file.path(output_dir, "mifuturo_high_paying_field_source_summary.csv")
report_path <- file.path(output_dir, "mifuturo_person_level_income_report.md")

# ------------------------- Helpers -------------------------

required_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
  path
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "n/a", "s/i", "S/I")] <- NA_character_
  x
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

read_universe <- function(path) {
  message("Reading universe: ", path)
  header <- names(fread(required_file(path), nrows = 0))
  keep <- intersect(
    c(
      "MRUN", "mrun", "student_id", "cohort_gr8", "timely_sae",
      "registered_psu", "completed_psu",
      "field_reclassified_m1", "f_science_m1", "f_law_m1", "f_eng_m1",
      "field_reclassified_ml", "f_science_ml", "f_law_ml", "f_eng_ml"
    ),
    header
  )
  if (!"MRUN" %in% keep) {
    stop("Universe file must contain MRUN.", call. = FALSE)
  }

  dt <- fread(path, select = keep, na.strings = c("", "NA"))
  dt[, MRUN := as.character(MRUN)]

  if (anyDuplicated(dt$MRUN) > 0) {
    stop("Universe file is not one row per MRUN.", call. = FALSE)
  }

  dt[]
}

high_paying_medicine_plus_area_keys <- normalize_text(c(
  "Medicina",
  "Quimica y Farmacia",
  "Enfermeria",
  "Obstetricia y Puericultura",
  "Tecnologia Medica",
  "Odontologia"
))

derive_high_paying_field <- function(out, suffix) {
  matriculated_col <- paste0("matriculated", suffix)
  area_col <- paste0("AREA_CARRERA_GENERICA", suffix)
  field_col <- paste0("field_reclassified", suffix)
  science_col <- paste0("f_science", suffix)
  law_col <- paste0("f_law", suffix)
  eng_col <- paste0("f_eng", suffix)

  high_paying_col <- paste0("high_paying_field", suffix)
  high_paying_source_col <- paste0("high_paying_field_source", suffix)
  high_paying_missing_col <- paste0("high_paying_field_missing", suffix)

  for (col in c(field_col, science_col, law_col, eng_col)) {
    if (!col %in% names(out)) {
      out[, (col) := NA]
    }
  }

  area_key <- normalize_text(out[[area_col]])
  field <- as.character(out[[field_col]])
  is_science <- fifelse(is.na(out[[science_col]]), 0, as.integer(out[[science_col]])) == 1L
  is_law <- fifelse(is.na(out[[law_col]]), 0, as.integer(out[[law_col]])) == 1L
  is_eng <- fifelse(is.na(out[[eng_col]]), 0, as.integer(out[[eng_col]])) == 1L
  is_medicine_plus <- field == "Medicine" & area_key %chin% high_paying_medicine_plus_area_keys
  is_high_paying <- is_science | is_law | is_eng | is_medicine_plus
  is_classified_non_high <- !is.na(field) & nzchar(field) & !is_high_paying

  out[, (high_paying_col) := fcase(
    !get(matriculated_col), 0L,
    get(matriculated_col) & is_high_paying, 1L,
    get(matriculated_col) & is_classified_non_high, 0L,
    default = NA_integer_
  )]
  out[, (high_paying_source_col) := fcase(
    !get(matriculated_col), "not_matriculated_zero",
    get(matriculated_col) & (is_science | is_law | is_eng), "matriculated_high_paying_existing_field",
    get(matriculated_col) & is_medicine_plus, "matriculated_high_paying_medicine_plus_area",
    get(matriculated_col) & is_classified_non_high, "matriculated_classified_non_high_paying",
    default = "matriculated_missing_field_classification"
  )]
  out[, (high_paying_missing_col) := as.integer(is.na(get(high_paying_col)))]

  # Stata-safe alias for the VA/EB runner. The analytic outcome is only
  # 1/0/NA; source/missing diagnostics stay out of the analysis exports.
  out[, (paste0("highpay_field", suffix)) := get(high_paying_col)]

  out[]
}

read_program_info <- function(path) {
  message("Reading program info: ", path)
  dt <- as.data.table(readRDS(required_file(path)))
  required_cols <- c(
    "COD_SIES",
    "COD_CARRERA",
    "CODIGO_DEMRE",
    "NOMB_CARRERA",
    "TIPO_INST_1",
    "NOMB_INST",
    "COD_INST",
    "AREA_CARRERA_GENERICA",
    "year_info"
  )
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      basename(path), " is missing expected columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- dt[!is.na(COD_SIES)]
  dt[, COD_SIES := as.character(COD_SIES)]

  duplicates <- dt[, .N, by = .(COD_SIES)][N > 1]
  if (nrow(duplicates) > 0) {
    stop(
      basename(path), " has duplicated COD_SIES values. ",
      "Resolve before using it for a person-level outcome.",
      call. = FALSE
    )
  }

  dt[, .(
    COD_SIES,
    program_info_COD_CARRERA = COD_CARRERA,
    program_info_CODIGO_DEMRE = CODIGO_DEMRE,
    program_info_NOMB_CARRERA = NOMB_CARRERA,
    program_info_TIPO_INST_1 = TIPO_INST_1,
    program_info_NOMB_INST = NOMB_INST,
    program_info_COD_INST = COD_INST,
    program_info_AREA_CARRERA_GENERICA = AREA_CARRERA_GENERICA,
    program_info_year_info = year_info
  )]
}

read_clean_matricula <- function(path, suffix, enrollment_measure, program_info) {
  message("Reading clean matricula file: ", path)
  header <- names(fread(required_file(path), nrows = 0))
  suffix_cols <- paste0(
    c(
      "year_info", "COD_SIES", "COD_CARRERA", "CODIGO_DEMRE", "NOMB_CARRERA",
      "TIPO_INST_1", "NOMB_INST", "COD_INST", "AREA_CARRERA_GENERICA"
    ),
    suffix
  )
  select_cols <- intersect(c("MRUN", suffix_cols), header)
  required_cols <- paste0(
    c("year_info", "COD_SIES", "COD_CARRERA", "CODIGO_DEMRE", "NOMB_CARRERA", "TIPO_INST_1", "NOMB_INST", "COD_INST"),
    suffix
  )
  missing_cols <- setdiff(required_cols, header)
  if (length(missing_cols) > 0) {
    stop(
      "Clean matricula file is missing expected columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- fread(path, select = select_cols, na.strings = c("", "NA"))
  setnames(
    dt,
    old = setdiff(names(dt), "MRUN"),
    new = sub(paste0(suffix, "$"), "", setdiff(names(dt), "MRUN"))
  )

  dt[, `:=`(
    MRUN = as.character(MRUN),
    COD_SIES = as.character(COD_SIES),
    enrollment_measure = enrollment_measure
  )]

  if (anyDuplicated(dt$MRUN) > 0) {
    stop("Clean matricula file is not one row per MRUN: ", path, call. = FALSE)
  }

  if (!"AREA_CARRERA_GENERICA" %in% names(dt)) {
    dt[, AREA_CARRERA_GENERICA := NA_character_]
  }

  dt <- merge(dt, program_info, by = "COD_SIES", all.x = TRUE, sort = FALSE)
  dt[, program_info_found := !is.na(program_info_AREA_CARRERA_GENERICA)]
  dt[, area_source := fcase(
    !is.na(AREA_CARRERA_GENERICA), "matricula_clean",
    is.na(AREA_CARRERA_GENERICA) & !is.na(program_info_AREA_CARRERA_GENERICA), "program_info",
    default = NA_character_
  )]
  dt[is.na(AREA_CARRERA_GENERICA), AREA_CARRERA_GENERICA := program_info_AREA_CARRERA_GENERICA]

  dt[, `:=`(
    key_cod_inst = as.character(suppressWarnings(as.integer(COD_INST))),
    key_tipo_inst = normalize_text(TIPO_INST_1),
    key_nomb_inst = normalize_text(NOMB_INST),
    key_area_generica = normalize_text(AREA_CARRERA_GENERICA),
    institution_key = paste(
      as.character(suppressWarnings(as.integer(COD_INST))),
      normalize_text(TIPO_INST_1),
      normalize_text(NOMB_INST),
      sep = " || "
    ),
    carrera_generica_key = normalize_text(AREA_CARRERA_GENERICA)
  )]

  dt[]
}

predict_fe_income <- function(dt, model_artifact) {
  model <- model_artifact$model
  fallback_models <- model_artifact$fallback_models

  pred_dt <- copy(dt)
  pred_dt[, institution_fe := factor(institution_key, levels = model$xlevels$institution_fe)]
  pred_dt[, carrera_fe := factor(carrera_generica_key, levels = model$xlevels$carrera_fe)]
  level_supported <- !is.na(pred_dt$institution_fe) & !is.na(pred_dt$carrera_fe)

  pred_log_two_way <- rep(NA_real_, nrow(pred_dt))
  if (any(level_supported)) {
    pred_log_two_way[level_supported] <- as.numeric(stats::predict(
      model,
      newdata = pred_dt[level_supported],
      rankdeficient = "NA"
    ))
  }
  two_way_estimable <- level_supported & is.finite(pred_log_two_way)

  carrera_model <- fallback_models$carrera_model
  carrera_dt <- copy(dt)
  carrera_dt[, carrera_fe := factor(carrera_generica_key, levels = carrera_model$xlevels$carrera_fe)]
  carrera_supported <- !is.na(carrera_dt$carrera_fe)
  pred_log_carrera <- rep(NA_real_, nrow(carrera_dt))
  if (any(carrera_supported)) {
    pred_log_carrera[carrera_supported] <- as.numeric(stats::predict(
      carrera_model,
      newdata = carrera_dt[carrera_supported],
      rankdeficient = "NA"
    ))
  }
  carrera_estimable <- carrera_supported & is.finite(pred_log_carrera)

  institution_model <- fallback_models$institution_model
  institution_dt <- copy(dt)
  institution_dt[, institution_fe := factor(institution_key, levels = institution_model$xlevels$institution_fe)]
  institution_supported <- !is.na(institution_dt$institution_fe)
  pred_log_institution <- rep(NA_real_, nrow(institution_dt))
  if (any(institution_supported)) {
    pred_log_institution[institution_supported] <- as.numeric(stats::predict(
      institution_model,
      newdata = institution_dt[institution_supported],
      rankdeficient = "NA"
    ))
  }
  institution_estimable <- institution_supported & is.finite(pred_log_institution)

  global_log <- rep(fallback_models$global_log_mean, nrow(dt))
  global_smear <- rep(fallback_models$global_smear_factor, nrow(dt))
  global_income <- exp(global_log) * global_smear

  area_income_model_source <- fcase(
    carrera_estimable, "matriculated_area_generica_fe",
    carrera_supported & !carrera_estimable, "matriculated_area_generica_nonestimable_rank_deficient",
    !carrera_supported, "matriculated_area_generica_unsupported_levels",
    default = "matriculated_area_generica_unclassified"
  )

  institution_income_model_source <- fcase(
    institution_estimable, "matriculated_institution_fe",
    institution_supported & !institution_estimable, "matriculated_institution_nonestimable_rank_deficient",
    !institution_supported, "matriculated_institution_unsupported_levels",
    default = "matriculated_institution_unclassified"
  )

  hier_log <- rep(NA_real_, nrow(dt))
  hier_smear <- rep(NA_real_, nrow(dt))
  hier_tier <- rep(NA_integer_, nrow(dt))
  hier_source <- rep(NA_character_, nrow(dt))

  fill_two_way <- two_way_estimable
  hier_log[fill_two_way] <- pred_log_two_way[fill_two_way]
  hier_smear[fill_two_way] <- model_artifact$smear_factor
  hier_tier[fill_two_way] <- 1L
  hier_source[fill_two_way] <- "matriculated_two_way_fe_institution_area_generica"

  fill_carrera <- is.na(hier_log) & carrera_estimable
  hier_log[fill_carrera] <- pred_log_carrera[fill_carrera]
  hier_smear[fill_carrera] <- fallback_models$carrera_smear_factor
  hier_tier[fill_carrera] <- 2L
  hier_source[fill_carrera] <- "matriculated_fallback_area_generica_fe"

  fill_institution <- is.na(hier_log) & institution_estimable
  hier_log[fill_institution] <- pred_log_institution[fill_institution]
  hier_smear[fill_institution] <- fallback_models$institution_smear_factor
  hier_tier[fill_institution] <- 3L
  hier_source[fill_institution] <- "matriculated_fallback_institution_fe"

  fill_global <- is.na(hier_log)
  hier_log[fill_global] <- fallback_models$global_log_mean
  hier_smear[fill_global] <- fallback_models$global_smear_factor
  hier_tier[fill_global] <- 4L
  hier_source[fill_global] <- "matriculated_fallback_global_mifuturo_mean"

  dt[, `:=`(
    mifuturo_fe_level_supported = level_supported,
    mifuturo_fe_estimable = two_way_estimable,
    mifuturo_fe_log_income_hat_clp = pred_log_two_way,
    mifuturo_fe_income_hat_clp = exp(pred_log_two_way) * model_artifact$smear_factor,
    mifuturo_fe_income_hat_usd = exp(pred_log_two_way) * model_artifact$smear_factor / model_artifact$dollar_clp_conversion,
    mifuturo_fe_model_name = model_artifact$model_name,
    mifuturo_fe_income_source = fcase(
      two_way_estimable, "matriculated_fe_institution_area_generica",
      level_supported & !two_way_estimable, "matriculated_fe_nonestimable_rank_deficient",
      !level_supported, "matriculated_fe_unsupported_levels",
      default = "matriculated_fe_unclassified"
    ),
    mifuturo_area_level_supported = carrera_supported,
    mifuturo_area_estimable = carrera_estimable,
    mifuturo_area_log_income_hat_clp = pred_log_carrera,
    mifuturo_area_smear_factor = ifelse(carrera_estimable, fallback_models$carrera_smear_factor, NA_real_),
    mifuturo_area_income_hat_clp = exp(pred_log_carrera) * fallback_models$carrera_smear_factor,
    mifuturo_area_income_hat_usd = exp(pred_log_carrera) * fallback_models$carrera_smear_factor / model_artifact$dollar_clp_conversion,
    mifuturo_area_income_source = area_income_model_source,
    mifuturo_institution_level_supported = institution_supported,
    mifuturo_institution_estimable = institution_estimable,
    mifuturo_institution_log_income_hat_clp = pred_log_institution,
    mifuturo_institution_smear_factor = ifelse(institution_estimable, fallback_models$institution_smear_factor, NA_real_),
    mifuturo_institution_income_hat_clp = exp(pred_log_institution) * fallback_models$institution_smear_factor,
    mifuturo_institution_income_hat_usd = exp(pred_log_institution) * fallback_models$institution_smear_factor / model_artifact$dollar_clp_conversion,
    mifuturo_institution_income_source = institution_income_model_source,
    mifuturo_global_log_income_hat_clp = global_log,
    mifuturo_global_smear_factor = global_smear,
    mifuturo_global_income_hat_clp = global_income,
    mifuturo_global_income_hat_usd = global_income / model_artifact$dollar_clp_conversion,
    mifuturo_global_income_source = "matriculated_global_mifuturo_mean",
    mifuturo_hier_tier = hier_tier,
    mifuturo_hier_income_source = hier_source,
    mifuturo_hier_log_income_hat_clp = hier_log,
    mifuturo_hier_smear_factor = hier_smear,
    mifuturo_hier_income_hat_clp = exp(hier_log) * hier_smear,
    mifuturo_hier_income_hat_usd = exp(hier_log) * hier_smear / model_artifact$dollar_clp_conversion,
    mifuturo_full_tier = hier_tier,
    mifuturo_full_income_source = hier_source,
    mifuturo_full_log_income_hat_clp = hier_log,
    mifuturo_full_smear_factor = hier_smear,
    mifuturo_full_income_hat_clp = exp(hier_log) * hier_smear,
    mifuturo_full_income_hat_usd = exp(hier_log) * hier_smear / model_artifact$dollar_clp_conversion
  )]

  dt[]
}

suffix_columns <- function(dt, suffix, keep_unsuffixed = "MRUN") {
  old <- setdiff(names(dt), keep_unsuffixed)
  setnames(dt, old = old, new = paste0(old, suffix))
  dt[]
}

build_person_outcome <- function(universe, enrolled_dt, suffix, enrollment_measure) {
  keep_cols <- c(
    "MRUN",
    "year_info",
    "COD_SIES",
    "COD_CARRERA",
    "CODIGO_DEMRE",
    "NOMB_CARRERA",
    "TIPO_INST_1",
    "NOMB_INST",
    "COD_INST",
    "AREA_CARRERA_GENERICA",
    "program_info_found",
    "area_source",
    "mifuturo_fe_level_supported",
    "mifuturo_fe_estimable",
    "mifuturo_fe_log_income_hat_clp",
    "mifuturo_fe_income_hat_clp",
    "mifuturo_fe_income_hat_usd",
    "mifuturo_fe_model_name",
    "mifuturo_fe_income_source",
    "mifuturo_area_level_supported",
    "mifuturo_area_estimable",
    "mifuturo_area_log_income_hat_clp",
    "mifuturo_area_smear_factor",
    "mifuturo_area_income_hat_clp",
    "mifuturo_area_income_hat_usd",
    "mifuturo_area_income_source",
    "mifuturo_institution_level_supported",
    "mifuturo_institution_estimable",
    "mifuturo_institution_log_income_hat_clp",
    "mifuturo_institution_smear_factor",
    "mifuturo_institution_income_hat_clp",
    "mifuturo_institution_income_hat_usd",
    "mifuturo_institution_income_source",
    "mifuturo_global_log_income_hat_clp",
    "mifuturo_global_smear_factor",
    "mifuturo_global_income_hat_clp",
    "mifuturo_global_income_hat_usd",
    "mifuturo_global_income_source",
    "mifuturo_hier_tier",
    "mifuturo_hier_income_source",
    "mifuturo_hier_log_income_hat_clp",
    "mifuturo_hier_smear_factor",
    "mifuturo_hier_income_hat_clp",
    "mifuturo_hier_income_hat_usd",
    "mifuturo_full_tier",
    "mifuturo_full_income_source",
    "mifuturo_full_log_income_hat_clp",
    "mifuturo_full_smear_factor",
    "mifuturo_full_income_hat_clp",
    "mifuturo_full_income_hat_usd"
  )

  enrolled_small <- copy(enrolled_dt[, ..keep_cols])
  suffix_columns(enrolled_small, suffix)

  out <- merge(universe, enrolled_small, by = "MRUN", all.x = TRUE, sort = FALSE)

  cod_sies_col <- paste0("COD_SIES", suffix)
  fe_income_col <- paste0("mifuturo_fe_income_hat_clp", suffix)
  fe_log_col <- paste0("mifuturo_fe_log_income_hat_clp", suffix)
  fe_estimable_col <- paste0("mifuturo_fe_estimable", suffix)
  source_col <- paste0("mifuturo_fe_income_source", suffix)

  matriculated_col <- paste0("matriculated", suffix)
  analysis_income_col <- paste0("mifuturo_income_fe_or_minwage_clp", suffix)
  analysis_log_col <- paste0("log_mifuturo_income_fe_or_minwage_clp", suffix)
  analysis_source_col <- paste0("mifuturo_income_fe_or_minwage_source", suffix)
  missing_col <- paste0("mifuturo_income_missing_after_fe_or_minwage", suffix)
  hier_income_col <- paste0("mifuturo_hier_income_hat_clp", suffix)
  hier_source_col <- paste0("mifuturo_hier_income_source", suffix)
  area_income_col <- paste0("mifuturo_area_income_hat_clp", suffix)
  area_estimable_col <- paste0("mifuturo_area_estimable", suffix)
  area_source_col <- paste0("mifuturo_area_income_source", suffix)
  institution_income_col <- paste0("mifuturo_institution_income_hat_clp", suffix)
  institution_estimable_col <- paste0("mifuturo_institution_estimable", suffix)
  institution_source_col <- paste0("mifuturo_institution_income_source", suffix)
  global_income_col <- paste0("mifuturo_global_income_hat_clp", suffix)
  complete_income_col <- paste0("mifuturo_income_hier_or_minwage_clp", suffix)
  complete_log_col <- paste0("log_mifuturo_income_hier_or_minwage_clp", suffix)
  complete_source_col <- paste0("mifuturo_income_hier_or_minwage_source", suffix)
  complete_missing_col <- paste0("mifuturo_income_missing_after_hier_or_minwage", suffix)
  program_income_area_col <- paste0("program_income_area_clp", suffix)
  log_program_income_area_col <- paste0("log_program_income_area_clp", suffix)
  program_income_area_source_col <- paste0("program_income_area_source", suffix)
  program_income_area_missing_col <- paste0("program_income_area_missing", suffix)
  program_income_institution_col <- paste0("program_income_institution_clp", suffix)
  log_program_income_institution_col <- paste0("log_program_income_institution_clp", suffix)
  program_income_institution_source_col <- paste0("program_income_institution_source", suffix)
  program_income_institution_missing_col <- paste0("program_income_institution_missing", suffix)
  program_income_full_col <- paste0("program_income_full_clp", suffix)
  log_program_income_full_col <- paste0("log_program_income_full_clp", suffix)
  program_income_full_source_col <- paste0("program_income_full_source", suffix)
  program_income_full_missing_col <- paste0("program_income_full_missing", suffix)
  program_income_col <- paste0("program_income_clp", suffix)
  log_program_income_col <- paste0("log_program_income_clp", suffix)
  program_income_source_col <- paste0("program_income_source", suffix)
  program_income_missing_col <- paste0("program_income_missing", suffix)
  floor_col <- paste0("mifuturo_non_matriculated_floor_clp", suffix)
  floor_label_col <- paste0("mifuturo_non_matriculated_floor_label", suffix)

  out[, (matriculated_col) := !is.na(get(cod_sies_col))]
  out <- derive_high_paying_field(out, suffix)
  out[, (floor_col) := non_matriculated_floor_clp]
  out[, (floor_label_col) := non_matriculated_floor_label]

  out[, (analysis_income_col) := fcase(
    get(matriculated_col) & get(fe_estimable_col) == TRUE, get(fe_income_col),
    !get(matriculated_col), non_matriculated_floor_clp,
    default = NA_real_
  )]

  out[, (analysis_log_col) := log(get(analysis_income_col))]
  out[, (analysis_source_col) := fcase(
    get(matriculated_col) & get(fe_estimable_col) == TRUE, get(source_col),
    !get(matriculated_col), "not_matriculated_minimum_wage_floor",
    get(matriculated_col) & !(get(fe_estimable_col) %in% TRUE), "matriculated_missing_fe_income",
    default = "unclassified"
  )]
  out[, (missing_col) := as.integer(is.na(get(analysis_income_col)))]

  out[, (complete_income_col) := fcase(
    get(matriculated_col) & !is.na(get(hier_income_col)), get(hier_income_col),
    !get(matriculated_col), non_matriculated_floor_clp,
    default = NA_real_
  )]
  out[, (complete_log_col) := log(get(complete_income_col))]
  out[, (complete_source_col) := fcase(
    get(matriculated_col) & !is.na(get(hier_income_col)), get(hier_source_col),
    !get(matriculated_col), "not_matriculated_minimum_wage_floor",
    get(matriculated_col) & is.na(get(hier_income_col)), "matriculated_missing_hier_income",
    default = "unclassified"
  )]
  out[, (complete_missing_col) := as.integer(is.na(get(complete_income_col)))]

  out[, (program_income_area_col) := fcase(
    get(matriculated_col) & get(area_estimable_col) %in% TRUE, get(area_income_col),
    get(matriculated_col) & !(get(area_estimable_col) %in% TRUE) & !is.na(get(global_income_col)), get(global_income_col),
    !get(matriculated_col), non_matriculated_floor_clp,
    default = NA_real_
  )]
  out[, (log_program_income_area_col) := log(get(program_income_area_col))]
  out[, (program_income_area_source_col) := fcase(
    get(matriculated_col) & get(area_estimable_col) %in% TRUE, get(area_source_col),
    get(matriculated_col) & !(get(area_estimable_col) %in% TRUE) & !is.na(get(global_income_col)), "matriculated_area_generica_global_mifuturo_mean",
    !get(matriculated_col), "not_matriculated_minimum_wage_floor",
    get(matriculated_col) & is.na(get(program_income_area_col)), "matriculated_area_generica_missing_income",
    default = "unclassified"
  )]
  out[, (program_income_area_missing_col) := as.integer(is.na(get(program_income_area_col)))]

  out[, (program_income_institution_col) := fcase(
    get(matriculated_col) & get(institution_estimable_col) %in% TRUE, get(institution_income_col),
    get(matriculated_col) & !(get(institution_estimable_col) %in% TRUE) & !is.na(get(global_income_col)), get(global_income_col),
    !get(matriculated_col), non_matriculated_floor_clp,
    default = NA_real_
  )]
  out[, (log_program_income_institution_col) := log(get(program_income_institution_col))]
  out[, (program_income_institution_source_col) := fcase(
    get(matriculated_col) & get(institution_estimable_col) %in% TRUE, get(institution_source_col),
    get(matriculated_col) & !(get(institution_estimable_col) %in% TRUE) & !is.na(get(global_income_col)), "matriculated_institution_global_mifuturo_mean",
    !get(matriculated_col), "not_matriculated_minimum_wage_floor",
    get(matriculated_col) & is.na(get(program_income_institution_col)), "matriculated_institution_missing_income",
    default = "unclassified"
  )]
  out[, (program_income_institution_missing_col) := as.integer(is.na(get(program_income_institution_col)))]

  out[, (program_income_full_col) := get(complete_income_col)]
  out[, (log_program_income_full_col) := get(complete_log_col)]
  out[, (program_income_full_source_col) := get(complete_source_col)]
  out[, (program_income_full_missing_col) := get(complete_missing_col)]

  # Backward-compatible alias for existing VA/Stata scripts. Canonical name is
  # program_income_full.
  out[, (program_income_col) := get(program_income_full_col)]
  out[, (log_program_income_col) := get(log_program_income_full_col)]
  out[, (program_income_source_col) := get(program_income_full_source_col)]
  out[, (program_income_missing_col) := get(program_income_full_missing_col)]

  # Stata variable names are capped at 32 characters, so keep short aliases for
  # the VA/EB Stata runners while preserving the canonical research names above.
  out[, (paste0("proginc_area_clp", suffix)) := get(program_income_area_col)]
  out[, (paste0("log_proginc_area_clp", suffix)) := get(log_program_income_area_col)]
  out[, (paste0("proginc_area_src", suffix)) := get(program_income_area_source_col)]
  out[, (paste0("proginc_area_miss", suffix)) := get(program_income_area_missing_col)]

  out[, (paste0("proginc_inst_clp", suffix)) := get(program_income_institution_col)]
  out[, (paste0("log_proginc_inst_clp", suffix)) := get(log_program_income_institution_col)]
  out[, (paste0("proginc_inst_src", suffix)) := get(program_income_institution_source_col)]
  out[, (paste0("proginc_inst_miss", suffix)) := get(program_income_institution_missing_col)]

  out[, (paste0("proginc_full_clp", suffix)) := get(program_income_full_col)]
  out[, (paste0("log_proginc_full_clp", suffix)) := get(log_program_income_full_col)]
  out[, (paste0("proginc_full_src", suffix)) := get(program_income_full_source_col)]
  out[, (paste0("proginc_full_miss", suffix)) := get(program_income_full_missing_col)]

  out[]
}

summarize_person_outcome <- function(person, suffix, enrollment_measure) {
  enrollment_measure_label <- enrollment_measure
  matriculated_col <- paste0("matriculated", suffix)
  fe_estimable_col <- paste0("mifuturo_fe_estimable", suffix)

  summarize_variant <- function(outcome_variant, income_col, source_col, estimable_col = NULL, tier_col = NULL) {
    if (!is.null(estimable_col)) {
      variant_estimable <- person[[estimable_col]] %in% TRUE
    } else if (!is.null(tier_col)) {
      variant_estimable <- person[[tier_col]] %in% c(1L, 2L, 3L)
    } else {
      variant_estimable <- rep(NA, nrow(person))
    }

    dt <- data.table(
      income = person[[income_col]],
      income_source = person[[source_col]],
      matriculated = person[[matriculated_col]],
      strict_two_way_estimable = person[[fe_estimable_col]] %in% TRUE,
      variant_estimable = variant_estimable
    )

    ans <- dt[, .(
      enrollment_measure = enrollment_measure_label,
      n_students = .N,
      n_matriculated = sum(matriculated, na.rm = TRUE),
      n_not_matriculated = sum(!matriculated, na.rm = TRUE),
      n_strict_two_way_fe_matriculated = sum(matriculated & strict_two_way_estimable, na.rm = TRUE),
      n_model_estimable_matriculated = if (all(is.na(variant_estimable))) {
        NA_integer_
      } else {
        sum(matriculated & variant_estimable, na.rm = TRUE)
      },
      n_missing_after_rule = sum(is.na(income)),
      share_missing_after_rule = mean(is.na(income)),
      mean_income_clp = safe_mean(income),
      median_income_clp = safe_median(income)
    ), by = .(income_source)]
    variant_name <- outcome_variant
    ans[, outcome_variant := variant_name]
    setcolorder(ans, c("enrollment_measure", "outcome_variant", "income_source"))
    ans[]
  }

  rbindlist(
    list(
      summarize_variant(
        "strict_two_way_fe_or_minwage",
        paste0("mifuturo_income_fe_or_minwage_clp", suffix),
        paste0("mifuturo_income_fe_or_minwage_source", suffix),
        estimable_col = fe_estimable_col
      ),
      summarize_variant(
        "program_income_area",
        paste0("program_income_area_clp", suffix),
        paste0("program_income_area_source", suffix),
        estimable_col = paste0("mifuturo_area_estimable", suffix)
      ),
      summarize_variant(
        "program_income_institution",
        paste0("program_income_institution_clp", suffix),
        paste0("program_income_institution_source", suffix),
        estimable_col = paste0("mifuturo_institution_estimable", suffix)
      ),
      summarize_variant(
        "program_income_full",
        paste0("program_income_full_clp", suffix),
        paste0("program_income_full_source", suffix),
        tier_col = paste0("mifuturo_full_tier", suffix)
      )
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

summarize_enrolled_programs <- function(enrolled_dt, enrollment_measure) {
  enrollment_measure_label <- enrollment_measure
  enrolled_dt[, .(
    enrollment_measure = enrollment_measure_label,
    n_enrolled_students = .N,
    n_unique_cod_sies = uniqueN(COD_SIES),
    n_unique_inst_area = uniqueN(paste(COD_INST, AREA_CARRERA_GENERICA, sep = " || ")),
    n_program_info_found = sum(program_info_found, na.rm = TRUE),
    n_fe_level_supported = sum(mifuturo_fe_level_supported, na.rm = TRUE),
    n_fe_estimable = sum(mifuturo_fe_estimable, na.rm = TRUE),
    share_fe_estimable = mean(mifuturo_fe_estimable, na.rm = TRUE),
    n_area_level_supported = sum(mifuturo_area_level_supported, na.rm = TRUE),
    n_area_estimable = sum(mifuturo_area_estimable, na.rm = TRUE),
    share_area_estimable = mean(mifuturo_area_estimable, na.rm = TRUE),
    n_institution_level_supported = sum(mifuturo_institution_level_supported, na.rm = TRUE),
    n_institution_estimable = sum(mifuturo_institution_estimable, na.rm = TRUE),
    share_institution_estimable = mean(mifuturo_institution_estimable, na.rm = TRUE),
    n_hier_two_way_fe = sum(mifuturo_hier_tier == 1L, na.rm = TRUE),
    n_hier_area_generica_fe = sum(mifuturo_hier_tier == 2L, na.rm = TRUE),
    n_hier_institution_fe = sum(mifuturo_hier_tier == 3L, na.rm = TRUE),
    n_hier_global_mean = sum(mifuturo_hier_tier == 4L, na.rm = TRUE),
    n_hier_complete = sum(!is.na(mifuturo_hier_income_hat_clp)),
    share_hier_complete = mean(!is.na(mifuturo_hier_income_hat_clp)),
    mean_fe_income_clp = safe_mean(mifuturo_fe_income_hat_clp),
    median_fe_income_clp = safe_median(mifuturo_fe_income_hat_clp),
    mean_area_income_clp = safe_mean(mifuturo_area_income_hat_clp),
    median_area_income_clp = safe_median(mifuturo_area_income_hat_clp),
    mean_institution_income_clp = safe_mean(mifuturo_institution_income_hat_clp),
    median_institution_income_clp = safe_median(mifuturo_institution_income_hat_clp),
    mean_hier_income_clp = safe_mean(mifuturo_hier_income_hat_clp),
    median_hier_income_clp = safe_median(mifuturo_hier_income_hat_clp)
  )]
}

unsupported_programs <- function(enrolled_dt, suffix, enrollment_measure, universe_mruns) {
  enrollment_measure_label <- enrollment_measure
  dt <- enrolled_dt[MRUN %in% universe_mruns & !(mifuturo_fe_estimable %in% TRUE)]
  if (nrow(dt) == 0) {
    return(data.table())
  }

  dt[, .(
    enrollment_measure = enrollment_measure_label,
    n_students = .N,
    example_mruns = paste(head(MRUN, 10), collapse = " | "),
    year_info = year_info[which(!is.na(year_info))[1]],
    TIPO_INST_1 = TIPO_INST_1[which(!is.na(TIPO_INST_1))[1]],
    NOMB_INST = NOMB_INST[which(!is.na(NOMB_INST))[1]],
    COD_INST = COD_INST[which(!is.na(COD_INST))[1]],
    AREA_CARRERA_GENERICA = AREA_CARRERA_GENERICA[which(!is.na(AREA_CARRERA_GENERICA))[1]],
    NOMB_CARRERA_examples = paste(head(unique(na.omit(NOMB_CARRERA)), 8), collapse = " | "),
    program_info_found = any(program_info_found, na.rm = TRUE),
    mifuturo_fe_level_supported = any(mifuturo_fe_level_supported, na.rm = TRUE),
    mifuturo_area_level_supported = any(mifuturo_area_level_supported, na.rm = TRUE),
    mifuturo_area_estimable = any(mifuturo_area_estimable, na.rm = TRUE),
    mifuturo_institution_level_supported = any(mifuturo_institution_level_supported, na.rm = TRUE),
    mifuturo_institution_estimable = any(mifuturo_institution_estimable, na.rm = TRUE),
    mifuturo_fe_income_source = mifuturo_fe_income_source[1]
  ), by = .(COD_SIES)][order(-n_students)]
}

# ------------------------- Run -------------------------

message("Using non-matriculated floor: ", non_matriculated_floor_clp, " CLP (", non_matriculated_floor_label, ")")

model_artifact <- readRDS(required_file(model_artifact_path))
universe <- read_universe(universe_path)
program_info <- read_program_info(program_info_path)

mat_first <- read_clean_matricula(mat_first_path, "_m1", "first_enrollment", program_info)
mat_last <- read_clean_matricula(mat_last_path, "_ml", "last_enrollment", program_info)

mat_first <- predict_fe_income(mat_first, model_artifact)
mat_last <- predict_fe_income(mat_last, model_artifact)

person_m1 <- build_person_outcome(universe, mat_first, "_m1", "first_enrollment")
person_ml <- build_person_outcome(universe, mat_last, "_ml", "last_enrollment")

ml_cols <- setdiff(names(person_ml), names(universe))
person_outcome <- merge(
  person_m1,
  person_ml[, c("MRUN", ml_cols), with = FALSE],
  by = "MRUN",
  all.x = TRUE,
  sort = FALSE
)

source_summary <- rbindlist(
  list(
    summarize_person_outcome(person_outcome, "_m1", "first_enrollment"),
    summarize_person_outcome(person_outcome, "_ml", "last_enrollment")
  ),
  use.names = TRUE,
  fill = TRUE
)

program_summary <- rbindlist(
  list(
    summarize_enrolled_programs(mat_first[MRUN %in% universe$MRUN], "first_enrollment"),
    summarize_enrolled_programs(mat_last[MRUN %in% universe$MRUN], "last_enrollment")
  ),
  use.names = TRUE,
  fill = TRUE
)

unsupported <- rbindlist(
  list(
    unsupported_programs(mat_first, "_m1", "first_enrollment", universe$MRUN),
    unsupported_programs(mat_last, "_ml", "last_enrollment", universe$MRUN)
  ),
  use.names = TRUE,
  fill = TRUE
)

highpay_summary <- person_outcome[
  ,
  .(
    n_students = .N,
    n_high_paying_field = sum(high_paying_field_m1 == 1L, na.rm = TRUE),
    n_non_high_paying_field = sum(high_paying_field_m1 == 0L, na.rm = TRUE),
    n_missing_high_paying_field = sum(is.na(high_paying_field_m1)),
    share_high_paying_field = mean(high_paying_field_m1 == 1L, na.rm = TRUE)
  ),
  by = .(high_paying_field_source_m1)
][order(-n_students)]

highpay_aux_cols <- grep(
  "^(high_paying_field_source|high_paying_field_missing)",
  names(person_outcome),
  value = TRUE
)
person_outcome_export <- copy(person_outcome)
if (length(highpay_aux_cols) > 0L) {
  person_outcome_export[, (highpay_aux_cols) := NULL]
}

fwrite(person_outcome_export, person_outcome_path)
stata_va_cols <- c(
  "MRUN",
  "proginc_area_clp_m1",
  "log_proginc_area_clp_m1",
  "proginc_area_src_m1",
  "proginc_area_miss_m1",
  "proginc_inst_clp_m1",
  "log_proginc_inst_clp_m1",
  "proginc_inst_src_m1",
  "proginc_inst_miss_m1",
  "proginc_full_clp_m1",
  "log_proginc_full_clp_m1",
  "proginc_full_src_m1",
  "proginc_full_miss_m1",
  "program_income_clp_m1",
  "log_program_income_clp_m1",
  "program_income_source_m1",
  "program_income_missing_m1",
  "highpay_field_m1"
)
fwrite(person_outcome_export[, ..stata_va_cols], stata_va_outcome_path)
fwrite(source_summary, source_summary_path)
fwrite(program_summary, program_summary_path)
fwrite(unsupported, unsupported_programs_path)
fwrite(highpay_summary, highpay_summary_path)

report <- c(
  "# MiFuturo Person-Level Income Outcomes",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Rule",
  "",
  "Canonical working outcome names: `program_income_area`, `program_income_institution`, and `program_income_full`.",
  "",
  "- `program_income_area`: matriculated students use the one-way `AREA_CARRERA_GENERICA` FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.",
  "- `program_income_institution`: matriculated students use the one-way institution FE prediction; non-estimable matriculated cases use the global MiFuturo mean with an explicit source flag.",
  "- `program_income_full`: matriculated students use the selected hierarchy: two-way institution + `AREA_CARRERA_GENERICA` FE, then area FE, then institution FE, then global MiFuturo mean.",
  "- Students with no observed matriculation receive the configured non-matriculation floor.",
  "- The non-matriculation floor is not used for matriculated students.",
  "- Backward-compatible `program_income` columns are retained as aliases to `program_income_full` for existing VA/Stata scripts.",
  "- `high_paying_field_m1`: non-matriculated students are coded 0; matriculated students are coded 1 for Science, Law, Engineering/Manufacturing/Construction, or Medicine+ (`Medicina`, `Quimica y Farmacia`, `Enfermeria`, `Obstetricia y Puericultura`, `Tecnologia Medica`, `Odontologia`). Matriculated students with insufficient field classification remain missing rather than being silently coded 0.",
  "",
  "## Non-Matriculation Floor",
  "",
  paste0("- CLP: ", non_matriculated_floor_clp),
  paste0("- Label: `", non_matriculated_floor_label, "`"),
  paste0("- Note: ", non_matriculated_floor_note),
  "",
  "## Person-Level Source Summary",
  "",
  paste(capture.output(print(source_summary)), collapse = "\n"),
  "",
  "## Enrolled Program FE Coverage",
  "",
  paste(capture.output(print(program_summary)), collapse = "\n"),
  "",
  "## High-Paying Field Coverage",
  "",
  paste(capture.output(print(highpay_summary)), collapse = "\n"),
  "",
  "## Outputs",
  "",
  paste0("- `", person_outcome_path, "`"),
  paste0("- `", stata_va_outcome_path, "`"),
  paste0("- `", source_summary_path, "`"),
  paste0("- `", program_summary_path, "`"),
  paste0("- `", unsupported_programs_path, "`"),
  paste0("- `", highpay_summary_path, "`")
)

writeLines(report, report_path)

message("Done.")
print(source_summary)
print(program_summary)
print(highpay_summary)
