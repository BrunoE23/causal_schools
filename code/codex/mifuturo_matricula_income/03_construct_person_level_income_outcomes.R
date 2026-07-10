###############################################################################
# Construct person-level MiFuturo income outcomes
#
# Rule:
# - Matriculated students receive a MiFuturo model prediction. The preferred
#   prediction is the selected two-way FE model:
#   log(income) ~ institution + AREA_CARRERA_GENERICA.
#   Programs outside two-way support are filled by an explicit hierarchy:
#   AREA_CARRERA_GENERICA FE, institution FE, then global MiFuturo mean.
# - Students with no observed matriculation receive a configurable raw
#   wage-floor proxy aligned with the approximate wage years behind MiFuturo.
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

non_matriculated_floor_clp <- as.numeric(Sys.getenv("MIFUTURO_NON_MATRICULATED_FLOOR_CLP", "350000"))
non_matriculated_floor_label <- Sys.getenv(
  "MIFUTURO_NON_MATRICULATED_FLOOR_LABEL",
  "raw_minimum_wage_proxy_2020_2023"
)
non_matriculated_floor_note <- Sys.getenv(
  "MIFUTURO_NON_MATRICULATED_FLOOR_NOTE",
  "Raw 350,000 CLP proxy for non-matriculated students, chosen to align with likely 2020-2023 wage years underlying MiFuturo income measures."
)

universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")
mat_first_path <- file.path(data_wd, "data", "clean", "mat_ingresos_22-24", "mat_1st_ing.csv")
mat_last_path <- file.path(data_wd, "data", "clean", "mat_ingresos_22-24", "mat_last_ing.csv")
program_info_path <- file.path(data_wd, "data", "clean", "program_info_22-24.rds")
model_artifact_path <- file.path(output_dir, "mifuturo_income_fe_model_artifact.rds")

person_outcome_path <- file.path(output_dir, "mifuturo_person_level_income_outcomes.csv")
source_summary_path <- file.path(output_dir, "mifuturo_person_level_income_source_summary.csv")
program_summary_path <- file.path(output_dir, "mifuturo_enrolled_program_income_summary.csv")
unsupported_programs_path <- file.path(output_dir, "mifuturo_enrolled_income_unsupported_programs.csv")
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
    c("MRUN", "mrun", "student_id", "cohort_gr8", "timely_sae", "registered_psu", "completed_psu"),
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
      "program_info_22-24.rds is missing expected columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- dt[!is.na(COD_SIES)]
  dt[, COD_SIES := as.character(COD_SIES)]

  duplicates <- dt[, .N, by = .(COD_SIES)][N > 1]
  if (nrow(duplicates) > 0) {
    stop(
      "program_info_22-24.rds has duplicated COD_SIES values. ",
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
    is.na(AREA_CARRERA_GENERICA) & !is.na(program_info_AREA_CARRERA_GENERICA), "program_info_22_24",
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
    mifuturo_hier_tier = hier_tier,
    mifuturo_hier_income_source = hier_source,
    mifuturo_hier_log_income_hat_clp = hier_log,
    mifuturo_hier_smear_factor = hier_smear,
    mifuturo_hier_income_hat_clp = exp(hier_log) * hier_smear,
    mifuturo_hier_income_hat_usd = exp(hier_log) * hier_smear / model_artifact$dollar_clp_conversion
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
    "mifuturo_hier_tier",
    "mifuturo_hier_income_source",
    "mifuturo_hier_log_income_hat_clp",
    "mifuturo_hier_smear_factor",
    "mifuturo_hier_income_hat_clp",
    "mifuturo_hier_income_hat_usd"
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
  complete_income_col <- paste0("mifuturo_income_hier_or_minwage_clp", suffix)
  complete_log_col <- paste0("log_mifuturo_income_hier_or_minwage_clp", suffix)
  complete_source_col <- paste0("mifuturo_income_hier_or_minwage_source", suffix)
  complete_missing_col <- paste0("mifuturo_income_missing_after_hier_or_minwage", suffix)
  program_income_col <- paste0("program_income_clp", suffix)
  log_program_income_col <- paste0("log_program_income_clp", suffix)
  program_income_source_col <- paste0("program_income_source", suffix)
  program_income_missing_col <- paste0("program_income_missing", suffix)
  floor_col <- paste0("mifuturo_non_matriculated_floor_clp", suffix)
  floor_label_col <- paste0("mifuturo_non_matriculated_floor_label", suffix)

  out[, (matriculated_col) := !is.na(get(cod_sies_col))]
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
  out[, (program_income_col) := get(complete_income_col)]
  out[, (log_program_income_col) := get(complete_log_col)]
  out[, (program_income_source_col) := get(complete_source_col)]
  out[, (program_income_missing_col) := get(complete_missing_col)]

  out[]
}

summarize_person_outcome <- function(person, suffix, enrollment_measure) {
  enrollment_measure_label <- enrollment_measure
  matriculated_col <- paste0("matriculated", suffix)
  analysis_income_col <- paste0("program_income_clp", suffix)
  analysis_source_col <- paste0("program_income_source", suffix)
  fe_estimable_col <- paste0("mifuturo_fe_estimable", suffix)

  person[, .(
    enrollment_measure = enrollment_measure_label,
    n_students = .N,
    n_matriculated = sum(get(matriculated_col), na.rm = TRUE),
    n_not_matriculated = sum(!get(matriculated_col), na.rm = TRUE),
    n_strict_two_way_fe_matriculated = sum(get(matriculated_col) & get(fe_estimable_col) == TRUE, na.rm = TRUE),
    n_missing_after_rule = sum(is.na(get(analysis_income_col))),
    share_missing_after_rule = mean(is.na(get(analysis_income_col))),
    mean_income_clp = safe_mean(get(analysis_income_col)),
    median_income_clp = safe_median(get(analysis_income_col))
  ), by = .(income_source = get(analysis_source_col))]
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
    n_hier_two_way_fe = sum(mifuturo_hier_tier == 1L, na.rm = TRUE),
    n_hier_area_generica_fe = sum(mifuturo_hier_tier == 2L, na.rm = TRUE),
    n_hier_institution_fe = sum(mifuturo_hier_tier == 3L, na.rm = TRUE),
    n_hier_global_mean = sum(mifuturo_hier_tier == 4L, na.rm = TRUE),
    n_hier_complete = sum(!is.na(mifuturo_hier_income_hat_clp)),
    share_hier_complete = mean(!is.na(mifuturo_hier_income_hat_clp)),
    mean_fe_income_clp = safe_mean(mifuturo_fe_income_hat_clp),
    median_fe_income_clp = safe_median(mifuturo_fe_income_hat_clp),
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

fwrite(person_outcome, person_outcome_path)
fwrite(source_summary, source_summary_path)
fwrite(program_summary, program_summary_path)
fwrite(unsupported, unsupported_programs_path)

report <- c(
  "# MiFuturo Person-Level Income Outcomes",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Rule",
  "",
  "Working outcome name: `program_income`.",
  "",
  "- Matriculated students use a MiFuturo model prediction.",
  "- The preferred tier is the selected two-way FE prediction from `log(income) ~ institution + AREA_CARRERA_GENERICA`.",
  "- Matriculated programs outside two-way FE support are filled by explicit fallback tiers: `AREA_CARRERA_GENERICA` FE, institution FE, then global MiFuturo mean.",
  "- Students with no observed matriculation receive the configured non-matriculation floor.",
  "- The non-matriculation floor is not used for matriculated students.",
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
  "## Outputs",
  "",
  paste0("- `", person_outcome_path, "`"),
  paste0("- `", source_summary_path, "`"),
  paste0("- `", program_summary_path, "`"),
  paste0("- `", unsupported_programs_path, "`")
)

writeLines(report, report_path)

message("Done.")
print(source_summary)
print(program_summary)
