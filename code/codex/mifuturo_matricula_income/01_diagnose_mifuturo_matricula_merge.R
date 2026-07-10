###############################################################################
# Diagnose MiFuturo income merge onto matriculated programs
#
# This script does not modify the official matricula or universe files. It
# builds candidate joins from MiFuturo income data to first/last matriculated
# programs and writes diagnostics on coverage, unmatched programs, and key
# multiplicity.
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

dollar_clp_conversion <- 913

clean_dir <- file.path(data_wd, "data", "clean")
raw_mifuturo_dir <- file.path(data_wd, "data", "raw", "mifuturo")
mat_dir <- file.path(clean_dir, "mat_ingresos_22-24")
output_dir <- file.path(repo_wd, "output", "tables", "mifuturo_matricula_income")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

mifuturo_csv <- file.path(
  raw_mifuturo_dir,
  "Buscador_Empleabilidad_ingresos_2025_2026_SIES(Carreras e IES (2025-2026)).csv"
)
program_info_path <- file.path(clean_dir, "program_info_22-24.rds")
mat_first_path <- file.path(mat_dir, "mat_1st_ing.csv")
mat_last_path <- file.path(mat_dir, "mat_last_ing.csv")

summary_path <- file.path(output_dir, "mifuturo_matricula_match_summary.csv")
unmatched_path <- file.path(output_dir, "mifuturo_matricula_unmatched_programs.csv")
key_multiplicity_path <- file.path(output_dir, "mifuturo_matricula_key_multiplicity.csv")
candidate_program_path <- file.path(output_dir, "mifuturo_matricula_candidate_program_income.csv")
source_map_path <- file.path(output_dir, "mifuturo_matricula_code_map.csv")
report_path <- file.path(output_dir, "mifuturo_matricula_match_report.md")

# ------------------------- Helpers -------------------------

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalize_col_name <- function(x) {
  out <- normalize_text(x)
  out <- tolower(gsub(" ", "_", out))
  out <- gsub("^_|_$", "", out)
  out[nchar(out) == 0] <- paste0("unnamed_", seq_len(sum(nchar(out) == 0)))
  out
}

blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "n/a", "s/i", "S/I")] <- NA_character_
  x
}

parse_percent <- function(x) {
  x <- blank_to_na(x)
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  out <- suppressWarnings(as.numeric(x))
  out / 100
}

extract_amounts_clp <- function(s) {
  if (is.na(s)) {
    return(numeric())
  }

  s <- normalize_text(gsub("\\$", "", s))
  pattern <- "(\\d+) MILLON(?:ES)?(?: (\\d+) MIL)?|(\\d+) MIL"
  hits <- gregexpr(pattern, s, perl = TRUE)
  pieces <- regmatches(s, hits)[[1]]

  if (length(pieces) == 0 || identical(pieces, character(0))) {
    return(numeric())
  }

  vapply(pieces, function(piece) {
    m <- regexec(pattern, piece, perl = TRUE)
    parts <- regmatches(piece, m)[[1]]
    million <- suppressWarnings(as.numeric(parts[2]))
    thousand_after_million <- suppressWarnings(as.numeric(parts[3]))
    thousand_only <- suppressWarnings(as.numeric(parts[4]))

    if (!is.na(million)) {
      return(million * 1e6 + ifelse(is.na(thousand_after_million), 0, thousand_after_million * 1e3))
    }

    if (!is.na(thousand_only)) {
      return(thousand_only * 1e3)
    }

    NA_real_
  }, numeric(1))
}

parse_income_midpoint_clp <- function(x) {
  x <- blank_to_na(x)

  vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_real_)
    }

    if (normalize_text(s) == "SOBRE 3 MILLONES 500 MIL") {
      return(4e6)
    }

    amounts <- extract_amounts_clp(s)
    amounts <- amounts[is.finite(amounts)]
    if (length(amounts) >= 2) {
      return(mean(amounts[1:2]))
    }

    NA_real_
  }, numeric(1))
}

key_string <- function(dt, cols) {
  do.call(paste, c(dt[, ..cols], sep = " || "))
}

required_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
  path
}

# ------------------------- MiFuturo -------------------------

read_mifuturo_income <- function(path) {
  message("Reading MiFuturo income file: ", path)
  raw <- fread(required_file(path), encoding = "UTF-8", na.strings = c("", "NA", "n/a", "s/i"))
  setnames(raw, normalize_col_name(names(raw)))

  required_cols <- c(
    "codigo",
    "tipo_de_institucion",
    "nombre_de_institucion",
    "area",
    "nombre_carrera_generica",
    "nombre_carrera_del_titulo",
    "empleabilidad_1er_ano",
    "empleabilidad_2_ano",
    "ingreso_promedio_al_4_ano"
  )
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "MiFuturo file is missing expected columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- raw[, .(
    mifuturo_codigo = blank_to_na(codigo),
    mifuturo_tipo_inst = blank_to_na(tipo_de_institucion),
    mifuturo_nomb_inst = blank_to_na(nombre_de_institucion),
    mifuturo_area = blank_to_na(area),
    mifuturo_area_carrera_generica = blank_to_na(nombre_carrera_generica),
    mifuturo_nomb_carrera_titulo = blank_to_na(nombre_carrera_del_titulo),
    mifuturo_employment_1st_year = parse_percent(empleabilidad_1er_ano),
    mifuturo_employment_2nd_year = parse_percent(empleabilidad_2_ano),
    mifuturo_income_4th_year_raw = blank_to_na(ingreso_promedio_al_4_ano)
  )]

  out <- out[
    !is.na(mifuturo_codigo) &
      !is.na(mifuturo_tipo_inst) &
      !is.na(mifuturo_nomb_inst) &
      !is.na(mifuturo_area_carrera_generica)
  ]

  out[, `:=`(
    mifuturo_cod_inst = suppressWarnings(as.integer(mifuturo_codigo)),
    mifuturo_income_4th_year_mid_clp = parse_income_midpoint_clp(mifuturo_income_4th_year_raw)
  )]
  out[, `:=`(
    mifuturo_income_4th_year_mid_usd = mifuturo_income_4th_year_mid_clp / dollar_clp_conversion,
    mifuturo_income_4th_year_log_usd = log(mifuturo_income_4th_year_mid_clp / dollar_clp_conversion),
    mifuturo_income_source = fifelse(
      is.na(mifuturo_income_4th_year_mid_clp),
      "missing_in_mifuturo",
      "observed_range_midpoint"
    )
  )]

  out[, `:=`(
    key_cod_inst = as.character(mifuturo_cod_inst),
    key_tipo_inst = normalize_text(mifuturo_tipo_inst),
    key_nomb_inst = normalize_text(mifuturo_nomb_inst),
    key_area_generica = normalize_text(mifuturo_area_carrera_generica),
    key_nomb_carrera = normalize_text(mifuturo_nomb_carrera_titulo)
  )]

  out[]
}

# ------------------------- Matricula -------------------------

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

  setorder(dt, COD_SIES, -year_info)
  dt <- dt[!duplicated(COD_SIES)]
  dt[, .(
    COD_SIES = as.character(COD_SIES),
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
  missing_core <- setdiff(
    paste0(
      c(
        "year_info", "COD_SIES", "COD_CARRERA", "CODIGO_DEMRE", "NOMB_CARRERA",
        "TIPO_INST_1", "NOMB_INST", "COD_INST"
      ),
      suffix
    ),
    header
  )
  if (length(missing_core) > 0) {
    stop(
      "Clean matricula file is missing expected columns: ",
      paste(missing_core, collapse = ", "),
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
    enrollment_measure = enrollment_measure,
    COD_SIES = as.character(COD_SIES)
  )]

  if (!"AREA_CARRERA_GENERICA" %in% names(dt)) {
    dt[, AREA_CARRERA_GENERICA := NA_character_]
  }

  dt <- merge(dt, program_info, by = "COD_SIES", all.x = TRUE, sort = FALSE)
  dt[, area_source := fcase(
    !is.na(AREA_CARRERA_GENERICA), "matricula_clean",
    is.na(AREA_CARRERA_GENERICA) & !is.na(program_info_AREA_CARRERA_GENERICA), "program_info_22_24",
    default = NA_character_
  )]
  dt[is.na(AREA_CARRERA_GENERICA), AREA_CARRERA_GENERICA := program_info_AREA_CARRERA_GENERICA]

  dt[, program_info_found := !is.na(program_info_AREA_CARRERA_GENERICA)]
  dt[, `:=`(
    key_cod_inst = as.character(suppressWarnings(as.integer(COD_INST))),
    key_tipo_inst = normalize_text(TIPO_INST_1),
    key_nomb_inst = normalize_text(NOMB_INST),
    key_area_generica = normalize_text(AREA_CARRERA_GENERICA),
    key_nomb_carrera = normalize_text(NOMB_CARRERA)
  )]

  dt[]
}

# ------------------------- Matching Diagnostics -------------------------

build_mifuturo_key_table <- function(mifuturo, key_cols, match_label) {
  value_cols <- c(
    "mifuturo_codigo",
    "mifuturo_cod_inst",
    "mifuturo_tipo_inst",
    "mifuturo_nomb_inst",
    "mifuturo_area",
    "mifuturo_area_carrera_generica",
    "mifuturo_nomb_carrera_titulo",
    "mifuturo_employment_1st_year",
    "mifuturo_employment_2nd_year",
    "mifuturo_income_4th_year_raw",
    "mifuturo_income_4th_year_mid_clp",
    "mifuturo_income_4th_year_mid_usd",
    "mifuturo_income_4th_year_log_usd",
    "mifuturo_income_source"
  )

  dt <- copy(mifuturo)
  dt[, match_key := key_string(.SD, key_cols), .SDcols = key_cols]
  key_status <- dt[, .(
    mifuturo_rows_for_key = .N,
    mifuturo_rows_with_income_for_key = sum(!is.na(mifuturo_income_4th_year_mid_clp)),
    mifuturo_example_titles = paste(head(unique(mifuturo_nomb_carrera_titulo), 5), collapse = " | ")
  ), by = match_key]

  unique_keys <- key_status[mifuturo_rows_for_key == 1, .(match_key)]
  unique_values <- dt[unique_keys, on = "match_key", nomatch = 0][
    ,
    c("match_key", value_cols),
    with = FALSE
  ]
  unique_values[, match_key_name := match_label]

  list(key_status = key_status, unique_values = unique_values)
}

attempt_match <- function(matricula, mifuturo, key_cols, match_label) {
  mi_keys <- build_mifuturo_key_table(mifuturo, key_cols, match_label)

  dt <- copy(matricula)
  dt[, match_key := key_string(.SD, key_cols), .SDcols = key_cols]
  dt[, missing_match_key_component := Reduce(`|`, lapply(.SD, function(x) is.na(x) | x == "")), .SDcols = key_cols]

  dt <- merge(
    dt,
    mi_keys$key_status,
    by = "match_key",
    all.x = TRUE,
    sort = FALSE
  )
  dt <- merge(
    dt,
    mi_keys$unique_values,
    by = "match_key",
    all.x = TRUE,
    sort = FALSE
  )

  dt[, match_key_name := match_label]
  dt[, match_status := fcase(
    missing_match_key_component, "missing_key_component",
    is.na(mifuturo_rows_for_key), "no_mifuturo_key",
    mifuturo_rows_for_key > 1, "mifuturo_duplicate_key",
    !is.na(mifuturo_income_4th_year_mid_clp), "matched_observed_income",
    !is.na(mifuturo_rows_for_key) & is.na(mifuturo_income_4th_year_mid_clp), "matched_missing_income",
    default = "unclassified"
  )]

  dt[]
}

summarise_match <- function(dt) {
  dt[, .(
    student_rows = .N,
    students = uniqueN(MRUN),
    cod_sies = uniqueN(COD_SIES),
    rows_matched_observed_income = sum(match_status == "matched_observed_income"),
    rows_matched_missing_income = sum(match_status == "matched_missing_income"),
    rows_no_mifuturo_key = sum(match_status == "no_mifuturo_key"),
    rows_missing_key_component = sum(match_status == "missing_key_component"),
    rows_mifuturo_duplicate_key = sum(match_status == "mifuturo_duplicate_key"),
    share_rows_observed_income = mean(match_status == "matched_observed_income"),
    share_rows_any_mifuturo_match = mean(match_status %in% c("matched_observed_income", "matched_missing_income")),
    cod_sies_observed_income = uniqueN(COD_SIES[match_status == "matched_observed_income"]),
    cod_sies_any_mifuturo_match = uniqueN(COD_SIES[match_status %in% c("matched_observed_income", "matched_missing_income")]),
    cod_sies_missing_area = uniqueN(COD_SIES[is.na(AREA_CARRERA_GENERICA) | AREA_CARRERA_GENERICA == ""]),
    cod_sies_program_info_found = uniqueN(COD_SIES[program_info_found == TRUE])
  ), by = .(enrollment_measure, match_key_name, year_info)]
}

summarise_unmatched_programs <- function(dt) {
  dt[
    match_status != "matched_observed_income",
    .(
      student_rows = .N,
      years = paste(sort(unique(year_info)), collapse = "|"),
      match_statuses = paste(sort(unique(match_status)), collapse = "|"),
      example_nomb_carrera = NOMB_CARRERA[which(!is.na(NOMB_CARRERA))[1]],
      example_nomb_inst = NOMB_INST[which(!is.na(NOMB_INST))[1]],
      example_tipo_inst = TIPO_INST_1[which(!is.na(TIPO_INST_1))[1]],
      area_carrera_generica = AREA_CARRERA_GENERICA[which(!is.na(AREA_CARRERA_GENERICA))[1]],
      area_source = area_source[which(!is.na(area_source))[1]],
      cod_inst = COD_INST[which(!is.na(COD_INST))[1]],
      cod_carrera = COD_CARRERA[which(!is.na(COD_CARRERA))[1]],
      codigo_demre = CODIGO_DEMRE[which(!is.na(CODIGO_DEMRE))[1]],
      match_key = match_key[1],
      mifuturo_rows_for_key = mifuturo_rows_for_key[1],
      mifuturo_rows_with_income_for_key = mifuturo_rows_with_income_for_key[1],
      mifuturo_example_titles = mifuturo_example_titles[1]
    ),
    by = .(enrollment_measure, match_key_name, COD_SIES)
  ][order(enrollment_measure, match_key_name, -student_rows)]
}

summarise_key_multiplicity <- function(dt) {
  dt[, .(
    student_rows = .N,
    cod_sies = uniqueN(COD_SIES),
    matricula_titles = uniqueN(NOMB_CARRERA),
    matricula_generic_areas = uniqueN(AREA_CARRERA_GENERICA),
    mifuturo_rows_for_key = mifuturo_rows_for_key[1],
    mifuturo_rows_with_income_for_key = mifuturo_rows_with_income_for_key[1],
    example_cod_sies = paste(head(unique(COD_SIES), 8), collapse = " | "),
    example_matricula_titles = paste(head(unique(NOMB_CARRERA), 8), collapse = " | "),
    mifuturo_example_titles = mifuturo_example_titles[1]
  ), by = .(enrollment_measure, match_key_name, match_key)][
    order(enrollment_measure, match_key_name, -student_rows)
  ]
}

candidate_program_income <- function(dt) {
  dt[, .(
    student_rows = .N,
    years = paste(sort(unique(year_info)), collapse = "|"),
    match_status = match_status[1],
    NOMB_CARRERA = NOMB_CARRERA[which(!is.na(NOMB_CARRERA))[1]],
    TIPO_INST_1 = TIPO_INST_1[which(!is.na(TIPO_INST_1))[1]],
    NOMB_INST = NOMB_INST[which(!is.na(NOMB_INST))[1]],
    COD_INST = COD_INST[which(!is.na(COD_INST))[1]],
    COD_CARRERA = COD_CARRERA[which(!is.na(COD_CARRERA))[1]],
    CODIGO_DEMRE = CODIGO_DEMRE[which(!is.na(CODIGO_DEMRE))[1]],
    AREA_CARRERA_GENERICA = AREA_CARRERA_GENERICA[which(!is.na(AREA_CARRERA_GENERICA))[1]],
    area_source = area_source[which(!is.na(area_source))[1]],
    mifuturo_income_4th_year_mid_clp = mifuturo_income_4th_year_mid_clp[1],
    mifuturo_income_4th_year_mid_usd = mifuturo_income_4th_year_mid_usd[1],
    mifuturo_income_4th_year_log_usd = mifuturo_income_4th_year_log_usd[1],
    mifuturo_income_source = mifuturo_income_source[1],
    mifuturo_employment_1st_year = mifuturo_employment_1st_year[1],
    mifuturo_employment_2nd_year = mifuturo_employment_2nd_year[1],
    mifuturo_key_rows = mifuturo_rows_for_key[1],
    match_key = match_key[1]
  ), by = .(enrollment_measure, match_key_name, COD_SIES)][
    order(enrollment_measure, match_key_name, COD_SIES)
  ]
}

# ------------------------- Run -------------------------

mifuturo <- read_mifuturo_income(mifuturo_csv)
program_info <- read_program_info(program_info_path)

matricula <- rbindlist(
  list(
    read_clean_matricula(mat_first_path, "_m1", "first_ingreso", program_info),
    read_clean_matricula(mat_last_path, "_ml", "last_ingreso", program_info)
  ),
  use.names = TRUE,
  fill = TRUE
)

match_specs <- list(
  inst_type_generic_title = c(
    "key_cod_inst",
    "key_tipo_inst",
    "key_nomb_inst",
    "key_area_generica",
    "key_nomb_carrera"
  ),
  inst_type_generic = c(
    "key_cod_inst",
    "key_tipo_inst",
    "key_nomb_inst",
    "key_area_generica"
  )
)

matched <- rbindlist(lapply(names(match_specs), function(spec_name) {
  attempt_match(
    matricula = matricula,
    mifuturo = mifuturo,
    key_cols = match_specs[[spec_name]],
    match_label = spec_name
  )
}), use.names = TRUE, fill = TRUE)

summary_out <- summarise_match(matched)
unmatched_out <- summarise_unmatched_programs(matched)
key_multiplicity_out <- summarise_key_multiplicity(matched)
candidate_out <- candidate_program_income(matched)

source_map <- data.table(
  source = c(
    "matricula_clean",
    "program_info_22_24",
    "mifuturo_income_csv",
    "oferta_codes_24_25",
    "college_applications"
  ),
  relevant_codes = c(
    "MRUN, COD_SIES/CODIGO_UNICO, COD_CARRERA, CODIGO_DEMRE, COD_INST",
    "COD_SIES/CODIGO_UNICO, COD_CARRERA, CODIGO_DEMRE, COD_INST, AREA_CARRERA_GENERICA",
    "Código (institution code), Tipo de institución, Nombre de institución, Nombre carrera genérica, Nombre carrera (del título)",
    "COD_CARRERA_PREF, COD_SIES",
    "COD_CARRERA_PREF"
  ),
  documented_or_inferred = c(
    "documented in ER_Matricula_Educacion_Superior_2007_2025_PUBL_MRUN.pdf",
    "documented in matricula PDF; assembled by code/facts_major_choice_prep.R",
    "inferred from MiFuturo headers and checked against institution-code behavior",
    "assembled by code/facts_major_choice_prep.R from oferta definitiva files",
    "assembled by code/clean_college_apps_data.R"
  ),
  role_in_this_script = c(
    "student-program rows to which income is attached for diagnostics",
    "fills AREA_CARRERA_GENERICA because current clean matricula exports do not carry it",
    "source of income/employment measures",
    "not used for matricula merge diagnostic",
    "not used for matricula merge diagnostic"
  )
)

message("Writing diagnostics to: ", output_dir)
fwrite(summary_out, summary_path)
fwrite(unmatched_out, unmatched_path)
fwrite(key_multiplicity_out, key_multiplicity_path)
fwrite(candidate_out, candidate_program_path)
fwrite(source_map, source_map_path)

best_summary <- summary_out[
  ,
  .(
    student_rows = sum(student_rows),
    rows_matched_observed_income = sum(rows_matched_observed_income),
    rows_matched_missing_income = sum(rows_matched_missing_income),
    rows_no_mifuturo_key = sum(rows_no_mifuturo_key),
    rows_missing_key_component = sum(rows_missing_key_component),
    rows_mifuturo_duplicate_key = sum(rows_mifuturo_duplicate_key),
    share_rows_observed_income = sum(rows_matched_observed_income) / sum(student_rows),
    share_rows_any_mifuturo_match = sum(rows_matched_observed_income + rows_matched_missing_income) / sum(student_rows)
  ),
  by = .(enrollment_measure, match_key_name)
][order(enrollment_measure, match_key_name)]

report <- c(
  "# MiFuturo Matricula Income Merge Diagnostic",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "This is a diagnostic merge only. It does not modify the official matricula or universe files.",
  "",
  "## Inputs",
  "",
  paste0("- MiFuturo income CSV: `", mifuturo_csv, "`"),
  paste0("- First ingreso matricula: `", mat_first_path, "`"),
  paste0("- Last ingreso matricula: `", mat_last_path, "`"),
  paste0("- Program info supplement: `", program_info_path, "`"),
  "",
  "## Code Map",
  "",
  "- Matricula `CODIGO_UNICO`, renamed `COD_SIES`, is the documented program code considering institution, sede, carrera, jornada, and version.",
  "- Matricula `COD_CARRERA` is the career/program code.",
  "- Matricula `CODIGO_DEMRE` is available only for institutions in the SUA admissions system.",
  "- Matricula `COD_INST` is the higher-education institution code.",
  "- MiFuturo income file does not expose `COD_SIES`; its `Código` behaves as an institution code and is used only together with institution type/name and career names.",
  "",
  "## Match Keys Tested",
  "",
  "- `inst_type_generic_title`: `COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA + NOMB_CARRERA`",
  "- `inst_type_generic`: `COD_INST + TIPO_INST_1 + NOMB_INST + AREA_CARRERA_GENERICA`",
  "",
  "## Overall Summary",
  "",
  paste(capture.output(print(best_summary)), collapse = "\n"),
  "",
  "## Outputs",
  "",
  paste0("- `", summary_path, "`"),
  paste0("- `", unmatched_path, "`"),
  paste0("- `", key_multiplicity_path, "`"),
  paste0("- `", candidate_program_path, "`"),
  paste0("- `", source_map_path, "`"),
  "",
  "## Important Caution",
  "",
  "MiFuturo income is not available at the full `COD_SIES` level in the file inspected here.",
  "Even a successful match can map multiple `COD_SIES` values, such as sede/jornada/version variants, to one MiFuturo income row.",
  "Use `mifuturo_matricula_key_multiplicity.csv` before promoting any candidate income variable into the official pipeline."
)

writeLines(report, report_path)

message("Done.")
print(best_summary)
