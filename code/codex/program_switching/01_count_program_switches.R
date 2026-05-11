suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# ------------------------- Configuration -------------------------

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

years_env <- Sys.getenv("PROGRAM_SWITCHING_YEARS", unset = "")
years <- if (nzchar(years_env)) {
  as.integer(str_split(years_env, ",", simplify = TRUE))
} else {
  2022:2025
}

output_dir <- file.path(data_wd, "data/clean/program_switching")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

student_summary_path <- file.path(output_dir, "program_switching_student_summary_2022_2025.csv")
overall_summary_path <- file.path(output_dir, "program_switching_overall_summary_2022_2025.csv")
dimension_summary_path <- file.path(output_dir, "program_switching_dimension_summary_2022_2025.csv")
pipeline_programs_path <- file.path(output_dir, "potential_pipeline_programs_2022_2025.csv")

professional_technical_levels <- c("Carreras Profesionales", "Carreras Tecnicas", "Carreras Técnicas")

pipeline_regex <- regex(
  paste(
    c(
      "bachiller",
      "bachillerato",
      "plan comun",
      "plan común",
      "ciclo basico",
      "ciclo básico",
      "college",
      "propedeutico",
      "propedéutico"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# ------------------------- Helpers -------------------------

log_msg <- function(...) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
}

find_matricula_file <- function(year_input) {
  raw_year_dir <- file.path(data_wd, "data/raw", as.character(year_input))
  preferred_path <- file.path(
    raw_year_dir,
    paste0("Matricula-Ed-Superior-", year_input),
    paste0("20250729_Matrícula_Ed_Superior_", year_input, "_PUBL_MRUN.csv")
  )

  if (file.exists(preferred_path)) {
    return(preferred_path)
  }

  candidates <- list.files(
    raw_year_dir,
    pattern = "(Matricula|Matrícula|MATRICULA).*MRUN.*\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(candidates) == 0) {
    stop("No higher-ed matricula CSV found for year ", year_input, " under ", raw_year_dir)
  }

  candidates[[1]]
}

field_reclassified_from_cine97 <- function(area, subarea) {
  case_when(
    subarea == "Veterinaria" ~ "Health and Welfare",
    area %in% c("Agricultura", "Servicios") ~ NA_character_,
    area == "Ciencias" ~ "Science",
    area == "Educación" ~ "Teaching",
    area == "Humanidades y Artes" ~ "Humanities and Arts",
    area == "Ingeniería, Industria y Construcción" ~
      "Engineering, Manufacturing and Construction",
    area == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
      subarea == "Derecho" ~ "Law",
    area == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
      subarea == "Enseñanza Comercial y Administración" ~ "Business",
    area == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
      subarea %in% c("Ciencias Sociales y del Comportamiento", "Periodismo e Información") ~
      "Social Sciences",
    area == "Salud y Servicios Sociales" & subarea == "Medicina" ~ "Medicine",
    area == "Salud y Servicios Sociales" ~ "Health and Welfare",
    TRUE ~ NA_character_
  )
}

load_student_mat_info <- function(year_input) {
  file_path <- find_matricula_file(year_input)
  log_msg("Reading higher-ed matricula year ", year_input, ": ", file_path)

  out <- read_csv2(
    file_path,
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    col_select = any_of(c(
      "mrun",
      "codigo_unico",
      "cod_carrera",
      "codigo_demre",
      "nomb_carrera",
      "tipo_inst_1",
      "nomb_inst",
      "cod_inst",
      "area_carrera_generica",
      "nivel_carrera_2",
      "cine_f_97_area_area",
      "cine_f_97_subarea"
    )),
    show_col_types = FALSE
  ) %>%
    rename_with(~ toupper(.x)) %>%
    filter(NIVEL_CARRERA_2 %in% professional_technical_levels) %>%
    transmute(
      MRUN,
      year_info = year_input,
      program_id = coalesce(as.character(CODIGO_UNICO), paste(COD_INST, COD_CARRERA, sep = "__")),
      institution_id = as.character(COD_INST),
      field_id = field_reclassified_from_cine97(CINE_F_97_AREA_AREA, CINE_F_97_SUBAREA),
      generic_major_id = AREA_CARRERA_GENERICA,
      COD_SIES = CODIGO_UNICO,
      COD_CARRERA,
      CODIGO_DEMRE,
      NOMB_CARRERA,
      TIPO_INST_1,
      NOMB_INST,
      COD_INST,
      AREA_CARRERA_GENERICA,
      CINE_F_97_AREA_AREA,
      CINE_F_97_SUBAREA,
      is_pipeline_like_program = str_detect(
        str_squish(str_to_lower(paste(NOMB_CARRERA, AREA_CARRERA_GENERICA, sep = " "))),
        pipeline_regex
      )
    )

  log_msg("Kept ", nrow(out), " professional/technical rows for ", year_input)
  out
}

# ------------------------- Load -------------------------

log_msg("Starting simple individual-level program-switching build for years: ", paste(years, collapse = ", "))

student_mat_all <- map_dfr(years, load_student_mat_info) %>%
  distinct(MRUN, year_info, program_id, institution_id, field_id, .keep_all = TRUE)

log_msg("Loaded distinct student-year-program rows: ", nrow(student_mat_all))

# ------------------------- Individual-Level Switch Counts -------------------------

dt <- as.data.table(student_mat_all)

student_summary <- dt[
  ,
  .(
    first_year_observed = min(year_info, na.rm = TRUE),
    last_year_observed = max(year_info, na.rm = TRUE),
    n_years_enrolled = uniqueN(year_info),
    n_student_year_program_rows = .N,
    n_programs_observed = uniqueN(program_id[!is.na(program_id)]),
    n_institutions_observed = uniqueN(institution_id[!is.na(institution_id)]),
    n_generic_majors_observed = uniqueN(generic_major_id[!is.na(generic_major_id)]),
    n_fields_observed = uniqueN(field_id[!is.na(field_id)]),
    any_pipeline_like_program_observed = any(is_pipeline_like_program %in% TRUE)
  ),
  by = MRUN
][
  ,
  `:=`(
    any_program_switch_observed = n_programs_observed > 1,
    any_institution_switch_observed = n_institutions_observed > 1,
    any_generic_major_switch_observed = n_generic_majors_observed > 1,
    any_field_switch_observed = n_fields_observed > 1,
    program_switch_same_institution_only = n_programs_observed > 1 & n_institutions_observed == 1,
    program_switch_cross_institution = n_programs_observed > 1 & n_institutions_observed > 1,
    program_switch_same_generic_major_only = n_programs_observed > 1 & n_generic_majors_observed == 1,
    program_switch_cross_generic_major = n_programs_observed > 1 & n_generic_majors_observed > 1,
    program_switch_same_field_only = n_programs_observed > 1 & n_fields_observed == 1,
    program_switch_cross_field = n_programs_observed > 1 & n_fields_observed > 1
  )
]

log_msg("Built student summary rows: ", nrow(student_summary))

overall_summary <- student_summary[
  ,
  .(
    years_loaded = paste(years, collapse = ", "),
    n_students_enrolled = .N,
    n_students_multiple_years = sum(n_years_enrolled > 1),
    n_students_any_program_switch = sum(any_program_switch_observed),
    n_students_program_switch_same_institution_only = sum(program_switch_same_institution_only),
    n_students_program_switch_cross_institution = sum(program_switch_cross_institution),
    n_students_any_institution_switch = sum(any_institution_switch_observed),
    n_students_any_generic_major_switch = sum(any_generic_major_switch_observed),
    n_students_program_switch_same_generic_major_only = sum(program_switch_same_generic_major_only),
    n_students_program_switch_cross_generic_major = sum(program_switch_cross_generic_major),
    n_students_any_field_switch = sum(any_field_switch_observed),
    n_students_program_switch_same_field_only = sum(program_switch_same_field_only),
    n_students_program_switch_cross_field = sum(program_switch_cross_field),
    n_students_any_pipeline_like_program = sum(any_pipeline_like_program_observed),
    n_students_program_switch_with_pipeline_like_program =
      sum(any_program_switch_observed & any_pipeline_like_program_observed)
  )
][
  ,
  `:=`(
    share_students_multiple_years = n_students_multiple_years / n_students_enrolled,
    share_students_any_program_switch = n_students_any_program_switch / n_students_enrolled,
    share_multiple_year_students_any_program_switch =
      n_students_any_program_switch / n_students_multiple_years,
    share_program_switchers_same_institution_only =
      n_students_program_switch_same_institution_only / n_students_any_program_switch,
    share_program_switchers_cross_institution =
      n_students_program_switch_cross_institution / n_students_any_program_switch,
    share_program_switchers_same_generic_major_only =
      n_students_program_switch_same_generic_major_only / n_students_any_program_switch,
    share_program_switchers_cross_generic_major =
      n_students_program_switch_cross_generic_major / n_students_any_program_switch,
    share_program_switchers_same_field_only =
      n_students_program_switch_same_field_only / n_students_any_program_switch,
    share_program_switchers_cross_field =
      n_students_program_switch_cross_field / n_students_any_program_switch,
    share_program_switchers_with_pipeline_like_program =
      n_students_program_switch_with_pipeline_like_program / n_students_any_program_switch
  )
]

dimension_summary <- data.table(
  dimension = c(
    "program",
    "institution",
    "generic_major",
    "field",
    "program_same_institution_only",
    "program_cross_institution",
    "program_same_generic_major_only",
    "program_cross_generic_major",
    "program_same_field_only",
    "program_cross_field",
    "program_with_pipeline_like_program"
  ),
  n_students = c(
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_institution_switch,
    overall_summary$n_students_any_generic_major_switch,
    overall_summary$n_students_any_field_switch,
    overall_summary$n_students_program_switch_same_institution_only,
    overall_summary$n_students_program_switch_cross_institution,
    overall_summary$n_students_program_switch_same_generic_major_only,
    overall_summary$n_students_program_switch_cross_generic_major,
    overall_summary$n_students_program_switch_same_field_only,
    overall_summary$n_students_program_switch_cross_field,
    overall_summary$n_students_program_switch_with_pipeline_like_program
  ),
  denominator = c(
    overall_summary$n_students_enrolled,
    overall_summary$n_students_enrolled,
    overall_summary$n_students_enrolled,
    overall_summary$n_students_enrolled,
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_program_switch,
    overall_summary$n_students_any_program_switch
  )
)[, share := n_students / denominator]

switchers <- student_summary[any_program_switch_observed == TRUE, .(MRUN)]

pipeline_programs <- merge(
  dt[is_pipeline_like_program == TRUE],
  switchers,
  by = "MRUN",
  allow.cartesian = TRUE
)[
  ,
  .(
    n_switching_students = uniqueN(MRUN),
    first_year_observed = min(year_info, na.rm = TRUE),
    last_year_observed = max(year_info, na.rm = TRUE)
  ),
  by = .(
    institution_id,
    NOMB_INST,
    program_id,
    NOMB_CARRERA,
    AREA_CARRERA_GENERICA
  )
][order(-n_switching_students)]

# ------------------------- Export -------------------------

write_csv(as_tibble(student_summary), student_summary_path)
write_csv(as_tibble(overall_summary), overall_summary_path)
write_csv(as_tibble(dimension_summary), dimension_summary_path)
write_csv(as_tibble(pipeline_programs), pipeline_programs_path)

cat("Wrote simple program-switching outputs to:\n")
cat("- ", student_summary_path, "\n", sep = "")
cat("- ", overall_summary_path, "\n", sep = "")
cat("- ", dimension_summary_path, "\n", sep = "")
cat("- ", pipeline_programs_path, "\n", sep = "")
