###############################################################################
# Append PAES 2026 first-application rows to stem_outcome.RData
#
# The existing stem_outcome object was built before PAES 2026. This script keeps
# all existing rows and adds only students whose first regular application year
# is 2026. It uses 2026 oferta codes, 2022-2025 SIES program metadata, and the
# existing MiFuturo-imputed program descriptors.
###############################################################################

find_existing_path <- function(candidates, label) {
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Update candidates.", call. = FALSE)
  }

  candidates[[1]]
}

repo_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools",
    "C:/Users/brunem/Research/causal_schools"
  ),
  "repo_wd"
)
data_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Dropbox/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools"
  ),
  "data_wd"
)

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

clean_dir <- file.path(data_wd, "data", "clean")
update_clean_dir <- file.path(clean_dir, "paes_2026_update")
diagnostic_dir <- file.path(repo_wd, "output", "tables", "paes_2026_update")
dir.create(update_clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

college_apps_path <- file.path(clean_dir, "psu_applications.RData")
stem_outcome_path <- file.path(clean_dir, "stem_outcome.RData")
program_info_base_path <- if (file.exists(file.path(clean_dir, "program_info_22-25.rds"))) {
  file.path(clean_dir, "program_info_22-25.rds")
} else {
  file.path(clean_dir, "program_info_22-24.rds")
}
mifuturo_imputed_path <- file.path(clean_dir, "mifuturo_imputed.rds")
oferta_rec_path <- file.path(update_clean_dir, "oferta_codes_2024_2026_rec.rds")
matricula_2025_path <- file.path(
  data_wd,
  "data/raw/2025/Matricula-Ed-Superior-2025/20250729_Matrícula_Ed_Superior_2025_PUBL_MRUN.csv"
)

required_files <- c(
  college_apps_path, stem_outcome_path, program_info_base_path,
  mifuturo_imputed_path, oferta_rec_path, matricula_2025_path
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"), call. = FALSE)
}

load(college_apps_path)
load(stem_outcome_path)
college_apps <- as.data.table(college_apps)
stem_outcome_old <- as.data.table(stem_outcome)

read_program_info_2025 <- function(path) {
  requested <- c(
    "CODIGO_UNICO", "COD_CARRERA", "CODIGO_DEMRE",
    "NOMB_CARRERA", "TIPO_INST_1", "NOMB_INST", "COD_INST",
    "AREA_CARRERA_GENERICA",
    "REGION_SEDE", "PROVINCIA_SEDE", "COMUNA_SEDE",
    "AREA_CONOCIMIENTO", "CINE_F_13_AREA",
    "CINE_F_97_AREA_AREA", "CINE_F_97_SUBAREA",
    "ACREDITADA_CARR", "ACREDITADA_INST", "NIVEL_CARRERA_2"
  )
  header <- names(fread(path, nrows = 0, encoding = "UTF-8"))
  lookup <- setNames(header, toupper(header))
  missing_cols <- setdiff(requested, names(lookup))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing 2025 program-info columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  dt <- fread(path, select = unname(lookup[requested]), encoding = "UTF-8")
  setnames(dt, names(dt), toupper(names(dt)))
  dt <- dt[
    NIVEL_CARRERA_2 %chin% c("Carreras Profesionales", "Carreras Técnicas")
  ]
  setnames(dt, "CODIGO_UNICO", "COD_SIES")
  dt[, year_info := 2025L]
  unique(dt[, .(
    COD_SIES = as.character(COD_SIES),
    COD_CARRERA,
    CODIGO_DEMRE,
    NOMB_CARRERA,
    TIPO_INST_1,
    NOMB_INST,
    COD_INST,
    AREA_CARRERA_GENERICA,
    REGION_SEDE,
    PROVINCIA_SEDE,
    COMUNA_SEDE,
    AREA_CONOCIMIENTO,
    CINE_F_13_AREA,
    CINE_F_97_AREA_AREA,
    CINE_F_97_SUBAREA,
    ACREDITADA_CARR,
    ACREDITADA_INST,
    year_info
  )])
}

field_reclassify <- function(area, subarea) {
  fcase(
    subarea == "Veterinaria", "Health and Welfare",
    area %chin% c("Agricultura", "Servicios"), NA_character_,
    area == "Ciencias", "Science",
    area == "Educación", "Teaching",
    area == "Humanidades y Artes", "Humanities and Arts",
    area == "Ingeniería, Industria y Construcción",
    "Engineering, Manufacturing and Construction",
    area == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
      subarea == "Derecho",
    "Law",
    area == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
      subarea == "Enseñanza Comercial y Administración",
    "Business",
    area == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
      subarea %chin% c("Ciencias Sociales y del Comportamiento", "Periodismo e Información"),
    "Social Sciences",
    area == "Salud y Servicios Sociales" & subarea == "Medicina",
    "Medicine",
    area == "Salud y Servicios Sociales",
    "Health and Welfare",
    default = NA_character_
  )
}

program_info <- as.data.table(readRDS(program_info_base_path))
program_info[, COD_SIES := as.character(COD_SIES)]
if (!"year_info" %in% names(program_info)) {
  program_info[, year_info := NA_integer_]
}
program_info_2025 <- read_program_info_2025(matricula_2025_path)
program_info_all <- rbindlist(
  list(program_info, program_info_2025),
  use.names = TRUE,
  fill = TRUE
)
program_info_all <- program_info_all[
  order(COD_SIES, year_info)
][
  ,
  .SD[.N],
  by = COD_SIES
]
program_info_all[
  ,
  field_reclassified := field_reclassify(CINE_F_97_AREA_AREA, CINE_F_97_SUBAREA)
]
program_info_all[, f_science := as.integer(field_reclassified == "Science")]
program_info_all[, f_social := as.integer(field_reclassified == "Social Sciences")]
program_info_all[, f_law := as.integer(field_reclassified == "Law")]
program_info_all[, f_teaching := as.integer(field_reclassified == "Teaching")]
program_info_all[, f_humarts := as.integer(field_reclassified == "Humanities and Arts")]
program_info_all[
  ,
  f_eng := as.integer(field_reclassified == "Engineering, Manufacturing and Construction")
]
program_info_all[, f_medicine := as.integer(field_reclassified == "Medicine")]
program_info_all[, f_health := as.integer(field_reclassified == "Health and Welfare")]

mifuturo_imputed <- as.data.table(readRDS(mifuturo_imputed_path))
mifuturo_imputed[
  ,
  `:=`(
    NOMB_INST = str_squish(NOMB_INST),
    AREA_CARRERA_GENERICA = str_squish(AREA_CARRERA_GENERICA),
    TIPO_INST_1 = str_squish(TIPO_INST_1)
  )
]
program_info_all[
  ,
  `:=`(
    NOMB_INST = str_squish(NOMB_INST),
    AREA_CARRERA_GENERICA = str_squish(AREA_CARRERA_GENERICA),
    TIPO_INST_1 = str_squish(TIPO_INST_1)
  )
]

program_info_joint <- merge(
  program_info_all,
  mifuturo_imputed[
    ,
    .(
      NOMB_INST,
      AREA_CARRERA_GENERICA,
      TIPO_INST_1,
      employment_1st_year,
      employment_2nd_year,
      income_4th_year_mid_clp
    )
  ],
  by = c("NOMB_INST", "AREA_CARRERA_GENERICA", "TIPO_INST_1"),
  all.x = TRUE
)

oferta_rec <- as.data.table(readRDS(oferta_rec_path))
oferta_rec[, COD_SIES := as.character(COD_SIES)]
oferta_rec[, stem_share := rowSums(.SD, na.rm = TRUE), .SDcols = c("CIEN", "M1", "M2")]
oferta_info <- merge(
  oferta_rec,
  program_info_joint,
  by = "COD_SIES",
  all.x = TRUE,
  allow.cartesian = TRUE
)

regular_apps <- college_apps[TIPO_PREF == "REGULAR"]
first_year <- regular_apps[, .(year_1st_app = min(year, na.rm = TRUE)), by = mrun]
new_2026_students <- first_year[
  year_1st_app == 2026L & !(mrun %in% stem_outcome_old[["mrun"]])
]

apps_new <- regular_apps[
  mrun %in% new_2026_students[["mrun"]] &
    year == 2026L &
    ORDEN_PREF <= 5L
]
apps_new <- merge(
  apps_new,
  oferta_info,
  by = "COD_CARRERA_PREF",
  all.x = TRUE,
  allow.cartesian = TRUE
)

stem_outcome_new <- apps_new[
  ,
  .(
    avg_stem_share = mean(stem_share, na.rm = TRUE),
    prop_f_science = mean(f_science, na.rm = TRUE),
    prop_f_social = mean(f_social, na.rm = TRUE),
    prop_f_law = mean(f_law, na.rm = TRUE),
    prop_f_teaching = mean(f_teaching, na.rm = TRUE),
    prop_f_humarts = mean(f_humarts, na.rm = TRUE),
    prop_f_eng = mean(f_eng, na.rm = TRUE),
    prop_f_medicine = mean(f_medicine, na.rm = TRUE),
    prop_f_health = mean(f_health, na.rm = TRUE),
    year_1st_app = min(year, na.rm = TRUE),
    avg_employ_y1 = mean(employment_1st_year, na.rm = TRUE),
    avg_employ_y2 = mean(employment_2nd_year, na.rm = TRUE),
    avg_income_y4 = mean(income_4th_year_mid_clp, na.rm = TRUE),
    n_top5_regular_apps = .N,
    n_missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == ""),
    n_missing_program_info = sum(is.na(field_reclassified)),
    n_missing_mifuturo_income = sum(is.na(income_4th_year_mid_clp))
  ),
  by = mrun
]

analysis_cols <- names(stem_outcome_old)
stem_outcome_appended <- rbindlist(
  list(stem_outcome_old, stem_outcome_new[, ..analysis_cols]),
  use.names = TRUE,
  fill = TRUE
)
setorder(stem_outcome_appended, mrun)
stem_outcome <- as.data.frame(stem_outcome_appended)

backup_path <- file.path(update_clean_dir, "stem_outcome_pre_paes2026_append.RData")
save(stem_outcome_old, file = backup_path)
save(stem_outcome, file = stem_outcome_path)

diagnostics <- data.table(
  metric = c(
    "old_stem_outcome_rows",
    "new_2026_first_app_students",
    "new_2026_rows_appended",
    "appended_total_rows",
    "new_2026_rows_with_missing_cod_sies",
    "new_2026_rows_with_missing_program_info",
    "new_2026_rows_with_missing_mifuturo_income"
  ),
  value = c(
    nrow(stem_outcome_old),
    nrow(new_2026_students),
    nrow(stem_outcome_new),
    nrow(stem_outcome_appended),
    sum(stem_outcome_new$n_missing_cod_sies > 0L, na.rm = TRUE),
    sum(stem_outcome_new$n_missing_program_info > 0L, na.rm = TRUE),
    sum(stem_outcome_new$n_missing_mifuturo_income > 0L, na.rm = TRUE)
  )
)

coverage_by_year <- stem_outcome_appended[
  ,
  .(
    rows = .N,
    missing_avg_stem_share = sum(is.na(avg_stem_share)),
    missing_field_props = sum(
      is.na(prop_f_science) &
        is.na(prop_f_eng) &
        is.na(prop_f_law)
    ),
    missing_avg_income_y4 = sum(is.na(avg_income_y4))
  ),
  by = year_1st_app
][order(year_1st_app)]

fwrite(diagnostics, file.path(diagnostic_dir, "stem_outcome_2026_append_diagnostics.csv"))
fwrite(coverage_by_year, file.path(diagnostic_dir, "stem_outcome_coverage_by_first_app_year.csv"))

print(diagnostics)
print(coverage_by_year)
