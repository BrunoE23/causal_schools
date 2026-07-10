###############################################################################
# Build canonical 2022-2025 SIES program metadata
#
# The historical clean object `program_info_22-24.rds` predates the July 2026
# update, but the project already has full SIES Matricula-Ed-Superior 2025.
# This script appends 2025 program metadata and keeps one most-recent row per
# COD_SIES. It does not use PAES matricula as a substitute for full SIES data.
###############################################################################

find_existing_path <- function(candidates, label) {
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0L) {
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
})

clean_dir <- file.path(data_wd, "data", "clean")
out_dir <- file.path(repo_wd, "output", "tables", "paes_2026_update")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

program_info_old_path <- file.path(clean_dir, "program_info_22-24.rds")
program_info_new_path <- file.path(clean_dir, "program_info_22-25.rds")
matricula_2025_path <- file.path(
  data_wd,
  "data/raw/2025/Matricula-Ed-Superior-2025/20250729_Matrícula_Ed_Superior_2025_PUBL_MRUN.csv"
)
oferta_24_26_path <- file.path(clean_dir, "oferta_codes_24_26_all.rds")

required_files <- c(program_info_old_path, matricula_2025_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"), call. = FALSE)
}

read_program_info_2025 <- function(path) {
  requested <- c(
    "CODIGO_UNICO",
    "COD_CARRERA",
    "CODIGO_DEMRE",
    "NOMB_CARRERA",
    "TIPO_INST_1",
    "NOMB_INST",
    "COD_INST",
    "AREA_CARRERA_GENERICA",
    "REGION_SEDE",
    "PROVINCIA_SEDE",
    "COMUNA_SEDE",
    "AREA_CONOCIMIENTO",
    "CINE_F_13_AREA",
    "CINE_F_13_SUBAREA",
    "CINE_F_97_AREA_AREA",
    "CINE_F_97_SUBAREA",
    "ACREDITADA_CARR",
    "ACREDITADA_INST",
    "NIVEL_CARRERA_2"
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
  dt <- dt[NIVEL_CARRERA_2 %chin% c("Carreras Profesionales", "Carreras Técnicas")]
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
    CINE_F_13_SUBAREA,
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

old <- as.data.table(readRDS(program_info_old_path))
old[, COD_SIES := as.character(COD_SIES)]
if (!"year_info" %in% names(old)) {
  old[, year_info := NA_integer_]
}

new_2025 <- read_program_info_2025(matricula_2025_path)
program_info_22_25_all <- rbindlist(list(old, new_2025), use.names = TRUE, fill = TRUE)
program_info_22_25 <- program_info_22_25_all[
  !is.na(COD_SIES) & COD_SIES != ""
][
  order(COD_SIES, year_info)
][
  ,
  .SD[.N],
  by = COD_SIES
]

program_info_22_25[
  ,
  field_reclassified := field_reclassify(CINE_F_97_AREA_AREA, CINE_F_97_SUBAREA)
]
program_info_22_25[, f_science := as.integer(field_reclassified == "Science")]
program_info_22_25[, f_social := as.integer(field_reclassified == "Social Sciences")]
program_info_22_25[, f_business := as.integer(field_reclassified == "Business")]
program_info_22_25[, f_law := as.integer(field_reclassified == "Law")]
program_info_22_25[, f_teaching := as.integer(field_reclassified == "Teaching")]
program_info_22_25[, f_humarts := as.integer(field_reclassified == "Humanities and Arts")]
program_info_22_25[
  ,
  f_eng := as.integer(field_reclassified == "Engineering, Manufacturing and Construction")
]
program_info_22_25[, f_medicine := as.integer(field_reclassified == "Medicine")]
program_info_22_25[, f_health := as.integer(field_reclassified == "Health and Welfare")]

if (anyDuplicated(program_info_22_25$COD_SIES) > 0L) {
  stop("program_info_22_25 has duplicated COD_SIES after most-recent collapse.", call. = FALSE)
}

saveRDS(program_info_22_25, program_info_new_path)

diagnostics <- rbindlist(
  list(
    copy(old)[
      ,
      source := "old_program_info_22_24"
    ][
      ,
      .(
        rows = .N,
        unique_cod_sies = uniqueN(COD_SIES),
        missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
      ),
      by = .(source, year_info)
    ],
    copy(new_2025)[
      ,
      source := "raw_sies_2025_program_info"
    ][
      ,
      .(
        rows = .N,
        unique_cod_sies = uniqueN(COD_SIES),
        missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
      ),
      by = .(source, year_info)
    ],
    copy(program_info_22_25)[
      ,
      source := "program_info_22_25_most_recent"
    ][
      ,
      .(
        rows = .N,
        unique_cod_sies = uniqueN(COD_SIES),
        missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
      ),
      by = .(source, year_info)
    ]
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(diagnostics, source, year_info)
fwrite(diagnostics, file.path(out_dir, "program_info_22_25_diagnostics.csv"))

if (file.exists(oferta_24_26_path)) {
  oferta <- as.data.table(readRDS(oferta_24_26_path))
  oferta[, COD_SIES := as.character(COD_SIES)]
  old_cod <- old[!is.na(COD_SIES) & COD_SIES != "", unique(COD_SIES)]
  new_cod <- program_info_22_25[, unique(COD_SIES)]
  coverage <- oferta[
    ,
    .(
      rows = .N,
      rows_with_cod_sies = sum(!is.na(COD_SIES) & COD_SIES != ""),
      matched_program_info_22_24 = sum(COD_SIES %chin% old_cod, na.rm = TRUE),
      matched_program_info_22_25 = sum(COD_SIES %chin% new_cod, na.rm = TRUE)
    ),
    by = year_info
  ][order(year_info)]
  coverage[
    ,
    `:=`(
      share_matched_program_info_22_24 = matched_program_info_22_24 / rows,
      share_matched_program_info_22_25 = matched_program_info_22_25 / rows
    )
  ]
  fwrite(coverage, file.path(out_dir, "program_info_22_25_oferta_coverage.csv"))
}

print(diagnostics)
cat("\nSaved: ", program_info_new_path, "\n", sep = "")
