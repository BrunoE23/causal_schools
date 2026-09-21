library(data.table)
library(haven)

raw_dir <- "C:/Users/brunem/Box/causal_schools/data/raw/becas_creditos"
clean_dir <- "C:/Users/brunem/Box/causal_schools/data/clean"

read_source <- function(path) {
  source_encoding <- if (grepl("POSTULACIONES_FUAS_2024_WEB\\.csv$", path)) "Latin-1" else "UTF-8"
  data <- fread(
    path,
    sep = ";",
    quote = "\"",
    na.strings = c("", " "),
    strip.white = TRUE,
    encoding = source_encoding,
    colClasses = "character",
    showProgress = FALSE
  )
  setnames(data, tolower(names(data)))
  data[, (names(data)) := lapply(.SD, function(x) fifelse(trimws(x) == "", NA_character_, trimws(x)))]
  data
}

to_integer <- function(data, columns) {
  present <- intersect(columns, names(data))
  data[, (present) := lapply(.SD, function(x) as.integer(x)), .SDcols = present]
  data
}

append_sources <- function(paths, standard_names, rename_map, integer_columns) {
  cleaned <- lapply(paths, function(path) {
    data <- read_source(path)
    for (from in names(rename_map)) {
      if (from %in% names(data)) setnames(data, from, rename_map[[from]])
    }
    missing <- setdiff(standard_names, names(data))
    if (length(missing)) data[, (missing) := NA_character_]
    data <- data[, ..standard_names]
    to_integer(data, integer_columns)
  })
  rbindlist(cleaned, use.names = TRUE)
}

postulaciones_paths <- list.files(
  raw_dir,
  pattern = "^POSTULACIONES.*\\.csv$|^BBDD_FUAS.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
asignaciones_paths <- list.files(
  raw_dir,
  pattern = "^Asignacion.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(postulaciones_paths) == 6L, length(asignaciones_paths) == 6L)

postulaciones_columns <- c(
  "anio_proceso", "mrun", "region", "nombre_region", "comuna", "nombre_comuna",
  "fec_nac", "anio_nacimiento", "edad", "genero", "quintil_se4", "decil_se4",
  "proceso", "cod_depe", "nem", "nacionalidad", "etnia"
)
postulaciones_integer_columns <- c(
  "anio_proceso", "mrun", "region", "comuna", "fec_nac", "anio_nacimiento", "edad",
  "genero", "quintil_se4", "decil_se4", "cod_depe", "nem", "nacionalidad", "etnia"
)

postulaciones <- append_sources(
  postulaciones_paths,
  postulaciones_columns,
  c("fec_nac_alu" = "fec_nac", "quinti_se4" = "quintil_se4"),
  postulaciones_integer_columns
)

asignaciones_columns <- c(
  "anio_beneficio", "mrun", "quintil_ingreso", "decil_dfe", "beneficio_beca_fscu", "tipo_alumno"
)
asignaciones <- append_sources(
  asignaciones_paths,
  asignaciones_columns,
  character(),
  c("anio_beneficio", "mrun", "quintil_ingreso", "decil_dfe", "tipo_alumno")
)

setorder(postulaciones, anio_proceso, mrun)
setorder(asignaciones, anio_beneficio, mrun)

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
write_dta(as.data.frame(postulaciones), file.path(clean_dir, "postulaciones.dta"), version = 15)
write_dta(as.data.frame(asignaciones), file.path(clean_dir, "asignaciones.dta"), version = 15)

summary <- rbindlist(list(
  postulaciones[, .(dataset = "postulaciones", year = anio_proceso, observations = .N), by = anio_proceso][, anio_proceso := NULL],
  asignaciones[, .(dataset = "asignaciones", year = anio_beneficio, observations = .N), by = anio_beneficio][, anio_beneficio := NULL]
))
print(summary[order(dataset, year)])
print(postulaciones[, .(observations = .N, duplicate_year_mrun = sum(duplicated(.SD))), .SDcols = c("anio_proceso", "mrun")])
print(asignaciones[, .(observations = .N, duplicate_year_mrun = sum(duplicated(.SD))), .SDcols = c("anio_beneficio", "mrun")])
