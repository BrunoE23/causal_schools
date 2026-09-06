suppressPackageStartupMessages({
  library(data.table)
})

# Clean the public Mineduc teacher-directory files, 2018 onward.
#
# Usage:
#   Rscript 01_clean_docentes_educacion.R
#   Rscript 01_clean_docentes_educacion.R --dry-run
#   Rscript 01_clean_docentes_educacion.R INPUT_DIR OUTPUT_DIR
#
# With no positional arguments, INPUT_DIR is the Dropbox raw-data directory
# below and OUTPUT_DIR is data/processed/docentes_educacion beside data/raw.

default_input_dir <- file.path(
  "C:/Users/brunem/Dropbox/causal_schools/data/raw/docentes_educacion"
)

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %chin% args
args <- args[args != "--dry-run"]

if (length(args) > 2L) {
  stop(
    "Usage: Rscript 01_clean_docentes_educacion.R ",
    "[INPUT_DIR] [OUTPUT_DIR] [--dry-run]",
    call. = FALSE
  )
}

input_dir <- if (length(args) >= 1L) args[[1L]] else default_input_dir
input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)

default_output_dir <- file.path(
  dirname(dirname(input_dir)), "processed", "docentes_educacion"
)
output_dir <- if (length(args) >= 2L) args[[2L]] else default_output_dir
output_dir <- normalizePath(
  output_dir, winslash = "/", mustWork = FALSE
)

csv_files <- sort(list.files(
  input_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
))

if (!length(csv_files)) {
  stop("No CSV files found below: ", input_dir, call. = FALSE)
}

id_vars <- c(
  "AGNO", "RBD", "NOMBRE_SLEP", "MRUN",
  "DOC_GENERO", "DOC_FEC_NAC"
)
career_vars <- c("TRAMO_CARR_DOCENTE", "BIENIOS_CARR_DOCENTE")
function_vars <- c("PERSONAS", "ID_IFP")
training_vars <- c(
  "TIP_INSTI_ID_1", "TIP_INSTI_ID_2",
  "ANO_TITULACION_1", "ANO_TITULACION_2",
  "DURACION_CARRERA_1", "DURACION_CARRERA_2",
  "MODALIDAD_ESTUDIO_1", "MODALIDAD_ESTUDIO_2"
)
selected_mention_vars <- c(
  "MEN_MATE_1", "MEN_MATE_2",
  "MEN_LENGUAJE_1", "MEN_LENGUAJE_2",
  "MEN_ORIENTACION_1", "MEN_ORIENTACION_2"
)
character_vars <- c("NOMBRE_SLEP", "MRUN")

id_ifp_labels <- c(
  "0" = "sin_funcion",
  "1" = "docente_de_aula",
  "2" = "planta_tecnico_pedagogica",
  "3" = "planta_directiva",
  "4" = "director",
  "5" = "otra_en_establecimiento",
  "6" = "otra_fuera_establecimiento",
  "7" = "jefe_unidad_tecnico_pedagogica",
  "8" = "inspector_general",
  "9" = "orientador",
  "10" = "directiva",
  "11" = "tecnico_pedagogica",
  "12" = "supervision",
  "13" = "jefe_daem",
  "14" = "jefe_corporacion_municipal",
  "15" = "subdirector",
  "16" = "profesor_encargado_establecimiento",
  "17" = "educador_tradicional"
)

label_id_ifp <- function(x) {
  labels <- unname(id_ifp_labels[as.character(x)])
  unknown <- is.na(labels) & !is.na(x)
  labels[unknown] <- paste0("codigo_", x[unknown])
  labels
}

output_vars <- c(
  "AGNO", "RBD", "NOMBRE_SLEP", "MRUN",
  "DOC_GENERO", "DOC_FEC_NAC",
  "PERSONAS", "ID_IFP", "FUNCION_PRINCIPAL",
  "TRAMO_CARR_DOCENTE", "BIENIOS_CARR_DOCENTE",
  training_vars,
  "N_MENCIONES_TOTAL",
  selected_mention_vars
)

clean_one_file <- function(path) {
  header <- names(fread(
    path, sep = ";", nrows = 0L, encoding = "Latin-1", showProgress = FALSE
  ))

  required <- c("AGNO", "RBD", "MRUN", "PERSONAS", "ID_IFP")
  missing_required <- setdiff(required, header)
  if (length(missing_required)) {
    stop(
      "Missing required variables in ", basename(path), ": ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  all_mention_vars <- grep(
    "^MEN_.*_[12]$", header, value = TRUE, perl = TRUE
  )
  substantive_mention_vars <- setdiff(
    all_mention_vars, c("MEN_SIN_MENCION_1", "MEN_SIN_MENCION_2")
  )

  desired <- unique(c(
    id_vars, career_vars, function_vars, training_vars,
    selected_mention_vars, substantive_mention_vars
  ))
  read_vars <- intersect(desired, header)
  string_vars_in_file <- intersect(character_vars, read_vars)
  column_classes <- if (length(string_vars_in_file)) {
    list(character = string_vars_in_file)
  } else {
    NULL
  }

  dt <- fread(
    path,
    sep = ";",
    select = read_vars,
    colClasses = column_classes,
    encoding = "Latin-1",
    na.strings = c("", "NA"),
    showProgress = FALSE
  )

  input_rows <- nrow(dt)
  main_position_rows <- dt[, sum(PERSONAS == 1L, na.rm = TRUE)]

  # Keep one main establishment/function record per person-year, but retain
  # every detailed ID_IFP function so function changes remain observable.
  dt <- dt[PERSONAS == 1L]
  dt[, FUNCION_PRINCIPAL := label_id_ifp(as.integer(ID_IFP))]

  # Count substantive mention flags across both reported titles. The two
  # MEN_SIN_MENCION variables are deliberately not counted as mentions.
  dt[, N_MENCIONES_TOTAL := rowSums(
    .SD == 1L, na.rm = TRUE
  ), .SDcols = substantive_mention_vars]

  for (name in setdiff(output_vars, names(dt))) {
    if (name %chin% c(character_vars, "FUNCION_PRINCIPAL")) {
      dt[, (name) := NA_character_]
    } else {
      dt[, (name) := NA_integer_]
    }
  }
  dt <- dt[, ..output_vars]

  year_values <- unique(dt$AGNO[!is.na(dt$AGNO)])
  if (length(year_values) != 1L) {
    stop(
      "Expected exactly one nonmissing AGNO in ", basename(path),
      "; found: ", paste(year_values, collapse = ", "),
      call. = FALSE
    )
  }

  duplicate_person_rows <- dt[
    !is.na(MRUN), .N, by = .(AGNO, MRUN)
  ][N > 1L, sum(N - 1L)]
  if (is.na(duplicate_person_rows)) duplicate_person_rows <- 0L

  audit <- data.table(
    AGNO = year_values,
    SOURCE_FILE = basename(path),
    N_INPUT = input_rows,
    N_PERSONAS_EQ_1 = main_position_rows,
    N_KEPT = nrow(dt),
    N_DOCENTES_AULA = dt[, sum(ID_IFP == 1L, na.rm = TRUE)],
    N_ORIENTADORES = dt[, sum(ID_IFP == 9L, na.rm = TRUE)],
    N_UNIQUE_MRUN = uniqueN(dt$MRUN, na.rm = TRUE),
    N_MISSING_MRUN = dt[, sum(is.na(MRUN) | MRUN == "")],
    N_DUPLICATE_PERSON_YEAR_ROWS = duplicate_person_rows,
    HAS_NOMBRE_SLEP = "NOMBRE_SLEP" %chin% header,
    HAS_CAREER_STAGE = all(c(
      "TRAMO_CARR_DOCENTE", "BIENIOS_CARR_DOCENTE"
    ) %chin% header)
  )

  list(data = dt, audit = audit)
}

message("Reading and cleaning ", length(csv_files), " teacher files...")
cleaned <- lapply(csv_files, function(path) {
  message("  ", basename(path))
  clean_one_file(path)
})

audit <- rbindlist(lapply(cleaned, `[[`, "audit"), fill = TRUE)
setorder(audit, AGNO)
print(audit)

combined <- rbindlist(lapply(cleaned, `[[`, "data"), fill = TRUE)
ever_orientador_ids <- combined[ID_IFP == 9L, unique(MRUN)]
ever_teacher_ids <- combined[ID_IFP == 1L, unique(MRUN)]
combined[, EVER_ORIENTADOR := as.integer(MRUN %chin% ever_orientador_ids)]
combined[, EVER_TEACHER := as.integer(MRUN %chin% ever_teacher_ids)]
setcolorder(
  combined,
  c(
    "AGNO", "RBD", "NOMBRE_SLEP", "MRUN",
    "EVER_TEACHER", "EVER_ORIENTADOR"
  )
)
function_counts <- combined[, .N, by = .(
  AGNO, ID_IFP, FUNCION_PRINCIPAL
)][order(AGNO, ID_IFP)]

if (dry_run) {
  message(
    "Dry run complete: ", format(nrow(combined), big.mark = ","),
    " cleaned person-year rows; no files written."
  )
} else {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cleaned_path <- file.path(
    output_dir,
    sprintf(
      "docentes_educacion_%d_%d_clean.csv.gz",
      min(combined$AGNO, na.rm = TRUE),
      max(combined$AGNO, na.rm = TRUE)
    )
  )
  audit_path <- file.path(output_dir, "docentes_educacion_cleaning_audit.csv")
  function_counts_path <- file.path(
    output_dir, "docentes_educacion_main_function_counts.csv"
  )

  fwrite(combined, cleaned_path, compress = "gzip", na = "")
  fwrite(audit, audit_path)
  fwrite(function_counts, function_counts_path)

  message("Wrote cleaned data: ", cleaned_path)
  message("Wrote audit table: ", audit_path)
  message("Wrote main-function counts: ", function_counts_path)
}
