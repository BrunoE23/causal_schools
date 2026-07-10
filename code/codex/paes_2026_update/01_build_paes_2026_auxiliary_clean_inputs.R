###############################################################################
# Build PAES 2026 auxiliary clean inputs
#
# This script handles small PAES-side files that complement the already-updated
# score/application inputs:
# - Oferta definitiva 2024-2026 code maps
# - PAES matricula 2025-2026 mapped to COD_SIES through same-year oferta
# - PAES 2026 socioeconomics
#
# Important: PAES matricula is not the full SIES Matricula-Ed-Superior file.
# Do not treat it as a substitute for full higher-ed enrollment coverage.
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
})

clean_dir <- file.path(data_wd, "data", "clean", "paes_2026_update")
diagnostic_dir <- file.path(repo_wd, "output", "tables", "paes_2026_update")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

oferta_paths <- list(
  `2024` = file.path(
    data_wd,
    "data/raw/2024/PAES-2024-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2024_REV.csv"
  ),
  `2025` = file.path(
    data_wd,
    "data/raw/2025/PAES-2025-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2025.csv"
  ),
  `2026` = file.path(
    data_wd,
    "data/raw/2026/PAES-2026-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2026.csv"
  )
)

matricula_paes_paths <- list(
  `2025` = file.path(
    data_wd,
    "data/raw/2025/PAES-2025-Matricula/D_MATRICULA_PAES_2025_PUB_MRUN.csv"
  ),
  `2026` = file.path(
    data_wd,
    "data/raw/2026/PAES-2026-Matricula/D_MATRICULA_PAES_2026_2026_PUB_MRUN.csv"
  )
)

socio_2026_path <- file.path(
  data_wd,
  "data/raw/2026/PAES-2026-Socioeconomicos/B_SOCIOECONOMICO_DOMICILIO_PAES_2026_PUB_MRUN.csv"
)

required_files <- c(unlist(oferta_paths), unlist(matricula_paes_paths), socio_2026_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"), call. = FALSE)
}

read_oferta <- function(year_label, path) {
  keep_cols <- c(
    "COD_CARRERA", "COD_SIES", "UNIVERSIDAD", "CARRERA", "LUGAR_IMPARTE",
    "NEM", "RANKING", "CLEC", "M1", "HSCO", "CIEN", "M2",
    "VAC_1ER", "VAC_2DO", "BEA", "SOBRECUPOS_1ER", "SOBRECUPOS_2DO",
    "PACE", "PEDAGOGIA", "PROM_MIN_POST", "POND_MIN_POST"
  )
  header <- names(fread(path, nrows = 0, encoding = "UTF-8"))
  keep <- intersect(keep_cols, header)
  dt <- fread(path, select = keep, encoding = "UTF-8")
  setnames(dt, "COD_CARRERA", "COD_CARRERA_PREF")
  dt[, year_info := as.integer(year_label)]
  dt[, COD_CARRERA_PREF := as.integer(COD_CARRERA_PREF)]
  dt[, COD_SIES := as.character(COD_SIES)]
  dt[]
}

oferta_all <- rbindlist(
  Map(read_oferta, names(oferta_paths), oferta_paths),
  use.names = TRUE,
  fill = TRUE
)
setcolorder(oferta_all, c("year_info", "COD_CARRERA_PREF", "COD_SIES"))

oferta_most_recent <- copy(oferta_all)[
  order(COD_CARRERA_PREF, year_info)
][
  ,
  .SD[.N],
  by = COD_CARRERA_PREF
]

read_paes_matricula <- function(year_label, path) {
  dt <- fread(path, encoding = "UTF-8")
  dt[, ANYO_PROCESO := as.integer(ANYO_PROCESO)]
  dt[, CODIGO_CARRERA := as.integer(CODIGO_CARRERA)]
  dt[, source_file_year := as.integer(year_label)]
  dt[]
}

paes_matricula <- rbindlist(
  Map(read_paes_matricula, names(matricula_paes_paths), matricula_paes_paths),
  use.names = TRUE,
  fill = TRUE
)

paes_matricula_mapped <- merge(
  paes_matricula,
  oferta_all[, .(
    ANYO_PROCESO = year_info,
    CODIGO_CARRERA = COD_CARRERA_PREF,
    COD_SIES
  )],
  by = c("ANYO_PROCESO", "CODIGO_CARRERA"),
  all.x = TRUE
)

socio_2026 <- fread(socio_2026_path, encoding = "UTF-8")
socio_2026[, ANYO_PROCESO := as.integer(ANYO_PROCESO)]

diagnostics <- rbindlist(
  list(
    oferta_all[
      ,
      .(
        rows = .N,
        students = NA_integer_,
        unique_program_codes = uniqueN(COD_CARRERA_PREF),
        unique_cod_sies = uniqueN(COD_SIES),
        missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
      ),
      by = .(year = year_info)
    ][, file_family := "oferta"][],
    paes_matricula_mapped[
      ,
      .(
        rows = .N,
        students = uniqueN(MRUN),
        unique_program_codes = uniqueN(CODIGO_CARRERA),
        unique_cod_sies = uniqueN(COD_SIES, na.rm = TRUE),
        missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
      ),
      by = .(year = ANYO_PROCESO)
    ][, file_family := "paes_matricula"][],
    socio_2026[
      ,
      .(
        rows = .N,
        students = uniqueN(MRUN),
        unique_program_codes = NA_integer_,
        unique_cod_sies = NA_integer_,
        missing_cod_sies = NA_integer_
      ),
      by = .(year = ANYO_PROCESO)
    ][, file_family := "paes_socioeconomics"][]
  ),
  use.names = TRUE,
  fill = TRUE
)

saveRDS(oferta_all, file.path(clean_dir, "oferta_codes_2024_2026_all.rds"))
saveRDS(oferta_most_recent, file.path(clean_dir, "oferta_codes_2024_2026_rec.rds"))
saveRDS(paes_matricula_mapped, file.path(clean_dir, "paes_matricula_2025_2026_mapped.rds"))
saveRDS(socio_2026, file.path(clean_dir, "paes_socioeconomics_2026.rds"))

# Also write top-level oferta maps with explicit 24-26 names. Older scripts use
# top-level clean paths, and keeping the old 24-25 files untouched avoids
# silently changing legacy outputs with misleading filenames.
saveRDS(oferta_all, file.path(data_wd, "data", "clean", "oferta_codes_24_26_all.rds"))
saveRDS(oferta_most_recent, file.path(data_wd, "data", "clean", "oferta_codes_24_26_rec.rds"))

fwrite(oferta_all, file.path(clean_dir, "oferta_codes_2024_2026_all.csv"))
fwrite(oferta_most_recent, file.path(clean_dir, "oferta_codes_2024_2026_rec.csv"))
fwrite(paes_matricula_mapped, file.path(clean_dir, "paes_matricula_2025_2026_mapped.csv"))
fwrite(socio_2026, file.path(clean_dir, "paes_socioeconomics_2026.csv"))
fwrite(diagnostics, file.path(diagnostic_dir, "paes_2026_auxiliary_clean_input_diagnostics.csv"))

print(diagnostics[order(file_family, year)])
