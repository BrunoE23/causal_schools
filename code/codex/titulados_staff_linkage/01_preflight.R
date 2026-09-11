suppressPackageStartupMessages(library(data.table))
source_root <- "C:/Users/brunem/Box/causal_schools/data/raw/titulados"
files <- list.files(source_root, pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
manifest <- data.table(PATH = files, BYTES = file.info(files)$size)
manifest[, YEAR := as.integer(sub(".*_Superior_([0-9]{4})_WEB.csv$", "\\1", PATH))]
manifest[, FOLDER_YEAR := as.integer(sub(".*Titulados-Ed-Superior-([0-9]{4})/.*", "\\1", PATH))]
manifest[, CANONICAL := YEAR == FOLDER_YEAR]
stopifnot(nrow(manifest[CANONICAL == TRUE]) == 19L, identical(sort(manifest[CANONICAL == TRUE, YEAR]), 2007:2025))
for (i in seq_len(nrow(manifest))) {
  path <- manifest$PATH[i]
  connection <- file(path, "rb"); bytes <- readBin(connection, "raw", 65536L); close(connection)
  # Never mistake a UTF-8 character cut by the byte budget for Latin-1.
  bytes <- bytes[seq_len(max(which(bytes == as.raw(10))))]
  utf8 <- !is.na(iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8"))
  enc <- if (utf8) "UTF-8" else "Latin-1"
  d <- fread(path, sep = ";", nrows = 1000, encoding = enc, colClasses = "character")
  stopifnot(all(c("cat_periodo", "mrun", "fecha_obtencion_titulo", "cod_inst", "nomb_inst", "nivel_global") %in% names(d)))
  message(basename(path), ": ", round(manifest$BYTES[i]/1024^2), " MB; ", enc, "; columns=", ncol(d))
  print(d[, .N, by = .(cat_periodo, nivel_global, nivel_carrera_2)])
  print(d[, .N, by = .(DOB_LENGTH = nchar(fec_nac_alu), AWARD_DATE_LENGTH = nchar(fecha_obtencion_titulo))])
}
