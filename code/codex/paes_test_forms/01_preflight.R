suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
root <- normalizePath('.', winslash = '/')
raw_root <- file.path(Sys.getenv('CAUSAL_SCHOOLS_DATA_WD', 'C:/Users/brunem/Box/causal_schools'), 'data/raw')
out <- file.path(root, 'data/clean/paes_test_forms')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
counts <- manifests <- columns <- list()
for (year in 2023:2026) {
  files <- list.files(file.path(raw_root, year), pattern = '^A_INSCRITOS.*[.]csv$', recursive = TRUE, full.names = TRUE)
  stopifnot(length(files) == 1L)
  path <- files[[1]]
  # Inspect the header first; omit large item-response strings and name columns.
  header <- names(fread(path, nrows = 0L, showProgress = FALSE))
  keep <- unique(c('MRUN', 'ANYO_PROCESO', 'COD_SEXO', 'FECHA_NACIMIENTO', 'RBD',
    'ANYO_DE_EGRESO', 'PROMEDIO_NOTAS', 'PTJE_NEM', 'PTJE_RANKING',
    'CODIGO_COMUNA_EGRESO', 'RAMA_EDUCACIONAL',
    grep('^(FORMA_|CORRECTAS_|ERRADAS_|OMITIDAS_|(CLEC|MATE1|MATE2|HCSOC|CIEN)_.*ACTUAL$)', header, value = TRUE)))
  stopifnot(all(keep %in% header))
  message(year, ': ', round(file.info(path)$size/1e6), ' MB source; ', length(keep), ' selected columns')
  d <- fread(path, select = keep, showProgress = FALSE)
  stopifnot(!anyDuplicated(d$MRUN), all(d$ANYO_PROCESO == year))
  saveRDS(d, file.path(out, paste0('selected_', year, '.rds')), compress = FALSE)
  manifests[[as.character(year)]] <- data.table(year, path, source_bytes = file.info(path)$size,
    modified = as.character(file.info(path)$mtime), rows = nrow(d), selected_columns = length(keep),
    selected_object_bytes = as.numeric(object.size(d)))
  columns[[as.character(year)]] <- data.table(year, column = header, selected = header %in% keep)
  counts[[as.character(year)]] <- rbindlist(lapply(grep('^FORMA_', keep, value = TRUE), function(v) {
    z <- d[, .(n = .N, current_graduates = sum(ANYO_DE_EGRESO == year-1L, na.rm = TRUE)), by = v]
    setnames(z, v, 'form'); z[, `:=`(year = year, variable = v)]; z
  }))
  rm(d); gc()
}
fwrite(rbindlist(manifests), file.path(out, 'source_manifest.csv'))
fwrite(rbindlist(columns), file.path(out, 'source_columns.csv'))
fwrite(rbindlist(counts), file.path(out, 'form_counts.csv'))
print(rbindlist(counts)[form > 0])
