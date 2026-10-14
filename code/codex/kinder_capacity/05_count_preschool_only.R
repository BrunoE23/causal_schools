# Preschool-only schools: ENS=10 and no other positive ENS teaching code.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
clean <- file.path(root, "data/clean/school_directory_preschool")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
schools <- rbindlist(lapply(2015:2025, function(year) {
  path <- list.files(file.path(data_root, "data/raw/school_directory", year), pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(path) == 1L, file.info(path)$size < 1e8)
  header <- names(fread(path, nrows = 0L))
  names_upper <- toupper(header)
  fields <- c("RBD", "NOM_RBD", "NOM_COM_RBD", "COD_DEPE", "ESTADO_ESTAB", "MATRICULA", grep("^ENS_", names_upper, value = TRUE))
  if (year == 2025L) fields <- c(fields, "MAT_TOTAL", grep("^MAT_ENS_", names_upper, value = TRUE))
  x <- fread(path, select = header[match(fields, names_upper)], showProgress = FALSE)
  setnames(x, toupper(names(x)))
  stopifnot(!anyDuplicated(x$RBD))
  ens <- grep("^ENS_", names(x), value = TRUE)
  x[, preschool := rowSums(.SD == 10L, na.rm = TRUE) > 0L, .SDcols = ens]
  x[, other_teaching := rowSums(.SD > 0L & .SD != 10L, na.rm = TRUE) > 0L, .SDcols = ens]
  x <- x[ESTADO_ESTAB == 1L & MATRICULA == 1L]
  if (year == 2025L) {
    only <- x[preschool & !other_teaching]
    stopifnot(all(only$MAT_TOTAL == only$MAT_ENS_1),
      all(rowSums(only[, .SD, .SDcols = paste0("MAT_ENS_", 2:8)]) == 0))
  }
  x[, .(rbd = RBD, year, school_name = NOM_RBD, comuna = NOM_COM_RBD, dependency = COD_DEPE,
    preschool, preschool_only = preschool & !other_teaching,
    preschool_enrollment = if (year == 2025L) MAT_ENS_1 else NA_integer_)]
}))
annual <- schools[, .(operating_preschool_schools = sum(preschool),
  preschool_only = sum(preschool_only), preschool_and_other_teaching = sum(preschool & !preschool_only),
  preschool_only_nonprivate = sum(preschool_only & dependency != 4L),
  preschool_only_private = sum(preschool_only & dependency == 4L),
  share_preschool_only = sum(preschool_only) / sum(preschool)), by = year]
stopifnot(all(annual$operating_preschool_schools == annual$preschool_only + annual$preschool_and_other_teaching),
  all(annual$preschool_only == annual$preschool_only_nonprivate + annual$preschool_only_private))
latest <- schools[year == 2025L & preschool_only == TRUE][order(school_name)]
by_dependency <- latest[, .(schools = .N, preschool_enrollment = sum(preschool_enrollment)), by = dependency][order(dependency)]
fwrite(annual, file.path(clean, "preschool_only_by_year.csv"))
fwrite(latest, file.path(clean, "preschool_only_schools_2025.csv"))
fwrite(by_dependency, file.path(clean, "preschool_only_dependency_2025.csv"))
print(annual)
print(by_dependency)
print(latest[1:5, .(rbd, school_name, comuna, dependency, preschool_enrollment)])
