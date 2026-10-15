# Fixed 2024 secondary offerings. These are school offerings, not pupil tracks.
read_school_types <- function() {
  path <- list.files(file.path(data_root, "data/raw/school_directory/2024"),
    pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(path) == 1L)
  header <- names(fread(path, nrows = 0L))
  ens <- grep("^ENS_", header, value = TRUE)
  x <- fread(path, select = c("RBD", "MAT_TOTAL", "ESTADO_ESTAB", "MATRICULA", ens))
  stopifnot(!anyDuplicated(x$RBD), length(ens) > 0L)
  x[, hc_school := as.integer(rowSums(.SD == 310L, na.rm = TRUE) > 0L), .SDcols = ens]
  x[, tp_school := as.integer(rowSums(.SD == 410L | .SD == 510L | .SD == 610L |
    .SD == 710L | .SD == 810L, na.rm = TRUE) > 0L), .SDcols = ens]
  x[, artistic_school := as.integer(rowSums(.SD == 910L, na.rm = TRUE) > 0L), .SDcols = ens]
  # No artistic/HC/TP overlap exists in this source. Do not silently prioritize
  # categories if a future source changes that support.
  stopifnot(!any(x$artistic_school == 1L & (x$hc_school == 1L | x$tp_school == 1L)))
  x[, `:=`(mixed_hc_tp = as.integer(hc_school == 1L & tp_school == 1L),
    tp_only = as.integer(hc_school == 0L & tp_school == 1L))]
  x[, school_type := fcase(mixed_hc_tp == 1L, "Mixed HC/TP", tp_only == 1L, "TP only",
    hc_school == 1L, "HC only", artistic_school == 1L, "Artistic only",
    default = "Other/no regular secondary")]
  x <- x[, .(rbd = RBD, hc_school, tp_school, artistic_school, mixed_hc_tp, tp_only,
    school_type, whole_school_enrollment = MAT_TOTAL, directory_status = ESTADO_ESTAB,
    directory_has_enrollment = MATRICULA)]
  stopifnot(all(x$mixed_hc_tp + x$tp_only == x$tp_school),
    all(is.finite(x$whole_school_enrollment) & x$whole_school_enrollment >= 0))
  attr(x, "source_path") <- path
  x
}
