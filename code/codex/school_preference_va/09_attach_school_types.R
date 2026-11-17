# Add the two school-type regressors without rebuilding submitted rankings or VA.
# Run08 first for the demand ranking, then this script before estimation.
source("code/codex/school_preference_va/common.R")
source(file.path(code_dir, "school_types.R"))
types <- read_school_types()
directory_path <- attr(types, "source_path")
fwrite(types, file.path(clean_dir, "school_type_classification.csv"))
new_columns <- c("hc_school", "mixed_hc_tp", "tp_only", "school_type")
coverage <- list()
old_estimation <- NULL
for (name in c("estimation_data.rds", "student_school_rankings.rds", "program_rankings.rds")) {
  path <- file.path(clean_dir, name)
  message("Attaching school types to ", name, " (", round(file.info(path)$size / 1e6, 1), " MB on disk)")
  x <- as.data.table(readRDS(path))
  old_names <- copy(names(x))
  old_n <- nrow(x)
  if (name == "estimation_data.rds") old_estimation <- copy(x)
  idx <- match(x$rbd, types$rbd)
  check(paste(name, "all destination schools match by RBD"), !anyNA(idx))
  if ("tp_school" %in% old_names) check(paste(name, "original anyTP coding unchanged"),
    all(x$tp_school == types$tp_school[idx]))
  for (v in new_columns) x[, (v) := types[[v]][idx]]
  check(paste(name, "same rows and original columns retained"), nrow(x) == old_n && all(old_names %in% names(x)))
  check(paste(name, "mutually exclusive and complete new controls"), all(x$mixed_hc_tp %in% 0:1) &&
    all(x$tp_only %in% 0:1) && all(x$mixed_hc_tp + x$tp_only == types$tp_school[idx]))
  if (name == "estimation_data.rds") check("all pre-existing estimation values unchanged",
    isTRUE(all.equal(x[, ..old_names], old_estimation, check.attributes = FALSE, tolerance = 0)))
  if (name != "program_rankings.rds") {
    population <- if (name == "estimation_data.rds") "Pooled estimation" else "All submitted rankings"
    a <- x[, .(schools = uniqueN(rbd), applicant_school_pairs = .N,
      rankings_listing_type = uniqueN(ranking_id)), by = school_type]
    a[, `:=`(population = population, share_schools = schools / sum(schools),
      share_pairs = applicant_school_pairs / sum(applicant_school_pairs))]
    coverage[[name]] <- a
  }
  tmp <- paste0(path, ".school_types.tmp")
  saveRDS(x, tmp)
  check(paste(name, "new dataset exported"), file.copy(tmp, path, overwrite = TRUE))
  unlink(tmp)
  rm(x); invisible(gc())
}
fwrite(rbindlist(coverage), file.path(clean_dir, "school_type_coverage.csv"))
manifest <- fread(file.path(clean_dir, "source_manifest.csv"))
manifest <- unique(rbindlist(list(manifest, manifest_rows(directory_path,
  "2024 HC/TP school-type indicators: HC310; TP410/510/610/710/810")), use.names = TRUE, fill = TRUE))
fwrite(manifest, file.path(clean_dir, "source_manifest.csv"))
save_checks("school_type_attachment_verification.csv")
print(rbindlist(coverage))
