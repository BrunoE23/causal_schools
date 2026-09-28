###############################################################################
# Combine per-outcome RC VAM noreg results into the single combined CSV that
# 02_construct_eb_rc_vam_values.R actually consumes.
#
# 01_construct_rc_vam_school_values_noreg.R now writes ONE FILE PER OUTCOME
# (data/clean/rc_vam_school_values/by_outcome/rc_vam_school_values_r_noreg_
# <outcome>.csv) instead of everyone sharing one combined CSV that every
# save had to read-merge-rewrite. This script is the other half of that
# split: read whatever's actually in by_outcome/ right now and assemble the
# combined results + diagnostics CSVs.
#
# Safe and cheap to rerun anytime -- it only reads what's on disk and
# rewrites the two combined files; it never touches by_outcome/ itself, and
# it never re-runs any regression. Run it after any noreg pass that added or
# changed outcomes, before running 02_construct_eb_rc_vam_values.R.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

log_ts <- function(...) {
  message(format(Sys.time(), "[%H:%M:%S] "), ...)
  flush(stderr())
}

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }
  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
output_dir <- file.path(clean_dir, "rc_vam_school_values")
by_outcome_dir <- file.path(output_dir, "by_outcome")

output_path <- Sys.getenv(
  "RC_VAM_NOREG_R_OUTPUT_PATH",
  unset = file.path(output_dir, "rc_vam_school_values_r_noreg.csv")
)
diagnostics_path <- Sys.getenv(
  "RC_VAM_NOREG_R_DIAGNOSTICS_PATH",
  unset = file.path(output_dir, "rc_vam_school_values_diagnostics_r_noreg.csv")
)

files <- list.files(by_outcome_dir, pattern = "^rc_vam_school_values_r_noreg_.*\\.csv$", full.names = TRUE)
if (length(files) == 0) {
  stop("No per-outcome files found in ", by_outcome_dir, ". Run 01_construct_rc_vam_school_values_noreg.R first.", call. = FALSE)
}

log_ts("Found ", length(files), " per-outcome file(s) in ", by_outcome_dir, ":")
file_info <- file.info(files)
for (i in seq_along(files)) {
  log_ts("  ", basename(files[i]), " -- last modified ", format(file_info$mtime[i], "%Y-%m-%d %H:%M:%S"))
}

combined <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)

dupe_outcomes <- combined[, .N, by = .(outcome, school_rbd)][N > 1]
if (nrow(dupe_outcomes) > 0) {
  stop(
    "Found duplicate (outcome, school_rbd) rows after combining -- check for ",
    "leftover/renamed files in ", by_outcome_dir, " (e.g. an old file for an ",
    "outcome that's been renamed). First few duplicates: ",
    paste(utils::head(dupe_outcomes$outcome, 5), collapse = ", "),
    call. = FALSE
  )
}

log_ts("Writing combined results: ", output_path, " (", nrow(combined), " rows, ",
       uniqueN(combined$outcome), " outcome(s): ", paste(sort(unique(combined$outcome)), collapse = ", "), ")")
fwrite(combined, output_path)

diagnostics <- combined %>%
  group_by(outcome) %>%
  summarise(
    n_schools = n(),
    n_students_regression_total = sum(n_students_regression, na.rm = TRUE),
    n_gp_terms_candidate = first(n_gp_terms_candidate),
    n_gp_terms_used = first(n_gp_terms_used),
    sd_rc_vam = sd(controlled_value_added_centered_student, na.rm = TRUE),
    .groups = "drop"
  )

log_ts("Writing diagnostics: ", diagnostics_path)
fwrite(diagnostics, diagnostics_path)

print(diagnostics)
log_ts("Done.")
