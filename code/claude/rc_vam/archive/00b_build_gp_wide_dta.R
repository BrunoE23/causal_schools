####################################
# ARCHIVED 2026-09-21 -- superseded by
# code/claude/rc_vam/00_build_rc_vam_analysis_sample.R, which consolidates
# this script's wide g(p_i) pivot AND 00_export_probability_controls_all_
# timely_sae.R's probs/roster/support-threshold construction AND the rich
# X/outcomes construction that used to live in Steps 1-3 of
# 01_construct_rc_vam_school_values.do into one script, writing one file
# (rc_vam_stata_analysis_sample.dta) that all three Stata regression
# scripts (ridge/lasso/noreg) load directly. See setup_md_codex/
# project_memory/decisions/
# 2026-09-21-consolidate-rc-vam-data-prep-into-one-r-script.md.
#
# Kept here for provenance only -- do not run as part of the current
# pipeline.
####################################

####################################
# Build the wide g(p_i) probability-control matrix OUTSIDE Stata and write it
# straight to a .dta file, so 01_construct_rc_vam_school_values.do can just
# `use` it instead of running `reshape`/`greshape` itself.
#
# WHY: Stata's `reshape wide` (and even gtools' faster `greshape`) still has
# to build ~2 x (num supported schools) columns from scratch inside Stata.
# data.table::dcast() is a purpose-built, C-level pivot specifically
# optimized for exactly this "many output columns" case -- it is typically
# much faster than either Stata option for a reshape this wide. This script
# does the pivot once here, in R, and hands Stata a ready-made binary file.
#
# INPUT: the long/sparse export from 00_export_probability_controls_all_timely_sae.R
#   (probs_long_k{N}_all_timely_sae.csv, probs_roster_k{N}_all_timely_sae.csv)
#   -- run that script first if these don't exist yet.
# OUTPUT: probs_columns_wide_k{N}_all_timely_sae.dta in the same DA_probs
#   folder. 01_construct_rc_vam_school_values.do checks for this file and
#   loads it directly if present, skipping its own reshape step entirely.
####################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

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

support_k <- Sys.getenv("RC_VAM_MIN_SUPPORT", unset = "100")
output_stub <- paste0("k", support_k, "_all_timely_sae")

probs_dir <- file.path(data_wd, "data", "clean", "DA_probs")
long_csv_path <- file.path(probs_dir, paste0("probs_long_", output_stub, ".csv"))
roster_path <- file.path(probs_dir, paste0("probs_roster_", output_stub, ".csv"))
wide_dta_path <- file.path(probs_dir, paste0("probs_columns_wide_", output_stub, ".dta"))

if (!file.exists(long_csv_path) || !file.exists(roster_path)) {
  stop(
    "Missing long/roster files: ", long_csv_path, " / ", roster_path,
    ". Run 00_export_probability_controls_all_timely_sae.R first",
    " (with RC_VAM_MIN_SUPPORT = ", support_k, ")."
  )
}

message("Reading long probability triples: ", long_csv_path)
t0 <- Sys.time()
probs_long <- fread(long_csv_path, colClasses = list(character = "rbd_prob"))
message("  -> ", nrow(probs_long), " nonzero rows, ", uniqueN(probs_long$rbd_prob),
        " distinct supported schools. (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " sec)")

message("Reading roster: ", roster_path)
roster <- fread(roster_path)

message("Pivoting wide with data.table::dcast (this is the step being timed) ...")
t0 <- Sys.time()
probs_wide <- dcast(
  probs_long,
  student_id + sae_proceso ~ rbd_prob,
  value.var = "prob_r",
  fill = 0
)
elapsed_pivot <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message("  -> dcast done in ", round(elapsed_pivot, 1), " sec. ",
        ncol(probs_wide) - 2, " probability columns.")

# Prefix school-id columns with "prob_" (Stata-safe names; dcast just uses the
# raw rbd_prob values, e.g. "3998", "unmatched").
prob_cols_raw <- setdiff(names(probs_wide), c("student_id", "sae_proceso"))
setnames(probs_wide, prob_cols_raw, paste0("prob_", prob_cols_raw))
prob_cols <- paste0("prob_", prob_cols_raw)

message("Joining onto the full roster (so students with zero retained probability still get a row) ...")
t0 <- Sys.time()
probs_wide <- merge(
  roster[, .(student_id, sae_proceso, mrun, cohort_gr8, timely_sae, rbd_treated_1R,
             any_risk, total_probability_mass, retained_probability_mass,
             n_positive_probability_options)],
  probs_wide,
  by = c("student_id", "sae_proceso"),
  all.x = TRUE
)
for (col in prob_cols) {
  probs_wide[is.na(get(col)), (col) := 0]
}
message("  -> ", nrow(probs_wide), " total rows. (",
        round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " sec)")

message("Building iszero_ columns ...")
t0 <- Sys.time()
iszero_cols <- sub("^prob_", "iszero_", prob_cols)
for (i in seq_along(prob_cols)) {
  probs_wide[, (iszero_cols[i]) := as.integer(get(prob_cols[i]) == 0)]
}
message("  -> done. (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " sec)")

message("Writing Stata .dta: ", wide_dta_path)
t0 <- Sys.time()
# haven writes variable names as-is; Stata's own `set maxvar 32000` (already
# in the .do file) covers reading a file this wide back in.
write_dta(probs_wide, wide_dta_path)
message("  -> done. (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " sec)")

message("Done.")
message("Wrote: ", wide_dta_path)
message("Total prob_* + iszero_* columns: ", length(prob_cols) + length(iszero_cols))
message("Pivot (dcast) step alone took ", round(elapsed_pivot, 1), " sec -- compare against how long Stata's reshape/greshape was taking.")
