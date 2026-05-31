####################################
# Append SIMCE 8B math heterogeneity variables to the broad regression dataset.
#
# This is a lightweight updater for an already-built univ_gr8_df.csv. The main
# durable pipeline hook is in code/universe_reg_df.R; this script avoids
# rebuilding the whole universe when only the new SIMCE 8B columns need to be
# attached.
####################################

suppressPackageStartupMessages({
  library(data.table)
})

find_data_wd <- function() {
  candidates <- c(
    Sys.getenv("CAUSAL_SCHOOLS_DATA_WD"),
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  )

  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find data_wd. Set CAUSAL_SCHOOLS_DATA_WD or update candidates.")
  }

  candidates[[1]]
}

data_wd <- find_data_wd()
clean_dir <- file.path(data_wd, "data", "clean")

univ_csv <- file.path(clean_dir, "univ_gr8_df.csv")
univ_dta <- file.path(clean_dir, "univ_gr8_df.dta")
heterogeneity_csv <- file.path(
  clean_dir,
  "simce8_heterogeneity",
  "cohort_2019_math_heterogeneity.csv"
)

write_stata_dta <- TRUE

heterogeneity_cols <- c(
  "mrun",
  "simce_year_8b",
  "ptje_mate8b_alu",
  "simce4_math_decile",
  "simce8_math_decile",
  "simce8_math_quintile",
  "simce8_vs_4to_math_decile_change",
  "simce8_math_decile_movement",
  "simce8_math_improved_gt1_decile",
  "simce8_math_within1_decile",
  "simce8_math_worsened_gt1_decile"
)

if (!file.exists(univ_csv)) {
  stop("Missing broad regression CSV: ", univ_csv)
}
if (!file.exists(heterogeneity_csv)) {
  stop(
    "Missing SIMCE 8B heterogeneity file: ",
    heterogeneity_csv,
    ". Run 01_clean_simce8_2019.R first."
  )
}

message("Reading SIMCE 8B heterogeneity columns: ", heterogeneity_csv)
heterogeneity <- fread(
  heterogeneity_csv,
  select = heterogeneity_cols,
  na.strings = c("", "NA")
)
heterogeneity <- unique(heterogeneity, by = "mrun")

message("Reading broad regression CSV: ", univ_csv)
univ <- fread(univ_csv, showProgress = TRUE, na.strings = c("", "NA"))
univ[, .row_id_for_restore := .I]

drop_cols <- intersect(setdiff(heterogeneity_cols, "mrun"), names(univ))
if (length(drop_cols) > 0) {
  message("Replacing existing SIMCE 8B columns: ", paste(drop_cols, collapse = ", "))
  univ[, (drop_cols) := NULL]
}

message("Merging heterogeneity variables by mrun.")
univ <- merge(univ, heterogeneity, by = "mrun", all.x = TRUE, sort = FALSE)
setorder(univ, .row_id_for_restore)
univ[, .row_id_for_restore := NULL]

message("Writing updated broad regression CSV: ", univ_csv)
fwrite(univ, univ_csv)

if (write_stata_dta) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    warning("Package haven is not installed; skipped Stata DTA update.")
  } else {
    message("Writing updated broad regression DTA: ", univ_dta)
    univ_dta_dt <- copy(univ)
    long_names <- grep("_missing_after_impute$", names(univ_dta_dt), value = TRUE)
    if (length(long_names) > 0) {
      setnames(
        univ_dta_dt,
        long_names,
        sub("_missing_after_impute$", "_miss_after_imp", long_names)
      )
    }
    haven::write_dta(univ_dta_dt, univ_dta)
  }
}

diagnostics <- data.table(
  diagnostic = c(
    "univ_rows",
    "simce8_math_decile_nonmissing",
    "simce8_math_quintile_nonmissing",
    "simce8_math_movement_nonmissing"
  ),
  value = c(
    nrow(univ),
    sum(!is.na(univ$simce8_math_decile)),
    sum(!is.na(univ$simce8_math_quintile)),
    sum(!is.na(univ$simce8_math_decile_movement))
  )
)

print(diagnostics)
message("Done.")
