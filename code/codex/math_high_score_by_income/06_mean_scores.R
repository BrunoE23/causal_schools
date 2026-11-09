# Mean math and verbal scores in the unchanged high-math sample.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
base <- file.path(repo, "data/clean/math_high_score_by_income")
clean <- file.path(base, "mean_scores")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(base, "premium_enrollment/high_math_analysis_data.rds")
cache_hash <- unname(tools::md5sum(cache))
d <- as.data.table(readRDS(cache))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun), all(d$z_math >= 1))
manifest <- fread(file.path(base, "source_manifest.csv"), colClasses = c(modified = "character"))
master <- manifest$path[[1L]]
source_info <- file.info(master)
stopifnot(source_info$size == manifest$bytes[[1L]],
          as.character(source_info$mtime) == manifest$modified[[1L]])
keep <- c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD", "psu_year", "leng_max")
stopifnot(all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size,
                 modified = as.character(source_info$mtime), selected_columns = paste(keep, collapse = ";"),
                 strategy = "Read six columns, verify broad-panel verbal normalization, join to unchanged high-math cache"),
       file.path(clean, "source_manifest.csv"))
message("Reading six columns for the verbal-score normalization and link...")
v <- fread(master, select = keep, na.strings = c("", "NA"))
stopifnot(!anyNA(v$mrun), !anyDuplicated(v$mrun))
v <- v[cohort_gr8 %in% 2017:2020 & is.finite(most_time_RBD) & most_time_RBD > 0 &
         is.finite(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16]
v[, verbal_observed := is.finite(leng_max) & leng_max > 0]
stopifnot(all(is.finite(v[verbal_observed == TRUE, psu_year])))
# Verbal uses its own observed positive scores in the same broad panel as math,
# before income/high-math restrictions, exactly as in the paper's VA constructor.
normalization <- v[verbal_observed == TRUE, .(
  n_verbal = .N, verbal_mean = mean(leng_max), verbal_sd = sd(leng_max)
), keyby = psu_year]
stopifnot(all(normalization$n_verbal > 1L), all(normalization$verbal_sd > 0))
ref_path <- file.path(dirname(master),
  "school_rbd_observational_values/cohorts_2017_2020/score_scale_diagnostics_by_year.csv")
ref <- fread(ref_path)[outcome == "leng_max" & score_scale == "raw"]
comparison <- merge(normalization,
  ref[, .(psu_year = score_year, reference_n = n_students, reference_mean = mean, reference_sd = sd)],
  by = "psu_year", all = TRUE)
comparison[, matches := n_verbal == reference_n & abs(verbal_mean - reference_mean) < 1e-8 &
              abs(verbal_sd - reference_sd) < 1e-8]
stopifnot(!anyNA(comparison), all(comparison$matches))
fwrite(comparison, file.path(clean, "verbal_normalization_verification.csv"))
fwrite(normalization, file.path(clean, "verbal_normalization.csv"))
v <- merge(v, normalization[, .(psu_year, verbal_mean, verbal_sd)],
           by = "psu_year", all.x = TRUE, sort = FALSE)
v[, z_verbal := fifelse(verbal_observed, (leng_max - verbal_mean) / verbal_sd, NA_real_)]
stopifnot(all(v[verbal_observed == TRUE, .(
  ok = abs(mean(z_verbal)) < 1e-10 & abs(sd(z_verbal) - 1) < 1e-10
), by = psu_year]$ok))
v <- v[mrun %in% d$mrun]
stopifnot(nrow(v) == nrow(d))
d <- merge(d, v[, .(mrun, psu_year, leng_max, verbal_observed, z_verbal)],
           by = c("mrun", "psu_year"), all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(d$verbal_observed), nrow(d) == uniqueN(d$mrun))
summarize <- function(x, groups) {
  ans <- x[, .(n_high_math = .N, n_math = sum(is.finite(z_math)),
               n_verbal = sum(verbal_observed), n_missing_verbal = sum(!verbal_observed),
               mean_math = mean(z_math), mean_verbal = mean(z_verbal, na.rm = TRUE),
               mean_math_with_verbal = mean(z_math[verbal_observed])), by = groups]
  setorderv(ans, groups)
  stopifnot(all(ans$n_math == ans$n_high_math), all(ans$mean_math >= 1),
            all(ans$n_verbal > 0), all(ans$n_verbal + ans$n_missing_verbal == ans$n_high_math))
  ans
}
means <- summarize(d, "income_decile_imputed")
previous <- fread(file.path(base, "shares_by_income_decile.csv"))
check_counts <- merge(means[, .(income_decile_imputed, n_high_math)],
                     previous[, .(income_decile_imputed, previous_n = n_high_math)],
                     by = "income_decile_imputed", all = TRUE)
stopifnot(nrow(check_counts) == 10L, !anyNA(check_counts),
          all(check_counts$n_high_math == check_counts$previous_n))
fwrite(means, file.path(clean, "mean_scores_by_income_decile.csv"))
fwrite(summarize(d, c("cohort_gr8", "income_decile_imputed")),
       file.path(clean, "mean_scores_by_cohort_income_decile.csv"))
fwrite(check_counts, file.path(clean, "sample_verification.csv"))
saveRDS(d, file.path(clean, "high_math_with_verbal.rds"))
stopifnot(unname(tools::md5sum(cache)) == cache_hash,
          file.info(master)$size == source_info$size, file.info(master)$mtime == source_info$mtime)
fwrite(data.table(check = c("Previous selected sample unchanged", "Unique student/admission-year join",
                           "Verbal normalization matches paper", "Broad-panel verbal z-score mean 0 and SD 1",
                           "All selected math scores at least 1", "Every decile count matches previous figures",
                           "Verbal missingness explicitly accounted for", "Prior cache and master unchanged"),
                 passed = TRUE), file.path(clean, "verification.csv"))
print(means)
source(file.path(repo, "code/codex/math_high_score_by_income/07_plot_mean_scores.R"))
