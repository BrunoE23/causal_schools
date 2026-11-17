# Conditional projected income and HE enrollment in the unchanged high-math group.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
base <- file.path(repo, "data/clean/math_high_score_by_income")
clean <- file.path(base, "income_enrollment")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(base, "premium_enrollment/high_math_analysis_data.rds")
cache_hash <- unname(tools::md5sum(cache))
d <- as.data.table(readRDS(cache))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun), all(d$z_math >= 1),
          !anyNA(d$enrolled), !any(trimws(as.character(d$COD_SIES_m1)) %in% "0"))
manifest <- fread(file.path(base, "source_manifest.csv"), colClasses = c(modified = "character"))
master <- manifest$path[[1L]]
source_info <- file.info(master)
stopifnot(source_info$size == manifest$bytes[[1L]],
          as.character(source_info$mtime) == manifest$modified[[1L]])
keep <- c("mrun", "log_program_income_full_clp_m1", "program_income_full_clp_m1", "mifuturo_fe_income_source_m1")
stopifnot(all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size,
                 modified = as.character(source_info$mtime), selected_columns = paste(keep, collapse = ";"),
                 strategy = "Read four master columns; link to the unchanged cached high-math group"),
       file.path(clean, "source_manifest.csv"))
message("Reading projected-income columns...")
inc <- fread(master, select = keep, na.strings = c("", "NA"))
inc <- inc[mrun %in% d$mrun]
stopifnot(nrow(inc) == nrow(d), !anyDuplicated(inc$mrun))
d <- merge(d, inc, by = "mrun", all.x = TRUE, sort = FALSE)
d[, valid_income := is.finite(log_program_income_full_clp_m1) &
                    is.finite(program_income_full_clp_m1) & program_income_full_clp_m1 > 0]
coverage <- d[, .(n_high_math = .N, n_enrolled = sum(enrolled),
                  n_enrolled_with_income = sum(enrolled & valid_income),
                  n_enrolled_missing_income = sum(enrolled & !valid_income)),
              keyby = income_decile_imputed]
fwrite(coverage, file.path(clean, "income_coverage.csv"))
# Keep every HE enrollee; stop rather than silently narrow this income sample.
stopifnot(all(d[enrolled == TRUE, valid_income]),
          max(abs(d[enrolled == TRUE, log_program_income_full_clp_m1 -
                      log(program_income_full_clp_m1)])) < 1e-10)
fwrite(d[enrolled == TRUE, .(n_enrolled = .N),
         keyby = .(income_decile_imputed, mifuturo_fe_income_source_m1)],
       file.path(clean, "enrolled_income_sources.csv"))
summarize <- function(x, groups) {
  ans <- x[, .(n_high_math = .N, n_enrolled = sum(enrolled),
               n_income = sum(enrolled & valid_income),
               share_enrolled = mean(enrolled),
               mean_log_income_enrolled = mean(log_program_income_full_clp_m1[enrolled]),
               mean_income_clp_enrolled = mean(program_income_full_clp_m1[enrolled]),
               geometric_mean_income_clp_enrolled = exp(mean(log_program_income_full_clp_m1[enrolled]))),
           by = groups]
  setorderv(ans, groups)
  stopifnot(all(ans$n_enrolled > 0), all(ans$n_income == ans$n_enrolled),
            all(is.finite(ans$mean_log_income_enrolled)),
            all(abs(ans$share_enrolled - ans$n_enrolled / ans$n_high_math) < 1e-12))
  ans
}
summary <- summarize(d, "income_decile_imputed")
previous <- fread(file.path(base, "premium_enrollment/premium_shares_by_income_decile.csv"))
comparison <- merge(summary[, .(income_decile_imputed, n_high_math, n_enrolled)],
                    previous[, .(income_decile_imputed, previous_n = n_high_math, previous_enrolled = n_enrolled)],
                    by = "income_decile_imputed", all = TRUE)
stopifnot(nrow(comparison) == 10L, !anyNA(comparison),
          all(comparison$n_high_math == comparison$previous_n),
          all(comparison$n_enrolled == comparison$previous_enrolled))
fwrite(summary, file.path(clean, "income_enrollment_by_income_decile.csv"))
fwrite(summarize(d, c("cohort_gr8", "income_decile_imputed")),
       file.path(clean, "income_enrollment_by_cohort_income_decile.csv"))
fwrite(comparison, file.path(clean, "sample_verification.csv"))
saveRDS(d, file.path(clean, "high_math_with_income.rds"))
stopifnot(unname(tools::md5sum(cache)) == cache_hash,
          file.info(master)$size == source_info$size, file.info(master)$mtime == source_info$mtime)
fwrite(data.table(check = c("Previous high-math sample unchanged", "Unique income join",
                           "Income observed for every HE enrollee", "Stored income is the natural log of CLP income",
                           "Income mean excludes all non-enrollees", "Enrollment share uses all selected students",
                           "Previous decile and enrollment counts match", "Original inputs unchanged"),
                 passed = TRUE), file.path(clean, "verification.csv"))
print(summary)
source(file.path(repo, "code/codex/math_high_score_by_income/09_plot_income_enrollment.R"))
