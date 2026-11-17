# First-enrollment premium outcomes conditional on the existing high-math group.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
prior <- file.path(repo, "data/clean/math_high_score_by_income")
clean <- file.path(prior, "premium_enrollment")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
manifest <- fread(file.path(prior, "source_manifest.csv"), colClasses = c(modified = "character"))
master <- manifest$path[[1L]]
source_info <- file.info(master)
# Preserve the previous figure's input vintage as well as its score cutoff.
stopifnot(source_info$size == manifest$bytes[[1L]],
          as.character(source_info$mtime) == manifest$modified[[1L]])
normalization <- fread(file.path(prior, "score_normalization.csv"))
previous_counts <- fread(file.path(prior, "shares_by_income_decile.csv"))
keep <- c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD", "psu_year", "math_max",
          "income_decile_imputed", "high_paying_field_m1", "high_inst_m1", "COD_SIES_m1")
stopifnot(all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size,
                 modified = as.character(source_info$mtime),
                 selected_columns = paste(keep, collapse = ";"),
                 strategy = "Read ten columns; reuse prior figure's exact normalization and verify decile counts"),
       file.path(clean, "source_manifest.csv"))
message("Reading ten master columns for conditional enrollment shares...")
d <- fread(master, select = keep, na.strings = c("", "NA"))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun))
d <- d[cohort_gr8 %in% 2017:2020 & is.finite(most_time_RBD) & most_time_RBD > 0 &
         is.finite(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16 &
         income_decile_imputed %in% 1:10 & is.finite(math_max) & math_max > 0]
d <- merge(d, normalization[, .(psu_year, math_mean, math_sd, threshold_raw)],
           by = "psu_year", all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(d$math_mean), !anyNA(d$math_sd))
d[, z_math := (math_max - math_mean) / math_sd]
stopifnot(all((d$z_math >= 1) == (d$math_max >= d$threshold_raw)))
d <- d[z_math >= 1]
comparison <- merge(d[, .(n_high_math_current = .N), keyby = income_decile_imputed],
                    previous_counts[, .(income_decile_imputed, n_high_math_previous = n_high_math)],
                    by = "income_decile_imputed", all = TRUE)
comparison[, matches := n_high_math_current == n_high_math_previous]
fwrite(comparison, file.path(clean, "high_math_sample_verification.csv"))
stopifnot(nrow(comparison) == 10L, all(comparison$matches), !anyNA(comparison$matches))
message("The high-math group matches the previous numerator in all ten income deciles: N = ", nrow(d))
d[, enrolled := !is.na(COD_SIES_m1) & !trimws(as.character(COD_SIES_m1)) %chin% c("", "NA")]
outcomes <- c("high_paying_field_m1", "high_inst_m1")
stopifnot(all(unlist(d[, ..outcomes]) %in% c(0L, 1L, NA_integer_)))
coverage <- d[, .(n_high_math = .N, n_enrolled = sum(enrolled),
                  n_missing_field = sum(is.na(high_paying_field_m1)),
                  n_missing_institution = sum(is.na(high_inst_m1))),
              keyby = income_decile_imputed]
fwrite(coverage, file.path(clean, "outcome_coverage.csv"))
# All four proportions must retain the exact high-math denominator; stop rather
# than silently use outcome-specific denominators or recode missing outcomes.
if (anyNA(d[, ..outcomes])) stop("Missing premium outcomes in the high-math group; inspect outcome_coverage.csv.")
stopifnot(all(d[enrolled == FALSE, high_paying_field_m1] == 0L),
          all(d[enrolled == FALSE, high_inst_m1] == 0L))
d[, `:=`(high_either = as.integer(high_paying_field_m1 == 1L | high_inst_m1 == 1L),
         high_both = as.integer(high_paying_field_m1 == 1L & high_inst_m1 == 1L))]
summarize <- function(x, groups) {
  ans <- x[, .(n_high_math = .N, n_enrolled = sum(enrolled),
               n_high_program = sum(high_paying_field_m1), n_high_institution = sum(high_inst_m1),
               n_high_either = sum(high_either), n_high_both = sum(high_both)), by = groups]
  ans[, `:=`(share_high_program = n_high_program / n_high_math,
             share_high_institution = n_high_institution / n_high_math,
             share_high_either = n_high_either / n_high_math,
             share_high_both = n_high_both / n_high_math)]
  setorderv(ans, groups)
  stopifnot(all(ans$n_high_either == ans$n_high_program + ans$n_high_institution - ans$n_high_both),
            all(ans$n_high_both <= pmin(ans$n_high_program, ans$n_high_institution)),
            all(ans$n_high_either >= pmax(ans$n_high_program, ans$n_high_institution)),
            all(ans$n_high_either <= ans$n_enrolled), all(ans$n_enrolled <= ans$n_high_math))
  ans
}
by_decile <- summarize(d, "income_decile_imputed")
by_cohort <- summarize(d, c("cohort_gr8", "income_decile_imputed"))
fwrite(by_decile, file.path(clean, "premium_shares_by_income_decile.csv"))
fwrite(by_cohort, file.path(clean, "premium_shares_by_cohort_income_decile.csv"))
saveRDS(d, file.path(clean, "high_math_analysis_data.rds"))
stopifnot(file.info(master)$size == source_info$size,
          file.info(master)$mtime == source_info$mtime)
fwrite(data.table(check = c("Prior source vintage unchanged", "Unique students",
                           "Exact previous high-math count in every decile", "No missing premium outcomes",
                           "Non-enrollee outcomes are zero", "Union/intersection identities",
                           "Identical denominators for all four shares", "Source unchanged"), passed = TRUE),
       file.path(clean, "verification.csv"))
print(by_decile)
source(file.path(repo, "code/codex/math_high_score_by_income/03_plot_premium_enrollment.R"))
