# Full broad-panel outcomes: no high-score, score-observation, or income restriction.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
prior <- file.path(repo, "data/clean/math_high_score_by_income")
clean <- file.path(repo, "data/clean/all_students_by_attended_va")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
manifest <- fread(file.path(prior, "source_manifest.csv"), colClasses = c(modified = "character"))
master <- manifest$path[1L]
source_info <- file.info(master)
stopifnot(source_info$size == manifest$bytes[1L],
          as.character(source_info$mtime) == manifest$modified[1L])
keep <- c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD", "psu_year", "math_max", "leng_max",
          "income_decile_imputed", "high_paying_field_m1", "high_inst_m1", "COD_SIES_m1",
          "log_program_income_full_clp_m1", "program_income_full_clp_m1")
stopifnot(all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size, modified = as.character(source_info$mtime),
                 selected_columns = paste(keep, collapse = ";"),
                 strategy = "Column-selective fread; keep full paper broad panel and cache for VA summaries"),
       file.path(clean, "master_source_manifest.csv"))
message("Reading 13 selected master columns for the full broad panel...")
d <- fread(master, select = keep, na.strings = c("", "NA"))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun))
n_master <- nrow(d)
d <- d[cohort_gr8 %in% 2017:2020 & is.finite(most_time_RBD) & most_time_RBD > 0 &
         is.finite(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16]
d[, `:=`(math_observed = is.finite(math_max) & math_max > 0,
         verbal_observed = is.finite(leng_max) & leng_max > 0,
         income_available = income_decile_imputed %in% 1:10)]
previous_counts <- fread(file.path(prior, "sample_counts.csv"))
stopifnot(nrow(d) == previous_counts[stage == "Paper broad panel", n],
          sum(d$math_observed) == previous_counts[stage == "Broad panel with math score", n],
          sum(d$income_available) == previous_counts[stage == "Broad panel with income decile", n])

# Retain exactly the existing score scales, defined before any outcome restrictions.
normalization_checks <- list()
for (subject in c("math", "verbal")) {
  raw <- if (subject == "math") "math_max" else "leng_max"
  obs <- paste0(subject, "_observed")
  path <- if (subject == "math") file.path(prior, "score_normalization.csv") else
    file.path(prior, "mean_scores/verbal_normalization.csv")
  norm <- fread(path)
  means <- norm[[paste0(subject, "_mean")]]
  sds <- norm[[paste0(subject, "_sd")]]
  ii <- match(d$psu_year, norm$psu_year)
  stopifnot(!anyNA(ii[d[[obs]]]))
  d[, (paste0("z_", subject)) := fifelse(get(obs), (get(raw) - means[ii]) / sds[ii], NA_real_)]
  check <- d[get(obs), .(n = .N, raw_mean = mean(get(raw)), raw_sd = sd(get(raw)),
                         z_mean = mean(get(paste0("z_", subject))),
                         z_sd = sd(get(paste0("z_", subject)))), keyby = psu_year]
  ni <- match(check$psu_year, norm$psu_year)
  check[, matches := n == norm[[paste0("n_", subject)]][ni] &
          abs(raw_mean - means[ni]) < 1e-8 & abs(raw_sd - sds[ni]) < 1e-8 &
          abs(z_mean) < 1e-10 & abs(z_sd - 1) < 1e-10]
  stopifnot(all(check$matches))
  normalization_checks[[subject]] <- check[, subject := subject]
}
fwrite(rbindlist(normalization_checks), file.path(clean, "normalization_verification.csv"))

# Use the existing first-enrollment definition and observed premium flags.
codes <- trimws(as.character(d$COD_SIES_m1))
stopifnot(!any(codes %in% "0"))
d[, enrolled := !is.na(COD_SIES_m1) & !codes %chin% c("", "NA")]
d[, valid_income := is.finite(log_program_income_full_clp_m1) &
      is.finite(program_income_full_clp_m1) & program_income_full_clp_m1 > 0]
coverage <- d[, .(n_students = .N, n_enrolled = sum(enrolled),
  n_math = sum(math_observed), n_verbal = sum(verbal_observed),
  n_without_household_income = sum(!income_available),
  n_missing_field = sum(is.na(high_paying_field_m1)), n_missing_institution = sum(is.na(high_inst_m1)),
  n_enrolled_with_income = sum(enrolled & valid_income), n_enrolled_missing_income = sum(enrolled & !valid_income))]
fwrite(coverage, file.path(clean, "broad_panel_coverage.csv"))
stopifnot(all(d$high_paying_field_m1 %in% 0:1), all(d$high_inst_m1 %in% 0:1),
          all(d[enrolled == FALSE, high_paying_field_m1] == 0L), all(d[enrolled == FALSE, high_inst_m1] == 0L),
          all(d[enrolled == TRUE, valid_income]),
          max(abs(d[enrolled == TRUE, log_program_income_full_clp_m1 - log(program_income_full_clp_m1)])) < 1e-10)
d[, `:=`(high_either = as.integer(high_paying_field_m1 == 1L | high_inst_m1 == 1L),
         high_both = as.integer(high_paying_field_m1 == 1L & high_inst_m1 == 1L))]

# The prior selected sample must be contained intact in the broader population.
previous <- as.data.table(readRDS(file.path(prior, "income_enrollment/high_math_with_income.rds")))
pi <- match(previous$mrun, d$mrun)
stopifnot(!anyNA(pi), identical(previous$z_math, d$z_math[pi]),
          identical(previous$enrolled, d$enrolled[pi]),
          identical(previous$high_either, d$high_either[pi]),
          identical(previous$high_both, d$high_both[pi]),
          isTRUE(all.equal(previous$log_program_income_full_clp_m1, d$log_program_income_full_clp_m1[pi])))
saveRDS(d, file.path(clean, "broad_panel_analysis_data.rds"))
stopifnot(file.info(master)$size == source_info$size, file.info(master)$mtime == source_info$mtime)
fwrite(data.table(check = c("Original master vintage and row counts", "Unique student IDs",
  "Original broad-panel score standardizations", "No missing premium flags",
  "Non-enrollee premium flags zero", "Income observed for all HE enrollees",
  "Stored income is natural-log CLP income", "Prior selected sample reproduced exactly",
  "Master source unchanged"), passed = TRUE), file.path(clean, "preparation_verification.csv"))
print(coverage)
