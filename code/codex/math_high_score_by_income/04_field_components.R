# Exact four-way decomposition of the previous high-paying program indicator.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
base <- file.path(repo, "data/clean/math_high_score_by_income")
prior <- file.path(base, "premium_enrollment")
clean <- file.path(base, "field_components")
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(prior, "high_math_analysis_data.rds")
cache_hash <- unname(tools::md5sum(cache))
d <- as.data.table(readRDS(cache))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun), all(d$z_math >= 1),
          all(d$income_decile_imputed %in% 1:10), !anyNA(d$high_paying_field_m1))
manifest <- fread(file.path(base, "source_manifest.csv"), colClasses = c(modified = "character"))
master <- manifest$path[[1L]]
source_info <- file.info(master)
stopifnot(source_info$size == manifest$bytes[[1L]],
          as.character(source_info$mtime) == manifest$modified[[1L]])
keep <- c("mrun", "field_reclassified_m1", "f_science_m1", "f_law_m1", "f_eng_m1")
stopifnot(all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size,
                 modified = as.character(source_info$mtime), selected_columns = paste(keep, collapse = ";"),
                 strategy = "Read five master columns and retain cached high-math student IDs"),
       file.path(clean, "source_manifest.csv"))
message("Reading five field-classification columns...")
fields <- fread(master, select = keep, na.strings = c("", "NA"))
fields <- fields[mrun %in% d$mrun]
stopifnot(nrow(fields) == nrow(d), !anyDuplicated(fields$mrun))
d <- merge(d, fields, by = "mrun", all.x = TRUE, sort = FALSE)
flags <- c("f_science_m1", "f_law_m1", "f_eng_m1")
stopifnot(all(unlist(d[, ..flags]) %in% c(0L, 1L, NA_integer_)))
# These positive-list rules match derive_high_paying_field. Medicine+ is
# exactly H intersect Medicine: H already encodes the six allowed Medicine
# programs, and Science/Law/Engineering are disjoint from the Medicine group.
# Verify that identity for every student; do not use the full Medicine flag.
d[, `:=`(
  science = as.integer(enrolled & f_science_m1 %in% 1L),
  law = as.integer(enrolled & f_law_m1 %in% 1L),
  engineering = as.integer(enrolled & f_eng_m1 %in% 1L),
  medicine_plus = as.integer(enrolled & high_paying_field_m1 == 1L &
                               field_reclassified_m1 %chin% "Medicine")
)]
components <- c("science", "law", "engineering", "medicine_plus")
d[, n_components := rowSums(.SD), .SDcols = components]
stopifnot(!anyNA(d[, ..components]), all(d$n_components <= 1L),
          all(d$n_components == d$high_paying_field_m1),
          all(d[enrolled == FALSE, n_components] == 0L),
          all(d[field_reclassified_m1 %chin% "Medicine", science + law + engineering] == 0L))
message("All four components are mutually exclusive and reproduce the premium-field flag for every student.")
fwrite(d[, .(n_students = .N), by = .(field_reclassified_m1, high_paying_field_m1,
                                    science, law, engineering, medicine_plus)],
       file.path(clean, "component_classification_audit.csv"))
summarize <- function(x, groups) {
  ans <- x[, c(list(n_high_math = .N, n_high_program = sum(high_paying_field_m1)),
               lapply(.SD, sum)), by = groups, .SDcols = components]
  setnames(ans, components, paste0("n_", components))
  for (v in components) ans[, (paste0("share_", v)) := get(paste0("n_", v)) / n_high_math]
  setorderv(ans, groups)
  stopifnot(all(rowSums(ans[, paste0("n_", components), with = FALSE]) == ans$n_high_program))
  ans
}
counts <- summarize(d, "income_decile_imputed")
previous <- fread(file.path(prior, "premium_shares_by_income_decile.csv"))
comparison <- merge(counts[, .(income_decile_imputed, n_high_math, n_high_program)],
                    previous[, .(income_decile_imputed, previous_n = n_high_math,
                                 previous_high_program = n_high_program)],
                    by = "income_decile_imputed", all = TRUE)
stopifnot(nrow(comparison) == 10L, !anyNA(comparison),
          all(comparison$n_high_math == comparison$previous_n),
          all(comparison$n_high_program == comparison$previous_high_program))
fwrite(counts, file.path(clean, "field_shares_by_income_decile.csv"))
fwrite(summarize(d, c("cohort_gr8", "income_decile_imputed")),
       file.path(clean, "field_shares_by_cohort_income_decile.csv"))
fwrite(comparison, file.path(clean, "previous_figure_reconciliation.csv"))
stopifnot(unname(tools::md5sum(cache)) == cache_hash,
          file.info(master)$size == source_info$size, file.info(master)$mtime == source_info$mtime)
fwrite(data.table(check = c("Exact previous high-math student sample", "One-to-one master linkage",
                           "Mutually exclusive components", "Individual components sum to original premium flag",
                           "Medicine+ separate from Science/Law/Engineering", "Non-enrollee components zero",
                           "Every decile matches previous numerator and denominator", "Original inputs unchanged"),
                 passed = TRUE), file.path(clean, "verification.csv"))
print(counts)
source(file.path(repo, "code/codex/math_high_score_by_income/05_plot_field_components.R"))
