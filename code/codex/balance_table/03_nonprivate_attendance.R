# Add attended-school sector shares without rebuilding Table 2's baseline means.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
clean <- file.path(root, "data/clean/balance_table_continuation")
master_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
roster_path <- file.path(root, "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds")
sector_path <- file.path(root, "data/clean/staff_lasso_va/school_predictors.csv")
counts <- fread(file.path(clean, "counts.csv"))
roster <- as.data.table(readRDS(roster_path))[, .(mrun, cohort_gr8, sample, any_risk)]
roster[, in_paper_sae := TRUE]
stopifnot(nrow(roster) == 294198L, !anyDuplicated(roster$mrun))
# Preflighted 1.23 GB master: only four fields are read and filtered immediately.
wanted <- c("mrun", "cohort_gr8", "most_time_rbd", "edad_alu")
h <- names(fread(master_path, nrows = 0L))
selected <- h[match(wanted, tolower(h))]
stopifnot(!anyNA(selected))
message("Reading four selected master fields for the existing Table 2 samples.")
d <- fread(master_path, select = selected, na.strings = c("", "NA"), showProgress = FALSE)
setnames(d, tolower(names(d)))
d <- d[cohort_gr8 %in% 2017:2020 & !is.na(most_time_rbd) & most_time_rbd > 0 & edad_alu >= 12 & edad_alu <= 16]
setkey(d, mrun); d <- unique(d, by = "mrun")
d <- merge(d, roster, by = c("mrun", "cohort_gr8"), all.x = TRUE, sort = FALSE)
d[, `:=`(g_broad = TRUE, g_sae_app = in_paper_sae %in% TRUE, g_sae_risk = in_paper_sae %in% TRUE & any_risk == 1L,
  g_ending = sample %in% "school_ending", g_voluntary = sample %in% "voluntary")]
school <- fread(sector_path, select = c("RBD", "school__dependency_4"))
stopifnot(!anyDuplicated(school$RBD), all(school$school__dependency_4 %in% 0:1 | is.na(school$school__dependency_4)))
d <- merge(d, school, by.x = "most_time_rbd", by.y = "RBD", all.x = TRUE, sort = FALSE)
extra <- rbindlist(lapply(counts$group, function(g) {
  x <- d[get(g) == TRUE]
  data.table(group = g, students = nrow(x), sector_observed = sum(!is.na(x$school__dependency_4)),
    nonprivate_students = sum(x$school__dependency_4 == 0L, na.rm = TRUE),
    nonprivate_share = mean(x$school__dependency_4 == 0L, na.rm = TRUE))
}))
extra[, sector_coverage := sector_observed / students]
check_counts <- merge(extra, counts[, .(group, expected_students = students)], by = "group")
stopifnot(all(check_counts$students == check_counts$expected_students), all(is.finite(extra$nonprivate_share)))
fwrite(extra, file.path(clean, "nonprivate_attendance.csv"))
paths <- c(master_path, roster_path, sector_path)
fwrite(data.table(path = paths, bytes = file.info(paths)$size, modified = as.character(file.info(paths)$mtime)), file.path(clean, "attendance_input_manifest.csv"))
print(extra)
