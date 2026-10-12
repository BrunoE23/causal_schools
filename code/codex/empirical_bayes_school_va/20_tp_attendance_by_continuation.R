suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
root <- normalizePath(file.path(dirname(script), "../../.."), winslash = "/")
out <- file.path(root, "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation")
d <- as.data.table(readRDS(file.path(out, "all_applicant_expected_va.rds")))[,
  .(student_id, cohort_gr8, sample, any_risk, most_time_RBD)]
stopifnot(nrow(d) == 294198L, sum(d$any_risk) == 162633L)
s <- fread(file.path(root, "data/clean/staff_lasso_va/school_predictors.csv"),
           select = c("RBD", "school__has_tp_or_artistic"))
a <- fread(file.path(root, "data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv"))
# Exact school-level classification used in admin_type_va/01_build_table.py.
# The 2024 directory measures TP offerings, including mixed schools.
s[, tp := school__has_tp_or_artistic - as.integer(RBD %in% a$RBD)]
stopifnot(!anyDuplicated(s$RBD), all(s$tp %in% 0:1))
d <- merge(d, s[, .(most_time_RBD = RBD, tp)], by = "most_time_RBD", all.x = TRUE)
summarize <- function(x, population) {
  ans <- x[, .(n_students = .N, n_classified = sum(!is.na(tp)),
    n_missing = sum(is.na(tp)), n_tp = sum(tp, na.rm = TRUE)), by = sample]
  ans[, `:=`(population = population, tp_share_classified = n_tp / n_classified,
    coverage = n_classified / n_students, tp_share_lower = n_tp / n_students,
    tp_share_upper = (n_tp + n_missing) / n_students)]
  ans
}
result <- rbindlist(list(summarize(d, "All SAE applicants"),
                        summarize(d[any_risk == 1L], "Applicants with assignment risk")))
fwrite(result, file.path(out, "tp_attendance_by_continuation.csv"))
print(result)
