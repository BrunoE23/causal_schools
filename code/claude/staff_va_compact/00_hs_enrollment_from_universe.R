# School HS enrollment from the grade-8 universe (most_time_RBD assignment).
# Reads only cohort_gr8 and most_time_RBD from univ_gr8_df.csv (~1.2M rows).
# Output: one row per most_time_RBD with per-cohort counts for 2017-2020 and
#   HS_ENROLLED_EST = 4 * mean(students per grade-8 cohort, 2017-2020)
# i.e. approximate enrollment across grades 9-12 at any point in time.
# Run from repo root:  Rscript --vanilla code/claude/staff_va_compact/00_hs_enrollment_from_universe.R
suppressPackageStartupMessages(library(data.table))
src <- "C:/Users/brunem/Box/causal_schools/data/clean/univ_gr8_df.csv"
out_dir <- "C:/Users/brunem/Box/causal_schools/data/clean/staff_va_compact_inputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

u <- fread(src, select = c("cohort_gr8", "most_time_RBD"))
cat("rows read:", nrow(u), "\n")
u <- u[cohort_gr8 %in% 2017:2020 & !is.na(most_time_RBD) & most_time_RBD != 0]
cat("rows with most_time_RBD, cohorts 2017-2020:", nrow(u), "\n")

by_c <- dcast(u[, .N, by = .(RBD = most_time_RBD, cohort_gr8)],
              RBD ~ cohort_gr8, value.var = "N", fill = 0L)
setnames(by_c, as.character(2017:2020), paste0("N_COHORT_", 2017:2020))
by_c[, N_UNIVERSE_2017_2020 := N_COHORT_2017 + N_COHORT_2018 + N_COHORT_2019 + N_COHORT_2020]
# 4 grades x mean per-cohort count = the 4-cohort sum.
by_c[, HS_ENROLLED_EST := N_UNIVERSE_2017_2020]
stopifnot(!anyDuplicated(by_c$RBD))
fwrite(by_c, file.path(out_dir, "hs_enrollment_universe_2017_2020.csv"))
cat("schools:", nrow(by_c), " total students:", sum(by_c$N_UNIVERSE_2017_2020), "\n")
print(summary(by_c$HS_ENROLLED_EST))
