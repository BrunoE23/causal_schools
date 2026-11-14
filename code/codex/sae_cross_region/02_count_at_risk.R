# Cross-region counts using the paper's existing any_risk flag.
local_lib <- "C:/Users/brunem/AppData/Local/R/win-library/4.5"
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
out <- "data/clean/sae_cross_region"
roster_path <- "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds"
choices_path <- "data/clean/school_preference_va/student_school_rankings.rds"
geography_path <- "data/clean/school_directory_preschool/directory_school_year.csv"
stopifnot(all(file.info(c(roster_path, choices_path, geography_path))$size < 100e6))
u <- as.data.table(readRDS(roster_path))[, .(mrun, sae_proceso, any_risk)]
stopifnot(nrow(u) == 294198L, !anyDuplicated(u$mrun), !anyNA(u),
  all(u$any_risk %in% 0:1), sum(u$any_risk) == 162633L)
x <- as.data.table(readRDS(choices_path))[, .(mrun, sae_proceso, rbd, agregada_por_continuidad)]
stopifnot(all(x$agregada_por_continuidad == 0L), !anyDuplicated(x[, .(mrun, sae_proceso, rbd)]))
d <- fread(geography_path, select = c("rbd", "year", "region"))[year %in% 2018:2020]
setnames(d, "year", "sae_proceso")
stopifnot(!anyDuplicated(d[, .(rbd, sae_proceso)]))
x <- merge(x, d, by = c("rbd", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(x$region))
p <- unique(x[, .(mrun, sae_proceso, region)])[, .(n_regions = .N), by = .(mrun, sae_proceso)]
p <- merge(u, p, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(p) == nrow(u), !anyNA(p$n_regions))
old <- fread(file.path(out, "counts_by_sample_year.csv"))[scope == "Paper grade-9 sample" & sae_proceso %chin% as.character(2018:2020)]
check <- p[, .(multiple_regions = sum(n_regions >= 2L)), by = sae_proceso]
stopifnot(identical(check[order(sae_proceso), multiple_regions], old[order(sae_proceso), multiple_regions]))
summarize <- function(z) z[, .(applicants = .N, applicants_at_risk = sum(any_risk),
  multi_region_applicants = sum(n_regions >= 2L),
  multi_region_at_risk = sum(n_regions >= 2L & any_risk == 1L),
  percent_multi_region_at_risk = 100 * mean(any_risk[n_regions >= 2L]),
  percent_at_risk_who_are_multi_region = 100 * mean(n_regions[any_risk == 1L] >= 2L))]
result <- rbindlist(lapply(2018:2020, function(y) {
  z <- summarize(p[sae_proceso == y]); z[, sae_proceso := as.character(y)]; z
}))
pooled <- summarize(p)[, sae_proceso := "2018-2020"]
result <- rbind(result, pooled)
setcolorder(result, c("sae_proceso", setdiff(names(result), "sae_proceso")))
fwrite(result, file.path(out, "paper_multi_region_assignment_risk.csv"))
print(result)
