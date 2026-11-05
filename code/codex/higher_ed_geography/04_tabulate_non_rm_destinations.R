suppressPackageStartupMessages(library(data.table))
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "higher_ed_geography")
students <- readRDS(file.path(out, "student_higher_ed_geography.rds"))
annual <- readRDS(file.path(out, "annual_enrollment_records.rds"))
stopifnot(!anyDuplicated(students$mrun))

# Audit simultaneous enrollment in RM and another non-home region in 2025.
home_2025 <- students[!is.na(home_region_paes) & home_region_paes != 13L &
                        !is.na(psu_year) & psu_year <= 2025L,
                      .(mrun, home_region_paes)]
campuses <- merge(unique(annual[enrollment_year == 2025L, .(mrun, campus_region)]),
                   home_2025, by = "mrun", all = FALSE)
overlaps <- campuses[, .(rm_and_other_outside = any(campus_region == 13L) &
                          any(campus_region != 13L & campus_region != home_region_paes)), by = mrun]
students <- merge(students, overlaps, by = "mrun", all.x = TRUE, sort = FALSE)
groups <- list("SAE applicants" = students[sae_applicant == TRUE],
                "At risk" = students[at_risk == TRUE])
tab <- rbindlist(lapply(names(groups), function(g) {
  rbindlist(lapply(c("m1", "2025"), function(w) {
    s <- groups[[g]][get(paste0("enrolled_", w)) == 1L]
    s[, home_region := if (w == "m1") home_region_paes_pre_entry else
        fifelse(is.na(psu_year) | psu_year > 2025L, NA_integer_, as.integer(home_region_paes))]
    s <- s[!is.na(home_region) & home_region != 13L]
    s[, `:=`(in_rm = get(paste0("in_rm_", w)),
              outside_home = get(paste0("outside_home_paes_", w)))]
    stopifnot(!anyNA(s$in_rm), !anyNA(s$outside_home), all(s$in_rm <= s$outside_home))
    n <- nrow(s)
    n_rm <- sum(s$in_rm == 1L)
    n_other <- sum(s$outside_home == 1L & s$in_rm == 0L)
    n_home <- sum(s$outside_home == 0L)
    # Any RM enrollment belongs in RM; the other-region category excludes RM.
    # The separate overlap field preserves any simultaneous third-region study.
    n_both <- if (w == "m1") 0L else sum(s$rm_and_other_outside, na.rm = TRUE)
    stopifnot(n_rm + n_other + n_home == n, n_rm + n_other == sum(s$outside_home))
    data.table(group = g, window = w, n_enrolled_non_rm_home = n,
      n_in_rm = n_rm, pct_in_rm = 100 * n_rm / n,
      n_other_non_home_outside_rm = n_other, pct_other_non_home_outside_rm = 100 * n_other / n,
      n_home_region_only = n_home, pct_home_region_only = 100 * n_home / n,
      n_rm_and_other_non_home = n_both,
      pct_outside_home = 100 * (n_rm + n_other) / n,
      pct_outside_home_students_in_rm = 100 * n_rm / (n_rm + n_other))
  }))
}))
previous <- fread(file.path(out, "enrollment_outside_home_by_paes_home_rm.csv"))[paes_home == "Outside RM"]
check <- merge(tab, previous[, .(group, window, previous_n = n_enrolled,
                                  previous_outside = n_outside_paes)], by = c("group", "window"))
stopifnot(nrow(check) == 4L, all(check$n_enrolled_non_rm_home == check$previous_n),
          all(check$n_in_rm + check$n_other_non_home_outside_rm == check$previous_outside))
fwrite(tab, file.path(out, "non_rm_paes_home_enrollment_destinations.csv"))
print(tab)
