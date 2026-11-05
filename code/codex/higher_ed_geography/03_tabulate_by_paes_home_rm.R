suppressPackageStartupMessages(library(data.table))
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "higher_ed_geography")
# Use the cached 31 MB RDS rather than rescanning the 161 MB CSV or raw SIES.
students <- readRDS(file.path(out, "student_higher_ed_geography.rds"))
stopifnot(!anyDuplicated(students$mrun))
groups <- list("SAE applicants" = students[sae_applicant == TRUE],
                "At risk" = students[at_risk == TRUE])
tab <- rbindlist(lapply(names(groups), function(g) {
  rbindlist(lapply(c("m1", "2025"), function(w) {
    s <- groups[[g]][get(paste0("enrolled_", w)) == 1L]
    # Keep the previous endpoint timing rules; stratify by PAES HOME, not campus.
    s[, stratification_region := if (w == "m1") home_region_paes_pre_entry else
        fifelse(is.na(psu_year) | psu_year > 2025L, NA_integer_, as.integer(home_region_paes))]
    n_missing_home <- sum(is.na(s$stratification_region))
    s <- s[!is.na(stratification_region)]
    s[, paes_home := fifelse(stratification_region == 13L, "RM", "Outside RM")]
    s[, `:=`(outside_sae = get(paste0("outside_home_sae_", w)),
              outside_paes = get(paste0("outside_home_paes_", w)))]
    stopifnot(!anyNA(s$outside_paes))
    ans <- s[, .(n_enrolled = .N, n_observed_sae = sum(!is.na(outside_sae)),
      n_outside_sae = sum(outside_sae == 1L, na.rm = TRUE),
      n_outside_paes = sum(outside_paes == 1L),
      pct_outside_sae = 100 * mean(outside_sae, na.rm = TRUE),
      pct_outside_paes = 100 * mean(outside_paes),
      n_common = sum(!is.na(outside_sae)),
      pct_outside_sae_common = 100 * mean(outside_sae, na.rm = TRUE),
      pct_outside_paes_common = 100 * mean(outside_paes[!is.na(outside_sae)])), by = paes_home]
    ans[, `:=`(group = g, window = w, n_enrolled_missing_paes_home = n_missing_home)]
    stopifnot(sum(ans$n_enrolled) + n_missing_home ==
                groups[[g]][get(paste0("enrolled_", w)) == 1L, .N])
    ans
  }))
}))
setcolorder(tab, c("group", "window", "paes_home", "n_enrolled", "pct_outside_sae", "pct_outside_paes"))
tab[, home_order := fifelse(paes_home == "RM", 1L, 2L)]
setorder(tab, window, group, home_order)
tab[, home_order := NULL]

# Splitting on PAES home must reconstruct the preceding PAES-denominator table.
previous <- fread(file.path(out, "enrollment_location_summary.csv"))[
  group %in% names(groups) & measure %in% c("outside_home_paes_m1", "outside_home_paes_2025")]
check <- merge(tab[, .(n = sum(n_enrolled), n_yes = sum(n_outside_paes)), by = .(group, window)],
               previous[, .(group, window, n_observed_enrolled, previous_yes = n_yes)],
               by = c("group", "window"))
stopifnot(nrow(check) == 4L, all(check$n == check$n_observed_enrolled),
          all(check$n_yes == check$previous_yes), nrow(tab) == 8L)
fwrite(tab, file.path(out, "enrollment_outside_home_by_paes_home_rm.csv"))
print(tab[, .(group, window, paes_home, n_enrolled, n_observed_sae,
               pct_outside_sae, pct_outside_paes, pct_outside_paes_common)])
