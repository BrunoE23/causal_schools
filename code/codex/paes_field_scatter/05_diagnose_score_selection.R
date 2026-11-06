suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
base <- file.path(repo, "data", "clean", "paes_field_scatter", "broad_panel", "by_gender")
out <- file.path(base, "raw_stata_va_terciles", "score_selection")
fig <- file.path(repo, "output", "figures", "paes_field_scatter", "broad_panel", "by_gender",
                 "raw_stata_va_terciles", "score_selection")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)

# Select only the columns needed from the roughly 1 GB universe. Retain students
# without admission-test scores; otherwise the selection denominator is wrong.
d <- fread(file.path(root, "data", "clean", "univ_gr8_df.csv"),
  select = c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD", "GEN_ALU", "psu_year",
              "math_max", "leng_max", "z_sim_mat_4to", "COD_SIES_m1"),
  na.strings = c("", "NA"), showProgress = FALSE)
d <- d[cohort_gr8 %in% 2017:2020 & EDAD_ALU %between% c(12, 16) & most_time_RBD > 0]
setkey(d, mrun)
d <- d[!duplicated(mrun)]
setnames(d, "most_time_RBD", "school_rbd")
stopifnot(nrow(d) == 931547L, !anyNA(d$mrun))
reference <- fread(file.path(dirname(base), "score_standardization_by_year.csv"))
d <- merge(d, reference[, .(psu_year, raw_mean, raw_sd)], by = "psu_year", all.x = TRUE, sort = FALSE)
d[, math_score := NA_real_]
d[!is.na(psu_year) & is.finite(math_max) & math_max > 0 & is.finite(raw_sd) & raw_sd > 0,
  math_score := (math_max - raw_mean) / raw_sd]
d[, `:=`(math_observed = is.finite(math_score),
          any_exam_score = !is.na(psu_year) & ((is.finite(math_max) & math_max > 0) |
                                               (is.finite(leng_max) & leng_max > 0)),
          matriculated = !is.na(COD_SIES_m1) & !trimws(as.character(COD_SIES_m1)) %chin% c("", "NA"))]
# Predetermined achievement strata are pooled over the full broad panel before
# gender or score filtering. A separate missing-baseline category is retained.
d[, baseline_quintile := NA_integer_]
d[is.finite(z_sim_mat_4to), baseline_quintile := pmin(5L,
    as.integer(ceiling(5 * frank(z_sim_mat_4to, ties.method = "average") / .N)))]
d[, baseline_band := fifelse(is.na(baseline_quintile), "Missing", paste0("Q", baseline_quintile))]
fwrite(d[, .(sample = "Before gender filter", n_broad_panel = .N, n_math_observed = sum(math_observed),
    n_unknown_gender = sum(!GEN_ALU %in% 1:2))],
    file.path(out, "broad_panel_coverage.csv"))
d <- d[GEN_ALU %in% 1:2]
d[, gender := factor(fifelse(GEN_ALU == 1L, "Male", "Female"), levels = c("Male", "Female"))]
cached <- as.data.table(readRDS(file.path(base, "gender_scatter_input.rds")))[outcome == "high_paying_field_m1"]
check <- merge(d[math_observed == TRUE, .(mrun, gender, math_score)], cached[, .(mrun, gender, math_score)],
               by = "mrun", suffixes = c("_new", "_cached"))
stopifnot(nrow(check) == nrow(cached), nrow(check) == d[, sum(math_observed)],
    all(check$gender_new == check$gender_cached), max(abs(check$math_score_new - check$math_score_cached)) < 1e-10)

selection_summary <- function(z) {
  list(n_students = nrow(z), n_math_observed = sum(z$math_observed),
    n_math_missing = sum(!z$math_observed), math_observed_pct = 100 * mean(z$math_observed),
    any_exam_score_pct = 100 * mean(z$any_exam_score),
    n_baseline_observed = sum(is.finite(z$z_sim_mat_4to)),
    mean_baseline_math_scored = mean(z$z_sim_mat_4to[z$math_observed], na.rm = TRUE),
    mean_baseline_math_unscored = mean(z$z_sim_mat_4to[!z$math_observed], na.rm = TRUE),
    highered_enrollment_scored_pct = 100 * mean(z$matriculated[z$math_observed]),
    highered_enrollment_unscored_pct = 100 * mean(z$matriculated[!z$math_observed]))
}
overall <- d[, selection_summary(.SD), by = gender]
cohort <- d[, selection_summary(.SD), by = .(gender, cohort_gr8)]
baseline <- d[, selection_summary(.SD), by = .(gender, baseline_band)]
fwrite(overall, file.path(out, "score_observation_by_gender.csv"))
fwrite(cohort, file.path(out, "score_observation_by_gender_cohort.csv"))
fwrite(baseline, file.path(out, "score_observation_by_gender_baseline.csv"))

va <- fread(file.path(dirname(out), "school_va_group_assignments.csv"))
stopifnot(!anyDuplicated(va[, .(school_rbd, outcome)]), setequal(va$va_group, paste0("T", 1:3)))
coverage <- merge(d[, .(n_broad = .N), by = gender],
  rbindlist(lapply(unique(va$outcome), function(o) {
    d[, .(n_va_missing = sum(!school_rbd %in% va[outcome == o, school_rbd])), by = gender][, outcome := o]
  })), by = "gender")
fwrite(coverage, file.path(out, "broad_panel_va_match_coverage.csv"))
v <- merge(d, va[, .(school_rbd, outcome, va_group)], by = "school_rbd", allow.cartesian = TRUE)
stopifnot(!anyDuplicated(v[, .(mrun, outcome)]))
by_va <- v[, selection_summary(.SD), by = .(outcome, gender, va_group)]
by_va_baseline <- v[, selection_summary(.SD), by = .(outcome, gender, va_group, baseline_band)]
by_va_cohort <- v[, selection_summary(.SD), by = .(outcome, gender, va_group, cohort_gr8)]
curve_counts <- fread(file.path(dirname(out), "group_sample_counts.csv"))
ncheck <- merge(by_va, curve_counts[, .(outcome, gender, va_group, n_curve_students = n_students)],
                by = c("outcome", "gender", "va_group"))
stopifnot(nrow(ncheck) == 12L, all(ncheck$n_math_observed == ncheck$n_curve_students))
fwrite(by_va, file.path(out, "score_observation_by_gender_va_tercile.csv"))
fwrite(by_va_baseline, file.path(out, "score_observation_by_gender_va_tercile_baseline.csv"))
fwrite(by_va_cohort, file.path(out, "score_observation_by_gender_va_tercile_cohort.csv"))
fwrite(ncheck, file.path(out, "curve_sample_parity.csv"))

labels <- c(high_paying_field_m1 = "High-premium field VA", high_inst_m1 = "High-premium institution VA")
plot_data <- by_va_baseline[baseline_band != "Missing"]
plot_data[, `:=`(panel = factor(outcome, levels = names(labels), labels = unname(labels)),
                 baseline_band = factor(baseline_band, levels = paste0("Q", 1:5)),
                 va_group = factor(va_group, levels = paste0("T", 1:3)))]
p <- ggplot(plot_data, aes(baseline_band, math_observed_pct, color = va_group, group = va_group)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) + facet_grid(panel ~ gender) +
  scale_color_manual(values = c(T1 = "#BC4B37", T2 = "#79838F", T3 = "#244F78")) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(z) paste0(z, "%")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Who has an observed PAES/PDT math score?",
    subtitle = "Full broad-panel denominator, including students omitted from the score-choice curves",
    x = "Grade 4 SIMCE math quintile (common broad-panel cutoffs)", y = "Share with observed math score", color = "School VA tercile",
    caption = paste0("Missing admission scores do not necessarily mean dropout: they may reflect nonparticipation, timing, or linkage.\n",
      "Students with missing Grade 4 math are retained as a separate category in the CSVs but not displayed here.\n",
      "School groups use outcome-specific raw Stata VA, with equal school weights and common cutoffs across genders.")) +
  theme_classic(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "#F0F3F6", color = NA), strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 9), plot.margin = margin(15, 15, 12, 15))
ggsave(file.path(fig, "math_score_observation_by_baseline_va_gender.png"), p, width = 12, height = 8, dpi = 220, bg = "white")
print(overall)
print(by_va[order(outcome, gender, va_group)])
print(baseline[order(gender, baseline_band)])
