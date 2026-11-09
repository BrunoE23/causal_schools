# Descriptive math achievement by the paper's existing household-income deciles.
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
roots <- c(Sys.getenv("CAUSAL_SCHOOLS_DATA_WD"),
           "C:/Users/brunem/Box/causal_schools",
           "C:/Users/brunem/Dropbox/causal_schools")
roots <- roots[nzchar(roots) & file.exists(file.path(roots, "data/clean/univ_gr8_df.csv"))]
stopifnot(length(roots) > 0L)
master <- file.path(roots[1L], "data/clean/univ_gr8_df.csv")
reference <- file.path(roots[1L], paste0("data/clean/school_rbd_observational_values/",
                                      "cohorts_2017_2020/score_scale_diagnostics_by_year.csv"))
clean <- file.path(repo, "data/clean/math_high_score_by_income")
fig <- file.path(repo, "output/figures/math_high_score_by_income")
for (p in c(clean, fig)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
keep <- c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD", "psu_year", "math_max",
          "income_decile_imputed", "income_mid_was_imputed")
source_info <- file.info(master)
stopifnot(all(keep %in% names(fread(master, nrows = 0L))))
fwrite(data.table(path = master, bytes = source_info$size,
                 modified = as.character(source_info$mtime), selected_columns = paste(keep, collapse = ";"),
                 strategy = "Column-selective fread; restrict immediately to paper broad panel"),
       file.path(clean, "source_manifest.csv"))

message("Reading eight columns from the master...")
d <- fread(master, select = keep, na.strings = c("", "NA"))
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun))
n_master <- nrow(d)
d <- d[cohort_gr8 %in% 2017:2020 & is.finite(most_time_RBD) & most_time_RBD > 0 &
         is.finite(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16]
d[, math_observed := is.finite(math_max) & math_max > 0]
stopifnot(all(is.finite(d[math_observed == TRUE, psu_year])))

# Match prepare_score_outcomes in 01_construct_school_rbd_values.R:
# positive math scores, standardized within psu_year in the broad panel,
# before income or baseline-control restrictions. Do not normalize by decile.
normalization <- d[math_observed == TRUE, .(
  n_math = .N, math_mean = mean(math_max), math_sd = sd(math_max)
), keyby = psu_year]
stopifnot(all(normalization$n_math > 1L), all(normalization$math_sd > 0))
normalization[, threshold_raw := math_mean + math_sd]
d <- merge(d, normalization[, .(psu_year, math_mean, math_sd, threshold_raw)],
           by = "psu_year", all.x = TRUE, sort = FALSE)
d[, z_math := fifelse(math_observed, (math_max - math_mean) / math_sd, NA_real_)]
d[, high_math := math_observed & !is.na(z_math) & z_math >= 1]
stopifnot(all(d[math_observed == TRUE, .(
  ok = abs(mean(z_math)) < 1e-10 & abs(sd(z_math) - 1) < 1e-10
), by = psu_year]$ok))
stopifnot(identical(d$high_math,
                   d$math_observed & !is.na(d$threshold_raw) & d$math_max >= d$threshold_raw))

if (file.exists(reference)) {
  ref <- fread(reference)[outcome == "math_max" & score_scale == "raw"]
  comp <- merge(normalization, ref[, .(psu_year = score_year, reference_n = n_students,
                                      reference_mean = mean, reference_sd = sd)],
                by = "psu_year", all = TRUE)
  comp[, matches := !is.na(n_math) & !is.na(reference_n) & n_math == reference_n &
         abs(math_mean - reference_mean) < 1e-8 & abs(math_sd - reference_sd) < 1e-8]
  fwrite(comp, file.path(clean, "normalization_verification.csv"))
  stopifnot(all(comp$matches))
  message("Verified exact normalization against the paper's saved score diagnostics.")
}
fwrite(normalization, file.path(clean, "score_normalization.csv"))
d[, income_available := income_decile_imputed %in% 1:10]
coverage <- data.table(
  stage = c("Master", "Paper broad panel", "Broad panel with math score",
            "Broad panel with income decile", "With income decile and math score",
            "With income decile and math z >= 1"),
  n = c(n_master, nrow(d), sum(d$math_observed), sum(d$income_available),
        sum(d$income_available & d$math_observed), sum(d$income_available & d$high_math)))
fwrite(coverage, file.path(clean, "sample_counts.csv"))

summarize <- function(x, groups) {
  ans <- x[, .(n_students = .N, n_math_observed = sum(math_observed),
               n_high_math = sum(high_math), n_income_imputed = sum(income_mid_was_imputed == 1L, na.rm = TRUE)),
           by = groups]
  ans[, `:=`(share_high_among_scored = n_high_math / n_math_observed,
             share_observed_high_among_all = n_high_math / n_students,
             share_math_observed = n_math_observed / n_students)]
  setorderv(ans, groups)
  ans
}
a <- d[income_available == TRUE]
by_decile <- summarize(a, "income_decile_imputed")
by_cohort <- summarize(a, c("cohort_gr8", "income_decile_imputed"))
stopifnot(identical(as.integer(by_decile$income_decile_imputed), 1:10),
          all(by_decile$n_math_observed > 0),
          all(by_decile$n_high_math <= by_decile$n_math_observed),
          all(by_decile$n_math_observed <= by_decile$n_students),
          sum(by_decile$n_students) == nrow(a),
          sum(by_decile$n_high_math) == sum(a$high_math),
          max(abs(by_decile$share_observed_high_among_all -
                    by_decile$share_high_among_scored * by_decile$share_math_observed)) < 1e-12)
fwrite(by_decile, file.path(clean, "shares_by_income_decile.csv"))
fwrite(by_cohort, file.path(clean, "shares_by_cohort_income_decile.csv"))

plot_data <- melt(by_decile, id.vars = "income_decile_imputed",
                  measure.vars = c("share_high_among_scored", "share_observed_high_among_all"),
                  variable.name = "denominator", value.name = "share")
plot_data[, denominator := factor(denominator,
  levels = c("share_high_among_scored", "share_observed_high_among_all"),
  labels = c("Among students with a math score", "Among all students with income data"))]
plot_data[, label := sprintf("%.1f%%", 100 * share)]
plot_data[, label_vjust := fifelse(as.integer(denominator) == 1L, -1.05, 1.9)]
fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
caption <- paste0(
  "Grade-8 cohorts 2017-2020; ages 12-16; attended high school observed. N = ", fmt_n(nrow(a)),
  " with income; ", fmt_n(sum(a$math_observed)), " with math scores.\n",
  "Scores standardized within admission year in the paper's broad panel, before restricting to available income deciles.\n",
  "Income: existing grade-4 household-income deciles, including the paper's imputations. Deciles are not re-ranked.\n",
  "Both lines count observed scores at or above +1 SD. The all-student denominator includes students without a score."
)
p <- ggplot(plot_data, aes(income_decile_imputed, share, color = denominator,
                           linetype = denominator, shape = denominator)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.7) +
  geom_text(aes(label = label, vjust = label_vjust), size = 3.6, show.legend = FALSE) +
  scale_color_manual(values = c("#185B84", "#B66C22")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(breaks = 1:10, labels = c("1\nLowest", as.character(2:9), "10\nHighest"),
                     expand = expansion(mult = c(.04, .04))) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                     breaks = seq(0, 1, .1), limits = c(0, NA),
                     expand = expansion(mult = c(.04, .12))) +
  labs(title = "High math scores by household income",
       subtitle = "Fraction with an observed PSU/PAES math score at least 1 SD above the admission-year mean",
       x = "Household-income decile", y = "Fraction of students", caption = caption,
       color = NULL, linetype = NULL, shape = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E7EB", linewidth = .3),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title = element_text(face = "bold", size = 19),
        plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.25, margin = margin(t = 12)),
        legend.position = "bottom", legend.text = element_text(size = 10.5),
        legend.margin = margin(t = 7), plot.margin = margin(15, 20, 12, 15))
ggsave(file.path(fig, "math_at_least_1sd_by_income_decile.png"), p,
       width = 10, height = 6.8, dpi = 180, bg = "white")
ggsave(file.path(fig, "math_at_least_1sd_by_income_decile.pdf"), p,
       width = 10, height = 6.8, device = grDevices::cairo_pdf, bg = "white")
stopifnot(file.info(master)$size == source_info$size,
          file.info(master)$mtime == source_info$mtime)
fwrite(data.table(check = c("Unique student IDs", "Year-specific standardization",
                           "Equivalent raw and z-score thresholds", "All ten original deciles",
                           "Numerators and denominators reconcile", "Source unchanged"), passed = TRUE),
       file.path(clean, "verification.csv"))
print(coverage)
print(by_decile)
message("Saved PNG and PDF in ", fig)
