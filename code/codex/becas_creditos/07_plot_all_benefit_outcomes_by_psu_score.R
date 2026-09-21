suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean_dir <- file.path(data_wd, "data", "clean")
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
outcomes_path <- file.path(clean_dir, "becas_creditos", "becas_creditos_outcomes.csv")
samples_path <- file.path(clean_dir, "samples.RData")
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

universe <- fread(universe_path, select = c("MRUN", "math_max", "leng_max"))
universe[, `:=`(
  mrun_key = key_from_numeric(MRUN),
  psu_average = (math_max + leng_max) / 2
)]
universe <- universe[math_max > 0 & leng_max > 0]
universe[, psu_score_bin := round(psu_average / 25) * 25]

outcomes <- fread(outcomes_path, select = c("mrun_key", "any_postulacion", "any_asignacion", "grat_asignacion"))
outcomes[, mrun_key := key_from_numeric(mrun_key)]

sample_env <- new.env(parent = emptyenv())
load(samples_path, envir = sample_env)
lottery_mrun_keys <- unique(key_from_numeric(as.data.table(sample_env$sample_students)$mrun))

plot_data <- merge(universe, outcomes, by = "mrun_key", all.x = TRUE)
outcome_vars <- c("any_postulacion", "any_asignacion", "grat_asignacion")
for (outcome in outcome_vars) set(plot_data, which(is.na(plot_data[[outcome]])), outcome, 0L)

outcome_labels <- c(
  any_postulacion = "Applied for any benefit",
  any_asignacion = "Assigned any benefit",
  grat_asignacion = "Assigned gratuidad"
)
long_data <- melt(
  plot_data,
  id.vars = c("mrun_key", "psu_score_bin"),
  measure.vars = outcome_vars,
  variable.name = "outcome",
  value.name = "outcome_value"
)
long_data[, outcome := factor(outcome, levels = names(outcome_labels), labels = outcome_labels)]

summarise_by_sample <- function(data, sample_label) {
  data[, .(
    students = .N,
    outcome_count = sum(outcome_value),
    share = mean(outcome_value)
  ), by = .(outcome, psu_score_bin)][, sample := sample_label][]
}
summary <- rbindlist(list(
  summarise_by_sample(long_data, "Whole grade-8 universe"),
  summarise_by_sample(long_data[mrun_key %chin% lottery_mrun_keys], "Lottery sample")
))[order(outcome, sample, psu_score_bin)]
summary <- summary[students >= 100]
summary[, se := sqrt(share * (1 - share) / students)]
summary[, `:=`(
  lower = pmax(0, share - 1.96 * se),
  upper = pmin(1, share + 1.96 * se)
)]

summary_path <- file.path(table_dir, "all_benefit_outcomes_by_psu_average.csv")
figure_path <- file.path(figure_dir, "all_benefit_outcomes_by_psu_average.png")
fwrite(summary, summary_path)

outcome_colors <- c(
  "Applied for any benefit" = "#4C78A8",
  "Assigned any benefit" = "#F58518",
  "Assigned gratuidad" = "#54A24B"
)
plot <- ggplot(summary, aes(x = psu_score_bin, y = share, colour = outcome, linetype = sample, shape = sample, group = interaction(outcome, sample))) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 4, linewidth = 0.35) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = outcome_colors, name = "Outcome") +
  scale_linetype_manual(values = c("Whole grade-8 universe" = "solid", "Lottery sample" = "dashed"), name = "Sample") +
  scale_shape_manual(values = c("Whole grade-8 universe" = 16, "Lottery sample" = 17), name = "Sample") +
  scale_x_continuous(breaks = seq(0, 1000, 100), limits = c(0, 1000)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = "Benefit applications and assignments by average PSU score",
    subtitle = "Colors identify outcomes; solid lines show the whole grade-8 universe and dashed lines the lottery sample",
    x = "Average math and verbal PSU score (25-point bins)",
    y = "Share of students",
    caption = "Students with positive math and verbal scores. Bins with fewer than 100 students in a sample are omitted. Observed at any point from 2020 to 2025; whiskers show 95% binomial confidence intervals."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    legend.position = "top",
    legend.box = "vertical"
  )

ggsave(figure_path, plot, width = 10, height = 7.2, dpi = 300, bg = "white")
print(summary)
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
