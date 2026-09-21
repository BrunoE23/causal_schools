suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean_dir <- file.path(data_wd, "data", "clean")
outcome_path <- file.path(clean_dir, "becas_creditos", "becas_creditos_outcomes.csv")
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
samples_path <- file.path(clean_dir, "samples.RData")
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

universe <- fread(universe_path, select = c("MRUN", "income_decile"), na.strings = c("", "NA"))
universe[, mrun_key := key_from_numeric(MRUN)]
outcomes <- fread(outcome_path, select = c("mrun_key", "any_postulacion"))
outcomes[, mrun_key := key_from_numeric(mrun_key)]

sample_env <- new.env(parent = emptyenv())
load(samples_path, envir = sample_env)
lottery_mrun_keys <- unique(key_from_numeric(as.data.table(sample_env$sample_students)$mrun))

plot_data <- merge(universe, outcomes, by = "mrun_key", all.x = TRUE)
plot_data <- plot_data[
  !is.na(income_decile) & income_decile %between% c(1, 10) & !is.na(any_postulacion)
]

summarise_by_decile <- function(data, sample_label) {
  data[, .(
    students = .N,
    benefit_applications = sum(any_postulacion),
    share_applied = mean(any_postulacion)
  ), by = income_decile][, sample := sample_label][]
}

summary <- rbindlist(list(
  summarise_by_decile(plot_data, "Whole grade-8 universe"),
  summarise_by_decile(plot_data[mrun_key %chin% lottery_mrun_keys], "Lottery sample")
))[order(income_decile, sample)]
summary[, se := sqrt(share_applied * (1 - share_applied) / students)]
summary[, `:=`(
  lower = pmax(0, share_applied - 1.96 * se),
  upper = pmin(1, share_applied + 1.96 * se)
)]

summary_path <- file.path(table_dir, "benefit_application_by_simce4_income_decile.csv")
figure_path <- file.path(figure_dir, "benefit_application_by_simce4_income_decile.png")
fwrite(summary, summary_path)

plot <- ggplot(summary, aes(x = income_decile, y = share_applied, colour = sample, linetype = sample, shape = sample)) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12, linewidth = 0.5) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c("Whole grade-8 universe" = "#1B6CA8", "Lottery sample" = "#E17C05")) +
  scale_linetype_manual(values = c("Whole grade-8 universe" = "solid", "Lottery sample" = "dashed")) +
  scale_shape_manual(values = c("Whole grade-8 universe" = 16, "Lottery sample" = 17)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = "Benefit application by grade-4 SIMCE income decile",
    subtitle = "Whole grade-8 universe and lottery sample\nApplication observed at any point from 2020 to 2025",
    x = "Raw grade-4 SIMCE household-income decile",
    y = "Ever applied for benefits",
    caption = "Whiskers show 95% binomial confidence intervals. The lottery sample is a subset of the whole universe."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0),
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(figure_path, plot, width = 8, height = 5.4, dpi = 300, bg = "white")
print(summary)
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
