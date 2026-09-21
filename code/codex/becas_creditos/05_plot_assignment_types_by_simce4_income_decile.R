suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(ggplot2)
  library(scales)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean_dir <- file.path(data_wd, "data", "clean")
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
assignments_path <- file.path(clean_dir, "asignaciones.dta")
samples_path <- file.path(clean_dir, "samples.RData")
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

universe <- fread(universe_path, select = c("MRUN", "income_decile"), na.strings = c("", "NA"))
universe[, mrun_key := key_from_numeric(MRUN)]
assignments <- as.data.table(read_dta(assignments_path, col_select = c("mrun", "beneficio_beca_fscu")))
assignments[, `:=`(
  mrun_key = key_from_numeric(mrun),
  benefit = toupper(trimws(beneficio_beca_fscu))
)]
assignment_outcomes <- assignments[, .(
  any_asignacion = 1L,
  grat_asignacion = as.integer(any(benefit == "GRATUIDAD")),
  bnm_asignacion = as.integer(any(benefit == "BNM")),
  bbic_asignacion = as.integer(any(benefit == "BBIC"))
), by = mrun_key]

sample_env <- new.env(parent = emptyenv())
load(samples_path, envir = sample_env)
lottery_mrun_keys <- unique(key_from_numeric(as.data.table(sample_env$sample_students)$mrun))

plot_data <- merge(universe, assignment_outcomes, by = "mrun_key", all.x = TRUE)
plot_data <- plot_data[!is.na(income_decile) & income_decile %between% c(1, 10)]
for (outcome in c("any_asignacion", "grat_asignacion", "bnm_asignacion", "bbic_asignacion")) {
  set(plot_data, which(is.na(plot_data[[outcome]])), outcome, 0L)
}

outcome_labels <- c(
  any_asignacion = "Any benefit",
  grat_asignacion = "Gratuidad",
  bnm_asignacion = "BNM",
  bbic_asignacion = "BBIC"
)
long_data <- melt(
  plot_data,
  id.vars = c("mrun_key", "income_decile"),
  measure.vars = names(outcome_labels),
  variable.name = "outcome",
  value.name = "assigned"
)
long_data[, outcome := factor(outcome, levels = names(outcome_labels), labels = outcome_labels)]

summarise_by_decile <- function(data, sample_label) {
  data[, .(
    students = .N,
    assignments = sum(assigned),
    share_assigned = mean(assigned)
  ), by = .(outcome, income_decile)][, sample := sample_label][]
}
summary <- rbindlist(list(
  summarise_by_decile(long_data, "Whole grade-8 universe"),
  summarise_by_decile(long_data[mrun_key %chin% lottery_mrun_keys], "Lottery sample")
))[order(outcome, income_decile, sample)]
summary[, se := sqrt(share_assigned * (1 - share_assigned) / students)]
summary[, `:=`(
  lower = pmax(0, share_assigned - 1.96 * se),
  upper = pmin(1, share_assigned + 1.96 * se)
)]

summary_path <- file.path(table_dir, "assignment_types_by_simce4_income_decile.csv")
figure_path <- file.path(figure_dir, "assignment_types_by_simce4_income_decile.png")
fwrite(summary, summary_path)

plot <- ggplot(summary, aes(x = income_decile, y = share_assigned, colour = sample, linetype = sample, shape = sample)) +
  geom_line(linewidth = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12, linewidth = 0.4) +
  geom_point(size = 2.1) +
  facet_wrap(~outcome, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c("Whole grade-8 universe" = "#1B6CA8", "Lottery sample" = "#E17C05")) +
  scale_linetype_manual(values = c("Whole grade-8 universe" = "solid", "Lottery sample" = "dashed")) +
  scale_shape_manual(values = c("Whole grade-8 universe" = 16, "Lottery sample" = 17)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Benefit assignment by grade-4 SIMCE income decile",
    subtitle = "Whole grade-8 universe and lottery sample; assignment observed at any point from 2020 to 2025",
    x = "Raw grade-4 SIMCE household-income decile",
    y = "Ever assigned",
    caption = "BNM and BBIC are the two most common non-gratuidad benefit codes. Whiskers show 95% binomial confidence intervals."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0),
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(figure_path, plot, width = 10, height = 7.2, dpi = 300, bg = "white")
print(summary)
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
