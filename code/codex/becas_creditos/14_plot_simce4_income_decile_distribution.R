suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

universe <- fread(universe_path, select = "income_decile", na.strings = c("", "NA"))
distribution <- universe[!is.na(income_decile) & income_decile %between% c(1, 10), .N, by = income_decile][order(income_decile)]
distribution[, share := N / sum(N)]
distribution[, label := percent(share, accuracy = 0.1)]

summary_path <- file.path(table_dir, "simce4_income_decile_distribution.csv")
figure_path <- file.path(figure_dir, "simce4_income_decile_distribution.png")
fwrite(distribution, summary_path)

plot <- ggplot(distribution, aes(x = factor(income_decile), y = N)) +
  geom_col(width = 0.78, fill = "#1B6CA8") +
  geom_text(aes(label = label), vjust = -0.35, size = 4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Distribution of raw grade-4 SIMCE income deciles",
    subtitle = "Full grade-8 universe",
    x = "Raw grade-4 SIMCE household-income decile",
    y = "Students",
    caption = "Students with an observed raw grade-4 SIMCE income decile."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggsave(figure_path, plot, width = 8.5, height = 5.8, dpi = 300, bg = "white")
print(distribution)
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
