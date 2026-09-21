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
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

universe <- fread(universe_path, select = c("MRUN", "income_decile"), na.strings = c("", "NA"))
universe[, mrun_key := key_from_numeric(MRUN)]
universe <- universe[!is.na(income_decile) & income_decile %between% c(1, 10)]

assignments <- as.data.table(read_dta(assignments_path, col_select = c("mrun", "anio_beneficio", "decil_dfe")))
assignments[, mrun_key := key_from_numeric(mrun)]
assignments <- assignments[!is.na(decil_dfe) & decil_dfe %between% c(1, 10)]

# Use each student's last observed DFE decile in the 2020--2025 assignment records.
latest_dfe <- assignments[order(mrun_key, -anio_beneficio)][, .SD[1L], by = mrun_key]
comparison <- merge(universe[, .(mrun_key, simce4_income_decile = income_decile)],
                    latest_dfe[, .(mrun_key, anio_beneficio, decil_dfe)],
                    by = "mrun_key")
comparison[, dfe_minus_simce4 := decil_dfe - simce4_income_decile]

histogram <- comparison[, .N, by = dfe_minus_simce4][order(dfe_minus_simce4)]
histogram[, share := N / sum(N)]
histogram[, label := percent(share, accuracy = 0.1)]

summary_path <- file.path(table_dir, "dfe_minus_simce4_income_decile_histogram.csv")
figure_path <- file.path(figure_dir, "dfe_minus_simce4_income_decile_histogram.png")
fwrite(histogram, summary_path)

plot <- ggplot(histogram, aes(x = factor(dfe_minus_simce4), y = N, fill = dfe_minus_simce4 == 0)) +
  geom_col(width = 0.82, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.35, size = 3.4) +
  scale_fill_manual(values = c(`TRUE` = "#1B6CA8", `FALSE` = "#A9C4D5")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Difference between DFE and grade-4 SIMCE income deciles",
    subtitle = "Latest observed DFE decile in benefits assignments, 2020 to 2025",
    x = "DFE decile minus raw grade-4 SIMCE income decile",
    y = "Students",
    caption = "Positive values mean the DFE decile is higher. One latest nonmissing DFE observation per student. Labels show the share of matched students."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggsave(figure_path, plot, width = 10, height = 6.5, dpi = 300, bg = "white")
print(comparison[, .(
  students = .N,
  mean_difference = mean(dfe_minus_simce4),
  share_same_decile = mean(dfe_minus_simce4 == 0)
)])
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
