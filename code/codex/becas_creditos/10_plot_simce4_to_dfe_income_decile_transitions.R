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

# The destination is each student's last observed nonmissing DFE decile, 2020--2025.
latest_dfe <- assignments[order(mrun_key, -anio_beneficio)][, .SD[1L], by = mrun_key]
comparison <- merge(universe[, .(mrun_key, simce4_income_decile = income_decile)],
                    latest_dfe[, .(mrun_key, anio_beneficio, dfe_income_decile = decil_dfe)],
                    by = "mrun_key")

transition <- comparison[, .N, by = .(simce4_income_decile, dfe_income_decile)]
transition <- CJ(simce4_income_decile = 1:10, dfe_income_decile = 1:10)[transition,
  on = .(simce4_income_decile, dfe_income_decile)]
transition[is.na(N), N := 0L]
transition[, row_total := sum(N), by = simce4_income_decile]
transition[, share_from_start := N / row_total]
transition[, label := percent(share_from_start, accuracy = 1)]

summary_path <- file.path(table_dir, "simce4_to_dfe_income_decile_transitions.csv")
figure_path <- file.path(figure_dir, "simce4_to_dfe_income_decile_transitions.png")
fwrite(transition, summary_path)

plot <- ggplot(transition, aes(x = factor(dfe_income_decile), y = factor(simce4_income_decile), fill = share_from_start)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = label, colour = share_from_start > 0.20), size = 3.5) +
  scale_fill_gradient(low = "#E8F1F7", high = "#0B5E8E", labels = percent_format(accuracy = 1), name = "Share of\nstarting decile") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1A1A1A"), guide = "none") +
  labs(
    title = "Transitions from grade-4 SIMCE to DFE income deciles",
    subtitle = "Rows sum to 100%: destinations for students in each raw grade-4 SIMCE decile\nDestination is the latest DFE record in the benefits assignments data",
    x = "Destination: latest DFE income decile",
    y = "Starting: raw grade-4 SIMCE income decile",
    caption = "390,233 students with both measures. DFE observations come from 2020 to 2025; the latest nonmissing DFE value is used."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank()
  )

ggsave(figure_path, plot, width = 9.5, height = 7.4, dpi = 300, bg = "white")
print(transition)
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
