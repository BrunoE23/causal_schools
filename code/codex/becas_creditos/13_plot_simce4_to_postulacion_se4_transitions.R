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
postulaciones_path <- file.path(clean_dir, "postulaciones.dta")
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

universe <- fread(universe_path, select = c("MRUN", "income_decile"), na.strings = c("", "NA"))
universe[, mrun_key := key_from_numeric(MRUN)]
universe <- universe[!is.na(income_decile) & income_decile %between% c(1, 10)]

postulaciones <- as.data.table(read_dta(postulaciones_path, col_select = c("mrun", "anio_proceso", "decil_se4")))
postulaciones[, mrun_key := key_from_numeric(mrun)]
applicant_ids <- unique(postulaciones$mrun_key)
valid_se4 <- postulaciones[!is.na(decil_se4) & decil_se4 %between% c(1, 10)]
latest_se4 <- valid_se4[order(mrun_key, -anio_proceso)][, .SD[1L], by = mrun_key]

comparison <- merge(universe[, .(mrun_key, simce4_income_decile = income_decile)],
                    latest_se4[, .(mrun_key, decil_se4)],
                    by = "mrun_key", all.x = TRUE)
comparison[, application_outcome := fifelse(
  !mrun_key %chin% applicant_ids, "Did not apply",
  fifelse(is.na(decil_se4), "Applied; SE4 decile missing", paste0("SE4 decile ", decil_se4))
)]

outcome_levels <- c(paste0("SE4 decile ", 1:10), "Did not apply")
comparison[, application_outcome := factor(application_outcome, levels = outcome_levels)]
comparison <- comparison[!is.na(application_outcome)]
transition <- comparison[, .N, by = .(simce4_income_decile, application_outcome)]
transition <- CJ(simce4_income_decile = 1:10, application_outcome = factor(outcome_levels, levels = outcome_levels))[transition,
  on = .(simce4_income_decile, application_outcome)]
transition[is.na(N), N := 0L]
transition[, row_total := sum(N), by = simce4_income_decile]
transition[, share_from_start := N / row_total]
transition[, label := percent(share_from_start, accuracy = 1)]

summary_path <- file.path(table_dir, "simce4_to_postulacion_se4_transitions.csv")
figure_path <- file.path(figure_dir, "simce4_to_postulacion_se4_transitions.png")
fwrite(transition, summary_path)

plot <- ggplot(transition, aes(x = application_outcome, y = factor(simce4_income_decile), fill = share_from_start)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = label, colour = share_from_start > 0.20), size = 3.25) +
  scale_fill_gradient(low = "#E8F1F7", high = "#0B5E8E", labels = percent_format(accuracy = 1), name = "Share of\nstarting decile") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1A1A1A"), guide = "none") +
  scale_x_discrete(labels = c(paste0("SE4 decile ", 1:10), "Did not\napply")) +
  labs(
    title = "Benefit-application outcomes by grade-4 SIMCE income decile",
    subtitle = "Rows sum to 100%: latest SE4 decile among applicants and students who did not apply",
    x = "Application outcome",
    y = "Starting: raw grade-4 SIMCE income decile",
    caption = "All grade-8-universe students with observed raw grade-4 SIMCE income decile. Applications observed at any point from 2020 to 2025."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggsave(figure_path, plot, width = 11.5, height = 7.5, dpi = 300, bg = "white")
print(transition)
message("Wrote: ", figure_path)
message("Wrote: ", summary_path)
