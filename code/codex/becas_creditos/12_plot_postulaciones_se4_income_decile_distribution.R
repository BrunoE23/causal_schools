suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(ggplot2)
  library(scales)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean_dir <- file.path(data_wd, "data", "clean")
postulaciones_path <- file.path(clean_dir, "postulaciones.dta")
figure_dir <- file.path(repo_wd, "output", "figures", "becas_creditos")
table_dir <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

postulaciones <- as.data.table(read_dta(postulaciones_path, col_select = c("mrun", "anio_proceso", "decil_se4")))
postulaciones[, mrun_key := key_from_numeric(mrun)]
postulaciones <- postulaciones[!is.na(decil_se4) & decil_se4 %between% c(1, 10)]

# Use one latest nonmissing SE4 decile from the 2020--2025 application records per student.
latest_se4 <- postulaciones[order(mrun_key, -anio_proceso)][, .SD[1L], by = mrun_key]
distribution <- latest_se4[, .N, by = decil_se4][order(decil_se4)]
distribution[, share := N / sum(N)]
distribution[, label := percent(share, accuracy = 0.1)]

summary_path <- file.path(table_dir, "postulaciones_se4_income_decile_distribution.csv")
figure_path <- file.path(figure_dir, "postulaciones_se4_income_decile_distribution.png")
fwrite(distribution, summary_path)

plot <- ggplot(distribution, aes(x = factor(decil_se4), y = N)) +
  geom_col(width = 0.78, fill = "#1B6CA8") +
  geom_text(aes(label = label), vjust = -0.35, size = 4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Distribution of SE4 income deciles in benefit applications",
    subtitle = "Latest observed nonmissing SE4 decile in applications, 2020 to 2025",
    x = "SE4 income decile",
    y = "Students",
    caption = "One latest nonmissing SE4 observation per student."
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
