# Two-panel descriptive figure; labels at income deciles 1, 8, and 10.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/math_high_score_by_income/income_enrollment")
fig <- file.path(repo, "output/figures/math_high_score_by_income")
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
summary <- fread(file.path(clean, "income_enrollment_by_income_decile.csv"))
metrics <- c("mean_log_income_enrolled", "share_enrolled")
metric_labels <- c("Mean log projected income\nHE enrollees only; ln(CLP/month)",
                   "Share enrolled in higher education\nAll selected high-math students")
pd <- melt(summary, id.vars = "income_decile_imputed", measure.vars = metrics,
           variable.name = "metric", value.name = "value")
pd[, metric := factor(metric, levels = metrics, labels = metric_labels)]
pd[, label := fifelse(as.integer(metric) == 1L, sprintf("%.3f", value), sprintf("%.1f%%", 100 * value))]
# Full probability scale for enrollment; income retains its natural log units.
bounds <- data.table(income_decile_imputed = c(1, 10), value = c(0, 1),
                     metric = factor(metric_labels[2L], levels = metric_labels))
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
caption <- paste0(
  "Same math z >= 1 sample, admission-year normalization and household-income deciles as the earlier figures.\n",
  "Income: mean natural log of projected monthly income at first HE enrollment; N = ", fmt_n(sum(summary$n_income)), " enrollees.\n",
  "Enrollment: students with observed HE enrollment / all ", fmt_n(sum(summary$n_high_math)), " high-math students. Non-enrollees enter only this denominator.\n",
  "Grade-8 cohorts 2017-2020; ages 12-16; attended high school observed. Values labeled at income deciles 1, 8 and 10."
)
p <- ggplot(pd, aes(income_decile_imputed, value, color = metric, group = metric)) +
  geom_blank(data = bounds, inherit.aes = FALSE, aes(income_decile_imputed, value)) +
  geom_line(linewidth = .85) + geom_point(size = 2.6) +
  geom_text(data = pd[income_decile_imputed %in% c(1L, 8L, 10L)], aes(label = label),
            vjust = -1.1, size = 3.6, show.legend = FALSE) +
  facet_wrap(~metric, nrow = 1, scales = "free_y") +
  scale_color_manual(values = c("#185B84", "#B66C22")) +
  scale_x_continuous(breaks = 1:10, labels = c("1\nLowest", as.character(2:9), "10\nHighest"),
                     expand = expansion(add = c(.6, .7))) +
  scale_y_continuous(breaks = function(x) {
    if (max(x, na.rm = TRUE) <= 2) seq(0, 1, .2) else pretty(x, n = 5)
  }, labels = function(x) {
    if (max(x, na.rm = TRUE) <= 1.05) paste0(round(x * 100), "%") else sprintf("%.2f", x)
  }, expand = expansion(mult = c(.03, .15))) +
  labs(title = "Projected income and HE enrollment among high math scorers",
       subtitle = "Conditional on PSU/PAES math score at least 1 SD above the admission-year mean",
       x = "Household-income decile", y = NULL, caption = caption) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E7EB", linewidth = .3),
        panel.spacing.x = grid::unit(1.1, "cm"),
        strip.text = element_text(face = "bold", size = 11, margin = margin(b = 13)),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(size = 11, margin = margin(b = 18)),
        plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.25, margin = margin(t = 18)),
        legend.position = "none", plot.margin = margin(15, 20, 12, 15))
for (ext in c("png", "pdf")) {
  path <- file.path(fig, paste0("income_enrollment_given_math_1sd_by_income_decile.", ext))
  if (ext == "pdf") {
    ggsave(path, p, width = 11, height = 6.7, device = grDevices::cairo_pdf, bg = "white")
  } else ggsave(path, p, width = 11, height = 6.7, dpi = 180, bg = "white")
}
message("Saved projected-income and enrollment figure in ", fig)
