# Render mean scores in the selected sample from aggregate statistics.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/math_high_score_by_income/mean_scores")
fig <- file.path(repo, "output/figures/math_high_score_by_income")
means <- fread(file.path(clean, "mean_scores_by_income_decile.csv"))
plot_data <- melt(means, id.vars = "income_decile_imputed",
                  measure.vars = c("mean_math", "mean_verbal"), variable.name = "subject", value.name = "mean_score")
plot_data[, subject := factor(subject, levels = c("mean_math", "mean_verbal"), labels = c("Math", "Verbal"))]
plot_data[, `:=`(label = sprintf("%.2f", mean_score),
                label_vjust = fifelse(as.integer(subject) == 1L, -1.05, 1.9))]
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
n_missing <- sum(means$n_missing_verbal)
coverage_note <- if (n_missing == 0L) {
  paste0("Both means use the same ", fmt_n(sum(means$n_high_math)), " students; both scores are observed for everyone.")
} else {
  paste0("Math: N = ", fmt_n(sum(means$n_math)), "; verbal: N = ", fmt_n(sum(means$n_verbal)),
         ". The ", fmt_n(n_missing), " missing verbal scores are excluded only from the verbal mean.")
}
caption <- paste0(
  "Selected sample: the same students with math z >= 1 and an income decile as in the enrollment figures, including non-enrollees.\n",
  "Each subject is standardized within admission year using observed positive scores in the paper's broad panel, before selection.\n",
  coverage_note, "\n",
  "Grade-8 cohorts 2017-2020; ages 12-16; attended high school observed. Existing household-income deciles include imputations."
)
p <- ggplot(plot_data, aes(income_decile_imputed, mean_score, color = subject, shape = subject, linetype = subject)) +
  geom_line(linewidth = .85) + geom_point(size = 2.6) +
  geom_text(aes(label = label, vjust = label_vjust), size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("#185B84", "#B66C22")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = 1:10, labels = c("1\nLowest", as.character(2:9), "10\nHighest"),
                     expand = expansion(mult = c(.04, .04))) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 5, .25),
                     labels = function(x) sprintf("%.2f", x),
                     expand = expansion(mult = c(.01, .10))) +
  labs(title = "Average math and verbal scores among high math scorers",
       subtitle = "Conditional on PSU/PAES math score at least 1 SD above the admission-year mean",
       x = "Household-income decile", y = "Mean score (admission-year SD units)",
       color = NULL, shape = NULL, linetype = NULL, caption = caption) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E7EB", linewidth = .3),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.25, margin = margin(t = 12)),
        legend.position = "bottom", legend.text = element_text(size = 11),
        legend.margin = margin(t = 7), plot.margin = margin(15, 20, 12, 15))
for (ext in c("png", "pdf")) {
  path <- file.path(fig, paste0("mean_scores_given_math_1sd_by_income_decile.", ext))
  if (ext == "pdf") {
    ggsave(path, p, width = 10, height = 7, device = grDevices::cairo_pdf, bg = "white")
  } else ggsave(path, p, width = 10, height = 7, dpi = 180, bg = "white")
}
message("Saved mean-score figure in ", fig)
