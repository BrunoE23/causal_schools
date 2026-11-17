# Plot the four high-paying field components from aggregate counts.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/math_high_score_by_income/field_components")
fig <- file.path(repo, "output/figures/math_high_score_by_income")
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
counts <- fread(file.path(clean, "field_shares_by_income_decile.csv"))
series <- c("share_science", "share_law", "share_engineering", "share_medicine_plus")
series_labels <- c("Science", "Law", "Engineering / Manufacturing / Construction", "Medicine+")
plot_data <- melt(counts, id.vars = "income_decile_imputed", measure.vars = series,
                  variable.name = "field", value.name = "share")
plot_data[, field := factor(field, levels = series, labels = series_labels)]
plot_data[, label := sprintf("%.1f%%", 100 * share)]
caption <- paste0(
  "Denominator: all students in each income decile with math z >= 1, including non-enrollees. Total N = ",
  format(sum(counts$n_high_math), big.mark = ",", trim = TRUE), ". First observed HE enrollment.\n",
  "Same score normalization, household-income deciles and 2017-2020 grade-8 cohort sample as the previous figures.\n",
  "Medicine+ comprises medicine, pharmacy, nursing, midwifery, medical technology and dentistry.\n",
  "The four mutually exclusive shares sum exactly to the previous high-paying program share in each income decile."
)
p <- ggplot(plot_data, aes(income_decile_imputed, share, color = field, shape = field, linetype = field)) +
  geom_line(linewidth = .85) + geom_point(size = 2.5) +
  geom_text(data = plot_data[income_decile_imputed == 1L], aes(label = label),
            hjust = 1.2, size = 3.6, show.legend = FALSE) +
  geom_text(data = plot_data[income_decile_imputed == 10L], aes(label = label),
            hjust = -.2, size = 3.6, show.legend = FALSE) +
  scale_color_manual(values = c("#237C60", "#855397", "#185B84", "#B66C22")) +
  scale_shape_manual(values = c(15, 18, 16, 17)) +
  scale_linetype_manual(values = c("dashed", "dotdash", "solid", "solid")) +
  scale_x_continuous(breaks = 1:10, labels = c("1\nLowest", as.character(2:9), "10\nHighest"),
                     expand = expansion(add = c(.75, .85))) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1, .05),
                     labels = function(x) paste0(round(x * 100), "%"),
                     expand = expansion(mult = c(.02, .10))) +
  labs(title = "High-paying fields among high math scorers",
       subtitle = "Conditional on PSU/PAES math score at least 1 SD above the admission-year mean",
       x = "Household-income decile", y = "Fraction of high math scorers",
       color = NULL, shape = NULL, linetype = NULL, caption = caption) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E7EB", linewidth = .3),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.25, margin = margin(t = 12)),
        legend.position = "bottom", legend.text = element_text(size = 10.5),
        legend.margin = margin(t = 7), plot.margin = margin(15, 20, 12, 15))
for (ext in c("png", "pdf")) {
  path <- file.path(fig, paste0("field_components_given_math_1sd_by_income_decile.", ext))
  if (ext == "pdf") {
    ggsave(path, p, width = 10, height = 7.3, device = grDevices::cairo_pdf, bg = "white")
  } else ggsave(path, p, width = 10, height = 7.3, dpi = 180, bg = "white")
}
message("Saved four-field figure in ", fig)
