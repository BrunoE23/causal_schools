suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
input_path <- file.path(
  repo_wd,
  "output/tables/rss_debiased_varcov/unshrunk_cross_outcome_iv_rss_validation.csv"
)
figure_dir <- file.path(repo_wd, "output/figures/rss_debiased_varcov")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

x <- fread(input_path)[from_key != to_key]
x[, `:=`(
  ci_low = direct_iv - 1.96 * direct_se,
  ci_high = direct_iv + 1.96 * direct_se,
  pair_label = paste0(from_label, " ", "\u2192", " ", to_label)
)]

# Label only the largest absolute prediction errors to preserve readability.
x[, label := ""]
x[order(-abs(difference))[seq_len(7)], label := pair_label]

agreement_correlation <- x[, cor(predicted_causal_gain, direct_iv)]
sign_agreement <- x[, mean(sign(predicted_causal_gain) == sign(direct_iv))]
axis_limit <- max(abs(c(x$predicted_causal_gain, x$ci_low, x$ci_high)), na.rm = TRUE) * 1.08

outcome_order <- c(
  "Math", "Verbal", "Exam taking", "High-premium inst.",
  "High-premium field", "Log income", "Fin. aid app."
)
x[, to_label := factor(to_label, levels = outcome_order)]

p <- ggplot(x, aes(x = predicted_causal_gain, y = direct_iv)) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "#B7B7B7") +
  geom_vline(xintercept = 0, linewidth = 0.35, color = "#B7B7B7") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.75, linetype = "dashed", color = "#3D3D3D") +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0,
    linewidth = 0.45,
    alpha = 0.55,
    color = "#666666"
  ) +
  geom_point(aes(color = to_label), size = 2.5, alpha = 0.9) +
  coord_equal(
    xlim = c(-axis_limit, axis_limit),
    ylim = c(-axis_limit, axis_limit),
    expand = FALSE,
    clip = "off"
  ) +
  scale_color_brewer(palette = "Dark2", name = "Outcome") +
  labs(
    title = "Direct IV estimates versus RSS-implied causal gains",
    x = "RSS-implied causal gain",
    y = "Direct cross-outcome IV estimate",
    subtitle = sprintf(
      "Off-diagonal comparisons: correlation = %.2f; same sign = %.0f%%",
      agreement_correlation,
      100 * sign_agreement
    ),
    caption = paste0(
      "Vertical bars are 95% confidence intervals for the direct IV estimates; ",
      "the dashed line denotes exact agreement.\n",
      "Labels mark the seven largest absolute discrepancies."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "#E5E5E5"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.caption = element_text(hjust = 0, color = "#555555", margin = margin(t = 8))
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

label_data <- x[label != ""]
if (requireNamespace("ggrepel", quietly = TRUE)) {
  p <- p + ggrepel::geom_label_repel(
    data = label_data,
    aes(label = label),
    size = 2.7,
    color = "#222222",
    fill = "white",
    label.size = 0.15,
    min.segment.length = 0,
    seed = 20260923,
    max.overlaps = Inf,
    show.legend = FALSE
  )
} else {
  p <- p + geom_text(
    data = label_data,
    aes(label = label),
    size = 2.7,
    color = "#222222",
    check_overlap = TRUE,
    nudge_y = 0.035,
    show.legend = FALSE
  )
}

png_path <- file.path(figure_dir, "unshrunk_cross_outcome_iv_rss_validation.png")
pdf_path <- file.path(figure_dir, "unshrunk_cross_outcome_iv_rss_validation.pdf")
ggsave(png_path, p, width = 8.4, height = 7.3, dpi = 300, bg = "white")
ggsave(pdf_path, p, width = 8.4, height = 7.3, bg = "white")

cat("Saved:", png_path, "\n")
cat("Saved:", pdf_path, "\n")
