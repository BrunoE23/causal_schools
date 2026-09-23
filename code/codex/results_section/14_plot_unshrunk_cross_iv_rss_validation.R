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
  ci_high = direct_iv + 1.96 * direct_se
)]

agreement_correlation <- x[, cor(predicted_causal_gain, direct_iv)]
sign_agreement <- x[, mean(sign(predicted_causal_gain) == sign(direct_iv))]
axis_limit <- max(abs(c(x$predicted_causal_gain, x$ci_low, x$ci_high)), na.rm = TRUE) * 1.08

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
  geom_point(color = "#2C7FB8", size = 2.6, alpha = 0.9) +
  coord_equal(
    xlim = c(-axis_limit, axis_limit),
    ylim = c(-axis_limit, axis_limit),
    expand = FALSE,
    clip = "off"
  ) +
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
      "the dashed line denotes exact agreement."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "#E5E5E5"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8)),
    plot.caption = element_text(hjust = 0, color = "#555555", margin = margin(t = 8))
  )

png_path <- file.path(figure_dir, "unshrunk_cross_outcome_iv_rss_validation.png")
pdf_path <- file.path(figure_dir, "unshrunk_cross_outcome_iv_rss_validation.pdf")
ggsave(png_path, p, width = 8.4, height = 7.3, dpi = 300, bg = "white")
ggsave(pdf_path, p, width = 8.4, height = 7.3, bg = "white")

cat("Saved:", png_path, "\n")
cat("Saved:", pdf_path, "\n")
