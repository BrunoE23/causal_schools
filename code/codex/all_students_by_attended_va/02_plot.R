# Full broad-panel figures with no high-math or income-availability restriction.
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggrepel)
  library(gridExtra)
})
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
clean <- file.path(repo, "data/clean/all_students_by_attended_va")
fig <- file.path(repo, "output/figures/all_students_by_attended_va")
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
summ <- fread(file.path(clean, "outcomes_by_attended_va_decile.csv"))
spec <- fread(file.path(clean, "va_definitions.csv"))
sample_counts <- fread(file.path(clean, "sample_counts.csv"))
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE)
base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E3E7EB", linewidth = .3),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(size = 10.5, margin = margin(b = 12)),
        plot.caption = element_text(hjust = 0, size = 8.2, lineheight = 1.2, margin = margin(t = 12)),
        plot.margin = margin(14, 20, 10, 15))
x_scale <- function() scale_x_continuous(breaks = 1:10,
  labels = c("1\nLowest", as.character(2:9), "10\nHighest"),
  expand = expansion(add = c(.85, .9)))
write_plot <- function(p, stem, width, height) {
  ggsave(file.path(fig, paste0(stem, ".png")), p, width = width, height = height, dpi = 160, bg = "white")
  ggsave(file.path(fig, paste0(stem, ".pdf")), p, width = width, height = height,
         device = grDevices::cairo_pdf, bg = "white")
}
common_note <- paste0(
  "Broad panel: grade-8 cohorts 2017-2020, ages 12-16, attended school observed. N = ", fmt_n(sample_counts$matched_n),
  "; ", fmt_n(sample_counts$excluded_n), " students without complete school VA excluded.\n",
  "Deciles contain roughly equal numbers of all matched students; schools stay intact. No score or household-income restriction. Labels: 1, 8 and 10.\n",
  "VA: pooled 2017-2020 All-sample EB-adjusted estimates; attended school = most-time high school. Bins are fixed across panels within each VA measure."
)
premium_vars <- c("share_high_program", "share_high_institution", "share_high_either", "share_high_both")
premium_labels <- c("High-income program", "High-income institution", "Either (or both)", "Both")
income_vars <- c("mean_log_income_enrolled", "share_enrolled")
income_labels <- c("Mean log projected income\nHE enrollees only; ln(CLP/month)",
                   "Share enrolled in higher education\nAll matched students")
income_bounds <- c(floor(min(summ$mean_log_income_enrolled) * 50) / 50,
                   ceiling(max(summ$mean_log_income_enrolled) * 50) / 50)
score_vars <- c("mean_math_score", "mean_verbal_score")
score_bounds <- c(floor(min(summ[, ..score_vars]) * 2) / 2, ceiling(max(summ[, ..score_vars]) * 2) / 2)
score_note <- paste0("Scores are standardized separately within admission year in the broad panel; means use observed scores only.\n",
                     "Math N = ", fmt_n(sample_counts$n_math), "; verbal N = ", fmt_n(sample_counts$n_verbal),
                     ". Missing-score students remain in all enrollment-rate denominators.")
for (k in spec$measure) {
  z <- summ[measure == k]
  title_va <- spec[measure == k, va_label]
  label_va <- sub(" va$", " VA", tolower(title_va))
  x_title <- paste0("Attended-school ", label_va, " decile")
  pd <- melt(z, id.vars = "va_decile", measure.vars = premium_vars,
             variable.name = "series", value.name = "share")
  pd[, series := factor(series, levels = premium_vars, labels = premium_labels)]
  pd[, label := sprintf("%.1f%%", 100 * share)]
  p1 <- ggplot(pd, aes(va_decile, share, color = series, shape = series, linetype = series)) +
    geom_line(linewidth = .8) + geom_point(size = 2.4) +
    geom_text_repel(data = pd[va_decile == 1L], aes(label = label), direction = "y",
                    nudge_x = -.42, hjust = 1, seed = 1, box.padding = .3, min.segment.length = 0,
                    max.overlaps = Inf, size = 3.4, show.legend = FALSE) +
    geom_text_repel(data = pd[va_decile == 8L], aes(label = label), direction = "y",
                    nudge_y = .045, seed = 8, box.padding = .35, min.segment.length = 0,
                    max.overlaps = Inf, size = 3.4, show.legend = FALSE) +
    geom_text_repel(data = pd[va_decile == 10L], aes(label = label), direction = "y",
                    nudge_x = .42, hjust = 0, seed = 10, box.padding = .3, min.segment.length = 0,
                    max.overlaps = Inf, size = 3.4, show.legend = FALSE) +
    scale_color_manual(values = c("#185B84", "#B66C22", "#237C60", "#855397")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    scale_linetype_manual(values = c("solid", "solid", "dashed", "dotdash")) +
    x_scale() + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2),
                                   labels = function(x) paste0(round(x * 100), "%"),
                                   expand = expansion(mult = c(.025, .055))) +
    labs(title = paste0("Premium enrollment by attended-school ", label_va),
         subtitle = "Full paper cohort | No restriction on entrance-exam scores or household-income availability",
         x = x_title, y = "Fraction of students", color = NULL, shape = NULL, linetype = NULL,
         caption = paste0(common_note, "\nFirst HE enrollment. Program is the existing high-paying field indicator; non-enrollees remain in all rate denominators.")) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE),
           linetype = guide_legend(nrow = 2, byrow = TRUE)) +
    base_theme + theme(legend.position = "bottom", legend.text = element_text(size = 10),
                       legend.margin = margin(t = 4))

  ip <- melt(z, id.vars = "va_decile", measure.vars = income_vars,
             variable.name = "metric", value.name = "value")
  ip[, metric := factor(metric, levels = income_vars, labels = income_labels)]
  ip[, label := fifelse(as.integer(metric) == 1L, sprintf("%.3f", value), sprintf("%.1f%%", value * 100))]
  bounds <- data.table(va_decile = rep(c(1, 10), 2), value = c(income_bounds, 0, 1),
                      metric = factor(rep(income_labels, each = 2), levels = income_labels))
  p2 <- ggplot(ip, aes(va_decile, value, color = metric, group = metric)) +
    geom_blank(data = bounds, inherit.aes = FALSE, aes(va_decile, value)) +
    geom_line(linewidth = .8) + geom_point(size = 2.4) +
    geom_text(data = ip[va_decile %in% c(1L, 8L, 10L)], aes(label = label),
              vjust = -1.05, size = 3.4, show.legend = FALSE) +
    facet_wrap(~metric, nrow = 1, scales = "free_y") +
    scale_color_manual(values = c("#185B84", "#B66C22")) + x_scale() +
    scale_y_continuous(breaks = function(x) {
      if (max(x, na.rm = TRUE) <= 2) seq(0, 1, .2) else pretty(x, n = 5)
    }, labels = function(x) {
      if (max(x, na.rm = TRUE) <= 1.05) paste0(round(x * 100), "%") else sprintf("%.2f", x)
    }, expand = expansion(mult = c(.02, .14))) +
    labs(title = paste0("Income and HE enrollment by attended-school ", label_va),
         subtitle = "Full paper cohort | No restriction on entrance-exam scores or household-income availability",
         x = x_title, y = NULL,
         caption = paste0(common_note, "\nMean log projected monthly income uses only the ",
                          fmt_n(sample_counts$n_enrolled), " HE enrollees; enrollment rates use all matched students.")) +
    base_theme + theme(legend.position = "none", panel.spacing.x = grid::unit(.9, "cm"),
                       strip.text = element_text(face = "bold", size = 10, margin = margin(b = 9)),
                       axis.text.x = element_text(size = 9.5))
  write_plot(p1, paste0(k, "_premium_enrollment"), 11, 7)
  write_plot(p2, paste0(k, "_income_enrollment"), 11, 6.5)

  sp <- melt(z, id.vars = "va_decile", measure.vars = score_vars,
             variable.name = "subject", value.name = "mean_score")
  sp[, subject := factor(subject, levels = score_vars, labels = c("Math", "Verbal"))]
  sp[, label := sprintf("%.2f", fifelse(abs(mean_score) < .005, 0, mean_score))]
  p3 <- ggplot(sp, aes(va_decile, mean_score, color = subject, shape = subject)) +
    geom_hline(yintercept = 0, linewidth = .35, color = "#929BA3", linetype = "dashed") +
    geom_line(linewidth = .8) + geom_point(size = 2.4) +
    geom_text_repel(data = sp[va_decile == 1L], aes(label = label), direction = "y",
                    nudge_x = -.42, hjust = 1, seed = 1, box.padding = .35, min.segment.length = 0,
                    max.overlaps = Inf, size = 3.4, show.legend = FALSE) +
    geom_text_repel(data = sp[va_decile == 8L], aes(label = label), direction = "y",
                    nudge_y = .1, seed = 8, box.padding = .35, min.segment.length = 0,
                    max.overlaps = Inf, size = 3.4, show.legend = FALSE) +
    geom_text_repel(data = sp[va_decile == 10L], aes(label = label), direction = "y",
                    nudge_x = .42, hjust = 0, seed = 10, box.padding = .35, min.segment.length = 0,
                    max.overlaps = Inf, size = 3.4, show.legend = FALSE) +
    scale_color_manual(values = c("#185B84", "#B66C22")) +
    scale_shape_manual(values = c(16, 17)) + x_scale() +
    scale_y_continuous(limits = score_bounds, breaks = seq(score_bounds[1L], score_bounds[2L], .5),
                       expand = expansion(mult = c(.02, .12))) +
    labs(title = paste0("Mean scores by attended-school ", label_va),
         subtitle = "Full paper cohort | Means among students with observed scores in each subject",
         x = x_title, y = "Mean score (admission-year SD units)", color = NULL, shape = NULL,
         caption = paste0(common_note, "\n", score_note)) +
    base_theme + theme(legend.position = "bottom", legend.text = element_text(size = 10))
  write_plot(p3, paste0(k, "_mean_scores"), 11, 6.5)

  compact_theme <- theme(plot.title = element_text(size = 13),
                         legend.text = element_text(size = 9),
                         legend.key.width = grid::unit(.8, "cm"),
                         axis.title = element_text(size = 10),
                         axis.text.x = element_text(size = 9))
  premium_panel <- p1 + labs(title = "A. Premium enrollment", subtitle = NULL, caption = NULL) + compact_theme
  score_panel <- p3 + labs(title = "B. Average math and verbal scores", subtitle = NULL, caption = NULL) + compact_theme
  top <- arrangeGrob(premium_panel, score_panel, ncol = 2)
  bottom <- p2 + labs(title = NULL, subtitle = NULL, caption = NULL) + compact_theme +
    facet_wrap(~metric, nrow = 1, scales = "free_y",
               labeller = as_labeller(setNames(paste0(c("C. ", "D. "), income_labels), income_labels)))
  title <- grid::grobTree(
    grid::textGrob(paste0("Outcomes by attended-school ", label_va),
                   x = .015, y = .73, hjust = 0, gp = grid::gpar(fontsize = 18, fontface = "bold")),
    grid::textGrob("All students in the paper cohort | Student-weighted VA deciles | No high-score restriction",
                   x = .015, y = .23, hjust = 0, gp = grid::gpar(fontsize = 11)))
  note <- grid::textGrob(paste0(common_note,
    "\nAll enrollment shares retain non-enrollees. Mean log projected monthly income uses only ",
    fmt_n(sample_counts$n_enrolled), " HE enrollees.\n", score_note),
    x = .015, y = .95, hjust = 0, vjust = 1, gp = grid::gpar(fontsize = 9, lineheight = 1.3))
  combined <- arrangeGrob(title, top, bottom, note, ncol = 1,
                           heights = grid::unit(c(.65, 4.8, 4.5, 1.25), "in"))
  write_plot(combined, paste0(k, "_combined"), 13.5, 11.2)
}
message("Saved separate and combined figures for all three VA dimensions in ", fig)
