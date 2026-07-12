###############################################################################
# Homogeneous Figure 2 observational school value-added correlations
#
# This produces both panels in Figure 2 from the same Stata school-value file,
# plotting math against verbal and high-premium-field VA with one shared style.
# It uses the unshrunken, student-centered school effects to match the
# orthogonal VA setup.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, ".", call. = FALSE)
  }
  candidates[[1]]
}

repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(getwd(), "C:/Users/brunem/Research/causal_schools"),
  "repository root"
)
table_dir <- file.path(repo_wd, "output", "tables", "results_section")
figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

school_values_path <- Sys.getenv(
  "MATH_HIGHPAY_SCHOOL_VALUES",
  unset = file.path(
    repo_wd,
    "output",
    "tables",
    "empirical_bayes_school_va",
    "stata_eb_school_rbd_observational_values_for_iv.csv"
  )
)
math_language_plot_path <- Sys.getenv(
  "MATH_LANGUAGE_FIGURE",
  unset = file.path(figure_dir, "math_language_va_correlation.png")
)
plot_path <- Sys.getenv(
  "MATH_HIGHPAY_FIGURE",
  unset = file.path(figure_dir, "math_highpay_va_correlation.png")
)
summary_path <- Sys.getenv(
  "MATH_HIGHPAY_SUMMARY",
  unset = file.path(table_dir, "math_highpay_va_correlation.csv")
)
figure2_summary_path <- Sys.getenv(
  "FIGURE2_CORRELATION_SUMMARY",
  unset = file.path(table_dir, "school_va_correlations.csv")
)

if (!file.exists(school_values_path)) {
  stop("Missing school-value input: ", school_values_path, call. = FALSE)
}
message("Reading Stata school values: ", school_values_path)
values <- fread(
  school_values_path,
  select = c(
    "school_rbd",
    "analysis_sample",
    "outcome",
    "n_students_regression",
    "controlled_value_added_centered_student"
  ),
  na.strings = c("", "NA")
)[
  analysis_sample == "All" &
    outcome %chin% c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1")
]

expected_outcomes <- c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1")
if (!setequal(unique(values$outcome), expected_outcomes)) {
  stop("The Stata school-value file is missing math, verbal, or high-premium-field VA.", call. = FALSE)
}
if (values[, anyDuplicated(paste(school_rbd, outcome))]) {
  stop("School-value input has duplicate school-outcome rows.", call. = FALSE)
}

values_wide <- dcast(
  values,
  school_rbd ~ outcome,
  value.var = "controlled_value_added_centered_student"
)
counts_wide <- dcast(
  values,
  school_rbd ~ outcome,
  value.var = "n_students_regression"
)
setnames(
  values_wide,
  c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1"),
  c("math_va", "language_va", "highpay_va")
)
setnames(
  counts_wide,
  c("z_year_math_max", "z_year_leng_max", "high_paying_field_m1"),
  c("n_math", "n_language", "n_highpay")
)
plot_dt <- merge(values_wide, counts_wide, by = "school_rbd", all = FALSE)

common_x_limits <- range(plot_dt$math_va, na.rm = TRUE)
common_x_limits <- max(abs(common_x_limits)) * c(-1, 1)

draw_correlation_panel <- function(dt, y_col, n_y_col, y_label, output_path, comparison) {
  panel_dt <- dt[complete.cases(dt[, c("math_va", y_col), with = FALSE])]
  panel_dt[, n_students := pmin(n_math, get(n_y_col), na.rm = TRUE)]
  if (nrow(panel_dt) == 0L) {
    stop("No schools have both math and ", y_label, ".", call. = FALSE)
  }

  fit <- lm(panel_dt[[y_col]] ~ panel_dt$math_va)
  correlation <- cor(panel_dt$math_va, panel_dt[[y_col]])
  y_limits <- range(panel_dt[[y_col]], na.rm = TRUE)
  y_limits <- max(abs(y_limits)) * c(-1, 1)

  annotation <- data.table(
    x = common_x_limits[1] + 0.08 * diff(common_x_limits),
    y = y_limits[2] - 0.08 * diff(y_limits),
    label = paste0(
      "r = ", sprintf("%.3f", correlation),
      "\nN = ", format(nrow(panel_dt), big.mark = ",")
    )
  )

  plot <- ggplot(panel_dt, aes(x = math_va, y = .data[[y_col]])) +
    geom_hline(yintercept = 0, color = "grey84", linewidth = 0.35) +
    geom_vline(xintercept = 0, color = "grey84", linewidth = 0.35) +
    geom_point(aes(size = n_students), alpha = 0.24, color = "#1B4D89") +
    geom_smooth(method = "lm", se = FALSE, color = "#B6422E", linewidth = 0.85) +
    geom_text(
      data = annotation,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      family = "serif",
      size = 3.3
    ) +
    coord_cartesian(xlim = common_x_limits, ylim = y_limits, expand = TRUE) +
    scale_size_continuous(range = c(0.25, 2.4), guide = "none") +
    labs(
      x = "Math value added",
      y = y_label
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9, color = "grey25"),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(output_path, plot, width = 6.2, height = 5.1, dpi = 300)
  data.table(
    comparison = comparison,
    correlation = correlation,
    n_schools = nrow(panel_dt),
    slope = unname(coef(fit)[[2]]),
    intercept = unname(coef(fit)[[1]]),
    figure = basename(output_path)
  )
}

math_language_summary <- draw_correlation_panel(
  plot_dt,
  y_col = "language_va",
  n_y_col = "n_language",
  y_label = "Verbal value added",
  output_path = math_language_plot_path,
  comparison = "math_vs_language"
)

math_highpay_summary <- draw_correlation_panel(
  plot_dt,
  y_col = "highpay_va",
  n_y_col = "n_highpay",
  y_label = "High-premium-field value added",
  output_path = plot_path,
  comparison = "math_vs_highpay"
)

fwrite(math_highpay_summary[, !"figure"], summary_path)
fwrite(rbindlist(list(math_language_summary, math_highpay_summary)), figure2_summary_path)
message("Wrote: ", summary_path)
message("Wrote: ", figure2_summary_path)
message("Wrote: ", math_language_plot_path)
message("Wrote: ", plot_path)
