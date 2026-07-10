###############################################################################
# School VA correlation plots with math always on the x-axis
#
# This script uses existing school value-added outputs and writes paper-facing
# scatter plots comparing math VA to other school-value dimensions.
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
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

clean_dir <- file.path(data_wd, "data", "clean")
school_values_path <- file.path(
  clean_dir,
  "school_rbd_observational_values",
  "school_rbd_observational_values.csv"
)
program_income_values_path <- file.path(
  clean_dir,
  "school_rbd_observational_values",
  "school_rbd_observational_values_program_income.csv"
)
figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
table_dir <- file.path(repo_wd, "output", "tables", "results_section")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

combined_plot_path <- file.path(
  figure_dir,
  "math_x_va_correlations.png"
)
summary_csv_path <- file.path(
  table_dir,
  "school_va_math_x_correlations.csv"
)

if (!file.exists(school_values_path)) {
  stop("Missing school value file: ", school_values_path, call. = FALSE)
}

school_values <- fread(
  school_values_path,
  select = c(
    "school_rbd",
    "analysis_sample",
    "outcome",
    "controlled_value_added_centered_student",
    "n_students_regression"
  ),
  na.strings = c("", "NA")
)
school_values <- school_values[analysis_sample == "All"]

outcome_specs <- data.table(
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "stem_enrollment_m1",
    "inst_certified_years_m1"
  ),
  short_name = c(
    "math",
    "language",
    "stem",
    "institution_quality"
  ),
  plot_label = c(
    "Math VA",
    "Verbal VA",
    "STEM enrollment VA",
    "Institutional quality VA"
  ),
  plot_order = c(0L, 1L, 2L, 3L)
)

values_long <- merge(
  school_values,
  outcome_specs,
  by = "outcome",
  all = FALSE,
  sort = FALSE
)

if (file.exists(program_income_values_path)) {
  program_income_values <- fread(
    program_income_values_path,
    select = c(
      "school_rbd",
      "analysis_sample",
      "outcome",
      "controlled_value_added_centered_student",
      "n_students_regression"
    ),
    na.strings = c("", "NA")
  )
  program_income_values <- program_income_values[
    analysis_sample == "All" &
      outcome == "log_program_income_clp_m1"
  ]
  program_income_values[, `:=`(
    short_name = "program_income",
    plot_label = "Program income VA",
    plot_order = 4L
  )]
  values_long <- rbindlist(
    list(
      values_long[, .(
        school_rbd,
        short_name,
        plot_label,
        plot_order,
        controlled_value_added_centered_student,
        n_students_regression
      )],
      program_income_values[, .(
        school_rbd,
        short_name,
        plot_label,
        plot_order,
        controlled_value_added_centered_student,
        n_students_regression
      )]
    ),
    use.names = TRUE
  )
} else {
  values_long <- values_long[, .(
    school_rbd,
    short_name,
    plot_label,
    plot_order,
    controlled_value_added_centered_student,
    n_students_regression
  )]
}

values_wide <- dcast(
  values_long,
  school_rbd ~ short_name,
  value.var = "controlled_value_added_centered_student"
)
count_wide <- dcast(
  values_long,
  school_rbd ~ short_name,
  value.var = "n_students_regression"
)
setnames(
  count_wide,
  setdiff(names(count_wide), "school_rbd"),
  paste0("n_", setdiff(names(count_wide), "school_rbd"))
)
values_wide <- merge(values_wide, count_wide, by = "school_rbd", all.x = TRUE)

plot_specs <- unique(values_long[short_name != "math", .(short_name, plot_label, plot_order)])
setorder(plot_specs, plot_order)

plot_dt <- rbindlist(lapply(seq_len(nrow(plot_specs)), function(i) {
  y_name <- plot_specs$short_name[i]
  y_label <- plot_specs$plot_label[i]
  n_col <- paste0("n_", y_name)

  out <- values_wide[, .(
    school_rbd,
    math_value = math,
    y_value = get(y_name),
    n_students = pmin(n_math, get(n_col), na.rm = TRUE),
    comparison = y_name,
    comparison_label = y_label
  )]
  out[is.infinite(n_students), n_students := NA_real_]
  out[complete.cases(math_value, y_value)]
}), use.names = TRUE)

if (nrow(plot_dt) == 0) {
  stop("No complete math-x correlation data found.", call. = FALSE)
}

summary_dt <- plot_dt[, {
  fit <- stats::lm(y_value ~ math_value)
  .(
    correlation = stats::cor(math_value, y_value),
    n_schools = .N,
    slope = unname(stats::coef(fit)[["math_value"]]),
    intercept = unname(stats::coef(fit)[["(Intercept)"]])
  )
}, by = .(comparison, comparison_label)]

summary_dt[, label := paste0(
  "r = ",
  sprintf("%.3f", correlation),
  "\nN = ",
  format(n_schools, big.mark = ",")
)]

annotation_dt <- plot_dt[, .(
  x = stats::quantile(math_value, 0.04, na.rm = TRUE),
  y = stats::quantile(y_value, 0.96, na.rm = TRUE)
), by = .(comparison, comparison_label)]
annotation_dt <- merge(
  annotation_dt,
  summary_dt[, .(comparison, label)],
  by = "comparison",
  all.x = TRUE,
  sort = FALSE
)

plot_dt[, comparison_label := factor(
  comparison_label,
  levels = plot_specs$plot_label
)]
annotation_dt[, comparison_label := factor(
  comparison_label,
  levels = plot_specs$plot_label
)]

math_x_plot <- ggplot(plot_dt, aes(x = math_value, y = y_value)) +
  geom_hline(yintercept = 0, color = "grey86", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey86", linewidth = 0.3) +
  geom_point(aes(size = n_students), alpha = 0.28, color = "#1B4D89") +
  geom_smooth(method = "lm", se = FALSE, color = "#B6422E", linewidth = 0.8) +
  geom_text(
    data = annotation_dt,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    family = "serif",
    size = 3.2
  ) +
  facet_wrap(~ comparison_label, scales = "free_y", ncol = 2) +
  scale_size_continuous(range = c(0.35, 2.8), guide = "none") +
  labs(
    x = "Math value added",
    y = "Comparison outcome value added",
    title = "School value-added correlations with math on the x-axis"
  ) +
  theme_minimal(base_family = "serif", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  combined_plot_path,
  math_x_plot,
  width = 9.5,
  height = 7.2,
  dpi = 300
)

for (i in seq_len(nrow(plot_specs))) {
  comp <- plot_specs$short_name[i]
  comp_label <- plot_specs$plot_label[i]
  individual_dt <- plot_dt[comparison == comp]
  individual_annotation <- annotation_dt[comparison == comp]

  individual_plot <- ggplot(individual_dt, aes(x = math_value, y = y_value)) +
    geom_hline(yintercept = 0, color = "grey86", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "grey86", linewidth = 0.3) +
    geom_point(aes(size = n_students), alpha = 0.30, color = "#1B4D89") +
    geom_smooth(method = "lm", se = FALSE, color = "#B6422E", linewidth = 0.9) +
    geom_text(
      data = individual_annotation,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      family = "serif",
      size = 3.4
    ) +
    scale_size_continuous(range = c(0.35, 2.8), guide = "none") +
    labs(
      x = "Math value added",
      y = comp_label,
      title = paste("Math VA vs", comp_label)
    ) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(
    file.path(figure_dir, paste0("math_x_", comp, "_va_correlation.png")),
    individual_plot,
    width = 6.2,
    height = 5.1,
    dpi = 300
  )
}

fwrite(
  summary_dt[, .(
    comparison,
    comparison_label,
    correlation,
    n_schools,
    slope,
    intercept
  )],
  summary_csv_path
)

message("Wrote: ", combined_plot_path)
message("Wrote: ", summary_csv_path)
for (comp in plot_specs$short_name) {
  message("Wrote: ", file.path(figure_dir, paste0("math_x_", comp, "_va_correlation.png")))
}
