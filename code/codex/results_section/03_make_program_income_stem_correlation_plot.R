###############################################################################
# Program-income VA vs STEM-enrollment VA correlation plot
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

plot_path <- file.path(
  figure_dir,
  "program_income_x_stem_va_correlation.png"
)
summary_csv_path <- file.path(
  table_dir,
  "program_income_stem_va_correlation.csv"
)

if (!file.exists(school_values_path)) {
  stop("Missing school value file: ", school_values_path, call. = FALSE)
}
if (!file.exists(program_income_values_path)) {
  stop("Missing program-income value file: ", program_income_values_path, call. = FALSE)
}

stem_values <- fread(
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
stem_values <- stem_values[
  analysis_sample == "All" &
    outcome == "stem_enrollment_m1",
  .(
    school_rbd,
    stem_va = controlled_value_added_centered_student,
    n_stem = n_students_regression
  )
]

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
    outcome == "log_program_income_clp_m1",
  .(
    school_rbd,
    program_income_va = controlled_value_added_centered_student,
    n_program_income = n_students_regression
  )
]

plot_dt <- merge(
  program_income_values,
  stem_values,
  by = "school_rbd",
  all = FALSE,
  sort = FALSE
)
plot_dt <- plot_dt[complete.cases(program_income_va, stem_va)]
plot_dt[, n_students := pmin(n_program_income, n_stem, na.rm = TRUE)]
plot_dt[is.infinite(n_students), n_students := NA_real_]

if (nrow(plot_dt) < 3) {
  stop("Too few complete schools for program-income/STEM correlation.", call. = FALSE)
}

fit <- stats::lm(stem_va ~ program_income_va, data = plot_dt)
correlation <- stats::cor(plot_dt$program_income_va, plot_dt$stem_va)
summary_dt <- data.table(
  comparison = "program_income_vs_stem",
  x_axis = "Program income VA",
  y_axis = "STEM enrollment VA",
  correlation = correlation,
  n_schools = nrow(plot_dt),
  slope = unname(stats::coef(fit)[["program_income_va"]]),
  intercept = unname(stats::coef(fit)[["(Intercept)"]])
)
summary_dt[, label := paste0(
  "r = ",
  sprintf("%.3f", correlation),
  "\nN = ",
  format(n_schools, big.mark = ",")
)]

annotation_dt <- data.table(
  x = stats::quantile(plot_dt$program_income_va, 0.04, na.rm = TRUE),
  y = stats::quantile(plot_dt$stem_va, 0.96, na.rm = TRUE),
  label = summary_dt$label
)

correlation_plot <- ggplot(plot_dt, aes(x = program_income_va, y = stem_va)) +
  geom_hline(yintercept = 0, color = "grey86", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey86", linewidth = 0.3) +
  geom_point(aes(size = n_students), alpha = 0.30, color = "#1B4D89") +
  geom_smooth(method = "lm", se = FALSE, color = "#B6422E", linewidth = 0.9) +
  geom_text(
    data = annotation_dt,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    family = "serif",
    size = 3.5
  ) +
  scale_size_continuous(range = c(0.35, 2.8), guide = "none") +
  labs(
    x = "Program income value added",
    y = "STEM enrollment value added",
    title = "Program income VA vs STEM enrollment VA"
  ) +
  theme_minimal(base_family = "serif", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  plot_path,
  correlation_plot,
  width = 6.2,
  height = 5.1,
  dpi = 300
)

fwrite(
  summary_dt[, .(
    comparison,
    x_axis,
    y_axis,
    correlation,
    n_schools,
    slope,
    intercept
  )],
  summary_csv_path
)

message("Wrote: ", plot_path)
message("Wrote: ", summary_csv_path)
