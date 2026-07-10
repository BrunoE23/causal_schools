###############################################################################
# Scatter plots: grade-4 SIMCE reported income vs program_income_full
#
# Version 1:
#   x = student's directly reported grade-4 SIMCE income midpoint (`income_mid`)
#
# Version 2:
#   x = median reported grade-4 SIMCE income among students in the grade-4 RBD,
#       pooling all observed donors in simce_4to.Rdata and requiring at least
#       15 observed donors per school.
#
# y = program_income_full_clp_m1, in CLP millions.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

find_existing_path <- function(env_var, candidates, label, must_be_dir = TRUE) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  if (must_be_dir) {
    candidates <- candidates[dir.exists(candidates)]
  } else {
    candidates <- candidates[file.exists(candidates)]
  }

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

comma_label <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE)
}

million_label <- function(x) {
  sprintf("%.1f", x)
}

make_scatter <- function(dt, x_col, title, subtitle, x_label) {
  ggplot(dt, aes(x = .data[[x_col]], y = program_income_full_millions)) +
    geom_point(color = "#1f5a85", alpha = 0.035, size = 0.28, stroke = 0) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "#b13f2a",
      fill = "#b13f2a",
      linewidth = 0.7,
      alpha = 0.18
    ) +
    scale_x_continuous(labels = million_label) +
    scale_y_continuous(labels = million_label) +
    coord_cartesian(xlim = c(0, 2.4), ylim = c(0.5, 4.55)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = "Program income full, CLP millions"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

summarize_plot_data <- function(dt, x_col, label) {
  fit <- lm(program_income_full_millions ~ dt[[x_col]], data = dt)
  data.table(
    income_measure = label,
    n_students = nrow(dt),
    n_cohorts = uniqueN(dt$cohort_gr8),
    min_cohort = min(dt$cohort_gr8, na.rm = TRUE),
    max_cohort = max(dt$cohort_gr8, na.rm = TRUE),
    mean_x_millions = mean(dt[[x_col]], na.rm = TRUE),
    median_x_millions = median(dt[[x_col]], na.rm = TRUE),
    mean_program_income_full_millions = mean(dt$program_income_full_millions, na.rm = TRUE),
    median_program_income_full_millions = median(dt$program_income_full_millions, na.rm = TRUE),
    corr_raw = cor(dt[[x_col]], dt$program_income_full_millions, use = "complete.obs"),
    intercept_millions = unname(coef(fit)[[1]]),
    slope_millions_per_million = unname(coef(fit)[[2]]),
    r_squared = summary(fit)$r.squared
  )
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
universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
simce_4to_path <- file.path(clean_dir, "simce_4to.Rdata")
program_income_path <- find_existing_path(
  "PROGRAM_INCOME_OUTCOMES_PATH",
  file.path(
    repo_wd,
    "output",
    "tables",
    "mifuturo_matricula_income",
    "mifuturo_person_level_income_outcomes.csv"
  ),
  "program income outcomes",
  must_be_dir = FALSE
)

figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
table_dir <- file.path(repo_wd, "output", "tables", "results_section")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

direct_plot_path <- file.path(
  figure_dir,
  "grade4_direct_income_x_program_income_full_scatter.png"
)
school_median_plot_path <- file.path(
  figure_dir,
  "grade4_school_median_income_x_program_income_full_scatter.png"
)
summary_path <- file.path(
  table_dir,
  "grade4_income_x_program_income_full_scatter_summary.csv"
)

message("Reading universe income columns: ", universe_path)
universe <- fread(
  universe_path,
  select = c("mrun", "cohort_gr8", "income_mid", "simce_rbd_4to"),
  na.strings = c("", "NA")
)
universe[, `:=`(
  mrun = as.numeric(mrun),
  cohort_gr8 = as.integer(cohort_gr8),
  income_mid = as.numeric(income_mid),
  simce_rbd_4to = as.numeric(simce_rbd_4to)
)]
universe[!is.finite(income_mid) | income_mid <= 0, income_mid := NA_real_]

message("Reading program_income_full columns: ", program_income_path)
program_income <- fread(
  program_income_path,
  select = c("mrun", "program_income_full_clp_m1", "program_income_full_source_m1", "matriculated_m1"),
  na.strings = c("", "NA")
)
program_income[, mrun := as.numeric(mrun)]
if (anyDuplicated(program_income$mrun) > 0) {
  stop("Program income file is not unique by mrun.", call. = FALSE)
}

message("Reading grade-4 SIMCE donor incomes: ", simce_4to_path)
simce_env <- new.env(parent = emptyenv())
load(simce_4to_path, envir = simce_env)
if (!exists("simce_4to", envir = simce_env)) {
  stop("simce_4to.Rdata does not contain object simce_4to.", call. = FALSE)
}
simce_4to <- as.data.table(get("simce_4to", envir = simce_env))
rm(simce_env)
required_simce_cols <- c("simce_rbd_4to", "income_mid")
missing_simce_cols <- setdiff(required_simce_cols, names(simce_4to))
if (length(missing_simce_cols) > 0) {
  stop("simce_4to missing columns: ", paste(missing_simce_cols, collapse = ", "))
}
simce_4to[, `:=`(
  simce_rbd_4to = as.numeric(simce_rbd_4to),
  income_mid = as.numeric(income_mid)
)]
simce_4to[!is.finite(income_mid) | income_mid <= 0, income_mid := NA_real_]
school_income <- simce_4to[
  !is.na(simce_rbd_4to) & !is.na(income_mid),
  .(
    income_school_median = median(income_mid, na.rm = TRUE),
    income_school_n = .N
  ),
  by = simce_rbd_4to
][income_school_n >= 15L]

plot_df <- merge(universe, program_income, by = "mrun", all.x = TRUE, sort = FALSE)
plot_df <- merge(plot_df, school_income, by = "simce_rbd_4to", all.x = TRUE, sort = FALSE)
plot_df <- plot_df[
  !is.na(program_income_full_clp_m1) &
    is.finite(program_income_full_clp_m1) &
    program_income_full_clp_m1 > 0
]
plot_df[, `:=`(
  income_mid_millions = income_mid / 1e6,
  income_school_median_millions = income_school_median / 1e6,
  program_income_full_millions = program_income_full_clp_m1 / 1e6
)]

direct_df <- plot_df[!is.na(income_mid_millions)]
school_median_df <- plot_df[!is.na(income_school_median_millions)]

summary_dt <- rbindlist(
  list(
    summarize_plot_data(direct_df, "income_mid_millions", "student_direct_income_mid"),
    summarize_plot_data(school_median_df, "income_school_median_millions", "school_median_income_mid")
  ),
  use.names = TRUE
)

message("Writing summary: ", summary_path)
fwrite(summary_dt, summary_path)

direct_plot <- make_scatter(
  direct_df,
  "income_mid_millions",
  "Program Income by Direct Grade-4 Reported Income",
  "X-axis uses the student's own SIMCE grade-4 income response",
  "Direct grade-4 SIMCE reported household income, CLP millions"
)
school_median_plot <- make_scatter(
  school_median_df,
  "income_school_median_millions",
  "Program Income by Grade-4 School Median Income",
  "X-axis uses median SIMCE grade-4 reported income in the student's grade-4 RBD",
  "Grade-4 RBD median SIMCE reported household income, CLP millions"
)

message("Writing direct-income scatter: ", direct_plot_path)
ggsave(direct_plot_path, direct_plot, width = 7.5, height = 5.75, dpi = 300)
message("Writing school-median scatter: ", school_median_plot_path)
ggsave(school_median_plot_path, school_median_plot, width = 7.5, height = 5.75, dpi = 300)

print(summary_dt)
