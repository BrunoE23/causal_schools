suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(stringr)
})

# Compare the adjusted observational STEM school value distribution for schools
# in the practical SAE support against all other schools.

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
)

support_stub <- "k100_timely_risk"
positive_prob_student_threshold <- 100L
stem_outcome <- "stem_enrollment_m1"
value_column <- "controlled_value_added_centered_student"
sae_group_label <- paste0(
  "SAE support schools (>",
  positive_prob_student_threshold,
  " positive-prob students)"
)
private_group_label <- "Private paid schools"
other_group_label <- "Other schools"

values_path <- file.path(
  data_wd,
  "data/clean/school_rbd_observational_values/school_rbd_observational_values.csv"
)

support_path <- file.path(
  data_wd,
  "data/clean/DA_probs",
  paste0("probability_support_", support_stub, ".csv")
)

school_directory_root <- file.path(data_wd, "data/raw/school_directory")

figure_dir <- file.path(repo_wd, "output/figures/scalar_school_value_iv")
table_dir <- file.path(repo_wd, "output/tables/scalar_school_value_iv")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

plot_path <- file.path(
  figure_dir,
  paste0("stem_adj_va_distribution_sae_gt", positive_prob_student_threshold, "_private_paid.png")
)

summary_path <- file.path(
  table_dir,
  paste0("stem_adj_va_distribution_sae_gt", positive_prob_student_threshold, "_private_paid_summary.csv")
)

latest_school_directory <- function(root) {
  if (!dir.exists(root)) {
    return(NULL)
  }

  files <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }

  years <- str_extract(files, "\\b20[0-9]{2}\\b") %>% as.integer()
  files[which.max(years)]
}

read_private_paid_schools <- function(root) {
  directory_file <- latest_school_directory(root)

  if (is.null(directory_file)) {
    warning("No school directory CSV found. Private-paid category will be empty.")
    return(tibble(school_rbd = numeric(), private_paid = logical()))
  }

  metadata <- read_delim(
    directory_file,
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "Latin1")
  )

  if (!"RBD" %in% names(metadata)) {
    warning("Latest school directory has no RBD column. Private-paid category will be empty.")
    return(tibble(school_rbd = numeric(), private_paid = logical()))
  }

  metadata %>%
    transmute(
      school_rbd = as.numeric(RBD),
      private_paid = case_when(
        "COD_DEPE" %in% names(metadata) ~ as.character(COD_DEPE) == "4",
        "COD_DEPE2" %in% names(metadata) ~ as.character(COD_DEPE2) == "3",
        TRUE ~ FALSE
      )
    ) %>%
    filter(!is.na(school_rbd)) %>%
    distinct(school_rbd, .keep_all = TRUE)
}

school_values <- read_csv(values_path, show_col_types = FALSE) %>%
  filter(
    outcome == stem_outcome,
    !is.na(.data[[value_column]])
  ) %>%
  transmute(
    school_rbd = as.numeric(school_rbd),
    stem_adj_va = .data[[value_column]],
    n_students_school,
    n_students_regression
  )

sae_support <- read_csv(support_path, show_col_types = FALSE) %>%
  transmute(
    school_rbd = suppressWarnings(as.numeric(rbd_prob)),
    n_students_positive_prob
  ) %>%
  filter(!is.na(school_rbd)) %>%
  filter(n_students_positive_prob > positive_prob_student_threshold)

private_paid_schools <- read_private_paid_schools(school_directory_root)

plot_df <- school_values %>%
  left_join(sae_support, by = "school_rbd") %>%
  left_join(private_paid_schools, by = "school_rbd") %>%
  mutate(private_paid = coalesce(private_paid, FALSE)) %>%
  mutate(
    school_group = case_when(
      !is.na(n_students_positive_prob) ~ sae_group_label,
      private_paid ~ private_group_label,
      TRUE ~ other_group_label
    ),
    school_group = factor(
      school_group,
      levels = c(sae_group_label, private_group_label, other_group_label)
    )
  )

summary_df <- plot_df %>%
  group_by(school_group) %>%
  summarise(
    n_schools = n(),
    mean_stem_adj_va = mean(stem_adj_va, na.rm = TRUE),
    sd_stem_adj_va = sd(stem_adj_va, na.rm = TRUE),
    p10_stem_adj_va = quantile(stem_adj_va, 0.10, na.rm = TRUE),
    p25_stem_adj_va = quantile(stem_adj_va, 0.25, na.rm = TRUE),
    median_stem_adj_va = median(stem_adj_va, na.rm = TRUE),
    p75_stem_adj_va = quantile(stem_adj_va, 0.75, na.rm = TRUE),
    p90_stem_adj_va = quantile(stem_adj_va, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_df, summary_path)

mean_lines <- summary_df %>%
  transmute(
    school_group,
    mean_stem_adj_va
  )

annotation_df <- summary_df %>%
  transmute(
    school_group,
    x = mean_stem_adj_va,
    label = paste0(
      "N = ", comma(n_schools),
      "\nMean = ", number(mean_stem_adj_va, accuracy = 0.001)
    )
  )

fill_values <- c("#2563eb", "#f59e0b", "#e11d48")
names(fill_values) <- c(sae_group_label, private_group_label, other_group_label)

color_values <- c("#1d4ed8", "#b45309", "#be123c")
names(color_values) <- c(sae_group_label, private_group_label, other_group_label)

dist_plot <- ggplot(plot_df, aes(x = stem_adj_va, fill = school_group, color = school_group)) +
  geom_density(alpha = 0.26, linewidth = 0.8, adjust = 1.1) +
  geom_vline(
    data = mean_lines,
    aes(xintercept = mean_stem_adj_va, color = school_group),
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) +
  geom_label(
    data = annotation_df,
    aes(x = x, y = Inf, label = label, color = school_group),
    inherit.aes = FALSE,
    vjust = 1.08,
    size = 3.2,
    label.size = 0.2,
    fill = "white",
    label.padding = unit(0.16, "lines"),
    show.legend = FALSE
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.01),
    breaks = pretty_breaks(n = 8)
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_fill_manual(
    values = fill_values
  ) +
  scale_color_manual(
    values = color_values
  ) +
  labs(
    title = "Distribution of Adjusted STEM Enrollment School Values",
    subtitle = "Controlled observational VA, centered by the student-weighted national mean",
    x = "Adjusted STEM enrollment school value",
    y = "Density",
    fill = NULL,
    color = NULL,
    caption = paste0(
      "SAE support based on ", support_stub,
      "; dashed lines show group means."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "top",
    legend.justification = "left",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = plot_path,
  plot = dist_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

message("Wrote plot: ", plot_path)
message("Wrote summary: ", summary_path)
