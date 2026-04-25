suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# ------------------------- Configuration -------------------------

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
)

values_path <- file.path(
  data_wd,
  "data/clean/school_rbd_observational_values/school_rbd_observational_values.csv"
)

school_directory_root <- file.path(data_wd, "data/raw/school_directory")
figures_dir <- file.path(repo_wd, "output/figures/school_rbd_observational_values")
csv_dir <- file.path(data_wd, "data/clean/school_rbd_observational_values/visualizations")
chile_outline_default_candidates <- c(
  "C:/Users/xd-br/Downloads/chile_outline2.png",
  "C:/Users/xd-br/Downloads/chile_outline.png"
)
chile_outline_default <- chile_outline_default_candidates[file.exists(chile_outline_default_candidates)][1]
chile_outline_path <- Sys.getenv(
  "CAUSAL_SCHOOLS_CHILE_OUTLINE_PATH",
  unset = if (!is.na(chile_outline_default)) chile_outline_default else ""
)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)

raw_score_outcomes <- c("math_max", "leng_max", "leng_math_total", "hist_max", "scien_max", "math2_max")
scale1000_score_outcomes <- paste0("scale1000_", raw_score_outcomes)
z_year_score_outcomes <- paste0("z_year_", raw_score_outcomes)
score_outcomes <- c(z_year_score_outcomes, scale1000_score_outcomes, raw_score_outcomes)
plot_score_outcomes <- c("z_year_math_max")
stem_outcomes <- c("stem_enrollment_m1", "stem_enrollment_ml", "f_science_m1", "f_eng_m1", "f_science_ml", "f_eng_ml")
primary_score_outcome <- "z_year_math_max"
primary_stem_outcome <- "stem_enrollment_m1"
top_n_schools <- 50L
min_students_for_top_list <- 50L
region_order_north_to_south <- c(
  "AYP",
  "TPCA",
  "ANTOF",
  "ATCMA",
  "COQ",
  "VALPO",
  "RM",
  "LGBO",
  "MAULE",
  "NUBLE",
  "BBIO",
  "ARAUC",
  "RIOS",
  "LAGOS",
  "AYSEN",
  "MAG"
)
dependency_group_order <- c("1", "2", "3", "4", "5")
dependency_group_labels <- c(
  "1" = "Municipal",
  "2" = "Particular Subvencionado",
  "3" = "Particular Pagado",
  "4" = "Administracion Delegada",
  "5" = "Servicio Local de Educacion"
)
dependency_detail_order <- c("1", "2", "3", "4", "5", "6")
dependency_detail_labels <- c(
  "1" = "Corporacion Municipal",
  "2" = "Municipal DAEM",
  "3" = "Particular Subvencionado",
  "4" = "Particular Pagado",
  "5" = "Administracion Delegada",
  "6" = "Servicio Local de Educacion"
)

# ------------------------- Helpers -------------------------

write_plot <- function(plot, filename, width = 8, height = 5) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "white", color = "#d9d9d9")
      ),
    width = width,
    height = height,
    dpi = 300
  )
}

value_col <- function(value_type, outcome) {
  paste(value_type, outcome, sep = "__")
}

null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

rotate_image_array <- function(image_array) {
  dims <- dim(image_array)

  if (length(dims) == 2) {
    return(t(apply(image_array, 2, rev)))
  }

  rotated <- aperm(image_array, c(2, 1, 3))
  rotated[nrow(rotated):1, , , drop = FALSE]
}

trim_image_array <- function(image_array, white_threshold = 0.98, pad = 3L) {
  dims <- dim(image_array)

  content_mask <- if (length(dims) == 2) {
    image_array < white_threshold
  } else if (dims[3] == 3) {
    rowMeans(image_array[, , 1:3, drop = FALSE], dims = 2) < white_threshold
  } else {
    alpha_mask <- image_array[, , 4] > 0.01
    rgb_mask <- rowMeans(image_array[, , 1:3, drop = FALSE], dims = 2) < white_threshold
    alpha_mask & rgb_mask
  }

  content_rows <- which(apply(content_mask, 1, any))
  content_cols <- which(apply(content_mask, 2, any))

  if (length(content_rows) == 0 || length(content_cols) == 0) {
    return(image_array)
  }

  row_min <- max(1L, min(content_rows) - pad)
  row_max <- min(nrow(content_mask), max(content_rows) + pad)
  col_min <- max(1L, min(content_cols) - pad)
  col_max <- min(ncol(content_mask), max(content_cols) + pad)

  if (length(dims) == 2) {
    return(image_array[row_min:row_max, col_min:col_max, drop = FALSE])
  }

  image_array[row_min:row_max, col_min:col_max, , drop = FALSE]
}

read_chile_outline_raster <- function(image_path, alpha = 0.08) {
  if (!nzchar(image_path) || !file.exists(image_path) || !requireNamespace("png", quietly = TRUE)) {
    return(NULL)
  }

  image_array <- png::readPNG(image_path)
  image_array <- trim_image_array(image_array)
  image_array <- rotate_image_array(image_array)
  dims <- dim(image_array)

  if (length(dims) == 2) {
    rgba_array <- array(0, dim = c(dims[1], dims[2], 4))
    rgba_array[, , 1] <- image_array
    rgba_array[, , 2] <- image_array
    rgba_array[, , 3] <- image_array
    rgba_array[, , 4] <- alpha
  } else if (dims[3] == 3) {
    rgba_array <- array(0, dim = c(dims[1], dims[2], 4))
    rgba_array[, , 1:3] <- image_array
    rgba_array[, , 4] <- alpha
  } else {
    rgba_array <- image_array
    rgba_array[, , 4] <- pmin(1, rgba_array[, , 4] * alpha)
  }

  rgba_array
}

chile_outline_annotation <- function(
  image_path,
  alpha = 0.12,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf
) {
  outline_raster <- read_chile_outline_raster(image_path, alpha = alpha)

  if (is.null(outline_raster)) {
    return(NULL)
  }

  annotation_raster(
    raster = outline_raster,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    interpolate = TRUE
  )
}

top_watermark_layer <- function(data, image_path, alpha = 0.14) {
  if (nrow(data) == 0) {
    return(NULL)
  }

  y_range <- range(data$mean_value, na.rm = TRUE)
  if (!all(is.finite(y_range))) {
    return(NULL)
  }

  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- max(abs(y_range), na.rm = TRUE)
  }
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  x_count <- if (is.factor(data$region_name)) nlevels(data$region_name) else length(unique(data$region_name))
  full_xmin <- 0.4
  full_xmax <- x_count + 0.6
  x_mid <- (full_xmin + full_xmax) / 2
  x_half_width <- 0.9 * (full_xmax - full_xmin) / 2

  y_top <- y_range[2] + 0.02 * y_span
  y_bottom <- y_range[2] - 0.38 * y_span

  chile_outline_annotation(
    image_path = image_path,
    alpha = alpha,
    xmin = x_mid - x_half_width,
    xmax = x_mid + x_half_width,
    ymin = y_bottom,
    ymax = y_top
  )
}

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

read_school_metadata <- function(root) {
  directory_file <- latest_school_directory(root)

  if (is.null(directory_file)) {
    return(NULL)
  }

  metadata <- read_delim(
    directory_file,
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "Latin1")
  )

  if (!"RBD" %in% names(metadata)) {
    return(NULL)
  }

  metadata %>%
    transmute(
      school_rbd = as.numeric(RBD),
      school_name = if ("NOM_RBD" %in% names(metadata)) NOM_RBD else NA_character_,
      region_code = if ("COD_REG_RBD" %in% names(metadata)) as.character(COD_REG_RBD) else NA_character_,
      region_name = if ("NOM_REG_RBD_A" %in% names(metadata)) NOM_REG_RBD_A else NA_character_,
      comuna_code = if ("COD_COM_RBD" %in% names(metadata)) as.character(COD_COM_RBD) else NA_character_,
      comuna_name = if ("NOM_COM_RBD" %in% names(metadata)) NOM_COM_RBD else NA_character_,
      dependency_group_code = if ("COD_DEPE2" %in% names(metadata)) as.character(COD_DEPE2) else NA_character_,
      dependency_group_label = if ("COD_DEPE2" %in% names(metadata)) recode(as.character(COD_DEPE2), !!!dependency_group_labels, .default = NA_character_) else NA_character_,
      dependency_detail_code = if ("COD_DEPE" %in% names(metadata)) as.character(COD_DEPE) else NA_character_,
      dependency_detail_label = if ("COD_DEPE" %in% names(metadata)) recode(as.character(COD_DEPE), !!!dependency_detail_labels, .default = NA_character_) else NA_character_,
      rural = if ("RURAL_RBD" %in% names(metadata)) as.character(RURAL_RBD) else NA_character_
    ) %>%
    distinct(school_rbd, .keep_all = TRUE)
}

pairwise_correlations <- function(wide_values, columns) {
  pairs <- combn(columns, 2, simplify = FALSE)

  map_dfr(pairs, function(pair) {
    x <- wide_values[[pair[1]]]
    y <- wide_values[[pair[2]]]
    ok <- complete.cases(x, y)

    tibble(
      var_1 = pair[1],
      var_2 = pair[2],
      correlation = if (sum(ok) >= 3) cor(x[ok], y[ok]) else NA_real_,
      n_schools = sum(ok)
    )
  })
}

safe_rank <- function(x) {
  if (sum(!is.na(x)) < 2) {
    return(rep(NA_real_, length(x)))
  }
  percent_rank(x)
}

safe_z <- function(x) {
  x_sd <- sd(x, na.rm = TRUE)
  if (is.na(x_sd) || x_sd == 0) {
    return(rep(NA_real_, length(x)))
  }
  as.numeric(scale(x))
}

math_stem_residuals <- function(data, score_outcome, stem_outcome, value_type) {
  score_col <- value_col(value_type, score_outcome)
  stem_col <- value_col(value_type, stem_outcome)

  model_data <- data %>%
    transmute(
      school_rbd,
      score_value = .data[[score_col]],
      stem_value = .data[[stem_col]],
      n_students_school
    ) %>%
    filter(complete.cases(.))

  if (nrow(model_data) < 3) {
    return(tibble())
  }

  model <- lm(stem_value ~ score_value, data = model_data)

  model_data %>%
    mutate(
      value_type = value_type,
      score_outcome = score_outcome,
      stem_outcome = stem_outcome,
      predicted_stem = as.numeric(predict(model)),
      stem_residual = stem_value - predicted_stem,
      abs_stem_residual = abs(stem_residual)
    ) %>%
    arrange(desc(abs_stem_residual))
}

# ------------------------- Load values -------------------------

if (!file.exists(values_path)) {
  stop("Values file does not exist. Run 01_construct_school_rbd_values.R first: ", values_path)
}

values <- read_csv(values_path, show_col_types = FALSE)

metadata <- read_school_metadata(school_directory_root)

wide_values <- values %>%
  select(
    school_rbd,
    outcome,
    raw_mean,
    raw_mean_centered,
    raw_mean_centered_student,
    controlled_value_added,
    controlled_value_added_centered,
    controlled_adjusted_mean,
    controlled_value_added_centered_student,
    controlled_adjusted_mean_student,
    n_students_school,
    n_students_outcome,
    n_students_regression
  ) %>%
  pivot_wider(
    id_cols = c(school_rbd, n_students_school),
    names_from = outcome,
    values_from = c(
      raw_mean,
      raw_mean_centered,
      raw_mean_centered_student,
      controlled_value_added,
      controlled_value_added_centered,
      controlled_adjusted_mean,
      controlled_value_added_centered_student,
      controlled_adjusted_mean_student,
      n_students_outcome,
      n_students_regression
    ),
    names_sep = "__"
  )

if (!is.null(metadata)) {
  wide_values <- wide_values %>%
    left_join(metadata, by = "school_rbd")
}

write_csv(wide_values, file.path(csv_dir, "school_rbd_values_wide_for_visualization.csv"))

# ------------------------- Adjustment differences -------------------------

score_adjustment <- map_dfr(intersect(score_outcomes, values$outcome), function(outcome_name) {
  raw_col <- value_col("raw_mean", outcome_name)
  raw_centered_col <- value_col("raw_mean_centered", outcome_name)
  adjusted_col <- value_col("controlled_adjusted_mean", outcome_name)
  centered_va_col <- value_col("controlled_value_added_centered", outcome_name)

  wide_values %>%
    transmute(
      school_rbd,
      school_name = if ("school_name" %in% names(wide_values)) school_name else NA_character_,
      region_name = if ("region_name" %in% names(wide_values)) region_name else NA_character_,
      dependency_group_code = if ("dependency_group_code" %in% names(wide_values)) dependency_group_code else NA_character_,
      dependency_group_label = if ("dependency_group_label" %in% names(wide_values)) dependency_group_label else NA_character_,
      dependency_detail_code = if ("dependency_detail_code" %in% names(wide_values)) dependency_detail_code else NA_character_,
      dependency_detail_label = if ("dependency_detail_label" %in% names(wide_values)) dependency_detail_label else NA_character_,
      outcome = outcome_name,
      n_students_school,
      raw_mean = .data[[raw_col]],
      raw_mean_centered = .data[[raw_centered_col]],
      adjusted_mean = .data[[adjusted_col]],
      controlled_value_added_centered = .data[[centered_va_col]],
      raw_percentile = safe_rank(.data[[raw_col]]),
      adjusted_percentile = safe_rank(.data[[adjusted_col]]),
      percentile_change = adjusted_percentile - raw_percentile,
      abs_percentile_change = abs(percentile_change),
      raw_z = safe_z(.data[[raw_col]]),
      adjusted_z = safe_z(.data[[adjusted_col]]),
      z_change = adjusted_z - raw_z,
      abs_z_change = abs(z_change),
      level_gap = adjusted_mean - raw_mean
    ) %>%
    filter(!is.na(raw_mean), !is.na(adjusted_mean))
})

score_adjustment_top <- score_adjustment %>%
  group_by(outcome) %>%
  slice_max(abs_percentile_change, n = top_n_schools, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(outcome, desc(abs_percentile_change))

score_adjustment_top_min_n <- score_adjustment %>%
  filter(n_students_school >= min_students_for_top_list) %>%
  group_by(outcome) %>%
  slice_max(abs_percentile_change, n = top_n_schools, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(outcome, desc(abs_percentile_change))

write_csv(score_adjustment, file.path(csv_dir, "score_adjustment_differences_all_schools.csv"))
write_csv(score_adjustment_top, file.path(csv_dir, "score_adjustment_differences_top_schools.csv"))
write_csv(score_adjustment_top_min_n, file.path(csv_dir, "score_adjustment_differences_top_schools_min_n.csv"))

score_adjustment_plot <- score_adjustment %>%
  filter(outcome %in% plot_score_outcomes) %>%
  ggplot(aes(x = raw_percentile, y = adjusted_percentile)) +
  geom_abline(slope = 1, intercept = 0, color = "gray55", linewidth = 0.5) +
  geom_point(aes(size = n_students_school), alpha = 0.35, color = "#2c6f7f") +
  facet_wrap(~ outcome, nrow = 1) +
  scale_size_continuous(range = c(0.4, 3), guide = "none") +
  labs(
    x = "Raw school percentile",
    y = "Adjusted school percentile",
    title = "Score Rankings Before and After Student-Level Adjustment"
  ) +
  theme_minimal(base_size = 11)

write_plot(score_adjustment_plot, "score_adjustment_percentile_scatter.png", width = 10, height = 4)

# ------------------------- Correlations -------------------------

available_outcomes <- values %>%
  filter(outcome %in% c(score_outcomes, stem_outcomes)) %>%
  distinct(outcome) %>%
  pull(outcome)

correlation_columns <- c(
  value_col("raw_mean", available_outcomes),
  value_col("controlled_adjusted_mean", available_outcomes)
) %>%
  intersect(names(wide_values))

correlation_table <- pairwise_correlations(wide_values, correlation_columns) %>%
  arrange(desc(abs(correlation)))

write_csv(correlation_table, file.path(csv_dir, "school_value_correlations_all_pairs.csv"))

math_stem_correlations <- crossing(
  score_outcome = intersect(
    c("z_year_math_max", "z_year_leng_math_total", "z_year_leng_max",
      "scale1000_math_max", "scale1000_leng_math_total", "scale1000_leng_max",
      "math_max", "leng_math_total", "leng_max"),
    available_outcomes
  ),
  stem_outcome = intersect(c("stem_enrollment_m1", "stem_enrollment_ml", "f_science_m1", "f_eng_m1"), available_outcomes),
  value_type = c("raw_mean", "controlled_adjusted_mean")
) %>%
  mutate(
    score_col = value_col(value_type, score_outcome),
    stem_col = value_col(value_type, stem_outcome),
    correlation = map2_dbl(score_col, stem_col, function(x_col, y_col) {
      ok <- complete.cases(wide_values[[x_col]], wide_values[[y_col]])
      if (sum(ok) >= 3) {
        cor(wide_values[[x_col]][ok], wide_values[[y_col]][ok])
      } else {
        NA_real_
      }
    }),
    n_schools = map2_int(score_col, stem_col, function(x_col, y_col) {
      sum(complete.cases(wide_values[[x_col]], wide_values[[y_col]]))
    })
  ) %>%
  select(value_type, score_outcome, stem_outcome, correlation, n_schools) %>%
  arrange(score_outcome, stem_outcome, value_type)

write_csv(math_stem_correlations, file.path(csv_dir, "math_stem_correlations.csv"))

scatter_data <- wide_values %>%
  transmute(
    school_rbd,
    n_students_school,
    raw_math = .data[[value_col("raw_mean", primary_score_outcome)]],
    raw_stem = .data[[value_col("raw_mean", primary_stem_outcome)]],
    adjusted_math = .data[[value_col("controlled_adjusted_mean", primary_score_outcome)]],
    adjusted_stem = .data[[value_col("controlled_adjusted_mean", primary_stem_outcome)]]
  ) %>%
  pivot_longer(
    cols = c(raw_math, raw_stem, adjusted_math, adjusted_stem),
    names_to = c("value_type", ".value"),
    names_pattern = "(raw|adjusted)_(math|stem)"
  )

math_stem_plot <- scatter_data %>%
  ggplot(aes(x = math, y = stem)) +
  geom_point(aes(size = n_students_school), alpha = 0.35, color = "#355c7d") +
  geom_smooth(method = "lm", se = FALSE, color = "#b13a2f", linewidth = 0.8) +
  facet_wrap(~ value_type, scales = "free", labeller = as_labeller(c(raw = "Raw", adjusted = "Controlled"))) +
  scale_size_continuous(range = c(0.4, 3), guide = "none") +
  labs(
    x = primary_score_outcome,
    y = primary_stem_outcome,
    title = "Math and STEM Enrollment Across Schools"
  ) +
  theme_minimal(base_size = 11)

write_plot(math_stem_plot, "math_stem_raw_vs_controlled_scatter.png", width = 9, height = 4.5)

math_stem_outliers <- bind_rows(
  math_stem_residuals(wide_values, primary_score_outcome, primary_stem_outcome, "raw_mean"),
  math_stem_residuals(wide_values, primary_score_outcome, primary_stem_outcome, "controlled_adjusted_mean")
) %>%
  left_join(
    null_coalesce(metadata, tibble(school_rbd = numeric())),
    by = "school_rbd"
  ) %>%
  group_by(value_type) %>%
  slice_max(abs_stem_residual, n = top_n_schools, with_ties = FALSE) %>%
  ungroup()

math_stem_outliers_min_n <- math_stem_outliers %>%
  filter(n_students_school >= min_students_for_top_list) %>%
  group_by(value_type) %>%
  slice_max(abs_stem_residual, n = top_n_schools, with_ties = FALSE) %>%
  ungroup()

write_csv(math_stem_outliers, file.path(csv_dir, "math_stem_largest_residual_schools.csv"))
write_csv(math_stem_outliers_min_n, file.path(csv_dir, "math_stem_largest_residual_schools_min_n.csv"))

# ------------------------- Region and school-type summaries -------------------------

if (!is.null(metadata)) {
  metadata_match_summary <- tibble(
    n_schools = nrow(wide_values),
    n_with_region = sum(!is.na(wide_values$region_name)),
    n_with_dependency_group = sum(!is.na(wide_values$dependency_group_code)),
    n_with_dependency_detail = sum(!is.na(wide_values$dependency_detail_code))
  )

  write_csv(metadata_match_summary, file.path(csv_dir, "metadata_match_summary.csv"))

  by_region <- score_adjustment %>%
    filter(!is.na(region_name)) %>%
    group_by(region_name, outcome) %>%
    summarise(
      n_schools = n(),
      mean_raw = mean(raw_mean, na.rm = TRUE),
      mean_adjusted = mean(adjusted_mean, na.rm = TRUE),
      mean_raw_centered = mean(raw_mean_centered, na.rm = TRUE),
      mean_adjusted_centered = mean(controlled_value_added_centered, na.rm = TRUE),
      mean_percentile_change = mean(percentile_change, na.rm = TRUE),
      mean_abs_percentile_change = mean(abs_percentile_change, na.rm = TRUE),
      .groups = "drop"
    )

  by_dependency <- score_adjustment %>%
    filter(!is.na(dependency_detail_code)) %>%
    group_by(dependency_detail_code, dependency_detail_label, outcome) %>%
    summarise(
      n_schools = n(),
      mean_raw = mean(raw_mean, na.rm = TRUE),
      mean_adjusted = mean(adjusted_mean, na.rm = TRUE),
      mean_raw_centered = mean(raw_mean_centered, na.rm = TRUE),
      mean_adjusted_centered = mean(controlled_value_added_centered, na.rm = TRUE),
      mean_percentile_change = mean(percentile_change, na.rm = TRUE),
      mean_abs_percentile_change = mean(abs_percentile_change, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(by_region, file.path(csv_dir, "score_adjustment_by_region.csv"))
  write_csv(by_dependency, file.path(csv_dir, "score_adjustment_by_dependency_code.csv"))

  region_plot_data <- wide_values %>%
    filter(!is.na(region_name)) %>%
    transmute(
      region_name,
      math_unadjusted = .data[[value_col("raw_mean_centered", primary_score_outcome)]],
      math_adjusted = .data[[value_col("controlled_value_added_centered", primary_score_outcome)]],
      stem_unadjusted = .data[[value_col("raw_mean_centered", primary_stem_outcome)]],
      stem_adjusted = .data[[value_col("controlled_value_added_centered", primary_stem_outcome)]]
    ) %>%
    pivot_longer(
      cols = c(math_unadjusted, math_adjusted, stem_unadjusted, stem_adjusted),
      names_to = "series",
      values_to = "mean_value"
    ) %>%
    group_by(region_name, series) %>%
    summarise(
      n_schools = sum(!is.na(mean_value)),
      mean_value = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      region_name = factor(region_name, levels = region_order_north_to_south),
      series = factor(
        series,
        levels = c("math_unadjusted", "math_adjusted", "stem_unadjusted", "stem_adjusted"),
        labels = c("Math Unadjusted", "Math Adjusted", "Stem Unadjusted", "Stem Adjusted")
      )
    )

  write_csv(region_plot_data, file.path(csv_dir, "region_math_stem_centered_values.csv"))

  chile_outline_layer <- top_watermark_layer(
    region_plot_data,
    chile_outline_path,
    alpha = 0.16
  )

  region_plot <- region_plot_data %>%
    ggplot(aes(x = region_name, y = mean_value, fill = series)) +
    chile_outline_layer +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.35) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    scale_fill_manual(
      values = c(
        "Math Unadjusted" = "#4c78a8",
        "Math Adjusted" = "#f58518",
        "Stem Unadjusted" = "#54a24b",
        "Stem Adjusted" = "#e45756"
      )
    ) +
    labs(
      x = "Region",
      y = "Mean centered school value",
      fill = NULL,
      title = "Demeaned Math and STEM Values by Region",
      subtitle = "School-weighted centering; regions ordered north to south"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  region_plot_data_no_rm_relative <- wide_values %>%
    filter(!is.na(region_name), region_name != "RM") %>%
    transmute(
      region_name,
      math_unadjusted = .data[[value_col("raw_mean_centered", primary_score_outcome)]],
      math_adjusted = .data[[value_col("controlled_value_added_centered", primary_score_outcome)]],
      stem_unadjusted = .data[[value_col("raw_mean_centered", primary_stem_outcome)]],
      stem_adjusted = .data[[value_col("controlled_value_added_centered", primary_stem_outcome)]]
    ) %>%
    pivot_longer(
      cols = c(math_unadjusted, math_adjusted, stem_unadjusted, stem_adjusted),
      names_to = "series",
      values_to = "mean_value"
    ) %>%
    group_by(series) %>%
    mutate(mean_value = mean_value - mean(mean_value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(region_name, series) %>%
    summarise(
      n_schools = sum(!is.na(mean_value)),
      mean_value = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      region_name = factor(region_name, levels = region_order_north_to_south),
      series = factor(
        series,
        levels = c("math_unadjusted", "math_adjusted", "stem_unadjusted", "stem_adjusted"),
        labels = c("Math Unadjusted", "Math Adjusted", "Stem Unadjusted", "Stem Adjusted")
      )
    )

  write_csv(region_plot_data_no_rm_relative, file.path(csv_dir, "region_math_stem_centered_values_no_rm_relative.csv"))

  chile_outline_layer_no_rm <- top_watermark_layer(
    region_plot_data_no_rm_relative,
    chile_outline_path,
    alpha = 0.16
  )

  region_plot_no_rm_relative <- region_plot_data_no_rm_relative %>%
    ggplot(aes(x = region_name, y = mean_value, fill = series)) +
    chile_outline_layer_no_rm +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.35) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    scale_fill_manual(
      values = c(
        "Math Unadjusted" = "#4c78a8",
        "Math Adjusted" = "#f58518",
        "Stem Unadjusted" = "#54a24b",
        "Stem Adjusted" = "#e45756"
      )
    ) +
    labs(
      x = "Region",
      y = "Mean school value relative to displayed regions",
      fill = NULL,
      title = "Relative Math and STEM Values by Region",
      subtitle = "Centered within displayed regions; RM excluded"
    ) +
    scale_x_discrete(drop = FALSE) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  dependency_plot_data <- wide_values %>%
    filter(!is.na(dependency_detail_label)) %>%
    transmute(
      dependency_detail_label,
      math_unadjusted = .data[[value_col("raw_mean_centered", primary_score_outcome)]],
      math_adjusted = .data[[value_col("controlled_value_added_centered", primary_score_outcome)]],
      stem_unadjusted = .data[[value_col("raw_mean_centered", primary_stem_outcome)]],
      stem_adjusted = .data[[value_col("controlled_value_added_centered", primary_stem_outcome)]]
    ) %>%
    pivot_longer(
      cols = c(math_unadjusted, math_adjusted, stem_unadjusted, stem_adjusted),
      names_to = "series",
      values_to = "mean_value"
    ) %>%
    group_by(dependency_detail_label, series) %>%
    summarise(
      n_schools = sum(!is.na(mean_value)),
      mean_value = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dependency_detail_label = factor(
        dependency_detail_label,
        levels = dependency_detail_labels[dependency_detail_order]
      ),
      series = factor(
        series,
        levels = c("math_unadjusted", "math_adjusted", "stem_unadjusted", "stem_adjusted"),
        labels = c("Math Unadjusted", "Math Adjusted", "Stem Unadjusted", "Stem Adjusted")
      )
    )

  write_csv(dependency_plot_data, file.path(csv_dir, "dependency_math_stem_centered_values.csv"))

  dependency_plot <- dependency_plot_data %>%
    ggplot(aes(x = dependency_detail_label, y = mean_value, fill = series)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.35) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    scale_fill_manual(
      values = c(
        "Math Unadjusted" = "#4c78a8",
        "Math Adjusted" = "#f58518",
        "Stem Unadjusted" = "#54a24b",
        "Stem Adjusted" = "#e45756"
      )
    ) +
    labs(
      x = "School type",
      y = "Mean centered school value",
      fill = NULL,
      title = "Demeaned Math and STEM Values by School Type",
      subtitle = "Detailed COD_DEPE categories with school-weighted centering"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  dependency_plot_no_fee <- dependency_plot_data %>%
    filter(dependency_detail_label != "Particular Pagado") %>%
    ggplot(aes(x = dependency_detail_label, y = mean_value, fill = series)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.35) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    scale_fill_manual(
      values = c(
        "Math Unadjusted" = "#4c78a8",
        "Math Adjusted" = "#f58518",
        "Stem Unadjusted" = "#54a24b",
        "Stem Adjusted" = "#e45756"
      )
    ) +
    labs(
      x = "School type",
      y = "Mean centered school value",
      fill = NULL,
      title = "Demeaned Math and STEM Values by School Type",
      subtitle = "Detailed COD_DEPE categories, excluding Particular Pagado"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  dependency_plot_data_no_fee_relative <- wide_values %>%
    filter(!is.na(dependency_detail_label), dependency_detail_label != "Particular Pagado") %>%
    transmute(
      dependency_detail_label,
      math_unadjusted = .data[[value_col("raw_mean_centered", primary_score_outcome)]],
      math_adjusted = .data[[value_col("controlled_value_added_centered", primary_score_outcome)]],
      stem_unadjusted = .data[[value_col("raw_mean_centered", primary_stem_outcome)]],
      stem_adjusted = .data[[value_col("controlled_value_added_centered", primary_stem_outcome)]]
    ) %>%
    pivot_longer(
      cols = c(math_unadjusted, math_adjusted, stem_unadjusted, stem_adjusted),
      names_to = "series",
      values_to = "mean_value"
    ) %>%
    group_by(series) %>%
    mutate(mean_value = mean_value - mean(mean_value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(dependency_detail_label, series) %>%
    summarise(
      n_schools = sum(!is.na(mean_value)),
      mean_value = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dependency_detail_label = factor(
        dependency_detail_label,
        levels = dependency_detail_labels[setdiff(dependency_detail_order, "4")]
      ),
      series = factor(
        series,
        levels = c("math_unadjusted", "math_adjusted", "stem_unadjusted", "stem_adjusted"),
        labels = c("Math Unadjusted", "Math Adjusted", "Stem Unadjusted", "Stem Adjusted")
      )
    )

  write_csv(dependency_plot_data_no_fee_relative, file.path(csv_dir, "dependency_math_stem_centered_values_no_private_paid_relative.csv"))

  dependency_plot_no_fee_relative <- dependency_plot_data_no_fee_relative %>%
    ggplot(aes(x = dependency_detail_label, y = mean_value, fill = series)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.35) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    scale_fill_manual(
      values = c(
        "Math Unadjusted" = "#4c78a8",
        "Math Adjusted" = "#f58518",
        "Stem Unadjusted" = "#54a24b",
        "Stem Adjusted" = "#e45756"
      )
    ) +
    labs(
      x = "School type",
      y = "Mean school value relative to displayed school types",
      fill = NULL,
      title = "Relative Math and STEM Values by School Type",
      subtitle = "Centered within displayed school types; excluding Particular Pagado"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  write_plot(region_plot, "math_values_by_region_raw_adjusted.png", width = 10, height = 5.5)
  write_plot(region_plot_no_rm_relative, "math_values_by_region_no_rm_relative.png", width = 10, height = 5.5)
  write_plot(dependency_plot, "score_adjustment_by_dependency_code.png", width = 8, height = 5)
  write_plot(dependency_plot_no_fee, "score_adjustment_by_dependency_code_no_private_paid.png", width = 8, height = 5)
  write_plot(dependency_plot_no_fee_relative, "score_adjustment_by_dependency_code_no_private_paid_relative.png", width = 8, height = 5)
}

message("Wrote figures to: ", figures_dir)
message("Wrote CSV diagnostics to: ", csv_dir)
