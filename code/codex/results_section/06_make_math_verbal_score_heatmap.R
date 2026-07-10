###############################################################################
# Heatmap of normalized math and verbal admission scores
#
# Heat is student frequency. Scores are normalized within psu_year using
# students with positive observed math and verbal scores.
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

z_within_group <- function(x) {
  sigma <- stats::sd(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sigma
}

comma_label <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE)
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
figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
table_dir <- file.path(repo_wd, "output", "tables", "results_section")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(figure_dir, "math_verbal_normalized_score_heatmap.png")
bins_path <- file.path(table_dir, "math_verbal_normalized_score_heatmap_bins.csv")
summary_path <- file.path(table_dir, "math_verbal_normalized_score_heatmap_summary.csv")

message("Reading score columns: ", universe_path)
scores <- fread(
  universe_path,
  select = c("student_id", "psu_year", "math_max", "leng_max"),
  na.strings = c("", "NA")
)

scores <- scores[
  !is.na(psu_year) &
    !is.na(math_max) & math_max > 0 &
    !is.na(leng_max) & leng_max > 0
]
scores[, `:=`(
  z_math = z_within_group(math_max),
  z_verbal = z_within_group(leng_max)
), by = psu_year]
scores <- scores[complete.cases(z_math, z_verbal)]

bin_width <- as.numeric(Sys.getenv("SCORE_HEATMAP_BIN_WIDTH", unset = "0.1"))
scores[, `:=`(
  z_math_bin = floor(z_math / bin_width) * bin_width + bin_width / 2,
  z_verbal_bin = floor(z_verbal / bin_width) * bin_width + bin_width / 2
)]

bins <- scores[, .(
  n_students = .N
), by = .(z_math_bin, z_verbal_bin)]
setorder(bins, z_math_bin, z_verbal_bin)

summary_dt <- data.table(
  n_students = nrow(scores),
  n_years = uniqueN(scores$psu_year),
  min_psu_year = min(scores$psu_year),
  max_psu_year = max(scores$psu_year),
  bin_width = bin_width,
  corr_z_math_z_verbal = stats::cor(scores$z_math, scores$z_verbal),
  min_z_math = min(scores$z_math),
  max_z_math = max(scores$z_math),
  min_z_verbal = min(scores$z_verbal),
  max_z_verbal = max(scores$z_verbal)
)

message("Writing binned frequencies: ", bins_path)
fwrite(bins, bins_path)
message("Writing summary: ", summary_path)
fwrite(summary_dt, summary_path)

plot <- ggplot(scores, aes(x = z_math, y = z_verbal)) +
  geom_bin2d(binwidth = c(bin_width, bin_width), color = NA) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.35, color = "white", alpha = 0.75) +
  scale_fill_gradientn(
    colors = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    labels = comma_label,
    name = "Students"
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = "Normalized Math and Verbal Scores",
    subtitle = "Heat is student frequency; scores normalized within admission year",
    x = "Math score, z-score within year",
    y = "Verbal score, z-score within year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

message("Writing figure: ", figure_path)
ggsave(figure_path, plot, width = 7.5, height = 6.5, dpi = 300)

print(summary_dt)
