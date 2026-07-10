###############################################################################
# Distribution of program_income_full for a normalized math/verbal score cell
#
# Default cell: 1.4 <= z_math <= 1.6 and 1.4 <= z_verbal <= 1.6.
# Scores are normalized within psu_year, matching
# 06_make_math_verbal_score_heatmap.R.
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

lower <- as.numeric(Sys.getenv("SCORE_CELL_LOWER", unset = "1.4"))
upper <- as.numeric(Sys.getenv("SCORE_CELL_UPPER", unset = "1.6"))
cell_label <- paste0(gsub("\\.", "p", lower), "_", gsub("\\.", "p", upper))

figure_path <- file.path(
  figure_dir,
  paste0("program_income_full_distribution_math_verbal_", cell_label, ".png")
)
summary_path <- file.path(
  table_dir,
  paste0("program_income_full_distribution_math_verbal_", cell_label, "_summary.csv")
)
source_path <- file.path(
  table_dir,
  paste0("program_income_full_distribution_math_verbal_", cell_label, "_source_counts.csv")
)
bins_path <- file.path(
  table_dir,
  paste0("program_income_full_distribution_math_verbal_", cell_label, "_bins.csv")
)

message("Reading score columns: ", universe_path)
scores <- fread(
  universe_path,
  select = c("mrun", "psu_year", "math_max", "leng_max"),
  na.strings = c("", "NA")
)
scores[, mrun := as.numeric(mrun)]
scores <- scores[
  !is.na(psu_year) &
    !is.na(math_max) & math_max > 0 &
    !is.na(leng_max) & leng_max > 0
]
scores[, `:=`(
  z_math = z_within_group(math_max),
  z_verbal = z_within_group(leng_max)
), by = psu_year]
scores <- scores[
  complete.cases(z_math, z_verbal) &
    z_math >= lower & z_math <= upper &
    z_verbal >= lower & z_verbal <= upper
]

message("Reading program_income_full columns: ", program_income_path)
income <- fread(
  program_income_path,
  select = c(
    "mrun",
    "program_income_full_clp_m1",
    "log_program_income_full_clp_m1",
    "program_income_full_source_m1",
    "matriculated_m1"
  ),
  na.strings = c("", "NA")
)
income[, mrun := as.numeric(mrun)]
if (anyDuplicated(income$mrun) > 0) {
  stop("Program income file is not unique by mrun.", call. = FALSE)
}

dt <- merge(scores, income, by = "mrun", all.x = TRUE, sort = FALSE)
dt <- dt[!is.na(program_income_full_clp_m1)]
dt[, program_income_full_millions := program_income_full_clp_m1 / 1e6]

summary_dt <- dt[, .(
  n_students = .N,
  n_years = uniqueN(psu_year),
  min_psu_year = min(psu_year),
  max_psu_year = max(psu_year),
  lower_z_bound = lower,
  upper_z_bound = upper,
  mean_clp = mean(program_income_full_clp_m1),
  sd_clp = stats::sd(program_income_full_clp_m1),
  p01_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.01)),
  p05_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.05)),
  p10_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.10)),
  p25_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.25)),
  p50_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.50)),
  p75_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.75)),
  p90_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.90)),
  p95_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.95)),
  p99_clp = as.numeric(stats::quantile(program_income_full_clp_m1, 0.99)),
  min_clp = min(program_income_full_clp_m1),
  max_clp = max(program_income_full_clp_m1),
  share_matriculated = mean(matriculated_m1 == 1, na.rm = TRUE),
  share_at_min_wage_floor = mean(program_income_full_clp_m1 == 553553, na.rm = TRUE)
)]

source_counts <- dt[, .(
  n_students = .N,
  share = .N / nrow(dt)
), by = .(program_income_full_source_m1, matriculated_m1)]
setorder(source_counts, -n_students)

bin_width <- as.numeric(Sys.getenv("PROGRAM_INCOME_BIN_WIDTH_MILLIONS", unset = "0.05"))
bins <- dt[, .(
  n_students = .N
), by = .(
  program_income_full_millions_bin = floor(program_income_full_millions / bin_width) * bin_width + bin_width / 2
)]
setorder(bins, program_income_full_millions_bin)

message("Writing summary: ", summary_path)
fwrite(summary_dt, summary_path)
message("Writing source counts: ", source_path)
fwrite(source_counts, source_path)
message("Writing histogram bins: ", bins_path)
fwrite(bins, bins_path)

plot <- ggplot(dt, aes(x = program_income_full_millions)) +
  geom_histogram(binwidth = bin_width, boundary = 0, fill = "#2f6f9f", color = "white", linewidth = 0.15) +
  geom_vline(xintercept = 553553 / 1e6, color = "#a33b20", linewidth = 0.55, linetype = "dashed") +
  scale_y_continuous(labels = comma_label) +
  labs(
    title = "Program Income Full Distribution",
    subtitle = paste0(
      "Students with both normalized math and verbal scores in [",
      lower,
      ", ",
      upper,
      "]"
    ),
    x = "Program income full, CLP millions",
    y = "Students"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

message("Writing figure: ", figure_path)
ggsave(figure_path, plot, width = 7.5, height = 5.25, dpi = 300)

print(summary_dt)
print(source_counts)
