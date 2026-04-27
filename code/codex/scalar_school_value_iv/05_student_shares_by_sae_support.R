suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# Student shares by attended school group.
#
# Attendance is defined using most_time_RBD in the broad grade-8 universe.
# SAE groups use the number of students with positive simulated assignment
# probability from the probability-support diagnostics.

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
)

universe_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
support_path <- file.path(
  data_wd,
  "data/clean/DA_probs"
)
school_directory_root <- file.path(data_wd, "data/raw/school_directory")
probability_years <- 2018:2021

output_dir <- file.path(repo_wd, "output/tables/scalar_school_value_iv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_path <- file.path(output_dir, "student_shares_by_sae_support_thresholds.csv")
output_tex_path <- file.path(output_dir, "student_shares_by_sae_support_thresholds.tex")

read_da_prob_file <- function(year) {
  path <- file.path(support_path, paste0("DA_probs_", year, ".csv"))

  read_csv(
    path,
    col_types = cols(
      .default = col_skip(),
      student_id = col_double(),
      school_id = col_character(),
      prob = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(
      school_id = if_else(school_id == "unmatched", paste0("unmatched_", year), school_id),
      sae_proceso = as.integer(coalesce(str_extract(school_id, "\\d{4}$"), as.character(year))),
      rbd_prob = trimws(as.character(sub("_.*", "", school_id)))
    ) %>%
    select(student_id, sae_proceso, rbd_prob, prob)
}

latest_school_directory <- function(root) {
  if (!dir.exists(root)) {
    stop("School directory root does not exist: ", root, call. = FALSE)
  }

  files <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    stop("No school directory CSVs found under: ", root, call. = FALSE)
  }

  years <- str_extract(files, "\\b20[0-9]{2}\\b") %>% as.integer()
  files[which.max(years)]
}

read_private_paid_schools <- function(root) {
  directory_file <- latest_school_directory(root)

  metadata <- read_delim(
    directory_file,
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "Latin1")
  )

  metadata %>%
    transmute(
      school_rbd = suppressWarnings(as.numeric(RBD)),
      private_paid = case_when(
        "COD_DEPE" %in% names(metadata) ~ as.character(COD_DEPE) == "4",
        "COD_DEPE2" %in% names(metadata) ~ as.character(COD_DEPE2) == "3",
        TRUE ~ FALSE
      )
    ) %>%
    filter(!is.na(school_rbd)) %>%
    distinct(school_rbd, .keep_all = TRUE)
}

student_universe <- read_csv(universe_path, show_col_types = FALSE) %>%
  transmute(
    mrun,
    student_id,
    cohort_gr8,
    sae_proceso,
    timely_sae,
    school_rbd = suppressWarnings(as.numeric(most_time_RBD))
  )

# Recompute all positive-probability SAE school counts, before applying k.
timely_universe <- student_universe %>%
  filter(timely_sae == 1L) %>%
  distinct(student_id, .keep_all = TRUE)

prob_files <- tibble(sae_proceso = probability_years) %>%
  mutate(path = file.path(support_path, paste0("DA_probs_", sae_proceso, ".csv"))) %>%
  filter(file.exists(path))

probs_all <- bind_rows(lapply(prob_files$sae_proceso, read_da_prob_file))

probs_estimation <- probs_all %>%
  inner_join(
    timely_universe %>%
      select(student_id, sae_proceso),
    by = c("student_id", "sae_proceso")
  ) %>%
  group_by(student_id, sae_proceso, rbd_prob) %>%
  summarise(prob_raw = sum(prob, na.rm = TRUE), .groups = "drop")

risk_flags <- probs_estimation %>%
  group_by(student_id, sae_proceso) %>%
  summarise(
    any_risk = as.integer(max(prob_raw, na.rm = TRUE) < 1),
    .groups = "drop"
  )

sae_counts <- probs_estimation %>%
  semi_join(
    risk_flags %>% filter(any_risk == 1L) %>% select(student_id, sae_proceso),
    by = c("student_id", "sae_proceso")
  ) %>%
  filter(prob_raw > 0) %>%
  group_by(rbd_prob) %>%
  summarise(
    n_students_positive_prob = n_distinct(student_id),
    .groups = "drop"
  ) %>%
  transmute(
    school_rbd = suppressWarnings(as.numeric(rbd_prob)),
    n_students_positive_prob
  ) %>%
  filter(!is.na(school_rbd)) %>%
  distinct(school_rbd, .keep_all = TRUE)

private_paid_schools <- read_private_paid_schools(school_directory_root)

base_df <- student_universe %>%
  left_join(sae_counts, by = "school_rbd") %>%
  left_join(private_paid_schools, by = "school_rbd") %>%
  mutate(
    private_paid = coalesce(private_paid, FALSE),
    in_sae_support = !is.na(n_students_positive_prob)
  )

threshold_summary <- function(data, threshold_value) {
  data %>%
    mutate(
      threshold_students_at_risk = threshold_value,
      school_group = case_when(
        in_sae_support & n_students_positive_prob < threshold_value ~ paste0("SAE <", threshold_value),
        in_sae_support & n_students_positive_prob >= threshold_value ~ paste0("SAE >=", threshold_value),
        private_paid ~ "Particular Pagado",
        TRUE ~ "Other non-SAE schools"
      ),
      school_group = factor(
        school_group,
        levels = c(
          paste0("SAE <", threshold_value),
          paste0("SAE >=", threshold_value),
          "Particular Pagado",
          "Other non-SAE schools"
        )
      )
    ) %>%
    group_by(threshold_students_at_risk, school_group) %>%
    summarise(
      n_schools = n_distinct(school_rbd, na.rm = TRUE),
      n_students = n(),
      .groups = "drop"
    ) %>%
    mutate(
      share = n_students / sum(n_students),
      pct = 100 * share
    )
}

summary_df <- bind_rows(
  threshold_summary(base_df, 100),
  threshold_summary(base_df, 25)
)

write_csv(summary_df, output_path)

latex_escape <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\\\", "\\\\textbackslash{}") %>%
    str_replace_all("([_&%$#{}])", "\\\\\\1") %>%
    str_replace_all("~", "\\\\textasciitilde{}") %>%
    str_replace_all("\\^", "\\\\textasciicircum{}")
}

latex_df <- summary_df %>%
  transmute(
    `Threshold students at risk` = as.character(threshold_students_at_risk),
    `School group` = as.character(school_group),
    `Schools` = format(n_schools, big.mark = ",", scientific = FALSE, trim = TRUE),
    `Students` = format(n_students, big.mark = ",", scientific = FALSE, trim = TRUE),
    `Share` = paste0(sprintf("%.1f", pct), "\\%")
  )

latex_rows <- vapply(seq_len(nrow(latex_df)), function(i) {
  row <- latex_df[i, ]
  line <- paste(
    latex_escape(row[["Threshold students at risk"]]),
    latex_escape(row[["School group"]]),
    row[["Schools"]],
    row[["Students"]],
    row[["Share"]],
    sep = " & "
  )

  if (i == 5) {
    line <- paste("\\midrule", line, sep = "\n")
  }

  line
}, character(1))

latex_rows <- vapply(latex_rows, function(row) {
  paste(
    row,
    "\\\\"
  )
}, character(1))

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Student shares by SAE support threshold and school type}",
  "\\label{tab:student_shares_by_sae_support_thresholds}",
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "Threshold students at risk & School group & Schools & Students & Share \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(latex_table, output_tex_path)

print(summary_df)
message("Total students: ", nrow(base_df))
message("Wrote: ", output_path)
message("Wrote: ", output_tex_path)
