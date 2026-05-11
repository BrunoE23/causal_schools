suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# Build a compact LaTeX table for STEM enrollment IV estimates by gender.

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
)

results_dir <- file.path(repo_wd, "output/tables/scalar_school_value_iv")
results_path <- file.path(
  results_dir,
  "scalar_school_value_iv_results_k100_timely_risk_prob_iszero.csv"
)
output_tex_path <- file.path(
  results_dir,
  "scalar_school_value_iv_stem_gender_results.tex"
)
output_csv_path <- file.path(
  results_dir,
  "scalar_school_value_iv_stem_gender_results.csv"
)

table_specs <- tibble::tribble(
  ~spec,           ~group, ~column_group,           ~sample,  ~column_id,
  "stem_adj",      "all",  "STEM VA",               "All",    "stem_adj_all",
  "stem_adj",      "boys", "STEM VA",               "Boys",   "stem_adj_boys",
  "stem_adj",      "girls","STEM VA",               "Girls",  "stem_adj_girls",
  "stem_gap_adj",  "all",  "STEM gender gap reduc. VA",    "All",    "stem_gap_all",
  "stem_gap_adj",  "boys", "STEM gender gap reduc. VA",    "Boys",   "stem_gap_boys",
  "stem_gap_adj",  "girls","STEM gender gap reduc. VA",    "Girls",  "stem_gap_girls"
)

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

format_p_value <- function(x) {
  case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "$<0.001$",
    TRUE ~ sprintf("%.3f", x)
  )
}

results <- read_csv(results_path, show_col_types = FALSE) %>%
  select(spec, group, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f) %>%
  right_join(table_specs, by = c("spec", "group")) %>%
  arrange(match(column_id, table_specs$column_id))

if (any(is.na(results$beta))) {
  missing_specs <- results %>%
    filter(is.na(beta)) %>%
    transmute(label = paste(spec, group, sep = " / ")) %>%
    pull(label)
  stop("Missing requested IV result(s): ", paste(missing_specs, collapse = ", "), call. = FALSE)
}

table_csv <- results %>%
  transmute(
    spec,
    group,
    column_group,
    sample,
    beta,
    se,
    p_value,
    n_obs,
    fs_beta,
    fs_se,
    fs_f
  )

write_csv(table_csv, output_csv_path)

display_wide <- bind_rows(
  results %>%
    transmute(stat = "$\\theta$", column_id, value = format_estimate(beta)),
  results %>%
    transmute(stat = "SE", column_id, value = format_estimate(se)),
  results %>%
    transmute(stat = "p-value", column_id, value = format_p_value(p_value)),
  results %>%
    transmute(stat = "First-stage coef.", column_id, value = format_estimate(fs_beta)),
  results %>%
    transmute(stat = "First-stage SE", column_id, value = format_estimate(fs_se)),
  results %>%
    transmute(stat = "First-stage F", column_id, value = format_estimate(fs_f)),
  results %>%
    transmute(stat = "N", column_id, value = format(n_obs, big.mark = ",", scientific = FALSE, trim = TRUE))
) %>%
  mutate(
    stat = factor(
      stat,
      levels = c(
        "$\\theta$",
        "SE",
        "p-value",
        "First-stage coef.",
        "First-stage SE",
        "First-stage F",
        "N"
      )
    ),
    column_id = factor(column_id, levels = table_specs$column_id)
  ) %>%
  arrange(stat, column_id) %>%
  pivot_wider(names_from = column_id, values_from = value) %>%
  arrange(stat)

latex_escape <- function(x) {
  x %>%
    as.character() %>%
    gsub("\\\\", "\\\\textbackslash{}", ., fixed = TRUE) %>%
    gsub("([_&%$#{}])", "\\\\\\1", ., perl = TRUE) %>%
    gsub("~", "\\\\textasciitilde{}", ., fixed = TRUE) %>%
    gsub("\\^", "\\\\textasciicircum{}", ., perl = TRUE)
}

row_values <- function(row) {
  c(
    as.character(row[["stat"]]),
    as.character(row[["stem_adj_all"]]),
    as.character(row[["stem_adj_boys"]]),
    as.character(row[["stem_adj_girls"]]),
    as.character(row[["stem_gap_all"]]),
    as.character(row[["stem_gap_boys"]]),
    as.character(row[["stem_gap_girls"]])
  )
}

latex_rows <- unlist(lapply(seq_len(nrow(display_wide)), function(i) {
  row <- paste(row_values(display_wide[i, ]), collapse = " & ")

  if (as.character(display_wide$stat[[i]]) == "p-value") {
    return(c(paste0(row, " \\\\"), "\\midrule"))
  }

  paste0(row, " \\\\")
}), use.names = FALSE)

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{STEM enrollment scalar school-value IV estimates by gender}",
  "\\label{tab:scalar_school_value_iv_stem_gender_results}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{STEM VA} & \\multicolumn{3}{c}{STEM gender gap reduc. VA} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  " & All & Boys & Girls & All & Boys & Girls \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(latex_table, output_tex_path)

message("Wrote: ", output_tex_path)
message("Wrote: ", output_csv_path)
