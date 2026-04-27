suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# Build a compact LaTeX table for the main scalar school-value IV estimates.

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
  "scalar_school_value_iv_main_results.tex"
)
output_csv_path <- file.path(
  results_dir,
  "scalar_school_value_iv_main_results.csv"
)

main_specs <- tibble::tribble(
  ~spec,          ~outcome_group, ~adjustment,   ~column_id,
  "math_unadj",  "Math",         "Unadjusted",  "math_unadj",
  "math_adj",    "Math",         "Adjusted",    "math_adj",
  "leng_unadj",  "Language",     "Unadjusted",  "leng_unadj",
  "leng_adj",    "Language",     "Adjusted",    "leng_adj",
  "stem_unadj",  "STEM",         "Unadjusted",  "stem_unadj",
  "stem_adj",    "STEM",         "Adjusted",    "stem_adj"
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
  filter(group == "all", spec %in% main_specs$spec) %>%
  select(spec, beta, se, p_value) %>%
  right_join(main_specs, by = "spec") %>%
  arrange(match(spec, main_specs$spec))

if (any(is.na(results$beta))) {
  missing_specs <- results %>%
    filter(is.na(beta)) %>%
    pull(spec)
  stop("Missing requested IV result spec(s): ", paste(missing_specs, collapse = ", "), call. = FALSE)
}

table_csv <- results %>%
  transmute(
    spec,
    outcome_group,
    adjustment,
    beta,
    se,
    p_value
  )

write_csv(table_csv, output_csv_path)

display_wide <- bind_rows(
  results %>%
    transmute(stat = "$\\theta$", column_id, value = format_estimate(beta)),
  results %>%
    transmute(stat = "SE", column_id, value = format_estimate(se)),
  results %>%
    transmute(stat = "p-value", column_id, value = format_p_value(p_value))
) %>%
  mutate(
    stat = factor(stat, levels = c("$\\theta$", "SE", "p-value")),
    column_id = factor(column_id, levels = main_specs$column_id)
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
    as.character(row[["math_unadj"]]),
    as.character(row[["math_adj"]]),
    as.character(row[["leng_unadj"]]),
    as.character(row[["leng_adj"]]),
    as.character(row[["stem_unadj"]]),
    as.character(row[["stem_adj"]])
  )
}

latex_rows <- vapply(seq_len(nrow(display_wide)), function(i) {
  paste(row_values(display_wide[i, ]), collapse = " & ")
}, character(1))

latex_table <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Scalar school-value IV estimates}",
  "\\label{tab:scalar_school_value_iv_main_results}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Math} & \\multicolumn{2}{c}{Language} & \\multicolumn{2}{c}{STEM} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  " & Unadjusted & Adjusted & Unadjusted & Adjusted & Unadjusted & Adjusted \\\\",
  "\\midrule",
  paste0(latex_rows, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(latex_table, output_tex_path)

message("Wrote: ", output_tex_path)
message("Wrote: ", output_csv_path)
