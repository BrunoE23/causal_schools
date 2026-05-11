suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# Build a compact LaTeX table for the school funding scalar IV estimates.

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
)

results_dir <- file.path(repo_wd, "output/tables/school_expenditure_values")
results_path <- file.path(
  results_dir,
  "money_scalar_iv_results_k100_timely_risk_prob_iszero.csv"
)
output_tex_path <- file.path(
  results_dir,
  "money_scalar_iv_results.tex"
)
output_csv_path <- file.path(
  results_dir,
  "money_scalar_iv_results_table.csv"
)

money_specs <- tibble::tribble(
  ~spec,                 ~measure_group,                  ~outcome_group, ~column_id,
  "math_money_level",    "Funding per student",           "Math",         "level_math",
  "leng_money_level",    "Funding per student",           "Language",     "level_leng",
  "stem_money_level",    "Funding per student",           "STEM",         "level_stem",
  "math_money_growth",   "Growth in funding per student", "Math",         "growth_math",
  "leng_money_growth",   "Growth in funding per student", "Language",     "growth_leng",
  "stem_money_growth",   "Growth in funding per student", "STEM",         "growth_stem"
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
  filter(spec %in% money_specs$spec) %>%
  select(spec, beta, se, p_value, n_obs, fs_beta, fs_se, fs_f) %>%
  right_join(money_specs, by = "spec") %>%
  arrange(match(spec, money_specs$spec))

if (any(is.na(results$beta))) {
  missing_specs <- results %>%
    filter(is.na(beta)) %>%
    pull(spec)
  stop("Missing requested IV result spec(s): ", paste(missing_specs, collapse = ", "), call. = FALSE)
}

table_csv <- results %>%
  transmute(
    spec,
    measure_group,
    outcome_group,
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
    column_id = factor(column_id, levels = money_specs$column_id)
  ) %>%
  arrange(stat, column_id) %>%
  pivot_wider(names_from = column_id, values_from = value) %>%
  arrange(stat)

row_values <- function(row) {
  c(
    as.character(row[["stat"]]),
    as.character(row[["level_math"]]),
    as.character(row[["level_leng"]]),
    as.character(row[["level_stem"]]),
    as.character(row[["growth_math"]]),
    as.character(row[["growth_leng"]]),
    as.character(row[["growth_stem"]])
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
  "\\caption{School funding per person scalar IV estimates, millions of 2021 pesos}",
  "\\label{tab:money_scalar_iv_results}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Funding per student} & \\multicolumn{3}{c}{Growth in funding per student} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  " & Math & Language & STEM & Math & Language & STEM \\\\",
  "\\midrule",
  latex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(latex_table, output_tex_path)

message("Wrote: ", output_tex_path)
message("Wrote: ", output_csv_path)
