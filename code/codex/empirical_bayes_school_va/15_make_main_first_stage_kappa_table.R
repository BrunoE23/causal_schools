# Build a paper-facing first-stage table from the exact regressions underlying
# the main seven-outcome EB scalar-IV table.

suppressPackageStartupMessages({
  library(data.table)
})

repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
pair_tag <- Sys.getenv(
  "EB_IV_PAIR_TAG",
  unset = "va_2017_2020__sae_2018_2020"
)
table_dir <- file.path(
  repo_wd, "output/tables/empirical_bayes_school_va", pair_tag
)
input_path <- Sys.getenv(
  "EB_IV_MAIN_RESULTS_INPUT",
  unset = file.path(table_dir, "main_table.csv")
)
csv_path <- Sys.getenv(
  "EB_IV_KAPPA_TABLE_CSV",
  unset = file.path(table_dir, "first_stage_kappa_table.csv")
)
tex_path <- Sys.getenv(
  "EB_IV_KAPPA_TABLE_TEX",
  unset = file.path(table_dir, "first_stage_kappa_table.tex")
)

results <- fread(input_path)
required <- c("spec", "outcome_group", "n_obs", "fs_beta", "fs_se", "fs_f")
stopifnot(all(required %in% names(results)))

order_specs <- c(
  "math_adj_eb", "leng_adj_eb", "exam_adj_eb", "anypost_adj_eb",
  "highered_adj_eb", "highinst_adj_eb", "highpay_adj_eb"
)
results <- results[match(order_specs, spec)]
stopifnot(nrow(results) == 7L, !anyNA(results$spec))

results[, fs_p_value := 2 * pnorm(-abs(fs_beta / fs_se))]
stars <- function(p) {
  fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.10, "*", "")))
}
results[, kappa_cell := paste0(sprintf("%.3f", fs_beta), stars(fs_p_value))]

table_out <- results[, .(
  spec,
  outcome_group,
  kappa = fs_beta,
  se = fs_se,
  p_value = fs_p_value,
  first_stage_f = fs_f,
  n_obs
)]
fwrite(table_out, csv_path)

kappa_row <- paste0("$\\kappa^{EB}$ & ", paste(results$kappa_cell, collapse = " & "), " \\\\")
se_row <- paste0(" & ", paste(sprintf("(%.3f)", results$fs_se), collapse = " & "), " \\\\")
f_row <- paste0("First-stage F & ", paste(sprintf("%.1f", results$fs_f), collapse = " & "), " \\\\")
n_row <- paste0("N & ", paste(format(results$n_obs, big.mark = ",", trim = TRUE), collapse = " & "), " \\\\")

tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{First-stage coefficients for the main EB school-value specifications}",
  "\\label{tab:scalar-school-value-iv-main-seven-eb-first-stage}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Achievement} & \\multicolumn{3}{c}{Postsecondary access} & \\multicolumn{2}{c}{Within higher-education choice} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-6} \\cmidrule(lr){7-8}",
  " & Math & Verbal & Exam taking & Fin. aid app. & Higher-ed enrollment & High-premium inst. & High-premium field \\\\",
  "\\midrule",
  kappa_row,
  se_row,
  f_row,
  n_row,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "\\textit{Notes:} Each column regresses attended-school EB value added on first-round offered-school EB value added using the corresponding Table~5 sample and controls. High-premium-institution and field specifications are restricted to exam takers, with non-enrollees coded zero. Robust standard errors are in parentheses; the $F$ statistic tests the excluded instrument. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(tex, tex_path)

print(table_out)
message("Wrote: ", csv_path)
message("Wrote: ", tex_path)
