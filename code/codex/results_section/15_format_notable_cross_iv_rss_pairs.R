# Format the strongest RSS-implied off-diagonal gains and their IV validation.
suppressPackageStartupMessages(library(data.table))

repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
out_dir <- file.path(repo_wd, "output/tables/rss_debiased_varcov")
input_path <- file.path(out_dir, "unshrunk_cross_outcome_iv_rss_validation.csv")

x <- fread(input_path)[from_key != to_key]
x[, validation_ratio := direct_iv / predicted_causal_gain]

# These pairs are chosen from the economic relationships of interest, without
# reference to the magnitude, sign, or precision of either estimator.
selection <- data.table(
  selection_rank = seq_len(10L),
  conceptual_link = rep(c(
    "Academic complementarities",
    "Academic preparation and access",
    "Postsecondary complementarities",
    "Postsecondary quality and earnings",
    "Access and financial aid"
  ), each = 2L),
  from_key = c(
    "math", "verbal",
    "math", "verbal",
    "highinst", "highpay",
    "highinst", "highpay",
    "exam", "aid"
  ),
  to_key = c(
    "verbal", "math",
    "exam", "exam",
    "highpay", "highinst",
    "income", "income",
    "aid", "exam"
  )
)
selected <- merge(selection, x, by = c("from_key", "to_key"), all.x = TRUE, sort = FALSE)
setorder(selected, selection_rank)
stopifnot(nrow(selected) == nrow(selection), !anyNA(selected$direct_iv))

csv_path <- file.path(out_dir, "unshrunk_cross_outcome_iv_rss_notable_pairs.csv")
fwrite(
  selected[, .(
    selection_rank, conceptual_link, from_key, from_label, to_key, to_label,
    rss_projection = beta, rss_projection_bootstrap_se = bootstrap_se,
    predicted_causal_gain, direct_iv, direct_se, validation_ratio,
    difference, first_stage_f, n_obs
  )],
  csv_path
)

fmt <- function(z) sprintf("%.2f", z)
escape_latex <- function(z) {
  z <- gsub("&", "\\\\&", z, fixed = TRUE)
  gsub("%", "\\\\%", z, fixed = TRUE)
}

body_rows <- unlist(lapply(seq_len(nrow(selected)), function(i) {
  z <- selected[i]
  pair <- paste0(escape_latex(z$from_label), " $\\rightarrow$ ", escape_latex(z$to_label))
  c(
    paste0(
      escape_latex(z$conceptual_link), " & ", pair, " & ", fmt(z$beta), " & ", fmt(z$predicted_causal_gain),
      " & ", fmt(z$direct_iv), " & ", fmt(z$validation_ratio), " \\\\"
    ),
    paste0(" &  & (", fmt(z$bootstrap_se), ") &  & (", fmt(z$direct_se), ") &  \\\\")
  )
}))

tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Selected cross-outcome relationships and direct IV validation}",
  "\\label{tab:unshrunk-cross-iv-rss-notable-pairs}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{llcccc}",
  "\\toprule",
  "Conceptual link & School VA $\\rightarrow$ outcome & RSS projection & RSS-implied gain & Direct IV & Direct / implied \\\\",
  "\\midrule",
  body_rows,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\par\\medskip",
  "\\footnotesize",
  "\\begin{minipage}{\\textwidth}",
  paste0(
    "Notes: The table reports ten off-diagonal pairs chosen a priori to represent five economically relevant links: academic complementarities, academic preparation and access, postsecondary complementarities, postsecondary quality and earnings, and access and financial aid. ",
    "The RSS projection maps the row value-added measure into the column value-added measure; bootstrap standard errors are in parentheses. ",
    "The implied gain multiplies that projection by the destination outcome's unshrunk same-outcome IV pass-through. ",
    "The direct IV column estimates the cross-outcome effect directly, with regression standard errors in parentheses. ",
    "A direct-to-implied ratio of one denotes exact agreement. Pair selection does not use the magnitude, sign, or precision of either estimate."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

tex_path <- file.path(out_dir, "unshrunk_cross_outcome_iv_rss_notable_pairs.tex")
writeLines(tex, tex_path)
cat("Saved:", csv_path, "\n")
cat("Saved:", tex_path, "\n")
