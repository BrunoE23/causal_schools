suppressPackageStartupMessages({
  library(data.table)
})

# Paper-facing results objects for writing/may_draft/may_draft.tex.
# Uses existing cleaned outputs; does not rebuild VA or IV inputs.

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
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
scalar_dir <- file.path(clean_dir, "scalar_school_value_iv")
orthogonal_dir <- file.path(clean_dir, "orthogonal_school_va")

table_dir <- file.path(repo_wd, "output", "tables", "results_section")
figure_dir <- file.path(repo_wd, "output", "figures", "results_section")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

scalar_results_path <- file.path(
  repo_wd,
  "output",
  "tables",
  "scalar_school_value_iv",
  "scalar_school_value_iv_results_expected_va.csv"
)
expected_va_path <- file.path(
  scalar_dir,
  "scalar_school_value_iv_expected_va.csv"
)
orthogonal_va_path <- file.path(
  orthogonal_dir,
  "school_orthogonal_va_exam_takers.csv"
)

main_table_csv <- file.path(table_dir, "main_four_va_metrics.csv")
main_table_tex <- file.path(table_dir, "main_four_va_metrics.tex")
heterogeneity_csv <- file.path(table_dir, "simce4_math_quintile_four_metrics.csv")
heterogeneity_plot <- file.path(
  figure_dir,
  "simce4_math_quintile_four_metrics.png"
)
correlation_plot <- file.path(
  figure_dir,
  "math_stem_va_correlation.png"
)

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

format_se <- function(x) {
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
}

specs <- data.table(
  spec = c("math_adj", "leng_adj", "stem_adj", "instcert_adj"),
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "stem_enrollment_m1",
    "inst_certified_years_m1"
  ),
  expected = c(
    "expected_math_adj",
    "expected_leng_adj",
    "expected_stem_adj",
    "expected_instcert_adj"
  ),
  treatment = c(
    "d_math_adj",
    "d_leng_adj",
    "d_stem_adj",
    "d_instcert_adj"
  ),
  instrument = c(
    "z_math_adj",
    "z_leng_adj",
    "z_stem_adj",
    "z_instcert_adj"
  ),
  label = c(
    "Math",
    "Language",
    "STEM enrollment",
    "Institutional quality"
  ),
  plot_label = c(
    "Math VA",
    "Language VA",
    "STEM VA",
    "Inst. quality VA"
  ),
  family = c("Achievement", "Achievement", "Higher education", "Higher education")
)

message("Writing main four-metric table.")
results <- fread(scalar_results_path, na.strings = c("", "NA"))
main <- merge(
  specs[, .(spec, label, family)],
  results[, .(spec, beta, se, p_value, n_obs)],
  by = "spec",
  all.x = TRUE,
  sort = FALSE
)
main[, spec := factor(spec, levels = specs$spec)]
setorder(main, spec)
fwrite(main, main_table_csv)

theta_row <- paste(format_estimate(main$beta), collapse = " & ")
se_row <- paste(format_se(main$se), collapse = " & ")
n_row <- paste(formatC(main$n_obs, format = "d", big.mark = ","), collapse = " & ")

main_tex <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Main scalar school-value IV estimates}",
  "\\label{tab:main-four-va-results}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Achievement} & \\multicolumn{2}{c}{Higher education} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  paste0(
    " & ",
    paste(main$label, collapse = " & "),
    " \\\\"
  ),
  "\\midrule",
  paste0("$\\theta$ & ", theta_row, " \\\\"),
  paste0("SE & ", se_row, " \\\\"),
  paste0("Observations & ", n_row, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(main_tex, main_table_tex)

make_rank_group <- function(x, n_groups = 5L) {
  out <- rep(NA_integer_, length(x))
  nonmissing <- !is.na(x)
  n <- sum(nonmissing)
  if (n == 0) {
    return(out)
  }
  ranks <- frank(x[nonmissing], ties.method = "average")
  out[nonmissing] <- pmin(n_groups, as.integer(ceiling(n_groups * ranks / n)))
  out
}

residualize <- function(x, controls) {
  as.numeric(qr.resid(qr(controls), x))
}

build_controls <- function(dt) {
  terms <- c("z_sim_mat_4to", "z_sim_leng_4to")

  if (uniqueN(dt$cohort_gr8) > 1) {
    terms <- c(terms, "factor(cohort_gr8)")
  }
  if (uniqueN(dt$GEN_ALU) > 1) {
    terms <- c(terms, "factor(GEN_ALU)")
  }
  if (uniqueN(dt$EDAD_ALU) > 1) {
    terms <- c(terms, "factor(EDAD_ALU)")
  }

  stats::model.matrix(
    stats::as.formula(paste("~", paste(terms, collapse = " + "))),
    data = dt
  )
}

empty_result <- function(spec_row, group_label, n_obs = 0L) {
  data.table(
    spec = spec_row$spec,
    label = spec_row$label,
    plot_label = spec_row$plot_label,
    family = spec_row$family,
    group = group_label,
    beta = NA_real_,
    se = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    n_obs = n_obs
  )
}

run_scalar_iv <- function(dt, spec_row, group_label) {
  needed <- c(
    spec_row$outcome,
    spec_row$treatment,
    spec_row$instrument,
    spec_row$expected,
    "cohort_gr8",
    "z_sim_mat_4to",
    "z_sim_leng_4to",
    "GEN_ALU",
    "EDAD_ALU"
  )
  reg_dt <- dt[complete.cases(dt[, ..needed])]

  if (
    nrow(reg_dt) == 0 ||
      uniqueN(reg_dt[[spec_row$instrument]]) < 2 ||
      uniqueN(reg_dt[[spec_row$treatment]]) < 2
  ) {
    return(empty_result(spec_row, group_label, nrow(reg_dt)))
  }

  controls <- cbind(build_controls(reg_dt), expected = reg_dt[[spec_row$expected]])
  y_resid <- residualize(reg_dt[[spec_row$outcome]], controls)
  d_resid <- residualize(reg_dt[[spec_row$treatment]], controls)
  z_resid <- residualize(reg_dt[[spec_row$instrument]], controls)

  denom <- sum(z_resid * d_resid)
  if (!is.finite(denom) || abs(denom) < .Machine$double.eps) {
    return(empty_result(spec_row, group_label, nrow(reg_dt)))
  }

  beta <- sum(z_resid * y_resid) / denom
  u <- y_resid - beta * d_resid
  n <- length(u)
  k <- ncol(controls) + 1L
  small_sample_adj <- n / (n - k)
  se <- sqrt(small_sample_adj * sum((z_resid^2) * (u^2)) / denom^2)

  data.table(
    spec = spec_row$spec,
    label = spec_row$label,
    plot_label = spec_row$plot_label,
    family = spec_row$family,
    group = group_label,
    beta = beta,
    se = se,
    ci_low = beta - 1.96 * se,
    ci_high = beta + 1.96 * se,
    n_obs = n
  )
}

message("Computing grade-4 math-quintile heterogeneity.")
needed_cols <- unique(c(
  "student_id",
  "cohort_gr8",
  "GEN_ALU",
  "EDAD_ALU",
  "z_sim_mat_4to",
  "z_sim_leng_4to",
  specs$outcome,
  specs$expected,
  specs$treatment,
  specs$instrument
))
dt <- fread(expected_va_path, select = needed_cols, na.strings = c("", "NA"))
dt[, simce4_math_quintile := make_rank_group(z_sim_mat_4to, n_groups = 5L)]
dt <- dt[!is.na(simce4_math_quintile)]

heterogeneity <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  rbindlist(lapply(1:5, function(q) {
    run_scalar_iv(
      dt[simce4_math_quintile == q],
      specs[i],
      paste0("Q", q)
    )
  }), use.names = TRUE)
}), use.names = TRUE)
heterogeneity[, quintile := as.integer(sub("Q", "", group))]
heterogeneity[, group_label := factor(
  group,
  levels = paste0("Q", 1:5),
  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")
)]
heterogeneity[, plot_label := factor(plot_label, levels = specs$plot_label)]
fwrite(heterogeneity, heterogeneity_csv)

message("Drawing heterogeneity plot.")
png(heterogeneity_plot, width = 1800, height = 1200, res = 200)
par(mar = c(5.2, 5.2, 1.5, 1.2), family = "serif")
plot(
  NA,
  xlim = c(0.55, 5.45),
  ylim = range(c(heterogeneity$ci_low, heterogeneity$ci_high), na.rm = TRUE),
  xaxt = "n",
  xlab = "Baseline grade-4 math achievement quintile",
  ylab = expression(paste("Estimated pass-through ", theta)),
  bty = "l"
)
axis(1, at = 1:5, labels = levels(heterogeneity$group_label))
abline(h = 1, col = "grey70", lty = 2)
cols <- c("#1B4D89", "#B6422E", "#1B7F5A", "#7A4EA3")
pchs <- c(16, 17, 15, 18)
offsets <- c(-0.18, -0.06, 0.06, 0.18)
for (i in seq_along(specs$plot_label)) {
  lab <- specs$plot_label[i]
  sub <- heterogeneity[plot_label == lab][order(quintile)]
  x <- sub$quintile + offsets[i]
  arrows(
    x0 = x,
    y0 = sub$ci_low,
    x1 = x,
    y1 = sub$ci_high,
    angle = 90,
    code = 3,
    length = 0.035,
    col = cols[i],
    lwd = 1.2
  )
  points(x, sub$beta, pch = pchs[i], col = cols[i], bg = cols[i], cex = 1.15)
}
legend(
  "topleft",
  legend = specs$plot_label,
  col = cols,
  pch = pchs,
  bty = "n",
  cex = 0.85
)
dev.off()

message("Drawing math/STEM VA correlation plot.")
school_va <- fread(
  orthogonal_va_path,
  select = c(
    "math_controlled_value_added_centered_student",
    "stem_controlled_value_added_centered_student"
  ),
  na.strings = c("", "NA")
)
school_va <- school_va[complete.cases(school_va)]
va_cor <- cor(
  school_va$math_controlled_value_added_centered_student,
  school_va$stem_controlled_value_added_centered_student
)
png(correlation_plot, width = 1500, height = 1200, res = 200)
par(mar = c(5.1, 5.2, 1.2, 1.1), family = "serif")
plot(
  school_va$math_controlled_value_added_centered_student,
  school_va$stem_controlled_value_added_centered_student,
  pch = 16,
  col = grDevices::adjustcolor("#1B4D89", alpha.f = 0.35),
  xlab = "Math value added",
  ylab = "STEM enrollment value added",
  bty = "l"
)
abline(
  lm(
    stem_controlled_value_added_centered_student ~
      math_controlled_value_added_centered_student,
    data = school_va
  ),
  col = "#B6422E",
  lwd = 2
)
legend(
  "topleft",
  legend = paste0("Correlation = ", sprintf("%.3f", va_cor)),
  bty = "n"
)
dev.off()

message("Wrote: ", main_table_tex)
message("Wrote: ", heterogeneity_plot)
message("Wrote: ", correlation_plot)
