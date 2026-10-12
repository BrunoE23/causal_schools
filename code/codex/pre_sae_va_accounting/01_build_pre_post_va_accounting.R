# Join cleaned pre/post-SAE high-school links to existing EB school VA.
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

setDTthreads(4L)
if (.Platform$OS.type == "windows") {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}

repo <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
clean_dir <- file.path(repo, "data/clean/pre_sae_va_accounting")
fig_dir <- file.path(repo, "output/figures/pre_sae_va_accounting")
report_dir <- file.path(repo, "output/reports/pre_sae_va_accounting")
for (p in c(clean_dir, fig_dir, report_dir)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

links_path <- file.path(clean_dir, "pre_post_student_high_school_links.rds")
if (!file.exists(links_path)) {
  stop("Missing cleaned links. Run code/clean_pre_sae_va_accounting.R first.")
}

value_path <- file.path(
  data_root,
  "data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv"
)

specs <- data.table(
  outcome = c(
    "z_year_math_max", "z_year_leng_max", "admission_exam_taker", "any_postulacion",
    "higher_ed_enrolled_m1", "high_inst_m1", "high_paying_field_m1"
  ),
  label = c(
    "Math", "Verbal", "Exam taking", "Fin. aid app.",
    "Higher-ed enrollment", "High-premium inst.", "High-premium field"
  ),
  scale = c(1, 1, 100, 100, 100, 100, 100),
  unit = c("outcome SD", "outcome SD", rep("percentage points", 5))
)

links <- as.data.table(readRDS(links_path))
links[, period := fifelse(cohort_gr8 <= 2017L, "Pre-SAE 2014-2017", "SAE 2018-2020")]
links[, period_id := fifelse(cohort_gr8 <= 2017L, "pre", "post")]
links[, group := switch_group]

values <- fread(
  value_path,
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"),
  showProgress = FALSE
)
values <- values[analysis_sample == "All" & outcome %in% specs$outcome]
values[, `:=`(
  school_rbd = as.numeric(school_rbd),
  value = controlled_value_added_eb_centered_student
)]
values <- values[, .(school_rbd, outcome, value)]
wide <- dcast(values, school_rbd ~ outcome, value.var = "value")
links <- merge(links, wide, by.x = "most_time_RBD", by.y = "school_rbd", all.x = TRUE, sort = FALSE)

summarize_by <- function(dt, by_cols) {
  rbindlist(lapply(specs$outcome, function(v) {
    sp <- specs[outcome == v]
    dt[!is.na(group), .(
      students = .N,
      va_observed = sum(!is.na(get(v))),
      va_coverage = mean(!is.na(get(v))),
      mean_va = mean(get(v), na.rm = TRUE),
      sd_va = sd(get(v), na.rm = TRUE)
    ), by = by_cols][
      ,
      `:=`(
        outcome = sp$outcome,
        label = sp$label,
        display_scale = sp$scale,
        unit = sp$unit,
        display_mean_va = mean_va * sp$scale,
        display_sd_va = sd_va * sp$scale
      )
    ]
  }), use.names = TRUE, fill = TRUE)
}

cohort_summary <- summarize_by(links, c("period", "period_id", "cohort_gr8", "group"))
period_summary <- summarize_by(links, c("period", "period_id", "group"))

change_summary <- dcast(
  period_summary,
  group + outcome + label + unit + display_scale ~ period_id,
  value.var = c("students", "va_coverage", "mean_va", "display_mean_va")
)
change_summary[, change_mean_va := mean_va_post - mean_va_pre]
change_summary[, change_display_mean_va := display_mean_va_post - display_mean_va_pre]

sample_accounting <- links[, .(
  students = .N,
  with_post_school = sum(!is.na(most_time_RBD)),
  with_va_math = sum(!is.na(z_year_math_max)),
  school_ends = sum(!has_grade9_next_year),
  voluntary_switch_nonprivate = sum(has_grade9_next_year & actual_switch & destination_nonprivate, na.rm = TRUE),
  unresolved_for_primary_groups = sum(is.na(group))
), by = .(period, cohort_gr8)]

fwrite(sample_accounting, file.path(clean_dir, "va_accounting_sample.csv"))
fwrite(cohort_summary, file.path(clean_dir, "cohort_attended_va_by_group.csv"))
fwrite(period_summary, file.path(clean_dir, "period_attended_va_by_group.csv"))
fwrite(change_summary, file.path(clean_dir, "pre_post_change_attended_va_by_group.csv"))
saveRDS(links, file.path(clean_dir, "student_accounting_frame_with_va.rds"))

plot_data <- cohort_summary[
  label %in% c("Math", "Exam taking", "High-premium inst.") &
    group %in% c("School ends before grade 9", "Voluntary switch to non-private HS")
]
plot_data[, group := factor(group, levels = c("School ends before grade 9", "Voluntary switch to non-private HS"))]
p <- ggplot(plot_data, aes(x = cohort_gr8, y = display_mean_va, color = group, group = group)) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ label, scales = "free_y") +
  scale_x_continuous(breaks = sort(unique(plot_data$cohort_gr8))) +
  labs(
    x = "Grade-8 cohort",
    y = "Mean attended-school VA",
    color = NULL,
    title = "Average attended high-school VA by grade-8 switch margin",
    subtitle = "Pre-SAE cohorts 2014-2017 vs SAE cohorts 2018-2020"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(fig_dir, "cohort_attended_va_selected_outcomes.png"), p, width = 9, height = 5.5, dpi = 180)

fmt_num <- function(x, digits = 2) {
  out <- formatC(x, digits = digits, format = "f", big.mark = ",")
  out[is.na(x)] <- ""
  out
}
fmt_int <- function(x) formatC(x, digits = 0, format = "f", big.mark = ",")
report_table <- change_summary[
  ,
  .(
    Group = group,
    Outcome = label,
    `Pre mean` = fmt_num(display_mean_va_pre, 3),
    `SAE mean` = fmt_num(display_mean_va_post, 3),
    `Change` = fmt_num(change_display_mean_va, 3),
    Unit = unit,
    `Pre N` = fmt_int(students_pre),
    `SAE N` = fmt_int(students_post),
    `Pre VA coverage` = fmt_num(100 * va_coverage_pre, 1),
    `SAE VA coverage` = fmt_num(100 * va_coverage_post, 1)
  )
]

md_table <- function(z) {
  c(
    paste0("| ", paste(names(z), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(z)), collapse = " | "), " |"),
    apply(z, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  )
}

notes <- c(
  "Pre-SAE cohorts are grade-8 cohorts 2014-2017; SAE-era cohorts are 2018-2020.",
  "Middle-school origin is the first observed grade-8 RBD; high school is most_time_RBD over the four years after grade 8, using the same attendance/promotion/highest-attendance duplicate logic as the paper construction.",
  "For pre-SAE years the continuation split uses a registry analogue: whether the origin RBD has grade-9 enrollment in the next school year. The literal SAE offering file is only available for the post period.",
  "Voluntary switchers are students whose origin has grade-9 continuation evidence, whose most-time high school differs from the origin, and whose destination is not private paid (COD_DEPE2 != 4).",
  "Attended VA is the existing EB school value measured on the 2017-2020 VA sample and joined to the attended most_time_RBD."
)

writeLines(
  c(
    "# Pre-SAE versus SAE attended-school VA accounting",
    "",
    "## Pre/post changes",
    "",
    md_table(report_table),
    "",
    "## Notes",
    "",
    paste0("- ", notes)
  ),
  file.path(report_dir, "pre_post_attended_va_accounting.md")
)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(z) {
  paste0(
    "<table><thead><tr>",
    paste0("<th>", html_escape(names(z)), "</th>", collapse = ""),
    "</tr></thead><tbody>",
    paste(apply(z, 1, function(row) {
      paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
    }), collapse = ""),
    "</tbody></table>"
  )
}
html <- paste0(
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Pre-SAE attended VA accounting</title>",
  "<style>body{font:15px/1.5 system-ui,sans-serif;margin:32px;color:#1f2937}main{max-width:1180px;margin:auto}",
  "table{border-collapse:collapse;width:100%;font-size:12px}th,td{padding:7px 9px;border-bottom:1px solid #ddd;text-align:left}",
  "th{background:#f3f4f6}td:nth-child(n+3){font-variant-numeric:tabular-nums}img{max-width:100%;height:auto}</style></head><body><main>",
  "<h1>Pre-SAE versus SAE attended-school VA accounting</h1>",
  "<p>Average attended-school EB VA by grade-8 switch margin. See notes below for the pre-SAE continuation analogue.</p>",
  "<img src=\"../../figures/pre_sae_va_accounting/cohort_attended_va_selected_outcomes.png\" alt=\"Selected cohort trends\">",
  html_table(report_table),
  "<h2>Notes</h2><ul>", paste0("<li>", html_escape(notes), "</li>", collapse = ""), "</ul>",
  "</main></body></html>"
)
writeLines(html, file.path(report_dir, "pre_post_attended_va_accounting.html"))
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "va_accounting_session_info.txt"))

print(sample_accounting)
print(change_summary[order(group, outcome), .(
  group, label, unit,
  pre = display_mean_va_pre,
  sae = display_mean_va_post,
  change = change_display_mean_va,
  pre_n = students_pre,
  sae_n = students_post
)])
