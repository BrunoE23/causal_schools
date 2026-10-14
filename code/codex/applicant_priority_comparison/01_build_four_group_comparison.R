# Compare applicants and attended high schools across continuation x priority groups.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))

root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
pair <- "va_2017_2020__sae_2018_2020"
continuation_dir <- file.path(root, "data/clean/empirical_bayes_school_va", pair, "by_continuation")
clean_dir <- file.path(root, "data/clean/applicant_priority_comparison")
table_dir <- file.path(root, "output/tables/applicant_priority_comparison")
report_dir <- file.path(root, "output/reports/applicant_priority_comparison")
for (p in c(clean_dir, table_dir, report_dir)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

applicant_path <- file.path(continuation_dir, "all_applicant_expected_va.rds")
priority_path <- file.path(root, "data/clean/empirical_bayes_school_va", pair, "prioritario_sae_2018_2020.csv")
universe_path <- file.path(data_root, "data/clean/univ_gr8_df.csv")
value_path <- file.path(data_root, "data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
predictor_path <- file.path(root, "data/clean/staff_lasso_va/school_predictors.csv")
artistic_path <- file.path(root, "data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv")
input_paths <- c(applicant_path, priority_path, universe_path, value_path, predictor_path, artistic_path)
stopifnot(all(file.exists(input_paths)))
fwrite(data.table(path = input_paths, bytes = file.info(input_paths)$size), file.path(clean_dir, "input_manifest.csv"))

# The cached object is exactly the Table 2 timely grade-9 applicant population.
d <- as.data.table(readRDS(applicant_path))
d <- d[sample %chin% c("school_ending", "voluntary")]
stopifnot(nrow(d) == 289432L, !anyDuplicated(d[, .(mrun, sae_proceso)]))

priority <- fread(priority_path, select = c("mrun", "sae_proceso", "prioritario"))
d <- merge(d, priority, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == 289432L, !anyNA(d$prioritario), all(d$prioritario %in% 0:1))
d[, group := fcase(
  sample == "school_ending" & prioritario == 0L, "ending_nonpriority",
  sample == "school_ending" & prioritario == 1L, "ending_priority",
  sample == "voluntary" & prioritario == 0L, "voluntary_nonpriority",
  sample == "voluntary" & prioritario == 1L, "voluntary_priority"
)]
group_order <- c("ending_nonpriority", "ending_priority", "voluntary_nonpriority", "voluntary_priority")
group_labels <- c(
  ending_nonpriority = "School ending, not prioritario",
  ending_priority = "School ending, prioritario",
  voluntary_nonpriority = "Voluntary, not prioritario",
  voluntary_priority = "Voluntary, prioritario"
)
stopifnot(!anyNA(d$group), all(group_order %in% d$group))

# Preflighted 1.23 GB universe: select only baseline fields and filter immediately.
baseline_vars <- c(
  "mrun", "cohort_gr8", "cod_reg_alu", "gen_alu", "edad_alu",
  "income_decile_imputed", "mother_educ_years_imputed", "z_sim_mat_4to",
  "z_sim_leng_4to", "z_gpa_middle_mean", "z_att_middle_mean"
)
universe_header <- names(fread(universe_path, nrows = 0L))
baseline_select <- universe_header[match(baseline_vars, tolower(universe_header))]
stopifnot(!anyNA(baseline_select))
baseline <- fread(universe_path, select = baseline_select, na.strings = c("", "NA"), showProgress = FALSE)
setnames(baseline, tolower(names(baseline)))
baseline <- baseline[cohort_gr8 %in% 2018:2020 & mrun %in% d$mrun]
setkey(baseline, mrun)
baseline <- unique(baseline, by = "mrun")
stopifnot(nrow(baseline) == nrow(d), !anyDuplicated(baseline$mrun))
baseline[, `:=`(female = as.integer(gen_alu == 2), in_rm = as.integer(cod_reg_alu == 13))]
d <- merge(d, baseline, by = c("mrun", "cohort_gr8"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(d) == 289432L)

# Attach the same EB school VA used in the paper to the high school attended most often.
outcome_map <- data.table(
  outcome = c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "any_postulacion",
              "higher_ed_enrolled_m1", "high_inst_m1", "high_paying_field_m1"),
  suffix = c("math", "verbal", "exam", "aid", "highered", "highinst", "highpay"),
  expected_variable = c("expected_math_adj_eb", "expected_leng_adj_eb", "expected_exam_adj_eb",
    "expected_anypost_adj_eb", "expected_highered_adj_eb", "expected_highinst_adj_eb", "expected_highpay_adj_eb"),
  label = c("Math VA", "Verbal VA", "Exam-taking VA", "Financial-aid-application VA",
            "Higher-ed-enrollment VA", "High-premium-institution VA", "High-premium-field VA"),
  scale = c(1, 1, 100, 100, 100, 100, 100)
)
values <- fread(value_path, select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
values <- values[analysis_sample == "All" & outcome %chin% outcome_map$outcome]
values <- merge(values, outcome_map[, .(outcome, suffix)], by = "outcome", sort = FALSE)
stopifnot(!anyDuplicated(values[, .(school_rbd, suffix)]))
school_va <- dcast(values, school_rbd ~ suffix, value.var = "controlled_value_added_eb_centered_student")
setnames(school_va, setdiff(names(school_va), "school_rbd"), paste0("attended_va_", setdiff(names(school_va), "school_rbd")))
d <- merge(d, school_va, by.x = "most_time_RBD", by.y = "school_rbd", all.x = TRUE, sort = FALSE)

# School sector and TP definitions match the existing Table 2 and TP follow-up scripts.
school <- fread(predictor_path, select = c("RBD", "school__dependency_4", "school__has_tp_or_artistic"))
artistic <- fread(artistic_path, select = "RBD")
school[, `:=`(
  private_paid_hs = school__dependency_4,
  tp_hs = school__has_tp_or_artistic - as.integer(RBD %in% artistic$RBD)
)]
stopifnot(!anyDuplicated(school$RBD), all(school$tp_hs %in% 0:1, na.rm = TRUE))
d <- merge(d, school[, .(most_time_RBD = RBD, private_paid_hs, tp_hs)], by = "most_time_RBD", all.x = TRUE, sort = FALSE)

specs <- rbindlist(list(
  data.table(panel = "Applicant characteristics", variable = c(
    "female", "in_rm", "edad_alu", "income_decile_imputed", "mother_educ_years_imputed",
    "z_sim_mat_4to", "z_sim_leng_4to", "z_gpa_middle_mean", "z_att_middle_mean",
    "cohort_2018", "cohort_2019", "cohort_2020", "any_risk"),
    label = c("Female", "Metropolitan Region", "Age", "Income decile", "Mother education years",
      "Grade-4 SIMCE math (z)", "Grade-4 SIMCE verbal (z)", "Middle-school GPA (z)",
      "Middle-school attendance (z)", "Cohort 2018", "Cohort 2019", "Cohort 2020", "Assignment risk"),
    scale = c(100, 100, 1, 1, 1, 1, 1, 1, 1, 100, 100, 100, 100),
    unit = c("%", "%", rep("", 7), rep("%", 4))),
  data.table(panel = "Application portfolio: expected VA", variable = outcome_map$expected_variable,
    label = outcome_map$label, scale = outcome_map$scale, unit = c("SD", "SD", rep("pp", 5))),
  data.table(panel = "High school attended", variable = c("private_paid_hs", "tp_hs"),
    label = c("Private-paid high school", "Technical-professional high school"), scale = 100, unit = "%"),
  data.table(panel = "Attended-school value added", variable = paste0("attended_va_", outcome_map$suffix),
    label = outcome_map$label, scale = outcome_map$scale, unit = c("SD", "SD", rep("pp", 5)))
))
specs[, row_order := .I]
panel_order <- c("Applicant characteristics", "Application portfolio: expected VA",
                 "High school attended", "Attended-school value added")
specs[, panel_order := match(panel, panel_order)]
d[, `:=`(cohort_2018 = as.integer(cohort_gr8 == 2018), cohort_2019 = as.integer(cohort_gr8 == 2019),
          cohort_2020 = as.integer(cohort_gr8 == 2020))]
stopifnot(all(specs$variable %in% names(d)))

counts <- d[, .(applicants = .N), by = group][match(group_order, group)]
counts[, group_label := unname(group_labels[group])]
stats <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  v <- specs$variable[i]
  d[, .(n = .N, observed = sum(!is.na(get(v))), mean = mean(get(v), na.rm = TRUE),
        sd = sd(get(v), na.rm = TRUE), coverage = mean(!is.na(get(v)))), by = group][,
    `:=`(panel = specs$panel[i], variable = v, label = specs$label[i], scale = specs$scale[i], unit = specs$unit[i],
         row_order = specs$row_order[i], panel_order = specs$panel_order[i])]
}))
stats[, display_mean := mean * scale]
stats[, display_sd := sd * scale]
setcolorder(stats, c("panel_order", "row_order", "panel", "variable", "label", "unit", "group", "n", "observed", "coverage", "mean", "sd", "scale", "display_mean", "display_sd"))
fwrite(stats, file.path(clean_dir, "four_group_summary_long.csv"))
fwrite(counts, file.path(clean_dir, "four_group_counts.csv"))
saveRDS(d, file.path(clean_dir, "four_group_analysis_frame.rds"))

fmt <- function(x, digits = 2L) sprintf(paste0("%.", digits, "f"), x)
num <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
display <- dcast(stats, panel_order + row_order + panel + variable + label + unit ~ group, value.var = "display_mean")
setorder(display, panel_order, row_order)
setcolorder(display, c("panel_order", "row_order", "panel", "variable", "label", "unit", group_order))
for (g in group_order) display[, (g) := fmt(get(g))]
fwrite(display, file.path(table_dir, "applicants_and_attended_schools_by_priority_continuation.csv"))

esc_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}
header <- paste0("<th>", esc_html(unname(group_labels[group_order])), "<br><span>N = ", num(counts$applicants), "</span></th>", collapse = "")
rows <- character()
for (p in unique(display$panel)) {
  rows <- c(rows, paste0('<tr class="panel"><th colspan="5">', esc_html(p), "</th></tr>"))
  z <- display[panel == p]
  for (i in seq_len(nrow(z))) {
    vals <- unlist(z[i, ..group_order])
    if (z$unit[i] == "%") vals <- paste0(vals, "%")
    rows <- c(rows, paste0("<tr><th>", esc_html(z$label[i]), "</th>", paste0("<td>", vals, "</td>", collapse = ""), "</tr>"))
  }
}
notes <- paste(
  "The population is timely grade-9 SAE applicants in 2018-2020 whose grade-8 origin is classified as either ending at grade 8 or offering grade 9.",
  "School ending is the operational non-voluntary group; voluntary means the origin offers grade 9 and does not establish the student's motive or an eventual school change.",
  "Prioritario is the applicant's SAE low-income priority indicator.",
  "Expected VA is the admission-probability-weighted value added of the application portfolio. Attended VA uses the high school attended most often and the paper's unchanged EB school VA.",
  "Math and verbal VA are in student-outcome SD units; binary-outcome VA is in percentage points. Means use available observations, and the underlying CSV reports coverage for every cell."
)
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  "<title>Applicants and attended schools by continuation and priority</title>",
  '<style>body{font:15px/1.5 system-ui,sans-serif;background:#f5f7fa;color:#243347;margin:0}main{max-width:1280px;margin:28px auto;background:#fff;padding:32px;border-radius:10px}h1{font-size:28px}.table{overflow-x:auto}table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px;border-bottom:1px solid #dfe5eb;text-align:right}th:first-child{text-align:left}thead th{background:#e9eff4;vertical-align:bottom}thead span{font-weight:400;font-size:12px}.panel th{background:#f3f6f8;padding-top:14px}.notes{font-size:13px}</style></head><body><main>',
  "<h1>Applicants and attended high schools</h1><p>Four groups defined by origin-school continuation and SAE priority status.</p>",
  '<div class="table"><table><thead><tr><th>Characteristic</th>', header, "</tr></thead><tbody>", paste(rows, collapse = ""),
  "</tbody></table></div><p class=\"notes\">", esc_html(notes), "</p></main></body></html>")
writeLines(html, file.path(report_dir, "applicants_and_attended_schools.html"))

tex_escape <- function(x) gsub("%", "\\\\%", x, fixed = TRUE)
tex_rows <- character()
for (p in unique(display$panel)) {
  tex_rows <- c(tex_rows, paste0("\\multicolumn{5}{l}{\\textit{", p, "}} \\\\"))
  z <- display[panel == p]
  for (i in seq_len(nrow(z))) {
    vals <- unlist(z[i, ..group_order]); if (z$unit[i] == "%") vals <- paste0(vals, "\\%")
    tex_rows <- c(tex_rows, paste0(z$label[i], " & ", paste(vals, collapse = " & "), " \\\\"))
  }
  tex_rows <- c(tex_rows, "\\addlinespace")
}
tex <- c("\\begin{table}[!htbp]", "\\centering", "\\caption{Applicants and attended high schools by continuation and priority status}",
  "\\label{tab:applicant-school-priority-continuation}", "\\scriptsize", "\\setlength{\\tabcolsep}{4pt}", "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccc}", "\\toprule",
  paste0(" & ", paste(unname(group_labels[group_order]), collapse = " & "), " \\\\"),
  paste0(" & ", paste0("N = ", num(counts$applicants), collapse = " & "), " \\\\"), "\\midrule", tex_rows,
  "\\bottomrule", "\\end{tabular}", "}", "\\par\\medskip\\footnotesize", "\\begin{minipage}{\\textwidth}",
  paste0("\\textit{Notes:} ", notes), "\\end{minipage}", "\\end{table}")
writeLines(tex, file.path(table_dir, "applicants_and_attended_schools_by_priority_continuation.tex"))

coverage <- stats[, .(minimum_coverage = min(coverage), maximum_coverage = max(coverage)), by = panel]
fwrite(coverage, file.path(clean_dir, "panel_coverage_ranges.csv"))
print(counts)
print(display)
print(coverage)
