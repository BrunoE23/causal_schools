# Compact specification comparison from saved pooled estimates only.
source("code/codex/school_preference_va/four_specifications_common.R")
read_clean <- function(name) fread(file.path(clean_dir, name))
co <- read_clean("coefficients.csv")
ms <- read_clean("model_summary.csv")
verification <- read_clean("verification.csv")

spec_order <- c("A", "A_means", "B", "C", "D")
check("Ten unique saved pooled models", nrow(ms) == 10L &&
  !anyDuplicated(ms[, .(specification, model)]) &&
  setequal(ms$specification, spec_order) && setequal(ms$model, names(distance_vars)))
check("Independent verification passed", nrow(verification) > 0L && all(verification$passed))
check("Coefficient keys are unique and estimates are finite", !anyDuplicated(co[, .(specification, model, term)]) &&
  all(is.finite(co$estimate)) && all(is.finite(co$std_error) & co$std_error > 0))
check("Every model uses the same applicant lists and destination schools", uniqueN(ms$applicants) == 1L &&
  uniqueN(ms$rankings) == 1L && uniqueN(ms$applicant_school_pairs) == 1L && uniqueN(ms$schools) == 1L)
check("Models include only the requested common slopes", !any(grepl("_x_|parent_", co$term)) &&
  all(vapply(spec_order, function(s) all(vapply(names(distance_vars), function(md)
    setequal(co[specification == s & model == md, term], c(specification_terms[[s]], distance_vars[[md]])),
    logical(1))), logical(1))))
check("All four school outcome averages enter only A plus averages and B", setequal(school_mean_terms,
  c("z_paes_m1", "z_mean_exam", "z_mean_highfield", "z_mean_highinst")) && all(vapply(school_mean_terms,
  function(term_name) co[term == term_name, .N] == 4L &&
    setequal(co[term == term_name, specification], c("A_means", "B")), logical(1))))
check("The outcome-average-only column excludes school-type indicators", co[specification == "A_means" &
  term %in% controls, .N] == 0L)
check("Displayed intervals match saved student-clustered standard errors",
  max(abs(co$lower95 - co$estimate + qnorm(.975) * co$std_error)) < 1e-8 &&
  max(abs(co$upper95 - co$estimate - qnorm(.975) * co$std_error)) < 1e-8)

fmt <- function(x) formatC(x, digits = 3L, format = "f", big.mark = ",")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
display_cell <- function(b, se) ifelse(is.finite(b), paste0(fmt(b), "<span class='se'>(", fmt(se), ")</span>"), "&mdash;")
csv_cell <- function(b, se) ifelse(is.finite(b), paste0(fmt(b), " (", fmt(se), ")"), "")

fee_rows <- fee_specs[term %in% active_fee_terms]
fee_rows[, fee_order := fifelse(field == "PAGO_MENSUAL", 1L, fifelse(field == "PAGO_MATRICULA", 2L, 3L))]
setorder(fee_rows, fee_order, code)
fee_rows[, row_label := fifelse(field == "PAGO_MENSUAL", paste0("Monthly: ", label),
  fifelse(field == "PAGO_MATRICULA", paste0("Enrollment: ", label), "Fee information unavailable"))]
base_rows <- data.table(term = c("distance", "mixed_hc_tp", "tp_only", "z_paes_m1", "z_mean_exam", "z_mean_highfield", "z_mean_highinst", "z_va_math", "z_va_exam", "z_va_highfield", "z_va_highinst"),
  row_label = c("log(1 + distance in km)", "Mixed HC/TP school", "TP-only school", "PAES M1 mean",
    "Exam-taking share", "High-premium-field enrollment share", "High-premium-institution enrollment share",
    "Math VA", "Exam-taking VA", "High-premium-field VA", "High-premium-institution VA"),
  section = c(rep("School attributes", 3L), rep("Observed school outcome averages", 4L), rep("School value added", 4L)))
rows <- rbindlist(list(base_rows, fee_rows[, .(term, row_label,
  section = fifelse(field == "PAGO_MENSUAL", "Monthly fees (reference: free)",
    fifelse(field == "PAGO_MATRICULA", "Enrollment fees (reference: free)", "Fee information")))]), use.names = TRUE)

table_data <- rbindlist(lapply(names(distance_vars), function(md) {
  out <- copy(rows)
  out[term == "distance", term := distance_vars[[md]]]
  out[, model := md]
  for (s in spec_order) {
    z <- co[specification == s & model == md]
    pos <- match(out$term, z$term)
    out[, (paste0(s, "_estimate")) := z$estimate[pos]]
    out[, (paste0(s, "_std_error")) := z$std_error[pos]]
    out[, (paste0(s, "_display")) := csv_cell(z$estimate[pos], z$std_error[pos])]
  }
  out
}))
check("Coefficient tables preserve every saved model term", sum(vapply(spec_order, function(s)
  sum(is.finite(table_data[[paste0(s, "_estimate")]])), integer(1))) == nrow(co))
fwrite(table_data, file.path(table_dir, "four_specifications.csv"))
sample_table <- ms[, .(specification, model, applicants, rankings, applicant_school_pairs, schools,
  log_likelihood, null_log_likelihood, informative_rank_decisions, parameters)]
sample_table[, definition := unname(specification_labels[specification])]
fwrite(sample_table, file.path(table_dir, "common_sample_and_specifications.csv"))

make_table <- function(md) {
  x <- table_data[model == md]
  body <- vapply(seq_len(nrow(x)), function(i) {
    block <- if (i == 1L || x$section[i] == x$section[i - 1L]) "" else
      paste0("<tr class='block'><th colspan='", length(spec_order) + 1L, "'>", esc_html(x$section[i]), "</th></tr>")
    cells <- vapply(spec_order, function(s) display_cell(x[[paste0(s, "_estimate")]][i],
      x[[paste0(s, "_std_error")]][i]), character(1))
    paste0(block, "<tr><th scope='row'>", esc_html(x$row_label[i]), "</th>",
      paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1))
  summaries <- ms[model == md][match(spec_order, specification)]
  footer_values <- list(Applicants = fmt_n(summaries$applicants),
    `Applicant-school pairs` = fmt_n(summaries$applicant_school_pairs),
    `Destination schools` = fmt_n(summaries$schools), `Log likelihood` = fmt(summaries$log_likelihood))
  footer <- paste(vapply(names(footer_values), function(label) paste0("<tr><th scope='row'>", label,
    "</th>", paste0("<td>", footer_values[[label]], "</td>", collapse = ""), "</tr>"), character(1)), collapse = "")
  paste0("<div class='table-wrap'><table><thead><tr><th>Attribute</th>",
    paste0("<th>", esc_html(unname(specification_display_labels[spec_order])), "</th>", collapse = ""), "</tr></thead><tbody>",
    paste(body, collapse = ""), "</tbody><tfoot>", footer, "</tfoot></table></div>")
}

get_b <- function(s, term_name) co[specification == s & model == "main" & term == term_name, estimate]
math_c <- get_b("C", "z_va_math"); math_d <- get_b("D", "z_va_math")
exam_c <- get_b("C", "z_va_exam"); exam_d <- get_b("D", "z_va_exam")
b_means <- co[specification == "B" & model == "main" & term %in% school_mean_terms]
mean_interpretation <- if (all(b_means$estimate > 0))
  "In B, all four observed school outcome averages are positively associated with ranking earlier, conditional on fees, distance and school type." else
  paste0("In B, the observed school outcome averages enter jointly with fees, distance and school type; ",
    sum(b_means$estimate > 0), " of the four coefficients are positive.")
interpretation <- paste0(mean_interpretation, " Adding school type to the four-VA model changes the math-VA coefficient from ",
  fmt(math_c), " in C to ", fmt(math_d), " in D, and exam-taking VA from ", fmt(exam_c), " to ", fmt(exam_d), ".")
interpretation <- paste0(interpretation, " These are associations within submitted lists.")

main_table <- make_table("main")
municipality_table <- make_table("municipality")
check("Each coefficient table has five specification columns in the requested order", all(vapply(list(main_table, municipality_table),
  function(h) length(regmatches(h, gregexpr("<table>", h, fixed = TRUE))[[1L]]) == 1L &&
    grepl(paste0("<th>", esc_html(unname(specification_display_labels[spec_order])), "</th>", collapse = ""),
      h, fixed = TRUE), logical(1))))
unsupported_monthly <- fee_specs[field == "PAGO_MENSUAL" & !term %in% active_fee_terms]
support_note <- if (nrow(unsupported_monthly)) paste0(" The monthly ",
  paste(unsupported_monthly$label, collapse = ", "), " band has no schools and is not estimated.") else ""
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>School preferences</title><style>',
  'body{font:16px/1.5 system-ui,sans-serif;color:#18283b;margin:28px}main{max-width:1000px;margin:auto}h1{font-size:26px;margin-bottom:16px}',
  '.specs{background:#f2f5f9;padding:12px 16px}.table-wrap{overflow:auto}table{border-collapse:collapse;width:100%;font-size:14px}',
  'th,td{padding:7px 12px;text-align:right;vertical-align:top;border-bottom:1px solid #e1e6ec}th:first-child{text-align:left}',
  'thead th{background:#eff3f8;font-weight:650}tbody th{font-weight:400}.se{display:block;color:#65758a;font-size:12px}',
  '.block th{padding-top:17px;border-bottom:1px solid #aebccc;font-weight:650;background:#fafbfd}tfoot th,tfoot td{font-weight:600}',
  'tfoot tr:first-child th,tfoot tr:first-child td{border-top:2px solid #aebccc}details{margin:22px 0}summary{cursor:pointer;font-weight:600}',
  '.notes{font-size:13px;color:#4e6076}.interpretation{margin-top:20px}a{color:#245d9b}</style></head><body><main>',
  '<h1>School preferences</h1>',
  '<p class="specs"><b>A:</b> Distance + fees.<br><b>A + averages:</b> A + four school outcome averages (no school-type indicators).<br>',
  '<b>B:</b> A + school type + four school outcome averages.<br>',
  '<b>C:</b> A + four VAs.<br><b>D:</b> C + school type (Mixed HC/TP, TP-only).</p>',
  main_table,
  '<p class="notes">Student-clustered standard errors in parentheses. A dash means the attribute is excluded. Fee coefficients compare each band with free in that field; the shared unknown-fee indicator compares unavailable prices with free in both fields.',
  esc_html(support_note), '</p>',
  '<p class="interpretation">', esc_html(interpretation), '</p>',
  '<details><summary>Municipality-distance robustness</summary><p class="notes">Home-comuna centroid to the same school campus; identical applicants, rankings and school-attribute normalization.</p>',
  municipality_table, '</details>',
  '<p class="notes">All columns use the same complete submitted rankings in an exact Plackett-Luce model, conditional on the schools listed; unlisted alternatives are outside this exercise. VAs and all four school outcome averages are measured per one destination-school SD. The three new shares use equal weights across unique destination schools; existing VA and PAES normalizations are unchanged. Non-TP is the reference school type. Slopes are common across school types.</p>',
  '<p class="notes">Observed outcome shares use grade-8 cohorts 2017-2020 (ages 12-16 at grade 8), grouped by the high school attended most. The exam-taking share covers all eligible students. Field and institution enrollment shares are among exam takers, counting non-enrollees as zero. PAES M1 is the current school math-score average, not a value-added measure.</p>',
  '<p class="notes">Directory fees are a 2024 snapshot and PAES M1 is measured in the 2025 admission process, after the 2018-2020 applications. Coefficients describe conditional ranking associations, not causal price effects or information necessarily available at application. Changes across logit specifications can also reflect utility-scale changes.</p>',
  '<p class="notes"><a href="../../tables/school_preference_va/directory_prices_2024/four_specifications/four_specifications.csv">Coefficient table (CSV)</a> &middot; ',
  '<a href="../../tables/school_preference_va/directory_prices_2024/four_specifications/common_sample_and_specifications.csv">Common sample and specifications (CSV)</a></p>',
  '</main></body></html>')
check("Report has only one visible table plus one optional robustness table",
  length(regmatches(html, gregexpr("<table>", html, fixed = TRUE))[[1L]]) == 2L &&
  length(regmatches(html, gregexpr("<details>", html, fixed = TRUE))[[1L]]) == 1L &&
  grepl(paste0("<details><summary>Municipality-distance robustness</summary>"), html, fixed = TRUE) &&
  !grepl("parent education|income decile|_x_", html, ignore.case = TRUE))
report_path <- file.path(report_dir, "with_directory_prices_2024.html")
writeLines(html, report_path, useBytes = TRUE)
writeLines(capture.output(sessionInfo()), file.path(clean_dir, "report_session_info.txt"))
fwrite(checks, file.path(clean_dir, "report_verification.csv"))
message("Updated price report specification comparison; all ", nrow(checks), " report checks passed.")
