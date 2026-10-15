suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
task <- "school_preference_va"
code_dir <- file.path(root, "code/codex", task)
clean_dir <- file.path(root, "data/clean", task)
table_dir <- file.path(root, "output/tables", task)
report_dir <- file.path(root, "output/reports", task)
for (p in c(clean_dir, table_dir, report_dir, file.path(clean_dir, "models"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)
roster_path <- file.path(root, "data/clean/empirical_bayes_school_va/va_2017_2020__sae_2018_2020/by_continuation/all_applicant_expected_va.rds")
va_path <- file.path(data_root, "data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
specs <- data.table(
  suffix = c("math", "exam", "highfield", "highinst"),
  outcome = c("z_year_math_max", "admission_exam_taker", "high_paying_field_m1", "high_inst_m1"),
  label = c("Math VA", "Exam-taking VA", "High-premium-field VA", "High-premium-institution VA"))
specs[, `:=`(raw = paste0("va_", suffix), standardized = paste0("z_va_", suffix))]
va_terms <- specs$standardized
controls <- c("mixed_hc_tp", "tp_only")
control_labels <- c(mixed_hc_tp = "Mixed HC/TP school", tp_only = "TP-only school")
model_static_terms <- c(va_terms, controls)
distance_vars <- c(main = "log1p_distance_main", municipality = "log1p_distance_comuna")
model_labels <- c(main = "Published student coordinates", municipality = "Home-comuna centroids")
checks <- data.table(check = character(), passed = logical(), detail = character())
check <- function(name, passed, detail = "") {
  checks <<- rbind(checks, data.table(check = name, passed = isTRUE(passed), detail = as.character(detail)))
  if (!isTRUE(passed)) stop("Failed: ", name, if (nzchar(detail)) paste0(" (", detail, ")"))
  invisible(TRUE)
}
save_checks <- function(file) fwrite(checks, file.path(clean_dir, file))
one_source <- function(year, prefix) {
  p <- list.files(file.path(data_root, "data/raw", year, paste0("SAE_", year)), pattern = paste0("^", prefix, "_.*[.]csv$"), full.names = TRUE)
  stopifnot(length(p) == 1L)
  p
}
manifest_rows <- function(paths, role = "input") {
  stopifnot(all(file.exists(paths)))
  data.table(role = role, path = normalizePath(paths, winslash = "/"), bytes = file.info(paths)$size,
    modified = format(file.info(paths)$mtime, "%Y-%m-%d %H:%M:%S %Z"))
}
esc_html <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(x) paste0('<div class="table"><table><thead><tr>', paste0("<th>", esc_html(names(x)), "</th>", collapse = ""),
  "</tr></thead><tbody>", paste(apply(x, 1L, function(row) paste0("<tr>", paste0("<td>", esc_html(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table></div>")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
