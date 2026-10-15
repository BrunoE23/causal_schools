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
interaction_specs <- rbindlist(lapply(controls, function(control_name) data.table(
  control = control_name, va = va_terms, suffix = specs$suffix,
  term = paste0(control_name, "_x_", specs$suffix),
  label = paste0(if (control_name == "mixed_hc_tp") "Mixed HC/TP" else "TP only", " x ", specs$label))))
interaction_terms <- interaction_specs$term
model_static_terms <- c(va_terms, controls, interaction_terms)
model_static_labels <- setNames(c(specs$label, unname(control_labels[controls]), interaction_specs$label), model_static_terms)
add_va_interactions <- function(x) {
  for (k in seq_len(nrow(interaction_specs))) {
    s <- interaction_specs[k]
    value <- x[[s$control]] * x[[s$va]]
    stopifnot(length(value) == nrow(x), all(is.finite(value)))
    if (s$term %in% names(x)) stopifnot(identical(as.numeric(x[[s$term]]), as.numeric(value)))
    set(x, j = s$term, value = value)
  }
  x
}
type_specific_va_slopes <- function(fit, model, group = NULL) {
  b <- coef(fit)
  V <- vcov(fit)
  type_controls <- c("Non-TP" = NA_character_, "Mixed HC/TP" = "mixed_hc_tp", "TP only" = "tp_only")
  out <- rbindlist(lapply(names(type_controls), function(type_name) rbindlist(lapply(seq_len(nrow(specs)), function(k) {
    contrast <- setNames(numeric(length(b)), names(b))
    contrast[specs$standardized[k]] <- 1
    control_name <- type_controls[[type_name]]
    if (!is.na(control_name)) contrast[interaction_specs[control == control_name & va == specs$standardized[k], term]] <- 1
    estimate <- sum(contrast * b)
    variance <- as.numeric(t(contrast) %*% V %*% contrast)
    stopifnot(is.finite(variance), variance > 0)
    se <- sqrt(variance)
    data.table(model = model, school_type = type_name, outcome = specs$outcome[k], term = specs$standardized[k],
      label = specs$label[k], estimate = estimate, std_error = se, z = estimate / se,
      p_value = 2 * pnorm(-abs(estimate / se)), lower95 = estimate - qnorm(.975) * se,
      upper95 = estimate + qnorm(.975) * se, contrast_terms = paste(names(contrast)[contrast != 0], collapse = " + "))
  }))))
  if (!is.null(group)) out[, group := group]
  out
}
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
