# Separate PAES-mean extension; preserve the existing four-VA interaction fits.
source("code/codex/school_preference_va/common.R")
base_clean_dir <- clean_dir
base_table_dir <- table_dir
clean_dir <- file.path(base_clean_dir, "paes_m1")
table_dir <- file.path(base_table_dir, "paes_m1")
for (p in c(clean_dir, table_dir, file.path(clean_dir, "models"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)
base_specs <- copy(specs)
base_va_terms <- va_terms
base_static_terms <- model_static_terms
metric_specs <- rbindlist(list(base_specs, data.table(suffix = "paes_m1",
  outcome = "paes_m1_mean", label = "PAES M1 school mean", raw = "paes_m1_mean", standardized = "z_paes_m1")), use.names = TRUE)
metric_terms <- metric_specs$standardized
metric_interactions <- rbindlist(lapply(controls, function(control_name) data.table(
  control = control_name, metric = metric_terms, suffix = metric_specs$suffix,
  term = paste0(control_name, "_x_", metric_specs$suffix),
  label = paste0(if (control_name == "mixed_hc_tp") "Mixed HC/TP" else "TP only", " x ", metric_specs$label))))
extended_static_terms <- c(metric_terms, controls, metric_interactions$term)
metric_labels <- setNames(c(metric_specs$label, unname(control_labels[controls]), metric_interactions$label), extended_static_terms)
variant_terms <- list(extended = extended_static_terms, matched_baseline = base_static_terms)
variant_labels <- c(extended = "Four VAs + PAES M1", matched_baseline = "Four VAs, same sample")
group_labels <- c(pooled = "All applicants", ending_nonpriority = "School ending, non-prioritario",
  ending_priority = "School ending, prioritario", voluntary_nonpriority = "Voluntary, non-prioritario",
  voluntary_priority = "Voluntary, prioritario")
primary_paes_process <- 2025L
paes_input_path <- file.path(clean_dir, "estimation_data.rds")
paes_model_path <- function(group, variant, model) file.path(clean_dir, "models", paste(group, variant, model, sep = "_"))

capture_prior_outputs <- function() {
  paths <- c(list.files(file.path(base_clean_dir, "models"), pattern = "[.]rds$", full.names = TRUE),
    list.files(file.path(base_clean_dir, "by_priority_continuation/models"), pattern = "[.]rds$", full.names = TRUE),
    file.path(base_clean_dir, c("estimation_data.rds", "va_normalization.csv",
      "by_priority_continuation/estimation_data.rds", "by_priority_continuation/va_normalization.csv")),
    file.path(report_dir, c("joint_rank_ordered_logit.html", "by_priority_continuation.html")),
    list.files(base_table_dir, pattern = "^(joint_rank_ordered_logit|by_priority_continuation|type_specific_va_slopes).*", full.names = TRUE))
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  stopifnot(length(paths) > 0L, !anyDuplicated(paths))
  saved <- data.table(path = normalizePath(paths, winslash = "/"), bytes = file.info(paths)$size,
    md5_before = unname(tools::md5sum(paths)))
  fwrite(saved, file.path(clean_dir, "prior_outputs_preservation.csv"))
  invisible(saved)
}

add_metric_interactions <- function(x) {
  for (k in seq_len(nrow(metric_interactions))) {
    s <- metric_interactions[k]
    value <- x[[s$control]] * x[[s$metric]]
    stopifnot(length(value) == nrow(x), all(is.finite(value)))
    if (s$term %in% names(x)) stopifnot(identical(as.numeric(x[[s$term]]), as.numeric(value)))
    set(x, j = s$term, value = value)
  }
  x
}

metric_type_slopes <- function(fit, group, variant, model) {
  b <- coef(fit); V <- vcov(fit)
  type_controls <- c("Non-TP" = NA_character_, "Mixed HC/TP" = "mixed_hc_tp", "TP only" = "tp_only")
  active <- metric_specs[standardized %in% names(b)]
  rbindlist(lapply(names(type_controls), function(type_name) rbindlist(lapply(seq_len(nrow(active)), function(k) {
    contrast <- setNames(numeric(length(b)), names(b))
    contrast[active$standardized[k]] <- 1
    control_name <- type_controls[[type_name]]
    if (!is.na(control_name)) contrast[metric_interactions[control == control_name & metric == active$standardized[k], term]] <- 1
    estimate <- sum(contrast * b)
    variance <- as.numeric(t(contrast) %*% V %*% contrast)
    stopifnot(is.finite(variance), variance > 0)
    se <- sqrt(variance)
    data.table(group = group, variant = variant, model = model, school_type = type_name,
      outcome = active$outcome[k], suffix = active$suffix[k], term = active$standardized[k], label = active$label[k],
      estimate = estimate, std_error = se, z = estimate / se, p_value = 2 * pnorm(-abs(estimate / se)),
      lower95 = estimate - qnorm(.975) * se, upper95 = estimate + qnorm(.975) * se,
      contrast_terms = paste(names(contrast)[contrast != 0], collapse = " + "))
  }))))
}

paes_pl_loglik <- function(x, beta, terms) {
  work <- data.table(ranking_id = x$ranking_id, eta = as.numeric(as.matrix(x[, ..terms]) %*% beta))
  work[, {
    centered <- eta - max(eta)
    log_denominator <- centered
    if (.N > 1L) for (r in seq.int(.N - 1L, 1L)) {
      a <- centered[r]; b <- log_denominator[r + 1L]; largest <- max(a, b)
      log_denominator[r] <- largest + log(exp(a - largest) + exp(b - largest))
    }
    .(loglik = sum(centered - log_denominator))
  }, by = ranking_id][, sum(loglik)]
}
