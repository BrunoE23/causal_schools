# Separate 2022 SIMCE parent-education extension to the PAES-M1 preference model.
source("code/codex/school_preference_va/paes_common.R")
base_paes_dir <- clean_dir
clean_dir <- file.path(base_clean_dir, "parent_education_2022")
table_dir <- file.path(base_table_dir, "parent_education_2022")
for (p in c(clean_dir, table_dir, file.path(clean_dir, "models"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)

base_paes_specs <- copy(metric_specs)
base_paes_terms <- extended_static_terms
metric_specs <- rbindlist(list(base_paes_specs, data.table(suffix = "parent_education_2022",
  outcome = "mean_parent_education_years", label = "Parent education years",
  raw = "mean_parent_education_years", standardized = "z_parent_education")), use.names = TRUE)
metric_terms <- metric_specs$standardized
metric_interactions <- rbindlist(lapply(controls, function(control_name) data.table(
  control = control_name, metric = metric_terms, suffix = metric_specs$suffix,
  term = paste0(control_name, "_x_", metric_specs$suffix),
  label = paste0(if (control_name == "mixed_hc_tp") "Mixed HC/TP" else "TP only", " x ", metric_specs$label))))
extended_static_terms <- c(metric_terms, controls, metric_interactions$term)
metric_labels <- setNames(c(metric_specs$label, unname(control_labels[controls]), metric_interactions$label), extended_static_terms)
variant_terms <- list(extended = extended_static_terms, matched_baseline = base_paes_terms)
variant_labels <- c(extended = "Four VAs + PAES M1 + parent education",
  matched_baseline = "Four VAs + PAES M1, same sample")
parent_education_input_path <- file.path(clean_dir, "estimation_data.rds")
parent_education_model_path <- function(group, variant, model) file.path(clean_dir, "models",
  paste(group, variant, model, sep = "_"))

