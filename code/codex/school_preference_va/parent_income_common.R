# Add the existing grade-4-SIMCE school-average income decile to the parent-education model.
source("code/codex/school_preference_va/parent_education_common.R")
parent_education_dir <- clean_dir
clean_dir <- file.path(base_clean_dir, "parent_income_decile_4to")
table_dir <- file.path(base_table_dir, "parent_income_decile_4to")
for (p in c(clean_dir, table_dir, file.path(clean_dir, "models"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)
base_parent_education_terms <- extended_static_terms
base_parent_education_specs <- copy(metric_specs)
metric_specs <- rbindlist(list(base_parent_education_specs, data.table(suffix = "parent_income_decile_4to",
  outcome = "income_decile_mean", label = "Mean baseline parent income decile",
  raw = "income_decile_mean", standardized = "z_parent_income_decile")), use.names = TRUE)
metric_terms <- metric_specs$standardized
metric_interactions <- rbindlist(lapply(controls, function(control_name) data.table(
  control = control_name, metric = metric_terms, suffix = metric_specs$suffix,
  term = paste0(control_name, "_x_", metric_specs$suffix),
  label = paste0(if (control_name == "mixed_hc_tp") "Mixed HC/TP" else "TP only", " x ", metric_specs$label))))
extended_static_terms <- c(metric_terms, controls, metric_interactions$term)
metric_labels <- setNames(c(metric_specs$label, unname(control_labels[controls]), metric_interactions$label), extended_static_terms)
variant_terms <- list(extended = extended_static_terms, matched_baseline = base_parent_education_terms)
variant_labels <- c(extended = "Four VAs + PAES M1 + parent education + parent income decile",
  matched_baseline = "Four VAs + PAES M1 + parent education, same sample")
parent_income_input_path <- file.path(clean_dir, "estimation_data.rds")
parent_income_model_path <- function(group, variant, model) file.path(clean_dir, "models",
  paste(group, variant, model, sep = "_"))
