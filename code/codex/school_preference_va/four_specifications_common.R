# Four simple, additive school-preference specifications on a common sample.
source("code/codex/school_preference_va/directory_prices_common.R")
source_price_dir <- clean_dir
four_spec_input_path <- price_input_path
clean_dir <- file.path(source_price_dir, "four_specifications")
table_dir <- file.path(table_dir, "four_specifications")
for (p in c(clean_dir, table_dir, file.path(clean_dir, "models"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)
# The monthly CLP1,000-10,000 band is absent in this fixed common sample.
active_fee_terms <- setdiff(fee_terms, "monthly_fee_band_2")
school_type_terms <- c("mixed_hc_tp", "tp_only")
observable_terms <- c(school_type_terms, "z_paes_m1")
specification_terms <- list(A = active_fee_terms, B = c(active_fee_terms, observable_terms),
  C = c(active_fee_terms, va_terms), D = c(active_fee_terms, school_type_terms, va_terms))
specification_labels <- c(A = "Distance + fees", B = "A + school type + PAES average",
  C = "A + four VAs", D = "C + school type (no PAES)")
four_spec_model_path <- function(specification, model) file.path(clean_dir, "models", paste0(specification, "_", model, ".rds"))
