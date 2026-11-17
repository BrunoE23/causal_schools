# Directory fee bands added to the current seven-attribute preference model.
source("code/codex/school_preference_va/parent_income_common.R")
prior_income_dir <- clean_dir
prior_income_input_path <- parent_income_input_path
prior_income_static_terms <- extended_static_terms
clean_dir <- file.path(base_clean_dir, "directory_prices_2024")
table_dir <- file.path(base_table_dir, "directory_prices_2024")
for (p in c(clean_dir, table_dir, file.path(clean_dir, "models"))) dir.create(p, recursive = TRUE, showWarnings = FALSE)
fee_map <- c("SIN INFORMACION" = 0L, "GRATUITO" = 1L, "$1.000 A $10.000" = 2L,
  "$10.001 A $25.000" = 3L, "$25.001 A $50.000" = 4L, "$50.001 A $100.000" = 5L, "MAS DE $100.000" = 6L)
fee_labels <- setNames(c("No information", "Free", "CLP 1,000-10,000", "CLP 10,001-25,000",
  "CLP 25,001-50,000", "CLP 50,001-100,000", "Above CLP 100,000"), as.character(0:6))
fee_specs <- rbindlist(lapply(c("PAGO_MATRICULA", "PAGO_MENSUAL"), function(field_name) {
  prefix <- if (field_name == "PAGO_MATRICULA") "enrollment_fee" else "monthly_fee"
  code <- 2:6
  data.table(field = field_name, prefix = prefix, code = code, label = unname(fee_labels[as.character(code)]),
    term = paste0(prefix, "_band_", code))
}))
fee_specs <- rbindlist(list(fee_specs, data.table(field = "BOTH", prefix = "fee_information", code = 0L,
  label = "Fee information unavailable (both)", term = "fee_information_unknown")))
fee_terms <- fee_specs$term
variant_terms <- list(extended = c(prior_income_static_terms, fee_terms), matched_baseline = prior_income_static_terms)
variant_labels <- c(extended = "Seven attributes plus directory fee bands", matched_baseline = "Seven attributes, same sample")
price_input_path <- file.path(clean_dir, "estimation_data.rds")
price_model_path <- function(group, variant, model) file.path(clean_dir, "models", paste(group, variant, model, sep = "_"))
prior_price_baseline_path <- function(group, model) file.path(prior_income_dir, "models", paste0(group, "_extended_", model, ".rds"))
add_fee_indicators <- function(d) {
  for (k in seq_len(nrow(fee_specs))) {
    s <- fee_specs[k]
    field <- if (s$field == "BOTH") "PAGO_MATRICULA" else s$field
    set(d, j = s$term, value = as.integer(d[[paste0(field, "_CODE")]] == s$code))
  }
  d
}
