# Join 2024 reported fee bands by RBD, retaining entire submitted rankings.
source("code/codex/school_preference_va/directory_prices_common.R")
directory_path <- list.files(file.path(data_root, "data/raw/school_directory/2024"), pattern = "[.]csv$", full.names = TRUE)
check("One 2024 school-directory source is available", length(directory_path) == 1L)
school <- fread(directory_path, sep = ";", encoding = "UTF-8", select = c("AGNO", "RBD", "PAGO_MATRICULA", "PAGO_MENSUAL"))
setnames(school, "RBD", "rbd")
check("Directory schools are unique RBDs in 2024", !anyDuplicated(school$rbd) && all(school$AGNO == 2024L))
for (field_name in c("PAGO_MATRICULA", "PAGO_MENSUAL")) {
  value <- toupper(trimws(school[[field_name]]))
  check(paste(field_name, "only documented category labels are present"), !anyNA(value) && all(value %in% names(fee_map)))
  set(school, j = paste0(field_name, "_CODE"), value = unname(fee_map[value]))
}
# Hash previous inputs/results before any new output is generated.
prior_paths <- c(prior_income_input_path, file.path(prior_income_dir, c("ranking_roster.csv", "coefficients.csv", "type_specific_slopes.csv", "model_summary.csv")),
  list.files(file.path(prior_income_dir, "models"), pattern = "[.]rds$", full.names = TRUE),
  file.path(report_dir, c("parent_education_2022.html", "parent_income_decile_4to.html")))
stopifnot(all(file.exists(prior_paths)), !anyDuplicated(prior_paths))
preservation <- data.table(path = prior_paths, md5_before = unname(tools::md5sum(prior_paths)))
fwrite(preservation, file.path(clean_dir, "prior_outputs_preservation.csv"))
full <- as.data.table(readRDS(file.path(prior_income_dir, "ranking_parent_income_decile_4to.rds")))
roster <- fread(file.path(prior_income_dir, "ranking_roster.csv"))
check("Full ranking input preserves original applicants and school alternatives", nrow(full) == 1007253L && nrow(roster) == 294198L &&
  !anyDuplicated(full[, .(ranking_id, rbd)]) && !anyDuplicated(roster$ranking_id))
join_prices <- function(d) {
  ix <- match(d$rbd, school$rbd)
  d[, price_directory_matched := !is.na(ix)]
  for (v in c("AGNO", "PAGO_MATRICULA", "PAGO_MENSUAL", "PAGO_MATRICULA_CODE", "PAGO_MENSUAL_CODE"))
    set(d, j = if (v == "AGNO") "price_directory_year" else v, value = school[[v]][ix])
  d
}
full <- join_prices(full)
coverage <- full[, .(list_all_price_directory_matched = all(price_directory_matched)), by = ranking_id]
roster <- merge(roster, coverage, by = "ranking_id", all.x = TRUE, sort = FALSE)
roster[, price_estimation_eligible := income_estimation_eligible & list_all_price_directory_matched]
base_est <- as.data.table(readRDS(prior_income_input_path))
eligible <- roster[price_estimation_eligible == TRUE, ranking_id]
est <- join_prices(copy(base_est[ranking_id %in% eligible]))
est <- add_fee_indicators(est)
check("Price sample retains entire original lists with at least two schools", !anyDuplicated(est[, .(ranking_id, rbd)]) &&
  all(est[, .N, by = ranking_id]$N >= 2L) && setequal(unique(est$ranking_id), eligible) &&
  identical(est[, .(ranking_id, rbd, rank_school)], base_est[ranking_id %in% eligible, .(ranking_id, rbd, rank_school)]))
check("All estimation fee codes including unknown are defined", all(est$PAGO_MATRICULA_CODE %in% 0:6) && all(est$PAGO_MENSUAL_CODE %in% 0:6))
check("Unknown fee information is identical for both fee fields", identical(est$PAGO_MATRICULA_CODE == 0L, est$PAGO_MENSUAL_CODE == 0L))
check("Fee bands use free as reference and keep unknown separate", {
  all(vapply(c("PAGO_MATRICULA", "PAGO_MENSUAL"), function(f) {
    s <- fee_specs[field == f]
    identical(as.numeric(rowSums(as.matrix(est[, s$term, with = FALSE])) + est$fee_information_unknown), as.numeric(est[[paste0(f, "_CODE")]] != 1L))
  }, logical(1)))
})
fee_counts <- rbindlist(lapply(c("PAGO_MATRICULA", "PAGO_MENSUAL"), function(f) rbindlist(lapply(0:6, function(cd) {
  z <- est[get(paste0(f, "_CODE")) == cd]
  data.table(field = f, code = cd, label = unname(fee_labels[as.character(cd)]), n_schools = uniqueN(z$rbd), n_pairs = nrow(z), n_rankings = uniqueN(z$ranking_id))
}))))
flow <- rbindlist(lapply(names(group_labels), function(g) {
  z <- if (g == "pooled") roster else roster[group == g]
  data.table(group = g, label = group_labels[[g]], income_model_applicants = sum(z$income_estimation_eligible),
    excluded_missing_directory = sum(z$income_estimation_eligible & !z$list_all_price_directory_matched), price_model_applicants = sum(z$price_estimation_eligible))
}))
annual_flow <- roster[, .(income_model_applicants = sum(income_estimation_eligible),
  excluded_missing_directory = sum(income_estimation_eligible & !list_all_price_directory_matched), price_model_applicants = sum(price_estimation_eligible)), by = sae_proceso]
check("Previous attribute values and normalizations are unchanged", all(vapply(metric_terms, function(v) identical(est[[v]], base_est[[v]][base_est$ranking_id %in% eligible]), logical(1))))
fwrite(school, file.path(clean_dir, "school_prices.csv")); fwrite(roster, file.path(clean_dir, "ranking_roster.csv"))
fwrite(fee_specs, file.path(clean_dir, "fee_specs.csv")); fwrite(fee_counts, file.path(clean_dir, "fee_band_counts.csv"))
fwrite(flow, file.path(clean_dir, "sample_flow.csv")); fwrite(annual_flow, file.path(clean_dir, "sample_flow_by_year.csv"))
saveRDS(full, file.path(clean_dir, "rankings_with_directory_prices.rds")); saveRDS(est, price_input_path)
manifest_paths <- c(directory_path, prior_income_input_path, file.path(prior_income_dir, "ranking_parent_income_decile_4to.rds"))
fwrite(data.table(role = c("2024 directorio reported fees", "current income-model estimation data", "full submitted rankings"),
  path = manifest_paths, bytes = file.info(manifest_paths)$size, md5 = unname(tools::md5sum(manifest_paths))), file.path(clean_dir, "source_manifest.csv"))
save_checks("build_verification.csv")
message("Built prices for ", uniqueN(est$ranking_id), " rankings, ", nrow(est), " school pairs; ", nrow(checks), " checks passed.")
print(flow); print(fee_counts)
