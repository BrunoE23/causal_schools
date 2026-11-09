# Match PAES/PDT reported domicile region to the exact high-math regression IDs/year.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
task <- "high_math_main_region_cubic_income_contribution"
clean <- file.path(repo, "data/clean", task)
dir.create(clean, recursive = TRUE, showWarnings = FALSE)
sample_path <- file.path(repo, "data/clean/premium_enrollment_scores_income/analysis_data.rds")
prior_path <- file.path(repo, "data/clean/high_math_main_cubic_income_contribution/specification.rds")
sample_hashes <- unname(tools::md5sum(c(sample_path, prior_path)))
a <- as.data.table(readRDS(sample_path))[z_math >= 1]
prior <- readRDS(prior_path)
stopifnot(nrow(a) == 93423L, !anyDuplicated(a$mrun), identical(a$mrun, prior$ids))
years <- sort(unique(a$psu_year))
parts <- list(); manifests <- list(); audits <- list(); value_counts <- list()
for (year in years) {
  paths <- list.files(file.path(data_root, "data/raw", year),
    pattern = "^B_SOCIOECONOMICO_DOMICILIO.*[.]csv$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(paths) == 1L)
  path <- paths[1L]; info <- file.info(path)
  keep <- c("MRUN", "ANYO_PROCESO", "CODIGO_REGION_DOMICILIO")
  stopifnot(all(keep %in% names(fread(path, nrows = 0L))))
  # Preflight sizes are 15-47 MB/year; select only the three linking/region columns.
  message("Reading domicile region, admission ", year, ": ", round(info$size / 1e6, 1), " MB source")
  d <- fread(path, select = keep, na.strings = c("", "NA"))
  stopifnot(!anyNA(d$MRUN), all(d$ANYO_PROCESO == year), !anyDuplicated(d[, .(MRUN, ANYO_PROCESO)]))
  setnames(d, keep, c("mrun", "psu_year", "region_origin_code"))
  value_counts[[as.character(year)]] <- d[, .(N = .N), keyby = .(psu_year, region_origin_code)]
  target <- a[psu_year == year, .(mrun, psu_year)]
  d <- d[mrun %in% target$mrun]
  d[, region_source_matched := TRUE]
  joined <- merge(target, d, by = c("mrun", "psu_year"), all.x = TRUE, sort = FALSE)
  joined[is.na(region_source_matched), region_source_matched := FALSE]
  joined[, region_observed := region_origin_code %in% 1:16]
  audits[[as.character(year)]] <- joined[, .(psu_year = year, N_target = .N,
    n_matched = sum(region_source_matched), n_valid_region = sum(region_observed),
    n_unmatched = sum(!region_source_matched), n_matched_invalid = sum(region_source_matched & !region_observed))]
  parts[[as.character(year)]] <- joined
  stopifnot(file.info(path)$size == info$size, file.info(path)$mtime == info$mtime)
  manifests[[as.character(year)]] <- data.table(admission_year = year, path = normalizePath(path, winslash = "/"),
    bytes = info$size, modified = as.character(info$mtime), md5 = unname(tools::md5sum(path)),
    columns_read = paste(keep, collapse = ";"))
}
regions <- rbindlist(parts)
stopifnot(nrow(regions) == nrow(a), !anyDuplicated(regions$mrun))
ids <- a$mrun
# Append matched values without allowing merge to coerce the original ID storage type.
ri <- match(a$mrun, regions$mrun)
stopifnot(!anyNA(ri), all(a$psu_year == regions$psu_year[ri]))
for (v in c("region_origin_code", "region_source_matched", "region_observed")) set(a, j = v, value = regions[[v]][ri])
stopifnot(identical(a$mrun, ids), !anyNA(a$region_source_matched), !anyNA(a$region_observed))
fwrite(rbindlist(manifests), file.path(clean, "region_source_manifest.csv"))
fwrite(data.table(path = c(sample_path, prior_path), md5 = sample_hashes), file.path(clean, "sample_source_manifest.csv"))
fwrite(rbindlist(audits), file.path(clean, "region_linkage_by_year.csv"))
fwrite(rbindlist(value_counts), file.path(clean, "raw_region_values_by_year.csv"))
fwrite(a[, .(N = .N, n_region_observed = sum(region_observed), n_missing_region = sum(!region_observed)),
         keyby = income_decile_imputed], file.path(clean, "region_coverage_by_income.csv"))
fwrite(a[, .(N = .N), keyby = .(region_origin_code, region_source_matched, region_observed)],
       file.path(clean, "region_counts.csv"))
saveRDS(a, file.path(clean, "analysis_data.rds"))
stopifnot(identical(unname(tools::md5sum(c(sample_path, prior_path))), sample_hashes))
print(rbindlist(audits)); print(a[, .(N = .N), keyby = region_origin_code])
