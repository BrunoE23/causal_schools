suppressPackageStartupMessages({library(data.table); library(sf)})
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
raw <- file.path(root, "data", "raw")
clean <- file.path(root, "data", "clean")
out <- file.path(repo, "data", "clean", "higher_ed_geography")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
log_msg <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)
one_file <- function(folder, pattern) {
  p <- list.files(folder, pattern = pattern, full.names = TRUE, recursive = TRUE)
  stopifnot(length(p) == 1L)
  p
}
read_cols <- function(path, wanted) {
  header <- names(fread(path, nrows = 0, showProgress = FALSE))
  selected <- header[match(wanted, tolower(header))]
  stopifnot(!anyNA(selected))
  d <- fread(path, select = selected, na.strings = c("", "NA"), showProgress = FALSE)
  setnames(d, tolower(names(d)))
  d
}
log_msg("Reading selected universe columns")
students <- read_cols(file.path(clean, "univ_gr8_df.csv"),
  c("mrun", "student_id", "cohort_gr8", "edad_alu", "most_time_rbd", "sae_proceso",
    "timely_sae", "cod_reg_alu", "cod_com_alu", "psu_year", "codigo_region_egreso",
    "year_info_m1", "cod_sies_m1"))
students <- students[cohort_gr8 %in% 2017:2020 & !is.na(most_time_rbd) & most_time_rbd > 0 &
                       edad_alu %between% c(12, 16)]
setkey(students, mrun)
students <- students[!duplicated(mrun)]
stopifnot(!anyNA(students$mrun))
setnames(students, c("cod_reg_alu", "cod_com_alu", "codigo_region_egreso"),
         c("home_region_grade8", "home_comuna_grade8", "paes_graduation_school_region"))
students[, expected_entry_year := cohort_gr8 + 5L]
students[, sae_applicant := cohort_gr8 %in% 2018:2020 & timely_sae %in% 1L]
risk <- rbindlist(lapply(2018:2020, function(y) {
  d <- read_cols(file.path(clean, "DA_probs", paste0("DA_probs_", y, ".csv")), c("student_id", "prob"))
  stopifnot(!anyNA(d$prob), all(d$prob >= 0 & d$prob <= 1))
  d[, .(lottery_risk = max(prob) < 1), by = student_id][, sae_proceso := y]
}))
stopifnot(!anyDuplicated(risk, by = c("student_id", "sae_proceso")))
students <- merge(students, risk, by = c("student_id", "sae_proceso"), all.x = TRUE, sort = FALSE)
students[is.na(lottery_risk), lottery_risk := FALSE]
students[, at_risk := sae_applicant & lottery_risk]
previous <- fread(file.path(repo, "data", "clean", "baseline_math_quintiles", "baseline_math_coverage.csv"))
stopifnot(sum(students$sae_applicant) == previous[group == "SAE applicants", n_total],
          sum(students$at_risk) == previous[group == "At risk", n_total])

log_msg("Reading SAE home coordinates for the recorded SAE process")
sae <- rbindlist(lapply(2017:2020, function(y) {
  path <- one_file(file.path(raw, y, paste0("SAE_", y)), "^B1.*[.]csv$")
  d <- read_cols(path, c("mrun", "cod_nivel", "lat_con_error", "lon_con_error", "calidad_georef"))
  d <- d[cod_nivel == 9 & mrun %in% students$mrun]
  d[, `:=`(sae_proceso = y, lat = as.numeric(gsub(",", ".", as.character(lat_con_error), fixed = TRUE)),
            lon = as.numeric(gsub(",", ".", as.character(lon_con_error), fixed = TRUE)))]
  unique(d[, .(mrun, sae_proceso, lat, lon, calidad_georef)])
}))
stopifnot(!anyDuplicated(sae, by = c("mrun", "sae_proceso")))
sae <- merge(students[, .(mrun, sae_proceso)], sae, by = c("mrun", "sae_proceso"), all = FALSE)
sae[, coord_valid := is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180 & lat != 0 & lon != 0]
shape <- st_read(file.path(raw, "spatial", "chl_admin_boundaries", "chl_admin1.shp"), quiet = TRUE)
shape$region_code <- as.integer(sub("CL", "", shape$adm1_pcode, fixed = TRUE))
stopifnot(setequal(shape$region_code, 1:16))
# Common 16-region geography, including Nuble, avoids historical code changes.
# Exact polygon containment only: no nearest-region assignment for offshore points.
shape <- st_make_valid(st_transform(shape, 3857))
coords <- unique(sae[coord_valid == TRUE, .(lat, lon)])
log_msg("Spatial join of ", nrow(coords), " unique coordinates to 16 regions")
pts <- st_transform(st_as_sf(coords, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 3857)
hits <- st_intersects(pts, shape)
coords[, n_region_matches := lengths(hits)]
coords[, home_region_sae := vapply(hits, function(i) if (length(i) == 1L) shape$region_code[i] else NA_integer_, integer(1))]
sae <- merge(sae, coords, by = c("lat", "lon"), all.x = TRUE, sort = FALSE)
sae[, sae_region_status := fifelse(!coord_valid, "invalid_coordinates",
   fifelse(n_region_matches == 0L, "outside_region_polygons",
           fifelse(n_region_matches > 1L, "multiple_regions", "matched")))]
students <- merge(students,
  sae[, .(mrun, sae_proceso, home_region_sae, sae_georef_quality = calidad_georef, sae_region_status)],
  by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
students[is.na(sae_region_status), sae_region_status := "no_sae_grade9_record"]
fwrite(students[sae_applicant == TRUE, .(n = .N), by = .(sae_proceso, sae_georef_quality, sae_region_status)],
       file.path(out, "sae_geocoding_coverage.csv"))
rm(pts, shape, coords, sae, hits)
invisible(gc())

log_msg("Reading PAES domicile, preserving process-specific records")
paes_years <- sort(unique(na.omit(c(students$psu_year, students$expected_entry_year))))
paes <- rbindlist(lapply(paes_years, function(y) {
  path <- one_file(file.path(raw, y), "^B_SOCIOECONOMICO.*[.]csv$")
  d <- read_cols(path, c("mrun", "anyo_proceso", "codigo_region_domicilio", "codigo_comuna_domicilio"))
  d <- d[mrun %in% students$mrun]
  stopifnot(all(d$anyo_proceso == y), !anyDuplicated(d$mrun))
  setnames(d, c("anyo_proceso", "codigo_region_domicilio", "codigo_comuna_domicilio"),
           c("paes_process", "home_region_paes", "home_comuna_paes"))
  stopifnot(all(na.omit(d$home_region_paes) %in% 1:16))
  d
}))
stopifnot(!anyDuplicated(paes, by = c("mrun", "paes_process")))
students <- merge(students, paes, by.x = c("mrun", "psu_year"), by.y = c("mrun", "paes_process"),
                  all.x = TRUE, sort = FALSE)
expected <- copy(paes)
setnames(expected, c("paes_process", "home_region_paes", "home_comuna_paes"),
         c("expected_entry_year", "home_region_paes_expected", "home_comuna_paes_expected"))
students <- merge(students, expected, by = c("mrun", "expected_entry_year"), all.x = TRUE, sort = FALSE)
students[, sae_paes_region_changed := fifelse(is.na(home_region_sae) | is.na(home_region_paes),
                                             NA_integer_, as.integer(home_region_sae != home_region_paes))]
students[, grade8_paes_region_changed := fifelse(is.na(home_region_grade8) | is.na(home_region_paes),
                                                NA_integer_, as.integer(home_region_grade8 != home_region_paes))]
students[, sae_paes_expected_region_changed := fifelse(is.na(home_region_sae) | is.na(home_region_paes_expected),
   NA_integer_, as.integer(home_region_sae != home_region_paes_expected))]
stopifnot(!anyDuplicated(students$mrun), nrow(students) == 931547L)
fwrite(paes, file.path(out, "paes_domicile_by_process.csv"))
saveRDS(students, file.path(out, "home_regions.rds"))
fwrite(students, file.path(out, "home_regions.csv"))
log_msg("Saved ", nrow(students), " students with home-region definitions")
print(students[, .(n = .N, n_sae_region = sum(!is.na(home_region_sae)),
                    n_paes_region = sum(!is.na(home_region_paes)))])
