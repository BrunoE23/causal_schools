suppressPackageStartupMessages(library(data.table))
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
raw <- file.path(root, "data", "raw")
out <- file.path(repo, "data", "clean", "higher_ed_geography")
setDTthreads(2)
log_msg <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)
normalize_text <- function(x) toupper(trimws(iconv(x, from = "", to = "ASCII//TRANSLIT")))
region_names <- c("Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso",
                  "Lib. Gral. B. O'Higgins", "Maule", "Biobio", "La Araucania",
                  "Los Lagos", "Aysen", "Magallanes", "Metropolitana", "Los Rios",
                  "Arica y Parinacota", "Nuble")
region_lookup <- setNames(1:16, normalize_text(region_names))
region_lookup["BIOBIO"] <- 8L
home <- readRDS(file.path(out, "home_regions.rds"))
stopifnot(!anyDuplicated(home$mrun))

# Read 8 columns, filter to eligible courses and target students before joining.
# Full SIES is required: PAES admissions matricula is not a substitute.
annual <- rbindlist(lapply(2022:2025, function(y) {
  paths <- list.files(file.path(raw, y, paste0("Matricula-Ed-Superior-", y)),
                      pattern = "MRUN[.]csv$", full.names = TRUE)
  stopifnot(length(paths) == 1L)
  log_msg("Reading selected columns of full SIES ", y)
  header <- names(fread(paths, nrows = 0, showProgress = FALSE))
  wanted <- c("mrun", "codigo_unico", "region_sede", "nivel_carrera_2",
              "anio_ing_carr_ori", "sem_ing_carr_ori", "modalidad", "comuna_sede")
  selected <- header[match(wanted, tolower(header))]
  stopifnot(!anyNA(selected))
  d <- fread(paths, select = selected, na.strings = c("", "NA"), showProgress = FALSE)
  setnames(d, tolower(names(d)))
  d <- d[normalize_text(nivel_carrera_2) %in% c("CARRERAS PROFESIONALES", "CARRERAS TECNICAS") &
           mrun %in% home$mrun]
  d[, `:=`(enrollment_year = y, program_code = trimws(as.character(codigo_unico)),
            campus_region = as.integer(region_lookup[normalize_text(region_sede)]))]
  unknown <- d[is.na(campus_region), .N, by = region_sede]
  if (nrow(unknown)) {
    fwrite(unknown, file.path(out, paste0("unmapped_campus_regions_", y, ".csv")))
    stop("Unmapped SIES region names: inspect diagnostics before proceeding")
  }
  log_msg("  Retained ", nrow(d), " enrollment records")
  unique(d[, .(mrun, enrollment_year, program_code, campus_region,
                anio_ing_carr_ori, sem_ing_carr_ori, modalidad)])
}))
stopifnot(all(annual$campus_region %in% 1:16), all(annual$enrollment_year %in% 2022:2025))
saveRDS(annual, file.path(out, "annual_enrollment_records.rds"))
fwrite(annual[, .(n_records = .N, n_students = uniqueN(mrun)), by = .(enrollment_year, campus_region)],
       file.path(out, "enrollment_scan_summary.csv"))

# Exact match to the already-selected first ingreso in the main universe.
# Do not select a different program or entry year based on row ordering.
home[, enrolled_m1 := as.integer(!is.na(cod_sies_m1) & nzchar(trimws(cod_sies_m1)) & cod_sies_m1 != "NA")]
first_keys <- home[enrolled_m1 == 1L,
                    .(mrun, enrollment_year = as.integer(year_info_m1), program_code = trimws(cod_sies_m1))]
stopifnot(!anyNA(first_keys$enrollment_year), all(first_keys$enrollment_year %in% 2022:2025))
first_records <- merge(first_keys, annual, by = c("mrun", "enrollment_year", "program_code"), all = FALSE)
first_map <- first_records[, .(n_matching_records = .N,
    n_campus_regions_m1 = uniqueN(campus_region),
    campus_region_m1 = if (uniqueN(campus_region) == 1L) campus_region[1L] else NA_integer_,
    campus_modality_m1 = paste(sort(unique(modalidad)), collapse = " | "),
    n_matched_entry_year = sum(anio_ing_carr_ori == enrollment_year, na.rm = TRUE)), by = mrun]
students <- merge(home, first_map, by = "mrun", all.x = TRUE, sort = FALSE)
students[, campus_location_matched_m1 := enrolled_m1 == 1L & !is.na(n_matching_records)]
students[, paes_home_after_first_entry := enrolled_m1 == 1L & !is.na(psu_year) & psu_year > year_info_m1]

binary_location <- function(enrolled, campus, reference) {
  ans <- rep(NA_integer_, length(enrolled))
  ans[enrolled == 0L] <- 0L
  ok <- enrolled == 1L & !is.na(campus) & !is.na(reference)
  ans[ok] <- as.integer(campus[ok] != reference[ok])
  ans
}
students[, in_rm_m1 := binary_location(enrolled_m1, campus_region_m1, rep(13L, .N))]
students[!is.na(in_rm_m1) & enrolled_m1 == 1L, in_rm_m1 := 1L - in_rm_m1]
students[, outside_home_sae_m1 := binary_location(enrolled_m1, campus_region_m1, home_region_sae)]
students[, outside_home_grade8_m1 := binary_location(enrolled_m1, campus_region_m1, home_region_grade8)]
# Home at the linked PSU/PAES process; do not label a post-entry address as baseline.
students[, home_region_paes_pre_entry := fifelse(paes_home_after_first_entry, NA_integer_, as.integer(home_region_paes))]
students[, outside_home_paes_m1 := binary_location(enrolled_m1, campus_region_m1, home_region_paes_pre_entry)]
students[, outside_home_paes_expected_m1 := binary_location(enrolled_m1, campus_region_m1,
  fifelse(expected_entry_year > year_info_m1 & enrolled_m1 == 1L, NA_integer_, as.integer(home_region_paes_expected)))]

# Separately describe currently observed enrollment in the latest full SIES year.
# Multiple enrollments are set-based: any campus in RM / outside home qualifies.
now <- merge(annual[enrollment_year == 2025L],
  home[, .(mrun, home_region_sae, home_region_grade8, home_region_paes, psu_year)], by = "mrun", all.x = TRUE)
any_status <- function(z) if (any(z == 1L, na.rm = TRUE)) 1L else if (anyNA(z)) NA_integer_ else 0L
now[, outside_paes := fifelse(is.na(psu_year) | psu_year > 2025L, NA_integer_,
                              as.integer(campus_region != home_region_paes))]
now_summary <- now[, .(enrolled_2025 = 1L, n_campus_regions_2025 = uniqueN(campus_region),
    in_rm_2025 = as.integer(any(campus_region == 13L)),
    outside_home_sae_2025 = any_status(as.integer(campus_region != home_region_sae)),
    outside_home_grade8_2025 = any_status(as.integer(campus_region != home_region_grade8)),
    outside_home_paes_2025 = any_status(outside_paes)), by = mrun]
students <- merge(students, now_summary, by = "mrun", all.x = TRUE, sort = FALSE)
new_cols <- c("enrolled_2025", "in_rm_2025", "outside_home_sae_2025",
              "outside_home_grade8_2025", "outside_home_paes_2025")
students[is.na(enrolled_2025), (new_cols) := lapply(new_cols, function(x) 0L)]
stopifnot(nrow(students) == nrow(home), !anyDuplicated(students$mrun),
          all(students[enrolled_m1 == 0L, in_rm_m1] == 0L),
          all(students[enrolled_2025 == 0L, in_rm_2025] == 0L),
          all(students$in_rm_m1 <= students$enrolled_m1, na.rm = TRUE))
fwrite(students, file.path(out, "student_higher_ed_geography.csv"))
saveRDS(students, file.path(out, "student_higher_ed_geography.rds"))
fwrite(students[enrolled_m1 == 1L, .N, by = .(campus_location_matched_m1, n_campus_regions_m1)],
       file.path(out, "first_enrollment_region_matching.csv"))

groups <- list("Broad panel" = students, "SAE applicants" = students[sae_applicant == TRUE],
                "At risk" = students[at_risk == TRUE])
summary <- rbindlist(lapply(names(groups), function(g) {
  s <- groups[[g]]
  rbindlist(lapply(c("m1", "2025"), function(w) {
    enrolled <- s[[paste0("enrolled_", w)]] == 1L
    measures <- paste0(c("in_rm_", "outside_home_sae_", "outside_home_paes_", "outside_home_grade8_"), w)
    rbindlist(lapply(measures, function(v) {
      x <- s[[v]]
      known <- !is.na(x)
      data.table(group = g, window = w, measure = v, n_total = nrow(s),
        n_enrolled = sum(enrolled), n_yes = sum(x == 1L, na.rm = TRUE),
        n_observed_all = sum(known), n_observed_enrolled = sum(known & enrolled),
        pct_all_observed = 100 * mean(x, na.rm = TRUE),
        pct_enrolled_observed = 100 * mean(x[enrolled], na.rm = TRUE),
        pct_coverage_enrolled = 100 * mean(known[enrolled]))
    }))
  }))
}))
movement <- rbindlist(lapply(names(groups), function(g) {
  s <- groups[[g]]
  sets <- list("All" = s, "Georef quality 1 or 5" = s[sae_georef_quality %in% c(1L, 5L)],
               "First-enrollment students" = s[enrolled_m1 == 1L])
  rbindlist(lapply(names(sets), function(set_name) {
    a <- sets[[set_name]]
    measures <- c("sae_paes_region_changed", "sae_paes_expected_region_changed", "grade8_paes_region_changed")
    rbindlist(lapply(measures, function(v) {
      x <- a[[v]]
      data.table(group = g, subset = set_name, measure = v, n_total = nrow(a),
        n_matched = sum(!is.na(x)), n_changed = sum(x == 1L, na.rm = TRUE),
        pct_changed_matched = 100 * mean(x, na.rm = TRUE))
    }))
  }))
}))
transitions <- rbindlist(lapply(names(groups), function(g) {
  a <- groups[[g]][!is.na(home_region_sae) & !is.na(home_region_paes)]
  z <- a[, .(n = .N), by = .(home_region_sae, home_region_paes)]
  z[, `:=`(group = g, pct_of_origin = 100 * n / sum(n)), by = home_region_sae]
  z
}))
origin_summary <- rbindlist(lapply(names(groups), function(g) {
  a <- groups[[g]][!is.na(home_region_sae) & !is.na(home_region_paes)]
  a[, .(group = g, n_matched = .N, n_changed = sum(sae_paes_region_changed),
         pct_changed = 100 * mean(sae_paes_region_changed),
         n_to_rm = sum(home_region_sae != 13L & home_region_paes == 13L)), by = home_region_sae]
}))
matched_summaries <- rbindlist(lapply(names(groups), function(g) {
  a <- groups[[g]][enrolled_m1 == 1L & !is.na(campus_region_m1) &
                  !is.na(home_region_sae) & !is.na(home_region_paes_pre_entry)]
  data.table(group = g, n_same_sample = nrow(a), pct_in_rm = 100 * mean(a$in_rm_m1),
    pct_outside_sae = 100 * mean(a$outside_home_sae_m1),
    pct_outside_paes = 100 * mean(a$outside_home_paes_m1),
    pct_changed_sae_paes = 100 * mean(a$sae_paes_region_changed))
}))
fwrite(summary, file.path(out, "enrollment_location_summary.csv"))
fwrite(movement, file.path(out, "home_region_movement_summary.csv"))
fwrite(transitions, file.path(out, "sae_to_paes_region_transitions.csv"))
fwrite(origin_summary, file.path(out, "home_region_movement_by_origin.csv"))
fwrite(matched_summaries, file.path(out, "enrollment_location_common_sample.csv"))
log_msg("Saved student indicators, coverage, summaries and region transitions")
print(summary[group != "Broad panel" & window == "m1", .(group, measure, n_enrolled, n_yes,
  n_observed_enrolled, pct_enrolled_observed, pct_coverage_enrolled)])
print(movement[group != "Broad panel" & subset == "All"])
print(matched_summaries)
