# Attach published student locations and admission-program locations without
# changing submitted program rows. Source common.R before calling this function.

geo_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

geo_haversine_km <- function(lon_from, lat_from, lon_to, lat_to) {
  # Geographic coordinates are longitude first, in decimal degrees.
  radians <- pi / 180
  dlat <- (lat_to - lat_from) * radians
  dlon <- (lon_to - lon_from) * radians
  h <- sin(dlat / 2)^2 + cos(lat_from * radians) *
    cos(lat_to * radians) * sin(dlon / 2)^2
  2 * 6371.0088 * asin(sqrt(pmin(1, pmax(0, h))))
}

attach_geography <- function(programs, roster) {
  required_program <- c("mrun", "sae_proceso", "rbd", "cod_curso", "rank_program")
  required_roster <- c("mrun", "sae_proceso", "cohort_gr8")
  check("Geography program keys available", all(required_program %in% names(programs)))
  check("Geography roster keys available", all(required_roster %in% names(roster)))
  check("Geography input has program rows", nrow(programs) > 0L)
  roster_key <- as.data.table(roster)[, ..required_roster]
  check("Geography roster keys unique",
    !anyDuplicated(roster_key[, .(mrun, sae_proceso)]))
  check("Geography roster cohorts complete", !anyNA(roster_key$cohort_gr8))

  out <- copy(as.data.table(programs))
  input_columns <- copy(names(out))
  input_n <- nrow(out)
  new_fields <- c("student_lat", "student_lon", "georef_quality", "school_lat", "school_lon",
    "cod_sede", "home_comuna_code", "home_comuna_name", "home_comuna_lat", "home_comuna_lon",
    "municipality_imputed", "distance_main_km", "distance_comuna_km",
    "log1p_distance_main", "log1p_distance_comuna", "student_coord_source", "school_coord_source",
    "home_comuna_coord_source", "home_comuna_name_corrected")
  check("Geography does not overwrite existing columns", !any(new_fields %in% input_columns))
  check("Geography temporary fields absent",
    !any(c("geo_row_id", "geo_program_key", "geo_cohort_gr8") %in% input_columns))
  out[, geo_row_id := .I]
  numeric_program <- suppressWarnings(as.numeric(as.character(out$cod_curso)))
  check("Geography program codes safely represented as integers",
    all(is.finite(numeric_program) & numeric_program == floor(numeric_program) &
      abs(numeric_program) < 2^53))
  out[, geo_program_key := sprintf("%.0f", numeric_program)]
  out[roster_key, on = .(mrun, sae_proceso), geo_cohort_gr8 := i.cohort_gr8]
  check("Geography all submitted rows have a roster cohort", !anyNA(out$geo_cohort_gr8))
  years <- sort(unique(as.integer(out$sae_proceso)))
  check("Geography process years covered by verified caches", all(years %in% 2018:2020))

  student_path <- file.path(data_root, "data/clean/sae_2017_19_stud_controls.RData")
  student_env <- new.env(parent = baseenv())
  load(student_path, envir = student_env)
  check("Geography student cache object available", exists("sae_stud_info", student_env, inherits = FALSE))
  students <- as.data.table(student_env$sae_stud_info)[, .(
    mrun, sae_proceso = as.integer(proceso), student_lat = geo_numeric(lat_con_error),
    student_lon = geo_numeric(lon_con_error), georef_quality = as.integer(calidad_georef))]
  check("Geography student cache keys unique", !anyDuplicated(students[, .(mrun, sae_proceso)]))
  out[students, on = .(mrun, sae_proceso), `:=`(
    student_lat = i.student_lat, student_lon = i.student_lon, georef_quality = i.georef_quality)]

  offer_paths <- vapply(years, one_source, character(1), prefix = "A1")
  offers <- rbindlist(lapply(seq_along(years), function(i) {
    x <- fread(offer_paths[i], sep = ";", showProgress = FALSE,
      select = c("rbd", "cod_nivel", "cod_curso", "cod_sede", "lat", "lon"),
      colClasses = c(cod_curso = "character"))
    x <- x[cod_nivel == 9L]
    key_numeric <- suppressWarnings(as.numeric(x$cod_curso))
    check(paste("Geography A1 program codes valid", years[i]),
      all(is.finite(key_numeric) & key_numeric == floor(key_numeric) & abs(key_numeric) < 2^53))
    x[, .(sae_proceso = years[i], rbd, geo_program_key = sprintf("%.0f", key_numeric),
      cod_sede, school_lat = geo_numeric(lat), school_lon = geo_numeric(lon))]
  }))
  check("Geography A1 school-program-year keys unique",
    !anyDuplicated(offers[, .(sae_proceso, rbd, geo_program_key)]))
  offers[, geo_offer_matched := TRUE]
  out[offers, on = .(sae_proceso, rbd, geo_program_key), `:=`(
    cod_sede = i.cod_sede, school_lat = i.school_lat, school_lon = i.school_lon,
    geo_offer_matched = i.geo_offer_matched)]
  check("Geography every submitted program matches A1", all(out$geo_offer_matched %in% TRUE))

  control_path <- file.path(data_root, "data/clean/universe_controls.csv")
  home <- fread(control_path, showProgress = FALSE,
    select = c("MRUN", "cohort_gr8", "COD_COM_ALU", "NOM_COM_ALU"))
  setnames(home, c("MRUN", "cohort_gr8", "COD_COM_ALU", "NOM_COM_ALU"),
    c("mrun", "geo_cohort_gr8", "home_comuna_code", "home_comuna_name"))
  home[, home_comuna_code := as.integer(home_comuna_code)]
  check("Geography residence-control keys unique", !anyDuplicated(home[, .(mrun, geo_cohort_gr8)]))
  out[home, on = .(mrun, geo_cohort_gr8), `:=`(
    home_comuna_code = i.home_comuna_code, home_comuna_name = i.home_comuna_name)]
  check("Geography residence municipality complete", !anyNA(out$home_comuna_code) &
    all(out$home_comuna_code > 0L) & !anyNA(out$home_comuna_name))

  center_path <- file.path(data_root, "data/clean/comuna_info.RDS")
  centers <- as.data.table(readRDS(center_path))[, .(
    home_comuna_code = as.integer(CODIGO_COMUNA),
    home_comuna_lat = geo_numeric(LAT), home_comuna_lon = geo_numeric(LON))]
  check("Geography municipality-center codes unique", !anyDuplicated(centers$home_comuna_code))
  # Join on official numeric code. The cached name for 1401 is incorrectly
  # Tocopilla; residence controls correctly name it Pozo Almonte. Coordinates
  # and the code are distinct from the actual Tocopilla row (2301).
  out[centers, on = "home_comuna_code", `:=`(
    home_comuna_lat = i.home_comuna_lat, home_comuna_lon = i.home_comuna_lon)]
  out[, home_comuna_name_corrected := home_comuna_code == 1401L]
  for (prefix in c("student", "school", "home_comuna")) {
    lat <- out[[paste0(prefix, "_lat")]]
    lon <- out[[paste0(prefix, "_lon")]]
    check(paste("Geography complete valid world coordinates", prefix),
      all(is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180))
  }
  check("Geography published quality classification complete", all(out$georef_quality %in% 1:5))
  check("Geography matched campus identifiers complete", !anyNA(out$cod_sede))

  out[, `:=`(
    municipality_imputed = georef_quality == 4L,
    student_coord_source = "Published SAE B1 student coordinates (existing clean cache)",
    school_coord_source = "Published SAE A1 admission-program coordinates",
    home_comuna_coord_source = "Existing numeric-code comuna center lookup (WGS84)",
    distance_main_km = geo_haversine_km(student_lon, student_lat, school_lon, school_lat),
    distance_comuna_km = geo_haversine_km(home_comuna_lon, home_comuna_lat, school_lon, school_lat))]
  out[, `:=`(log1p_distance_main = log1p(distance_main_km),
    log1p_distance_comuna = log1p(distance_comuna_km))]
  check("Geography distances and transforms finite and nonnegative",
    all(is.finite(out$distance_main_km) & out$distance_main_km >= 0 &
      is.finite(out$distance_comuna_km) & out$distance_comuna_km >= 0 &
      is.finite(out$log1p_distance_main) & is.finite(out$log1p_distance_comuna)))

  summarize_geo <- function(x, year_label) data.table(
    sae_proceso = year_label, program_rows = nrow(x),
    applicant_years = uniqueN(x[, .(mrun, sae_proceso)]),
    applicant_school_pairs = uniqueN(x[, .(mrun, sae_proceso, rbd)]),
    student_coordinates_valid = sum(is.finite(x$student_lat) & is.finite(x$student_lon)),
    school_programs_matched = sum(x$geo_offer_matched %in% TRUE),
    school_coordinates_valid = sum(is.finite(x$school_lat) & is.finite(x$school_lon)),
    home_comuna_centers_matched = sum(is.finite(x$home_comuna_lat) & is.finite(x$home_comuna_lon)),
    main_distance_valid = sum(is.finite(x$distance_main_km)),
    comuna_distance_valid = sum(is.finite(x$distance_comuna_km)),
    municipality_imputed_program_rows = sum(x$municipality_imputed),
    directory_coordinate_fallback_rows = 0L,
    main_distance_median_km = median(x$distance_main_km),
    main_distance_p99_km = unname(quantile(x$distance_main_km, .99)),
    main_distance_max_km = max(x$distance_main_km))
  coverage <- rbindlist(c(lapply(years, function(yr) {
    summarize_geo(out[sae_proceso == yr], as.character(yr))
  }), list(summarize_geo(out, "Pooled"))))
  fwrite(coverage, file.path(clean_dir, "geography_coverage.csv"))

  quality_definitions <- data.table(georef_quality = 1:5,
    quality_label = c("Rooftop or interpolated address", "Approximate geocoder location",
      "Multiple coherent geocoder results", "Municipality coordinates imputed", "User-shared location"),
    definition = c("Unique geocoder response: rooftop or range_interpolated",
      "Unique response: geometric_center; 2019-2020 manual also includes approximate",
      "Center of multiple coherent geocoder responses; observed in 2018",
      "Municipality location substituted for the declared student address",
      "Location shared by the user; introduced in the 2019-2020 files"))
  student_quality <- unique(out[, .(mrun, sae_proceso, georef_quality)])
  quality <- rbindlist(c(lapply(years, function(yr) {
    x <- student_quality[sae_proceso == yr]
    z <- merge(quality_definitions, x[, .(applicant_years = .N), by = georef_quality],
      by = "georef_quality", all.x = TRUE, sort = TRUE)
    z[is.na(applicant_years), applicant_years := 0L]
    z[, `:=`(sae_proceso = as.character(yr), share = applicant_years / nrow(x))]
    z
  }), list({
    z <- merge(quality_definitions, student_quality[, .(applicant_years = .N), by = georef_quality],
      by = "georef_quality", all.x = TRUE, sort = TRUE)
    z[is.na(applicant_years), applicant_years := 0L]
    z[, `:=`(sae_proceso = "Pooled", share = applicant_years / nrow(student_quality))]
    z
  })))
  setcolorder(quality, c("sae_proceso", "georef_quality", "quality_label", "definition", "applicant_years", "share"))
  fwrite(quality, file.path(clean_dir, "geography_quality.csv"))

  campus <- offers[, .(programs = .N, campuses = uniqueN(cod_sede),
    coordinate_pairs = uniqueN(.SD[, .(school_lat, school_lon)])), by = .(sae_proceso, rbd)]
  campus <- campus[coordinate_pairs > 1L]
  campus[, max_separation_km := vapply(seq_len(.N), function(i) {
    xy <- unique(offers[sae_proceso == campus$sae_proceso[i] & rbd == campus$rbd[i],
      .(school_lat, school_lon)])
    pairs <- combn(seq_len(nrow(xy)), 2L)
    max(geo_haversine_km(xy$school_lon[pairs[1L, ]], xy$school_lat[pairs[1L, ]],
      xy$school_lon[pairs[2L, ]], xy$school_lat[pairs[2L, ]]))
  }, numeric(1))]
  setorder(campus, sae_proceso, -max_separation_km, rbd)
  fwrite(campus, file.path(clean_dir, "geography_campus_conflicts.csv"))

  geo_manifest <- rbindlist(list(
    manifest_rows(student_path, "Student coordinates and geocoder quality: verified existing SAE B1 cache"),
    manifest_rows(offer_paths, "Admission-program school/campus coordinates: raw SAE A1 regular offers"),
    manifest_rows(control_path, "Student residence municipality: grade-8 universe controls"),
    manifest_rows(center_path, "Residence municipality centers: existing numeric-code lookup")))
  fwrite(geo_manifest, file.path(clean_dir, "geography_manifest.csv"))
  notes <- c(
    "Main distance uses every published student coordinate, including quality 4 municipality imputations.",
    "municipality_imputed flags quality 4; no applicant is dropped or re-geocoded because of that quality code.",
    "Robustness replaces only the student location by the center of the grade-8 residence comuna; the same actual A1 school-program campus remains the destination.",
    "Distances are straight-line Haversine distances in kilometers, longitude first, using mean Earth radius 6371.0088 km. Regressors are log1p(distance_km).",
    "Published student locations refer to the declared location and can differ from the current residence.",
    "2018 privacy noise: independent uniform perturbations of latitude and longitude between -0.0013 and +0.0013 degrees. Manual reports mean displacement 101 m and maximum 198 m.",
    "2019-2020 privacy noise: uniform direction 0-360 degrees and uniform radial displacement 50-300 m, mean 175 m. B1 draws are repeated when displaced points are not on land.",
    "Quality 1 is rooftop/range_interpolated; quality 2 is geometric_center/approximate; quality 3 is the center of multiple coherent geocoder responses; quality 4 is municipality imputation; quality 5 is a user-shared location.",
    "Comuna centers come from the existing comuna_info.RDS lookup, generated from raw/spatial/chl_admin_boundaries/chl_admin3.shp center_lat/center_lon (WGS84). They are comuna representative centers, not municipal-office coordinates.",
    "Known source-label error: comuna_info.RDS and its source DBF call code 1401 TOCOPILLA. That code is Pozo Almonte; the distinct actual Tocopilla code is 2301. Joins use numeric codes, preserve each code's coordinates, and use the official residence name from universe_controls.csv. home_comuna_name_corrected flags code 1401; no raw or existing clean source file is altered.",
    "The paper's 2018-2020 residence records already use the new 16xxx Nuble comuna codes; all match the lookup. No historical-code crosswalk is applied.",
    "Some RBDs have multiple campus coordinates. Coordinates attach to rbd/cod_curso/process, so downstream school collapse keeps the first explicitly submitted program's campus, never an arbitrary school-level row.",
    "Every submitted program has an A1 coordinate match in the audited paper sample. No school-directory fallback is used.",
    "Coordinate validity checks use global latitude/longitude ranges and do not discard Easter Island or Antarctic records.")
  writeLines(notes, file.path(clean_dir, "geography_notes.txt"))

  check("Geography preserves submitted row count and order",
    nrow(out) == input_n & identical(out$geo_row_id, seq_len(input_n)))
  unchanged_columns <- vapply(input_columns, function(nm) identical(out[[nm]], programs[[nm]]), logical(1))
  check("Geography preserves all input columns exactly", all(unchanged_columns),
    paste(input_columns[!unchanged_columns], collapse = ", "))
  out[, c("geo_row_id", "geo_program_key", "geo_cohort_gr8", "geo_offer_matched") := NULL]
  setattr(out, "geography_manifest", geo_manifest)
  out
}
