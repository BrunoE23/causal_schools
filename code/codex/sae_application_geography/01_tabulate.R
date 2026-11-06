suppressPackageStartupMessages({library(data.table); library(sf)})
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "sae_application_geography")
tab <- file.path(repo, "output", "tables", "sae_application_geography")
for (p in c(out, tab)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
students <- readRDS(file.path(repo, "data", "clean", "higher_ed_geography", "home_regions.rds"))
setDT(students)
students <- students[sae_applicant == TRUE, .(mrun, sae_proceso, home_region_sae, sae_georef_quality, at_risk)]
stopifnot(nrow(students) == 294198L, sum(students$at_risk) == 162633L,
          !anyDuplicated(students[, .(mrun, sae_proceso)]))
one_file <- function(year, prefix) {
  p <- list.files(file.path(root, "data", "raw", year, paste0("SAE_", year)),
    pattern = paste0("^", prefix, ".*[.]csv$"), full.names = TRUE)
  stopifnot(length(p) == 1L)
  p
}

# Each C1 input is below 100 MB (about 48/87/78 MB); read just five columns,
# filter to the existing Grade 9 applicant sample before any joins.
applications <- rbindlist(lapply(2018:2020, function(year) {
  x <- fread(one_file(year, "C1_"), select = c("mrun", "cod_nivel", "rbd", "preferencia_postulante", "agregada_por_continuidad"),
             na.strings = c("", "NA"), showProgress = FALSE)
  x <- x[cod_nivel == 9L & mrun %in% students[sae_proceso == year, mrun]]
  stopifnot(!anyNA(x$rbd), all(x$rbd > 0), !anyNA(x$agregada_por_continuidad),
            all(x$agregada_por_continuidad %in% 0:1))
  x[, .(mrun, sae_proceso = year, school_rbd = as.numeric(rbd),
        preference = preferencia_postulante, automatic_continuity = agregada_por_continuidad == 1L)]
}))
fwrite(applications[, .(n_program_rows = .N, n_automatic_continuity_rows = sum(automatic_continuity)), by = sae_proceso],
       file.path(out, "application_row_audit.csv"))

schools <- rbindlist(lapply(2018:2020, function(year) {
  x <- fread(one_file(year, "A1_"), select = c("rbd", "cod_nivel", "lat", "lon"),
             na.strings = c("", "NA"), showProgress = FALSE)
  x <- x[cod_nivel == 9L & rbd %in% applications[sae_proceso == year, school_rbd]]
  unique(x[, .(school_rbd = as.numeric(rbd), sae_proceso = year, lat = as.numeric(lat), lon = as.numeric(lon))])
}))
# Same common 16-region polygons and exact-containment convention as SAE homes,
# avoiding any mismatch in historical Nuble/Biobio administrative coding.
shape <- st_read(file.path(root, "data", "raw", "spatial", "chl_admin_boundaries", "chl_admin1.shp"), quiet = TRUE)
shape$region_code <- as.integer(sub("CL", "", shape$adm1_pcode, fixed = TRUE))
stopifnot(setequal(shape$region_code, 1:16))
shape <- st_make_valid(st_transform(shape, 3857))
coords <- unique(schools[is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180 & lat != 0 & lon != 0, .(lat, lon)])
pts <- st_transform(st_as_sf(coords, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 3857)
hits <- st_intersects(pts, shape)
coords[, `:=`(n_region_matches = lengths(hits), school_region = vapply(hits,
  function(j) if (length(j) == 1L) shape$region_code[j] else NA_integer_, integer(1)))]
schools <- merge(schools, coords, by = c("lat", "lon"), all.x = TRUE, sort = FALSE)
school_regions <- schools[, .(n_locations = .N, n_unknown_locations = sum(is.na(school_region)),
  n_distinct_known_regions = uniqueN(school_region[!is.na(school_region)]),
  school_region = if (uniqueN(school_region[!is.na(school_region)]) == 1L)
    unique(school_region[!is.na(school_region)]) else NA_integer_), by = .(school_rbd, sae_proceso)]
stopifnot(!anyDuplicated(school_regions[, .(school_rbd, sae_proceso)]))
fwrite(school_regions, file.path(out, "school_region_assignment_audit.csv"))
rm(shape, pts, hits, coords, schools)

all_choices <- unique(applications[, .(mrun, sae_proceso, school_rbd)])
submitted_choices <- unique(applications[automatic_continuity == FALSE, .(mrun, sae_proceso, school_rbd)])
stopifnot(!anyDuplicated(submitted_choices))
summarize_version <- function(choices, version) {
  choices <- merge(choices, students, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
  choices <- merge(choices, school_regions[, .(school_rbd, sae_proceso, school_region)],
                    by = c("school_rbd", "sae_proceso"), all.x = TRUE, sort = FALSE)
  stopifnot(!anyNA(choices$at_risk))
  choices[, outside_home := fifelse(is.na(home_region_sae) | is.na(school_region), NA, school_region != home_region_sae)]
  profile <- choices[, .(n_school_choices = .N, n_known_choices = sum(!is.na(outside_home)),
    n_home_choices = sum(outside_home == FALSE, na.rm = TRUE), n_outside_choices = sum(outside_home == TRUE, na.rm = TRUE)),
    by = .(mrun, sae_proceso)]
  profile <- merge(students, profile, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
  profile[is.na(n_school_choices), `:=`(n_school_choices = 0L, n_known_choices = 0L, n_home_choices = 0L, n_outside_choices = 0L)]
  profile[, fully_classified := n_school_choices > 0L & n_known_choices == n_school_choices]
  profile[, application_profile := fcase(n_school_choices == 0L, "No school choices",
    !fully_classified, "Unknown geography", n_outside_choices == 0L, "Home region only",
    n_home_choices == 0L, "Outside home region only", default = "Both home and outside")]
  choice_summaries <- applicant_summaries <- list()
  for (g in c("SAE applicants", "At risk")) {
    c <- if (g == "At risk") choices[at_risk == TRUE] else choices
    s <- if (g == "At risk") profile[at_risk == TRUE] else profile
    n_known <- sum(!is.na(c$outside_home))
    n_students_known <- sum(s$fully_classified)
    choice_summaries[[g]] <- data.table(version = version, group = g, n_all_school_choices = nrow(c),
      n_known_school_choices = n_known, n_missing_geography_choices = nrow(c) - n_known,
      n_home_choices = sum(c$outside_home == FALSE, na.rm = TRUE), n_outside_choices = sum(c$outside_home == TRUE, na.rm = TRUE),
      pct_home_choices = 100 * sum(c$outside_home == FALSE, na.rm = TRUE) / n_known,
      pct_outside_choices = 100 * sum(c$outside_home == TRUE, na.rm = TRUE) / n_known)
    applicant_summaries[[g]] <- data.table(version = version, group = g, n_applicants = nrow(s),
      n_with_school_choices = sum(s$n_school_choices > 0L), n_fully_classified = n_students_known,
      n_home_only = sum(s$application_profile == "Home region only"),
      n_outside_only = sum(s$application_profile == "Outside home region only"),
      n_both = sum(s$application_profile == "Both home and outside"),
      n_unknown_geography = sum(s$application_profile == "Unknown geography"), n_no_choices = sum(s$n_school_choices == 0L),
      n_any_outside_fully_classified = sum(s$fully_classified & s$n_outside_choices > 0L),
      pct_any_outside_fully_classified = 100 * sum(s$fully_classified & s$n_outside_choices > 0L) / n_students_known,
      n_any_known_outside = sum(s$n_outside_choices > 0L))
  }
  cs <- rbindlist(choice_summaries); aps <- rbindlist(applicant_summaries)
  stopifnot(all(cs$n_home_choices + cs$n_outside_choices == cs$n_known_school_choices),
    all(aps$n_home_only + aps$n_outside_only + aps$n_both == aps$n_fully_classified),
    all(aps$n_fully_classified + aps$n_unknown_geography + aps$n_no_choices == aps$n_applicants))
  fwrite(cs, file.path(out, paste0(version, "_school_choice_summary.csv")))
  fwrite(aps, file.path(out, paste0(version, "_applicant_summary.csv")))
  fwrite(profile, file.path(out, paste0(version, "_student_application_profiles.csv")))
  fwrite(choices, file.path(out, paste0(version, "_school_choices_with_regions.csv")))
  list(choices = cs, applicants = aps)
}
primary <- summarize_version(submitted_choices, "submitted")
sensitivity <- summarize_version(all_choices, "including_automatic_continuity")

fmt <- function(n, p) paste0(format(n, big.mark = ",", trim = TRUE), " (", sprintf("%.2f", p), "%)")
cs <- primary$choices; aps <- primary$applicants
choice_rows <- vapply(c("home", "outside"), function(k) paste0('<tr><th>',
  if (k == "home") "Home region" else "Outside home region", '</th>',
  paste(vapply(1:2, function(i) paste0('<td>', fmt(cs[[paste0("n_", k, "_choices")]][i],
    cs[[paste0("pct_", k, "_choices")]][i]), '</td>'), character(1)), collapse = ""), '</tr>'), character(1))
profile_rows <- vapply(c("n_home_only", "n_both", "n_outside_only"), function(k) {
  label <- switch(k, n_home_only = "Home region only", n_both = "Both home and outside", n_outside_only = "Outside home region only")
  paste0('<tr><th>', label, '</th>', paste(vapply(1:2, function(i) paste0('<td>',
    fmt(aps[[k]][i], 100 * aps[[k]][i] / aps$n_fully_classified[i]), '</td>'), character(1)), collapse = ""), '</tr>')
}, character(1))
header <- '<thead><tr><th></th><th>SAE applicants</th><th>At risk</th></tr></thead>'
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>SAE applications: home versus outside region</title>',
 '<style>body{font:16px system-ui,sans-serif;max-width:1050px;margin:35px auto;padding:0 20px;color:#172b40}h1{font-size:26px}h2{font-size:21px;margin-top:28px}table{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums}th,td{padding:11px;text-align:right}th:first-child{text-align:left}thead{background:#e6ecf2}tbody tr:nth-child(even){background:#f1f4f7}.notes{font-size:14px;line-height:1.6;color:#475569}</style></head><body>',
 '<h1>SAE applications: home versus outside home region</h1><p>Timely Grade 9 SAE applicants, 2018-2020, and their at-risk subset.</p>',
 '<h2>School choices on submitted application lists</h2><table>', header, '<tbody>', paste(choice_rows, collapse = ""), '</tbody></table>',
 sprintf('<p>Known choice geography: %s of %s SAE school choices; %s of %s at-risk school choices.</p>',
   format(cs$n_known_school_choices[1], big.mark = ",", trim = TRUE), format(cs$n_all_school_choices[1], big.mark = ",", trim = TRUE),
   format(cs$n_known_school_choices[2], big.mark = ",", trim = TRUE), format(cs$n_all_school_choices[2], big.mark = ",", trim = TRUE)),
 '<h2>Applicants by composition of their lists</h2><table>', header, '<tbody>', paste(profile_rows, collapse = ""), '</tbody></table>',
 sprintf('<p>Fully classified lists: %s of %s SAE applicants; %s of %s at-risk applicants.</p>',
   format(aps$n_fully_classified[1], big.mark = ",", trim = TRUE), format(aps$n_applicants[1], big.mark = ",", trim = TRUE),
   format(aps$n_fully_classified[2], big.mark = ",", trim = TRUE), format(aps$n_applicants[2], big.mark = ",", trim = TRUE)),
 '<div class="notes"><p>Primary applications are actual C1 regular-round choices with agregada_por_continuidad=0; sibling and other priority choices remain included. Each student-school RBD pair counts once per application year, collapsing multiple programs at the same school. Applicant percentages use students whose entire submitted school list has observed home and school regions.</p>',
 '<p>Home is the application-year SAE B1 home-coordinate region, from the earlier geography construction. School regions are derived from same-year A1 coordinates, using the same current 16-region polygons, including Nuble. Only unique polygon containment is accepted; no nearest-region or Grade 8 fallback is used. Unknown geography and students with no submitted choices are reported separately in the CSVs. A separate sensitivity includes automatically added continuity schools. No PAES-taking or baseline-score-completeness restriction is added.</p></div></body></html>')
writeLines(html, file.path(tab, "sae_applications_home_vs_outside_region.html"), useBytes = TRUE)
print(primary$choices)
print(primary$applicants)
print(sensitivity$choices)
