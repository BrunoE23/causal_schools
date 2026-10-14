# Declared kinder capacity: existing-school changes versus new SAE offerings.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/")
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
clean <- file.path(root, "data/clean/kinder_capacity")
reports <- file.path(root, "output/reports/kinder_capacity")
for (p in c(clean, reports)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
manifest <- list()

one_csv <- function(folder, pattern = "[.]csv$") {
  p <- list.files(folder, pattern = pattern, full.names = TRUE)
  stopifnot(length(p) == 1L)
  p
}

# Small annual directories; no pupil-level data are required.
directory <- rbindlist(lapply(2015:2025, function(year) {
  path <- one_csv(file.path(data_root, "data/raw/school_directory", year))
  manifest[[length(manifest) + 1L]] <<- data.table(source = "directory", year, path, bytes = file.info(path)$size)
  h <- names(fread(path, nrows = 0L))
  wanted <- c("RBD", "NOM_RBD", "COD_REG_RBD", "COD_COM_RBD", "NOM_COM_RBD", "COD_DEPE",
    "ESTADO_ESTAB", "MATRICULA", grep("^ENS_", toupper(h), value = TRUE))
  selected <- h[match(wanted, toupper(h))]
  stopifnot(!anyNA(selected))
  x <- fread(path, select = selected, showProgress = FALSE)
  setnames(x, toupper(names(x)))
  stopifnot(!anyDuplicated(x$RBD))
  teaching_columns <- grep("^ENS_", names(x), value = TRUE)
  x[, preschool_enrolled := rowSums(.SD == 10L, na.rm = TRUE) > 0L, .SDcols = teaching_columns]
  x[, .(rbd = RBD, directory_year = year, school_name = NOM_RBD, region = COD_REG_RBD,
    comuna_code = COD_COM_RBD, comuna = NOM_COM_RBD, dependency = COD_DEPE,
    operating = ESTADO_ESTAB == 1L & MATRICULA == 1L,
    school_state = ESTADO_ESTAB, preschool_enrolled)]
}))

# Only use directories through the entry year when describing opening histories.
# Later records supply names/location for comparability, never opening status.
histories <- directory[, .(
  first_record_year = min(directory_year),
  first_operating_year = if (any(operating)) min(directory_year[operating]) else NA_integer_,
  first_preschool_enrolled_year = if (any(preschool_enrolled)) min(directory_year[preschool_enrolled]) else NA_integer_
), by = rbd]
metadata <- copy(directory)
setorder(metadata, rbd, -directory_year)
metadata <- unique(metadata, by = "rbd")[, .(rbd, school_name, region, comuna_code, comuna, dependency)]
# Nuble was split from Biobio in 2018: normalize to the earlier region boundary.
metadata[, coverage_region := fifelse(region == 16L, 8L, region)]

program_audit <- list()
all_sae <- list()
kinder <- rbindlist(lapply(2017:2021, function(year) {
  path <- one_csv(file.path(data_root, "data/raw", year, paste0("SAE_", year)), "^A1_.*[.]csv$")
  manifest[[length(manifest) + 1L]] <<- data.table(source = "SAE A1 regular offerings", year, path, bytes = file.info(path)$size)
  x <- fread(path, sep = ";", select = c("rbd", "cod_nivel", "cod_ense", "cod_grado",
    "cod_curso", "cupos_totales", "vacantes"), colClasses = c(cod_curso = "character"))
  all_sae[[as.character(year)]] <<- unique(x[, .(rbd, process_year = year)])
  x <- x[cod_nivel == 0L]
  stopifnot(all(x$cod_ense == 10L), all(x$cod_grado == 5L), !anyNA(x$cupos_totales),
    !anyNA(x$vacantes), all(x$cupos_totales >= 0), all(x$vacantes >= 0),
    all(x$vacantes <= x$cupos_totales), !anyDuplicated(x[, .(rbd, cod_curso)]))
  program_audit[[as.character(year)]] <<- data.table(process_year = year,
    program_rows = nrow(x), schools_listed = uniqueN(x$rbd), zero_capacity_programs = sum(x$cupos_totales == 0L),
    total_capacity = sum(x$cupos_totales), vacancies = sum(x$vacantes))
  x[, .(capacity = sum(cupos_totales), vacancies = sum(vacantes), programs = .N), by = rbd][,
    `:=`(process_year = year, entry_year = year + 1L)]
}))
all_sae <- rbindlist(all_sae)
kinder <- merge(kinder, metadata, by = "rbd", all.x = TRUE)
kinder <- merge(kinder, histories, by = "rbd", all.x = TRUE)
stopifnot(!anyNA(kinder$coverage_region), !anyDuplicated(kinder[, .(rbd, process_year)]))

annual <- kinder[, .(schools_with_positive_capacity = sum(capacity > 0L),
  declared_capacity = sum(capacity), available_vacancies = sum(vacancies),
  vacancy_share = sum(vacancies) / sum(capacity)), by = .(process_year, entry_year)]
setorder(annual, process_year)

transition_rows <- list()
decompositions <- list()
for (year in 2018:2021) {
  prev <- kinder[process_year == year - 1L]
  curr <- kinder[process_year == year]
  common_regions <- intersect(prev$coverage_region, curr$coverage_region)
  prev <- prev[coverage_region %in% common_regions]
  curr <- curr[coverage_region %in% common_regions]
  scope <- if (year == 2018L) "2017 rollout regions" else if (year == 2019L) "Outside Metropolitan Region" else "National"
  z <- merge(prev[, .(rbd, capacity_before = capacity, vacancies_before = vacancies)],
    curr[, .(rbd, capacity_after = capacity, vacancies_after = vacancies)], by = "rbd", all = TRUE)
  for (v in c("capacity_before", "capacity_after", "vacancies_before", "vacancies_after")) {
    set(z, which(is.na(z[[v]])), v, 0)
  }
  z <- z[capacity_before > 0 | capacity_after > 0]
  z <- merge(z, metadata, by = "rbd", all.x = TRUE)
  z <- merge(z, histories, by = "rbd", all.x = TRUE)
  z[, `:=`(process_before = year - 1L, process_after = year, entry_before = year,
    entry_after = year + 1L, geography = scope, change_capacity = capacity_after - capacity_before,
    change_vacancies = vacancies_after - vacancies_before)]
  z[, margin := fcase(capacity_before > 0 & capacity_after > 0, "Continuing kinder offering",
    capacity_before == 0 & capacity_after > 0, "New SAE kinder offering",
    default = "Kinder offering no longer listed with positive capacity")]
  z[, entrant_type := NA_character_]
  # Annual directories do not establish legal founding dates. Candidate openings
  # are first observed operating in the current process year or its entry year,
  # after no operating observation in the directory history since 2015.
  z[margin == "New SAE kinder offering", entrant_type := fcase(
    !is.na(first_operating_year) & first_operating_year <= year - 1L,
      "Previously operating school newly listing kinder",
    !is.na(first_operating_year) & first_operating_year %in% c(year, year + 1L),
      "First observed operating school (candidate opening)",
    default = "Opening status unresolved")]
  z[, new_record_in_opening_window := !is.na(first_record_year) & first_record_year %in% c(year, year + 1L)]
  z[, preschool_enrollment_before_process := !is.na(first_preschool_enrolled_year) & first_preschool_enrolled_year <= year - 1L]
  z[, previously_listed_any_SAE_level := rbd %in% all_sae[process_year < year, rbd]]
  continuing <- z[margin == "Continuing kinder offering"]
  entrants <- z[margin == "New SAE kinder offering"]
  exits <- z[margin == "Kinder offering no longer listed with positive capacity"]
  ans <- data.table(process_before = year - 1L, process_after = year, entry_before = year,
    entry_after = year + 1L, geography = scope,
    schools_before = sum(z$capacity_before > 0), schools_after = sum(z$capacity_after > 0),
    capacity_before = sum(z$capacity_before), capacity_after = sum(z$capacity_after),
    vacancies_before = sum(z$vacancies_before), vacancies_after = sum(z$vacancies_after),
    continuing_schools = nrow(continuing),
    expanding_schools = continuing[change_capacity > 0, .N],
    contracting_schools = continuing[change_capacity < 0, .N],
    unchanged_schools = continuing[change_capacity == 0, .N],
    seats_added_by_continuing = continuing[change_capacity > 0, sum(change_capacity)],
    seats_removed_by_continuing = continuing[change_capacity < 0, sum(-change_capacity)],
    new_kinder_offerings = nrow(entrants), seats_in_new_kinder_offerings = sum(entrants$capacity_after),
    candidate_newly_operating_schools = entrants[entrant_type == "First observed operating school (candidate opening)", .N],
    seats_candidate_newly_operating_schools = entrants[entrant_type == "First observed operating school (candidate opening)", sum(capacity_after)],
    existing_schools_new_kinder_listing = entrants[entrant_type == "Previously operating school newly listing kinder", .N],
    seats_existing_schools_new_kinder_listing = entrants[entrant_type == "Previously operating school newly listing kinder", sum(capacity_after)],
    unresolved_entrants = entrants[entrant_type == "Opening status unresolved", .N],
    seats_unresolved_entrants = entrants[entrant_type == "Opening status unresolved", sum(capacity_after)],
    existing_entrants_with_prior_preschool = entrants[entrant_type == "Previously operating school newly listing kinder" & preschool_enrollment_before_process, .N],
    exiting_offerings = nrow(exits), seats_lost_from_exiting = sum(exits$capacity_before))
  ans[, `:=`(gross_seats_added = seats_added_by_continuing + seats_in_new_kinder_offerings,
    gross_seats_removed = seats_removed_by_continuing + seats_lost_from_exiting,
    net_capacity_change = capacity_after - capacity_before,
    net_vacancy_change = vacancies_after - vacancies_before)]
  ans[, accounting_residual := net_capacity_change - (gross_seats_added - gross_seats_removed)]
  stopifnot(ans$accounting_residual == 0,
    ans$new_kinder_offerings == ans$candidate_newly_operating_schools + ans$existing_schools_new_kinder_listing + ans$unresolved_entrants,
    ans$seats_in_new_kinder_offerings == ans$seats_candidate_newly_operating_schools + ans$seats_existing_schools_new_kinder_listing + ans$seats_unresolved_entrants,
    ans$schools_after == ans$schools_before + ans$new_kinder_offerings - ans$exiting_offerings)
  transition_rows[[as.character(year)]] <- z
  decompositions[[as.character(year)]] <- ans
}
transition <- rbindlist(transition_rows)
decomposition <- rbindlist(decompositions)
fwrite(kinder, file.path(clean, "school_year_kinder_capacity.csv"))
fwrite(annual, file.path(clean, "annual_capacity_and_vacancies.csv"))
fwrite(transition, file.path(clean, "school_capacity_transitions.csv"))
fwrite(decomposition, file.path(clean, "capacity_change_accounting.csv"))
fwrite(rbindlist(program_audit), file.path(clean, "program_uniqueness_and_capacity_audit.csv"))
fwrite(rbindlist(manifest), file.path(clean, "input_manifest.csv"))
saveRDS(directory, file.path(clean, "directory_history.rds"))
writeLines(capture.output(sessionInfo()), file.path(clean, "session_info.txt"))

source(file.path(root, "code/codex/kinder_capacity/02_report_capacity.R"))
print(annual)
print(decomposition)
