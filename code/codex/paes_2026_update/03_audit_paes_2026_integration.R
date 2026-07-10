###############################################################################
# Audit PAES 2026 integration
#
# This script verifies which clean products already include PAES 2026 and where
# the current integration still stops. It is intentionally diagnostic only: it
# reads Dropbox/raw-clean data and writes repo-side audit tables.
###############################################################################

find_existing_path <- function(candidates, label) {
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Update candidates.", call. = FALSE)
  }

  candidates[[1]]
}

repo_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools",
    "C:/Users/brunem/Research/causal_schools"
  ),
  "repo_wd"
)
data_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Dropbox/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools"
  ),
  "data_wd"
)

suppressPackageStartupMessages({
  library(data.table)
})

clean_dir <- file.path(data_wd, "data", "clean")
raw_2026_dir <- file.path(data_wd, "data", "raw", "2026")
out_dir <- file.path(repo_wd, "output", "tables", "paes_2026_update")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

metric_rows <- list()
add_metric <- function(section, metric, value, note = NA_character_) {
  metric_rows[[length(metric_rows) + 1L]] <<- data.table(
    section = section,
    metric = metric,
    value = as.character(value),
    note = note
  )
}

count_by_year <- function(dt, year_col, label) {
  if (!year_col %in% names(dt)) {
    add_metric(label, paste0(year_col, "_column_present"), FALSE)
    return(data.table())
  }
  out <- dt[, .(rows = .N, unique_mrun = uniqueN(mrun, na.rm = TRUE)), by = year_col]
  setnames(out, year_col, "year")
  out[, source := label]
  out[]
}

raw_files <- data.table(
  label = c(
    "scores",
    "applications",
    "matricula_paes",
    "oferta",
    "socioeconomics"
  ),
  path = file.path(
    raw_2026_dir,
    c(
      "PAES-2026-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PAES_2026_PUB_MRUN.csv",
      "PAES-2026-Postulantes/C_POSTULANTES_SELECCION_PAES_2026_PUB_MRUN.csv",
      "PAES-2026-Matricula/D_MATRICULA_PAES_2026_2026_PUB_MRUN.csv",
      "PAES-2026-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2026.csv",
      "PAES-2026-Socioeconomicos/B_SOCIOECONOMICO_DOMICILIO_PAES_2026_PUB_MRUN.csv"
    )
  )
)
raw_files[, exists := file.exists(path)]
raw_files[, size_bytes := fifelse(exists, file.info(path)$size, NA_real_)]
fwrite(raw_files, file.path(out_dir, "paes_2026_raw_file_audit.csv"))
for (i in seq_len(nrow(raw_files))) {
  add_metric("raw_2026", paste0(raw_files$label[i], "_exists"), raw_files$exists[i], raw_files$path[i])
}

student_counts <- data.table()
students_path <- file.path(clean_dir, "psu_students.RData")
if (file.exists(students_path)) {
  env <- new.env(parent = emptyenv())
  load(students_path, envir = env)
  if (exists("students_apps", envir = env)) {
    students_apps <- as.data.table(get("students_apps", envir = env))
    student_counts <- count_by_year(students_apps, "year", "psu_students")
    add_metric("clean_scores", "has_2026", 2026L %in% student_counts$year)
    add_metric(
      "clean_scores",
      "rows_2026",
      student_counts[year == 2026L, fifelse(.N == 0L, 0L, rows)]
    )
  } else {
    add_metric("clean_scores", "students_apps_object_present", FALSE)
  }
} else {
  add_metric("clean_scores", "psu_students_file_exists", FALSE, students_path)
}

application_counts <- data.table()
applications_path <- file.path(clean_dir, "psu_applications.RData")
if (file.exists(applications_path)) {
  env <- new.env(parent = emptyenv())
  load(applications_path, envir = env)
  if (exists("college_apps", envir = env)) {
    college_apps <- as.data.table(get("college_apps", envir = env))
    application_counts <- count_by_year(college_apps, "year", "psu_applications")
    add_metric("clean_applications", "has_2026", 2026L %in% application_counts$year)
    add_metric(
      "clean_applications",
      "rows_2026",
      application_counts[year == 2026L, fifelse(.N == 0L, 0L, rows)]
    )
  } else {
    add_metric("clean_applications", "college_apps_object_present", FALSE)
  }
} else {
  add_metric("clean_applications", "psu_applications_file_exists", FALSE, applications_path)
}

year_counts <- rbindlist(list(student_counts, application_counts), use.names = TRUE, fill = TRUE)
fwrite(year_counts, file.path(out_dir, "paes_2026_clean_score_application_year_counts.csv"))

universe_path <- file.path(clean_dir, "univ_gr8_df.csv")
universe_cohort <- data.table()
rbd_match_by_cohort <- data.table()
if (file.exists(universe_path)) {
  universe_header <- names(fread(universe_path, nrows = 0))
  universe_select <- intersect(
    c("MRUN", "mrun", "cohort_gr8", "psu_year", "timely_psu", "completed"),
    universe_header
  )
  universe <- fread(universe_path, select = universe_select, showProgress = TRUE)
  if ("cohort_gr8" %in% names(universe)) {
    universe_cohort <- universe[
      ,
      .(
        rows = .N,
        unique_MRUN_original = if ("MRUN" %in% names(universe)) uniqueN(MRUN) else NA_integer_,
        unique_mrun_clean = if ("mrun" %in% names(universe)) uniqueN(mrun) else NA_integer_,
        has_psu = if ("psu_year" %in% names(universe)) sum(!is.na(psu_year)) else NA_integer_,
        psu_2026 = if ("psu_year" %in% names(universe)) sum(psu_year == 2026L, na.rm = TRUE) else NA_integer_,
        timely_psu = if ("timely_psu" %in% names(universe)) sum(timely_psu == 1L, na.rm = TRUE) else NA_integer_,
        completed = if ("completed" %in% names(universe)) sum(completed == 1L, na.rm = TRUE) else NA_integer_
      ),
      by = cohort_gr8
    ][order(cohort_gr8)]
    add_metric("universe", "min_cohort_gr8", min(universe_cohort$cohort_gr8, na.rm = TRUE))
    add_metric("universe", "max_cohort_gr8", max(universe_cohort$cohort_gr8, na.rm = TRUE))
    add_metric("universe", "cohort_2021_present", 2021L %in% universe_cohort$cohort_gr8)
    add_metric("universe", "psu_2026_records", sum(universe_cohort$psu_2026, na.rm = TRUE))
  }

  rbd_path <- file.path(clean_dir, "rbd_universe.csv")
  if (file.exists(rbd_path) && all(c("MRUN", "cohort_gr8") %in% names(universe))) {
    rbd <- fread(rbd_path, select = c("MRUN", "most_time_RBD"))
    rbd[, MRUN := as.character(MRUN)]
    universe[, MRUN := as.character(MRUN)]
    rbd_match <- merge(universe[, .(MRUN, cohort_gr8)], rbd, by = "MRUN", all.x = TRUE)
    rbd_match_by_cohort <- rbd_match[
      ,
      .(
        rows = .N,
        matched_most_time_RBD = sum(!is.na(most_time_RBD)),
        share_matched_most_time_RBD = mean(!is.na(most_time_RBD))
      ),
      by = cohort_gr8
    ][order(cohort_gr8)]
    add_metric(
      "rbd_universe",
      "all_universe_rows_have_most_time_RBD",
      all(rbd_match_by_cohort$share_matched_most_time_RBD == 1)
    )
  }
} else {
  add_metric("universe", "univ_gr8_df_file_exists", FALSE, universe_path)
}
fwrite(universe_cohort, file.path(out_dir, "paes_2026_universe_cohort_audit.csv"))
fwrite(rbd_match_by_cohort, file.path(out_dir, "paes_2026_rbd_match_by_cohort.csv"))

stem_path <- file.path(clean_dir, "stem_outcome.RData")
stem_counts <- data.table()
if (file.exists(stem_path)) {
  env <- new.env(parent = emptyenv())
  load(stem_path, envir = env)
  if (exists("stem_outcome", envir = env)) {
    stem_outcome <- as.data.table(get("stem_outcome", envir = env))
    if ("year_1st_app" %in% names(stem_outcome)) {
      stem_counts <- stem_outcome[, .(rows = .N, unique_mrun = uniqueN(mrun)), by = year_1st_app]
      setnames(stem_counts, "year_1st_app", "year")
      add_metric("stem_outcome", "has_2026_first_app", 2026L %in% stem_counts$year)
      add_metric(
        "stem_outcome",
        "rows_2026_first_app",
        stem_counts[year == 2026L, fifelse(.N == 0L, 0L, rows)]
      )
    } else {
      add_metric("stem_outcome", "year_1st_app_column_present", FALSE)
    }
  } else {
    add_metric("stem_outcome", "stem_outcome_object_present", FALSE)
  }
} else {
  add_metric("stem_outcome", "stem_outcome_file_exists", FALSE, stem_path)
}
fwrite(stem_counts, file.path(out_dir, "paes_2026_stem_outcome_year_counts.csv"))

oferta_24_26_path <- file.path(clean_dir, "oferta_codes_24_26_all.rds")
oferta_counts <- data.table()
if (file.exists(oferta_24_26_path)) {
  oferta <- as.data.table(readRDS(oferta_24_26_path))
  oferta_counts <- oferta[
    ,
    .(
      rows = .N,
      unique_program_codes = uniqueN(COD_CARRERA_PREF),
      unique_cod_sies = uniqueN(COD_SIES, na.rm = TRUE),
      missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
    ),
    by = year_info
  ][order(year_info)]
  fwrite(oferta_counts, file.path(out_dir, "paes_2026_oferta_24_26_year_counts.csv"))
  add_metric("oferta", "top_level_24_26_exists", TRUE, oferta_24_26_path)
  add_metric("oferta", "has_2026", 2026L %in% oferta_counts$year_info)
} else {
  add_metric("oferta", "top_level_24_26_exists", FALSE, oferta_24_26_path)
}

program_info_22_25_path <- file.path(clean_dir, "program_info_22-25.rds")
program_info_22_24_path <- file.path(clean_dir, "program_info_22-24.rds")
if (file.exists(program_info_22_25_path)) {
  program_info_22_25 <- as.data.table(readRDS(program_info_22_25_path))
  program_info_22_25[, COD_SIES := as.character(COD_SIES)]
  program_info_counts <- program_info_22_25[
    ,
    .(
      rows = .N,
      unique_cod_sies = uniqueN(COD_SIES),
      missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
    ),
    by = year_info
  ][order(year_info)]
  fwrite(program_info_counts, file.path(out_dir, "paes_2026_program_info_22_25_year_counts.csv"))
  add_metric("program_info", "program_info_22_25_exists", TRUE, program_info_22_25_path)
  add_metric("program_info", "program_info_22_25_rows", nrow(program_info_22_25))
  add_metric("program_info", "program_info_22_25_unique_cod_sies", uniqueN(program_info_22_25$COD_SIES))

  if (exists("oferta") && nrow(oferta) > 0L) {
    old_cod <- character()
    if (file.exists(program_info_22_24_path)) {
      program_info_22_24 <- as.data.table(readRDS(program_info_22_24_path))
      program_info_22_24[, COD_SIES := as.character(COD_SIES)]
      old_cod <- program_info_22_24[!is.na(COD_SIES) & COD_SIES != "", unique(COD_SIES)]
    }
    new_cod <- program_info_22_25[!is.na(COD_SIES) & COD_SIES != "", unique(COD_SIES)]
    program_info_oferta_coverage <- oferta[
      ,
      .(
        rows = .N,
        rows_with_cod_sies = sum(!is.na(COD_SIES) & COD_SIES != ""),
        matched_program_info_22_24 = sum(COD_SIES %chin% old_cod, na.rm = TRUE),
        matched_program_info_22_25 = sum(COD_SIES %chin% new_cod, na.rm = TRUE)
      ),
      by = year_info
    ][order(year_info)]
    program_info_oferta_coverage[
      ,
      `:=`(
        share_matched_program_info_22_24 = matched_program_info_22_24 / rows,
        share_matched_program_info_22_25 = matched_program_info_22_25 / rows
      )
    ]
    fwrite(
      program_info_oferta_coverage,
      file.path(out_dir, "paes_2026_program_info_oferta_coverage.csv")
    )
    add_metric(
      "program_info",
      "oferta_2026_matched_program_info_22_25",
      program_info_oferta_coverage[year_info == 2026L, matched_program_info_22_25][1]
    )
  }
} else {
  add_metric("program_info", "program_info_22_25_exists", FALSE, program_info_22_25_path)
}

paes_matricula_path <- file.path(clean_dir, "paes_2026_update", "paes_matricula_2025_2026_mapped.rds")
if (file.exists(paes_matricula_path)) {
  paes_mat <- as.data.table(readRDS(paes_matricula_path))
  paes_mat_counts <- paes_mat[
    ,
    .(
      rows = .N,
      unique_mrun = uniqueN(MRUN),
      unique_program_codes = uniqueN(CODIGO_CARRERA),
      missing_cod_sies = sum(is.na(COD_SIES) | COD_SIES == "")
    ),
    by = ANYO_PROCESO
  ][order(ANYO_PROCESO)]
  fwrite(paes_mat_counts, file.path(out_dir, "paes_2026_matricula_paes_year_counts.csv"))
  add_metric("paes_matricula", "mapped_file_exists", TRUE, paes_matricula_path)
  add_metric("paes_matricula", "has_2026", 2026L %in% paes_mat_counts$ANYO_PROCESO)
} else {
  add_metric("paes_matricula", "mapped_file_exists", FALSE, paes_matricula_path)
}

tracking_2025_dir <- file.path(data_wd, "data", "raw", "student_tracking", "2025")
sies_matricula_2026_dir <- file.path(data_wd, "data", "raw", "2026", "Matricula-Ed-Superior-2026")
add_metric("remaining_blockers", "student_tracking_2025_exists", dir.exists(tracking_2025_dir), tracking_2025_dir)
add_metric(
  "remaining_blockers",
  "full_sies_matricula_2026_exists",
  dir.exists(sies_matricula_2026_dir),
  sies_matricula_2026_dir
)
add_metric(
  "remaining_blockers",
  "paes_matricula_is_full_sies_substitute",
  FALSE,
  "PAES matricula is admissions-process matricula, not full SIES Matricula-Ed-Superior coverage."
)

audit <- rbindlist(metric_rows, use.names = TRUE, fill = TRUE)
fwrite(audit, file.path(out_dir, "paes_2026_integration_audit.csv"))

cohort_2021_present <- audit[
  section == "universe" & metric == "cohort_2021_present",
  value
][1] == "TRUE"
tracking_2025_exists <- audit[
  section == "remaining_blockers" & metric == "student_tracking_2025_exists",
  value
][1] == "TRUE"
full_sies_2026_exists <- audit[
  section == "remaining_blockers" & metric == "full_sies_matricula_2026_exists",
  value
][1] == "TRUE"

cohort_scope_note <- if (isTRUE(cohort_2021_present)) {
  "- Grade-8 cohort 2021 is present in the main universe audit."
} else {
  "- Grade-8 cohort 2021 is not in the current main universe audit unless `cohort_2021_present` is TRUE."
}
tracking_scope_note <- if (isTRUE(tracking_2025_exists)) {
  "- Student tracking 2025 is present, so the four-year post-grade-8 `most_time_RBD` window can be reproduced for grade-8 cohort 2021."
} else {
  "- The current raw `student_tracking` folder lacks 2025 tracking in this Dropbox snapshot, so the existing four-year `most_time_RBD` definition cannot yet be reproduced for grade-8 cohort 2021."
}
sies_scope_note <- if (isTRUE(full_sies_2026_exists)) {
  "- Full SIES `Matricula-Ed-Superior-2026` appears to be present; downstream enrollment/program-income scripts should explicitly integrate it before treating 2021 higher-ed outcomes as full SIES outcomes."
} else {
  "- Full SIES `Matricula-Ed-Superior-2026` is still absent; PAES matricula 2026 is admissions-process matricula and is not a replacement for full SIES coverage."
}

markdown_lines <- c(
  "# PAES 2026 Integration Audit",
  "",
  paste0("- Raw 2026 file families present: ", sum(raw_files$exists), " / ", nrow(raw_files), "."),
  paste0(
    "- Clean score rows for 2026: ",
    audit[section == "clean_scores" & metric == "rows_2026", value][1]
  ),
  paste0(
    "- Clean application rows for 2026: ",
    audit[section == "clean_applications" & metric == "rows_2026", value][1]
  ),
  paste0(
    "- Main universe cohort range: ",
    audit[section == "universe" & metric == "min_cohort_gr8", value][1],
    "-",
    audit[section == "universe" & metric == "max_cohort_gr8", value][1],
    "."
  ),
  paste0(
    "- Main universe records with `psu_year == 2026`: ",
    audit[section == "universe" & metric == "psu_2026_records", value][1],
    "."
  ),
  paste0(
    "- `stem_outcome.RData` first-application rows in 2026: ",
    audit[section == "stem_outcome" & metric == "rows_2026_first_app", value][1],
    "."
  ),
  paste0(
    "- `program_info_22-25.rds` rows: ",
    audit[section == "program_info" & metric == "program_info_22_25_rows", value][1],
    "."
  ),
  paste0(
    "- PAES oferta 2026 rows matched to `program_info_22-25.rds`: ",
    audit[section == "program_info" & metric == "oferta_2026_matched_program_info_22_25", value][1],
    "."
  ),
  "",
  "## Remaining Scope Notes",
  "",
  cohort_scope_note,
  tracking_scope_note,
  sies_scope_note,
  "- PAES matricula 2026 has been mapped to oferta/COD_SIES for diagnostics, but it remains a separately labeled PAES-only source."
)
writeLines(markdown_lines, file.path(out_dir, "paes_2026_integration_audit.md"))

print(audit)
