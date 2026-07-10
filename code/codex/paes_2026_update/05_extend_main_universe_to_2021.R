###############################################################################
# Extend the main grade-8 universe to cohort 2021
#
# This rebuilds the canonical clean inputs needed by universe_reg_df.R:
# - data/clean/universe.csv
# - data/clean/universe_controls.csv
# - data/clean/tracking_univ8gr.RData
# - data/clean/rbd_universe.csv
# - data/clean/sae_grade9_unique_proceso.RData
# - data/clean/sae_grade9_students_frame.RData
# - data/clean/sae_binary_prep.RData
# - data/clean/treatment_1R_v2.RData
# - data/clean/offers_1R_p_proceso.RData
#
# It uses the same four-year post-grade-8 attended-school window for every
# cohort. For cohort 2021 that requires raw student_tracking/2025.
#
# Important: this script does not treat PAES Matricula 2026 as full SIES
# Matricula-Ed-Superior 2026. It only prepares the school/SAE universe inputs.
###############################################################################

find_existing_path <- function(candidates, label) {
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0L) {
    stop("Could not find ", label, ". Update candidates.", call. = FALSE)
  }

  candidates[[1L]]
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
diagnostic_dir <- file.path(repo_wd, "output", "tables", "paes_2026_update")
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

cohort_years <- 2017:2021
tracking_years <- 2017:2025
sae_years <- 2017:2021

tracking_cols <- c(
  "AGNO",
  "RBD", "NOM_RBD", "COD_REG_RBD", "COD_PRO_RBD", "COD_COM_RBD",
  "NOM_COM_RBD", "COD_DEPE", "COD_DEPE2", "RURAL_RBD",
  "COD_ENSE", "COD_ENSE2", "COD_GRADO", "LET_CUR", "COD_JOR",
  "MRUN", "GEN_ALU", "FEC_NAC_ALU", "EDAD_ALU",
  "COD_COM_ALU", "NOM_COM_ALU", "COD_REG_ALU",
  "PROM_GRAL", "ASISTENCIA", "SIT_FIN", "SIT_FIN_R"
)

single_csv_in_folder <- function(folder, label) {
  files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0L) {
    stop("No CSV files found for ", label, " in ", folder, call. = FALSE)
  }
  if (length(files) > 1L) {
    stop(
      "Multiple CSV files found for ", label, " in ", folder, ":\n",
      paste(files, collapse = "\n"),
      call. = FALSE
    )
  }
  files[[1L]]
}

tracking_path <- function(year) {
  folder <- file.path(
    data_wd,
    "data", "raw", "student_tracking", as.character(year),
    paste0("Rendimiento-", year)
  )
  single_csv_in_folder(folder, paste0("student_tracking ", year))
}

read_tracking_year <- function(year, keep_mrun = NULL) {
  path <- tracking_path(year)
  header <- names(fread(path, nrows = 0, encoding = "UTF-8"))
  header_lookup <- setNames(header, toupper(header))
  keep <- unname(header_lookup[intersect(tracking_cols, names(header_lookup))])

  dt <- fread(
    path,
    select = keep,
    encoding = "UTF-8",
    showProgress = TRUE
  )
  setnames(dt, toupper(names(dt)))

  dt <- dt[!is.na(SIT_FIN)]
  dt[, MRUN := as.integer(MRUN)]
  dt[, AGNO := as.integer(AGNO)]
  dt[, RBD := as.integer(RBD)]
  dt[, COD_GRADO := as.integer(COD_GRADO)]
  dt[, COD_ENSE := as.integer(COD_ENSE)]

  if (!is.null(keep_mrun)) {
    dt <- dt[MRUN %in% keep_mrun]
  }

  dt[
    ,
    `:=`(
      school_grade_avg_GPA = mean(PROM_GRAL[PROM_GRAL != 0], na.rm = TRUE),
      school_grade_sd_GPA = sd(PROM_GRAL[PROM_GRAL != 0], na.rm = TRUE),
      school_grade_avg_ATT = mean(ASISTENCIA[ASISTENCIA != 0], na.rm = TRUE),
      school_grade_sd_ATT = sd(ASISTENCIA[ASISTENCIA != 0], na.rm = TRUE)
    ),
    by = .(RBD, COD_GRADO)
  ]

  dt[
    ,
    pctl_school_grade := {
      if (.N == 1L) {
        50
      } else {
        round(100 * (frank(PROM_GRAL, ties.method = "min", na.last = "keep") - 1) / (.N - 1), 1)
      }
    },
    by = .(RBD, COD_GRADO)
  ]

  dt[]
}

resolve_student_year_rows <- function(dt, year_var = "AGNO") {
  year_col <- year_var
  dt <- copy(dt)

  dt[, nonzero_attendance := fifelse(!is.na(ASISTENCIA) & ASISTENCIA > 0, 1L, 0L)]
  dt[, passed_record := fifelse(SIT_FIN == "P", 1L, 0L)]
  dt[, attendance_for_sort := fifelse(is.na(ASISTENCIA), -Inf, as.numeric(ASISTENCIA))]

  setorderv(
    dt,
    c("MRUN", year_col, "nonzero_attendance", "passed_record", "attendance_for_sort", "RBD"),
    c(1L, 1L, -1L, -1L, -1L, 1L)
  )

  out <- dt[, .SD[1L], by = c("MRUN", year_col)]
  out[, c("nonzero_attendance", "passed_record", "attendance_for_sort") := NULL]
  out[]
}

message("Building grade-8 cohort universe for ", paste(cohort_years, collapse = "-"), ".")
grade8_list <- lapply(cohort_years, function(year) {
  dt <- read_tracking_year(year)
  dt <- dt[COD_GRADO == 8L & COD_ENSE == 110L]
  resolve_student_year_rows(dt)
})
grade8_all <- rbindlist(grade8_list, use.names = TRUE, fill = TRUE)

setorder(grade8_all, MRUN, AGNO)
grade8_first <- grade8_all[, .SD[1L], by = MRUN]
grade8_first[, cohort_gr8 := AGNO]

universe <- grade8_first[, .(MRUN, cohort_gr8)]
universe_controls <- grade8_first[
  ,
  .(
    MRUN,
    cohort_gr8,
    GEN_ALU,
    FEC_NAC_ALU,
    EDAD_ALU,
    COD_COM_ALU,
    NOM_COM_ALU,
    COD_REG_ALU
  )
]

message("Building tracking_univ8gr.RData with tracking years ", paste(tracking_years, collapse = "-"), ".")
universe_mrun <- universe$MRUN
tracking_list <- lapply(tracking_years, function(year) {
  read_tracking_year(year, keep_mrun = universe_mrun)
})
tracking_all <- rbindlist(tracking_list, use.names = TRUE, fill = TRUE)

message("Assigning post-grade-8 attended high school RBDs.")
tracking_window <- merge(
  tracking_all,
  universe,
  by = "MRUN",
  all.x = FALSE,
  all.y = FALSE
)
tracking_window[, year_rel_cohort8 := AGNO - cohort_gr8]
tracking_window <- tracking_window[year_rel_cohort8 > 0L & year_rel_cohort8 <= 4L]
tracking_window_clean <- resolve_student_year_rows(tracking_window, year_var = "year_rel_cohort8")

first_last <- tracking_window_clean[
  ,
  .(
    RBD_rel1 = {
      x <- RBD[year_rel_cohort8 == 1L]
      if (length(x) == 0L) NA_integer_ else x[[1L]]
    },
    RBD_rel4 = {
      x <- RBD[year_rel_cohort8 == 4L]
      if (length(x) == 0L) NA_integer_ else x[[1L]]
    }
  ),
  by = MRUN
]

most_time <- tracking_window_clean[
  ,
  .(
    n_years = .N,
    last_year = max(year_rel_cohort8, na.rm = TRUE)
  ),
  by = .(MRUN, RBD)
]
setorder(most_time, MRUN, -n_years, -last_year, RBD)
most_time <- most_time[
  ,
  .(
    most_time_RBD = RBD[[1L]],
    post_gr8_years_observed = n_years[[1L]],
    post_gr8_latest_year_rel = last_year[[1L]]
  ),
  by = MRUN
]

rbd_attended <- merge(first_last, most_time, by = "MRUN", all = TRUE)
rbd_attended[, first_last_same := RBD_rel1 == RBD_rel4]

read_sae_regular_apps <- function(year) {
  folder <- file.path(data_wd, "data", "raw", as.character(year), paste0("SAE_", year))
  path <- list.files(
    folder,
    pattern = paste0("^C1_Postulaciones_etapa_regular_", year, ".*\\.csv$"),
    full.names = TRUE
  )
  if (length(path) != 1L) {
    stop("Could not uniquely find regular C1 applications for SAE ", year, call. = FALSE)
  }

  dt <- fread(path, encoding = "UTF-8", showProgress = TRUE)
  setnames(dt, tolower(names(dt)))
  dt <- dt[cod_nivel == 9L]
  dt[, sae_proceso := as.integer(year)]
  dt[, br_code := paste0(as.character(rbd), "_", cod_curso, "_", sae_proceso)]
  dt[
    ,
    .(
      mrun = as.integer(mrun),
      sae_proceso,
      rbd = as.integer(rbd),
      br_code,
      preferencia_postulante = as.integer(preferencia_postulante)
    )
  ]
}

read_sae_regular_results <- function(year) {
  folder <- file.path(data_wd, "data", "raw", as.character(year), paste0("SAE_", year))
  path <- list.files(
    folder,
    pattern = paste0("^D1_Resultados_etapa_regular_", year, ".*\\.csv$"),
    full.names = TRUE
  )
  if (length(path) != 1L) {
    stop("Could not uniquely find regular D1 results for SAE ", year, call. = FALSE)
  }

  dt <- fread(path, encoding = "UTF-8", showProgress = TRUE)
  setnames(dt, tolower(names(dt)))
  dt <- dt[cod_nivel == 9L]
  dt[, sae_proceso := as.integer(year)]
  dt[, rbd_admitido := suppressWarnings(as.integer(rbd_admitido))]
  dt[, rbd_admitido_post_resp := suppressWarnings(as.integer(rbd_admitido_post_resp))]
  dt[
    ,
    .(
      mrun = as.integer(mrun),
      sae_proceso,
      rbd_admitido,
      rbd_admitido_post_resp
    )
  ]
}

message("Building SAE grade-9 application and first-offer objects through 2021.")
sae_apps_all <- rbindlist(
  lapply(sae_years, read_sae_regular_apps),
  use.names = TRUE,
  fill = TRUE
)
first_sae_process <- sae_apps_all[, .(sae_proceso = min(sae_proceso)), by = mrun]
sae_apps_grade9 <- sae_apps_all[
  first_sae_process,
  on = .(mrun, sae_proceso),
  nomatch = 0L
]
setorder(sae_apps_grade9, mrun, sae_proceso, preferencia_postulante, rbd, br_code)

students_sae_grade9_frame <- unique(sae_apps_grade9[, .(mrun, sae_proceso)])
setorder(students_sae_grade9_frame, mrun, sae_proceso)

sae_results_all <- rbindlist(
  lapply(sae_years, read_sae_regular_results),
  use.names = TRUE,
  fill = TRUE
)
setorder(sae_results_all, mrun, sae_proceso)
sae_results_all <- unique(sae_results_all, by = c("mrun", "sae_proceso"))

all_treatments <- merge(
  sae_apps_grade9,
  sae_results_all,
  by = c("mrun", "sae_proceso"),
  all.x = TRUE,
  all.y = FALSE
)
all_treatments[
  ,
  `:=`(
    offered_spot_1R = fifelse(!is.na(rbd_admitido) & rbd == rbd_admitido, 1L, 0L),
    offered_spot_2R = fifelse(
      !is.na(rbd_admitido_post_resp) & rbd == rbd_admitido_post_resp,
      1L,
      0L
    )
  )
]
all_treatments[, offered_spot_anyR := pmax(offered_spot_1R, offered_spot_2R)]
all_treatments[
  ,
  `:=`(
    rbd_treated_1R = fifelse(offered_spot_1R == 1L, rbd, 0L),
    rbd_treated_2R = fifelse(offered_spot_2R == 1L, rbd, 0L),
    rbd_treated = fifelse(offered_spot_anyR == 1L, rbd, 0L)
  )
]

setorder(all_treatments, mrun, sae_proceso, preferencia_postulante, rbd)
offers_1R_proceso <- all_treatments[
  ,
  {
    offered_rbd <- rbd_treated_1R[!is.na(rbd_treated_1R) & rbd_treated_1R != 0L]
    .(
      offered_1R = as.integer(length(offered_rbd) > 0L),
      rbd_treated_1R = if (length(offered_rbd) == 0L) 0L else offered_rbd[[1L]],
      n_application_rows = .N,
      n_distinct_offered_rbd_1R = uniqueN(offered_rbd)
    )
  },
  by = .(mrun, sae_proceso)
]
setorder(offers_1R_proceso, mrun, sae_proceso)

message("Writing clean universe and SAE objects.")
write.csv(as.data.frame(universe), file.path(clean_dir, "universe.csv"))
write.csv(as.data.frame(universe_controls), file.path(clean_dir, "universe_controls.csv"))
save(tracking_all, file = file.path(clean_dir, "tracking_univ8gr.RData"))
write.csv(as.data.frame(rbd_attended), file.path(clean_dir, "rbd_universe.csv"))

save(sae_apps_grade9, file = file.path(clean_dir, "sae_grade9_unique_proceso.RData"))
save(students_sae_grade9_frame, file = file.path(clean_dir, "sae_grade9_students_frame.RData"))

sae_apps_grade9_binary <- copy(students_sae_grade9_frame)
sae_apps_grade9 <- sae_apps_grade9_binary
save(sae_apps_grade9, file = file.path(clean_dir, "sae_binary_prep.RData"))
rm(sae_apps_grade9)

save(all_treatments, file = file.path(clean_dir, "treatment_1R_v2.RData"))
save(offers_1R_proceso, file = file.path(clean_dir, "offers_1R_p_proceso.RData"))

cohort_counts <- universe[
  ,
  .(n_students = .N),
  by = cohort_gr8
][order(cohort_gr8)]

tracking_coverage <- merge(
  tracking_window_clean[
    ,
    .(
      n_students_with_tracking = uniqueN(MRUN),
      n_student_year_rows = .N
    ),
    by = .(cohort_gr8, year_rel_cohort8)
  ],
  cohort_counts,
  by = "cohort_gr8",
  all.x = TRUE
)
tracking_coverage[, share_students_with_tracking := n_students_with_tracking / n_students]
setorder(tracking_coverage, cohort_gr8, year_rel_cohort8)

rbd_coverage <- merge(
  universe,
  rbd_attended,
  by = "MRUN",
  all.x = TRUE
)[
  ,
  .(
    n_students = .N,
    n_with_RBD_rel1 = sum(!is.na(RBD_rel1)),
    n_with_RBD_rel4 = sum(!is.na(RBD_rel4)),
    n_with_most_time_RBD = sum(!is.na(most_time_RBD)),
    share_with_RBD_rel1 = mean(!is.na(RBD_rel1)),
    share_with_RBD_rel4 = mean(!is.na(RBD_rel4)),
    share_with_most_time_RBD = mean(!is.na(most_time_RBD))
  ),
  by = cohort_gr8
][order(cohort_gr8)]

sae_counts <- students_sae_grade9_frame[
  ,
  .(n_sae_students = uniqueN(mrun)),
  by = sae_proceso
][order(sae_proceso)]

offer_counts <- offers_1R_proceso[
  ,
  .(
    n_offer_students = uniqueN(mrun),
    n_offered_1R = sum(offered_1R == 1L, na.rm = TRUE),
    n_multi_offered_rbd_1R = sum(n_distinct_offered_rbd_1R > 1L, na.rm = TRUE)
  ),
  by = sae_proceso
][order(sae_proceso)]

fwrite(cohort_counts, file.path(diagnostic_dir, "grade8_2017_2021_universe_counts.csv"))
fwrite(tracking_coverage, file.path(diagnostic_dir, "grade8_2017_2021_tracking_coverage.csv"))
fwrite(rbd_coverage, file.path(diagnostic_dir, "grade8_2017_2021_rbd_coverage.csv"))
fwrite(sae_counts, file.path(diagnostic_dir, "grade8_2017_2021_sae_counts.csv"))
fwrite(offer_counts, file.path(diagnostic_dir, "grade8_2017_2021_offer_counts.csv"))

print(cohort_counts)
print(rbd_coverage)
print(sae_counts)
print(offer_counts)
