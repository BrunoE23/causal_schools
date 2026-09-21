library(data.table)
library(haven)

clean_dir <- "C:/Users/brunem/Box/causal_schools/data/clean"

sample_env <- new.env()
load(file.path(clean_dir, "samples.RData"), envir = sample_env)
sample <- unique(as.data.table(sample_env$sample_students)[
  , .(mrun = as.integer(mrun), cohort_gr8 = as.integer(proceso) - 1L)
])

postulaciones <- unique(as.data.table(read_dta(
  file.path(clean_dir, "postulaciones.dta"),
  col_select = c("mrun", "anio_proceso")
))[, .(mrun = as.integer(mrun), year = as.integer(anio_proceso))])

asignaciones <- unique(as.data.table(read_dta(
  file.path(clean_dir, "asignaciones.dta"),
  col_select = c("mrun", "anio_beneficio")
))[, .(mrun = as.integer(mrun), year = as.integer(anio_beneficio))])

cohorts <- sample[, .(students = .N), by = cohort_gr8][order(cohort_gr8)]

year_coverage <- function(keys, dataset_name) {
  grid <- CJ(cohort_gr8 = cohorts$cohort_gr8, year = sort(unique(keys$year)))
  matched <- merge(sample, keys, by = "mrun", allow.cartesian = TRUE)[
    , .(matched_students = uniqueN(mrun)), by = .(cohort_gr8, year)
  ]
  output <- merge(grid, matched, by = c("cohort_gr8", "year"), all.x = TRUE)
  output <- merge(output, cohorts, by = "cohort_gr8")
  output[is.na(matched_students), matched_students := 0L]
  output[, share := matched_students / students]
  output[, dataset := dataset_name]
  output[, .(dataset, cohort_gr8, year, students, matched_students, share)]
}

any_coverage <- function(keys, dataset_name) {
  matched <- merge(sample, unique(keys[, .(mrun)]), by = "mrun")[
    , .(matched_students = uniqueN(mrun)), by = cohort_gr8
  ]
  output <- merge(cohorts, matched, by = "cohort_gr8", all.x = TRUE)
  output[is.na(matched_students), matched_students := 0L]
  output[, share := matched_students / students]
  output[, dataset := dataset_name]
  output[, .(dataset, cohort_gr8, students, matched_students, share)]
}

year_results <- rbindlist(list(
  year_coverage(postulaciones, "postulaciones"),
  year_coverage(asignaciones, "asignaciones")
))[order(dataset, cohort_gr8, year)]

any_results <- rbindlist(list(
  any_coverage(postulaciones, "postulaciones_any_available_years"),
  any_coverage(asignaciones, "asignaciones_any_available_years"),
  any_coverage(
    unique(rbindlist(list(postulaciones[, .(mrun)], asignaciones[, .(mrun)]))),
    "either_database_any_available_years"
  )
))[order(dataset, cohort_gr8)]

print(year_results)
print(any_results)
cat("Students observed in more than one grade-8 cohort: ", sample[, .N, by = mrun][N > 1, .N], "\n", sep = "")
