# Regenerate both paper-facing observational VA samples.
script <- "code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R"

run_window <- function(min_year, max_year) {
  Sys.setenv(
    SCHOOL_VA_COHORT_MIN = as.character(min_year),
    SCHOOL_VA_COHORT_MAX = as.character(max_year)
  )
  message("Running observational VA for grade-8 cohorts ", min_year, "-", max_year)
  sys.source(script, envir = new.env(parent = globalenv()))
}

run_window(2017L, 2020L)
run_window(2017L, 2018L)
