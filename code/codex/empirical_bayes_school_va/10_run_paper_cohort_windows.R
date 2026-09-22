# Apply empirical-Bayes shrinkage to both paper-facing observational VA runs.
script <- "code/codex/empirical_bayes_school_va/01_construct_eb_school_values.R"

run_window <- function(min_year, max_year) {
  data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
  cohort_tag <- paste0("cohorts_", min_year, "_", max_year)
  values_dir <- file.path(data_wd, "data/clean/school_rbd_observational_values", cohort_tag)
  output_dir <- file.path(data_wd, "data/clean/empirical_bayes_school_va", cohort_tag)
  Sys.setenv(
    SCHOOL_RBD_VALUES_INPUT_PATH = file.path(values_dir, "school_rbd_observational_values.csv"),
    EB_SCHOOL_VALUES_OUTPUT_PATH = file.path(output_dir, "eb_school_rbd_observational_values.csv"),
    EB_SCHOOL_DIAGNOSTICS_OUTPUT_PATH = file.path(output_dir, "eb_school_rbd_observational_values_diagnostics.csv")
  )
  message("Running EB shrinkage for grade-8 cohorts ", min_year, "-", max_year)
  sys.source(script, envir = new.env(parent = globalenv()))
}

run_window(2017L, 2020L)
run_window(2017L, 2018L)
