# Run the two paper-facing EB scalar-IV cohort pairings.
script <- "code/codex/empirical_bayes_school_va/02_run_expected_va_scalar_iv_eb.R"

run_pair <- function(va_min, va_max, sae_min, sae_max) {
  data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
  repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
  va_tag <- paste0("cohorts_", va_min, "_", va_max)
  pair_tag <- paste0("va_", va_min, "_", va_max, "__sae_", sae_min, "_", sae_max)
  eb_input <- file.path(
    data_wd, "data/clean/empirical_bayes_school_va", va_tag,
    "eb_school_rbd_observational_values.csv"
  )
  clean_out <- file.path(data_wd, "data/clean/empirical_bayes_school_va", pair_tag)
  table_out <- file.path(repo_wd, "output/tables/empirical_bayes_school_va", pair_tag)
  dir.create(clean_out, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_out, recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(
    EB_SCHOOL_VALUES_INPUT_PATH = eb_input,
    PROGRAM_INCOME_OUTCOMES_PATH = file.path(data_wd, "data/clean/univ_gr8_df.csv"),
    EB_IV_SAE_MIN_YEAR = as.character(sae_min),
    EB_IV_SAE_MAX_YEAR = as.character(sae_max),
    EB_IV_VA_COHORT_LABEL = paste0(va_min, "--", va_max),
    EB_IV_VALUE_SPECS = paste(c(
      "math_adj_eb", "leng_adj_eb", "exam_adj_eb", "highinst_adj_eb",
      "highpay_adj_eb", "program_income_adj_eb", "anypost_adj_eb"
    ), collapse = ","),
    EB_IV_REGRESSION_OUTPUT_PATH = file.path(clean_out, "scalar_iv_regression_df.csv"),
    EB_IV_RESULTS_OUTPUT_PATH = file.path(table_out, "main_results.csv"),
    EB_IV_MAIN_TABLE_CSV = file.path(table_out, "main_table.csv"),
    EB_IV_MAIN_TABLE_TEX = file.path(table_out, "main_table.tex"),
    EB_IV_ACCREDITATION_TABLE_CSV = file.path(table_out, "accreditation.csv"),
    EB_IV_ACCREDITATION_TABLE_TEX = file.path(table_out, "accreditation.tex"),
    EB_IV_PROGRAM_INCOME_TABLE_CSV = file.path(table_out, "program_income.csv"),
    EB_IV_PROGRAM_INCOME_TABLE_TEX = file.path(table_out, "program_income.tex"),
    EB_IV_ENROLLMENT_TABLE_CSV = file.path(table_out, "enrollment.csv"),
    EB_IV_ENROLLMENT_TABLE_TEX = file.path(table_out, "enrollment.tex"),
    EB_IV_EXAM_TABLE_CSV = file.path(table_out, "exam.csv"),
    EB_IV_EXAM_TABLE_TEX = file.path(table_out, "exam.tex"),
    EB_IV_DIAGNOSTICS_OUTPUT_PATH = file.path(clean_out, "diagnostics.csv")
  )
  message("Running pairing: VA ", va_min, "-", va_max, "; SAE ", sae_min, "-", sae_max)
  sys.source(script, envir = new.env(parent = globalenv()))
}

run_pair(2017L, 2020L, 2018L, 2020L)
if (identical(Sys.getenv("RUN_NO_OVERLAP", unset = "0"), "1")) {
  run_pair(2017L, 2018L, 2019L, 2020L)
}
