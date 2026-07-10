###############################################################################
# Top schools by Empirical-Bayes value added
#
# Lists the highest EB-shrunken attended-school VA estimates for selected
# outcomes, using the All-sample school-value file.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

required_file <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
  path
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

school_directory_root <- file.path(data_wd, "data", "raw", "school_directory")
table_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

school_values_path <- Sys.getenv(
  "EB_SCHOOL_VALUES_INPUT_PATH",
  unset = file.path(table_dir, "stata_eb_school_rbd_observational_values_for_iv.csv")
)
top_n <- as.integer(Sys.getenv("TOP_EB_VA_SCHOOLS_N", unset = "30"))

output_csv <- file.path(table_dir, "top_eb_va_schools_main_outcomes.csv")
output_wide_csv <- file.path(table_dir, "top_eb_va_schools_main_outcomes_wide.csv")
output_no_private_paid_csv <- file.path(
  table_dir,
  "top_eb_va_schools_main_outcomes_no_private_paid.csv"
)
output_no_private_paid_wide_csv <- file.path(
  table_dir,
  "top_eb_va_schools_main_outcomes_no_private_paid_wide.csv"
)

outcome_specs <- data.table(
  outcome = c(
    "z_year_math_max",
    "z_year_leng_max",
    "log_program_income_clp_m1",
    "stem_enrollment_m1"
  ),
  outcome_label = c(
    "Math",
    "Verbal",
    "Program income",
    "STEM enrollment"
  ),
  outcome_order = seq_len(4L)
)

dependency_detail_labels <- c(
  "1" = "Corporacion Municipal",
  "2" = "Municipal DAEM",
  "3" = "Particular Subvencionado",
  "4" = "Particular Pagado",
  "5" = "Administracion Delegada",
  "6" = "Servicio Local de Educacion"
)
dependency_group_labels <- c(
  "1" = "Municipal",
  "2" = "Particular Subvencionado",
  "3" = "Particular Pagado",
  "4" = "Administracion Delegada",
  "5" = "Servicio Local de Educacion"
)

read_school_directory_metadata <- function(root) {
  if (!dir.exists(root)) {
    warning("School directory root does not exist: ", root)
    return(data.table(school_rbd = numeric()))
  }

  directory_files <- list.files(
    root,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(directory_files) == 0) {
    warning("No school directory CSVs found under: ", root)
    return(data.table(school_rbd = numeric()))
  }

  directory_year <- as.integer(regmatches(
    directory_files,
    regexpr("\\b20[0-9]{2}\\b", directory_files)
  ))

  metadata <- rbindlist(
    lapply(seq_along(directory_files), function(i) {
      directory_file <- directory_files[[i]]
      header <- names(fread(
        directory_file,
        nrows = 0,
        encoding = "UTF-8",
        showProgress = FALSE
      ))
      header_upper <- toupper(trimws(header))

      wanted <- c(
        "RBD",
        "NOM_RBD",
        "NOM_COM_RBD",
        "NOM_REG_RBD_A",
        "COD_REG_RBD",
        "COD_COM_RBD",
        "COD_DEPE",
        "COD_DEPE2",
        "RURAL_RBD"
      )
      original_cols <- header[match(wanted, header_upper)]
      original_cols <- original_cols[!is.na(original_cols)]

      if (!any(toupper(trimws(original_cols)) == "RBD")) {
        warning("School directory has no RBD column and will be skipped: ", directory_file)
        return(data.table())
      }

      dt <- fread(
        directory_file,
        select = original_cols,
        encoding = "UTF-8",
        showProgress = FALSE
      )
      setnames(dt, names(dt), toupper(trimws(names(dt))))

      for (col in wanted) {
        if (!col %in% names(dt)) {
          dt[, (col) := NA_character_]
        }
      }

      dt[, .(
        school_rbd = as.numeric(RBD),
        directory_year = directory_year[[i]],
        school_name = as.character(NOM_RBD),
        comuna_name = as.character(NOM_COM_RBD),
        region_name = as.character(NOM_REG_RBD_A),
        region_code = as.character(COD_REG_RBD),
        comuna_code = as.character(COD_COM_RBD),
        dependency_detail_code = as.character(COD_DEPE),
        dependency_group_code = as.character(COD_DEPE2),
        rural = as.character(RURAL_RBD)
      )]
    }),
    use.names = TRUE,
    fill = TRUE
  )

  metadata <- metadata[!is.na(school_rbd)]
  if (nrow(metadata) == 0) {
    return(data.table(school_rbd = numeric()))
  }

  metadata[, dependency_detail_label := dependency_detail_labels[dependency_detail_code]]
  metadata[, dependency_group_label := dependency_group_labels[dependency_group_code]]
  metadata[, private_paid := dependency_detail_code == "4" | dependency_group_code == "3"]

  setorder(metadata, school_rbd, -directory_year)
  metadata[, .SD[1L], by = school_rbd]
}

message("Reading EB school values: ", school_values_path)
school_values <- fread(
  required_file(school_values_path, "EB school values"),
  na.strings = c("", "NA")
)

message("Reading school directory metadata: ", school_directory_root)
school_metadata <- read_school_directory_metadata(school_directory_root)

top_schools <- school_values[
  analysis_sample == "All" &
    outcome %chin% outcome_specs$outcome &
    is.finite(controlled_value_added_eb_centered_student)
]
top_schools <- merge(
  top_schools,
  outcome_specs,
  by = "outcome",
  all.x = TRUE,
  sort = FALSE
)
top_schools <- merge(
  top_schools,
  school_metadata,
  by = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

setorder(
  top_schools,
  outcome_order,
  -controlled_value_added_eb_centered_student,
  school_rbd
)
top_schools <- top_schools[, head(.SD, top_n), by = .(outcome, outcome_label, outcome_order)]
top_schools[, rank := seq_len(.N), by = .(outcome, outcome_label)]
setcolorder(top_schools, c(
  "outcome_label",
  "rank",
  "school_rbd",
  "school_name",
  "comuna_name",
  "region_name",
  "dependency_detail_label",
  "dependency_group_label",
  "private_paid",
  "controlled_value_added_eb_centered_student",
  "controlled_value_added_centered_student",
  "eb_reliability",
  "n_students_regression",
  "n_total",
  "controlled_value_added_se",
  "outcome"
))

top_schools_export <- top_schools[, .(
  outcome_label,
  rank,
  school_rbd,
  school_name,
  comuna_name,
  region_name,
  dependency_detail_label,
  dependency_group_label,
  private_paid,
  eb_va = controlled_value_added_eb_centered_student,
  original_va = controlled_value_added_centered_student,
  eb_reliability,
  n_students_regression,
  n_total,
  controlled_value_added_se,
  outcome
)]

message("Writing top EB VA schools: ", output_csv)
fwrite(top_schools_export, output_csv)

top_schools_wide <- dcast(
  top_schools_export,
  rank ~ outcome_label,
  value.var = c(
    "school_rbd",
    "school_name",
    "comuna_name",
    "dependency_detail_label",
    "eb_va",
    "n_students_regression"
  )
)

message("Writing wide top EB VA schools: ", output_wide_csv)
fwrite(top_schools_wide, output_wide_csv)

top_schools_no_private_paid <- school_values[
  analysis_sample == "All" &
    outcome %chin% outcome_specs$outcome &
    is.finite(controlled_value_added_eb_centered_student)
]
top_schools_no_private_paid <- merge(
  top_schools_no_private_paid,
  outcome_specs,
  by = "outcome",
  all.x = TRUE,
  sort = FALSE
)
top_schools_no_private_paid <- merge(
  top_schools_no_private_paid,
  school_metadata,
  by = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)
top_schools_no_private_paid[is.na(private_paid), private_paid := FALSE]
top_schools_no_private_paid <- top_schools_no_private_paid[private_paid == FALSE]

setorder(
  top_schools_no_private_paid,
  outcome_order,
  -controlled_value_added_eb_centered_student,
  school_rbd
)
top_schools_no_private_paid <- top_schools_no_private_paid[
  ,
  head(.SD, top_n),
  by = .(outcome, outcome_label, outcome_order)
]
top_schools_no_private_paid[, rank := seq_len(.N), by = .(outcome, outcome_label)]

top_schools_no_private_paid_export <- top_schools_no_private_paid[, .(
  outcome_label,
  rank,
  school_rbd,
  school_name,
  comuna_name,
  region_name,
  dependency_detail_label,
  dependency_group_label,
  private_paid,
  eb_va = controlled_value_added_eb_centered_student,
  original_va = controlled_value_added_centered_student,
  eb_reliability,
  n_students_regression,
  n_total,
  controlled_value_added_se,
  outcome
)]

message("Writing top EB VA schools excluding Particular Pagado: ", output_no_private_paid_csv)
fwrite(top_schools_no_private_paid_export, output_no_private_paid_csv)

top_schools_no_private_paid_wide <- dcast(
  top_schools_no_private_paid_export,
  rank ~ outcome_label,
  value.var = c(
    "school_rbd",
    "school_name",
    "comuna_name",
    "dependency_detail_label",
    "eb_va",
    "n_students_regression"
  )
)

message("Writing wide top EB VA schools excluding Particular Pagado: ", output_no_private_paid_wide_csv)
fwrite(top_schools_no_private_paid_wide, output_no_private_paid_wide_csv)

print(top_schools_export[, .(
  outcome_label,
  rank,
  school_rbd,
  school_name,
  comuna_name,
  dependency_detail_label,
  eb_va,
  eb_reliability,
  n_students_regression
)])
