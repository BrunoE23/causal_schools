###############################################################################
# Orthogonal decomposition of program-income-full EB value added
#
# This script decomposes the school-level EB VA for log program_income_full into
# mutually orthogonal components spanned by:
#
#   1. math VA
#   2. institution-income VA, residualized on math VA
#   3. area-income VA, residualized on math VA and institution-income VA
#
# The order can be changed with PROGRAM_INCOME_DECOMP_ORDER, for example:
# PROGRAM_INCOME_DECOMP_ORDER=exam,math,area,institution
#
# The decomposition is school-level and weighted by the number of students in the
# program_income_full VA regression. It does not re-estimate VA or EB.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }

  candidates[[1]]
}

weighted_mean <- function(x, w) {
  sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

weighted_center <- function(x, w) {
  x - weighted_mean(x, w)
}

weighted_projection <- function(x, z, w) {
  if (is.null(z) || ncol(z) == 0) {
    return(rep(0, length(x)))
  }
  sw <- sqrt(w)
  fit <- lm.wfit(x = z * sw, y = x * sw, w = rep(1, length(x)))
  as.vector(z %*% fit$coefficients)
}

weighted_residualize <- function(x, z, w) {
  x - weighted_projection(x, z, w)
}

weighted_var <- function(x, w) {
  sum(w * x^2, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

weighted_cor <- function(x, y, w) {
  denom <- sqrt(weighted_var(x, w) * weighted_var(y, w))
  if (is.na(denom) || denom == 0) {
    return(NA_real_)
  }
  sum(w * x * y, na.rm = TRUE) / sum(w[!is.na(x) & !is.na(y)], na.rm = TRUE) / denom
}

parse_decomp_order <- function(value) {
  if (!nzchar(trimws(value))) {
    value <- "math,institution,area"
  }
  order <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  base <- c("math", "institution", "area")
  with_exam <- c("exam", base)
  valid_order <- (setequal(order, base) && length(order) == length(base)) ||
    (setequal(order, with_exam) && length(order) == length(with_exam))
  if (!valid_order || anyDuplicated(order)) {
    stop(
      "PROGRAM_INCOME_DECOMP_ORDER must be a comma-separated permutation of either ",
      paste(base, collapse = ", "),
      " or ",
      paste(with_exam, collapse = ", "),
      call. = FALSE
    )
  }
  order
}

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = getwd()
)
out_dir <- file.path(repo_wd, "output", "tables", "empirical_bayes_school_va")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

school_values_path <- find_existing_path(
  "PROGRAM_INCOME_DECOMP_SCHOOL_VALUES_INPUT_PATH",
  file.path(out_dir, "stata_eb_school_rbd_observational_values_for_iv.csv"),
  "school-level EB VA input"
)

school_output_path <- Sys.getenv(
  "PROGRAM_INCOME_DECOMP_SCHOOL_OUTPUT_PATH",
  unset = file.path(out_dir, "program_income_full_va_orthogonal_decomposition_school_values.csv")
)
summary_output_path <- Sys.getenv(
  "PROGRAM_INCOME_DECOMP_SUMMARY_OUTPUT_PATH",
  unset = file.path(out_dir, "program_income_full_va_orthogonal_decomposition_summary.csv")
)
correlation_output_path <- Sys.getenv(
  "PROGRAM_INCOME_DECOMP_CORRELATION_OUTPUT_PATH",
  unset = file.path(out_dir, "program_income_full_va_orthogonal_decomposition_correlations.csv")
)
extended_school_values_path <- Sys.getenv(
  "PROGRAM_INCOME_DECOMP_EXTENDED_SCHOOL_VALUES_OUTPUT_PATH",
  unset = file.path(out_dir, "stata_eb_school_rbd_observational_values_for_iv_with_program_income_decomposition.csv")
)

exam_outcome <- "admission_exam_taker"
math_outcome <- "z_year_math_max"
institution_income_outcome <- "log_program_income_institution_clp_m1"
area_income_outcome <- "log_program_income_area_clp_m1"
full_income_outcome <- "log_program_income_full_clp_m1"
needed_outcomes <- c(
  exam_outcome,
  math_outcome,
  institution_income_outcome,
  area_income_outcome,
  full_income_outcome
)

component_outcomes <- c(
  exam = "log_program_income_full_component_exam_taking_va",
  math = "log_program_income_full_component_math_va",
  institution = "log_program_income_full_component_institution_income_va",
  area = "log_program_income_full_component_area_income_va",
  residual = "log_program_income_full_component_residual_va"
)
decomp_order <- parse_decomp_order(Sys.getenv(
  "PROGRAM_INCOME_DECOMP_ORDER",
  unset = "math,institution,area"
))
decomp_order_label <- paste(decomp_order, collapse = "_")

source_outcome_by_component <- c(
  exam = exam_outcome,
  math = math_outcome,
  institution = institution_income_outcome,
  area = area_income_outcome
)
component_label_by_source <- c(
  exam = "exam_taking_va_orthogonalized",
  math = "math_va",
  institution = "institution_income_va_orthogonalized",
  area = "area_income_va_orthogonalized"
)
orth_col_by_source <- c(
  exam = "exam_taking_va_orthogonalized",
  math = "math_va_orthogonalized",
  institution = "institution_income_va_orthogonalized",
  area = "area_income_va_orthogonalized"
)
component_col_by_source <- c(
  exam = "program_income_full_component_exam_taking_va",
  math = "program_income_full_component_math_va",
  institution = "program_income_full_component_institution_income_va",
  area = "program_income_full_component_area_income_va"
)

message("Reading school-level EB VA: ", school_values_path)
message("Decomposition order: ", decomp_order_label)
school_values <- fread(school_values_path, na.strings = c("", "NA"))
required_cols <- c(
  "school_rbd",
  "analysis_sample",
  "outcome",
  "n_students_regression",
  "controlled_value_added_eb_centered_student"
)
missing_cols <- setdiff(required_cols, names(school_values))
if (length(missing_cols) > 0) {
  stop("School values file is missing required columns: ", paste(missing_cols, collapse = ", "))
}

decomp_long <- school_values[
  analysis_sample == "All" & outcome %chin% needed_outcomes,
  .(
    school_rbd = as.numeric(school_rbd),
    outcome,
    n_students_regression = as.numeric(n_students_regression),
    eb_va = as.numeric(controlled_value_added_eb_centered_student)
  )
]

missing_outcomes <- setdiff(needed_outcomes, unique(decomp_long$outcome))
if (length(missing_outcomes) > 0) {
  stop("Missing required outcomes in EB VA input: ", paste(missing_outcomes, collapse = ", "))
}

va_wide <- dcast(
  decomp_long,
  school_rbd ~ outcome,
  value.var = c("eb_va", "n_students_regression")
)
setnames(
  va_wide,
  old = paste0("eb_va_", c(exam_outcome, math_outcome, institution_income_outcome, area_income_outcome, full_income_outcome)),
  new = c("exam_taking_va", "math_va", "institution_income_va", "area_income_va", "program_income_full_va")
)
setnames(
  va_wide,
  old = paste0("n_students_regression_", c(exam_outcome, math_outcome, institution_income_outcome, area_income_outcome, full_income_outcome)),
  new = c("exam_taking_n", "math_n", "institution_income_n", "area_income_n", "program_income_full_n")
)

raw_col_by_source <- c(
  exam = "exam_taking_va",
  math = "math_va",
  institution = "institution_income_va",
  area = "area_income_va"
)
required_decomp_cols <- c(unname(raw_col_by_source[decomp_order]), "program_income_full_va", "program_income_full_n")
va_wide[, complete_decomposition_input := complete.cases(
  .SD
) & program_income_full_n > 0, .SDcols = required_decomp_cols]

work <- va_wide[complete_decomposition_input == TRUE]
if (nrow(work) == 0) {
  stop("No schools have complete inputs for the program-income decomposition.")
}

w <- work$program_income_full_n
y <- weighted_center(work$program_income_full_va, w)
exam_centered <- weighted_center(work$exam_taking_va, w)
math_centered <- weighted_center(work$math_va, w)
institution_centered <- weighted_center(work$institution_income_va, w)
area_centered <- weighted_center(work$area_income_va, w)

centered_by_source <- list(
  exam = exam_centered,
  math = math_centered,
  institution = institution_centered,
  area = area_centered
)

orth_by_source <- list()
orth_against_by_source <- character()
for (component_name in decomp_order) {
  previous_components <- names(orth_by_source)
  if (length(previous_components) == 0) {
    orth_by_source[[component_name]] <- centered_by_source[[component_name]]
    orth_against_by_source[[component_name]] <- "weighted mean only"
  } else {
    z <- do.call(cbind, orth_by_source[previous_components])
    orth_by_source[[component_name]] <- weighted_residualize(
      centered_by_source[[component_name]],
      z,
      w
    )
    orth_against_by_source[[component_name]] <- paste(
      component_label_by_source[previous_components],
      collapse = " + "
    )
  }
}

x_orth <- do.call(cbind, orth_by_source[decomp_order])
colnames(x_orth) <- paste0(decomp_order, "_orthogonalized")
sw <- sqrt(w)
decomp_fit <- lm.wfit(x = x_orth * sw, y = y * sw, w = rep(1, length(y)))
beta <- decomp_fit$coefficients
names(beta) <- decomp_order

components <- data.table(.rows = seq_len(nrow(work)))
for (component_name in decomp_order) {
  components[, (component_col_by_source[[component_name]]) :=
    orth_by_source[[component_name]] * beta[[component_name]]]
}
components[, .rows := NULL]
components[, program_income_full_component_residual_va := y - rowSums(.SD), .SDcols = names(components)]
component_value_cols <- unname(component_col_by_source[decomp_order])
component_cols <- c(component_value_cols, "program_income_full_component_residual_va")

work <- cbind(
  work,
  data.table(
    program_income_full_va_weighted_centered = y,
    exam_taking_va_weighted_centered = exam_centered,
    math_va_weighted_centered = math_centered,
    institution_income_va_weighted_centered = institution_centered,
    area_income_va_weighted_centered = area_centered,
    exam_taking_va_orthogonalized = if ("exam" %in% decomp_order) orth_by_source[["exam"]] else rep(NA_real_, nrow(work)),
    math_va_orthogonalized = orth_by_source[["math"]],
    institution_income_va_orthogonalized = orth_by_source[["institution"]],
    area_income_va_orthogonalized = orth_by_source[["area"]]
  ),
  components
)
work[, decomposition_order := decomp_order_label]
work[, program_income_full_component_sum_va := rowSums(.SD), .SDcols = component_cols]
work[, reconstruction_error := program_income_full_va_weighted_centered -
  program_income_full_component_sum_va]

total_var <- weighted_var(work$program_income_full_va_weighted_centered, w)
summary_component_cols <- component_cols
summary_dt <- data.table(
  decomposition_order = decomp_order_label,
  component_order = c(seq_along(decomp_order), NA_integer_),
  component = c(component_label_by_source[decomp_order], "residual"),
  source_outcome = c(source_outcome_by_component[decomp_order], NA_character_),
  orthogonalized_against = c(orth_against_by_source[decomp_order], NA_character_),
  coefficient = c(beta[decomp_order], NA_real_),
  weighted_sd_component = sapply(summary_component_cols, function(col) sqrt(weighted_var(work[[col]], w))),
  share_of_program_income_full_va_variance = sapply(summary_component_cols, function(col) {
    weighted_var(work[[col]], w) / total_var
  }),
  weighted_correlation_with_program_income_full_va = sapply(summary_component_cols, function(col) {
    weighted_cor(work[[col]], work$program_income_full_va_weighted_centered, w)
  })
)
summary_dt[, `:=`(
  n_schools_common = nrow(work),
  n_schools_input = nrow(va_wide),
  weighted_r2 = 1 - weighted_var(work$program_income_full_component_residual_va, w) / total_var,
  max_abs_reconstruction_error = max(abs(work$reconstruction_error), na.rm = TRUE)
)]

cor_vars <- c(
  "program_income_full_va_weighted_centered",
  "exam_taking_va_weighted_centered",
  "math_va_weighted_centered",
  "institution_income_va_weighted_centered",
  "area_income_va_weighted_centered",
  "exam_taking_va_orthogonalized",
  "math_va_orthogonalized",
  "institution_income_va_orthogonalized",
  "area_income_va_orthogonalized"
)
correlation_dt <- rbindlist(lapply(cor_vars, function(x) {
  rbindlist(lapply(cor_vars, function(yvar) {
    data.table(
      variable_1 = x,
      variable_2 = yvar,
      weighted_correlation = weighted_cor(work[[x]], work[[yvar]], w)
    )
  }))
}))

decomp_school_values <- merge(
  va_wide,
  work[, c("school_rbd", "decomposition_order", component_cols, "program_income_full_component_sum_va", "reconstruction_error"), with = FALSE],
  by = "school_rbd",
  all.x = TRUE,
  sort = FALSE
)

template <- school_values[analysis_sample == "All" & outcome == full_income_outcome]
if (nrow(template) == 0) {
  stop("Could not find template rows for ", full_income_outcome)
}
make_component_rows <- function(outcome_label, value_col) {
  rows <- merge(
    template,
    decomp_school_values[, .(school_rbd = as.numeric(school_rbd), component_value = get(value_col))],
    by = "school_rbd",
    all.x = TRUE,
    sort = FALSE
  )
  rows[, outcome := outcome_label]
  for (col in c(
    "controlled_value_added",
    "controlled_value_added_centered_student",
    "controlled_value_added_eb",
    "controlled_value_added_eb_centered_student"
  )) {
    if (col %in% names(rows)) {
      rows[, (col) := component_value]
    }
  }
  for (col in c(
    "controlled_value_added_se",
    "controlled_value_added_resid_sd",
    "eb_tau2",
    "eb_tau",
    "eb_reliability"
  )) {
    if (col %in% names(rows)) {
      rows[, (col) := NA_real_]
    }
  }
  if ("controlled_value_added_se_method" %in% names(rows)) {
    rows[, controlled_value_added_se_method := "orthogonal_decomposition_no_stage1_se"]
  }
  if ("eb_method" %in% names(rows)) {
    rows[, eb_method := paste0("weighted_school_level_gram_schmidt_from_stata_eb_order_", decomp_order_label)]
  }
  rows[, component_value := NULL]
  rows[, names(school_values), with = FALSE]
}

component_rows <- rbindlist(
  c(
    lapply(decomp_order, function(component_name) {
      make_component_rows(
        component_outcomes[[component_name]],
        component_col_by_source[[component_name]]
      )
    }),
    list(make_component_rows(
      component_outcomes[["residual"]],
      "program_income_full_component_residual_va"
    ))
  ),
  use.names = TRUE
)
extended_school_values <- rbindlist(
  list(school_values, component_rows),
  use.names = TRUE,
  fill = TRUE
)

message("Writing decomposition school values: ", school_output_path)
fwrite(decomp_school_values, school_output_path)
message("Writing decomposition summary: ", summary_output_path)
fwrite(summary_dt, summary_output_path)
message("Writing decomposition correlations: ", correlation_output_path)
fwrite(correlation_dt, correlation_output_path)
message("Writing extended EB VA file for IV: ", extended_school_values_path)
fwrite(extended_school_values, extended_school_values_path)

print(summary_dt)
