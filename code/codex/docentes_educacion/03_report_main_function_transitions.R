suppressPackageStartupMessages({
  library(data.table)
})

# Track next-year school and function outcomes using every main-position record.
# PERSONAS == 1 is retained, but no ID_IFP function is filtered out.

default_input_dir <- file.path(
  "C:/Users/brunem/Dropbox/causal_schools/data/raw/docentes_educacion"
)
default_output_dir <- file.path(
  getwd(), "output", "tables", "docentes_educacion"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2L) {
  stop(
    "Usage: Rscript 03_report_main_function_transitions.R ",
    "[INPUT_DIR] [OUTPUT_DIR]",
    call. = FALSE
  )
}
input_dir <- if (length(args) >= 1L) args[[1L]] else default_input_dir
output_dir <- if (length(args) >= 2L) args[[2L]] else default_output_dir
input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

function_labels <- c(
  "0" = "No function",
  "1" = "Classroom teacher",
  "2" = "Technical-pedagogical staff",
  "3" = "Management staff",
  "4" = "Director",
  "5" = "Other function in school",
  "6" = "Other function outside school",
  "7" = "Head of UTP",
  "8" = "Inspector general",
  "9" = "Orientador",
  "10" = "Management",
  "11" = "Technical-pedagogical",
  "12" = "Supervision",
  "13" = "Head of DAEM",
  "14" = "Head of municipal corporation",
  "15" = "Deputy director",
  "16" = "Teacher in charge of school",
  "17" = "Traditional educator"
)

label_function <- function(x) {
  labels <- unname(function_labels[as.character(x)])
  unknown <- is.na(labels) & !is.na(x)
  labels[unknown] <- paste0("ID_IFP ", x[unknown])
  labels
}

csv_files <- sort(list.files(
  input_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
))
if (!length(csv_files)) {
  stop("No CSV files found below: ", input_dir, call. = FALSE)
}

read_main_positions <- function(path) {
  required <- c("AGNO", "RBD", "MRUN", "PERSONAS", "ID_IFP")
  header <- names(fread(
    path, sep = ";", nrows = 0L, encoding = "Latin-1", showProgress = FALSE
  ))
  missing_required <- setdiff(required, header)
  if (length(missing_required)) {
    stop(
      "Missing required variables in ", basename(path), ": ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- fread(
    path,
    sep = ";",
    select = required,
    colClasses = list(character = "MRUN"),
    encoding = "Latin-1",
    na.strings = c("", "NA"),
    showProgress = FALSE
  )
  dt <- dt[
    PERSONAS == 1L & !is.na(AGNO) & !is.na(RBD) &
      !is.na(MRUN) & MRUN != "" & !is.na(ID_IFP),
    .(AGNO, RBD, MRUN, ID_IFP)
  ]
  if (dt[, anyDuplicated(paste(AGNO, MRUN, sep = "|"))] > 0L) {
    stop(
      "PERSONAS == 1 does not uniquely identify person-years in ",
      basename(path), call. = FALSE
    )
  }
  dt[, FUNCTION := label_function(ID_IFP)]
  dt
}

message("Reading all main-position functions from ", length(csv_files), " files...")
panel <- rbindlist(lapply(csv_files, function(path) {
  message("  ", basename(path))
  read_main_positions(path)
}))

# Persistent person-level definition: once a person is observed as an
# orientador, treat them as part of the orientador cohort in every year.
ever_orientador_ids <- panel[ID_IFP == 9L, unique(MRUN)]
ever_orientador_panel <- panel[MRUN %chin% ever_orientador_ids]
setorder(ever_orientador_panel, MRUN, AGNO)
ever_orientador_panel[, EVER_ORIENTADOR := 1L]

ever_orientador_person <- ever_orientador_panel[, .(
  N_YEARS_OBSERVED = uniqueN(AGNO),
  N_FUNCTIONS_OBSERVED = uniqueN(ID_IFP),
  N_RBDS_OBSERVED = uniqueN(RBD),
  FIRST_YEAR_OBSERVED = min(AGNO),
  LAST_YEAR_OBSERVED = max(AGNO),
  FIRST_ORIENTADOR_YEAR = min(AGNO[ID_IFP == 9L]),
  LAST_ORIENTADOR_YEAR = max(AGNO[ID_IFP == 9L]),
  NEVER_CHANGES_ROLE = all(ID_IFP == 9L),
  EVER_CHANGES_ROLE = any(ID_IFP != 9L)
), by = MRUN]

ever_orientador_function_counts <- ever_orientador_panel[, .(
  N_PERSON_YEARS = .N,
  N_PEOPLE_EVER_IN_FUNCTION = uniqueN(MRUN)
), by = .(ID_IFP, FUNCTION)][order(-N_PERSON_YEARS)]
ever_orientador_function_counts[, `:=`(
  SHARE_OF_PERSON_YEARS = N_PERSON_YEARS / sum(N_PERSON_YEARS),
  SHARE_OF_EVER_ORIENTADORES =
    N_PEOPLE_EVER_IN_FUNCTION / length(ever_orientador_ids)
)]

years <- sort(unique(panel$AGNO))
if (length(years) < 2L) {
  stop("At least two years are required.", call. = FALSE)
}

transitions <- rbindlist(lapply(years[-1L], function(to_year) {
  from_year <- to_year - 1L
  from <- panel[
    AGNO == from_year,
    .(
      MRUN,
      FROM_YEAR = AGNO,
      FROM_RBD = RBD,
      FROM_ID_IFP = ID_IFP,
      FROM_FUNCTION = FUNCTION
    )
  ]
  to <- panel[
    AGNO == to_year,
    .(
      MRUN,
      TO_YEAR = AGNO,
      TO_RBD = RBD,
      TO_ID_IFP = ID_IFP,
      TO_FUNCTION = FUNCTION,
      OBSERVED_NEXT = TRUE
    )
  ]
  out <- merge(from, to, by = "MRUN", all.x = TRUE, sort = FALSE)
  out[is.na(OBSERVED_NEXT), OBSERVED_NEXT := FALSE]
  out[OBSERVED_NEXT == FALSE, TO_YEAR := to_year]
  out[, STATUS := fcase(
    !OBSERVED_NEXT, "not_observed_next_year",
    FROM_ID_IFP == TO_ID_IFP & FROM_RBD == TO_RBD,
      "same_function_same_school",
    FROM_ID_IFP == TO_ID_IFP & FROM_RBD != TO_RBD,
      "same_function_changed_school",
    FROM_ID_IFP != TO_ID_IFP & FROM_RBD == TO_RBD,
      "changed_function_same_school",
    FROM_ID_IFP != TO_ID_IFP & FROM_RBD != TO_RBD,
      "changed_function_changed_school",
    default = "unclassified"
  )]
  out
}))

ever_orientador_transitions <- transitions[
  MRUN %chin% ever_orientador_ids
]

status_by_function_year <- transitions[, .N, by = .(
  FROM_YEAR, TO_YEAR, FROM_ID_IFP, FROM_FUNCTION, STATUS
)][, SHARE_OF_STARTERS := N / sum(N), by = .(
  FROM_YEAR, FROM_ID_IFP
)][order(FROM_YEAR, FROM_ID_IFP, STATUS)]

transition_matrix <- transitions[, .N, by = .(
  FROM_ID_IFP,
  FROM_FUNCTION,
  TO_ID_IFP,
  TO_FUNCTION,
  OBSERVED_NEXT
)][, SHARE_WITHIN_START_FUNCTION := N / sum(N), by = FROM_ID_IFP][
  order(FROM_ID_IFP, -N)
]

ever_orientador_transition_matrix <- ever_orientador_transitions[, .N, by = .(
  FROM_ID_IFP,
  FROM_FUNCTION,
  TO_ID_IFP,
  TO_FUNCTION,
  OBSERVED_NEXT
)][, SHARE_WITHIN_START_FUNCTION := N / sum(N), by = FROM_ID_IFP][
  order(FROM_ID_IFP, -N)
]

orientador <- transitions[FROM_ID_IFP == 9L]
orientador_by_year <- orientador[, .(
  N_START = .N,
  N_OBSERVED_NEXT = sum(OBSERVED_NEXT),
  N_NOT_OBSERVED_NEXT = sum(!OBSERVED_NEXT),
  N_REMAIN_ORIENTADOR = sum(OBSERVED_NEXT & TO_ID_IFP == 9L),
  N_CHANGE_FUNCTION = sum(OBSERVED_NEXT & TO_ID_IFP != 9L),
  N_CHANGE_RBD_ANY_FUNCTION = sum(
    OBSERVED_NEXT & FROM_RBD != TO_RBD, na.rm = TRUE
  ),
  N_REMAIN_ORIENTADOR_CHANGE_RBD = sum(
    OBSERVED_NEXT & TO_ID_IFP == 9L & FROM_RBD != TO_RBD,
    na.rm = TRUE
  )
), by = .(FROM_YEAR, TO_YEAR)]
orientador_by_year[, `:=`(
  OBSERVED_NEXT_RATE = N_OBSERVED_NEXT / N_START,
  REMAIN_ORIENTADOR_RATE_AMONG_OBSERVED =
    N_REMAIN_ORIENTADOR / N_OBSERVED_NEXT,
  CHANGE_FUNCTION_RATE_AMONG_OBSERVED =
    N_CHANGE_FUNCTION / N_OBSERVED_NEXT,
  CHANGE_RBD_RATE_AMONG_OBSERVED =
    N_CHANGE_RBD_ANY_FUNCTION / N_OBSERVED_NEXT,
  CHANGE_RBD_RATE_IF_REMAIN_ORIENTADOR =
    N_REMAIN_ORIENTADOR_CHANGE_RBD / N_REMAIN_ORIENTADOR
)]

orientador_status <- orientador[, .N, by = STATUS][order(-N)]
orientador_status[, SHARE_OF_STARTERS := N / sum(N)]

orientador_next_function <- orientador[
  OBSERVED_NEXT == TRUE,
  .N,
  by = .(TO_ID_IFP, TO_FUNCTION)
][order(-N)]
orientador_next_function[, SHARE_AMONG_OBSERVED_NEXT := N / sum(N)]
orientador_next_function[, SHARE_AMONG_ALL_STARTERS := N / nrow(orientador)]

n_start <- nrow(orientador)
n_observed <- orientador[, sum(OBSERVED_NEXT)]
n_not_observed <- n_start - n_observed
n_same_role <- orientador[, sum(OBSERVED_NEXT & TO_ID_IFP == 9L)]
n_changed_role <- n_observed - n_same_role
n_changed_school <- orientador[, sum(
  OBSERVED_NEXT & FROM_RBD != TO_RBD, na.rm = TRUE
)]
n_same_role_changed_school <- orientador[, sum(
  OBSERVED_NEXT & TO_ID_IFP == 9L & FROM_RBD != TO_RBD,
  na.rm = TRUE
)]

n_ever_orientadores <- nrow(ever_orientador_person)
n_ever_orientadores_multi_year <- ever_orientador_person[
  N_YEARS_OBSERVED >= 2L, .N
]
n_never_change_role <- ever_orientador_person[NEVER_CHANGES_ROLE == TRUE, .N]
n_never_change_role_multi_year <- ever_orientador_person[
  N_YEARS_OBSERVED >= 2L & NEVER_CHANGES_ROLE == TRUE, .N
]
n_ever_change_role <- ever_orientador_person[EVER_CHANGES_ROLE == TRUE, .N]

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digits = 1L) sprintf(paste0("%.", digits, "f%%"), 100 * x)

year_rows <- vapply(seq_len(nrow(orientador_by_year)), function(i) {
  x <- orientador_by_year[i]
  paste0(
    "| ", x$FROM_YEAR, "--", x$TO_YEAR,
    " | ", fmt_n(x$N_START),
    " | ", fmt_pct(x$OBSERVED_NEXT_RATE),
    " | ", fmt_pct(x$REMAIN_ORIENTADOR_RATE_AMONG_OBSERVED),
    " | ", fmt_pct(x$CHANGE_FUNCTION_RATE_AMONG_OBSERVED),
    " | ", fmt_pct(x$CHANGE_RBD_RATE_AMONG_OBSERVED), " |"
  )
}, character(1))

next_function_rows <- vapply(
  seq_len(min(8L, nrow(orientador_next_function))),
  function(i) {
    x <- orientador_next_function[i]
    paste0(
      "| ", x$TO_ID_IFP, " | ", x$TO_FUNCTION,
      " | ", fmt_n(x$N),
      " | ", fmt_pct(x$SHARE_AMONG_OBSERVED_NEXT), " |"
    )
  },
  character(1)
)

cohort_function_rows <- vapply(
  seq_len(min(10L, nrow(ever_orientador_function_counts))),
  function(i) {
    x <- ever_orientador_function_counts[i]
    paste0(
      "| ", x$ID_IFP, " | ", x$FUNCTION,
      " | ", fmt_n(x$N_PEOPLE_EVER_IN_FUNCTION),
      " | ", fmt_pct(x$SHARE_OF_EVER_ORIENTADORES),
      " | ", fmt_n(x$N_PERSON_YEARS), " |"
    )
  },
  character(1)
)

report_lines <- c(
  "# Ever-orientador careers and function transitions, 2018--2025",
  "",
  paste0(
    "A person is defined as an orientador if they have `ID_IFP == 9` in at ",
    "least one year. Once identified, all of that person's main-position ",
    "records (`PERSONAS == 1`) are retained, regardless of function."
  ),
  "",
  "## Person-level persistence",
  "",
  paste0(
    "- There are ", fmt_n(n_ever_orientadores), " ever-orientadores. Of ",
    "these, ", fmt_n(n_never_change_role), " (",
    fmt_pct(n_never_change_role / n_ever_orientadores),
    ") are observed only as orientadores in every available year."
  ),
  paste0(
    "- Restricting to the ", fmt_n(n_ever_orientadores_multi_year),
    " people observed in at least two years, ",
    fmt_n(n_never_change_role_multi_year), " (",
    fmt_pct(
      n_never_change_role_multi_year / n_ever_orientadores_multi_year
    ), ") never change recorded main function."
  ),
  paste0(
    "- Conversely, ", fmt_n(n_ever_change_role), " (",
    fmt_pct(n_ever_change_role / n_ever_orientadores),
    ") are observed in at least one non-orientador main function."
  ),
  "",
  "## Functions ever held by the orientador cohort",
  "",
  "| ID_IFP | Function | Ever observed in function | Share of cohort | Person-years |",
  "|---:|---|---:|---:|---:|",
  cohort_function_rows,
  "",
  "## What happens after an orientador year",
  "",
  paste0(
    "This section starts from every `ID_IFP == 9` person-year in 2018--2024, ",
    "then searches all main functions in the following year."
  ),
  "",
  paste0(
    "- Of ", fmt_n(n_start), " orientador transition-years, ",
    fmt_n(n_observed), " (", fmt_pct(n_observed / n_start),
    ") are observed in any main position the next year; ",
    fmt_n(n_not_observed), " are not observed."
  ),
  paste0(
    "- Among those observed next year, ", fmt_n(n_same_role), " (",
    fmt_pct(n_same_role / n_observed), ") remain orientadores and ",
    fmt_n(n_changed_role), " (", fmt_pct(n_changed_role / n_observed),
    ") change main function."
  ),
  paste0(
    "- Among those observed next year, ", fmt_n(n_changed_school), " (",
    fmt_pct(n_changed_school / n_observed), ") change RBD, whether or not ",
    "they also change function."
  ),
  paste0(
    "- Conditional on remaining an orientador, ",
    fmt_n(n_same_role_changed_school), " (",
    fmt_pct(n_same_role_changed_school / n_same_role), ") change RBD."
  ),
  "",
  "### Outcomes by transition year",
  "",
  "| Years | Starting orientadores | Observed next year | Remain orientador (if observed) | Change function (if observed) | Change RBD (if observed) |",
  "|---|---:|---:|---:|---:|---:|",
  year_rows,
  "",
  "### Main function in the following year",
  "",
  "| ID_IFP | Function | Transitions | Share among observed next year |",
  "|---:|---|---:|---:|",
  next_function_rows,
  "",
  "A missing following-year record can mean leaving the directory, moving out of a reported main position, or a temporary gap; it is not necessarily an exit from education.",
  ""
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
status_path <- file.path(
  output_dir, "main_position_function_transition_status_by_year.csv"
)
matrix_path <- file.path(
  output_dir, "main_position_function_transition_matrix.csv"
)
ever_panel_path <- file.path(
  output_dir, "ever_orientador_all_main_positions.csv.gz"
)
ever_person_path <- file.path(
  output_dir, "ever_orientador_person_summary.csv"
)
ever_function_counts_path <- file.path(
  output_dir, "ever_orientador_function_counts.csv"
)
ever_matrix_path <- file.path(
  output_dir, "ever_orientador_function_transition_matrix.csv"
)
orientador_year_path <- file.path(
  output_dir, "orientador_all_functions_next_year_by_year.csv"
)
orientador_status_path <- file.path(
  output_dir, "orientador_all_functions_next_year_status.csv"
)
orientador_function_path <- file.path(
  output_dir, "orientador_next_main_function.csv"
)
report_path <- file.path(
  output_dir, "orientador_all_functions_transition_report.md"
)

fwrite(status_by_function_year, status_path)
fwrite(transition_matrix, matrix_path)
fwrite(ever_orientador_panel, ever_panel_path, compress = "gzip")
fwrite(ever_orientador_person, ever_person_path)
fwrite(ever_orientador_function_counts, ever_function_counts_path)
fwrite(ever_orientador_transition_matrix, ever_matrix_path)
fwrite(orientador_by_year, orientador_year_path)
fwrite(orientador_status, orientador_status_path)
fwrite(orientador_next_function, orientador_function_path)
writeLines(report_lines, report_path, useBytes = TRUE)

message("Wrote all-function status report: ", status_path)
message("Wrote all-function transition matrix: ", matrix_path)
message("Wrote ever-orientador panel: ", ever_panel_path)
message("Wrote ever-orientador person summary: ", ever_person_path)
message("Wrote ever-orientador function counts: ", ever_function_counts_path)
message("Wrote ever-orientador transition matrix: ", ever_matrix_path)
message("Wrote orientador yearly outcomes: ", orientador_year_path)
message("Wrote orientador pooled status: ", orientador_status_path)
message("Wrote orientador next functions: ", orientador_function_path)
message("Wrote orientador report: ", report_path)

print(orientador_by_year)
print(orientador_next_function)
