suppressPackageStartupMessages({
  library(data.table)
})

# Follow everyone who ever holds a selected main function, retaining all of
# their observed main functions. Defaults to classroom teachers (ID_IFP == 1).

default_input <- file.path(
  getwd(), "output", "data", "docentes_educacion",
  "docentes_educacion_2018_2025_clean.csv.gz"
)
default_output <- file.path(
  getwd(), "output", "tables", "docentes_educacion"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 3L) {
  stop(
    "Usage: Rscript 04_report_ever_function_cohort.R ",
    "[CLEANED_FILE] [OUTPUT_DIR] [ID_IFP]",
    call. = FALSE
  )
}

input_file <- if (length(args) >= 1L) args[[1L]] else default_input
output_dir <- if (length(args) >= 2L) args[[2L]] else default_output
target_id_ifp <- if (length(args) >= 3L) as.integer(args[[3L]]) else 1L
if (is.na(target_id_ifp)) stop("ID_IFP must be an integer.", call. = FALSE)

input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

meta <- switch(
  as.character(target_id_ifp),
  "1" = list(
    slug = "teacher",
    singular = "classroom teacher",
    plural = "classroom teachers"
  ),
  "9" = list(
    slug = "orientador",
    singular = "orientador",
    plural = "orientadores"
  ),
  list(
    slug = paste0("id_ifp_", target_id_ifp),
    singular = paste0("ID_IFP ", target_id_ifp, " holder"),
    plural = paste0("ID_IFP ", target_id_ifp, " holders")
  )
)

needed <- c("AGNO", "RBD", "MRUN", "ID_IFP", "FUNCION_PRINCIPAL")
panel <- fread(
  input_file,
  select = needed,
  colClasses = list(character = "MRUN"),
  showProgress = FALSE
)
if (panel[, anyDuplicated(paste(AGNO, MRUN, sep = "|"))] > 0L) {
  stop("The cleaned panel contains duplicate person-years.", call. = FALSE)
}

cohort_ids <- panel[ID_IFP == target_id_ifp, unique(MRUN)]
cohort <- panel[MRUN %chin% cohort_ids]
setorder(cohort, MRUN, AGNO)

person_summary <- cohort[, .(
  N_YEARS_OBSERVED = uniqueN(AGNO),
  N_FUNCTIONS_OBSERVED = uniqueN(ID_IFP),
  N_RBDS_OBSERVED = uniqueN(RBD),
  FIRST_YEAR_OBSERVED = min(AGNO),
  LAST_YEAR_OBSERVED = max(AGNO),
  FIRST_TARGET_FUNCTION_YEAR = min(AGNO[ID_IFP == target_id_ifp]),
  LAST_TARGET_FUNCTION_YEAR = max(AGNO[ID_IFP == target_id_ifp]),
  NEVER_CHANGES_ROLE = all(ID_IFP == target_id_ifp),
  EVER_CHANGES_ROLE = any(ID_IFP != target_id_ifp)
), by = MRUN]

function_counts <- cohort[, .(
  N_PERSON_YEARS = .N,
  N_PEOPLE_EVER_IN_FUNCTION = uniqueN(MRUN)
), by = .(ID_IFP, FUNCION_PRINCIPAL)][order(-N_PERSON_YEARS)]
function_counts[, `:=`(
  SHARE_OF_PERSON_YEARS = N_PERSON_YEARS / sum(N_PERSON_YEARS),
  SHARE_OF_COHORT = N_PEOPLE_EVER_IN_FUNCTION / length(cohort_ids)
)]

# Construct each person's next observed record, then require calendar-year
# adjacency when classifying a following-year outcome.
cohort[, `:=`(
  NEXT_AGNO = shift(AGNO, type = "lead"),
  NEXT_RBD = shift(RBD, type = "lead"),
  NEXT_ID_IFP = shift(ID_IFP, type = "lead"),
  NEXT_FUNCTION = shift(FUNCION_PRINCIPAL, type = "lead")
), by = MRUN]
cohort[, OBSERVED_NEXT_YEAR := NEXT_AGNO == AGNO + 1L]
cohort[is.na(OBSERVED_NEXT_YEAR), OBSERVED_NEXT_YEAR := FALSE]

years <- sort(unique(panel$AGNO))
target_starts <- cohort[
  ID_IFP == target_id_ifp & AGNO < max(years)
]
target_starts[, STATUS := fcase(
  OBSERVED_NEXT_YEAR == FALSE, "not_observed_next_year",
  NEXT_ID_IFP == target_id_ifp & NEXT_RBD == RBD,
    "same_function_same_school",
  NEXT_ID_IFP == target_id_ifp & NEXT_RBD != RBD,
    "same_function_changed_school",
  NEXT_ID_IFP != target_id_ifp & NEXT_RBD == RBD,
    "changed_function_same_school",
  NEXT_ID_IFP != target_id_ifp & NEXT_RBD != RBD,
    "changed_function_changed_school",
  default = "unclassified"
)]

by_year <- target_starts[, .(
  N_START = .N,
  N_OBSERVED_NEXT = sum(OBSERVED_NEXT_YEAR),
  N_NOT_OBSERVED_NEXT = sum(!OBSERVED_NEXT_YEAR),
  N_REMAIN_FUNCTION = sum(
    OBSERVED_NEXT_YEAR & NEXT_ID_IFP == target_id_ifp
  ),
  N_CHANGE_FUNCTION = sum(
    OBSERVED_NEXT_YEAR & NEXT_ID_IFP != target_id_ifp
  ),
  N_CHANGE_RBD_ANY_FUNCTION = sum(
    OBSERVED_NEXT_YEAR & NEXT_RBD != RBD, na.rm = TRUE
  ),
  N_REMAIN_FUNCTION_CHANGE_RBD = sum(
    OBSERVED_NEXT_YEAR & NEXT_ID_IFP == target_id_ifp & NEXT_RBD != RBD,
    na.rm = TRUE
  )
), by = .(FROM_YEAR = AGNO, TO_YEAR = AGNO + 1L)]
by_year[, `:=`(
  OBSERVED_NEXT_RATE = N_OBSERVED_NEXT / N_START,
  REMAIN_FUNCTION_RATE_AMONG_OBSERVED =
    N_REMAIN_FUNCTION / N_OBSERVED_NEXT,
  CHANGE_FUNCTION_RATE_AMONG_OBSERVED =
    N_CHANGE_FUNCTION / N_OBSERVED_NEXT,
  CHANGE_RBD_RATE_AMONG_OBSERVED =
    N_CHANGE_RBD_ANY_FUNCTION / N_OBSERVED_NEXT,
  CHANGE_RBD_RATE_IF_REMAIN_FUNCTION =
    N_REMAIN_FUNCTION_CHANGE_RBD / N_REMAIN_FUNCTION
)]

next_function <- target_starts[
  OBSERVED_NEXT_YEAR == TRUE,
  .N,
  by = .(NEXT_ID_IFP, NEXT_FUNCTION)
][order(-N)]
next_function[, SHARE_AMONG_OBSERVED_NEXT := N / sum(N)]
next_function[, SHARE_AMONG_ALL_STARTS := N / nrow(target_starts)]

cohort_transitions <- cohort[
  OBSERVED_NEXT_YEAR == TRUE,
  .N,
  by = .(
    FROM_ID_IFP = ID_IFP,
    FROM_FUNCTION = FUNCION_PRINCIPAL,
    TO_ID_IFP = NEXT_ID_IFP,
    TO_FUNCTION = NEXT_FUNCTION
  )
][, SHARE_WITHIN_START_FUNCTION := N / sum(N), by = FROM_ID_IFP][
  order(FROM_ID_IFP, -N)
]

n_people <- nrow(person_summary)
n_multi <- person_summary[N_YEARS_OBSERVED >= 2L, .N]
n_never <- person_summary[NEVER_CHANGES_ROLE == TRUE, .N]
n_never_multi <- person_summary[
  N_YEARS_OBSERVED >= 2L & NEVER_CHANGES_ROLE == TRUE, .N
]
n_change <- person_summary[EVER_CHANGES_ROLE == TRUE, .N]
n_starts <- nrow(target_starts)
n_observed <- target_starts[, sum(OBSERVED_NEXT_YEAR)]
n_remain <- target_starts[, sum(
  OBSERVED_NEXT_YEAR & NEXT_ID_IFP == target_id_ifp
)]
n_change_function <- n_observed - n_remain
n_change_rbd <- target_starts[, sum(
  OBSERVED_NEXT_YEAR & NEXT_RBD != RBD, na.rm = TRUE
)]
n_remain_change_rbd <- target_starts[, sum(
  OBSERVED_NEXT_YEAR & NEXT_ID_IFP == target_id_ifp & NEXT_RBD != RBD,
  na.rm = TRUE
)]

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digits = 1L) sprintf(paste0("%.", digits, "f%%"), 100 * x)

function_rows <- vapply(
  seq_len(min(10L, nrow(function_counts))),
  function(i) {
    x <- function_counts[i]
    paste0(
      "| ", x$ID_IFP, " | ", x$FUNCION_PRINCIPAL,
      " | ", fmt_n(x$N_PEOPLE_EVER_IN_FUNCTION),
      " | ", fmt_pct(x$SHARE_OF_COHORT),
      " | ", fmt_n(x$N_PERSON_YEARS), " |"
    )
  },
  character(1)
)

year_rows <- vapply(seq_len(nrow(by_year)), function(i) {
  x <- by_year[i]
  paste0(
    "| ", x$FROM_YEAR, "--", x$TO_YEAR,
    " | ", fmt_n(x$N_START),
    " | ", fmt_pct(x$OBSERVED_NEXT_RATE),
    " | ", fmt_pct(x$REMAIN_FUNCTION_RATE_AMONG_OBSERVED),
    " | ", fmt_pct(x$CHANGE_FUNCTION_RATE_AMONG_OBSERVED),
    " | ", fmt_pct(x$CHANGE_RBD_RATE_AMONG_OBSERVED), " |"
  )
}, character(1))

report_lines <- c(
  paste0(
    "# Ever-", meta$singular,
    " careers and function transitions, 2018--2025"
  ),
  "",
  paste0(
    "A person enters the cohort if they have `ID_IFP == ", target_id_ifp,
    "` in at least one year. All of their main-position records ",
    "(`PERSONAS == 1`) are then retained, regardless of function."
  ),
  "",
  "## Person-level persistence",
  "",
  paste0(
    "- There are ", fmt_n(n_people), " ever-", meta$plural, ". Of these, ",
    fmt_n(n_never), " (", fmt_pct(n_never / n_people),
    ") are observed only in the target function."
  ),
  paste0(
    "- Among ", fmt_n(n_multi), " people observed in at least two years, ",
    fmt_n(n_never_multi), " (", fmt_pct(n_never_multi / n_multi),
    ") never change recorded main function."
  ),
  paste0(
    "- Conversely, ", fmt_n(n_change), " (", fmt_pct(n_change / n_people),
    ") are observed in at least one other main function."
  ),
  "",
  "## Functions held by the cohort",
  "",
  "| ID_IFP | Function | Ever observed in function | Share of cohort | Person-years |",
  "|---:|---|---:|---:|---:|",
  function_rows,
  "",
  paste0("## What happens after a ", meta$singular, " year"),
  "",
  paste0(
    "- Of ", fmt_n(n_starts), " target-function transition-years, ",
    fmt_n(n_observed), " (", fmt_pct(n_observed / n_starts),
    ") are observed in any main position the next year."
  ),
  paste0(
    "- Among those observed, ", fmt_n(n_remain), " (",
    fmt_pct(n_remain / n_observed), ") remain in the target function and ",
    fmt_n(n_change_function), " (",
    fmt_pct(n_change_function / n_observed), ") change function."
  ),
  paste0(
    "- Among those observed, ", fmt_n(n_change_rbd), " (",
    fmt_pct(n_change_rbd / n_observed),
    ") change RBD across all next-year functions. Conditional on remaining ",
    "in the target function, ", fmt_n(n_remain_change_rbd), " (",
    fmt_pct(n_remain_change_rbd / n_remain), ") change RBD."
  ),
  "",
  "| Years | Starting records | Observed next year | Remain in function | Change function | Change RBD |",
  "|---|---:|---:|---:|---:|---:|",
  year_rows,
  ""
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
prefix <- paste0("ever_", meta$slug)
person_path <- file.path(output_dir, paste0(prefix, "_person_summary.csv"))
function_path <- file.path(output_dir, paste0(prefix, "_function_counts.csv"))
year_path <- file.path(output_dir, paste0(prefix, "_next_year_by_year.csv"))
next_path <- file.path(output_dir, paste0(prefix, "_next_main_function.csv"))
matrix_path <- file.path(
  output_dir, paste0(prefix, "_function_transition_matrix.csv")
)
report_path <- file.path(output_dir, paste0(prefix, "_transition_report.md"))

fwrite(person_summary, person_path)
fwrite(function_counts, function_path)
fwrite(by_year, year_path)
fwrite(next_function, next_path)
fwrite(cohort_transitions, matrix_path)
writeLines(report_lines, report_path, useBytes = TRUE)

message("Wrote person summary: ", person_path)
message("Wrote function counts: ", function_path)
message("Wrote next-year outcomes: ", year_path)
message("Wrote next functions: ", next_path)
message("Wrote transition matrix: ", matrix_path)
message("Wrote report: ", report_path)

print(data.table(
  N_PEOPLE = n_people,
  N_MULTI_YEAR = n_multi,
  N_NEVER_CHANGE_ROLE = n_never,
  SHARE_NEVER_CHANGE_ROLE = n_never / n_people,
  N_NEVER_CHANGE_ROLE_MULTI_YEAR = n_never_multi,
  SHARE_NEVER_CHANGE_ROLE_MULTI_YEAR = n_never_multi / n_multi,
  NEXT_YEAR_FUNCTION_CHANGE_RATE = n_change_function / n_observed,
  NEXT_YEAR_RBD_CHANGE_RATE = n_change_rbd / n_observed,
  RBD_CHANGE_RATE_IF_REMAIN_FUNCTION = n_remain_change_rbd / n_remain
))
print(next_function)
