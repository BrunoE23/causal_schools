suppressPackageStartupMessages({
  library(data.table)
})

# Measure school-to-school mobility for a selected detailed ID_IFP function.
# The default universe is main-position classroom teachers (ID_IFP == 1).

default_input_dir <- file.path(
  "C:/Users/brunem/Dropbox/causal_schools/data/raw/docentes_educacion"
)
default_output_dir <- file.path(
  getwd(), "output", "tables", "docentes_educacion"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 3L) {
  stop(
    "Usage: Rscript 02_report_teacher_school_mobility.R ",
    "[INPUT_DIR] [OUTPUT_DIR] [ID_IFP]",
    call. = FALSE
  )
}

input_dir <- if (length(args) >= 1L) args[[1L]] else default_input_dir
output_dir <- if (length(args) >= 2L) args[[2L]] else default_output_dir
id_ifp_code <- if (length(args) >= 3L) as.integer(args[[3L]]) else 1L
if (is.na(id_ifp_code)) stop("ID_IFP must be an integer.", call. = FALSE)

function_meta <- switch(
  as.character(id_ifp_code),
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
    slug = paste0("id_ifp_", id_ifp_code),
    singular = paste0("person with ID_IFP == ", id_ifp_code),
    plural = paste0("people with ID_IFP == ", id_ifp_code)
  )
)
input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

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

read_function_panel <- function(path) {
  header <- names(fread(
    path, sep = ";", nrows = 0L, encoding = "Latin-1", showProgress = FALSE
  ))
  required <- c("AGNO", "RBD", "MRUN", "PERSONAS", "ID_IFP")
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
    PERSONAS == 1L & ID_IFP == id_ifp_code &
      !is.na(AGNO) & !is.na(RBD) & !is.na(MRUN) & MRUN != "",
    .(AGNO, RBD, MRUN)
  ]

  duplicates <- dt[, .N, by = .(AGNO, MRUN)][N > 1L]
  if (nrow(duplicates)) {
    stop(
      "PERSONAS == 1 does not uniquely identify person-years in ",
      basename(path), call. = FALSE
    )
  }
  dt
}

message(
  "Reading ", length(csv_files), " annual files for ",
  function_meta$plural, " (ID_IFP == ", id_ifp_code, ")..."
)
panel <- rbindlist(lapply(csv_files, function(path) {
  message("  ", basename(path))
  read_function_panel(path)
}))
setorder(panel, MRUN, AGNO)

years <- sort(unique(panel$AGNO))
if (length(years) < 2L) {
  stop("At least two years are required for a mobility report.", call. = FALSE)
}

annual_counts <- panel[, .(
  N_PEOPLE = .N,
  N_SCHOOLS = uniqueN(RBD)
), by = AGNO][order(AGNO)]

adjacent_mobility <- rbindlist(lapply(years[-1L], function(to_year) {
  from_year <- to_year - 1L
  from <- panel[AGNO == from_year, .(MRUN, RBD_FROM = RBD)]
  to <- panel[AGNO == to_year, .(MRUN, RBD_TO = RBD)]
  matched <- merge(from, to, by = "MRUN", all = FALSE)

  n_from <- nrow(from)
  n_to <- nrow(to)
  n_matched <- nrow(matched)
  n_moved <- matched[, sum(RBD_FROM != RBD_TO)]

  data.table(
    FROM_YEAR = from_year,
    TO_YEAR = to_year,
    N_FROM = n_from,
    N_TO = n_to,
    N_OBSERVED_BOTH = n_matched,
    N_SAME_RBD = n_matched - n_moved,
    N_MOVED_RBD = n_moved,
    MOVE_RATE_AMONG_OBSERVED_BOTH = n_moved / n_matched,
    RETENTION_RATE = n_matched / n_from,
    N_NOT_OBSERVED_NEXT_YEAR = n_from - n_matched,
    N_NEWLY_OBSERVED = n_to - n_matched
  )
}))

person_summary <- panel[, .(
  N_YEARS = uniqueN(AGNO),
  N_SCHOOLS = uniqueN(RBD),
  FIRST_YEAR = min(AGNO),
  LAST_YEAR = max(AGNO)
), by = MRUN]

transition_panel <- panel[, .(
  FROM_YEAR = shift(AGNO),
  TO_YEAR = AGNO,
  RBD_FROM = shift(RBD),
  RBD_TO = RBD
), by = MRUN]
consecutive_transitions <- transition_panel[
  !is.na(FROM_YEAR) & TO_YEAR == FROM_YEAR + 1L
]

n_unique_teachers <- nrow(person_summary)
n_multi_year <- person_summary[N_YEARS >= 2L, .N]
n_ever_moved <- person_summary[N_YEARS >= 2L & N_SCHOOLS >= 2L, .N]
n_consecutive_transitions <- nrow(consecutive_transitions)
n_consecutive_moves <- consecutive_transitions[, sum(RBD_FROM != RBD_TO)]

overall_summary <- data.table(
  METRIC = c(
    paste0(function_meta$singular, "-year observations"),
    paste0("Unique ", function_meta$plural),
    paste0(function_meta$plural, " observed in one year only"),
    paste0(function_meta$plural, " observed in at least two years"),
    paste0("Multi-year ", function_meta$plural, " observed at multiple schools"),
    paste0("Share of multi-year ", function_meta$plural, " observed at multiple schools"),
    paste0("Consecutive-year ", function_meta$singular, " transitions"),
    "Consecutive-year school changes",
    "School-change rate across consecutive-year transitions"
  ),
  VALUE = c(
    nrow(panel),
    n_unique_teachers,
    person_summary[N_YEARS == 1L, .N],
    n_multi_year,
    n_ever_moved,
    n_ever_moved / n_multi_year,
    n_consecutive_transitions,
    n_consecutive_moves,
    n_consecutive_moves / n_consecutive_transitions
  )
)

school_count_distribution <- person_summary[, .(
  N_PEOPLE = .N
), by = .(
  NUMBER_OF_SCHOOLS = fifelse(N_SCHOOLS >= 4L, "4+", as.character(N_SCHOOLS))
)][order(factor(NUMBER_OF_SCHOOLS, levels = c("1", "2", "3", "4+")))]
school_count_distribution[, SHARE := N_PEOPLE / sum(N_PEOPLE)]

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digits = 1L) sprintf(paste0("%.", digits, "f%%"), 100 * x)

year_rows <- vapply(seq_len(nrow(adjacent_mobility)), function(i) {
  x <- adjacent_mobility[i]
  paste0(
    "| ", x$FROM_YEAR, "--", x$TO_YEAR,
    " | ", fmt_n(x$N_OBSERVED_BOTH),
    " | ", fmt_n(x$N_MOVED_RBD),
    " | ", fmt_pct(x$MOVE_RATE_AMONG_OBSERVED_BOTH),
    " | ", fmt_pct(x$RETENTION_RATE), " |"
  )
}, character(1))

report_lines <- c(
  paste0(
    "# ", tools::toTitleCase(function_meta$singular),
    " movement across schools, 2018--2025"
  ),
  "",
  paste0(
    "Universe: annual records satisfying `PERSONAS == 1` and `ID_IFP == ",
    id_ifp_code, "` (main-position ", function_meta$plural,
    "). A move is a change in `RBD`."
  ),
  "",
  "## Main findings",
  "",
  paste0(
    "- The panel contains ", fmt_n(nrow(panel)), " ",
    function_meta$singular, "-year observations for ",
    fmt_n(n_unique_teachers), " unique ", function_meta$plural, "."
  ),
  paste0(
    "- Among ", fmt_n(n_multi_year), " ", function_meta$plural,
    " observed in at least two ",
    "years, ", fmt_n(n_ever_moved), " (", fmt_pct(n_ever_moved / n_multi_year),
    ") are observed at more than one RBD during 2018--2025."
  ),
  paste0(
    "- Across ", fmt_n(n_consecutive_transitions), " consecutive-year ",
    function_meta$singular, " transitions, ", fmt_n(n_consecutive_moves), " (",
    fmt_pct(n_consecutive_moves / n_consecutive_transitions),
    ") involve a change in RBD."
  ),
  "",
  paste0(
    "The ever-moved measure includes changes separated by gaps in observation. ",
    "The annual rates below require the person to be observed in the selected ",
    "function in both adjacent years."
  ),
  "",
  "## Consecutive-year movement",
  "",
  paste0(
    "| Years | ", tools::toTitleCase(function_meta$plural),
    " observed in both | Changed RBD | Move rate | Retained from first year |"
  ),
  "|---|---:|---:|---:|---:|",
  year_rows,
  "",
  paste0(
    "`Not observed next year` and `newly observed` should not be interpreted ",
    "as literal exits and entries: people can change function or return after ",
    "a gap."
  ),
  ""
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
file_prefix <- paste0(function_meta$slug, "_school_mobility")
annual_path <- file.path(output_dir, paste0(file_prefix, "_annual_counts.csv"))
adjacent_path <- file.path(output_dir, paste0(file_prefix, "_adjacent_year.csv"))
overall_path <- file.path(output_dir, paste0(file_prefix, "_overall.csv"))
distribution_path <- file.path(
  output_dir, paste0(file_prefix, "_number_of_schools.csv")
)
report_path <- file.path(output_dir, paste0(file_prefix, "_report.md"))

fwrite(annual_counts, annual_path)
fwrite(adjacent_mobility, adjacent_path)
fwrite(overall_summary, overall_path)
fwrite(school_count_distribution, distribution_path)
writeLines(report_lines, report_path, useBytes = TRUE)

message("Wrote annual counts: ", annual_path)
message("Wrote adjacent-year mobility: ", adjacent_path)
message("Wrote overall summary: ", overall_path)
message("Wrote school-count distribution: ", distribution_path)
message("Wrote report: ", report_path)

print(adjacent_mobility)
print(overall_summary)
