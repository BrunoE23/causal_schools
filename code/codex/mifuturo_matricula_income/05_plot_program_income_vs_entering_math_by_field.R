###############################################################################
# Program projected income vs average entrance-exam math score
#
# Each point is a COD_SIES program. Entrance-exam math scores are standardized
# within test year before they are averaged across the program's observed
# 2022--2025 entering cohorts. Field labels match the existing generic-program
# plot, including separate Medicine + and Other Healthcare categories.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(jsonlite)
  library(scales)
})

find_existing_path <- function(env_var, candidates, label, must_be_dir = TRUE) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates)]
  if (must_be_dir) {
    candidates <- candidates[dir.exists(candidates)]
  } else {
    candidates <- candidates[file.exists(candidates)]
  }
  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Set ", env_var, " or update candidates.")
  }
  candidates[[1]]
}

z_within_group <- function(x) {
  sigma <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sigma) || sigma == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / sigma
}

first_nonmissing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) NA_character_ else x[[1]]
}

modal_nonmissing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(NA_character_)
  counts <- sort(table(x), decreasing = TRUE)
  names(counts)[[1]]
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

presentation_field <- function(field, area_label) {
  field <- as.character(field)
  area_key <- normalize_text(area_label)
  other_healthcare_from_medicine <- c(
    "KINESIOLOGIA",
    "TERAPIA OCUPACIONAL",
    "NUTRICION Y DIETETICA",
    "TECNICO EN NUTRICION Y DIETETICA",
    "TECNICO EN FARMACIA",
    "FONOAUDIOLOGIA",
    "TECNICO EN RADIOLOGIA Y RADIOTERAPIA",
    "TECNICO EN ENFERMERIA",
    "TECNICO EN LABORATORIO CLINICO",
    "TECNICO LABORATORISTA DENTAL",
    "TECNICO EN MASOTERAPIA",
    "TECNICO EN PODOLOGIA",
    "TECNICO DENTAL Y ASISTENTE DE ODONTOLOGIA",
    "BACHILLERATO Y O LICENCIATURA EN SALUD",
    "NATUROPATIA",
    "TECNICO EN OPTICA",
    "TECNICO EN TERAPIAS NATURALES Y NATUROPATIA"
  )

  out <- field
  out[field == "Medicine"] <- "Medicine +"
  out[field == "Health and Welfare"] <- "Other Healthcare"
  out[area_key %in% other_healthcare_from_medicine] <- "Other Healthcare"
  out[out == "Engineering, Manufacturing and Construction"] <- "Engineering & Construction"
  out[out == "Humanities and Arts"] <- "Humanities & Arts"
  out
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

universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")
income_path <- file.path(
  repo_wd,
  "output",
  "tables",
  "mifuturo_matricula_income",
  "mifuturo_person_level_income_outcomes.csv"
)
figure_dir <- file.path(repo_wd, "output", "figures", "mifuturo_matricula_income")
table_dir <- file.path(repo_wd, "output", "tables", "mifuturo_matricula_income")
template_path <- file.path(
  repo_wd,
  "code",
  "codex",
  "mifuturo_matricula_income",
  "program-income-math-score-template.html"
)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(figure_dir, "program_income_vs_entering_math_by_field.png")
table_path <- file.path(table_dir, "program_income_vs_entering_math_by_field.csv")
summary_path <- file.path(table_dir, "program_income_vs_entering_math_by_field_summary.csv")
viz_path <- Sys.getenv("PROGRAM_INCOME_MATH_VIZ_PATH", unset = "")
min_entrants <- as.integer(Sys.getenv("PROGRAM_MATH_MIN_ENTRANTS", unset = "10"))

message("Reading and standardizing math scores: ", universe_path)
scores <- fread(
  universe_path,
  select = c("mrun", "psu_year", "math_max"),
  na.strings = c("", "NA")
)
scores[, mrun := as.numeric(mrun)]
scores <- scores[!is.na(psu_year) & is.finite(math_max) & math_max > 0]
scores[, math_score_z := z_within_group(math_max), by = psu_year]
scores <- scores[is.finite(math_score_z)]
if (anyDuplicated(scores$mrun) > 0) {
  stop("Score file is not unique by mrun.", call. = FALSE)
}

message("Reading matriculation and program-income columns: ", income_path)
income <- fread(
  income_path,
  select = c(
    "mrun",
    "COD_SIES_m1",
    "NOMB_CARRERA_m1",
    "NOMB_INST_m1",
    "AREA_CARRERA_GENERICA_m1",
    "field_reclassified_m1",
    "matriculated_m1",
    "program_income_full_clp_m1"
  ),
  na.strings = c("", "NA")
)
income[, mrun := as.numeric(mrun)]
income <- income[
  matriculated_m1 %in% c(TRUE, 1L) &
    !is.na(COD_SIES_m1) & nzchar(trimws(COD_SIES_m1)) &
    is.finite(program_income_full_clp_m1)
]
if (anyDuplicated(income$mrun) > 0) {
  stop("Program-income file is not unique by mrun.", call. = FALSE)
}

dt <- merge(scores, income, by = "mrun", all = FALSE, sort = FALSE)
dt[, field_group := presentation_field(field_reclassified_m1, AREA_CARRERA_GENERICA_m1)]
dt <- dt[!is.na(field_group)]

program_dt <- dt[
  ,
  .(
    program_name = first_nonmissing(NOMB_CARRERA_m1),
    institution_name = first_nonmissing(NOMB_INST_m1),
    field_group = modal_nonmissing(field_group),
    avg_math_score_z = mean(math_score_z),
    projected_income_clp = stats::median(program_income_full_clp_m1),
    n_entrants = .N,
    n_entry_years = uniqueN(psu_year),
    first_entry_year = min(psu_year),
    last_entry_year = max(psu_year)
  ),
  by = .(program_code = COD_SIES_m1)
]
program_dt <- program_dt[
  n_entrants >= min_entrants &
    is.finite(avg_math_score_z) &
    is.finite(projected_income_clp)
]
program_dt[, projected_income_millions := projected_income_clp / 1e6]

field_order <- c(
  "Medicine +",
  "Engineering & Construction",
  "Science",
  "Law",
  "Other Healthcare",
  "Business",
  "Social Sciences",
  "Humanities & Arts",
  "Teaching"
)
program_dt[, field_group := factor(field_group, levels = field_order)]
program_dt <- program_dt[!is.na(field_group)]
setorder(program_dt, field_group, avg_math_score_z, projected_income_millions)

if (nrow(program_dt) < 3) {
  stop("Too few programs remain to plot.", call. = FALSE)
}

fit <- stats::lm(projected_income_millions ~ avg_math_score_z, data = program_dt)
correlation <- stats::cor(program_dt$avg_math_score_z, program_dt$projected_income_millions)
summary_dt <- data.table(
  n_programs = nrow(program_dt),
  n_entrants = sum(program_dt$n_entrants),
  min_entrants_per_program = min_entrants,
  correlation = correlation,
  slope_million_clp_per_sd = unname(stats::coef(fit)[["avg_math_score_z"]]),
  min_entry_year = min(program_dt$first_entry_year),
  max_entry_year = max(program_dt$last_entry_year),
  n_field_groups = uniqueN(program_dt$field_group)
)

message("Writing table: ", table_path)
fwrite(program_dt, table_path)
fwrite(summary_dt, summary_path)

palette <- c(
  "Engineering & Construction" = "#1B4D89",
  "Medicine +" = "#B6422E",
  "Other Healthcare" = "#C46B91",
  "Business" = "#D18B00",
  "Science" = "#4C7A3D",
  "Teaching" = "#7B4FA3",
  "Social Sciences" = "#2A7F8E",
  "Humanities & Arts" = "#B75D8D",
  "Law" = "#6B625B"
)

plot <- ggplot(
  program_dt,
  aes(
    x = avg_math_score_z,
    y = projected_income_millions,
    color = field_group
  )
) +
  geom_point(alpha = 0.56, size = 1.65) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "grey28",
    linewidth = 0.75
  ) +
  scale_color_manual(values = palette, drop = FALSE) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Projected Program Income and Entrant Math Achievement",
    subtitle = "Each point is a program; 2022--2025 entering cohorts combined",
    x = "Average entrance-exam math score (SD within test year)",
    y = "Projected monthly income (millions of CLP)",
    color = NULL,
    caption = paste0(
      "Programs require at least ", min_entrants,
      " observed entrants. Health categories follow the Medicine + / Other Healthcare split used in the field-premium plot."
    )
  ) +
  theme_minimal(base_family = "serif", base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey35"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8.5),
    plot.caption = element_text(hjust = 0, color = "grey40", size = 8.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

message("Writing figure: ", figure_path)
ggsave(figure_path, plot, width = 9.2, height = 5.9, dpi = 300)

if (nzchar(viz_path)) {
  if (!file.exists(template_path)) {
    stop("Missing visualization template: ", template_path, call. = FALSE)
  }
  html <- paste(readLines(template_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  viz_dt <- program_dt[
    ,
    .(
      code = program_code,
      program = program_name,
      institution = institution_name,
      field = as.character(field_group),
      x = round(avg_math_score_z, 4),
      y = round(projected_income_millions, 4),
      n = n_entrants,
      years = paste0(first_entry_year, "--", last_entry_year)
    )
  ]
  html <- sub(
    "__PROGRAM_DATA_JSON__",
    toJSON(viz_dt, dataframe = "rows", na = "null", auto_unbox = TRUE),
    html,
    fixed = TRUE
  )
  html <- sub(
    "__SUMMARY_JSON__",
    toJSON(summary_dt, dataframe = "rows", na = "null", auto_unbox = TRUE),
    html,
    fixed = TRUE
  )
  dir.create(dirname(viz_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(html), viz_path, useBytes = TRUE)
  message("Writing inline visualization: ", viz_path)
}

print(summary_dt)
