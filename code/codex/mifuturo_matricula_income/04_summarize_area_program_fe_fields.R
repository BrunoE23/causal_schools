###############################################################################
# Summarize AREA_CARRERA_GENERICA program fixed effects and field classifications
#
# Purpose:
# - Audit the actual values of field_reclassified_m1.
# - Rank AREA_CARRERA_GENERICA by the career/program FE from the MiFuturo
#   institution + AREA_CARRERA_GENERICA regression.
# - Attach field_reclassified_m1 using the student-weighted modal field for each
#   area. Because field_reclassified_m1 is categorical, a literal weighted
#   average is not defined; the script therefore reports mean FE within each
#   field in two explicit ways:
#   (1) unweighted across AREA_CARRERA_GENERICA cells, and
#   (2) weighted by the number of matriculated students in each area.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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

repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(
    getwd(),
    "C:/Users/brunem/Research/causal_schools",
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
  ),
  "repo_wd"
)

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c(
    "C:/Users/brunem/Dropbox/causal_schools",
    "C:/Users/xd-br/Dropbox/causal_schools"
  ),
  "data_wd"
)

output_dir <- file.path(repo_wd, "output", "tables", "mifuturo_matricula_income")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path(repo_wd, "output", "figures", "mifuturo_matricula_income")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

person_path <- file.path(output_dir, "mifuturo_person_level_income_outcomes.csv")
effects_path <- file.path(output_dir, "mifuturo_income_fe_carrera_effects.csv")
universe_path <- file.path(data_wd, "data", "clean", "univ_gr8_df.csv")

field_values_path <- file.path(output_dir, "mifuturo_field_reclassified_values.csv")
top_bottom_path <- file.path(output_dir, "mifuturo_area_carrera_generica_program_fe_top_bottom_fields.csv")
top_bottom_by_field_path <- file.path(
  output_dir,
  "mifuturo_area_carrera_generica_program_fe_top_bottom_by_field.csv"
)
top_bottom_by_field_compact_path <- file.path(
  output_dir,
  "mifuturo_area_carrera_generica_program_fe_top_bottom_by_field_compact.csv"
)
field_weighted_path <- file.path(output_dir, "mifuturo_field_reclassified_program_fe_weighted_average.csv")
field_means_path <- file.path(output_dir, "mifuturo_field_reclassified_program_fe_means.csv")
field_scatter_path <- file.path(figure_dir, "area_carrera_generica_program_fe_by_field.png")

required_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
  path
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & trimws(as.character(x)) != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

weighted_modal_field <- function(dt) {
  field_counts <- dt[
    !is.na(field_reclassified_m1) & field_reclassified_m1 != "",
    .(n_students_field = .N),
    by = .(area_key, field_reclassified_m1)
  ]

  if (nrow(field_counts) == 0) {
    return(data.table())
  }

  field_totals <- field_counts[
    ,
    .(n_students_with_field = sum(n_students_field)),
    by = area_key
  ]
  field_counts <- merge(field_counts, field_totals, by = "area_key", all.x = TRUE)
  field_counts[, field_share_within_area := n_students_field / n_students_with_field]
  setorder(field_counts, area_key, -n_students_field, field_reclassified_m1)

  dominant <- field_counts[
    ,
    .SD[1],
    by = area_key
  ][
    ,
    .(
      area_key,
      dominant_field_reclassified_m1 = field_reclassified_m1,
      dominant_field_n_students = n_students_field,
      dominant_field_share_within_area = field_share_within_area
    )
  ]

  distributions <- field_counts[
    ,
    .(
      field_reclassified_distribution = paste0(
        field_reclassified_m1,
        " (",
        sprintf("%.1f%%", 100 * field_share_within_area),
        ")",
        collapse = " | "
      )
    ),
    by = area_key
  ]

  merge(dominant, distributions, by = "area_key", all.x = TRUE)
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
  out
}

message("Reading program/career FE effects: ", effects_path)
effects <- fread(required_file(effects_path), na.strings = c("", "NA"))
effects <- effects[
  effect_type == "carrera" & !is.na(level) & !is.na(effect_log_clp_centered),
  .(
    area_key = normalize_text(level),
    area_carrera_generica_fe_label = level,
    program_fe_log_clp_raw = effect_log_clp_raw,
    program_fe_log_clp_centered = effect_log_clp_centered
  )
]
effects[, program_fe_percent_vs_center := 100 * (exp(program_fe_log_clp_centered) - 1)]

message("Reading person-level MiFuturo outcomes: ", person_path)
person_cols <- c(
  "MRUN",
  "matriculated_m1",
  "AREA_CARRERA_GENERICA_m1",
  "mifuturo_area_income_hat_clp_m1",
  "mifuturo_area_log_income_hat_clp_m1",
  "mifuturo_area_estimable_m1",
  "program_income_area_source_m1"
)
person_header <- names(fread(required_file(person_path), nrows = 0))
missing_person_cols <- setdiff(person_cols, person_header)
if (length(missing_person_cols) > 0) {
  stop(
    "Person-level income file is missing expected columns: ",
    paste(missing_person_cols, collapse = ", "),
    call. = FALSE
  )
}
person <- fread(required_file(person_path), select = person_cols, na.strings = c("", "NA"))

message("Reading universe field classifications: ", universe_path)
universe <- fread(
  required_file(universe_path),
  select = c("MRUN", "field_reclassified_m1"),
  na.strings = c("", "NA")
)

person[, MRUN := as.character(MRUN)]
universe[, MRUN := as.character(MRUN)]
dt <- merge(person, universe, by = "MRUN", all.x = TRUE, sort = FALSE)

dt[, matriculated_m1 := matriculated_m1 %in% TRUE]
dt[, area_key := normalize_text(AREA_CARRERA_GENERICA_m1)]
dt[area_key == "", area_key := NA_character_]

field_values <- dt[
  ,
  .(
    n_students = .N,
    n_matriculated_m1 = sum(matriculated_m1, na.rm = TRUE),
    n_matriculated_with_area = sum(matriculated_m1 & !is.na(area_key), na.rm = TRUE)
  ),
  by = .(field_reclassified_m1)
]
field_values[is.na(field_reclassified_m1), field_reclassified_m1 := "(missing)"]
field_values[, share_students := n_students / sum(n_students)]
field_values[, share_matriculated_m1 := n_matriculated_m1 / sum(n_matriculated_m1)]
setorder(field_values, -n_matriculated_m1, field_reclassified_m1)

matriculated_area <- dt[matriculated_m1 & !is.na(area_key)]

area_student_summary <- matriculated_area[
  ,
  .(
    n_matriculated_students = .N,
    area_carrera_generica_student_label = first_nonmissing(AREA_CARRERA_GENERICA_m1),
    n_area_fe_estimable_students = sum(mifuturo_area_estimable_m1 %in% TRUE, na.rm = TRUE),
    area_only_income_hat_clp_weighted = mean(mifuturo_area_income_hat_clp_m1, na.rm = TRUE),
    area_only_log_income_hat_clp_weighted = mean(mifuturo_area_log_income_hat_clp_m1, na.rm = TRUE)
  ),
  by = area_key
]

area_fields <- weighted_modal_field(matriculated_area)

area_summary <- merge(effects, area_student_summary, by = "area_key", all.x = TRUE, sort = FALSE)
area_summary <- merge(area_summary, area_fields, by = "area_key", all.x = TRUE, sort = FALSE)
area_summary[
  ,
  field_reclassified_presentation := presentation_field(
    dominant_field_reclassified_m1,
    area_carrera_generica_fe_label
  )
]

field_weighted <- area_summary[
  !is.na(field_reclassified_presentation) &
    !is.na(program_fe_log_clp_centered) &
    !is.na(n_matriculated_students) &
    n_matriculated_students > 0,
  .(
    raw_field_reclassified_components = paste(
      sort(unique(dominant_field_reclassified_m1)),
      collapse = " | "
    ),
    n_area_carrera_generica = uniqueN(area_key),
    n_matriculated_students = sum(n_matriculated_students),
    unweighted_avg_program_fe_log_clp_centered = mean(
      program_fe_log_clp_centered,
      na.rm = TRUE
    ),
    weighted_avg_program_fe_log_clp_centered = weighted.mean(
      program_fe_log_clp_centered,
      w = n_matriculated_students,
      na.rm = TRUE
    ),
    unweighted_avg_area_only_income_hat_clp = mean(
      area_only_income_hat_clp_weighted,
      na.rm = TRUE
    ),
    weighted_avg_area_only_income_hat_clp = weighted.mean(
      area_only_income_hat_clp_weighted,
      w = n_matriculated_students,
      na.rm = TRUE
    )
  ),
  by = .(field_reclassified_m1 = field_reclassified_presentation)
]
field_weighted[
  ,
  unweighted_avg_program_fe_percent_vs_center :=
    100 * (exp(unweighted_avg_program_fe_log_clp_centered) - 1)
]
field_weighted[
  ,
  weighted_avg_program_fe_percent_vs_center :=
    100 * (exp(weighted_avg_program_fe_log_clp_centered) - 1)
]
setorder(field_weighted, weighted_avg_program_fe_log_clp_centered)

area_summary <- merge(
  area_summary,
  field_weighted[
    ,
    .(
      field_reclassified_presentation = field_reclassified_m1,
      presentation_field_weighted_avg_program_fe_log_clp_centered =
        weighted_avg_program_fe_log_clp_centered,
      presentation_field_weighted_avg_program_fe_percent_vs_center =
        weighted_avg_program_fe_percent_vs_center
    )
  ],
  by = "field_reclassified_presentation",
  all.x = TRUE,
  sort = FALSE
)

ranked <- area_summary[!is.na(program_fe_log_clp_centered)]
setorder(ranked, -program_fe_log_clp_centered, area_carrera_generica_fe_label)
ranked[, rank_program_fe_desc := seq_len(.N)]
setorder(ranked, program_fe_log_clp_centered, area_carrera_generica_fe_label)
ranked[, rank_program_fe_asc := seq_len(.N)]

top3 <- ranked[rank_program_fe_desc <= 3]
top3[, rank_group := "top_3"]
top3[, rank_within_group := rank_program_fe_desc]

bottom3 <- ranked[rank_program_fe_asc <= 3]
bottom3[, rank_group := "bottom_3"]
bottom3[, rank_within_group := rank_program_fe_asc]

top_bottom <- rbindlist(list(top3, bottom3), use.names = TRUE, fill = TRUE)
top_bottom[, rank_group_order := fifelse(rank_group == "top_3", 1L, 2L)]
setorder(top_bottom, rank_group_order, rank_within_group)
top_bottom <- top_bottom[
  ,
  .(
    rank_group,
    rank_within_group,
    rank_program_fe_desc,
    area_carrera_generica = area_carrera_generica_fe_label,
    program_fe_log_clp_centered,
    program_fe_percent_vs_center,
    program_fe_log_clp_raw,
    n_matriculated_students,
    n_area_fe_estimable_students,
    area_only_income_hat_clp_weighted,
    area_only_log_income_hat_clp_weighted,
    field_reclassified_raw = dominant_field_reclassified_m1,
    field_reclassified_presentation,
    dominant_field_n_students,
    dominant_field_share_within_area,
    presentation_field_weighted_avg_program_fe_log_clp_centered,
    presentation_field_weighted_avg_program_fe_percent_vs_center,
    field_reclassified_distribution
  )
]

ranked_by_field <- area_summary[
  !is.na(field_reclassified_presentation) &
    !is.na(program_fe_log_clp_centered)
]
ranked_by_field[
  ,
  n_area_carrera_generica_in_field := .N,
  by = field_reclassified_presentation
]
setorder(
  ranked_by_field,
  field_reclassified_presentation,
  -program_fe_log_clp_centered,
  area_carrera_generica_fe_label
)
ranked_by_field[
  ,
  rank_program_fe_within_field_desc := seq_len(.N),
  by = field_reclassified_presentation
]
setorder(
  ranked_by_field,
  field_reclassified_presentation,
  program_fe_log_clp_centered,
  area_carrera_generica_fe_label
)
ranked_by_field[
  ,
  rank_program_fe_within_field_asc := seq_len(.N),
  by = field_reclassified_presentation
]

top_by_field <- ranked_by_field[rank_program_fe_within_field_desc <= 3]
top_by_field[, rank_group := "top_3_within_field"]
top_by_field[, rank_within_field := rank_program_fe_within_field_desc]

bottom_by_field <- ranked_by_field[rank_program_fe_within_field_asc <= 3]
bottom_by_field[, rank_group := "bottom_3_within_field"]
bottom_by_field[, rank_within_field := rank_program_fe_within_field_asc]

top_bottom_by_field <- rbindlist(
  list(top_by_field, bottom_by_field),
  use.names = TRUE,
  fill = TRUE
)
top_bottom_by_field[
  ,
  rank_group_order := fifelse(rank_group == "top_3_within_field", 1L, 2L)
]
setorder(
  top_bottom_by_field,
  field_reclassified_presentation,
  rank_group_order,
  rank_within_field
)
top_bottom_by_field <- top_bottom_by_field[
  ,
  .(
    field_reclassified_m1 = field_reclassified_presentation,
    raw_field_reclassified_m1 = dominant_field_reclassified_m1,
    n_area_carrera_generica_in_field,
    rank_group,
    rank_within_field,
    area_carrera_generica = area_carrera_generica_fe_label,
    program_fe_log_clp_centered,
    program_fe_percent_vs_center,
    program_fe_log_clp_raw,
    n_matriculated_students,
    n_area_fe_estimable_students,
    area_only_income_hat_clp_weighted,
    area_only_log_income_hat_clp_weighted,
    dominant_field_n_students,
    dominant_field_share_within_area,
    field_reclassified_distribution
  )
]

format_area_list <- function(rank, area, pct) {
  paste0(
    rank,
    ". ",
    area,
    " (",
    sprintf("%+.1f%%", pct),
    ")",
    collapse = "; "
  )
}

top_by_field_compact <- top_bottom_by_field[
  rank_group == "top_3_within_field",
  .(
    n_area_carrera_generica_in_field = n_area_carrera_generica_in_field[1],
    top_areas = format_area_list(
      rank_within_field,
      area_carrera_generica,
      program_fe_percent_vs_center
    )
  ),
  by = field_reclassified_m1
]

bottom_by_field_compact <- top_bottom_by_field[
  rank_group == "bottom_3_within_field",
  .(
    bottom_areas = format_area_list(
      rank_within_field,
      area_carrera_generica,
      program_fe_percent_vs_center
    )
  ),
  by = field_reclassified_m1
]

top_bottom_by_field_compact <- merge(
  top_by_field_compact,
  bottom_by_field_compact,
  by = "field_reclassified_m1",
  all = TRUE,
  sort = FALSE
)
top_bottom_by_field_compact <- merge(
  field_weighted[
    ,
    .(
      field_reclassified_m1,
      field_unweighted_avg_program_fe_log_clp_centered =
        unweighted_avg_program_fe_log_clp_centered,
      field_unweighted_avg_program_fe_percent_vs_center =
        unweighted_avg_program_fe_percent_vs_center,
      field_weighted_avg_program_fe_log_clp_centered =
        weighted_avg_program_fe_log_clp_centered,
      field_weighted_avg_program_fe_percent_vs_center =
        weighted_avg_program_fe_percent_vs_center
    )
  ],
  top_bottom_by_field_compact,
  by = "field_reclassified_m1",
  all.y = TRUE,
  sort = FALSE
)
setorder(
  top_bottom_by_field_compact,
  field_weighted_avg_program_fe_log_clp_centered,
  field_reclassified_m1
)

field_order <- field_weighted[
  order(weighted_avg_program_fe_log_clp_centered),
  field_reclassified_m1
]

plot_dt <- area_summary[
  !is.na(field_reclassified_presentation) &
    !is.na(program_fe_log_clp_centered) &
    !is.na(n_matriculated_students)
]
plot_dt[
  ,
  field_reclassified_m1 := factor(
    field_reclassified_presentation,
    levels = field_order
  )
]

means_plot_dt <- copy(field_weighted)
means_plot_dt[
  ,
  field_reclassified_m1 := factor(field_reclassified_m1, levels = field_order)
]

wrap_axis_label <- function(x, width = 18) {
  vapply(
    x,
    function(label) paste(strwrap(label, width = width), collapse = "\n"),
    character(1)
  )
}

field_scatter <- ggplot(
  plot_dt,
  aes(
    x = field_reclassified_m1,
    y = program_fe_log_clp_centered
  )
) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_jitter(
    aes(size = n_matriculated_students),
    width = 0.18,
    height = 0,
    alpha = 0.58,
    color = "#2563eb"
  ) +
  geom_point(
    data = means_plot_dt,
    aes(
      x = field_reclassified_m1,
      y = unweighted_avg_program_fe_log_clp_centered
    ),
    inherit.aes = FALSE,
    shape = 23,
    fill = "white",
    color = "black",
    size = 3,
    stroke = 0.8
  ) +
  geom_point(
    data = means_plot_dt,
    aes(
      x = field_reclassified_m1,
      y = weighted_avg_program_fe_log_clp_centered
    ),
    inherit.aes = FALSE,
    shape = 21,
    fill = "#dc2626",
    color = "white",
    size = 3,
    stroke = 0.4
  ) +
  scale_x_discrete(labels = wrap_axis_label) +
  scale_size_continuous(
    name = "Matriculated students",
    range = c(1.6, 6),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) +
  labs(
    title = "Area Carrera Generica FE by Field",
    subtitle = "Blue points are areas; white diamonds are unweighted field means; red points are student-weighted field means",
    x = "Field reclassified",
    y = "Centered career FE on log income"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 8.5, lineheight = 0.95),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  field_scatter_path,
  field_scatter,
  width = 10.5,
  height = 6.6,
  dpi = 300
)

fwrite(field_values, field_values_path)
fwrite(top_bottom, top_bottom_path)
fwrite(top_bottom_by_field, top_bottom_by_field_path)
fwrite(top_bottom_by_field_compact, top_bottom_by_field_compact_path)
fwrite(field_weighted, field_weighted_path)
fwrite(field_weighted, field_means_path)

message("Wrote: ", field_values_path)
message("Wrote: ", top_bottom_path)
message("Wrote: ", top_bottom_by_field_path)
message("Wrote: ", top_bottom_by_field_compact_path)
message("Wrote: ", field_weighted_path)
message("Wrote: ", field_means_path)
message("Wrote: ", field_scatter_path)

print(field_values)
print(top_bottom)
print(top_bottom_by_field)
print(top_bottom_by_field_compact)
print(field_weighted)
