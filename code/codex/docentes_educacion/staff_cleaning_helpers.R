# Pure helpers shared by the cleaner and its synthetic regression tests.
# Codebook: MINEDUC, Esquema de registro Cargos Docentes, pp. 3-6, 12-16.
# Keep observed characteristics separate from unvalidated quality/PCA scores.

staff_history_start_year <- 2013L

# Annex V, pp. 19-20 of the annual codebooks (2013-2025): regular youth media.
# Adult media is distinct and outside the school VA cohort's teaching scope.
staff_hs_teaching_codes <- c(310L, 410L, 510L, 610L, 710L, 810L, 910L)
staff_teaching_levels <- c(
  "0" = 0L, "10" = 1L, "110" = 2L,
  setNames(rep(6L, 5L), c(160L, 161L, 163L, 165L, 167L)),
  setNames(rep(3L, 10L), c(211:219, 299L)),
  "310" = 4L, setNames(rep(5L, 6L), c(410L, 510L, 610L, 710L, 810L, 910L)),
  setNames(rep(7L, 3L), c(360L, 361L, 363L)),
  setNames(rep(8L, 17L), c(460L, 461L, 463L, 470L, 560L, 561L, 563L,
    660L, 661L, 663L, 760L, 761L, 763L, 860L, 861L, 863L, 963L)),
  setNames(rep(9L, 7L), 990:996)
)

staff_role_labels <- c(
  "0" = "sin_funcion_secundaria", "1" = "docente_de_aula",
  "2" = "planta_tecnico_pedagogica", "3" = "planta_directiva",
  "4" = "director", "5" = "otra_en_establecimiento",
  "6" = "otra_fuera_establecimiento", "7" = "jefe_unidad_tecnico_pedagogica",
  "8" = "inspector_general", "9" = "orientador", "10" = "directiva",
  "11" = "tecnico_pedagogica", "12" = "supervision", "13" = "jefe_daem",
  "14" = "jefe_corporacion_municipal", "15" = "subdirector",
  "16" = "profesor_encargado_establecimiento", "17" = "educador_tradicional"
)

staff_specialty_codes <- list(
  "11" = c(110L, 1101L, 1614:1617),
  "12" = c(120:127, 1208L, 1618:1620),
  "13" = c(130:134, 1305:1306, 1621:1626),
  "14" = c(140:149, 1410:1416, 1418:1419, 1627:1635),
  "15" = c(150:151, 1502:1503, 1636:1641),
  "16" = c(160:164, 1605:1606, 1613L, 1642:1645),
  "20" = 200L, "21" = 210L, "22" = 220L, "23" = 230L,
  "24" = 1646:1692, "25" = 1693:1703, "26" = 1704:1730,
  "30" = 300L, "31" = 310L, "32" = 320L, "33" = 330L
)

staff_raw_fields <- function() {
  c(
    "AGNO", "RBD", "NOMBRE_SLEP", "MRUN", "DOC_GENERO", "DOC_FEC_NAC",
    "COD_DEPE", "ESTADO_ESTAB", "PERSONAS", "ID_IFP", "ID_IFS",
    "TRAMO_CARR_DOCENTE", "BIENIOS_CARR_DOCENTE",
    "ANO_SERVICIO_SISTEMA", "ANO_SERVICIO_EE",
    "ID_ITC", "ID_ITC_CORR", "HORAS_CONTRATO", "HORAS_DIRECT",
    "HORAS_TEC_PED", "HORAS_AULA", "HORAS_DENTRO_ESTAB", "HORAS_FUERA_ESTAB",
    "ID_ICH", "NIVEL1", "NIVEL2", "SECTOR1", "SECTOR2",
    "SUBSECTOR1", "SUBSECTOR2", "HORAS1", "HORAS2", "COD_ENS_1", "COD_ENS_2",
    unlist(lapply(1:2, function(k) paste0(c(
      "TIT_ID_", "TIP_TIT_ID_", "ESP_ID_", "TIP_INSTI_ID_",
      "ANO_TITULACION_", "DURACION_CARRERA_", "MODALIDAD_ESTUDIO_",
      "MEN_SIN_MENCION_", "MEN_MATE_", "MEN_LENGUAJE_", "MEN_ORIENTACION_"
    ), k)), use.names = FALSE)
  )
}

staff_any_observed <- function(...) {
  # Evidence among reported slots, NOT proof about every unreported credential.
  m <- cbind(...)
  n <- rowSums(!is.na(m))
  ans <- as.integer(rowSums(m == 1L, na.rm = TRUE) > 0L)
  ans[n == 0L] <- NA_integer_
  ans
}

staff_role_flag <- function(code, target, secondary = FALSE) {
  valid <- code %in% if (secondary) 0:17 else 1:17
  data.table::fifelse(valid, as.integer(code == target), NA_integer_)
}

staff_or <- function(a, b) {
  # Three-valued OR: a known positive dominates unknown; 0 OR unknown is unknown.
  as.integer(as.logical(a) | as.logical(b))
}

staff_add_hs_assignments <- function(dt) {
  # Use the person's assignment at this RBD, not the school's offered levels,
  # the person's degree, ID_ICH, or an HS assignment at a different school.
  for (k in 1:2) {
    code <- dt[[paste0("COD_ENS_", k)]]
    expected_level <- unname(staff_teaching_levels[as.character(code)])
    level <- dt[[paste0("NIVEL", k)]]
    data.table::set(dt, j = paste0("HS_ASSIGNMENT_", k), value = data.table::fifelse(
      !is.na(expected_level), as.integer(code %in% staff_hs_teaching_codes), NA_integer_))
    data.table::set(dt, j = paste0("TEACHING_CODE_UNMAPPED_", k),
      value = as.integer(!is.na(code) & is.na(expected_level)))
    data.table::set(dt, j = paste0("TEACHING_LEVEL_MISMATCH_", k),
      value = data.table::fifelse(!is.na(expected_level) & level %in% 0:9,
        as.integer(expected_level != level), NA_integer_))
  }
  dt[, HS_ASSIGNMENT_ANY := staff_or(HS_ASSIGNMENT_1, HS_ASSIGNMENT_2)]
  for (scope in c("PRIMARY", "SECONDARY", "ANY", "SECONDARY_ONLY", "PRIMARY_NO_SECONDARY")) {
    data.table::set(dt, j = paste0("TEACHER_HS_", scope), value = as.integer(
      as.logical(dt[[paste0("TEACHER_", scope)]]) & as.logical(dt$HS_ASSIGNMENT_ANY)))
  }
  invisible(dt)
}

staff_add_credentials <- function(dt, mention_columns) {
  n <- nrow(dt)
  for (k in 1:2) {
    getk <- function(prefix) dt[[paste0(prefix, k)]]
    tit <- getk("TIT_ID_")
    tip <- getk("TIP_TIT_ID_")
    esp <- getk("ESP_ID_")
    inst <- getk("TIP_INSTI_ID_")
    title_known <- tit %in% 1:3
    has_title <- data.table::fifelse(title_known, as.integer(tit %in% 1:2), NA_integer_)
    teaching <- data.table::fifelse(title_known, as.integer(tit == 1L), NA_integer_)
    tertiary <- data.table::fcase(
      tit == 1L, 1L,
      tit == 2L & tip %in% 21:23, 1L,
      tit == 3L | (tit == 2L & tip %in% 24:26), 0L,
      default = NA_integer_
    )
    institution <- data.table::fifelse(
      tit %in% 1:2 & inst %in% 1:5, as.integer(inst), NA_integer_
    )
    # A zero institution code for a declared qualification is unknown/conflicting,
    # not "no degree". TIT_ID=0 means no information, not no qualification.
    for (field in c("HAS_TITLE", "TEACHING_TITLE", "TERTIARY_QUALIFICATION")) {
      value <- switch(field, HAS_TITLE = has_title, TEACHING_TITLE = teaching,
                      TERTIARY_QUALIFICATION = tertiary)
      data.table::set(dt, j = paste0(field, "_", k), value = value)
    }
    data.table::set(dt, j = paste0("INSTITUTION_TYPE_", k), value = institution)
    data.table::set(dt, j = paste0("TITLE_CODE_INVALID_", k),
                   value = as.integer(!is.na(tit) & !tit %in% 0:3))
    data.table::set(dt, j = paste0("TITLE_INSTITUTION_CONFLICT_", k),
                   value = as.integer((tit %in% 1:2 & !is.na(inst) & inst == 0L) |
                     (tit %in% c(0L, 3L) & inst %in% 1:5)))
    expected_tip <- data.table::fcase(
      tit == 1L, tip %in% 11:16, tit == 2L, tip %in% 20:26,
      tit == 3L, tip %in% 30:33, default = FALSE
    )
    specialty_valid <- rep(FALSE, n)
    for (type in names(staff_specialty_codes)) {
      specialty_valid <- specialty_valid |
        (tip %in% as.integer(type) & esp %in% staff_specialty_codes[[type]])
    }
    specialty_valid <- specialty_valid & expected_tip
    specialty <- data.table::fifelse(
      specialty_valid & tit %in% 1:2 & !tip %in% 20L,
      as.integer(!esp %in% c(110L, 120L, 130L, 140L, 150L, 160L,
                            210L, 220L, 230L, 1625L)),
      NA_integer_
    )
    math <- data.table::fifelse(
      specialty_valid & tit == 1L,
      as.integer((tip == 13L & esp == 1622L) | (tip == 14L & esp == 142L) |
                   (tip == 16L & esp == 1606L)), NA_integer_
    )
    language <- data.table::fifelse(
      specialty_valid & tit == 1L,
      as.integer((tip == 13L & esp == 1306L) | (tip == 14L & esp == 141L) |
                   (tip == 15L & esp == 1503L) | (tip == 16L & esp == 1605L)),
      NA_integer_
    )
    data.table::set(dt, j = paste0("SPECIALTY_CODE_UNMAPPED_", k),
                   value = as.integer(!is.na(esp) & esp != 0L & !specialty_valid))
    data.table::set(dt, j = paste0("HAS_SPECIALTY_", k), value = specialty)
    data.table::set(dt, j = paste0("HAS_MATH_TITLE_SPECIALTY_", k), value = math)
    data.table::set(dt, j = paste0("HAS_LANGUAGE_TITLE_SPECIALTY_", k), value = language)
    # Codebook p. 3: mentions were asked only for TIT_ID=1, TIP_TIT_ID=13.
    eligible <- data.table::fifelse(
      title_known & !is.na(tip), as.integer(tit == 1L & tip == 13L), NA_integer_
    )
    data.table::set(dt, j = paste0("MENTIONS_APPLICABLE_", k), value = eligible)
    mcols <- grep(paste0("_", k, "$"), mention_columns, value = TRUE)
    if (length(mcols)) {
      m <- as.matrix(dt[, ..mcols])
      positive <- rowSums(m == 1L, na.rm = TRUE)
    } else {
      m <- matrix(NA_integer_, nrow = n, ncol = 0L)
      positive <- integer(n)
    }
    valid_n <- if (length(mcols)) rowSums(m == 0L | m == 1L, na.rm = TRUE) else integer(n)
    complete_n <- data.table::fifelse(
      eligible == 1L & valid_n == length(mcols) & length(mcols) > 0L,
      as.integer(positive), NA_integer_
    )
    data.table::set(dt, j = paste0("N_MENCIONES_RECORDED_", k), value = as.integer(positive))
    data.table::set(dt, j = paste0("N_MENCIONES_VALID_FLAGS_", k), value = as.integer(valid_n))
    data.table::set(dt, j = paste0("N_MENCIONES_APPLICABLE_", k), value = complete_n)
    data.table::set(dt, j = paste0("MENTION_OUTSIDE_APPLICABILITY_", k),
                   value = as.integer(positive > 0L & (is.na(eligible) | eligible != 1L)))
    for (subject in c("MATE", "LENGUAJE", "ORIENTACION")) {
      raw <- getk(paste0("MEN_", subject, "_"))
      value <- data.table::fifelse(eligible == 1L & raw %in% 0:1,
                                   as.integer(raw), NA_integer_)
      data.table::set(dt, j = paste0("MENTION_", subject, "_APPLICABLE_", k), value = value)
    }
    graduation <- getk("ANO_TITULACION_")
    good_year <- has_title == 1L & !is.na(graduation) &
      graduation > 0L & graduation <= dt$AGNO
    data.table::set(dt, j = paste0("YEARS_SINCE_REPORTED_TITLE_", k),
                   value = data.table::fifelse(good_year, dt$AGNO - graduation, NA_integer_))
    data.table::set(dt, j = paste0("TITLE_YEAR_IN_FUTURE_", k),
                   value = as.integer(!is.na(graduation) & graduation > dt$AGNO))
    duration <- getk("DURACION_CARRERA_")
    data.table::set(dt, j = paste0("TITLE_DURATION_SEMESTERS_", k),
                   value = data.table::fifelse(has_title == 1L & duration > 0L,
                                              as.numeric(duration), NA_real_))
    modality <- getk("MODALIDAD_ESTUDIO_")
    data.table::set(dt, j = paste0("STUDY_MODALITY_", k),
                   value = data.table::fifelse(has_title == 1L & modality %in% 1:3,
                                              as.integer(modality), NA_integer_))
  }
  # Reported title slots are not assumed to be two distinct university degrees.
  dt[, N_REPORTED_TITLE_SLOTS := rowSums(cbind(HAS_TITLE_1, HAS_TITLE_2) == 1L, na.rm = TRUE)]
  dt[, N_TITLE_SLOTS_WITH_INFORMATION := as.integer(TIT_ID_1 %in% 1:3) + as.integer(TIT_ID_2 %in% 1:3)]
  for (field in c("HAS_TITLE", "TEACHING_TITLE", "TERTIARY_QUALIFICATION",
                  "HAS_SPECIALTY", "HAS_MATH_TITLE_SPECIALTY", "HAS_LANGUAGE_TITLE_SPECIALTY")) {
    data.table::set(dt, j = paste0(field, "_REPORTED"),
                   value = staff_any_observed(dt[[paste0(field, "_1")]], dt[[paste0(field, "_2")]]))
  }
  for (inst_type in 1:5) {
    indicators <- lapply(1:2, function(k) {
      code <- dt[[paste0("INSTITUTION_TYPE_", k)]]
      no_title <- dt[[paste0("HAS_TITLE_", k)]] == 0L
      data.table::fifelse(no_title, 0L,
        data.table::fifelse(!is.na(code), as.integer(code == inst_type), NA_integer_))
    })
    label <- c("UNIVERSITY", "CFT", "IP", "NORMAL_SCHOOL", "OTHER")[inst_type]
    data.table::set(dt, j = paste0("TRAINED_", label, "_REPORTED"),
                   value = staff_any_observed(indicators[[1L]], indicators[[2L]]))
  }
  for (subject in c("MATE", "LENGUAJE", "ORIENTACION")) {
    a <- dt[[paste0("MENTION_", subject, "_APPLICABLE_1")]]
    b <- dt[[paste0("MENTION_", subject, "_APPLICABLE_2")]]
    data.table::set(dt, j = paste0("MENTION_", subject, "_REPORTED"),
                   value = staff_any_observed(a, b))
  }
  university_degrees <- lapply(1:2, function(k) {
    tertiary <- dt[[paste0("TERTIARY_QUALIFICATION_", k)]]
    institution <- dt[[paste0("INSTITUTION_TYPE_", k)]]
    data.table::fifelse(tertiary == 0L, 0L,
      data.table::fifelse(tertiary == 1L & !is.na(institution),
                         as.integer(institution == 1L), NA_integer_))
  })
  dt[, UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED :=
       staff_any_observed(university_degrees[[1L]], university_degrees[[2L]])]
  dt[, N_MENCIONES_TOTAL := N_MENCIONES_RECORDED_1 + N_MENCIONES_RECORDED_2]
  dt[, N_MENCIONES_TOTAL_IS_RAW_FLAG_COUNT := 1L]
  # These combine recorded title specialties and applicable mentions; they are
  # evidence indicators, not a certification of the subject currently taught.
  dt[, MATH_SPECIALIZATION_REPORTED := staff_any_observed(
    HAS_MATH_TITLE_SPECIALTY_REPORTED, MENTION_MATE_REPORTED)]
  dt[, LANGUAGE_SPECIALIZATION_REPORTED := staff_any_observed(
    HAS_LANGUAGE_TITLE_SPECIALTY_REPORTED, MENTION_LENGUAJE_REPORTED)]
  invisible(dt)
}

staff_clean_annual <- function(path, nrows = Inf) {
  header <- names(data.table::fread(path, sep = ";", nrows = 0L,
                                   encoding = "Latin-1", showProgress = FALSE))
  required <- c("AGNO", "RBD", "MRUN", "PERSONAS", "ID_IFP")
  missing <- setdiff(required, header)
  if (length(missing)) stop("Missing required columns in ", basename(path), ": ",
                            paste(missing, collapse = ", "))
  mentions <- setdiff(grep("^MEN_.*_[12]$", header, value = TRUE),
                      c("MEN_SIN_MENCION_1", "MEN_SIN_MENCION_2"))
  selected <- intersect(unique(c(staff_raw_fields(), mentions)), header)
  chars <- intersect(c("MRUN", "NOMBRE_SLEP", "DOC_FEC_NAC"), selected)
  dt <- data.table::fread(
    path, sep = ";", select = selected, nrows = nrows,
    colClasses = list(character = chars), encoding = "Latin-1",
    na.strings = c("", "NA"), showProgress = FALSE
  )
  if (!nrow(dt)) stop("Empty annual data file: ", basename(path))
  for (field in setdiff(staff_raw_fields(), names(dt))) {
    data.table::set(dt, j = field, value =
      if (field %in% c("MRUN", "NOMBRE_SLEP", "DOC_FEC_NAC")) NA_character_ else NA_integer_)
  }
  numeric_fields <- setdiff(names(dt), c("MRUN", "NOMBRE_SLEP", "DOC_FEC_NAC"))
  measure_fields <- grep("^(HORAS|DURACION_CARRERA_|ANO_SERVICIO_|BIENIOS_CARR_DOCENTE$)",
                         numeric_fields, value = TRUE)
  for (field in numeric_fields) {
    raw <- dt[[field]]
    number <- suppressWarnings(as.numeric(raw))
    invalid_number <- !is.na(raw) & (is.na(number) | !is.finite(number))
    if (!field %in% measure_fields) invalid_number <- invalid_number |
      (!is.na(number) & number != trunc(number))
    if (any(invalid_number)) {
      stop("Invalid numeric value/code in ", basename(path), ": ", field)
    }
    data.table::set(dt, j = field,
                   value = if (field %in% measure_fields) number else as.integer(number))
  }
  years <- unique(dt$AGNO)
  if (length(years) != 1L || is.na(years)) stop("Expected one valid AGNO in ", basename(path))
  dt[, SOURCE_FILE := basename(path)]
  dt[, SOURCE_ROW := seq_len(.N)]
  dt[, VALID_PERSON_ID := as.integer(!is.na(MRUN) & grepl("^[0-9]+$", MRUN) & grepl("[1-9]", MRUN))]
  dt[, VALID_SCHOOL_ID := as.integer(!is.na(RBD) & RBD > 0L)]
  dt[, FUNCION_PRINCIPAL := unname(staff_role_labels[as.character(ID_IFP)])]
  dt[, FUNCION_SECUNDARIA := unname(staff_role_labels[as.character(ID_IFS)])]
  dt[, PRIMARY_ROLE_CODE_VALID := as.integer(ID_IFP %in% 1:17)]
  dt[, SECONDARY_ROLE_CODE_VALID := as.integer(ID_IFS %in% 0:17)]
  for (role in c("TEACHER", "ORIENTADOR")) {
    target <- if (role == "TEACHER") 1L else 9L
    a <- staff_role_flag(dt$ID_IFP, target)
    b <- staff_role_flag(dt$ID_IFS, target, secondary = TRUE)
    data.table::set(dt, j = paste0(role, "_PRIMARY"), value = a)
    data.table::set(dt, j = paste0(role, "_SECONDARY"), value = b)
    data.table::set(dt, j = paste0(role, "_ANY"), value = staff_or(a, b))
    data.table::set(dt, j = paste0(role, "_SECONDARY_ONLY"),
                   value = as.integer(a == 0L & b == 1L))
    data.table::set(dt, j = paste0(role, "_PRIMARY_NO_SECONDARY"),
                   value = as.integer(a == 1L & staff_role_flag(dt$ID_IFS, 0L, TRUE) == 1L))
  }
  staff_add_hs_assignments(dt)
  staff_add_credentials(dt, mentions)
  # Historical HORAS1/2 changed from 45-minute periods to clock hours in 2015.
  # Preserve raw fields and flag their units; do not silently impose hours weights.
  dt[, TEACHING_HOURS_UNIT_MINUTES := data.table::fifelse(AGNO <= 2014L, 45L, 60L)]
  dt[, IN_VA_WINDOW := as.integer(AGNO >= 2018L & AGNO <= 2024L)]
  dt[, REPORTED_SERVICE_SYSTEM_YEARS := data.table::fifelse(
    ANO_SERVICIO_SISTEMA >= 0L, ANO_SERVICIO_SISTEMA, NA_integer_)]
  dt[, REPORTED_SERVICE_SCHOOL_YEARS := data.table::fifelse(
    ANO_SERVICIO_EE >= 0L, ANO_SERVICIO_EE, NA_integer_)]
  # Preferred school tenure uses the current record's reported service years.
  # Do not fill missing reports with panel appearances, cap tenure at 2013,
  # or interpret this measure as experience in a particular role.
  dt[, YEARS_AT_SCHOOL := REPORTED_SERVICE_SCHOOL_YEARS]
  # The full mention array is summarized; retain the six requested flags and
  # both no-mention flags, rather than exporting dozens of redundant columns.
  drop <- setdiff(mentions, staff_raw_fields())
  if (length(drop)) dt[, (drop) := NULL]
  audit <- data.table::data.table(
    AGNO = years, SOURCE_FILE = basename(path), N_INPUT = nrow(dt),
    N_PERSONAS_EQ_1 = sum(dt$PERSONAS == 1L, na.rm = TRUE),
    N_INVALID_PERSON_ID = sum(dt$VALID_PERSON_ID == 0L),
    N_INVALID_SCHOOL_ID = sum(dt$VALID_SCHOOL_ID == 0L),
    N_INVALID_PRIMARY_FUNCTION = sum(dt$PRIMARY_ROLE_CODE_VALID == 0L),
    N_INVALID_SECONDARY_FUNCTION = sum(dt$SECONDARY_ROLE_CODE_VALID == 0L),
    N_MISSING_PERSONAS = sum(!dt$PERSONAS %in% 0:1),
    N_TEACHER_MAIN = sum(dt$PERSONAS == 1L & dt$TEACHER_PRIMARY == 1L, na.rm = TRUE),
    N_TEACHER_HS_ANY = sum(dt$TEACHER_HS_ANY == 1L, na.rm = TRUE),
    N_TEACHER_HS_ASSIGNMENT_UNKNOWN = sum(dt$TEACHER_ANY == 1L & is.na(dt$HS_ASSIGNMENT_ANY), na.rm = TRUE),
    N_TEACHING_LEVEL_MISMATCH = sum(
      dt$TEACHING_LEVEL_MISMATCH_1 == 1L | dt$TEACHING_LEVEL_MISMATCH_2 == 1L, na.rm = TRUE),
    N_TEACHING_CODE_UNMAPPED = sum(
      dt$TEACHING_CODE_UNMAPPED_1 == 1L | dt$TEACHING_CODE_UNMAPPED_2 == 1L, na.rm = TRUE),
    N_ORIENTADOR_MAIN = sum(dt$PERSONAS == 1L & dt$ORIENTADOR_PRIMARY == 1L, na.rm = TRUE),
    N_ORIENTADOR_SECONDARY = sum(dt$ORIENTADOR_SECONDARY_ONLY == 1L, na.rm = TRUE),
    N_TITLE_INSTITUTION_CONFLICT = sum(
      dt$TITLE_INSTITUTION_CONFLICT_1 == 1L | dt$TITLE_INSTITUTION_CONFLICT_2 == 1L, na.rm = TRUE),
    N_UNMAPPED_SPECIALTY = sum(
      dt$SPECIALTY_CODE_UNMAPPED_1 == 1L | dt$SPECIALTY_CODE_UNMAPPED_2 == 1L, na.rm = TRUE),
    N_MENTION_OUTSIDE_APPLICABILITY = sum(
      dt$MENTION_OUTSIDE_APPLICABILITY_1 == 1L | dt$MENTION_OUTSIDE_APPLICABILITY_2 == 1L, na.rm = TRUE),
    MISSING_OPTIONAL_COLUMNS = paste(setdiff(staff_raw_fields(), header), collapse = ";")
  )
  list(data = dt, audit = audit)
}

staff_decode_or <- function(x) {
  data.table::fcase(x == 2L, 1L, x == 0L, 0L, default = NA_integer_)
}

staff_collapse_roles <- function(dt, group) {
  compact <- dt[, c(group, "TEACHER_ANY", "TEACHER_HS_ANY", "ORIENTADOR_ANY", "ORIENTADOR_PRIMARY"), with = FALSE]
  # Encoding permits fast groupwise max with three-valued OR: TRUE > unknown > FALSE.
  compact[, TEACHER_ANY := data.table::fcase(
    TEACHER_ANY == 1L, 2L, is.na(TEACHER_ANY), 1L, default = 0L)]
  compact[, TEACHER_HS_ANY := data.table::fcase(
    TEACHER_HS_ANY == 1L, 2L, is.na(TEACHER_HS_ANY), 1L, default = 0L)]
  compact[, ORIENTADOR_ANY := data.table::fcase(
    ORIENTADOR_ANY == 1L, 2L, is.na(ORIENTADOR_ANY), 1L, default = 0L)]
  compact[, ORIENTADOR_PRIMARY := data.table::fcase(
    ORIENTADOR_PRIMARY == 1L, 2L, is.na(ORIENTADOR_PRIMARY), 1L, default = 0L)]
  ans <- compact[, .(TEACHER_ANY = max(TEACHER_ANY),
                     TEACHER_HS_ANY = max(TEACHER_HS_ANY),
                     ORIENTADOR_ANY = max(ORIENTADOR_ANY),
                     ORIENTADOR_PRIMARY = max(ORIENTADOR_PRIMARY),
                     N_APPOINTMENT_ROWS = .N), by = group]
  ans[, TEACHER_ANY := staff_decode_or(TEACHER_ANY)]
  ans[, TEACHER_HS_ANY := staff_decode_or(TEACHER_HS_ANY)]
  ans[, ORIENTADOR_ANY := staff_decode_or(ORIENTADOR_ANY)]
  ans[, ORIENTADOR_PRIMARY := staff_decode_or(ORIENTADOR_PRIMARY)]
  ans
}

staff_add_role_history <- function(dt, role_col, prefix, group) {
  # dt must be unique and sorted within group by AGNO.
  value <- dt[[role_col]]
  dt[, STAFF_TMP_POS := as.integer(get(role_col) %in% 1L)]
  dt[, STAFF_TMP_KNOWN := as.integer(!is.na(get(role_col)))]
  dt[, STAFF_TMP_PREV_POS := shift(get(role_col)), by = group]
  dt[, STAFF_TMP_PREV_YEAR := shift(AGNO), by = group]
  dt[, STAFF_TMP_PRIOR_N := seq_len(.N) - 1L, by = group]
  dt[, STAFF_TMP_PRIOR_POS := cumsum(STAFF_TMP_POS) - STAFF_TMP_POS, by = group]
  dt[, STAFF_TMP_PRIOR_KNOWN := cumsum(STAFF_TMP_KNOWN) - STAFF_TMP_KNOWN, by = group]
  data.table::set(dt, j = paste0(prefix, "_PRIOR_KNOWN_YEARS"), value = dt$STAFF_TMP_PRIOR_KNOWN)
  data.table::set(dt, j = paste0(prefix, "_PRIOR_YEARS_OBSERVED"),
    value = data.table::fifelse(dt$STAFF_TMP_PRIOR_KNOWN > 0L, dt$STAFF_TMP_PRIOR_POS, NA_integer_))
  # Cumulative experience includes the current observed year and survives gaps,
  # moves and later role changes. It is not the current consecutive spell.
  known_to_date <- dt$STAFF_TMP_PRIOR_KNOWN + dt$STAFF_TMP_KNOWN
  data.table::set(dt, j = paste0(prefix, "_KNOWN_YEARS_TO_DATE"), value = known_to_date)
  data.table::set(dt, j = paste0(prefix, "_CUMULATIVE_YEARS_OBSERVED"),
    value = data.table::fifelse(known_to_date > 0L,
      dt$STAFF_TMP_PRIOR_POS + dt$STAFF_TMP_POS, NA_integer_))
  data.table::set(dt, j = paste0(prefix, "_PRIOR_SHARE"),
    value = data.table::fifelse(dt$STAFF_TMP_PRIOR_KNOWN > 0L,
                              dt$STAFF_TMP_PRIOR_POS / dt$STAFF_TMP_PRIOR_KNOWN, NA_real_))
  # At least one earlier record required; a singleton is not "permanent".
  data.table::set(dt, j = paste0(prefix, "_EXCLUSIVE_HISTORY_TO_DATE"),
    value = data.table::fifelse(
      !is.na(value) & dt$STAFF_TMP_PRIOR_N > 0L &
        dt$STAFF_TMP_PRIOR_KNOWN == dt$STAFF_TMP_PRIOR_N,
      as.integer(value == 1L & dt$STAFF_TMP_PRIOR_POS == dt$STAFF_TMP_PRIOR_N), NA_integer_))
  start <- is.na(dt$STAFF_TMP_PREV_YEAR) | dt$AGNO != dt$STAFF_TMP_PREV_YEAR + 1L |
    is.na(value) | value != 1L | is.na(dt$STAFF_TMP_PREV_POS) | dt$STAFF_TMP_PREV_POS != 1L
  dt[, STAFF_TMP_SPELL_START := data.table::fifelse(start, AGNO, 0L)]
  dt[, STAFF_TMP_SPELL_START := cummax(STAFF_TMP_SPELL_START), by = group]
  data.table::set(dt, j = paste0(prefix, "_CONSECUTIVE_YEARS_TO_DATE"),
    value = data.table::fifelse(value == 1L, dt$AGNO - dt$STAFF_TMP_SPELL_START + 1L,
                              data.table::fifelse(value == 0L, 0L, NA_integer_)))
  tmp <- grep("^STAFF_TMP_", names(dt), value = TRUE)
  dt[, (tmp) := NULL]
  invisible(dt)
}

staff_build_histories <- function(appointments) {
  history_columns <- c(
    "MRUN", "AGNO", "RBD", "PERSONAS", "ID_IFP", "VALID_SCHOOL_ID",
    "TEACHER_PRIMARY", "TEACHER_HS_PRIMARY", "ORIENTADOR_PRIMARY",
    "TEACHER_ANY", "TEACHER_HS_ANY", "ORIENTADOR_ANY"
  )
  valid <- appointments[
    VALID_PERSON_ID == 1L & AGNO >= staff_history_start_year, ..history_columns
  ]
  main <- valid[PERSONAS == 1L]
  if (anyDuplicated(main, by = c("MRUN", "AGNO"))) {
    stop("Duplicate PERSONAS=1 person-years: resolve the source ambiguity; no arbitrary row selection.")
  }
  # Experience in a role counts a person once per year across ALL appointments.
  py <- staff_collapse_roles(valid, c("MRUN", "AGNO"))
  main_codes <- main[, .(MRUN, AGNO, MAIN_ID_IFP = ID_IFP,
                         TEACHER_MAIN = TEACHER_PRIMARY, TEACHER_HS_MAIN = TEACHER_HS_PRIMARY,
                         ORIENTADOR_MAIN = ORIENTADOR_PRIMARY)]
  py <- merge(py, main_codes, by = c("MRUN", "AGNO"), all.x = TRUE, sort = FALSE)
  data.table::setorder(py, MRUN, AGNO)
  py[, HISTORY_WINDOW_START_YEAR := staff_history_start_year]
  py[, HISTORY_FIRST_OBSERVED_YEAR := min(AGNO), by = MRUN]
  py[, HISTORY_PRIOR_OBSERVED_YEARS := seq_len(.N) - 1L, by = MRUN]
  py[, HISTORY_PRIOR_MISSING_CALENDAR_YEARS :=
       AGNO - HISTORY_FIRST_OBSERVED_YEAR - HISTORY_PRIOR_OBSERVED_YEARS]
  py[, HISTORY_OBSERVED_IN_2013 := as.integer(HISTORY_FIRST_OBSERVED_YEAR == 2013L)]
  py[, HAS_PRIOR_OBSERVED_HISTORY := as.integer(HISTORY_PRIOR_OBSERVED_YEARS > 0L)]
  py[, MAIN_ID_IFP := data.table::fifelse(MAIN_ID_IFP %in% 1:17, MAIN_ID_IFP, NA_integer_)]
  py[, STAFF_TMP_PREV_ROLE := shift(MAIN_ID_IFP), by = MRUN]
  py[, STAFF_TMP_PREV_YEAR := shift(AGNO), by = MRUN]
  py[, PRIMARY_ROLE_CHANGED_SINCE_LAST_OBS := data.table::fifelse(
    !is.na(MAIN_ID_IFP) & !is.na(STAFF_TMP_PREV_ROLE),
    as.integer(MAIN_ID_IFP != STAFF_TMP_PREV_ROLE), NA_integer_)]
  py[, PRIMARY_ROLE_CHANGE_ADJACENT := data.table::fifelse(
    AGNO == STAFF_TMP_PREV_YEAR + 1L, PRIMARY_ROLE_CHANGED_SINCE_LAST_OBS, NA_integer_)]
  # Director (4) included profesor encargado until 2014; from 2015 code 16
  # identifies the latter separately. Keep raw changes but flag this ambiguity.
  py[, PRIMARY_ROLE_CHANGE_POSSIBLE_2015_CODE_BREAK := as.integer(
    STAFF_TMP_PREV_YEAR %in% 2014L & AGNO == 2015L &
      STAFF_TMP_PREV_ROLE %in% 4L & MAIN_ID_IFP %in% 16L)]
  py[, N_PRIMARY_ROLE_CHANGES_ADJACENT_TO_DATE :=
       cumsum(as.integer(PRIMARY_ROLE_CHANGE_ADJACENT %in% 1L)), by = MRUN]
  py[, N_PRIMARY_ROLE_COMPARISONS_ADJACENT_TO_DATE :=
       cumsum(as.integer(!is.na(PRIMARY_ROLE_CHANGE_ADJACENT))), by = MRUN]
  py[, N_PRIMARY_ROLE_CHANGES_PRIOR := N_PRIMARY_ROLE_CHANGES_ADJACENT_TO_DATE -
       as.integer(PRIMARY_ROLE_CHANGE_ADJACENT %in% 1L)]
  py[, N_DISTINCT_PRIMARY_FUNCTIONS_TO_DATE :=
       cumsum(as.integer(!duplicated(MAIN_ID_IFP) & !is.na(MAIN_ID_IFP))), by = MRUN]
  py[, c("STAFF_TMP_PREV_ROLE", "STAFF_TMP_PREV_YEAR") := NULL]
  for (role in c("TEACHER", "TEACHER_HS", "ORIENTADOR")) {
    staff_add_role_history(py, paste0(role, "_ANY"), paste0(role, "_ANY"), "MRUN")
    staff_add_role_history(py, paste0(role, "_MAIN"), paste0(role, "_MAIN"), "MRUN")
  }
  # Primary function at ANY appointment (ID_IFP=9), distinct from the primary
  # function of the PERSONAS=1 main appointment captured by ORIENTADOR_MAIN.
  staff_add_role_history(py, "ORIENTADOR_PRIMARY", "ORIENTADOR_PRIMARY", "MRUN")
  # Supplemental role-at-school history counts each observed year once.
  # General school tenure is YEARS_AT_SCHOOL, reported in ANO_SERVICIO_EE.
  sy <- staff_collapse_roles(valid[VALID_SCHOOL_ID == 1L], c("MRUN", "RBD", "AGNO"))
  data.table::setorder(sy, MRUN, RBD, AGNO)
  sy[, SCHOOL_PRIOR_RECORD_YEARS_SINCE_2013 := seq_len(.N) - 1L, by = .(MRUN, RBD)]
  for (role in c("TEACHER", "TEACHER_HS", "ORIENTADOR")) {
    staff_add_role_history(sy, paste0(role, "_ANY"), paste0(role, "_AT_SCHOOL"),
                           c("MRUN", "RBD"))
  }
  # No EVER_* (full-window/future-dependent) labels enter these feature tables.
  data.table::setnames(py, c("TEACHER_ANY", "ORIENTADOR_ANY", "N_APPOINTMENT_ROWS"),
                      c("TEACHER_ANYWHERE_THIS_YEAR", "ORIENTADOR_ANYWHERE_THIS_YEAR",
                        "N_APPOINTMENTS_THIS_YEAR"))
  data.table::setnames(sy, c("TEACHER_ANY", "ORIENTADOR_ANY", "N_APPOINTMENT_ROWS"),
                      c("TEACHER_AT_SCHOOL_THIS_YEAR", "ORIENTADOR_AT_SCHOOL_THIS_YEAR",
                        "N_APPOINTMENTS_AT_SCHOOL_THIS_YEAR"))
  data.table::setnames(py, "TEACHER_HS_ANY", "TEACHER_HS_ANYWHERE_THIS_YEAR")
  data.table::setnames(sy, "TEACHER_HS_ANY", "TEACHER_HS_AT_SCHOOL_THIS_YEAR")
  data.table::setnames(py, "ORIENTADOR_PRIMARY", "ORIENTADOR_PRIMARY_ANYWHERE_THIS_YEAR")
  data.table::setnames(sy, "ORIENTADOR_PRIMARY", "ORIENTADOR_PRIMARY_AT_SCHOOL_THIS_YEAR")
  list(person_year = py, person_school_year = sy)
}
