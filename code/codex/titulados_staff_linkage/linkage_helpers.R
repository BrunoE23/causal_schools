# Pure helpers. Missing identifiers never match; real multiple awards remain.
t_id <- function(x) {
  x <- trimws(as.character(x)); ok <- !is.na(x) & grepl("^[0-9]+$", x) & grepl("[1-9]", x)
  out <- rep(NA_character_, length(x)); out[ok] <- sub("^0+", "", x[ok]); out
}
t_int <- function(x) {
  x <- trimws(as.character(x)); ok <- !is.na(x) & grepl("^-?[0-9]+$", x)
  out <- rep(NA_integer_, length(x)); out[ok] <- suppressWarnings(as.integer(x[ok])); out
}
t_text <- function(x) {
  x <- trimws(as.character(x))
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1", "aeiouunAEIOUUN", x)
  toupper(gsub("[[:space:]]+", " ", x))
}
t_birth <- function(x) {
  x <- trimws(as.character(x)); y <- t_int(substr(x, 1, 4)); m <- t_int(substr(x, 5, 6))
  ok <- !is.na(x) & grepl("^[0-9]{6}([0-9]{2})?$", x) & y > 1900L & y <= 2025L & m %in% 1:12
  fifelse(ok, substr(x, 1, 6), NA_character_)
}
t_date <- function(x) {
  x <- trimws(as.character(x)); ok <- !is.na(x) & grepl("^[0-9]{8}$", x) & x != "19000101"
  out <- as.Date(rep(NA_character_, length(x)))
  out[ok] <- suppressWarnings(as.Date(x[ok], format = "%Y%m%d"))
  out[ok & !is.na(out) & format(out, "%Y%m%d") != x] <- as.Date(NA_character_)
  out
}
t_expected <- setNames(c(97624L,111913L,122995L,118490L,136408L,156000L,182179L,197558L,216587L,
  230506L,244006L,250349L,246619L,203598L,281491L,291460L,299893L,304727L,328998L), 2007:2025)
t_clean_awards <- function(raw, year, filename) {
  year <- as.integer(year)
  setnames(raw, toupper(names(raw)))
  stopifnot(!anyDuplicated(names(raw)), all(t_int(raw$CAT_PERIODO) == year))
  original <- copy(names(raw))
  # Check complete strings, not an arbitrary byte slice, before interpretation.
  for (f in original) {
    vals <- unique(raw[[f]])
    if (any(!is.na(vals) & is.na(iconv(vals, from = "UTF-8", to = "UTF-8")))) stop("Invalid UTF-8: ", f)
  }
  raw[, SOURCE_ROW := seq_len(.N)]
  duplicate <- duplicated(raw, by = original)
  d <- copy(raw[!duplicate])
  setnames(d, "MRUN", "MRUN_RAW")
  d[, `:=`(MRUN = t_id(MRUN_RAW), REPORT_YEAR = year, SOURCE_FILE = filename,
    RECORD_ID = paste0(year, ":", SOURCE_ROW), BIRTH_YM = t_birth(FEC_NAC_ALU),
    SEX = fifelse(t_int(GEN_ALU) %in% 1:2, t_int(GEN_ALU), NA_integer_),
    AWARD_DATE = t_date(FECHA_OBTENCION_TITULO))]
  d[, AWARD_YEAR := t_int(format(AWARD_DATE, "%Y"))]
  d[, LEVEL := fcase(t_text(NIVEL_GLOBAL) == "PREGRADO", "undergraduate",
    t_text(NIVEL_GLOBAL) %chin% c("POSGRADO", "POSTGRADO"), "postgraduate",
    t_text(NIVEL_GLOBAL) == "POSTITULO", "postitulo", default = "unmapped")]
  d[, `:=`(IS_UNDERGRAD = LEVEL == "undergraduate", IS_POSTGRAD = LEVEL == "postgraduate",
    IS_POSTITULO = LEVEL == "postitulo", IS_EDUCATION = t_text(AREA_CONOCIMIENTO) == "EDUCACION")]
  d[, `:=`(IS_EDUCATION_UNDERGRAD = IS_UNDERGRAD & IS_EDUCATION,
    AWARD_YEAR_DIFFERS_FROM_REPORT = !is.na(AWARD_YEAR) & AWARD_YEAR != REPORT_YEAR,
    DATE_MISSING_OR_INVALID = is.na(AWARD_DATE),
    ASOF_YEAR = pmax(REPORT_YEAR, AWARD_YEAR, na.rm = TRUE),
    ASOF_BASIS = fifelse(is.na(AWARD_YEAR), "report_year_only", "report_and_award_year"))]
  d[, INSTITUTION_ID := t_id(COD_INST)]
  d[, INSTITUTION_NAMED := !is.na(NOMB_INST) & nzchar(trimws(NOMB_INST))]
  d[, PROGRAM_NAMED := !is.na(NOMB_CARRERA) & nzchar(trimws(NOMB_CARRERA))]
  d[, EDUCATION_FIELD_NAMED := IS_EDUCATION] # broad field; not a teaching-license claim
  audit <- data.table(YEAR = year, N_INPUT = nrow(raw), N_CODEBOOK = unname(t_expected[as.character(year)]),
    N_EXACT_DUPLICATES_REMOVED = sum(duplicate), N_CLEAN = nrow(d), N_INVALID_MRUN = sum(is.na(d$MRUN)),
    N_UNIQUE_VALID_PEOPLE = uniqueN(d$MRUN[!is.na(d$MRUN)]), N_UNMAPPED_LEVEL = sum(d$LEVEL == "unmapped"),
    N_BAD_BIRTH = sum(is.na(d$BIRTH_YM)), N_BAD_AWARD_DATE = sum(d$DATE_MISSING_OR_INVALID),
    N_AWARD_YEAR_DIFFERS = sum(d$AWARD_YEAR_DIFFERS_FROM_REPORT))
  list(data = d, audit = audit)
}
t_single <- function(x) {
  x <- unique(x[!is.na(x)]); if (length(x) == 1L) x else NA
}
t_flags_by_year <- function(awards) {
  out <- awards[, .(N_AWARDS = .N, FIRST_ANY_YEAR = min(ASOF_YEAR),
    FIRST_UNDERGRAD_YEAR = if (any(IS_UNDERGRAD)) min(ASOF_YEAR[IS_UNDERGRAD]) else NA_integer_,
    FIRST_EDUCATION_UNDERGRAD_YEAR = if (any(IS_EDUCATION_UNDERGRAD)) min(ASOF_YEAR[IS_EDUCATION_UNDERGRAD]) else NA_integer_,
    FIRST_POSTGRAD_YEAR = if (any(IS_POSTGRAD)) min(ASOF_YEAR[IS_POSTGRAD]) else NA_integer_,
    FIRST_POSTITULO_YEAR = if (any(IS_POSTITULO)) min(ASOF_YEAR[IS_POSTITULO]) else NA_integer_,
    FIRST_NAMED_INSTITUTION_YEAR = if (any(INSTITUTION_NAMED)) min(ASOF_YEAR[INSTITUTION_NAMED]) else NA_integer_), by = MRUN]
  out
}
