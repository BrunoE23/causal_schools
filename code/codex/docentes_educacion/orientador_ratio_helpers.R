# Shared calculations for VA-sample students per period-average orientador.
# Requires data.table and staff_cleaning_helpers.R (the same role-code rules).

ori_collapse_flag <- function(x) {
  if (any(x == 1L, na.rm = TRUE)) return(1L)
  if (anyNA(x)) return(NA_integer_)
  0L
}

ori_annual_counts <- function(appointments, school_ids) {
  dt <- copy(appointments)
  required <- c("AGNO", "RBD", "MRUN", "ID_IFP", "ID_IFS")
  if (!all(required %in% names(dt))) stop("Missing annual staff columns.")
  if (uniqueN(dt$AGNO) != 1L || anyNA(dt$AGNO)) stop("Expected one annual staff year.")
  dt <- dt[RBD %in% school_ids]
  dt[, VALID_PERSON_ID := !is.na(MRUN) & grepl("^[0-9]+$", MRUN) & grepl("[1-9]", MRUN)]
  dt[, PRIMARY := staff_role_flag(ID_IFP, 9L)]
  dt[, ANY := staff_or(PRIMARY, staff_role_flag(ID_IFS, 9L, secondary = TRUE))]
  roster <- dt[, .(
    N_APPOINTMENTS = .N,
    N_INVALID_PERSON_ROWS = sum(!VALID_PERSON_ID),
    N_UNIDENTIFIED_POTENTIAL_PRIMARY_ROWS = sum(!VALID_PERSON_ID & (is.na(PRIMARY) | PRIMARY == 1L)),
    N_UNIDENTIFIED_POTENTIAL_ANY_ROWS = sum(!VALID_PERSON_ID & (is.na(ANY) | ANY == 1L))
  ), by = .(RBD, AGNO)]
  # A positive role on another appointment at THIS school dominates an unknown.
  people <- dt[VALID_PERSON_ID == TRUE, .(
    PRIMARY = ori_collapse_flag(PRIMARY), ANY = ori_collapse_flag(ANY)
  ), by = .(RBD, AGNO, MRUN)]
  counts <- people[, .(
    N_STAFF_IDENTIFIED = .N,
    N_PRIMARY_IDENTIFIED = sum(PRIMARY == 1L, na.rm = TRUE),
    N_ANY_IDENTIFIED = sum(ANY == 1L, na.rm = TRUE),
    N_PRIMARY_UNKNOWN_PERSONS = sum(is.na(PRIMARY)),
    N_ANY_UNKNOWN_PERSONS = sum(is.na(ANY))
  ), by = .(RBD, AGNO)]
  out <- merge(roster, counts, by = c("RBD", "AGNO"), all.x = TRUE)
  # No valid person IDs means no identified people, not necessarily zero staff.
  for (field in setdiff(names(counts), c("RBD", "AGNO"))) {
    set(out, which(is.na(out[[field]])), field, 0L)
  }
  out[, ROSTER_OBSERVED := 1L]
  out[, N_ORIENTADORES_PRIMARY := fifelse(
    N_PRIMARY_UNKNOWN_PERSONS == 0L & N_UNIDENTIFIED_POTENTIAL_PRIMARY_ROWS == 0L,
    N_PRIMARY_IDENTIFIED, NA_integer_)]
  out[, N_ORIENTADORES_ANY := fifelse(
    N_ANY_UNKNOWN_PERSONS == 0L & N_UNIDENTIFIED_POTENTIAL_ANY_ROWS == 0L,
    N_ANY_IDENTIFIED, NA_integer_)]
  setorder(out, RBD, AGNO)
  out
}

ori_validate_va_counts <- function(va) {
  required <- c("school_rbd", "analysis_sample", "outcome", "n_students", "n_total")
  if (!all(required %in% names(va))) stop("Missing VA count columns.")
  if (!nrow(va) || anyNA(va[, ..required])) stop("Empty/missing VA counts.")
  if (any(va$analysis_sample != "All") || any(va$outcome != "admission_exam_taker")) {
    stop("Use the broad All-sample exam-TAKING VA input, not exam-score VA.")
  }
  if (anyDuplicated(va$school_rbd) || any(va$school_rbd <= 0) ||
      any(va$school_rbd != trunc(va$school_rbd))) stop("Invalid/duplicate VA school IDs.")
  if (any(va$n_students <= 0) || any(va$n_students != trunc(va$n_students)) ||
      uniqueN(va$n_total) != 1L || sum(va$n_students) != va$n_total[1L]) {
    stop("VA school counts do not reconcile with the estimation total.")
  }
  data.table(RBD = as.integer(va$school_rbd), N_VA_STUDENTS = as.integer(va$n_students))
}

ori_build_period_ratios <- function(va_counts, annual_counts, years = 2018:2024) {
  if (!length(years) || anyNA(years) || anyDuplicated(years) || any(diff(years) != 1L)) {
    stop("Staff years must be a nonempty consecutive sequence.")
  }
  if (anyDuplicated(va_counts$RBD) || anyDuplicated(annual_counts, by = c("RBD", "AGNO"))) {
    stop("Duplicate school or school-year key.")
  }
  if (any(!annual_counts$AGNO %in% years) || any(!annual_counts$RBD %in% va_counts$RBD)) {
    stop("Annual counts outside the requested school-period support.")
  }
  annual <- merge(CJ(RBD = va_counts$RBD, AGNO = years), annual_counts,
                  by = c("RBD", "AGNO"), all.x = TRUE)
  annual[is.na(ROSTER_OBSERVED), ROSTER_OBSERVED := 0L]
  # Only appointment records can be zero-filled here. Missing headcounts stay NA.
  annual[ROSTER_OBSERVED == 0L, N_APPOINTMENTS := 0L]
  period <- annual[, .(
    N_YEARS_EXPECTED = length(years), N_YEARS_ROSTER_OBSERVED = sum(ROSTER_OBSERVED)
  ), by = RBD]
  for (scope in c("PRIMARY", "ANY")) {
    field <- paste0("N_ORIENTADORES_", scope)
    summary <- annual[, {
      x <- get(field)
      n_known <- sum(!is.na(x))
      n_zero <- sum(x == 0L, na.rm = TRUE)
      mean_full <- if (n_known == length(years)) mean(x) else NA_real_
      .(N_YEARS_KNOWN = n_known, N_YEARS_ZERO = n_zero,
        MEAN_ORIENTADORES = mean_full,
        MEAN_ORIENTADORES_OBSERVED_YEARS = if (n_known) mean(x, na.rm = TRUE) else NA_real_,
        NO_ORIENTADOR_FULL_PERIOD = if (n_known == length(years)) as.integer(n_zero == length(years)) else NA_integer_)
    }, by = RBD]
    setnames(summary, setdiff(names(summary), "RBD"), paste0(setdiff(names(summary), "RBD"), "_", scope))
    period <- merge(period, summary, by = "RBD")
  }
  period <- merge(va_counts, period, by = "RBD", all.x = TRUE)
  for (scope in c("PRIMARY", "ANY")) {
    denominator <- period[[paste0("MEAN_ORIENTADORES_", scope)]]
    ratio <- rep(NA_real_, nrow(period))
    valid <- !is.na(denominator) & denominator > 0
    ratio[valid] <- period$N_VA_STUDENTS[valid] / denominator[valid]
    set(period, j = paste0("VA_STUDENTS_PER_AVG_ORIENTADOR_", scope), value = ratio)
    set(period, j = paste0("RATIO_STATUS_", scope), value = fifelse(
      is.na(denominator), "incomplete_staff_coverage",
      fifelse(denominator == 0, "zero_orientadores_full_period", "available")))
  }
  period[, `:=`(COHORT_GR8_START = 2017L, COHORT_GR8_END = 2020L,
                STAFF_YEAR_START = min(years), STAFF_YEAR_END = max(years),
                STUDENT_COUNT_IS_VA_SAMPLE = 1L)]
  setorder(period, RBD)
  stopifnot(nrow(period) == nrow(va_counts), !anyDuplicated(period$RBD),
            nrow(annual) == nrow(va_counts) * length(years))
  comparable <- period[!is.na(MEAN_ORIENTADORES_PRIMARY) & !is.na(MEAN_ORIENTADORES_ANY)]
  stopifnot(all(comparable$MEAN_ORIENTADORES_ANY >= comparable$MEAN_ORIENTADORES_PRIMARY))
  list(period = period, annual = annual)
}
