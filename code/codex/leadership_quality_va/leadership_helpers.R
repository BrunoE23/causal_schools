# Uses the existing staff cleaner, q_prepare_people and staff_add_role_history.
leadership_codes <- c(3L, 4L, 10L, 15L)
l_flags <- c("LEADER_PRIMARY", "LEADER_ANY", "LEADER_NONDEDICATED",
             paste0("LEADER_CODE_", leadership_codes))
l_role_flag <- function(code, targets = leadership_codes, secondary = FALSE) {
  fifelse(code %in% if (secondary) 0:17 else 1:17, as.integer(code %in% targets), NA_integer_)
}
l_add_flags <- function(dt) {
  dt[, LEADER_PRIMARY := l_role_flag(ID_IFP)]
  dt[, LEADER_ANY := staff_or(LEADER_PRIMARY, l_role_flag(ID_IFS, secondary = TRUE))]
  dt[, LEADER_NONDEDICATED := as.integer(!(as.logical(LEADER_PRIMARY) &
                                           as.logical(staff_role_flag(ID_IFS, 0L, TRUE))))]
  for (code in leadership_codes) set(dt, j = paste0("LEADER_CODE_", code),
    value = staff_or(staff_role_flag(dt$ID_IFP, code), staff_role_flag(dt$ID_IFS, code, TRUE)))
  invisible(dt)
}
l_collapse <- function(dt, keys, flags) {
  thin <- copy(dt[, c(keys, flags), with = FALSE])
  for (f in flags) set(thin, j = f, value = fcase(thin[[f]] == 1L, 2L, is.na(thin[[f]]), 1L, default = 0L))
  ans <- thin[, lapply(.SD, max), by = keys, .SDcols = flags]
  for (f in flags) set(ans, j = f, value = staff_decode_or(ans[[f]]))
  ans
}
l_prepare <- function(dt, schools) {
  l_add_flags(dt)
  if (dt[RBD %in% schools, any(VALID_PERSON_ID != 1L)]) stop("Invalid person ID at VA school; review before counting.")
  prepared <- q_prepare_people(dt, schools, extra_role_fields = l_flags, keep_all = TRUE)
  people <- prepared$people
  roster <- people[, .(N_STAFF = .N, N_IDENTIFIED = sum(LEADER_ANY == 1L, na.rm = TRUE),
      N_UNKNOWN = sum(is.na(LEADER_ANY)), N_PRIMARY_IDENTIFIED = sum(LEADER_PRIMARY == 1L, na.rm = TRUE),
      N_PRIMARY_UNKNOWN = sum(is.na(LEADER_PRIMARY))), by = .(RBD, AGNO)]
  roster[, `:=`(N_ROLE = fifelse(N_UNKNOWN == 0L, N_IDENTIFIED, NA_integer_),
                  N_PRIMARY = fifelse(N_PRIMARY_UNKNOWN == 0L, N_PRIMARY_IDENTIFIED, NA_integer_))]
  # Independent direct raw-record count, without relying on the collapsed flags.
  direct <- unique(dt[RBD %in% schools & (ID_IFP %in% leadership_codes | ID_IFS %in% leadership_codes), .(RBD, AGNO, MRUN)])
  direct <- direct[, .(N_DIRECT = .N), by = .(RBD, AGNO)]
  check <- merge(roster, direct, by = c("RBD", "AGNO"), all.x = TRUE)
  check[is.na(N_DIRECT), N_DIRECT := 0L]
  stopifnot(all(check$N_IDENTIFIED == check$N_DIRECT))
  list(people = people[LEADER_ANY == 1L], roster = roster, conflicts = prepared$conflicts)
}
l_histories <- function(rows, ids) {
  rows <- copy(rows[VALID_PERSON_ID == 1L & MRUN %chin% ids & AGNO %in% 2013:2024])
  l_add_flags(rows)
  main <- rows[PERSONAS == 1L]
  if (anyDuplicated(main, by = c("MRUN", "AGNO"))) stop("Conflicting main appointments; no arbitrary main-role choice.")
  main <- main[, .(MRUN, AGNO, MAIN_FUNCTION = fifelse(ID_IFP %in% 1:17, ID_IFP, NA_integer_))]
  py <- l_collapse(rows, c("MRUN", "AGNO"), c("LEADER_PRIMARY", "LEADER_ANY", "LEADER_CODE_4"))
  py <- merge(py, main, by = c("MRUN", "AGNO"), all.x = TRUE)
  setorder(py, MRUN, AGNO)
  py[, HISTORY_PRIOR_OBSERVED_YEARS := seq_len(.N)-1L, by = MRUN]
  py[, PREVIOUS_YEAR := shift(AGNO), by = MRUN]
  py[, PREVIOUS_FUNCTION := shift(MAIN_FUNCTION), by = MRUN]
  py[, POSSIBLE_2015_CODE_BREAK := as.integer(AGNO == 2015L & PREVIOUS_YEAR %in% 2014L &
      PREVIOUS_FUNCTION %in% 4L & MAIN_FUNCTION %in% 16L)]
  comparable <- with(py, !is.na(MAIN_FUNCTION) & !is.na(PREVIOUS_FUNCTION) &
    !is.na(PREVIOUS_YEAR) & AGNO == PREVIOUS_YEAR + 1L & POSSIBLE_2015_CODE_BREAK == 0L)
  py[, MAIN_FUNCTION_CHANGED := fifelse(comparable, as.integer(MAIN_FUNCTION != PREVIOUS_FUNCTION), NA_integer_)]
  py[, LEADERSHIP_STATUS_CHANGED := fifelse(comparable,
    as.integer((MAIN_FUNCTION %in% leadership_codes) != (PREVIOUS_FUNCTION %in% leadership_codes)), NA_integer_)]
  for (f in c("MAIN_FUNCTION_CHANGED", "LEADERSHIP_STATUS_CHANGED")) {
    py[, TMP_N := cumsum(!is.na(get(f))) - as.integer(!is.na(get(f))), by = MRUN]
    py[, TMP_CHANGES := cumsum(get(f) %in% 1L) - as.integer(get(f) %in% 1L), by = MRUN]
    set(py, j = paste0(f, "_PRIOR_RATE"), value = fifelse(py$TMP_N > 0, py$TMP_CHANGES / py$TMP_N, NA_real_))
  }
  py[, PRE2015_CODE4_PRIOR := as.integer(cumsum(AGNO <= 2014 & LEADER_CODE_4 %in% 1L) -
      as.integer(AGNO <= 2014 & LEADER_CODE_4 %in% 1L) > 0), by = MRUN]
  py[, POSSIBLE_2015_CODE_BREAK_PRIOR := as.integer(cumsum(POSSIBLE_2015_CODE_BREAK) - POSSIBLE_2015_CODE_BREAK > 0), by = MRUN]
  staff_add_role_history(py, "LEADER_PRIMARY", "LEAD_PRIMARY", "MRUN")
  staff_add_role_history(py, "LEADER_ANY", "LEAD_ANY", "MRUN")
  recent <- copy(py[AGNO >= 2015, .(MRUN, AGNO, LEADER_PRIMARY)])
  staff_add_role_history(recent, "LEADER_PRIMARY", "LEAD_2015", "MRUN")
  py[recent, on = .(MRUN, AGNO), PRIOR_LEADERSHIP_YEARS_2015 := i.LEAD_2015_PRIOR_YEARS_OBSERVED]
  sy <- l_collapse(rows[VALID_SCHOOL_ID == 1L], c("MRUN", "RBD", "AGNO"), "LEADER_ANY")
  setorder(sy, MRUN, RBD, AGNO)
  sy[, SCHOOL_PRIOR_OBSERVED_YEARS := seq_len(.N)-1L, by = .(MRUN, RBD)]
  staff_add_role_history(sy, "LEADER_ANY", "LEAD_SCHOOL", c("MRUN", "RBD"))
  recent_school <- copy(sy[AGNO >= 2015, .(MRUN, RBD, AGNO, LEADER_ANY)])
  staff_add_role_history(recent_school, "LEADER_ANY", "LEAD_SCHOOL_2015", c("MRUN", "RBD"))
  sy[recent_school, on = .(MRUN, RBD, AGNO), SCHOOL_LEADERSHIP_SPELL_2015 := i.LEAD_SCHOOL_2015_CONSECUTIVE_YEARS_TO_DATE]
  py[, c("TMP_N", "TMP_CHANGES", "PREVIOUS_YEAR", "PREVIOUS_FUNCTION") := NULL]
  list(py = py, sy = sy)
}
l_attach <- function(people, history) {
  pcols <- setdiff(names(history$py), c("MRUN", "AGNO", "LEADER_PRIMARY", "LEADER_ANY", "LEADER_CODE_4"))
  scols <- setdiff(names(history$sy), c("MRUN", "RBD", "AGNO", "LEADER_ANY"))
  people[history$py, on = .(MRUN, AGNO), (pcols) := mget(paste0("i.", pcols))]
  people[history$sy, on = .(MRUN, RBD, AGNO), (scols) := mget(paste0("i.", scols))]
  stopifnot(!anyDuplicated(people, by = c("MRUN", "RBD", "AGNO")))
  people
}
l_metric_map <- c(
  prior_role_years = "LEAD_PRIMARY_PRIOR_YEARS_OBSERVED", cumulative_role_years = "LEAD_PRIMARY_CUMULATIVE_YEARS_OBSERVED",
  prior_any_role_years = "LEAD_ANY_PRIOR_YEARS_OBSERVED", prior_school_role_years = "LEAD_SCHOOL_PRIOR_YEARS_OBSERVED",
  role_spell_years = "LEAD_ANY_CONSECUTIVE_YEARS_TO_DATE", school_role_spell_years = "LEAD_SCHOOL_CONSECUTIVE_YEARS_TO_DATE",
  prior_role_share = "LEAD_PRIMARY_PRIOR_SHARE", exclusive_history_share = "LEAD_PRIMARY_EXCLUSIVE_HISTORY_TO_DATE",
  primary_role_share = "LEADER_PRIMARY", prior_main_function_change_rate = "MAIN_FUNCTION_CHANGED_PRIOR_RATE",
  prior_leadership_entry_exit_rate = "LEADERSHIP_STATUS_CHANGED_PRIOR_RATE",
  university_share = "UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED", teaching_title_share = "TEACHING_TITLE_REPORTED",
  tertiary_title_share = "TERTIARY_QUALIFICATION_REPORTED", specialty_share = "HAS_SPECIALTY_REPORTED",
  ip_share = "TRAINED_IP_REPORTED", cft_share = "TRAINED_CFT_REPORTED", normal_school_share = "TRAINED_NORMAL_SCHOOL_REPORTED",
  other_institution_share = "TRAINED_OTHER_REPORTED", title_duration_semesters = "TITLE_DURATION_MAX",
  years_since_title = "YEARS_SINCE_TITLE_MAX", reported_school_tenure = "YEARS_AT_SCHOOL",
  reported_system_tenure = "REPORTED_SERVICE_SYSTEM_YEARS", prior_role_years_2015 = "PRIOR_LEADERSHIP_YEARS_2015",
  school_role_spell_years_2015 = "SCHOOL_LEADERSHIP_SPELL_2015", pre2015_director_history_share = "PRE2015_CODE4_PRIOR",
  possible_2015_code_break_share = "POSSIBLE_2015_CODE_BREAK_PRIOR",
  planta_directiva_share = "LEADER_CODE_3", director_share = "LEADER_CODE_4",
  directiva_share = "LEADER_CODE_10", subdirector_share = "LEADER_CODE_15")
l_dictionary <- function() {
  labels <- c("Prior primary-function leadership years", "Cumulative primary-function leadership years",
    "Prior primary-or-secondary leadership years", "Prior leadership years at this school", "Current leadership spell",
    "Current school-leadership spell", "Leadership share of prior known primary years", "Always primary leadership to date",
    "Primary-function leadership share", "Prior detailed main-function change rate", "Prior main-function leadership entry/exit rate",
    "University tertiary qualification", "Teaching qualification", "Tertiary qualification", "Recorded title specialty",
    "IP-trained share", "CFT-trained share", "Normal-school-trained share", "Other institution type share",
    "Longest degree (semesters)", "Years since earliest title", "Reported school tenure", "Reported system tenure",
    "Prior primary leadership years since 2015", "School-leadership spell since 2015", "Prior pre-2015 director-code history",
    "Possible 2015 director/encargado coding break", "Planta Directiva (3) share", "Director (4) share", "Directiva (10) share", "Subdirector (15) share")
  d <- data.table(METRIC = names(l_metric_map), LABEL = labels, SOURCE_FIELD = unname(l_metric_map))
  d[, BLOCK := fcase(METRIC %chin% names(l_metric_map)[1:11], "career", METRIC %chin% names(l_metric_map)[12:21], "credentials",
    METRIC %chin% c("reported_school_tenure", "reported_system_tenure"), "tenure_audit",
    METRIC %chin% c("prior_role_years_2015", "school_role_spell_years_2015"), "history_sensitivity",
    METRIC %chin% c("pre2015_director_history_share", "possible_2015_code_break_share"), "history_audit", default = "role_composition")]
  d <- rbind(d, data.table(METRIC = c("dedicated_primary_share", "secondary_only_share", "history_known_share"),
    LABEL = c("Only primary leadership across school appointments", "Secondary-only leadership share", "Known prior leadership-history share"),
    SOURCE_FIELD = c("1 - LEADER_NONDEDICATED", "1 - LEADER_PRIMARY", "LEAD_PRIMARY_PRIOR_KNOWN_YEARS / HISTORY_PRIOR_OBSERVED_YEARS"),
    BLOCK = c("career", "career", "history_audit")))
  d[, CORE_COMPONENT := METRIC %chin% c("prior_role_years", "school_role_spell_years", "university_share", "teaching_title_share")]
  d[, ANALYZE := !BLOCK %chin% c("tenure_audit", "history_audit")]
  d
}
l_members <- function(people) {
  d <- copy(people)
  for (m in names(l_metric_map)) set(d, j = m, value = as.numeric(d[[l_metric_map[[m]]]]))
  d[, `:=`(dedicated_primary_share = 1 - LEADER_NONDEDICATED, secondary_only_share = 1 - LEADER_PRIMARY,
    history_known_share = fifelse(HISTORY_PRIOR_OBSERVED_YEARS > 0,
      LEAD_PRIMARY_PRIOR_KNOWN_YEARS / HISTORY_PRIOR_OBSERVED_YEARS, NA_real_))]
  d
}
l_aggregate <- function(people, roster, schools) {
  members <- l_members(people); dictionary <- l_dictionary()
  counts <- merge(CJ(RBD = schools, AGNO = 2018:2024), roster, by = c("RBD", "AGNO"), all.x = TRUE)
  annual <- rbindlist(lapply(dictionary$METRIC, function(metric) {
    a <- members[, .(N_ELIGIBLE = .N, N_OBSERVED = sum(is.finite(get(metric))),
      MEAN_OBSERVED = if (any(is.finite(get(metric)))) mean(get(metric)[is.finite(get(metric))]) else NA_real_), by = .(RBD, AGNO)]
    a <- merge(counts[, .(RBD, AGNO, N_ROLE)], a, by = c("RBD", "AGNO"), all.x = TRUE)
    a[!is.na(N_ROLE) & is.na(N_ELIGIBLE), `:=`(N_ELIGIBLE = 0L, N_OBSERVED = 0L)]
    a[, VALUE := fifelse(!is.na(N_ROLE) & N_ELIGIBLE > 0 & N_OBSERVED / N_ELIGIBLE >= .8, MEAN_OBSERVED, NA_real_)]
    a[, METRIC := metric]; a
  }))
  period <- annual[, {
    known <- sum(!is.na(N_ROLE)); active <- sum(N_ROLE > 0, na.rm = TRUE); valid <- sum(is.finite(VALUE))
    raw <- if (valid > 0) mean(VALUE[is.finite(VALUE)]) else NA_real_
    .(N_ROLE_YEARS_KNOWN = known, N_ACTIVE_YEARS = active, N_VALID_YEARS = valid,
      N_ELIGIBLE_PERSON_YEARS = sum(N_ELIGIBLE, na.rm = TRUE), N_OBSERVED_PERSON_YEARS = sum(N_OBSERVED, na.rm = TRUE),
      MEAN_OBSERVED_YEARS = raw, VALUE = if (known == 7L && active > 0 && valid/active >= .8) raw else NA_real_)
  }, by = .(RBD, METRIC)]
  list(annual = annual, period = period, dictionary = dictionary, roster = counts)
}
