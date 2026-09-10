# Requires data.table and the existing docentes_educacion staff-cleaning helpers.
q_flag_any <- function(...) staff_any_observed(...)
q_max_reported <- function(a, b) {
  ans <- pmax(a, b, na.rm = TRUE)
  ans[!is.finite(ans)] <- NA_real_
  ans
}

q_prepare_people <- function(dt, school_ids, extra_role_fields = character(), keep_all = FALSE) {
  dt <- copy(dt[RBD %in% school_ids & VALID_PERSON_ID == 1L])
  # Extra title and same-slot teaching indicators; never infer subject from title.
  hs_title <- lapply(1:2, function(k) {
    tit <- dt[[paste0("TIT_ID_", k)]]
    tip <- dt[[paste0("TIP_TIT_ID_", k)]]
    fcase(tit == 1L & tip %in% 11:16, as.integer(tip %in% c(14L, 16L)),
          tit %in% 2:3, 0L, default = NA_integer_)
  })
  dt[, HS_TEACHING_TITLE_REPORTED := q_flag_any(hs_title[[1]], hs_title[[2]])]
  dt[, ORIENTATION_MENTION_EVIDENCE := q_flag_any(
    fifelse(MEN_ORIENTACION_1 %in% 0:1, MEN_ORIENTACION_1, NA_integer_),
    fifelse(MEN_ORIENTACION_2 %in% 0:1, MEN_ORIENTACION_2, NA_integer_))]
  dt[, ORIENTATION_MENTION_APPLICABLE := q_flag_any(MENTIONS_APPLICABLE_1, MENTIONS_APPLICABLE_2)]
  for (subject in c("MATH", "LANGUAGE")) {
    subsector <- if (subject == "MATH") 32001L else 31001L
    slots <- lapply(1:2, function(k) as.integer(
      dt[[paste0("COD_ENS_", k)]] %in% staff_hs_teaching_codes &
        dt[[paste0("SUBSECTOR", k)]] %in% subsector))
    set(dt, j = paste0("ASSIGNED_HS_", subject), value = as.integer(
      dt$TEACHER_ANY %in% 1L & (slots[[1L]] == 1L | slots[[2L]] == 1L)))
  }
  dt[, TITLE_DURATION_MAX := q_max_reported(TITLE_DURATION_SEMESTERS_1, TITLE_DURATION_SEMESTERS_2)]
  dt[, YEARS_SINCE_TITLE_MAX := q_max_reported(YEARS_SINCE_REPORTED_TITLE_1, YEARS_SINCE_REPORTED_TITLE_2)]
  # Dedicated means every appointment at THIS school has only that primary role.
  dt[, COUNSELOR_DEDICATED := as.integer(as.logical(ORIENTADOR_PRIMARY) &
                                          as.logical(staff_role_flag(ID_IFS, 0L, TRUE)))]
  dt[, TEACHER_DEDICATED := as.integer(as.logical(TEACHER_PRIMARY) &
                                        as.logical(staff_role_flag(ID_IFS, 0L, TRUE)))]
  role_fields <- c("ORIENTADOR_ANY", "ORIENTADOR_PRIMARY", "TEACHER_HS_ANY", "TEACHER_HS_PRIMARY", extra_role_fields)
  dedicated_fields <- c("COUNSELOR_DEDICATED", "TEACHER_DEDICATED")
  binary_fields <- c("UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED", "TEACHING_TITLE_REPORTED",
    "HS_TEACHING_TITLE_REPORTED", "TERTIARY_QUALIFICATION_REPORTED", "HAS_SPECIALTY_REPORTED",
    "TRAINED_CFT_REPORTED", "TRAINED_IP_REPORTED", "TRAINED_NORMAL_SCHOOL_REPORTED",
    "TRAINED_OTHER_REPORTED", "MATH_SPECIALIZATION_REPORTED", "LANGUAGE_SPECIALIZATION_REPORTED",
    "MENTION_ORIENTACION_REPORTED", "ORIENTATION_MENTION_EVIDENCE", "ORIENTATION_MENTION_APPLICABLE",
    "ASSIGNED_HS_MATH", "ASSIGNED_HS_LANGUAGE")
  numeric_fields <- c("YEARS_AT_SCHOOL", "REPORTED_SERVICE_SYSTEM_YEARS", "TITLE_DURATION_MAX", "YEARS_SINCE_TITLE_MAX")
  fields <- c(role_fields, dedicated_fields, binary_fields, numeric_fields)
  thin <- dt[, c("RBD", "AGNO", "MRUN", fields), with = FALSE]
  for (field in role_fields) set(thin, j = field, value = fcase(
    thin[[field]] == 1L, 2L, is.na(thin[[field]]), 1L, default = 0L))
  for (field in c(binary_fields, numeric_fields)) set(thin, j = field,
    value = fifelse(is.na(thin[[field]]), -Inf, as.numeric(thin[[field]])))
  # Negated three-valued AND becomes a max reduction: false dominates unknown.
  for (field in dedicated_fields) set(thin, j = field, value = fcase(
    thin[[field]] == 0L, 2L, is.na(thin[[field]]), 1L, default = 0L))
  people <- thin[, lapply(.SD, max), by = .(RBD, AGNO, MRUN), .SDcols = fields]
  mins <- thin[, lapply(.SD, function(x) min(x[x > -Inf], Inf)),
               by = .(RBD, AGNO, MRUN), .SDcols = numeric_fields]
  stopifnot(identical(people[, .(RBD, AGNO, MRUN)], mins[, .(RBD, AGNO, MRUN)]))
  # Reject conflicting within-school numeric reports, rather than selecting a job.
  conflicts <- data.table(FIELD = numeric_fields, N_CONFLICTING_PERSON_SCHOOL_YEARS = 0L)
  for (field in numeric_fields) {
    conflict <- is.finite(mins[[field]]) & is.finite(people[[field]]) & mins[[field]] != people[[field]]
    conflicts[FIELD == field, N_CONFLICTING_PERSON_SCHOOL_YEARS := sum(conflict)]
    set(people, which(conflict), field, NA_real_)
  }
  for (field in role_fields) set(people, j = field, value = staff_decode_or(people[[field]]))
  for (field in dedicated_fields) set(people, j = field,
    value = fcase(people[[field]] == 0L, 1L, people[[field]] == 2L, 0L, default = NA_integer_))
  for (field in c(binary_fields, numeric_fields)) set(people, which(!is.finite(people[[field]])), field, NA_real_)
  roster <- people[, .(
    N_STAFF_IDENTIFIED = .N,
    N_COUNSELOR_IDENTIFIED = sum(ORIENTADOR_ANY == 1L, na.rm = TRUE),
    N_COUNSELOR_UNKNOWN = sum(is.na(ORIENTADOR_ANY)),
    N_TEACHER_IDENTIFIED = sum(TEACHER_HS_ANY == 1L, na.rm = TRUE),
    N_TEACHER_UNKNOWN = sum(is.na(TEACHER_HS_ANY))
  ), by = .(RBD, AGNO)]
  roster[, `:=`(N_COUNSELOR = fifelse(N_COUNSELOR_UNKNOWN == 0L, N_COUNSELOR_IDENTIFIED, NA_integer_),
                 N_TEACHER = fifelse(N_TEACHER_UNKNOWN == 0L, N_TEACHER_IDENTIFIED, NA_integer_))]
  if (!keep_all) people <- people[ORIENTADOR_ANY == 1L | TEACHER_HS_ANY == 1L]
  list(people = people, roster = roster, conflicts = conflicts)
}

q_history_columns <- function() c("MRUN", "AGNO", "RBD", "PERSONAS", "ID_IFP", "VALID_SCHOOL_ID",
  "TEACHER_PRIMARY", "TEACHER_HS_PRIMARY", "ORIENTADOR_PRIMARY", "TEACHER_ANY",
  "TEACHER_HS_ANY", "ORIENTADOR_ANY", "VALID_PERSON_ID")

q_attach_histories <- function(people, history_rows) {
  rows <- history_rows[MRUN %chin% unique(people$MRUN)]
  hist <- staff_build_histories(rows)
  py_fields <- c("HISTORY_FIRST_OBSERVED_YEAR", "HISTORY_PRIOR_OBSERVED_YEARS",
    "N_PRIMARY_ROLE_CHANGES_PRIOR", "N_PRIMARY_ROLE_COMPARISONS_ADJACENT_TO_DATE", "PRIMARY_ROLE_CHANGE_ADJACENT",
    unlist(lapply(c("ORIENTADOR_PRIMARY", "ORIENTADOR_ANY", "TEACHER_HS_ANY"), function(prefix) paste0(prefix, c(
      "_PRIOR_YEARS_OBSERVED", "_CUMULATIVE_YEARS_OBSERVED", "_PRIOR_SHARE",
      "_EXCLUSIVE_HISTORY_TO_DATE", "_CONSECUTIVE_YEARS_TO_DATE", "_PRIOR_KNOWN_YEARS")))))
  sy_fields <- c("SCHOOL_PRIOR_RECORD_YEARS_SINCE_2013", unlist(lapply(
    c("ORIENTADOR_AT_SCHOOL", "TEACHER_HS_AT_SCHOOL"), function(prefix) paste0(prefix,
      c("_PRIOR_YEARS_OBSERVED", "_CONSECUTIVE_YEARS_TO_DATE")))))
  people[hist$person_year, on = .(MRUN, AGNO), (py_fields) := mget(paste0("i.", py_fields))]
  people[hist$person_school_year, on = .(MRUN, RBD, AGNO), (sy_fields) := mget(paste0("i.", sy_fields))]
  # Consistent teacher-history sensitivity excluding less-complete early slots.
  recent <- hist$person_year[AGNO >= 2016L, .(MRUN, AGNO, TEACHER_HS_ANYWHERE_THIS_YEAR)]
  staff_add_role_history(recent, "TEACHER_HS_ANYWHERE_THIS_YEAR", "TEACHER_HS_2016", "MRUN")
  people[recent, on = .(MRUN, AGNO), TEACHER_HS_2016_PRIOR_YEARS := i.TEACHER_HS_2016_PRIOR_YEARS_OBSERVED]
  people[, PRIOR_ROLE_CHANGE_RATE := fifelse(
    N_PRIMARY_ROLE_COMPARISONS_ADJACENT_TO_DATE - as.integer(!is.na(PRIMARY_ROLE_CHANGE_ADJACENT)) > 0,
    N_PRIMARY_ROLE_CHANGES_PRIOR / (N_PRIMARY_ROLE_COMPARISONS_ADJACENT_TO_DATE -
      as.integer(!is.na(PRIMARY_ROLE_CHANGE_ADJACENT))), NA_real_)]
  stopifnot(!anyDuplicated(people, by = c("MRUN", "RBD", "AGNO")))
  people
}

q_metric_dictionary <- function() {
  common <- data.table(
    METRIC = c("prior_role_years", "cumulative_role_years", "prior_school_role_years", "role_spell_years",
      "school_role_spell_years", "prior_role_share", "exclusive_history_share", "primary_role_share",
      "dedicated_primary_share", "prior_main_role_change_rate", "university_share", "teaching_title_share",
      "hs_teaching_title_share", "tertiary_title_share", "specialty_share", "ip_share", "cft_share",
      "normal_school_share", "other_institution_share", "reported_school_tenure", "reported_system_tenure",
      "title_duration_semesters", "years_since_title", "history_known_share"),
    LABEL = c("Prior observed years in role", "Cumulative observed years in role", "Prior role-years at this school",
      "Current observed role spell (years)", "Current role-at-school spell (years)", "Share of prior known years in role",
      "Always in role in observed history", "Primary-function share", "Only primary role across school appointments",
      "Prior main-function switching rate", "University tertiary qualification", "Teaching qualification",
      "HS teaching qualification", "Tertiary qualification", "Recorded title specialty", "IP-trained share",
      "CFT-trained share", "Normal-school-trained share", "Other institution type share", "Reported years at school",
      "Reported years in school system", "Longest reported degree (semesters)", "Years since earliest reported title",
      "Known prior role-history share"),
    BLOCK = c(rep("career", 10), rep("credentials", 9), "tenure_audit", "tenure_audit", "credentials",
              "credentials", "history_audit"))
  out <- rbindlist(lapply(c("counselor", "teacher"), function(role) cbind(ROLE = role, common)))
  out <- rbind(out, data.table(ROLE = "counselor",
    METRIC = c("orientation_mention_evidence_share", "orientation_mention_applicable_rate", "orientation_mention_applicability_share"),
    LABEL = c("Recorded orientation mention (all counselors)", "Orientation mention among applicable titles", "Orientation-mention applicability share"),
    BLOCK = c("credentials", "credentials_conditional", "applicability")))
  out <- rbind(out, data.table(ROLE = "teacher",
    METRIC = c("math_subject_match", "language_subject_match", "prior_hs_years_since2016"),
    LABEL = c("Math credentials among assigned HS math teachers", "Language credentials among assigned HS language teachers",
              "Prior HS years observed since 2016"),
    BLOCK = c("credentials_conditional", "credentials_conditional", "history_sensitivity")))
  out[, CORE_COMPONENT := METRIC %chin% c("prior_role_years", "school_role_spell_years", "university_share") |
        (ROLE == "counselor" & METRIC == "teaching_title_share") |
        (ROLE == "teacher" & METRIC == "hs_teaching_title_share")]
  out
}

q_person_metric_columns <- function(people, role) {
  counselor <- role == "counselor"
  dt <- copy(people[if (counselor) ORIENTADOR_ANY == 1L else TEACHER_HS_ANY == 1L])
  prefix <- if (counselor) "ORIENTADOR_PRIMARY" else "TEACHER_HS_ANY"
  any_prefix <- if (counselor) "ORIENTADOR_ANY" else "TEACHER_HS_ANY"
  school_prefix <- if (counselor) "ORIENTADOR_AT_SCHOOL" else "TEACHER_HS_AT_SCHOOL"
  map <- c(prior_role_years = paste0(prefix, "_PRIOR_YEARS_OBSERVED"),
    cumulative_role_years = paste0(prefix, "_CUMULATIVE_YEARS_OBSERVED"),
    prior_school_role_years = paste0(school_prefix, "_PRIOR_YEARS_OBSERVED"),
    role_spell_years = paste0(any_prefix, "_CONSECUTIVE_YEARS_TO_DATE"),
    school_role_spell_years = paste0(school_prefix, "_CONSECUTIVE_YEARS_TO_DATE"),
    prior_role_share = paste0(prefix, "_PRIOR_SHARE"),
    exclusive_history_share = paste0(prefix, "_EXCLUSIVE_HISTORY_TO_DATE"),
    primary_role_share = if (counselor) "ORIENTADOR_PRIMARY" else "TEACHER_HS_PRIMARY",
    dedicated_primary_share = if (counselor) "COUNSELOR_DEDICATED" else "TEACHER_DEDICATED",
    prior_main_role_change_rate = "PRIOR_ROLE_CHANGE_RATE", university_share = "UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED",
    teaching_title_share = "TEACHING_TITLE_REPORTED", hs_teaching_title_share = "HS_TEACHING_TITLE_REPORTED",
    tertiary_title_share = "TERTIARY_QUALIFICATION_REPORTED", specialty_share = "HAS_SPECIALTY_REPORTED",
    ip_share = "TRAINED_IP_REPORTED", cft_share = "TRAINED_CFT_REPORTED", normal_school_share = "TRAINED_NORMAL_SCHOOL_REPORTED",
    other_institution_share = "TRAINED_OTHER_REPORTED", reported_school_tenure = "YEARS_AT_SCHOOL",
    reported_system_tenure = "REPORTED_SERVICE_SYSTEM_YEARS", title_duration_semesters = "TITLE_DURATION_MAX",
    years_since_title = "YEARS_SINCE_TITLE_MAX")
  for (metric in names(map)) set(dt, j = metric, value = as.numeric(dt[[map[[metric]]]]))
  dt[, history_known_share := fifelse(HISTORY_PRIOR_OBSERVED_YEARS > 0L,
    get(paste0(prefix, "_PRIOR_KNOWN_YEARS")) / HISTORY_PRIOR_OBSERVED_YEARS, NA_real_)]
  if (counselor) {
    # Blank mention slots for known non-applicable titles mean no recorded
    # mention, not a missing all-counselor denominator. The applicable-title
    # rate remains separate; neither establishes all counselor training.
    dt[, `:=`(orientation_mention_evidence_share = fcase(
                 ORIENTATION_MENTION_EVIDENCE == 1L, 1,
                 ORIENTATION_MENTION_APPLICABLE == 0L, 0,
                 ORIENTATION_MENTION_APPLICABLE == 1L, as.numeric(MENTION_ORIENTACION_REPORTED),
                 default = NA_real_),
               orientation_mention_applicable_rate = MENTION_ORIENTACION_REPORTED,
               orientation_mention_applicability_share = ORIENTATION_MENTION_APPLICABLE)]
  } else {
    # No reported teaching qualification is zero recorded teaching specialty,
    # not unknown specialty among the subject's assigned teachers. This does not
    # classify non-teaching degrees (e.g. engineering) as no subject knowledge.
    dt[, `:=`(math_subject_match = fifelse(TEACHING_TITLE_REPORTED == 0L, 0, MATH_SPECIALIZATION_REPORTED),
               language_subject_match = fifelse(TEACHING_TITLE_REPORTED == 0L, 0, LANGUAGE_SPECIALIZATION_REPORTED),
               prior_hs_years_since2016 = TEACHER_HS_2016_PRIOR_YEARS)]
  }
  dt
}

q_aggregate_measures <- function(people, roster, school_ids, years = 2018:2024) {
  dictionary <- q_metric_dictionary()
  annual <- rbindlist(lapply(c("counselor", "teacher"), function(role) {
    members <- q_person_metric_columns(people, role)
    role_roster <- merge(CJ(RBD = school_ids, AGNO = years), roster[, .(
      RBD, AGNO, N_ROLE = get(if (role == "counselor") "N_COUNSELOR" else "N_TEACHER"))],
      by = c("RBD", "AGNO"), all.x = TRUE)
    rbindlist(lapply(dictionary[ROLE == role, METRIC], function(metric) {
      eligible <- switch(metric,
        orientation_mention_applicable_rate = members$ORIENTATION_MENTION_APPLICABLE %in% 1L,
        math_subject_match = members$ASSIGNED_HS_MATH %in% 1L,
        language_subject_match = members$ASSIGNED_HS_LANGUAGE %in% 1L,
        rep(TRUE, nrow(members)))
      thin <- members[eligible, .(RBD, AGNO, VALUE = get(metric))]
      agg <- thin[, .(N_ELIGIBLE = .N, N_OBSERVED = sum(is.finite(VALUE)),
                       MEAN_OBSERVED = if (any(is.finite(VALUE))) mean(VALUE[is.finite(VALUE)]) else NA_real_),
                  by = .(RBD, AGNO)]
      ans <- merge(role_roster, agg, by = c("RBD", "AGNO"), all.x = TRUE)
      ans[is.na(N_ELIGIBLE) & !is.na(N_ROLE), `:=`(N_ELIGIBLE = 0L, N_OBSERVED = 0L)]
      ans[, VALUE := fifelse(!is.na(N_ROLE) & N_ELIGIBLE > 0L & N_OBSERVED / N_ELIGIBLE >= .8,
                             MEAN_OBSERVED, NA_real_)]
      ans[, `:=`(ROLE = role, METRIC = metric)]
      ans
    }))
  }))
  period <- annual[, {
    known <- sum(!is.na(N_ROLE))
    active <- sum(N_ROLE > 0L, na.rm = TRUE)
    eligible_years <- sum(N_ELIGIBLE > 0L, na.rm = TRUE)
    valid <- sum(is.finite(VALUE))
    raw <- if (valid) mean(VALUE[is.finite(VALUE)]) else NA_real_
    .(N_ROLE_YEARS_KNOWN = known, N_ACTIVE_YEARS = active, N_ELIGIBLE_YEARS = eligible_years,
      N_VALID_YEARS = valid, N_ELIGIBLE_PERSON_YEARS = sum(N_ELIGIBLE, na.rm = TRUE),
      N_OBSERVED_PERSON_YEARS = sum(N_OBSERVED, na.rm = TRUE),
      MEAN_OBSERVED_YEARS = raw,
      VALUE = if (known == length(years) && eligible_years > 0L && valid / eligible_years >= .8) raw else NA_real_)
  }, by = .(RBD, ROLE, METRIC)]
  list(annual = annual, period = period, dictionary = dictionary)
}
