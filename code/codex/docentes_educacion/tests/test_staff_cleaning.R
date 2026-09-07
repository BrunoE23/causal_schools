suppressPackageStartupMessages(library(data.table))
test_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task_dir <- normalizePath(file.path(dirname(test_file), ".."), winslash = "/", mustWork = TRUE)
source(file.path(task_dir, "staff_cleaning_helpers.R"))
setDTthreads(2L)
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop("FAILED: ", label)
  checks <<- checks + 1L
  cat("PASS: ", label, "\n", sep = "")
}
fixture_dir <- tempfile("staff-cleaning-tests-")
dir.create(fixture_dir)
# Fixtures contain invented numeric identifiers only, never research records.
make_raw <- function(id, years, rbd = 100L, primary = 9L, secondary = 0L, main = 1L) {
  dt <- data.table(AGNO = as.integer(years), MRUN = as.character(id),
                   RBD = as.integer(rbd), ID_IFP = as.integer(primary),
                   ID_IFS = as.integer(secondary), PERSONAS = as.integer(main))
  for (field in setdiff(staff_raw_fields(), names(dt))) {
    set(dt, j = field, value = if (field %in% c("NOMBRE_SLEP", "DOC_FEC_NAC")) NA_character_ else NA_integer_)
  }
  dt[, c("TIT_ID_1", "TIP_TIT_ID_1", "ESP_ID_1", "TIP_INSTI_ID_1") := .(1L, 14L, 142L, 1L)]
  dt[, c("TIT_ID_2", "TIP_TIT_ID_2", "ESP_ID_2", "TIP_INSTI_ID_2") := .(0L, 0L, 0L, 0L)]
  dt[, c("ANO_TITULACION_1", "DURACION_CARRERA_1", "MODALIDAD_ESTUDIO_1") := .(2005L, 10L, 1L)]
  for (field in grep("^MEN_", names(dt), value = TRUE)) set(dt, j = field, value = 0L)
  dt
}
clean_fixture <- function(raw, label) {
  ans <- lapply(sort(unique(raw$AGNO)), function(year) {
    path <- file.path(fixture_dir, paste0(label, "_", year, ".csv"))
    fwrite(raw[AGNO == year], path, sep = ";", na = "")
    staff_clean_annual(path)$data
  })
  rbindlist(ans, use.names = TRUE)
}

# Four orientador years, then director. Future observations cannot alter the past.
promotion <- clean_fixture(make_raw("10001", 2018:2022,
                                    primary = c(9L, 9L, 9L, 9L, 4L)), "promotion")
full <- staff_build_histories(promotion)
past <- staff_build_histories(promotion[AGNO <= 2021L])
check(isTRUE(all.equal(full$person_year[AGNO <= 2021L], past$person_year)),
      "Future promotion leaves every earlier person-year feature unchanged")
check(isTRUE(all.equal(full$person_school_year[AGNO <= 2021L], past$person_school_year)),
      "Future promotion leaves every earlier school-history feature unchanged")
check(full$person_year[AGNO == 2021L, ORIENTADOR_MAIN_EXCLUSIVE_HISTORY_TO_DATE] == 1L,
      "Four observed orientador years remain specialized before promotion")
check(full$person_year[AGNO == 2022L, ORIENTADOR_ANY_PRIOR_YEARS_OBSERVED] == 4L,
      "Director retains four prior orientador years")
check(full$person_year[AGNO == 2022L, N_PRIMARY_ROLE_CHANGES_ADJACENT_TO_DATE] == 1L,
      "Promotion is recorded as a role change, without a quality penalty")
check(is.na(full$person_year[AGNO == 2018L, ORIENTADOR_ANY_PRIOR_YEARS_OBSERVED]),
      "First observation has unknown prior experience, not zero")
check(is.na(full$person_year[AGNO == 2018L, ORIENTADOR_MAIN_EXCLUSIVE_HISTORY_TO_DATE]),
      "Single observed year is not classified as permanent")
check(full$person_year[AGNO == 2018L, ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED] == 1L,
      "Cumulative observed primary-role experience includes the first current year")
check(full$person_year[AGNO == 2022L, ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED] == 4L &&
        full$person_year[AGNO == 2022L, ORIENTADOR_PRIMARY_CONSECUTIVE_YEARS_TO_DATE] == 0L,
      "Promotion stops the primary-role spell but preserves accumulated experience")

# Secondary orientador; multiple simultaneous jobs count once per person-year.
multi_raw <- rbindlist(list(
  make_raw("10002", 2018:2020, rbd = 100L, primary = 1L, secondary = 9L),
  make_raw("10002", 2018:2020, rbd = 200L, primary = 1L, secondary = 9L, main = 0L)
))
multi <- clean_fixture(multi_raw, "multi")
multi_hist <- staff_build_histories(multi)
check(all(multi$ORIENTADOR_SECONDARY_ONLY == 1L), "Secondary-only orientador is retained")
check(multi_hist$person_year[AGNO == 2020L, ORIENTADOR_ANY_PRIOR_YEARS_OBSERVED] == 2L,
      "Simultaneous appointments do not double-count years in role")
check(multi_hist$person_year[AGNO == 2020L, ORIENTADOR_MAIN_PRIOR_YEARS_OBSERVED] == 0L,
      "Primary-role and any-appointment experience remain distinct")
check(multi_hist$person_year[AGNO == 2020L, ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED] == 0L &&
        multi_hist$person_year[AGNO == 2020L, ORIENTADOR_ANY_CUMULATIVE_YEARS_OBSERVED] == 3L,
      "Secondary-only orientador years do not count as primary-function experience")
check(all(multi_hist$person_school_year[AGNO == 2020L,
                                      ORIENTADOR_AT_SCHOOL_PRIOR_YEARS_OBSERVED] == 2L),
      "Role-and-school experience is tracked separately at both schools")
dup_other <- rbindlist(list(multi, multi[PERSONAS == 0L]))
check(staff_build_histories(dup_other)$person_year[
  AGNO == 2020L, ORIENTADOR_ANY_PRIOR_YEARS_OBSERVED] == 2L,
  "Repeated non-main appointment rows do not inflate role experience")
dup_main <- rbindlist(list(multi, multi[PERSONAS == 1L][1L]))
err <- tryCatch({ staff_build_histories(dup_main); "" }, error = conditionMessage)
check(grepl("Duplicate", err), "Conflicting main person-years fail explicitly")

# Moves and gaps: distinguish cumulative experience from consecutive spells.
mover <- clean_fixture(make_raw("10003", 2018:2021,
                                rbd = c(100L, 100L, 200L, 100L)), "mover")
mover_hist <- staff_build_histories(mover)$person_school_year
check(mover_hist[AGNO == 2021L, ORIENTADOR_AT_SCHOOL_PRIOR_YEARS_OBSERVED] == 2L,
      "Returning to a school preserves accumulated prior experience there")
check(mover_hist[AGNO == 2021L, ORIENTADOR_AT_SCHOOL_CONSECUTIVE_YEARS_TO_DATE] == 1L,
      "Returning to a school starts a new observed consecutive spell")
check(mover_hist[AGNO == 2021L, ORIENTADOR_AT_SCHOOL_CUMULATIVE_YEARS_OBSERVED] == 3L,
      "Returning to a school adds to cumulative experience without extending the spell")
check(staff_build_histories(mover)$person_year[
  AGNO == 2021L, ORIENTADOR_ANY_CONSECUTIVE_YEARS_TO_DATE] == 4L,
  "Moving school does not reset an uninterrupted person-level orientador spell")

primary_nonmain_raw <- rbindlist(list(
  make_raw("10005", 2013:2015, rbd = 100L, primary = 1L),
  make_raw("10005", 2013:2015, rbd = 200L, primary = 9L, main = 0L),
  make_raw("10005", 2013:2015, rbd = 300L, primary = 9L, main = 0L)
))
primary_nonmain <- staff_build_histories(clean_fixture(primary_nonmain_raw, "primary_nonmain"))$person_year
check(primary_nonmain[AGNO == 2015L, ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED] == 3L &&
        primary_nonmain[AGNO == 2015L, ORIENTADOR_MAIN_CUMULATIVE_YEARS_OBSERVED] == 0L,
      "Primary function at a non-main appointment counts once, separately from PERSONAS=1")
check(primary_nonmain[AGNO == 2015L, ORIENTADOR_PRIMARY_PRIOR_YEARS_OBSERVED] == 2L,
      "Prior primary-role count excludes the current year")
gap_primary <- staff_build_histories(clean_fixture(make_raw("10006", c(2013L, 2015L)), "gap_primary"))$person_year
check(gap_primary[AGNO == 2015L, ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED] == 2L &&
        gap_primary[AGNO == 2015L, ORIENTADOR_PRIMARY_CONSECUTIVE_YEARS_TO_DATE] == 1L,
      "An unobserved year breaks the spell but does not add a cumulative year")
unknown_primary <- staff_build_histories(clean_fixture(make_raw("10007", 2013L,
  primary = NA_integer_, secondary = 9L), "unknown_primary"))$person_year
check(is.na(unknown_primary$ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED) &&
        unknown_primary$ORIENTADOR_ANY_CUMULATIVE_YEARS_OBSERVED == 1L,
      "Unknown primary function stays unknown while a recorded secondary role counts")
gap <- clean_fixture(make_raw("10004", c(2018L, 2020L), primary = c(9L, 4L)), "gap")
gap_hist <- staff_build_histories(gap)$person_year
check(gap_hist[AGNO == 2020L, HISTORY_PRIOR_MISSING_CALENDAR_YEARS] == 1L,
      "Calendar gaps remain visible")
check(is.na(gap_hist[AGNO == 2020L, PRIMARY_ROLE_CHANGE_ADJACENT]),
      "A change across a gap is not called a consecutive-year transition")
check(gap_hist[AGNO == 2020L, PRIMARY_ROLE_CHANGED_SINCE_LAST_OBS] == 1L,
      "Endpoint change across a gap is retained separately")

# Credentials: unknown, explicit no title, applicability, and real specialties.
credentials <- rbindlist(lapply(10010:10016, function(id) make_raw(id, 2020L)))
credentials[MRUN == "10010", c("TIT_ID_1", "TIP_TIT_ID_1", "ESP_ID_1", "TIP_INSTI_ID_1") := .(0L, 0L, 0L, 0L)]
credentials[MRUN == "10011", c("TIT_ID_1", "TIP_TIT_ID_1", "ESP_ID_1", "TIP_INSTI_ID_1") := .(3L, 31L, 310L, 0L)]
credentials[MRUN == "10012", c("TIP_TIT_ID_1", "ESP_ID_1", "MEN_ORIENTACION_1") := .(13L, 130L, 1L)]
credentials[MRUN == "10013", TIP_INSTI_ID_1 := 3L]
credentials[MRUN == "10014", ANO_TITULACION_1 := 2030L]
credentials[MRUN == "10015", ESP_ID_1 := 99999L]
credentials[MRUN == "10016", c("TIP_TIT_ID_1", "ESP_ID_1", "MEN_MATE_1", "MEN_LENGUAJE_1", "MEN_ORIENTACION_1") :=
              .(13L, 130L, NA_integer_, NA_integer_, NA_integer_)]
creds <- clean_fixture(credentials, "credentials")
check(is.na(creds[MRUN == "10010", HAS_TITLE_REPORTED]), "TIT_ID=0 is missing information")
check(creds[MRUN == "10011", HAS_TITLE_REPORTED] == 0L, "TIT_ID=3 is explicit no title")
check(creds[MRUN == "10011", TRAINED_UNIVERSITY_REPORTED] == 0L,
      "Explicit no title is not university training")
check(creds[MRUN == "10012", MENTION_ORIENTACION_REPORTED] == 1L,
      "Applicable orientation mention is retained")
check(creds[MRUN == "10013", MATH_SPECIALIZATION_REPORTED] == 1L,
      "HS math specialty counts even with zero math mention")
check(is.na(creds[MRUN == "10013", MENTION_ORIENTACION_REPORTED]),
      "Non-basic title has unknown/not-applicable orientation mention, not zero")
check(creds[MRUN == "10013", TRAINED_IP_REPORTED] == 1L &&
        creds[MRUN == "10013", TRAINED_UNIVERSITY_REPORTED] == 0L,
      "Institution categories remain distinct without a numerical quality ranking")
check(creds[MRUN == "10014", TITLE_YEAR_IN_FUTURE_1] == 1L &&
        is.na(creds[MRUN == "10014", YEARS_SINCE_REPORTED_TITLE_1]),
      "Future-dated qualification does not create negative experience")
check(creds[MRUN == "10015", SPECIALTY_CODE_UNMAPPED_1] == 1L &&
        is.na(creds[MRUN == "10015", HAS_MATH_TITLE_SPECIALTY_REPORTED]),
      "Unknown specialty is flagged and not silently classified")
check(is.na(creds[MRUN == "10016", N_MENCIONES_APPLICABLE_1]),
      "Incomplete applicable mention array is not presented as a complete zero count")
check(!"CLAVE" %in% names(creds) && !"ID_IFP2" %in% names(creds),
      "Unrequested identifiers and grouped-function code stay excluded")

invalid <- clean_fixture(rbindlist(list(make_raw("0", 2020L), make_raw("10020", 2020L))),
                         "invalid_id")
check(nrow(staff_build_histories(invalid)$person_year) == 1L,
      "Invalid identifiers are retained in cleaning but never joined as one person")
invalid_role <- clean_fixture(make_raw("10021", 2020L, primary = 0L), "invalid_role")
check(is.na(invalid_role$ORIENTADOR_ANY), "Invalid primary function does not become known absence")
missing_school_raw <- make_raw("10022", 2018:2019, rbd = c(NA_integer_, 100L))
missing_school <- staff_build_histories(clean_fixture(missing_school_raw, "missing_school"))
check(missing_school$person_year[AGNO == 2019L, ORIENTADOR_ANY_PRIOR_YEARS_OBSERVED] == 1L,
      "Unknown school does not erase known experience in the role")
check(nrow(missing_school$person_school_year) == 1L,
      "Unknown school is excluded only from school-specific history")
fractional_raw <- make_raw("10023", 2020L)
fractional_raw[, c("DURACION_CARRERA_1", "HORAS1") := .(9.5, 22.5)]
fractional <- clean_fixture(fractional_raw, "fractional")
check(fractional$TITLE_DURATION_SEMESTERS_1 == 9.5 && fractional$HORAS1 == 22.5,
      "Measured hours and duration are preserved without integer truncation")
break_raw <- make_raw("10024", 2014:2015, primary = c(4L, 16L))
break_hist <- staff_build_histories(clean_fixture(break_raw, "code_break"))$person_year
check(break_hist[AGNO == 2015L, PRIMARY_ROLE_CHANGE_POSSIBLE_2015_CODE_BREAK] == 1L,
      "2015 director/encargado coding break is flagged")
check(promotion[AGNO == 2018L, ORIENTADOR_PRIMARY_NO_SECONDARY] == 1L &&
        all(multi$ORIENTADOR_PRIMARY_NO_SECONDARY == 0L),
      "Primary-only current assignment is distinct from secondary orientador")
check(creds[MRUN == "10013", UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED] == 0L,
      "University tertiary qualification remains separate from other institution types")
check(identical(staff_or(c(1L, 0L, 0L), c(NA_integer_, NA_integer_, 0L)),
                c(1L, NA_integer_, 0L)), "Role flags use three-valued logic")

# Reported school tenure is distinct from role history observed since 2013.
tenure_raw <- make_raw("10025", 2012:2018)
tenure_raw[, ANO_SERVICIO_EE := as.numeric(AGNO - 2006L)]
tenure_raw[, ANO_SERVICIO_SISTEMA := as.numeric(AGNO - 2000L)]
tenure <- clean_fixture(tenure_raw, "reported_tenure")
tenure_hist <- staff_build_histories(tenure)$person_year
check(tenure[AGNO == 2013L, YEARS_AT_SCHOOL] == 7L,
      "Reported school tenure is retained at the start of the history window")
check(tenure[AGNO == 2018L, YEARS_AT_SCHOOL] == 12L,
      "Reported school tenure is not capped by the observed panel length")
check(tenure_hist[AGNO == 2018L, ORIENTADOR_ANY_PRIOR_YEARS_OBSERVED] == 5L &&
        tenure_hist[AGNO == 2018L, ORIENTADOR_MAIN_PRIOR_YEARS_OBSERVED] == 5L,
      "Prior role experience counts 2013 through t-1, excluding pre-2013 records")
check(all(tenure_hist$HISTORY_WINDOW_START_YEAR == 2013L) && min(tenure_hist$AGNO) == 2013L,
      "Role-history output explicitly records and enforces its 2013 start")
tenure_raw[AGNO == 2018L, ANO_SERVICIO_EE := NA_real_]
missing_tenure <- clean_fixture(tenure_raw, "missing_tenure")
check(is.na(missing_tenure[AGNO == 2018L, YEARS_AT_SCHOOL]),
      "Missing reported tenure is not filled with observed panel years")
tenure_raw[AGNO == 2018L, ANO_SERVICIO_EE := -1]
negative_tenure <- clean_fixture(tenure_raw, "negative_tenure")
check(is.na(negative_tenure[AGNO == 2018L, YEARS_AT_SCHOOL]) &&
        negative_tenure[AGNO == 2018L, ANO_SERVICIO_EE] == -1,
      "Negative tenure is missing in the preferred measure but preserved raw")
tenure_raw[AGNO == 2018L, ANO_SERVICIO_EE := 0]
zero_tenure <- clean_fixture(tenure_raw, "zero_tenure")
check(zero_tenure[AGNO == 2018L, YEARS_AT_SCHOOL] == 0L,
      "Explicit zero reported school tenure remains zero")
new_school_raw <- make_raw("10026", 2013:2018, rbd = c(rep(100L, 5L), 200L))
new_school_raw[, ANO_SERVICIO_EE := 7]
new_school <- clean_fixture(new_school_raw, "new_school_tenure")
check(new_school[AGNO == 2018L, YEARS_AT_SCHOOL] == 7L,
      "First observed record at a school keeps its reported school tenure")
check(staff_build_histories(new_school)$person_school_year[
  AGNO == 2018L, SCHOOL_PRIOR_RECORD_YEARS_SINCE_2013] == 0L,
  "School record coverage stays separate from reported years at the school")

# HS eligibility follows actual teaching assignments, never title or school type.
hs_raw <- rbindlist(lapply(10040:10049, function(id) make_raw(id, 2018L, primary = 1L)))
hs_raw[, c("COD_ENS_1", "COD_ENS_2", "NIVEL1", "NIVEL2") := .(110L, 0L, 2L, 0L)]
hs_raw[MRUN == "10040", c("COD_ENS_1", "NIVEL1") := .(310L, 4L)]
hs_raw[MRUN == "10041", c("COD_ENS_2", "NIVEL2") := .(410L, 5L)]
hs_raw[MRUN == "10042", c("COD_ENS_1", "NIVEL1", "ID_IFP", "ID_IFS") := .(910L, 5L, 9L, 1L)]
hs_raw[MRUN == "10043", c("COD_ENS_1", "NIVEL1", "PERSONAS") := .(610L, 5L, 0L)]
hs_raw[MRUN == "10044", c("COD_ENS_1", "NIVEL1") := .(310L, 2L)]
hs_raw[MRUN == "10046", c("COD_ENS_1", "NIVEL1") := .(363L, 7L)]
hs_raw[MRUN == "10047", c("COD_ENS_1", "COD_ENS_2", "NIVEL1") := .(NA_integer_, NA_integer_, 4L)]
hs_raw[MRUN == "10048", COD_ENS_1 := 9999L]
hs_raw[MRUN == "10049", ID_IFS := 9L]
hs <- clean_fixture(hs_raw, "hs_assignments")
check(hs[MRUN == "10040", TEACHER_HS_ANY] == 1L, "Regular media H-C teacher qualifies")
check(hs[MRUN == "10041", TEACHER_HS_ANY] == 1L,
      "HS teaching in slot two qualifies even when slot one is basic education")
check(hs[MRUN == "10042", TEACHER_HS_SECONDARY_ONLY] == 1L,
      "Secondary teaching function with an artistic HS assignment qualifies")
check(hs[MRUN == "10043", TEACHER_HS_ANY] == 1L,
      "HS teaching at a non-main appointment is retained")
check(hs[MRUN == "10044", TEACHER_HS_ANY] == 1L &&
        hs[MRUN == "10044", TEACHING_LEVEL_MISMATCH_1] == 1L,
      "COD_ENS controls eligibility and conflicting NIVEL is audited")
check(hs[MRUN == "10045", TEACHER_HS_ANY] == 0L &&
        hs[MRUN == "10045", MATH_SPECIALIZATION_REPORTED] == 1L,
      "HS degree specialization does not qualify a basic-only teaching assignment")
check(hs[MRUN == "10046", TEACHER_HS_ANY] == 0L,
      "Adult-only media is outside the regular HS VA teaching scope")
check(is.na(hs[MRUN == "10047", TEACHER_HS_ANY]),
      "Missing teaching codes stay unknown even when NIVEL suggests HS")
check(is.na(hs[MRUN == "10048", TEACHER_HS_ANY]) &&
        hs[MRUN == "10048", TEACHING_CODE_UNMAPPED_1] == 1L,
      "Unmapped teaching codes are audited and do not become known absence")
check(hs[MRUN == "10049", TEACHER_HS_ANY] == 0L &&
        hs[MRUN == "10049", ORIENTADOR_ANY] == 1L,
      "Basic-only teacher can remain eligible separately as an orientador")
code_cases <- make_raw("10050", 2018L, primary = 1L)[rep(1L, 7L)]
code_cases[, c("COD_ENS_1", "COD_ENS_2") := .(staff_hs_teaching_codes, 0L)]
check(all(clean_fixture(code_cases, "hs_all_codes")$TEACHER_HS_ANY == 1L),
      "All seven regular HS teaching codes qualify")

hs_career_raw <- make_raw("10060", 2013:2018, primary = 1L)
hs_career_raw[, c("COD_ENS_1", "COD_ENS_2") := .(c(110L, 110L, rep(310L, 4L)), 0L)]
hs_career <- clean_fixture(hs_career_raw, "hs_career")
hs_career_hist <- staff_build_histories(hs_career)
check(hs_career_hist$person_year[AGNO == 2018L, TEACHER_HS_ANY_PRIOR_YEARS_OBSERVED] == 3L &&
        hs_career_hist$person_year[AGNO == 2018L, TEACHER_ANY_PRIOR_YEARS_OBSERVED] == 5L,
      "HS-specific history excludes basic teaching years without erasing broad career history")
check(hs_career_hist$person_school_year[
  AGNO == 2018L, TEACHER_HS_AT_SCHOOL_PRIOR_YEARS_OBSERVED] == 3L,
  "Role-at-school history uses the HS-specific teaching definition")
check(isTRUE(all.equal(hs_career_hist$person_year[AGNO <= 2017L],
                      staff_build_histories(hs_career[AGNO <= 2017L])$person_year)),
      "HS-specific histories do not use future observations")
hs_other_school_raw <- rbindlist(list(
  make_raw("10061", 2013:2018, rbd = 100L, primary = 1L),
  make_raw("10061", 2013:2018, rbd = 200L, primary = 1L, main = 0L)
))
hs_other_school_raw[, c("COD_ENS_1", "COD_ENS_2") := .(ifelse(RBD == 100L, 110L, 310L), 0L)]
hs_other_school <- clean_fixture(hs_other_school_raw, "hs_other_school")
hs_other_hist <- staff_build_histories(hs_other_school)
check(all(hs_other_school[RBD == 100L, TEACHER_HS_ANY] == 0L) &&
        all(hs_other_school[RBD == 200L, TEACHER_HS_ANY] == 1L),
      "HS teaching at another school does not qualify a basic-only appointment")
check(hs_other_hist$person_year[AGNO == 2018L, TEACHER_HS_ANY_PRIOR_YEARS_OBSERVED] == 5L &&
        hs_other_hist$person_year[AGNO == 2018L, TEACHER_HS_MAIN_PRIOR_YEARS_OBSERVED] == 0L,
      "HS teaching history counts non-main appointments once per person-year")

# End-to-end CLI with small invented annual files, including excluded year 2025.
input_dir <- file.path(fixture_dir, "input")
output_dir <- file.path(fixture_dir, "output")
dir.create(input_dir)
for (year in c(2018:2022, 2025L)) {
  annual_raw <- make_raw("10030", year)
  annual_raw[, ANO_SERVICIO_EE := as.numeric(year - 2008L)]
  if (year == 2018L) {
    hs_raw[, ANO_SERVICIO_EE := as.numeric(year - 2008L)]
    annual_raw <- rbindlist(list(annual_raw, hs_raw))
  }
  fwrite(annual_raw, file.path(input_dir, paste0("annual_", year, ".csv")), sep = ";")
}
before <- tools::md5sum(list.files(input_dir, full.names = TRUE))
rscript <- file.path(R.home("bin"), "Rscript.exe")
if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
entry <- file.path(task_dir, "01_clean_docentes_educacion.R")
invoke <- function(extra = character()) suppressWarnings(system2(
  rscript, c("--vanilla", shQuote(entry), shQuote(input_dir), shQuote(output_dir), extra),
  stdout = TRUE, stderr = TRUE
))
result <- invoke()
if (!is.null(attr(result, "status"))) cat(paste(result, collapse = "\n"), "\n")
check(is.null(attr(result, "status")), "End-to-end cleaner completes on synthetic annual files")
check(identical(before, tools::md5sum(names(before))), "Cleaner leaves raw inputs byte-identical")
features <- fread(file.path(output_dir, "docentes_educacion_va_2018_2024_staff_features.csv.gz"))
check(all(features$AGNO %in% 2018:2024) && !any(features$AGNO == 2025L),
      "2025 is excluded from the VA-window feature output")
check(!any(grepl("^EVER_", names(features))), "Future-dependent EVER flags never enter VA features")
check(all(features[MRUN == 10030, ORIENTADOR_PRIMARY_CUMULATIVE_YEARS_OBSERVED] == 1:5),
      "Exported VA features carry the explicit cumulative primary-role measure")
check(all(features$YEARS_AT_SCHOOL == features$AGNO - 2008L),
      "Exported VA features use reported school tenure without history replacement")
check(setequal(features[VA_TEACHER_ELIGIBLE == 1L, MRUN], 10040:10044),
      "Exported teacher roster includes only confirmed HS teaching appointments")
check(features[MRUN == 10049, VA_ORIENTADOR_ELIGIBLE] == 1L &&
        features[MRUN == 10049, VA_TEACHER_ELIGIBLE] == 0L,
      "Combined VA file does not let a basic-only orientador enter the teacher measure")
check(!any(features$MRUN %in% 10045:10048),
      "Basic-only, adult-only, missing and unmapped teacher assignments are excluded")
existing <- invoke()
check(!is.null(attr(existing, "status")) && any(grepl("already exist", existing)),
      "Rerun refuses to overwrite existing outputs without explicit flag")
sampled <- invoke(c("--sample-rows=1"))
check(!is.null(attr(sampled, "status")) && any(grepl("requires --dry-run", sampled)),
      "Sampled test runs cannot export misleading full-panel files")
cat("\n", checks, " checks passed. Synthetic fixtures: ", fixture_dir, "\n", sep = "")
