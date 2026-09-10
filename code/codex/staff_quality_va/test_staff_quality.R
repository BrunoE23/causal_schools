suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
task <- dirname(normalizePath(script, winslash = "/"))
source(file.path(task, "../docentes_educacion/staff_cleaning_helpers.R"))
source(file.path(task, "staff_quality_helpers.R"))
setDTthreads(2L)
checks <- 0L
check <- function(x, label) {
  if (!isTRUE(x)) stop("FAILED: ", label)
  checks <<- checks + 1L
  cat("PASS: ", label, "\n", sep = "")
}
raw <- CJ(AGNO = 2013:2024, MRUN = c("101", "102", "103", "104"))
raw[, `:=`(RBD = 1L, PERSONAS = 1L, ID_IFP = fifelse(MRUN == "101", 9L, 1L), ID_IFS = 0L)]
for (f in setdiff(staff_raw_fields(), names(raw))) set(raw, j = f,
  value = if (f %in% c("NOMBRE_SLEP", "DOC_FEC_NAC")) NA_character_ else NA_integer_)
raw[, `:=`(COD_ENS_1 = 310L, COD_ENS_2 = 0L, NIVEL1 = 4L, NIVEL2 = 0L,
  SUBSECTOR1 = 32001L, TIT_ID_1 = 1L, TIP_TIT_ID_1 = 14L, ESP_ID_1 = 142L,
  TIP_INSTI_ID_1 = 1L, ANO_TITULACION_1 = 2005L, DURACION_CARRERA_1 = 10,
  TIT_ID_2 = 3L, TIP_TIT_ID_2 = 30L, ESP_ID_2 = 300L, TIP_INSTI_ID_2 = 0L,
  ANO_SERVICIO_EE = 0, ANO_SERVICIO_SISTEMA = 0)]
raw[MRUN == "101", c("COD_ENS_1", "NIVEL1", "SUBSECTOR1") := .(0L,0L,0L)]
raw[MRUN == "101" & AGNO >= 2021L, ID_IFP := 4L]
raw[MRUN == "103", `:=`(SUBSECTOR1 = 31002L, ESP_ID_1 = 141L)] # English is not language.
raw[MRUN == "104", `:=`(COD_ENS_1 = 110L, COD_ENS_2 = 310L, NIVEL1 = 2L, NIVEL2 = 4L,
  SUBSECTOR1 = 32001L, SUBSECTOR2 = 31001L, ESP_ID_1 = 141L)] # Math code on basic slot is insufficient.
for (f in grep("^MEN_", names(raw), value = TRUE)) set(raw, j = f, value = 0L)
tmp <- tempfile("staff-quality-tests-"); dir.create(tmp)
cleaned <- rbindlist(lapply(2013:2024, function(year) {
  f <- file.path(tmp, paste0(year, ".csv")); fwrite(raw[AGNO == year], f, sep = ";")
  staff_clean_annual(f)$data
}))
current <- cleaned[AGNO >= 2018L]
duplicate <- copy(current[MRUN == "102" & AGNO == 2018L]); duplicate[, PERSONAS := 0L]
prepared <- q_prepare_people(rbind(current, duplicate), 1L)
check(nrow(prepared$people) == 24L, "Current staff restricted to active roles; appointments deduplicated")
check(all(prepared$people[MRUN == "102", ASSIGNED_HS_MATH] == 1), "Math assignment identified at HS slot")
check(all(prepared$people[MRUN == "103", ASSIGNED_HS_LANGUAGE] == 0), "Foreign-language assignment excluded")
check(all(prepared$people[MRUN == "104", ASSIGNED_HS_MATH] == 0), "Math in non-HS slot not treated as HS math")
check(all(prepared$people[MRUN == "104", ASSIGNED_HS_LANGUAGE] == 1), "Language in second HS slot included")
check(all(prepared$roster$N_TEACHER == 3L), "Teacher roster counts people, not appointments")
check(all(prepared$roster[AGNO >= 2021, N_COUNSELOR] == 0), "Promoted counselor no longer counted as current counselor")
people <- q_attach_histories(prepared$people, cleaned[, q_history_columns(), with = FALSE])
check(people[MRUN == "101" & AGNO == 2020, ORIENTADOR_PRIMARY_PRIOR_YEARS_OBSERVED] == 7L,
      "Counselor experience uses 2013 through previous year")
check(people[MRUN == "102" & AGNO == 2020, TEACHER_HS_2016_PRIOR_YEARS] == 4L,
      "2016-onward history retained as a separate sensitivity")
check(all(people$YEARS_AT_SCHOOL == 0), "Reported tenure preserved, not overwritten by observed history")
past <- q_attach_histories(q_prepare_people(current[AGNO <= 2020], 1L)$people,
                           cleaned[AGNO <= 2020, q_history_columns(), with = FALSE])
check(identical(people[AGNO <= 2020, ORIENTADOR_PRIMARY_PRIOR_YEARS_OBSERVED],
                past$ORIENTADOR_PRIMARY_PRIOR_YEARS_OBSERVED), "Future promotion leaves earlier role experience unchanged")
ans <- q_aggregate_measures(people, prepared$roster, c(1L, 2L))
check(ans$period[RBD == 1 & ROLE == "counselor" & METRIC == "prior_role_years", VALUE] == 6,
      "Counselor period mean averages active years only, not absent-role zeros")
check(ans$period[RBD == 1 & ROLE == "counselor" & METRIC == "prior_role_years", N_ACTIVE_YEARS] == 3L,
      "Active-year support retained")
check(all(is.na(ans$period[RBD == 2, VALUE])), "Unobserved school not turned into zero attributes")
check(ans$period[RBD == 1 & ROLE == "teacher" & METRIC == "math_subject_match", VALUE] == 1,
      "Math credential rate uses math-assigned teachers as denominator")
check(ans$period[RBD == 1 & ROLE == "teacher" & METRIC == "language_subject_match", VALUE] == 1,
      "Language credential rate uses relevant language teachers only")
check(is.na(ans$period[RBD == 1 & ROLE == "counselor" & METRIC == "orientation_mention_applicable_rate", VALUE]),
      "Non-applicable orientation mentions not presented as zero qualification")
check(ans$period[RBD == 1 & ROLE == "counselor" & METRIC == "orientation_mention_evidence_share", VALUE] == 0,
      "Known non-applicability stays in all-counselor recorded-evidence denominator")
missing <- copy(people); missing[MRUN == "102" & AGNO == 2018, UNIVERSITY_TERTIARY_QUALIFICATION_REPORTED := NA_real_]
gated <- q_aggregate_measures(missing, prepared$roster, 1L)
check(is.na(gated$annual[ROLE == "teacher" & AGNO == 2018 & METRIC == "university_share", VALUE]),
      "Annual mean requires at least 80 percent member coverage")
check(gated$period[ROLE == "teacher" & METRIC == "university_share", VALUE] == 1,
      "Six of seven valid years pass explicit period gate")
conflict <- copy(duplicate); conflict[, YEARS_AT_SCHOOL := 8]
revised <- q_prepare_people(rbind(current, conflict), 1L)
check(is.na(revised$people[MRUN == "102" & AGNO == 2018, YEARS_AT_SCHOOL]), "Conflicting numeric appointments become missing")
check(revised$conflicts[FIELD == "YEARS_AT_SCHOOL", N_CONFLICTING_PERSON_SCHOOL_YEARS] == 1L,
      "Numeric conflicts are audited")
check(sum(ans$dictionary$CORE_COMPONENT) == 8L, "Four prespecified core components per staff group")
check(all(ans$dictionary[CORE_COMPONENT == TRUE & BLOCK == "career", METRIC] %in%
            c("prior_role_years", "school_role_spell_years")), "Core school experience does not require incumbent-only prior history")
no_title <- copy(people)
no_title[MRUN == "102", `:=`(TEACHING_TITLE_REPORTED = 0, MATH_SPECIALIZATION_REPORTED = NA_real_)]
check(all(q_person_metric_columns(no_title, "teacher")[MRUN == "102", math_subject_match] == 0),
      "No reported teaching title stays in subject-matching denominator with zero recorded teaching specialty")

source(file.path(task, "staff_association_helpers.R"))
set.seed(7391)
fake <- data.table(RBD = 1:240, N_ACTIVE_YEARS = 7L, prior_role_years = rnorm(240),
  school_role_spell_years = rnorm(240), university_share = runif(240), teaching_title_share = runif(240), VA = rnorm(240))
fit <- q_fit_indices(copy(fake), "counselor")
altered <- copy(fake); altered[, VA := VA*100 + 1000]
fit2 <- q_fit_indices(altered, "counselor")
check(identical(fit$wide$balanced_index, fit2$wide$balanced_index), "Staff-index weights do not use VA outcomes")
check(abs(mean(fit$wide$balanced_index)) < 1e-12 && abs(sd(fit$wide$balanced_index)-1) < 1e-12,
      "Balanced index centered and scaled over declared complete-case schools")
check(abs(sd(fit$wide$career_index)-1) < 1e-12 && abs(sd(fit$wide$credentials_index)-1) < 1e-12,
      "Blocks standardized before equal weighting")
check(cor(fit$wide$pca_index, fit$wide$balanced_index) >= 0, "PCA sign aligned with staff index, not outcomes")
missing_fake <- copy(fake); missing_fake[1, university_share := NA_real_]; missing_fake[2, N_ACTIVE_YEARS := 2L]
limited <- q_fit_indices(missing_fake, "counselor")
check(all(is.na(limited$wide$balanced_index[1:2])), "No imputed core components or low-active-year indices")
check(abs(q_weighted_cor(fake$prior_role_years, fake$university_share, rep(1,240)) -
            cor(fake$prior_role_years, fake$university_share)) < 1e-12, "Equal-weight correlation matches ordinary Pearson")
ad <- data.table(X = rnorm(240), N_VA_STUDENTS = exp(runif(240, 4, 8)), COD_DEPE = 1L,
  COD_REG_RBD = 1L, RURAL_RBD = 0L, HAS_TP_OR_ARTISTIC = 0L, HAS_BASIC = 1L)
ad[, Y := .4*X + .2*log(N_VA_STUDENTS) + rnorm(240)]
adj <- q_adjusted_association(ad)
ad[, `:=`(X_Z=q_z(X), Y_Z=q_z(Y), LOG_N=log(N_VA_STUDENTS))]
lm_check <- lm(Y_Z ~ X_Z + LOG_N + I(LOG_N^2), data=ad)
design <- model.matrix(lm_check); bread <- solve(crossprod(design))
manual_hc1 <- bread %*% crossprod(design*residuals(lm_check)) %*% bread * nrow(ad)/df.residual(lm_check)
check(abs(adj$ADJUSTED_BETA_SD - coef(lm_check)["X_Z"]) < 1e-12, "Adjusted standardized coefficient matches independent lm")
check(abs(adj$ADJUSTED_SE_HC1 - sqrt(manual_hc1["X_Z","X_Z"])) < 1e-10,
      "HC1 inference matches exact regression sandwich covariance")
check(adj$N_ADJUSTED == 240L, "Constant categorical controls safely omitted without dropping schools")
cat("All ", checks, " staff-quality construction checks passed.\n", sep = "")
