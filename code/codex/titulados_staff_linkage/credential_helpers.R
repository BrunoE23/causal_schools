# Observed qualification indicators, not claims about lifetime absence.
credential_names <- c("UG_HIGH_PREMIUM", "ANY_POST_UG", "POST_UG_HIGH_PREMIUM",
  "ANY_HIGH_PREMIUM", "ROLE_SPECIFIC_QUALIFICATION", "ANY_MAGISTER",
  "MAGISTER_HIGH_PREMIUM", "ROLE_SPECIFIC_MAGISTER")

credential_text <- function(x) {
  x <- t_text(x)
  x[is.na(x)] <- ""
  trimws(gsub(" +", " ", gsub("[^A-Z0-9 ]", " ", x)))
}

classify_subjects <- function(d) {
  # Only actual program/title/degree fields are searched, never institution names.
  texts <- lapply(d[, .(NOMB_CARRERA, NOMBRE_TITULO, NOMBRE_GRADO)], credential_text)
  hit <- function(pattern) Reduce(`|`, lapply(texts, grepl, pattern=pattern, perl=TRUE))
  education <- "\\b(EDUCACION|EDUCACIONAL|EDUCACIONALES|EDUCATIV[A-Z]*|PEDAGOG[A-Z]*|DOCENCIA|DOCENTE[A-Z]*|ESCOLAR[A-Z]*|ESCUELA[A-Z]*|COLEGIO[A-Z]*|ENSENANZA|APRENDIZAJE[A-Z]*)\\b"
  teaching <- "\\b(CURRICUL[A-Z]*|DIDACTIC[A-Z]*|PSICOPEDAGOG[A-Z]*|NEUROEDUCACION)\\b"
  # Standalone psychology/family law and generic 'orientation' are not enough.
  orientation <- "\\bORIENTACION (EDUCACIONAL|EDUCATIVA|VOCACIONAL|PROFESIONAL|FAMILIAR|PSICOEDUCACION|Y MEDIACION|(EN )?RELACIONES HUMANAS|FAMILIA|PERSONAL AFECTIVA)\\b|\\bCONSEJERIA (EDUCACIONAL|EDUCATIVA|VOCACIONAL|PROFESIONAL|FAMILIAR)\\b|\\bPSICOEDUC[A-Z]*\\b|\\b(MEDIACION|ASESORAMIENTO|ASESORIA) FAMILIAR\\b|\\bMENCION (EN )?ORIENTACION$"
  management <- "\\b(GESTION|ADMINISTRACION|DIRECCION|LIDERAZGO|DIRECTIV[A-Z]*|GERENCIA)\\b"
  lhits <- lapply(texts, function(x)
    grepl(management,x,perl=TRUE) & grepl(education,x,perl=TRUE))
  ohits <- lapply(texts, function(x) grepl(orientation,x,perl=TRUE) |
    (grepl("\\bMEDIACION\\b",x) & grepl("\\b(FAMILIA|FAMILIAR|FAMILIARES)\\b",x)))
  # A program may list several alternative mentions. A specific awarded mention
  # takes precedence over that menu when neither award field supports the role.
  awarded_mention <- Reduce(`|`,lapply(texts[2:3],grepl,pattern="\\bMENCION\\b"))
  o_awarded <- Reduce(`|`,ohits[2:3]); l_awarded <- Reduce(`|`,lhits[2:3])
  o_conflict <- ohits[[1]] & awarded_mention & !o_awarded
  l_conflict <- lhits[[1]] & awarded_mention & !l_awarded
  leadership <- Reduce(`|`,lhits) & !l_conflict
  orientation_hit <- Reduce(`|`,ohits) & !o_conflict
  teacher_area <- credential_text(d$AREA_CONOCIMIENTO)=="EDUCACION"
  teacher_title <- hit(education) | hit(teaching)
  data.table(
    TEACHER_RELEVANT=as.integer(teacher_area | teacher_title),
    ORIENTADOR_RELEVANT=as.integer(orientation_hit),
    LEADERSHIP_RELEVANT=as.integer(leadership),
    ORIENTADOR_PROGRAM_ONLY=as.integer(orientation_hit & !o_awarded),
    LEADERSHIP_PROGRAM_ONLY=as.integer(leadership & !l_awarded),
    ORIENTADOR_MENTION_CONFLICT=as.integer(o_conflict),
    LEADERSHIP_MENTION_CONFLICT=as.integer(l_conflict),
    TEACHER_RULE=fifelse(teacher_area,"T1: SIES Education area",
      fifelse(teacher_title,"T2: explicit education/teaching subject","No qualifying subject")),
    ORIENTADOR_RULE=fifelse(o_conflict,"O0: different awarded mention overrides program menu",
      fifelse(orientation_hit,"O1: explicit orientation/counseling/psychoeducation/family mediation","No qualifying subject")),
    LEADERSHIP_RULE=fifelse(l_conflict,"L0: different awarded mention overrides program menu",
      fifelse(leadership,"L1: management/leadership AND education context in same field","No qualifying subject")))
}

apply_subject_overrides <- function(a,overrides) {
  # Exact normalized program/title/degree/area signature. Blanks mean missing,
  # never wildcards. Each role override is optional but an explanation is required.
  cols <- c("NOMB_CARRERA","NOMBRE_TITULO","NOMBRE_GRADO","AREA_CONOCIMIENTO")
  roles <- c("TEACHER","ORIENTADOR","LEADERSHIP")
  a <- copy(a)
  a[,SUBJECT_OVERRIDE_REASON:=""]
  if (!nrow(overrides)) return(a)
  stopifnot(all(c(cols,paste0(roles,"_OVERRIDE"),"REASON") %chin% names(overrides)))
  make_key <- function(d) do.call(paste,c(lapply(d[,..cols],credential_text),sep=" || "))
  ak <- make_key(a); ok <- make_key(overrides)
  stopifnot(!anyDuplicated(ok),all(ok %chin% ak),
    all(!is.na(overrides$REASON) & nzchar(trimws(overrides$REASON))))
  index <- match(ak,ok)
  for (r in roles) {
    vals <- overrides[[paste0(r,"_OVERRIDE")]]
    stopifnot(all(is.na(vals) | vals %in% 0:1))
    used <- which(!is.na(index) & !is.na(vals[index]))
    if (length(used)) {
      set(a,i=used,j=paste0(r,"_RELEVANT"),value=as.integer(vals[index[used]]))
      set(a,i=used,j=paste0(r,"_RULE"),value="Explicit reviewed override")
      set(a,i=used,j="SUBJECT_OVERRIDE_REASON",value=overrides$REASON[index[used]])
    }
  }
  a[,`:=`(TEACHER_MAGISTER=ANY_MAGISTER*TEACHER_RELEVANT,
    ORIENTADOR_MAGISTER=ANY_MAGISTER*ORIENTADOR_RELEVANT,
    LEADERSHIP_MAGISTER=ANY_MAGISTER*LEADERSHIP_RELEVANT)]
  a[]
}

prepare_credential_awards <- function(a, effects) {
  a <- copy(a)
  # SIES COD_INST is the first component of the existing MiFuturo key.
  e <- copy(effects[effect_type=="institution"])
  stopifnot(all(e$model_name=="institution_plus_carrera_generica"))
  e[, INSTITUTION_ID:=t_id(sub(" .*", "", level))]
  stopifnot(!anyNA(e$INSTITUTION_ID), !anyDuplicated(e$INSTITUTION_ID),
    !anyNA(e$effect_log_clp_centered))
  e <- e[,.(INSTITUTION_ID, FE_INSTITUTION_KEY=level,
    INSTITUTION_PREMIUM=effect_log_clp_centered)]
  a <- merge(a,e,by="INSTITUTION_ID",all.x=TRUE,sort=FALSE)
  a[, FE_COVERED:=as.integer(!is.na(INSTITUTION_PREMIUM))]
  # Match the existing high_inst_m1 rule: unsupported institution is observed zero.
  a[, HIGH_PREMIUM:=as.integer(FE_COVERED==1L & INSTITUTION_PREMIUM>0.1)]
  sub <- classify_subjects(a)
  a <- cbind(a,sub)
  kind <- credential_text(a$NIVEL_CARRERA_1)
  a[, `:=`(ANY_POST_UG=as.integer(LEVEL %chin% c("postgraduate","postitulo")),
    ANY_MAGISTER=as.integer(kind=="MAGISTER"),
    ANY_DIPLOMADO=as.integer(grepl("^DIPLOMADO\\b",kind)),
    ANY_POSTITULO_STRICT=as.integer(kind=="POSTITULO"),
    ANY_DOCTORATE=as.integer(kind=="DOCTORADO"),
    MATCH_ANY_BY_STAFF_YEAR=1L,
    MATCH_UNDERGRAD_BY_STAFF_YEAR=as.integer(LEVEL=="undergraduate"),
    ANY_UNCOVERED_INSTITUTION=as.integer(FE_COVERED==0L),
    UG_UNCOVERED_INSTITUTION=as.integer(FE_COVERED==0L & LEVEL=="undergraduate"))]
  a[, `:=`(UG_HIGH_PREMIUM=as.integer(LEVEL=="undergraduate" & HIGH_PREMIUM==1L),
    POST_UG_HIGH_PREMIUM=ANY_POST_UG*HIGH_PREMIUM,
    ANY_HIGH_PREMIUM=HIGH_PREMIUM,
    MAGISTER_HIGH_PREMIUM=ANY_MAGISTER*HIGH_PREMIUM,
    POST_UG_UNCOVERED_INSTITUTION=ANY_POST_UG*(1L-FE_COVERED),
    MAGISTER_UNCOVERED_INSTITUTION=ANY_MAGISTER*(1L-FE_COVERED),
    TEACHER_MAGISTER=ANY_MAGISTER*TEACHER_RELEVANT,
    ORIENTADOR_MAGISTER=ANY_MAGISTER*ORIENTADOR_RELEVANT,
    LEADERSHIP_MAGISTER=ANY_MAGISTER*LEADERSHIP_RELEVANT)]
  a[]
}

credential_aux <- c("MATCH_ANY_BY_STAFF_YEAR","MATCH_UNDERGRAD_BY_STAFF_YEAR",
  "ANY_DIPLOMADO","ANY_POSTITULO_STRICT","ANY_DOCTORATE",
  "ANY_UNCOVERED_INSTITUTION","UG_UNCOVERED_INSTITUTION",
  "POST_UG_UNCOVERED_INSTITUTION","MAGISTER_UNCOVERED_INSTITUTION")

build_credential_panel <- function(people,a) {
  stopifnot(!anyDuplicated(people,by=c("MRUN","ROLE","AGNO")),
    all(people$ROLE %chin% c("HS teachers","Non-HS teachers","Orientadores","Leadership")))
  flags <- c(setdiff(credential_names,c("ROLE_SPECIFIC_QUALIFICATION","ROLE_SPECIFIC_MAGISTER")),
    credential_aux,"TEACHER_RELEVANT","ORIENTADOR_RELEVANT","LEADERSHIP_RELEVANT",
    "TEACHER_MAGISTER","ORIENTADOR_MAGISTER","LEADERSHIP_MAGISTER")
  # Earliest qualifying award, independently for each criterion. This avoids
  # joining every staff-year to every award and preserves the unmatched universe.
  out <- copy(people)
  for (f in flags) {
    eligible <- a[get(f)==1L,.(MRUN,ASOF_YEAR)]
    first <- if(nrow(eligible)) eligible[,.(FIRST_YEAR=min(ASOF_YEAR)),by=MRUN] else
      data.table(MRUN=character(),FIRST_YEAR=integer())
    y <- first$FIRST_YEAR[match(out$MRUN,first$MRUN)]
    set(out,j=f,value=as.integer(!is.na(y) & y<=out$AGNO))
  }
  out[,ROLE_SPECIFIC_QUALIFICATION:=fcase(
    ROLE %chin% c("HS teachers","Non-HS teachers"),TEACHER_RELEVANT,
    ROLE=="Orientadores",ORIENTADOR_RELEVANT,ROLE=="Leadership",LEADERSHIP_RELEVANT)]
  out[,ROLE_SPECIFIC_MAGISTER:=fcase(
    ROLE %chin% c("HS teachers","Non-HS teachers"),TEACHER_MAGISTER,
    ROLE=="Orientadores",ORIENTADOR_MAGISTER,ROLE=="Leadership",LEADERSHIP_MAGISTER)]
  out[,c("TEACHER_RELEVANT","ORIENTADOR_RELEVANT","LEADERSHIP_RELEVANT",
    "TEACHER_MAGISTER","ORIENTADOR_MAGISTER","LEADERSHIP_MAGISTER"):=NULL]
  setcolorder(out,c("MRUN","ROLE","AGNO",credential_names,credential_aux,
    setdiff(names(out),c("MRUN","ROLE","AGNO",credential_names,credential_aux))))
  out[]
}
