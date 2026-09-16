suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE))
task <- dirname(normalizePath(script,winslash="/"))
source(file.path(task,"linkage_helpers.R"))
source(file.path(task,"credential_helpers.R"))
subjects <- data.table(NOMB_CARRERA=c("MAGISTER EN DESARROLLO CURRICULAR",
  "MAGISTER EN ORIENTACION, PSICOEDUCACION Y FAMILIA",
  "MAGISTER EN GESTION EDUCACIONAL", "MASTER OF BUSINESS ADMINISTRATION",
  "PSICOLOGIA", "DIPLOMA EN DERECHO DE FAMILIA", "POSTITULO EN ORIENTACION EDUCACIONAL",
  "DIPLOMADO EN INCLUSION PSICOEDUCATIVA", "MAGISTER EN FAMILIA Y MEDIACION",
  "ORIENTACION RELACIONES HUMANAS Y FAMILIA", "FORMACION CON ORIENTACION EN AGRONEGOCIOS",
  "GESTION EMPRESARIAL", "MAGISTER EN EDUCACION", "ORIENTACION EN INVERSIONES"),
  NOMBRE_TITULO=NA_character_,NOMBRE_GRADO=NA_character_,AREA_CONOCIMIENTO="Otro")
res <- classify_subjects(subjects)
stopifnot(identical(which(res$ORIENTADOR_RELEVANT==1L),c(2L,7L,8L,9L,10L)),
  identical(which(res$LEADERSHIP_RELEVANT==1L),3L),res$TEACHER_RELEVANT[1]==1L,
  res$TEACHER_RELEVANT[13]==1L,res$TEACHER_RELEVANT[5]==0L)
menu <- data.table(NOMB_CARRERA="MAGISTER EN EDUCACION MENCION GESTION EDUCACIONAL Y ORIENTACION EDUCACIONAL",
  NOMBRE_TITULO=NA_character_,NOMBRE_GRADO="MAGISTER EN EDUCACION MENCION CURRICULUM",AREA_CONOCIMIENTO="Educacion")
mr <- classify_subjects(menu)
stopifnot(mr$ORIENTADOR_RELEVANT==0L,mr$LEADERSHIP_RELEVANT==0L,
  mr$ORIENTADOR_MENTION_CONFLICT==1L,mr$LEADERSHIP_MENTION_CONFLICT==1L)
menu[,NOMBRE_GRADO:="MAGISTER EN EDUCACION"]
mr <- classify_subjects(menu)
stopifnot(mr$ORIENTADOR_RELEVANT==1L,mr$ORIENTADOR_PROGRAM_ONLY==1L)
fx <- data.table(model_name="institution_plus_carrera_generica",effect_type="institution",
  level=c("1 || UNIVERSIDADES || A","2 || UNIVERSIDADES || B"),
  effect_log_clp_centered=c(.10001,.1))
a <- subjects[c(1,7,3,5,13)]
a[,`:=`(RECORD_ID=as.character(1:5),MRUN=c("p1","p1","p2","p3","p4"),
  INSTITUTION_ID=c("1","2","1","3","1"),
  NIVEL_CARRERA_1=c("Magister","Postitulo","Magister","Profesional","Doctorado"),
  LEVEL=c("postgraduate","postitulo","postgraduate","undergraduate","postgraduate"),
  ASOF_YEAR=c(2020L,2019L,2025L,2018L,2020L))]
a <- prepare_credential_awards(a,fx)
p <- data.table(MRUN=c("p1","p1","p1","p2","p3","p4","unmatched"),
  ROLE=c("Orientadores","Orientadores","HS teachers","Leadership","Orientadores","HS teachers","Leadership"),
  AGNO=c(2019L,2024L,2024L,2024L,2024L,2024L,2024L))
o <- build_credential_panel(p,a)
stopifnot(o[MRUN=="p1" & AGNO==2019,ANY_MAGISTER]==0L,
  o[MRUN=="p1" & ROLE=="Orientadores" & AGNO==2024,ROLE_SPECIFIC_MAGISTER]==0L,
  o[MRUN=="p1" & ROLE=="Orientadores" & AGNO==2024,ROLE_SPECIFIC_QUALIFICATION]==1L,
  o[MRUN=="p1" & ROLE=="HS teachers",ROLE_SPECIFIC_MAGISTER]==1L,
  o[MRUN=="p2",ANY_MAGISTER]==0L,
  o[MRUN=="p3",UG_HIGH_PREMIUM]==0L,o[MRUN=="p3",UG_UNCOVERED_INSTITUTION]==1L,
  o[MRUN=="p4",ANY_MAGISTER]==0L,o[MRUN=="p4",ANY_DOCTORATE]==1L,
  all(unlist(o[MRUN=="unmatched",..credential_names])==0L),
  a[RECORD_ID=="2",HIGH_PREMIUM]==0L)
ov <- subjects[7]
ov[,`:=`(TEACHER_OVERRIDE=NA_integer_,ORIENTADOR_OVERRIDE=0L,LEADERSHIP_OVERRIDE=NA_integer_,REASON="test exclusion")]
b <- apply_subject_overrides(a,ov)
stopifnot(b[RECORD_ID=="2",ORIENTADOR_RELEVANT]==0L)
bad <- copy(ov); bad[,NOMB_CARRERA:="NOT IN INPUT"]
stopifnot(inherits(try(apply_subject_overrides(a,bad),silent=TRUE),"try-error"))
message("PASS: subjects, strict cutoff, same-award master rule, doctorate distinction, timing, unmatched people, role overlap and exact overrides.")
