# Build actual April-30 secondary enrollment from official classroom aggregates.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
out <- "data/clean/secondary_sae_spots/enrollment_trends"
dir.create(out, recursive=TRUE, showWarnings=FALSE)
regular_secondary <- c(310L,410L,510L,610L,710L,810L,910L)
panels <- audits <- manifests <- list()
for (y in 2010:2025) {
  p <- list.files(file.path(out,"source_courses",y),pattern="[.]csv$",recursive=TRUE,full.names=TRUE)
  stopifnot(length(p)==1L, file.info(p)$size < 1e8)
  h <- names(fread(p,nrows=0L)); hu <- toupper(h)
  wanted <- c("AGNO","RBD","COD_REG_RBD","COD_DEPE","ESTADO_ESTAB","COD_ENSE","COD_GRADO","LET_CUR","COD_JOR","COD_TIP_CUR","COD_RAMA","COD_SEC","COD_ESPE","COD_DES_CUR","TIPO_AULA","N_ALU","N_ALU_HOM","N_ALU_MUJ","N_ALU_SI","N_ALU_NB",paste0("N_ALU_GRADO",1:9))
  selected <- h[hu %in% wanted]
  x <- fread(p,select=selected,na.strings=c("","NA"),showProgress=FALSE)
  setnames(x,toupper(names(x)))
  stopifnot(all(x$AGNO==y), !anyNA(x$RBD), all(x$RBD>0), !anyNA(x$N_ALU), all(x$N_ALU>=0))
  if (!"N_ALU_SI" %in% names(x)) x[,N_ALU_SI:=0L]
  if (!"N_ALU_NB" %in% names(x)) x[,N_ALU_NB:=0L]
  # Some preschool/primary rows have missing sex detail. Validate only the
  # target population; N_ALU remains the unmodified official total.
  source_missing_sex_detail <- sum(is.na(x$N_ALU_HOM+x$N_ALU_MUJ+x$N_ALU_SI+x$N_ALU_NB))
  source_rows <- nrow(x)
  if (!"ESTADO_ESTAB" %in% names(x)) x[,ESTADO_ESTAB:=1L]
  inactive_secondary_students <- x[COD_ENSE %in% regular_secondary & ESTADO_ESTAB!=1L,sum(N_ALU)]
  x <- x[ESTADO_ESTAB==1L & COD_ENSE %in% regular_secondary]
  stopifnot(all(x$N_ALU == x$N_ALU_HOM+x$N_ALU_MUJ+x$N_ALU_SI+x$N_ALU_NB))
  key <- intersect(c("RBD","COD_ENSE","COD_GRADO","LET_CUR","COD_JOR","COD_TIP_CUR","COD_RAMA","COD_SEC","COD_ESPE","COD_DES_CUR","TIPO_AULA"),names(x))
  stopifnot(!anyDuplicated(x[,..key]))
  # Combined classroom records have grade-specific counts instead of COD_GRADO.
  stopifnot(all(x[COD_TIP_CUR==0L,COD_GRADO] %in% 1:4))
  for(g in 1:4) {
    v<-paste0("N_ALU_GRADO",g)
    x[, (paste0("grade",g+8L)) := fifelse(COD_TIP_CUR==0L, fifelse(COD_GRADO==g,N_ALU,0L), get(v))]
  }
  stopifnot(!anyNA(x[,paste0("grade",9:12),with=FALSE]),all(rowSums(x[,paste0("grade",9:12),with=FALSE])==x$N_ALU))
  a<-x[,.(year=y,secondary_students=sum(N_ALU),grade9_students=sum(grade9),grade10_students=sum(grade10),grade11_students=sum(grade11),grade12_students=sum(grade12),classroom_rows=.N,source_region=first(COD_REG_RBD),dependency=first(COD_DEPE)),by=.(rbd=RBD)]
  stopifnot(!anyDuplicated(a$rbd),sum(a$secondary_students)==sum(x$N_ALU),all(a$secondary_students==a$grade9_students+a$grade10_students+a$grade11_students+a$grade12_students))
  panels[[as.character(y)]]<-a
  audits[[as.character(y)]]<-data.table(year=y,source_classrooms=source_rows,source_missing_sex_detail=source_missing_sex_detail,secondary_classrooms=nrow(x),secondary_schools=sum(a$secondary_students>0),secondary_students=sum(a$secondary_students),grade9_students=sum(a$grade9_students),combined_secondary_classrooms=sum(x$COD_TIP_CUR>0),excluded_inactive_secondary_students=inactive_secondary_students)
  manifests[[as.character(y)]]<-data.table(year=y,path=normalizePath(p,winslash="/"),bytes=file.info(p)$size,md5=unname(tools::md5sum(p)))
  message(y,": ",sum(a$secondary_students)," secondary students; ",nrow(a)," RBDs")
}
panel<-rbindlist(panels)
stopifnot(!anyDuplicated(panel[,.(rbd,year)]))
fwrite(panel,file.path(out,"school_year_enrollment.csv"))
fwrite(rbindlist(audits),file.path(out,"enrollment_source_audit.csv"))
fwrite(rbindlist(manifests),file.path(out,"enrollment_input_manifest.csv"))
