suppressPackageStartupMessages(library(data.table))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
source(file.path(dirname(script), "linkage_helpers.R"))
checks <- 0L
check <- function(x, name) { stopifnot(isTRUE(x)); checks <<- checks+1L; message("PASS: ", name) }
check(identical(t_id(c("00123", "0", "", NA, "1.2", "1e3")), c("123", rep(NA_character_,5))), "ID normalization preserves exact keys, invalid never match")
check(identical(t_birth(c("199905","190001","19000101","199913","19990512")), c("199905",NA,NA,NA,"199905")), "Birth-month and sentinel rules")
check(identical(as.character(t_date(c("20200229","20210229","19000101","20200101"))), c("2020-02-29",NA,NA,"2020-01-01")), "Actual calendar dates, leap years and sentinels")
raw <- data.table(cat_periodo="2020",mrun=c("1","1","2","2"), gen_alu="2", fec_nac_alu="198001",
  fecha_obtencion_titulo=c("20200101","20200101","20210101","19000101"), nivel_global=c("Pregrado","Pregrado","Posgrado","Postitulo"),
  area_conocimiento="Educacion",cod_inst="001",nomb_inst="University A",nomb_carrera="Program A")
x <- t_clean_awards(raw,2020,"fixture.csv")
check(x$audit$N_EXACT_DUPLICATES_REMOVED == 1L, "Exact duplicates counted once")
check(nrow(x$data[MRUN=="2"]) == 2L, "Different awards for one person retained")
check(identical(x$data$ASOF_YEAR,c(2020L,2021L,2020L)), "No earlier-than-award match; missing date basis explicit")
check(x$data[MRUN=="1",IS_EDUCATION_UNDERGRAD], "Education undergraduate distinct from postgraduate")
f <- t_flags_by_year(x$data)
check(f[MRUN=="2",FIRST_POSTGRAD_YEAR] == 2021L, "Future postgraduate award cannot enter earlier staff year")
check(is.na(f[MRUN=="2",FIRST_UNDERGRAD_YEAR]), "Postgraduate match cannot masquerade as original undergraduate degree")
early <- t_flags_by_year(x$data[ASOF_YEAR<=2020])
check(early[MRUN=="2",FIRST_ANY_YEAR] == f[MRUN=="2",FIRST_ANY_YEAR], "Later award preserves earlier any-award history")
message("All ", checks, " linkage checks passed.")
