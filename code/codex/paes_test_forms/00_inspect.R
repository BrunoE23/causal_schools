library(data.table)
d <- readRDS('data/clean/paes_test_forms/selected_2026.rds')
print(d[, lapply(.SD, function(x) paste(head(unique(x),10),collapse=' | ')),
  .SDcols=c('PROMEDIO_NOTAS','FECHA_NACIMIENTO','PTJE_NEM','PTJE_RANKING','ANYO_DE_EGRESO','COD_SEXO')])
print(d[FORMA_REG_CL>0 & FORMA_REG_M1>0, .N, by=.(FORMA_REG_CL,FORMA_REG_M1)])
print(d[FORMA_REG_M1>0, .(N=.N, score_valid=sum(between(MATE1_REG_ACTUAL,100,1000)),
  score_zero=sum(MATE1_REG_ACTUAL==0), rbd_missing=sum(is.na(RBD)|RBD<=0))])
print(summary(d$PROMEDIO_NOTAS))
