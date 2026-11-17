source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
checks <- list()
check <- function(name,condition,detail='') {
  checks[[length(checks)+1L]] <<- data.table(check=name,passed=isTRUE(condition),detail=as.character(detail))
  if(!isTRUE(condition)) { audit_write(rbindlist(checks),'verification'); stop('FAILED: ',name,' ',detail) }
  cat('PASS ',name,' ',detail,'\n',sep='')
}
panel <- readRDS(file.path(out,'admin_panel.rds'))
parents <- readRDS(file.path(out,'parent_predictors.rds'))
frame <- readRDS(file.path(out,'model_frame.rds'))
pred <- readRDS(file.path(out,'heldout_predictions.rds'))
metrics <- fread(file.path(out,'predictive_performance.csv'))
aud <- fread(file.path(out,'model_audit.csv'))
check('one row per universe student',nrow(panel)==944510L && !anyDuplicated(panel$mrun))
check('all outcome values binary',all(panel$applied_sae %in% 0:1))
check('round union identity',all(panel$applied_sae==pmax(panel$applied_regular,panel$applied_complementary)))
check('2018 Nuble retained',all(panel[cohort_gr8==2018 & origin_region==16]$analysis_eligible==1))
check('unknown offerings not recoded terminal',all(is.na(panel[is.na(max_offered_grade)]$ends8_sae)))
check('grade9 offering inconsistent with terminal flag never occurs',nrow(panel[ends8_sae==1 & offers_grade9==1])==0)
check('school splits disjoint',length(intersect(frame$data$origin_rbd[frame$train],frame$data$origin_rbd[frame$test]))==0)
check('prediction IDs align',identical(frame$data$mrun[frame$test],pred$mrun))
check('common sample and all models converged',all(aud$converged) && uniqueN(aud$train_n)==1 && uniqueN(aud$test_n)==1 && nrow(aud)==7)
check('grade5-7 sensitivity excludes grade8 annual predictors',!grepl('_g8',aud[model=='grades5_7_parent_answers']$formula))
check('no distance geography or postgrade8 predictors',!any(grepl('distance|comuna|grade9_outcome|most_time_rbd',aud$formula,ignore.case=TRUE)))

# Independent grouped-rank AUC avoids integer-overflow and handles ties.
auc_independent <- function(y,p) {
  z <- data.table(y,p)[,.(positive=sum(y),negative=sum(1-y)),by=p][order(p)]
  below <- cumsum(as.double(z$negative))-z$negative
  sum(z$positive*(below+.5*z$negative))/(as.double(sum(y))*sum(1-y))
}
check('large-sample AUC arithmetic',abs(auc_independent(rep(c(0L,1L),each=70000),rep(c(.1,.9),each=70000))-1)<1e-12)
check('tied-score AUC arithmetic',abs(auc_independent(c(0,1,0,1),rep(.5,4))-.5)<1e-12)
for(m in names(frame$specs)) {
  p <- pred[[m]]; y <- pred$applied_sae
  row <- metrics[model==m & group_type=='overall']
  check(paste('probability bounds',m),all(is.finite(p)&p>=0&p<=1))
  check(paste('independent AUC',m),abs(auc_independent(y,p)-row$auc)<1e-10)
  check(paste('independent Brier',m),abs(sum((y-p)^2)/length(y)-row$brier)<1e-12)
}

# Verify train-only scaling and imputation against the original numeric fields.
prep <- fread(file.path(out,'training_preprocessing.csv'))
for(v in c('gpa_g5','attendance_g8','gap_math_g7','parent_reenroll','parent_discipline')) {
  x <- frame$data[[v]][frame$train]; x <- x[is.finite(x)]
  row <- prep[feature==v]
  check(paste('training moments',v),abs(mean(x)-row$mean)<1e-10 && abs(sd(x)-row$sd)<1e-10 && abs(median(x)-row$median)<1e-10)
}

# Direct reconstruction of 2019 application labels from raw stage files.
ids <- numeric()
for(prefix in c('^C1_','^C2_')) {
  path <- one_file(file.path(data_root,'data/raw/2019/SAE_2019'),paste0(prefix,'.*csv$'))
  a <- read_cols(path,c('mrun','cod_nivel','agregada_por_continuidad'),';')
  ids <- union(ids,a[cod_nivel==9 & agregada_por_continuidad==0]$mrun)
}
check('2019 raw application reconstruction',identical(panel[cohort_gr8==2019]$applied_sae,as.integer(panel[cohort_gr8==2019]$mrun %in% ids)))

# Direct source check of scale reversal and identity in two questionnaire years.
s <- readRDS(file.path(out,'baseline_simce.rds'))
for(year in 2013:2014) {
  raw_name <- if(year==2013) 'cpad_p29_06' else 'cpad_p25_06'
  a <- read_cols(one_file(file.path(simce_folder(year),'Archivos TXT (Planos)'),'cpad.*csv$'),c('idalumno','rbd',raw_name),'|')
  link <- merge(s[simce_year==year,.(mrun,idalumno,simce_rbd)],a,by='idalumno')
  expected <- as.numeric(link[[raw_name]])
  expected[!expected %in% 1:4 | link$simce_rbd!=link$rbd] <- NA_real_
  if(year==2013) expected <- 5-expected
  got <- parents$parent_reenroll[match(link$mrun,parents$mrun)]
  check(paste('parent re-enrollment raw coding',year),isTRUE(all.equal(expected,got,check.attributes=FALSE)))
}

# Inspect 20 full raw school-grade rosters in 2019, not just the selected cohort.
raw <- read_cols(one_file(file.path(data_root,'data/raw/student_tracking/2019/Rendimiento-2019'),'csv$'),
                 c('MRUN','RBD','COD_ENSE','COD_GRADO','PROM_GRAL','ASISTENCIA','SIT_FIN'),';',',')
saved <- readRDS(file.path(out,'tracking_features_2019.rds'))
cells <- unique(saved[,.(rbd,cod_grado)])[order(rbd,cod_grado)]
cells <- cells[unique(round(seq(1,nrow(cells),length.out=20)))]
raw <- merge(raw,cells,by=c('rbd','cod_grado'))
raw <- unique(raw[cod_ense==110 & !is.na(sit_fin) & mrun>0 & rbd>0])
raw[, `:=`(pos=as.integer(asistencia>0 & !is.na(asistencia)),pass=as.integer(sit_fin=='P'))]
setorderv(raw,c('mrun','rbd','cod_grado','pos','pass','asistencia'),c(1,1,1,-1,-1,-1),na.last=TRUE)
raw <- unique(raw,by=c('mrun','rbd','cod_grado'))
raw[,gpa:=fifelse(prom_gral %between% c(1,7),prom_gral,NA_real_)]
raw[,att:=fifelse(asistencia %between% c(0,100),asistencia,NA_real_)]
raw[s,on='mrun',`:=`(base=i.simce_math_national_z,base_year=i.simce_year)]
raw[base_year>=2019,base:=NA_real_]
direct_z <- function(x) {
  nn <- sum(!is.na(x)); ss <- sum(x,na.rm=TRUE)
  if(nn<2) return(rep(NA_real_,length(x)))
  variance <- sum((x-ss/nn)^2,na.rm=TRUE)/(nn-1)
  if(variance<=0) return(rep(NA_real_,length(x)))
  (x-ss/nn)/sqrt(variance)
}
raw[,`:=`(expected_gpa=direct_z(gpa),expected_att=direct_z(att),expected_base=direct_z(base)),by=.(rbd,cod_grado)]
joined <- merge(saved,raw[,.(mrun,rbd,cod_grado,expected_gpa,expected_att,expected_base)],by=c('mrun','rbd','cod_grado'))
check('independent peer GPA standardization',isTRUE(all.equal(joined$z_gpa,joined$expected_gpa,tolerance=1e-10)))
check('independent peer attendance standardization',isTRUE(all.equal(joined$z_attendance,joined$expected_att,tolerance=1e-10)))
check('independent peer SIMCE gap construction',isTRUE(all.equal(joined$gap_math,joined$expected_gpa-joined$expected_base,tolerance=1e-10)))

# Confirm one saved fit predicts the saved probabilities and reproduces a standard
# glm fit on a small, nonseparated data fixture with a quadratic and missing flag.
set.seed(8001)
toy <- data.frame(x=rnorm(1000),missing=rbinom(1000,1,.1))
toy$y <- rbinom(1000,1,plogis(.2+.4*toy$x-.15*toy$x^2+.1*toy$missing))
ref <- glm(y~x+I(x^2)+missing,data=toy,family=binomial())
fit <- feglm(y~x+I(x^2)+missing,data=toy,family='binomial',notes=FALSE)
check('logit engine agrees with base R',max(abs(coef(ref)-coef(fit)))<1e-7)
model <- restore_model('plus_parent_answers',frame)
check('saved full model prediction reproduction',max(abs(predict(model,newdata=frame$data[frame$test[1:1000]],type='response')-pred$plus_parent_answers[1:1000]))<1e-12)
check('saved full model complete holdout reproduction',max(abs(predict(model,newdata=frame$data[frame$test],type='response')-pred$plus_parent_answers))<1e-12)
audit_write(rbindlist(checks),'verification')
cat('All ',length(checks),' checks passed.\n',sep='')
