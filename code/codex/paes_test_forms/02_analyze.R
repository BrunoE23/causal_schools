# All output is local to this task; raw files and earlier project analyses are read-only.
suppressPackageStartupMessages({library(data.table); library(fixest)})
setDTthreads(4L); setFixest_nthreads(4L)
out <- 'data/clean/paes_test_forms'
dir.create(file.path(out, 'models'), showWarnings = FALSE)
subjects <- c(CL='Reading', M1='Math M1', M2='Math M2', HCS='History',
  CBIO='Science: biology', CFIS='Science: physics', CQUI='Science: chemistry', CTP='Science: technical')
score_prefix <- c(CL='CLEC', M1='MATE1', M2='MATE2', HCS='HCSOC',
  CBIO='CIEN', CFIS='CIEN', CQUI='CIEN', CTP='CIEN')
joints <- contrasts <- descriptives <- audits <- list()
tag <- function(x, year, sitting, subject, specification, outcome) {
  x[, `:=`(year=year, sitting=sitting, subject=subject, label=subjects[[subject]],
    specification=specification, outcome=outcome)]
  x
}
extract <- function(fit, d, year, sitting, subject, specification, outcome) {
  b <- coef(fit); V <- fit$cov.scaled; idx <- grep('^form::', names(b)); q <- length(idx)
  stopifnot(q == uniqueN(d$form)-1L, nobs(fit) == nrow(d))
  # Never regularize the covariance. The form submatrix must be positive definite.
  form_eigen <- eigen(V[idx,idx,drop=FALSE],symmetric=TRUE,only.values=TRUE)$values
  stopifnot(all(is.finite(V)),min(form_eigen)>0)
  G <- uniqueN(d$rbd); df <- G-1L
  w <- as.numeric(crossprod(b[idx], solve(V[idx,idx,drop=FALSE], b[idx])))/q
  joint <- data.table(n=nobs(fit), schools=G, forms=uniqueN(d$form), F_stat=w,
    df_num=q, df_denom=df, p_value=pf(w,q,df,lower.tail=FALSE), outcome_sd=sd(d[[outcome]]),
    form_vcov_min_eigenvalue=min(form_eigen))
  joints[[length(joints)+1L]] <<- tag(joint,year,sitting,subject,specification,outcome)
  forms <- sort(unique(d$form)); pairs <- combn(forms,2L)
  pp <- rbindlist(lapply(seq_len(ncol(pairs)), function(k) {
    lo <- pairs[1L,k]; hi <- pairs[2L,k]
    L <- setNames(numeric(length(b)),names(b))
    if(paste0('form::',hi) %in% names(b)) L[paste0('form::',hi)] <- 1
    if(paste0('form::',lo) %in% names(b)) L[paste0('form::',lo)] <- -1
    est <- sum(L*b); se <- sqrt(as.numeric(crossprod(L,V %*% L)))
    data.table(form_a=lo,form_b=hi,estimate_b_minus_a=est,std_error=se,df=df,
      p_value=2*pt(-abs(est/se),df),lower95=est-qt(.975,df)*se,
      upper95=est+qt(.975,df)*se,estimate_sd=est/sd(d[[outcome]]))
  }))
  contrasts[[length(contrasts)+1L]] <<- tag(pp,year,sitting,subject,specification,outcome)
}
fit_one <- function(d, year, sitting, subject, specification, outcome='score', adjusted=TRUE, school_fe=TRUE) {
  # Continuous controls are centered/scaled for numerical stability, not outcomes.
  controls <- if(adjusted) c('gpa_c','I(gpa_c^2)','I(gpa_c^3)',
    'rank_c','I(rank_c^2)','I(rank_c^3)',
    'age_c','I(age_c^2)','I(age_c^3)','female') else character()
  if(adjusted && uniqueN(d$grad)>1L) controls <- c(controls,'i(grad)')
  if(adjusted && uniqueN(d$track)>1L) controls <- c(controls,'i(track)')
  rhs <- paste(c('i(form)',controls),collapse=' + ')
  f <- as.formula(paste(outcome,'~',rhs,if(school_fe) '| rbd' else ''))
  fit <- feols(f, data=d,
    vcov=vcov_cluster(~rbd,vcov_fix=FALSE,
      ssc=ssc(K.adj=TRUE,K.fixef='nonnested',G.adj=TRUE,t.df='min')),
    fixef.rm='none', notes=FALSE, lean=TRUE, warn=TRUE)
  extract(fit,d,year,sitting,subject,specification,outcome)
  if(outcome=='score') saveRDS(fit, file.path(out,'models',paste0(paste(year,sitting,subject,specification,sep='_'),'.rds')))
  invisible(fit)
}
for(year in 2023:2026) {
  message('Analyze ',year)
  # Each cached file is approximately 95-111 MB; read one year, then only one subject at a time.
  raw <- readRDS(file.path(out,paste0('selected_',year,'.rds')))
  stopifnot(!anyDuplicated(raw$MRUN))
  birth <- as.integer(raw$FECHA_NACIMIENTO)
  common <- raw[,.(mrun=MRUN,rbd=as.integer(RBD),grad=as.integer(ANYO_DE_EGRESO),
    gpa=as.numeric(gsub(',','.',as.character(PROMEDIO_NOTAS),fixed=TRUE)),
    ranking=as.numeric(PTJE_RANKING),female=fifelse(COD_SEXO %in% c(1,2),as.numeric(COD_SEXO==2),NA_real_),
    track=fifelse(is.na(RAMA_EDUCACIONAL)|RAMA_EDUCACIONAL=='','Unknown',as.character(RAMA_EDUCACIONAL)))]
  common[, age := (year-1L)-(birth %/% 100L)+(12L-(birth %% 100L))/12]
  common[!(birth %% 100L %in% 1:12), age := NA_real_]
  common[, recent := as.numeric(grad==year-1L)]
  common[, eligible := !is.na(rbd)&rbd>0&between(gpa,4,7)&between(ranking,100,1000)&
    between(age,14,90)&!is.na(female)&between(grad,1930,year-1L)]
  common[, `:=`(gpa_c=gpa-6,rank_c=(ranking-600)/100,age_c=(age-20)/10)]
  for(sitting in c('REG','INV')) {
    # Winter admission 2023 was PDT rather than PAES. Do not mix the instruments.
    if(sitting=='INV' && year==2023L) next
    science_cols <- paste0('FORMA_',sitting,'_',c('CBIO','CFIS','CQUI','CTP'))
    stopifnot(all(rowSums(raw[,..science_cols]>0,na.rm=TRUE)<=1L))
    for(subject in names(subjects)) {
      form_col <- paste0('FORMA_',sitting,'_',subject)
      score_col <- paste0(score_prefix[[subject]],'_',sitting,'_ACTUAL')
      correct_col <- paste0('CORRECTAS_',sitting,'_',subject)
      stopifnot(all(c(form_col,score_col,correct_col) %in% names(raw)))
      d <- copy(common)
      d[, `:=`(form=raw[[form_col]],score=raw[[score_col]],correct=raw[[correct_col]])]
      # Science score is shared across modules; module participation is determined by its form code.
      valid_score <- between(d$score,100,1000)
      usable <- d[form>0 & valid_score]
      descriptives[[length(descriptives)+1L]] <- tag(usable[,.(n=.N,mean_score=mean(score),sd_score=sd(score),
        mean_correct=mean(correct),mean_gpa=mean(gpa[gpa>=4],na.rm=TRUE),female_share=mean(female,na.rm=TRUE),
        mean_age=mean(age,na.rm=TRUE),recent_share=mean(recent,na.rm=TRUE)),by=form],year,sitting,subject,'all_takers','score')
      eligible <- usable[eligible==TRUE]
      supports <- eligible[,.(n=.N,nforms=uniqueN(form)),by=rbd]
      eligible <- eligible[rbd %in% supports[nforms>1L,rbd]]
      stopifnot(nrow(eligible)>100,uniqueN(eligible$form)>=2)
      audits[[length(audits)+1L]] <- data.table(year,sitting,subject,label=subjects[[subject]],
        raw_rows=nrow(raw),positive_form=sum(d$form>0,na.rm=TRUE),valid_form_score=nrow(usable),
        positive_form_invalid_score=sum(d$form>0 & !valid_score,na.rm=TRUE),
        complete_controls=sum(usable$eligible,na.rm=TRUE),within_school_supported=nrow(eligible),
        missing_or_invalid_gpa=sum(!between(usable$gpa,4,7)),
        missing_or_invalid_ranking=sum(!between(usable$ranking,100,1000)),
        missing_or_invalid_rbd=sum(is.na(usable$rbd)|usable$rbd<=0),
        score_column=score_col,form_column=form_col)
      # Unadjusted and adjusted specifications use exactly the same observations.
      fit_one(eligible,year,sitting,subject,'raw_same_sample',adjusted=FALSE,school_fe=FALSE)
      fit_one(eligible,year,sitting,subject,'school_only',adjusted=FALSE)
      fit_one(eligible,year,sitting,subject,'adjusted')
      fit_one(eligible,year,sitting,subject,'adjusted','correct')
      # Balance tests condition on attended school only; never control for the characteristic being tested.
      balance_vars <- c('gpa','female','age')
      if(uniqueN(eligible$recent)>1L) balance_vars <- c(balance_vars,'recent')
      for(v in balance_vars) fit_one(eligible,year,sitting,subject,'balance_school',v,adjusted=FALSE)
      if(sitting=='REG') {
        recent <- eligible[recent==1]
        supports <- recent[,.(nforms=uniqueN(form)),by=rbd]
        recent <- recent[rbd %in% supports[nforms>1L,rbd]]
        fit_one(recent,year,sitting,subject,'adjusted_current_graduates')
      }
      message(year,' ',sitting,' ',subject,': ',nrow(eligible),' observations')
    }
  }
  rm(raw,common,d,usable,eligible); invisible(gc())
}
joint <- rbindlist(joints)
pairwise <- rbindlist(contrasts)
# Separate predeclared test families: 32 regular year-subject tests and 24 winter tests,
# by specification/outcome. For balance, adjust over all characteristics together.
joint[, family_outcome := fifelse(specification=='balance_school','all_balance',outcome)]
pairwise[, family_outcome := fifelse(specification=='balance_school','all_balance',outcome)]
joint[, p_holm := p.adjust(p_value,'holm'),by=.(sitting,specification,family_outcome)]
pairwise[, `:=`(p_holm=p.adjust(p_value,'holm'),family_comparisons=.N),by=.(sitting,specification,family_outcome)]
pairwise[, `:=`(lower_simultaneous95=estimate_b_minus_a-qt(1-.05/(2*family_comparisons),df)*std_error,
  upper_simultaneous95=estimate_b_minus_a+qt(1-.05/(2*family_comparisons),df)*std_error)]
largest <- pairwise[outcome=='score',.SD[which.max(abs(estimate_b_minus_a))],by=.(year,sitting,subject,specification)]
largest[, `:=`(gap=abs(estimate_b_minus_a),advantaged_form=fifelse(estimate_b_minus_a>=0,form_b,form_a),
  other_form=fifelse(estimate_b_minus_a>=0,form_a,form_b))]
fwrite(joint,file.path(out,'joint_tests.csv'))
fwrite(pairwise,file.path(out,'pairwise_contrasts.csv'))
fwrite(largest,file.path(out,'largest_score_gaps.csv'))
fwrite(rbindlist(descriptives),file.path(out,'form_descriptives.csv'))
fwrite(rbindlist(audits),file.path(out,'sample_audit.csv'))
capture.output(sessionInfo(),file=file.path(out,'session_info.txt'))
message('Adjusted score tests:')
print(joint[specification=='adjusted' & outcome=='score',.(year,sitting,subject,n,forms,p_value,p_holm)])
message('Largest regular adjusted score contrasts:')
print(largest[sitting=='REG' & specification=='adjusted'][order(-gap),.(year,subject,n=NULL,gap,
  advantaged_form,other_form,std_error,p_holm,lower_simultaneous95,upper_simultaneous95)])
