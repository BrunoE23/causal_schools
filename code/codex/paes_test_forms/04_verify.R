suppressPackageStartupMessages({library(data.table);library(fixest)})
setDTthreads(4L)
out <- 'data/clean/paes_test_forms'
checks <- list()
check <- function(label,ok,value='') {
  checks[[length(checks)+1L]] <<- data.table(check=label,passed=isTRUE(ok),detail=as.character(value))
  if(!isTRUE(ok)) stop(label, ': ', value)
}
j <- fread(file.path(out,'joint_tests.csv')); p <- fread(file.path(out,'pairwise_contrasts.csv'))
a <- fread(file.path(out,'sample_audit.csv')); counts <- fread(file.path(out,'form_counts.csv'))
check('32 regular adjusted score joint tests', nrow(j[sitting=='REG'&specification=='adjusted'&outcome=='score'])==32)
check('24 PAES winter adjusted score joint tests', nrow(j[sitting=='INV'&specification=='adjusted'&outcome=='score'])==24)
check('No PDT winter 2023 in PAES results', !any(j$year==2023 & j$sitting=='INV'))
check('Positive-form rows reconcile with excluded invalid scores',
  all(a$positive_form==a$valid_form_score+a$positive_form_invalid_score),
  paste(sum(a$positive_form_invalid_score),'invalid score records excluded'))
check('All form covariance blocks positive definite',all(j$form_vcov_min_eigenvalue>0))
check('All probabilities valid',all(between(j$p_value,0,1)) && all(between(j$p_holm,0,1)))
check('Holm adjustment never decreases a p value',all(j$p_holm>=j$p_value-1e-14))
check('Simultaneous intervals contain pointwise intervals',all(p$lower_simultaneous95<=p$lower95) && all(p$upper_simultaneous95>=p$upper95))
models <- list.files(file.path(out,'models'),full.names=TRUE)
covdiag <- rbindlist(lapply(models,function(path) {
  fit <- readRDS(path)
  ev <- eigen(fit$cov.scaled,symmetric=TRUE,only.values=TRUE)$values
  data.table(model=basename(path),min_eigen=min(ev),max_eigen=max(ev),relative_min=min(ev)/max(ev))
}))
check('Full covariance negative eigenvalues are rounding scale only',all(covdiag$relative_min > -1e-12))
fwrite(covdiag,file.path(out,'covariance_diagnostics.csv'))
for(s in c('REG','INV')) {
  for(y in unique(a$year)) {
    sub <- a[year==y & sitting==s]
    for(v in sub$subject) {
      expected <- counts[year==y & variable==paste0('FORMA_',s,'_',v) & form>0,sum(n)]
      check(paste('Counts agree',y,s,v),sub[subject==v,positive_form]==expected)
    }
  }
}
# Independent FWL / QR regression and cluster sandwich for the largest headline case.
# Rebuild its rows from the cache, then calculate the estimator without feols.
d <- readRDS(file.path(out,'selected_2026.rds'))
b <- as.integer(d$FECHA_NACIMIENTO)
z <- data.table(rbd=as.integer(d$RBD),form=d$FORMA_REG_CL,score=d$CLEC_REG_ACTUAL,
  grad=as.integer(d$ANYO_DE_EGRESO),gpa=as.numeric(gsub(',','.',as.character(d$PROMEDIO_NOTAS),fixed=TRUE)),
  ranking=as.numeric(d$PTJE_RANKING),female=ifelse(d$COD_SEXO %in% c(1,2),as.numeric(d$COD_SEXO==2),NA_real_),
  track=ifelse(is.na(d$RAMA_EDUCACIONAL)|d$RAMA_EDUCACIONAL=='','Unknown',as.character(d$RAMA_EDUCACIONAL)),
  age=2025-(b%/%100)+(12-b%%100)/12)
z[!(b%%100 %in% 1:12),age:=NA_real_]
z <- z[form>0 & between(score,100,1000) & !is.na(rbd)&rbd>0 &
  between(gpa,4,7)&between(ranking,100,1000)&between(age,14,90)&!is.na(female)&between(grad,1930,2025)]
school <- z[,.(nf=uniqueN(form)),by=rbd]
z <- z[rbd %in% school[nf>1,rbd]]
z[,`:=`(gpa_c=gpa-6,rank_c=(ranking-600)/100,age_c=(age-20)/10)]
f <- readRDS(file.path(out,'models/2026_REG_CL_adjusted.rds'))
check('Independent headline sample matches',nrow(z)==nobs(f),nrow(z))
base_formula <- as.formula(gsub('i(', 'factor(', paste(deparse(f$fml_all$linear),collapse=' '),fixed=TRUE))
X <- model.matrix(base_formula,data=as.data.frame(z))
colnames(X) <- sub('^factor\\(([^)]+)\\)', '\\1::', colnames(X))
X <- X[,names(coef(f)),drop=FALSE]
ids <- match(z$rbd,sort(unique(z$rbd)))
ng <- tabulate(ids)
Xw <- X-(rowsum(X,ids,reorder=TRUE)/ng)[ids,,drop=FALSE]
yw <- z$score-as.numeric(rowsum(z$score,ids,reorder=TRUE)/ng)[ids]
qrX <- qr(Xw,tol=1e-10)
check('Within-school design full rank',qrX$rank==ncol(Xw))
bh <- qr.coef(qrX,yw)
err <- max(abs(bh-coef(f)))
check('Independent QR coefficients reproduce fit',err<1e-5,err)
res <- as.numeric(yw-Xw%*%bh)
bread <- solve(crossprod(Xw))
scores <- rowsum(Xw*res,ids,reorder=TRUE)
V0 <- bread%*%crossprod(scores)%*%bread
V <- f$cov.scaled
K <- attr(V,'df.K')
if(is.null(K)) K <- length(coef(f))+1L
correction <- (nrow(z)-1)/(nrow(z)-K)*length(ng)/(length(ng)-1)
Vmanual <- V0*correction
fi <- grep('^form::',names(coef(f)))
se_err <- max(abs(sqrt(diag(Vmanual)[fi])-sqrt(diag(V)[fi])))
check('Independent clustered form SEs reproduce fit',se_err<1e-6,paste('max difference',se_err,'K',K))
check('Saved contrast agrees with independent result',
  abs(p[year==2026&sitting=='REG'&subject=='CL'&specification=='adjusted'&outcome=='score'&form_a==103&form_b==106,estimate_b_minus_a]-bh['form::106'])<1e-6)
fwrite(rbindlist(checks),file.path(out,'verification.csv'))
print(rbindlist(checks)[.N-4:0])
