# One grade-specific predictor at a time, plus transparent joint comparisons.
source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
setFixest_nthreads(4L)
new_out <- file.path(out,'interpretation_pca')
write_audit <- function(z,name) fwrite(z,file.path(new_out,paste0(name,'.csv')))
d <- readRDS(file.path(new_out,'analysis_frame.rds'))
prep <- fread(file.path(out,'voluntary/training_preprocessing.csv'))
stems <- c('gpa','attendance','z_gpa','z_attendance','gap_math','gap_language')
academic <- unlist(lapply(stems,function(s) paste0(s,'_g',5:8)))
terms_for <- function(vars) unlist(lapply(vars,function(v) paste0(v,c('__z','__sq','__missing'))))
context <- c('factor(cohort_gr8)*factor(origin_region)','factor(origin_sector)','factor(gender)',
  'age__z','age__sq','age__missing','same_simce_origin__z','same_simce_origin__missing','parent_record__z',
  'factor(survey_year)','survey_fraction_answered',paste0('survey_complete_',2013:2016))
pc1 <- paste0('survey_pc1_',2013:2016)
pc23 <- c(paste0('survey_pc2_',2013:2016),paste0('survey_pc3_',2013:2016))
common <- c(context,pc1)
coefficients <- summaries <- audit <- list(); fitted_models <- list(); checks <- list()
check <- function(name,ok) {
  checks[[length(checks)+1L]] <<- data.table(check=name,passed=isTRUE(ok))
  write_audit(rbindlist(checks),'regression_verification')
  if(!isTRUE(ok)) stop('Failed: ',name)
}
check('same full voluntary population',nrow(d)==313813L && sum(d$applied_sae)==45947L && !anyDuplicated(d$mrun))
dir.create(file.path(new_out,'models'),showWarnings=FALSE)
estimate <- function(name,terms,rows=seq_len(nrow(d))) {
  fml <- as.formula(paste('applied_sae ~',paste(unique(terms),collapse=' + ')))
  log_message('OLS: ',name,' (N=',length(rows),')')
  fit <- feols(fml,data=d[rows],vcov=~origin_rbd,notes=FALSE,lean=TRUE)
  check(paste(name,'no silent sample loss'),nobs(fit)==length(rows))
  ct <- as.data.table(coeftable(fit),keep.rownames='term'); setnames(ct,c('term','estimate','se_cluster_school','t','p_value'))
  ci <- confint(fit)
  ct[,`:=`(model=name,estimate_pp=100*estimate,se_pp=100*se_cluster_school,
    ci_low_pp=100*ci[match(term,rownames(ci)),1],ci_high_pp=100*ci[match(term,rownames(ci)),2])]
  coefficients[[name]] <<- ct
  summaries[[name]] <<- data.table(model=name,students=nobs(fit),schools=uniqueN(d$origin_rbd[rows]),
    applicants=sum(d$applied_sae[rows]),application_rate=mean(d$applied_sae[rows]),r_squared=as.numeric(fitstat(fit,'r2')[[1]]),
    coefficients=nrow(ct))
  audit[[name]] <<- data.table(model=name,formula=paste(deparse(fml),collapse=' '),
    omitted_collinear=paste(fit$collin.var,collapse='|'))
  # Coefficients, clustered covariance and the actual lean regression object;
  # all input data are in analysis_frame.rds, avoiding repeated large copies.
  saveRDS(fit,file.path(new_out,'models',paste0(name,'.rds')))
  fitted_models[[name]] <<- fit
  invisible(fit)
}
estimate('context_only',context)
estimate('survey_only',common)
for(v in academic) estimate(paste0('one_',v),c(common,terms_for(v)))
estimate('joint_academic_pc1',c(common,terms_for(academic)))
estimate('joint_academic_pc123',c(common,pc23,terms_for(academic)))

# Focused comparison: the same observed GPA + math-gap sample in all three
# models for each grade, with the same PCA/context controls and same units.
correlations <- list(); pair_transforms <- list()
for(g in 5:8) {
  gv <- paste0('z_gpa_g',g); dv <- paste0('gap_math_g',g)
  rows <- which(is.finite(d[[gv]]) & is.finite(d[[dv]]))
  estimate(paste0('pair_g',g,'_gpa_only'),c(common,terms_for(gv)),rows)
  estimate(paste0('pair_g',g,'_gap_only'),c(common,terms_for(dv)),rows)
  fit <- estimate(paste0('pair_g',g,'_both'),c(common,terms_for(c(gv,dv))),rows)
  G <- d[[gv]][rows]; D <- d[[dv]][rows]; M <- G-D
  correlations[[as.character(g)]] <- data.table(grade=g,students=length(rows),corr_gpa_gap=cor(G,D),
    corr_gpa_baseline_math=cor(G,M),corr_gap_baseline_math=cor(D,M))
  sg <- prep[feature==gv,sd]; sd_gap <- prep[feature==dv,sd]
  # At centered reference values, re-express the pair in current GPA and
  # baseline SIMCE math units. No rescaling of a reported SE by hand.
  R <- matrix(0,2,length(coef(fit)),dimnames=list(c('Current GPA at fixed baseline math','Baseline math at fixed current GPA'),names(coef(fit))))
  R[1,paste0(gv,'__z')] <- 1/sg; R[1,paste0(dv,'__z')] <- 1/sd_gap
  R[2,paste0(dv,'__z')] <- -1/sd_gap
  b <- as.numeric(R %*% coef(fit)); V <- R %*% vcov(fit) %*% t(R)
  pair_transforms[[as.character(g)]] <- data.table(grade=g,parameter=rownames(R),estimate_pp=100*b,se_pp=100*sqrt(diag(V)))
}
write_audit(rbindlist(correlations),'pair_correlations')
write_audit(rbindlist(pair_transforms),'pair_reexpression')

# Re-express the ORIGINAL all-variable regression as well. In that model the
# language gap also includes G, so its coefficient contributes to G's slope.
log_message('Reading original OLS once for exact coefficient/covariance re-expression.')
original <- readRDS(file.path(out,'voluntary_ols/model.rds'))
original_b <- coef(original); original_V <- vcov(original); rm(original); invisible(gc())
original_reexpression <- list()
for(g in 5:8) {
  vs <- paste0(c('z_gpa','gap_math','gap_language'),'_g',g)
  ss <- prep$sd[match(vs,prep$feature)]
  R <- matrix(0,3,length(original_b),dimnames=list(c('Current peer GPA at fixed math and language baselines',
    'Baseline SIMCE math at fixed GPA and language','Baseline SIMCE language at fixed GPA and math'),names(original_b)))
  R[1,paste0(vs,'__z')] <- 1/ss
  R[2,paste0(vs[2],'__z')] <- -1/ss[2]
  R[3,paste0(vs[3],'__z')] <- -1/ss[3]
  b <- as.numeric(R %*% original_b); V <- R %*% original_V %*% t(R)
  original_reexpression[[as.character(g)]] <- data.table(grade=g,parameter=rownames(R),estimate_pp=100*b,se_pp=100*sqrt(diag(V)))
  # Exact polynomial identity for the linear and quadratic contribution,
  # evaluated on observed G/M/L with arbitrary units, not a refitted proxy.
  means <- prep$mean[match(vs,prep$feature)]
  rawG <- c(-1.3,.2,1.1); rawM <- c(.2,-.5,.8); rawL <- c(-.6,.5,.1)
  A <- rawG-means[1]; B <- rawM-(means[1]-means[2]); C <- rawL-(means[1]-means[3])
  linear <- original_b[paste0(vs,'__z')]/ss
  quadratic <- original_b[paste0(vs,'__sq')]/ss^2
  direct <- linear[1]*A+linear[2]*(A-B)+linear[3]*(A-C)+quadratic[1]*A^2+quadratic[2]*(A-B)^2+quadratic[3]*(A-C)^2
  expanded <- sum(linear)*A-linear[2]*B-linear[3]*C+sum(quadratic)*A^2+quadratic[2]*B^2+
    quadratic[3]*C^2-2*quadratic[2]*A*B-2*quadratic[3]*A*C
  check(paste('grade',g,'exact quadratic reparameterization'),max(abs(direct-expanded))<1e-12)
}
write_audit(rbindlist(original_reexpression),'original_reexpression')
write_audit(rbindlist(coefficients),'coefficients')
write_audit(rbindlist(summaries),'model_summary')
write_audit(rbindlist(audit),'model_audit')

# The actual joint Wald tests use the stored clustered covariance matrix.
joint <- fitted_models[['joint_academic_pc1']]
wald_pc1 <- wald(joint,keep=paste0('^(',paste(pc1,collapse='|'),')$'),print=FALSE)
write_audit(data.table(test='Four year-specific PC1 coefficients jointly zero',F=wald_pc1$stat,
  df1=wald_pc1$df1,df2=wald_pc1$df2,p_value=wald_pc1$p),'pca_joint_test')

# Independent clustered covariance and normal-equation check for the key
# same-sample grade-8 pair, with the exact model-implied correction.
fit <- fitted_models[['pair_g8_both']]
rows <- which(is.finite(d$z_gpa_g8)&is.finite(d$gap_math_g8))
X <- model.matrix(fit$fml_all$linear,data=d[rows])[,names(coef(fit)),drop=FALSE]
e <- d$applied_sae[rows]-as.numeric(X%*%coef(fit))
inv <- solve(crossprod(X)); scores <- rowsum(X*e,d$origin_rbd[rows],reorder=FALSE)
G <- uniqueN(d$origin_rbd[rows]); N <- length(rows); K <- ncol(X)
independent <- (G/(G-1))*((N-1)/(N-K))*inv%*%crossprod(scores)%*%inv
check('key pair OLS normal equations',max(abs(crossprod(X,e)))/N<1e-8)
check('key pair exact clustered SE reconstruction',max(abs(sqrt(diag(independent))-sqrt(diag(vcov(fit)))))<1e-7)
check('same-sample pair comparisons',all(vapply(5:8,function(g) uniqueN(rbindlist(summaries)[grepl(paste0('^pair_g',g),model),students])==1L,logical(1))))
check('single measure formulas contain only that academic measure',all(vapply(academic,function(v) {
  vars <- all.vars(fitted_models[[paste0('one_',v)]]$fml_all$linear)
  all(vars[grepl('_g[5-8]__',vars)] %in% terms_for(v))
},logical(1))))
log_message('Separate-measure and PCA regressions completed.')
print(rbindlist(coefficients)[model %in% c('one_z_gpa_g8','one_gap_math_g8','pair_g8_both','joint_academic_pc1') &
  term %in% c('z_gpa_g8__z','gap_math_g8__z',pc1),.(model,term,estimate_pp,se_pp)])
