suppressPackageStartupMessages(library(data.table))
.libPaths(c('data/clean/staff_lasso_va/r_library',.libPaths()))
source('code/codex/staff_lasso_va/lasso_helpers.R')
glmnet::glmnet.control(fdev=0,devmax=1)
set.seed(72)
raw <- cbind(signal=rnorm(250),noise=rnorm(250),partial=rnorm(250),
  constant=1,all_missing=NA_real_,absent=rep(c(0,1),125))
raw[1:35,'partial'] <- NA_real_
y <- 2*raw[,'signal']+rnorm(250,sd=.2)
p <- sl_preprocess(raw[1:200,])
stopifnot(!p$keep['constant'],!p$keep['all_missing'],all(is.finite(p$x)),
  max(abs(colMeans(p$x)))<1e-12,max(abs(colMeans(p$x^2)-1))<1e-12)
test <- raw[201:250,]; test[,'partial'] <- NA_real_
trans <- sl_transform(test,p)
expected <- (p$medians['partial']-p$center['partial'])/p$scale['partial']
stopifnot(max(abs(trans[,'partial']-expected))<1e-12)
saved <- p
test[,'signal'] <- 1e9
invisible(sl_transform(test,p))
stopifnot(identical(p,saved))
lambda <- exp(seq(log(1),log(.0005),length.out=70))
model <- sl_path(raw,y,lambda)
tune <- sl_tune(raw,y,sample(rep(1:5,length.out=250)),lambda)
stopifnot(tune$index['one_se']<=tune$index['minimum'])
b <- sl_coefficients(model,.1)
stopifnot(b[FEATURE=='signal',BETA_SD]>.8,b[FEATURE=='all_missing',BETA_SD]==0,
  sl_kkt(model,y,.1)<2e-3)
# At the exact fitted lambda grid, coefficients and predictions must agree.
lam <- lambda[tune$index['one_se']]
bet <- as.matrix(coef(model$fit,s=lam))
manual <- model$ym+model$ys*(bet[1,1]+as.numeric(model$prep$x %*% bet[-1,1]))
stopifnot(max(abs(manual-as.numeric(sl_predict(model,raw,lam))))<1e-10)
cat('PASS: training-only medians, constant/all-missing features, transform isolation, scaling, penalty choice, KKT and prediction reconstruction.\n')
