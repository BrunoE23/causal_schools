# Fresh-session reproduction is necessary: in-session checks can retain the
# original fitting environment and conceal missing factor-reference data.
source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
suppressPackageStartupMessages(library(rpart))
vol_out <- file.path(out,'voluntary')
frame <- readRDS(file.path(vol_out,'evaluation_frame.rds'))
pred <- readRDS(file.path(vol_out,'heldout_predictions.rds'))
checks <- fread(file.path(vol_out,'verification.csv'))
check <- function(name,ok) {
  checks <<- rbind(checks,data.table(check=name,passed=isTRUE(ok)))
  fwrite(checks,file.path(vol_out,'verification.csv'))
  if(!isTRUE(ok)) stop('Failed check: ',name)
}
check('fresh-session evaluation IDs align',identical(pred$mrun,frame$data$mrun))
for(name in c('full','before_grade8','tree_full','tree_before_grade8')) {
  fit <- readRDS(file.path(vol_out,'models',paste0(name,'.rds')))
  if(inherits(fit,'fixest')) {
    check(paste(name,'contains training reference data'),!is.null(fit$data) && nrow(fit$data)==251624L)
    p <- as.numeric(predict(fit,newdata=frame$data,type='response'))
  } else {
    p <- as.numeric(predict(fit,newdata=frame$data))
    check(paste(name,'leaf probabilities in bounds'),all(fit$frame$yval>=0 & fit$frame$yval<=1))
  }
  check(paste(name,'fresh-session complete holdout reproduced'),max(abs(p-pred[[name]]))<1e-10)
  if(grepl('before_grade8',name)) {
    fields <- if(inherits(fit,'fixest')) all.vars(fit$fml_all$linear) else all.vars(fit$terms)
    check(paste(name,'no grade8 annual terms'),!any(grepl('_g8',fields)))
  }
  rm(fit); gc(verbose=FALSE)
}
check('all voluntary checks pass',all(checks$passed))
cat(nrow(checks),'voluntary checks passed.\n')
