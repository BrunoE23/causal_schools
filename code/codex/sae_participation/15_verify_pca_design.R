# Verify that rebuilding the raw frame preserved the previous regressor units.
source('code/codex/sae_participation/common.R')
new_out <- file.path(out,'interpretation_pca')
d <- readRDS(file.path(new_out,'analysis_frame.rds'))
school_split <- fread(file.path(out,'school_split.csv'))
old <- readRDS(file.path(out,'voluntary/models/full.rds'))
checks <- fread(file.path(new_out,'regression_verification.csv'))
common_names <- intersect(names(old$data),names(d))
train_schools <- school_split[split=='train',origin_rbd]
new_train <- d[origin_rbd %in% train_schools,common_names,with=FALSE]
old_train <- old$data[,common_names,with=FALSE]
ok_train <- isTRUE(all.equal(new_train,old_train,check.attributes=FALSE,tolerance=1e-12))
stopifnot(ok_train)
rm(old,old_train,new_train); invisible(gc())
old_test <- readRDS(file.path(out,'voluntary/evaluation_frame.rds'))$data
new_test <- d[!origin_rbd %in% train_schools]
ok_test <- identical(new_test$mrun,old_test$mrun) &&
  isTRUE(all.equal(new_test[,common_names,with=FALSE],old_test[,common_names,with=FALSE],check.attributes=FALSE,tolerance=1e-12))
stopifnot(ok_test)
checks <- checks[!check %in% c('rebuilt training design equals original','rebuilt evaluation design equals original')]
checks <- rbind(checks,data.table(check=c('rebuilt training design equals original','rebuilt evaluation design equals original'),passed=c(ok_train,ok_test)))
fwrite(checks,file.path(new_out,'regression_verification.csv'))
cat(nrow(checks),'regression/design checks passed, plus PCA verification.\n')
