root <- normalizePath('.', winslash='/')
lib <- file.path(root, 'data/clean/staff_lasso_va/r_library')
dir.create(lib, recursive=TRUE, showWarnings=FALSE)
.libPaths(c(lib, .libPaths()))
if (!requireNamespace('glmnet', quietly=TRUE)) {
  install.packages('glmnet', lib=lib, repos='https://cloud.r-project.org', type='binary')
}
stopifnot(requireNamespace('glmnet', quietly=TRUE))
print(packageVersion('glmnet'))
