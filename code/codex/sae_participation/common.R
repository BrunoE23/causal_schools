suppressPackageStartupMessages(library(data.table))
if (.Platform$OS.type == 'windows') invisible(Sys.setlocale('LC_CTYPE','English_United States.utf8'))
setDTthreads(4L)
root <- normalizePath('.', winslash='/')
data_root <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD','C:/Users/brunem/Box/causal_schools')
out <- file.path(root,'data/clean/sae_participation')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
one_file <- function(folder,pattern) {
  p <- list.files(folder,pattern,full.names=TRUE,ignore.case=TRUE)
  p <- p[!startsWith(basename(p),'~$')]
  if(length(p)!=1L || !file.exists(p)) stop('Expected one input: ',folder,' / ',pattern)
  p
}
read_cols <- function(path,cols,sep='auto',dec='auto') {
  h <- names(fread(path,nrows=0,sep=sep,showProgress=FALSE))
  take <- h[tolower(h) %in% tolower(cols)]
  z <- fread(path,select=take,sep=sep,dec=dec,showProgress=FALSE,na.strings=c('','NA'))
  setnames(z,tolower(names(z)))
  z
}
simce_folder <- function(year) {
  p <- list.dirs(file.path(data_root,'data/raw/simce'),recursive=FALSE)
  p <- p[grepl(paste0('cuarto.*',year),p)]
  stopifnot(length(p)==1L)
  p
}
safe_z <- function(x) {
  x[!is.finite(x)] <- NA_real_
  s <- sd(x,na.rm=TRUE)
  if(!is.finite(s)||s<=0) return(rep(NA_real_,length(x)))
  (x-mean(x,na.rm=TRUE))/s
}
audit_write <- function(x,name) fwrite(x,file.path(out,paste0(name,'.csv')))
log_message <- function(...) message(format(Sys.time(),'%H:%M:%S'),' ',...)

# fixest reconstructs factor levels from its original estimation data when
# predicting. RDS files do not serialize the global training environment.
# Restore the exact training columns from the shared saved frame instead of
# duplicating that large frame inside all seven model files.
restore_model <- function(name,frame=NULL) {
  if(is.null(frame)) frame <- readRDS(file.path(out,'model_frame.rds'))
  fit <- readRDS(file.path(out,'models',paste0(name,'.rds')))
  required <- unique(c(all.vars(fit$fml_all$linear),'origin_rbd'))
  stopifnot(all(required %in% names(frame$data)))
  fit$data <- frame$data[frame$train,required,with=FALSE]
  fit
}
