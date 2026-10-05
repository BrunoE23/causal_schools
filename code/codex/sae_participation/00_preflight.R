suppressPackageStartupMessages(library(data.table))
if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','English_United States.utf8')
setDTthreads(4L)
root <- normalizePath('.', winslash = '/')
data_root <- Sys.getenv('CAUSAL_SCHOOLS_DATA_WD', 'C:/Users/brunem/Box/causal_schools')
out <- file.path(root, 'data/clean/sae_participation')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cat('PACKAGES\n')
print(sapply(c('data.table','readxl','fixest','ggplot2'), requireNamespace, quietly=TRUE))
manifest <- list()
inspect <- function(path, tag, sep='auto') {
  stopifnot(length(path)==1L, file.exists(path))
  x <- fread(path, nrows=0L, sep=sep, showProgress=FALSE)
  manifest[[length(manifest)+1L]] <<- data.table(tag=tag,path=path,bytes=file.info(path)$size,columns=paste(names(x),collapse='|'))
  cat('\n',tag,': ',paste(names(x),collapse=', '),'\n',sep='')
}
for (year in 2013:2020) {
  path <- list.files(file.path(data_root,'data/raw/student_tracking',year,paste0('Rendimiento-',year)), '\\.csv$', full.names=TRUE)
  inspect(path,paste0('tracking_',year),';')
}
for (year in 2017:2020) {
  folder <- file.path(data_root,'data/raw',year,paste0('SAE_',year))
  for (prefix in c('A1_','C1_','C2_')) inspect(list.files(folder,paste0('^',prefix,'.*\\.csv$'),full.names=TRUE),paste0(prefix,year))
  inspect(list.files(file.path(data_root,'data/raw/school_directory',year),'\\.csv$',full.names=TRUE),paste0('directory_',year),';')
}
for (year in 2013:2016) {
  folder <- list.dirs(file.path(data_root,'data/raw/simce'),recursive=FALSE,full.names=TRUE)
  folder <- folder[grepl(paste0('cuarto.*',year),folder)]
  for (type in c('alu','cpad')) inspect(list.files(file.path(folder,'Archivos TXT (Planos)'),paste0(type,'.*\\.csv$'),full.names=TRUE),paste0(type,year),'|')
  glosa <- list.files(folder,'^simce.*GLOSAS.*xlsx$',full.names=TRUE)
  cat('\nGLOSA ',year,' SHEETS: ',paste(readxl::excel_sheets(glosa),collapse=', '),'\n',sep='')
  for (sheet in readxl::excel_sheets(glosa)) {
    x <- as.data.table(readxl::read_excel(glosa,sheet=sheet,col_names=FALSE,.name_repair='minimal'))
    setnames(x,paste0('V',seq_len(ncol(x))))
    fwrite(x,file.path(out,paste0('glosa_',year,'_',gsub('[^A-Za-z0-9]','_',sheet),'.csv')))
    if (grepl('cpad|padre',sheet,ignore.case=TRUE)) print(head(x,12))
  }
}
fwrite(rbindlist(manifest),file.path(out,'input_manifest.csv'))
