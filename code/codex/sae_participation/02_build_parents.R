source('code/codex/sae_participation/common.R')
cw <- fread('code/codex/sae_participation/parent_crosswalk.csv',colClasses='character',na.strings='')
cw[,max_code:=as.integer(max_code)]
simce <- readRDS(file.path(out,'baseline_simce.rds'))
all <- list(); dictionary <- list(); audit <- list()
for(year in 2013:2016) {
  log_message('Parent questionnaire ',year)
  map <- cw[!is.na(get(paste0('code',year)))]
  map[,raw_code:=paste0('cpad_p',get(paste0('code',year)))]
  d <- fread(file.path(out,paste0('glosa_',year,'__cpad.csv')))
  d <- d[grepl('^cpad_p',V1),.(raw_code=V1,question=V2,source_coding=V3)]
  map <- merge(map,d,by='raw_code',all.x=TRUE)
  stopifnot(!anyNA(map$question),!anyDuplicated(map$feature))
  p <- one_file(file.path(simce_folder(year),'Archivos TXT (Planos)'),'cpad.*csv$')
  z <- read_cols(p,c('idalumno','rbd',map$raw_code),'|')
  stopifnot(all(map$raw_code %in% names(z)))
  z <- unique(z)
  if(anyDuplicated(z$idalumno)) stop('Conflicting parent records within year; needs explicit adjudication.')
  z[,`:=`(simce_year=year,parent_record=1L)]
  for(j in seq_len(nrow(map))) {
    v <- as.numeric(z[[map$raw_code[j]]]); v[!v %in% seq_len(map$max_code[j])] <- NA_real_
    reverse <- (year==2013 && map$reverse2013[j]=='1') || (year==2016 && map$reverse2016[j]=='1')
    if(reverse) v <- map$max_code[j]+1-v
    set(z,j=paste0('parent_',map$feature[j]),value=v)
  }
  z[, (map$raw_code):=NULL]
  setnames(z,'rbd','parent_rbd')
  z <- merge(simce[simce_year==year,.(mrun,idalumno,simce_year,simce_rbd)],z,by=c('idalumno','simce_year'),all.x=TRUE)
  z[,parent_school_matches_simce:=as.integer(parent_rbd==simce_rbd)]
  # Keep a failed school linkage explicit rather than assigning another school's views.
  wrong <- which(!is.na(z$parent_rbd)&z$parent_rbd!=z$simce_rbd)
  for(v in paste0('parent_',map$feature)) if(length(wrong)) set(z,wrong,v,NA_real_)
  all[[as.character(year)]] <- z[,c('mrun','simce_year','parent_record','parent_rbd','parent_school_matches_simce',paste0('parent_',map$feature)),with=FALSE]
  dictionary[[as.character(year)]] <- map[,.(simce_year=year,feature=paste0('parent_',feature),label,raw_code,max_code,question,source_coding,
    reversed=as.integer((year==2013 & reverse2013=='1')|(year==2016 & reverse2016=='1')))]
  audit[[as.character(year)]] <- data.table(simce_year=year,students=nrow(z),parent_record=sum(z$parent_record==1L,na.rm=TRUE),school_disagreements=length(wrong))
}
parents <- rbindlist(all,fill=TRUE)
stopifnot(!anyDuplicated(parents$mrun))
saveRDS(parents,file.path(out,'parent_predictors.rds'))
audit_write(rbindlist(dictionary),'parent_dictionary'); audit_write(rbindlist(audit),'parent_linkage_audit')
log_message('Saved ',nrow(parents),' parent linkage rows and ',nrow(cw),' candidate questions/constructs.')
