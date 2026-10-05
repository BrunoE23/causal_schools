# Idempotent migration for panels constructed before the Nuble-2018 audit fix.
source('code/codex/sae_participation/common.R')
panel <- readRDS(file.path(out,'admin_panel.rds'))
panel[cohort_gr8==2018 & origin_region==16,sae_region_covered:=1L]
panel[,analysis_eligible:=as.integer(sae_region_covered==1L & !is.na(origin_rbd))]
saveRDS(panel,file.path(out,'admin_panel.rds'))
cov <- fread(file.path(out,'region_year_coverage.csv'))
cov <- unique(rbind(cov,data.table(cohort_gr8=2018L,origin_region=16L,sae_region_covered=1L)))
audit_write(cov,'region_year_coverage')
audit_write(panel[,.(students=.N,applicants=sum(applied_sae),regular=sum(applied_regular),application_rate=mean(applied_sae)),by=.(cohort_gr8,analysis_eligible,continuation_group)],'sample_summary')
cat('Eligible students:',sum(panel$analysis_eligible),'\n')
