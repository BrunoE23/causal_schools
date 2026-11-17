# Render every coefficient from the single joint voluntary-margin OLS regression.
source('code/codex/sae_participation/common.R')
ols_out <- file.path(out,'voluntary_ols')
write_audit <- function(z,name) fwrite(z,file.path(ols_out,paste0(name,'.csv')))
ct <- fread(file.path(ols_out,'coefficients.csv'))
omitted <- fread(file.path(ols_out,'omitted_collinear.csv'))
checks <- fread(file.path(ols_out,'verification.csv'))
summary <- fread(file.path(ols_out,'model_summary.csv'))
stopifnot(all(checks$passed))
N <- summary$students; G <- summary$schools; r2 <- summary$r_squared
cw <- fread('code/codex/sae_participation/parent_crosswalk.csv')
parent_names <- setNames(cw$label,paste0('parent_',cw$feature))
admin_names <- c(gpa='GPA',attendance='Attendance',z_gpa='GPA relative to peers',z_attendance='Attendance relative to peers',
  gap_math='GPA minus baseline SIMCE math (relative z-scores)',gap_language='GPA minus baseline SIMCE language (relative z-scores)')
fmt <- function(x,k=3) formatC(x,digits=k,format='f')
num <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
cell <- function(term_name) {
  row <- ct[term==term_name]
  if(nrow(row)!=1L) return('Omitted')
  paste0(fmt(row$estimate_pp),' (',fmt(row$se_pp),')')
}
admin_table <- rbindlist(lapply(names(admin_names),function(v) rbindlist(lapply(c('Linear','Quadratic'),function(kind) {
  row <- data.table(Variable=unname(admin_names[v]),Term=kind)
  for(g in 5:8) row[,(paste('Grade',g)):=cell(paste0(v,'_g',g,if(kind=='Linear') '__z' else '__sq'))]
  row
}))))
parent_table <- data.table(Variable=unname(parent_names),`Coefficient (SE)`=vapply(paste0(names(parent_names),'__z'),cell,character(1)))
other_vars <- c('age__z','age__sq','same_simce_origin__z','parent_record__z')
other_table <- data.table(Variable=c('Age: linear','Age: quadratic','Same grade-4 and grade-8 school','Parent questionnaire record linked'),
  `Coefficient (SE)`=vapply(other_vars,cell,character(1)))
stopifnot(!any(unlist(admin_table,use.names=FALSE)=='Omitted'),
  !any(parent_table[['Coefficient (SE)']]=='Omitted'),!any(other_table[['Coefficient (SE)']]=='Omitted'))
missing <- ct[kind=='Missingness',.(Variable=label,`Coefficient (SE)`=paste0(fmt(estimate_pp),' (',fmt(se_pp),')'))]
context <- ct[kind=='Context',.(Variable=term,`Coefficient (SE)`=paste0(fmt(estimate_pp),' (',fmt(se_pp),')'))]
write_audit(admin_table,'table_grades_attendance'); write_audit(parent_table,'table_parent_answers')
notes <- c(
  'Outcome: explicit grade-9 SAE application during grade 8, in either regular or complementary rounds. Sample: all eligible students whose grade-8 origin offers grade 9. All displayed coefficients come from the same joint OLS regression.',
  'Coefficients and standard errors are in percentage points. Numeric measures use the existing voluntary-training mean and SD; missing values retain the existing training-median imputation and missingness indicators. This preserves the constructed regressors while fitting the regression on the full sample.',
  'For grades, attendance, gaps and age, the linear row is the coefficient on the standardized value, and the quadratic row is the coefficient on its square. With a quadratic term, a one-SD increase starting at the mean has association equal to the linear plus quadratic coefficients. Parent items enter linearly, so their coefficients are percentage points per one training-sample SD.',
  'Same-school and parent-record indicators also retain their original standardization. Their coefficients are per one SD, not a full zero-to-one change. Their original SDs are saved in the preprocessing file.',
  'Higher parent ratings indicate more favorable reports after the documented scale reversals. They refer to the grade-4 school; same-school and response/missingness indicators are included jointly.',
  'Controls include cohort-by-region, school sector, gender, age and age squared. Standard errors cluster by grade-8 origin school. Omitted collinear terms are listed below. School-ending status is constant in this sample. Geography remains deferred.',
  'These are conditional descriptive associations. Grade-8 annual measures can postdate application. The model is an in-sample linear probability regression; its R-squared is not held-out prediction performance.',
  sprintf('Verification passed %s checks, including independent reconstruction of school-clustered standard errors, OLS normal equations, and R-squared.',nrow(checks))
)
intro <- sprintf('One joint linear probability regression. N = %s students; %s origin schools; %s applicants (%.2f%%); R-squared = %.4f. Each cell is coefficient (school-clustered standard error), in percentage points. All constructed predictors are entered together, including quadratic and missingness terms.',
  num(N),num(G),num(summary$applicants),100*summary$application_rate,r2)
sections <- list('Grades, attendance and SIMCE-relative gaps'=admin_table,'Parent questionnaire answers'=parent_table,
  'Age and questionnaire matching'=other_table,'Missingness indicators'=missing,'Context controls and intercept'=context)
mdtable <- function(z) c(paste0('| ',paste(names(z),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
  apply(z,1,function(r) paste0('| ',paste(r,collapse=' | '),' |')))
report_dir <- file.path(root,'output/reports/sae_participation')
md <- c('# Voluntary SAE application: one joint regression','',
  '[Latest compact table: grades 5-8 averages and survey PCA](voluntary_averages.html)','',
  '[October 10 update: separate predictors and parent-survey PCA](voluntary_separate_pca.html)','',intro,'')
for(s in names(sections)) md <- c(md,paste0('## ',s),'',mdtable(sections[[s]]),'')
md <- c(md,'## Notes','',paste0('- ',notes),'','## Omitted collinear terms','',paste0('- `',omitted$term,'`'))
writeLines(md,file.path(report_dir,'voluntary_ols.md'))
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
htmltable <- function(z) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(z)),'</th>',collapse=''),'</tr></thead><tbody>',
  paste(apply(z,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
body <- paste0(vapply(names(sections),function(s) paste0('<h2>',esc(s),'</h2>',htmltable(sections[[s]])),character(1)),collapse='')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Voluntary SAE application: joint OLS</title>',
  '<style>body{font:16px/1.5 system-ui,sans-serif;color:#243347;background:#f5f7fa;margin:0}main{max-width:1120px;margin:32px auto;padding:36px;background:white;border-radius:12px}h1{font-size:30px;line-height:1.2}h2{font-size:22px;margin-top:32px}table{border-collapse:collapse;width:100%;font-size:14px}th{background:#e9eff4;text-align:left}th,td{padding:9px;border-bottom:1px solid #dde3ea}td:not(:first-child){font-variant-numeric:tabular-nums;white-space:nowrap}.table{overflow-x:auto}.notes{font-size:14px}li{margin-bottom:9px}@media print{body{background:white}main{margin:0;padding:0}thead{display:table-header-group}tr{break-inside:avoid}}</style></head><body><main>',
  '<h1>Voluntary SAE application: one joint regression</h1><p><a href="voluntary_averages.html">Latest compact table: grades 5-8 averages and survey PCA</a></p><p><a href="voluntary_separate_pca.html">October 10 update: separate predictors and parent-survey PCA</a></p><p>',esc(intro),'</p>',body,
  '<h2>Notes</h2><ul class="notes">',paste0('<li>',esc(notes),'</li>',collapse=''),'</ul>',
  '<details><summary>Omitted collinear terms</summary><ul>',paste0('<li>',esc(omitted$term),'</li>',collapse=''),'</ul></details></main></body></html>')
writeLines(html,file.path(report_dir,'voluntary_ols.html'))
writeLines(capture.output(sessionInfo()),file.path(ols_out,'session_info.txt'))
cat(intro,'\n'); print(admin_table); print(parent_table)
