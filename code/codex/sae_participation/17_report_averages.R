# Compact presentation of the averaged-history regressions; no refitting.
source('code/codex/sae_participation/common.R')
average_out <- file.path(out,'averaged_grades')
ct <- fread(file.path(average_out,'coefficients.csv'))
sm <- fread(file.path(average_out,'model_summary.csv'))
checks <- fread(file.path(average_out,'verification.csv'))
coverage <- fread(file.path(average_out,'coverage.csv'))
prep <- fread(file.path(average_out,'preprocessing.csv'))
prior <- fread(file.path(out,'interpretation_pca/model_summary.csv'))
pca_coverage <- fread(file.path(out,'interpretation_pca/pca_coverage.csv'))
pca_variance <- fread(file.path(out,'interpretation_pca/pca_variance.csv'))
stopifnot(all(checks$passed),!anyDuplicated(ct[,.(model,term)]))
fmt <- function(x,digits=3L) formatC(x,digits=digits,format='f')
num <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
cell <- function(model_name,term_name) {
  z <- ct[model==model_name & term==term_name]
  stopifnot(nrow(z)==1L,is.finite(z$estimate_pp),is.finite(z$se_pp))
  paste0(fmt(z$estimate_pp),' (',fmt(z$se_pp),')')
}
stems <- c(gpa='GPA',attendance='Attendance',z_gpa='GPA relative to peers',
  z_attendance='Attendance relative to peers',gap_math='GPA minus baseline SIMCE math',
  gap_language='GPA minus baseline SIMCE language')
academic_table <- function(joint=FALSE) rbindlist(lapply(names(stems),function(s) {
  model_name <- if(joint) 'joint_avg58' else paste0('one_',s)
  data.table(`Average across grades 5-8`=unname(stems[s]),
    `Linear coefficient (SE)`=cell(model_name,paste0(s,'_avg58__z')),
    `Squared term (SE)`=cell(model_name,paste0(s,'_avg58__sq')))
}))
one <- academic_table(); joint <- academic_table(TRUE)
pca <- rbindlist(lapply(2013:2016,function(yr) data.table(`Survey year`=yr,
  `Complete questionnaires`=num(pca_coverage[survey_year==yr,complete_students]),
  `Variance explained (%)`=fmt(100*pca_variance[survey_year==yr & component==1,share],1),
  `PC1 coefficient (SE)`=cell('joint_avg58',paste0('survey_pc1_',yr)))))
periods <- rbindlist(lapply(names(stems),function(s) data.table(Measure=unname(stems[s]),
  `Grades 5-6: linear (SE)`=cell(paste0('periods_',s),paste0(s,'_avg56__z')),
  `Grades 7-8: linear (SE)`=cell(paste0('periods_',s),paste0(s,'_avg78__z')),
  `Grades 5-6: squared (SE)`=cell(paste0('periods_',s),paste0(s,'_avg56__sq')),
  `Grades 7-8: squared (SE)`=cell(paste0('periods_',s),paste0(s,'_avg78__sq')))))
pair <- data.table(Measure=c('GPA relative to peers','GPA minus baseline SIMCE math'),
  `Alone: linear (SE)`=c(cell('paired_gpa_only','paired_gpa_avg58__z'),cell('paired_gap_only','paired_gap_avg58__z')),
  `Together: linear (SE)`=c(cell('paired_both','paired_gpa_avg58__z'),cell('paired_both','paired_gap_avg58__z')),
  `Alone: squared (SE)`=c(cell('paired_gpa_only','paired_gpa_avg58__sq'),cell('paired_gap_only','paired_gap_avg58__sq')),
  `Together: squared (SE)`=c(cell('paired_both','paired_gpa_avg58__sq'),cell('paired_both','paired_gap_avg58__sq')))
fit_table <- data.table(Specification=c('One average, grades 5-8','Two averages, grades 5-6 and 7-8','Separate grades 5, 6, 7, 8'),
  `Academic measures`=c(6L,12L,24L),
  `R-squared`=fmt(c(sm[model=='joint_avg58',r_squared],sm[model=='joint_two_periods',r_squared],
    prior[model=='joint_academic_pc1',r_squared]),4))
coverage_table <- rbindlist(lapply(names(stems),function(s) {
  feature_name <- paste0(s,'_avg58')
  z <- coverage[feature==feature_name]
  data.table(Measure=unname(stems[s]),`All 4 grades`=num(z[observed_grades==4,students]),
    `1-3 grades`=num(sum(z[observed_grades %in% 1:3,students])),
    `No observed grades`=num(sum(z[observed_grades==0,students])),
    `SD of average`=fmt(prep[feature==feature_name,sd]))
}))
tables <- list(one_at_a_time=one,joint=joint,pca=pca,two_periods=periods,paired=pair,
  fit_comparison=fit_table,coverage=coverage_table)
for(nm in names(tables)) fwrite(tables[[nm]],file.path(average_out,paste0('table_',nm,'.csv')))

joint_sm <- sm[model=='joint_avg58']
g <- ct[model=='one_z_gpa' & term=='z_gpa_avg58__z',estimate_pp]
g_sq <- ct[model=='one_z_gpa' & term=='z_gpa_avg58__sq',estimate_pp]
math <- ct[model=='one_gap_math' & term=='gap_math_avg58__z',estimate_pp]
pcs <- ct[model=='joint_avg58' & grepl('^survey_pc1_',term),estimate_pp]
early <- ct[model=='periods_z_gpa' & term=='z_gpa_avg56__z',estimate_pp]
late <- ct[model=='periods_z_gpa' & term=='z_gpa_avg78__z',estimate_pp]
intro <- sprintf('Six academic summaries replace the 24 grade-specific measures. Each summary averages the available observations in grades 5-8, then enters with its square. The outcome remains voluntary SAE application: %s applicants among %s students (%.2f%%) whose grade-8 school offers grade 9.',
  num(joint_sm$applicants),num(joint_sm$students),100*joint_sm$applicants/joint_sm$students)
reading <- paste('Coefficients and school-clustered standard errors are in percentage points.',
  'Each row below is a separate regression with that academic average and its square, plus the same context and survey-PC1 controls.',
  'An average is standardized using its mean and SD among the original training pupils.',
  'The linear coefficient is the slope at that reference mean; with squares, a change from the mean to one SD above it is the sum of the linear and squared coefficients.')
findings <- c(
  sprintf('Peer-relative GPA has a near-zero linear coefficient (%+.3f), but a negative squared term (%+.3f). Its fitted association is therefore an inverted U, not no association: moving from the mean to one SD above or below it changes fitted application by %+.2f or %+.2f pp, respectively.',g,g_sq,g+g_sq,-g+g_sq),
  sprintf('The average GPA-minus-SIMCE-math gap has a negative linear coefficient (%+.3f pp), while its squared coefficient is close to zero.',math),
  sprintf('More favorable parent-survey PC1 scores remain associated with %.2f-%.2f pp less application per SD across survey years, controlling jointly for the six academic averages.',min(-pcs),max(-pcs)),
  sprintf('Averaging can hide timing: when grades 5-6 and 7-8 peer-relative GPA averages enter together, their linear coefficients are %+.2f and %+.2f pp. The compact average is useful for presentation, but the two-period check retains this distinction.',early,late))
pair_note <- sprintf('These three regressions use the same %s pupils and average GPA and its math gap over exactly the same observed grades. This preserves D = G - M, where M is baseline math relative to later peers. In a joint linear specification aG + bD = (a+b)G - bM. A larger gap holding GPA fixed means lower baseline math; a higher GPA holding the gap fixed also means higher baseline math. The table uses separately standardized averages, so undo their SDs before combining coefficients. Squared terms make the slopes local. Main-table averages can use different available grades; the exact identity applies to this matched-grade comparison.',
  num(sm[model=='paired_both',students]))
notes <- c(
  'All main regressions use the same 313,813 pupils and 2,000 origin schools. Applicants and nonapplicants are included. Offering grade 9 defines the voluntary margin operationally; applications do not establish actual moves or motives.',
  'For each measure, take the arithmetic mean of the available grade-level values, weighting observed grades equally. GPA/attendance use their original units; relative measures use the previously constructed school-grade-year peer z-scores. Gaps average the existing peer-GPA z minus baseline grade-4 SIMCE peer z; they are not learning gains.',
  'Normalize each new average with its own observed original-training mean and SD. If all grades are missing, impute the training median. Every included average has categorical controls for its number of observed grades (0-4, or 0-2 in the two-period check). These controls retain all pupils but do not eliminate all selection concerns. The quadratic term is the square of the standardized average, not the average of grade-specific squares.',
  'Common controls: cohort-by-region, school sector, gender, age and its square/missingness, grade-4 versus grade-8 school match/missingness, parent-record indicator, survey year, fraction of applicable questions answered, and year-specific complete-survey indicators and PC1 scores. One-at-a-time models contain no other academic measure.',
  'Survey PC1 is reused unchanged from the previous analysis: correlation PCA separately for 2013-2016 on 19, 29, 20 and 16 harmonized questions among complete respondents, with higher values indicating favorable answers. Scores have within-year mean zero and SD one; zero scores plus completeness controls retain incomplete respondents. The 218,241 complete questionnaires cover 69.5% of the sample. Scores concern the grade-4 school, which may differ from the grade-8 origin.',
  'All fits are OLS linear probability models with standard errors clustered at origin school. Inference conditions on the estimated PCA scores. These are descriptive associations; annual grade-8 inputs may postdate application. Geography remains deferred.',
  'The fit comparison is in-sample R-squared, not held-out predictive performance. All three fits use the same pupils and context/PC1 controls, with squares. Their academic coverage controls differ, so this is not a formal nested-model test.',
  sprintf('Verification: %s checks passed, including sample counts, observed-grade averaging, the matched-grade gap identity, model composition, OLS normal equations, and an independent reconstruction of the compact joint model school-clustered covariance.',num(nrow(checks))))

esc <- function(x) { x <- gsub('&','&amp;',x,fixed=TRUE); x <- gsub('<','&lt;',x,fixed=TRUE); gsub('>','&gt;',x,fixed=TRUE) }
htmltable <- function(z) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(z)),'</th>',collapse=''),'</tr></thead><tbody>',
  paste(apply(z,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
paragraphs <- function(x) paste0('<p>',esc(x),'</p>',collapse='')
details <- function(title,z,note='') paste0('<details><summary>',esc(title),'</summary>',paragraphs(note),htmltable(z),'</details>')
report_dir <- file.path(root,'output/reports/sae_participation')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Voluntary SAE: average academic histories</title>',
  '<style>body{font:16px/1.55 system-ui,sans-serif;color:#243347;background:#f5f7fa;margin:0}main{max-width:1080px;margin:30px auto;padding:34px;background:#fff;border-radius:12px}h1{font-size:30px;line-height:1.2}h2{font-size:21px;margin-top:30px}.tag{font-size:12px;font-weight:700;color:#216b86;text-transform:uppercase}table{border-collapse:collapse;width:100%;font-size:14px}th{background:#e9eff4;text-align:left}th,td{padding:10px;border-bottom:1px solid #dde3ea}td:not(:first-child){white-space:nowrap;font-variant-numeric:tabular-nums}.table{overflow-x:auto}details{margin:20px 0;padding:14px;border:1px solid #dde3ea;border-radius:6px}summary{font-weight:600;cursor:pointer}.notes{font-size:14px}li{margin-bottom:10px}a{color:#176b87}@media(max-width:650px){main{padding:18px;margin:10px}h1{font-size:25px}}@media print{body{background:white}main{margin:0;padding:0}tr{break-inside:avoid}}</style></head><body><main>',
  '<div class="tag">October 10, 2026 · Voluntary applications · Averaged grades</div><h1>One academic history per measure</h1>',paragraphs(intro),
  '<h2>One measure at a time: grades 5-8 average</h2>',paragraphs(reading),htmltable(one),
  '<h2>What the averages show</h2>',paragraphs(findings),
  '<h2>Parent-survey PCA, with all six averages</h2>',paragraphs('Higher PC1 means more favorable parent reports. Each coefficient is for a one-SD higher score within that survey year. The six academic averages and their squares enter together.'),htmltable(pca),
  details('All six academic averages entered together',joint,'Large changes from the separate regressions reflect conditioning on overlapping measures. In particular, controlling for absolute GPA and peer-relative GPA together changes what is held fixed.'),
  details('Timing check: grades 5-6 versus grades 7-8',periods,'Each row is one regression containing both period averages and both squares for that measure, plus common controls and observed-grade counts. Other academic measures are excluded.'),
  details('GPA and the math gap on identical observed grades',pair,pair_note),
  details('Model fit',fit_table,notes[7]),details('Available grade histories and scaling',coverage_table,'The SD is computed among original training pupils with an observed average. GPA SD is in grade points; attendance SD is in percentage points; relative measures use their underlying z-score units.'),
  '<details><summary>Definitions and verification</summary><ul class="notes">',paste0('<li>',esc(notes),'</li>',collapse=''),'</ul></details>',
  '<p><a href="voluntary_separate_pca.html">Grade-specific results and PCA loadings</a> · <a href="voluntary_ols.html">Original all-variable table</a></p></main></body></html>')
writeLines(html,file.path(report_dir,'voluntary_averages.html'))
mdtable <- function(z) c(paste0('| ',paste(names(z),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
  apply(z,1,function(r) paste0('| ',paste(r,collapse=' | '),' |')))
md <- c('# Voluntary SAE: average academic histories','',intro,'','## One measure at a time','',reading,'',mdtable(one),'',
  '## What the averages show','',paste(findings,collapse='\n\n'),'','## Parent-survey PCA with all six academic averages','',mdtable(pca),'',
  '## All six academic averages entered together','',mdtable(joint),'','## Grades 5-6 versus 7-8','',
  'Each row enters the two period averages and squares together; no other academic measure enters.','',mdtable(periods),'',
  '## GPA and math gap on identical observed grades','',pair_note,'',mdtable(pair),'','## Model fit','',notes[7],'',mdtable(fit_table),'',
  '## Coverage and scaling','',mdtable(coverage_table),'','## Definitions and verification','',paste0('- ',notes),'',
  '[Grade-specific results and PCA loadings](voluntary_separate_pca.html)')
writeLines(md,file.path(report_dir,'voluntary_averages.md'))
cat(intro,'\n'); print(one); print(pca); print(fit_table)
