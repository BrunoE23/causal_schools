source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(ggplot2))
new_out <- file.path(out,'interpretation_pca')
write_audit <- function(z,name) fwrite(z,file.path(new_out,paste0(name,'.csv')))
ct <- fread(file.path(new_out,'coefficients.csv'))
sm <- fread(file.path(new_out,'model_summary.csv'))
coverage <- fread(file.path(new_out,'pca_coverage.csv'))
variance <- fread(file.path(new_out,'pca_variance.csv'))
loadings <- fread(file.path(new_out,'pca_loadings.csv'))
correlations <- fread(file.path(new_out,'pair_correlations.csv'))
reexpression <- fread(file.path(new_out,'original_reexpression.csv'))
checks <- fread(file.path(new_out,'regression_verification.csv'))
sample <- fread(file.path(new_out,'sample.csv'))
wald <- fread(file.path(new_out,'pca_joint_test.csv'))
stopifnot(all(checks$passed))
fmt <- function(x,k=3) formatC(x,digits=k,format='f')
num <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
cell <- function(model_name,term_name) {
  row <- ct[model==model_name & term==term_name]
  stopifnot(nrow(row)==1L)
  paste0(fmt(row$estimate_pp),' (',fmt(row$se_pp),')')
}
stems <- c(gpa='GPA',attendance='Attendance',z_gpa='GPA relative to peers',z_attendance='Attendance relative to peers',
  gap_math='GPA minus baseline SIMCE math',gap_language='GPA minus baseline SIMCE language')
academic_table <- function(joint=FALSE) rbindlist(lapply(names(stems),function(v) rbindlist(lapply(c('Linear','Quadratic'),function(kind) {
  row <- data.table(Variable=unname(stems[v]),Term=kind)
  for(g in 5:8) row[,(paste('Grade',g)):=cell(if(joint) 'joint_academic_pc1' else paste0('one_',v,'_g',g),
    paste0(v,'_g',g,if(kind=='Linear') '__z' else '__sq'))]
  row
}))))
one <- academic_table(); joint <- academic_table(TRUE)
pair_table <- rbindlist(lapply(5:8,function(g) data.table(Grade=g,
  Students=num(sm[model==paste0('pair_g',g,'_both'),students]),
  `GPA: alone`=cell(paste0('pair_g',g,'_gpa_only'),paste0('z_gpa_g',g,'__z')),
  `GPA: with math gap`=cell(paste0('pair_g',g,'_both'),paste0('z_gpa_g',g,'__z')),
  `Math gap: alone`=cell(paste0('pair_g',g,'_gap_only'),paste0('gap_math_g',g,'__z')),
  `Math gap: with GPA`=cell(paste0('pair_g',g,'_both'),paste0('gap_math_g',g,'__z')))))
pair_sq <- rbindlist(lapply(5:8,function(g) data.table(Grade=g,
  `GPA squared: alone`=cell(paste0('pair_g',g,'_gpa_only'),paste0('z_gpa_g',g,'__sq')),
  `GPA squared: joint`=cell(paste0('pair_g',g,'_both'),paste0('z_gpa_g',g,'__sq')),
  `Math gap squared: alone`=cell(paste0('pair_g',g,'_gap_only'),paste0('gap_math_g',g,'__sq')),
  `Math gap squared: joint`=cell(paste0('pair_g',g,'_both'),paste0('gap_math_g',g,'__sq')))))
pca_table <- rbindlist(lapply(2013:2016,function(yr) data.table(`Survey year`=yr,
  Items=coverage[survey_year==yr,items],`Complete questionnaires`=num(coverage[survey_year==yr,complete_students]),
  `PC1 variance (%)`=fmt(100*variance[survey_year==yr & component==1,share],1),
  `PC1: context controls`=cell('survey_only',paste0('survey_pc1_',yr)),
  `PC1: all academic controls`=cell('joint_academic_pc1',paste0('survey_pc1_',yr)))))
pca3_table <- rbindlist(lapply(2013:2016,function(yr) data.table(`Survey year`=yr,
  `PC1`=cell('joint_academic_pc123',paste0('survey_pc1_',yr)),
  `PC2`=cell('joint_academic_pc123',paste0('survey_pc2_',yr)),
  `PC3`=cell('joint_academic_pc123',paste0('survey_pc3_',yr)),
  `Variance, first 3 PCs (%)`=fmt(100*variance[survey_year==yr & component==3,cumulative],1))))
pc1_loadings <- loadings[component==1][order(survey_year,-abs(loading)),head(.SD,5),by=survey_year]
loading_table <- pc1_loadings[,.(Year=survey_year,Question=label,Loading=fmt(loading),`Correlation with PC1`=fmt(correlation_with_score))]
original_table <- reexpression[,.(Grade=grade,Parameter=parameter,`Coefficient (SE)`=paste0(fmt(estimate_pp),' (',fmt(se_pp),')'))]
cor_table <- correlations[,.(Grade=grade,Students=num(students),`Corr(GPA, gap)`=fmt(corr_gpa_gap),
  `Corr(GPA, baseline math)`=fmt(corr_gpa_baseline_math),`Corr(gap, baseline math)`=fmt(corr_gap_baseline_math))]
write_audit(one,'table_one_at_a_time'); write_audit(joint,'table_joint_academic')
write_audit(pair_table,'table_same_sample_pair'); write_audit(pca_table,'table_pca')

# Same observations within each grade: draw only the focused pair comparison.
plot_rows <- rbindlist(lapply(5:8,function(g) rbindlist(lapply(c('GPA relative to peers','GPA minus SIMCE math'),function(measure) {
  v <- if(measure=='GPA relative to peers') 'z_gpa' else 'gap_math'
  alone <- if(v=='z_gpa') 'gpa_only' else 'gap_only'
  rows <- ct[model %in% c(paste0('pair_g',g,'_',alone),paste0('pair_g',g,'_both')) & term==paste0(v,'_g',g,'__z')]
  rows[,`:=`(grade=g,measure=measure,Specification=ifelse(grepl('_both$',model),'Entered together','Entered separately'))]
  rows
}))))
chart <- ggplot(plot_rows,aes(estimate_pp,factor(grade),color=Specification))+
  geom_vline(xintercept=0,color='#939eaa',linetype='dashed')+
  geom_errorbar(aes(xmin=ci_low_pp,xmax=ci_high_pp),orientation='y',width=.12,position=position_dodge(width=.45))+
  geom_point(size=2.6,position=position_dodge(width=.45))+
  facet_wrap(~measure,scales='free_x')+scale_color_manual(values=c('Entered separately'='#176b87','Entered together'='#cb742d'))+
  labs(title='The coefficient depends on what else is held fixed',subtitle='Identical students within each grade; same context and survey-PC1 controls',
    x='Linear coefficient in percentage points (95% school-clustered interval)',y='Grade',color=NULL,
    caption='Each measure also enters quadratically. Coefficients use the original training-sample standardization.')+
  theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank(),plot.caption=element_text(hjust=0,size=9))
figure_dir <- file.path(root,'output/figures/sae_participation')
ggsave(file.path(figure_dir,'separate_gpa_math_gap.png'),chart,width=11,height=5.6,dpi=160,bg='white')
g <- ct[model=='one_z_gpa_g8' & term=='z_gpa_g8__z']
dg <- ct[model=='one_gap_math_g8' & term=='gap_math_g8__z']
jg <- ct[model=='joint_academic_pc1' & term=='z_gpa_g8__z']
jd <- ct[model=='joint_academic_pc1' & term=='gap_math_g8__z']
pc_coef <- ct[model=='joint_academic_pc1' & grepl('^survey_pc1_',term)]
key <- c(
  sprintf('Each separate regression includes one grade-specific measure and its square, context controls and year-specific survey PC1 scores. All use the same %s voluntary-margin students and school-clustered standard errors. The joint comparison enters all six measures for all four grades together with the same controls.',num(sample$students)),
  sprintf('For grade 8, peer-relative GPA changes from %+.3f (%s) alone to %+.3f (%s) jointly. The math gap changes from %+.3f (%s) alone to %+.3f (%s) jointly. These are coefficients on standardized regressors, in percentage points; the large joint coefficients are not standalone associations.',
    g$estimate_pp,fmt(g$se_pp),jg$estimate_pp,fmt(jg$se_pp),dg$estimate_pp,fmt(dg$se_pp),jd$estimate_pp,fmt(jd$se_pp)),
  sprintf('Survey PC1 explains %.1f-%.1f%% of item variation across survey years. Higher scores reflect more favorable reports. With all academic controls, one SD higher PC1 is associated with %.2f-%.2f percentage points less voluntary application across the four survey years.',
    100*min(variance[component==1,share]),100*max(variance[component==1,share]),min(-pc_coef$estimate_pp),max(-pc_coef$estimate_pp)),
  sprintf('PCA uses %s complete questionnaires (%.1f%% of the regression sample). Students without a complete questionnaire remain in every full-sample regression; missing scores are set to the zero reference with explicit year-specific completeness indicators.',
    num(sample$complete_survey),100*sample$complete_survey/sample$students)
)
interpretation <- c(
  'Let G be GPA standardized within school-grade-year peers, M baseline grade-4 SIMCE math relative to those peers, and D = G - M. The gap is a difference in relative positions, not a measured learning gain.',
  'In the linear part, a*G + b*(G-M) = (a+b)*G - b*M. Holding G fixed, a larger gap means lower baseline SIMCE math. Holding the gap fixed, a higher G also means a higher baseline score. Therefore the coefficient on G in the joint gap specification is not the association with current GPA holding baseline achievement fixed.',
  'The original regression also contains the language gap, so its linear coefficient contributes to the current-GPA re-expression too. The report below undoes each regressor SD before combining coefficients and computes SEs from the full original school-clustered covariance matrix.',
  'Quadratic terms matter: q*(G-M)^2 expands to q*G^2 - 2*q*G*M + q*M^2. The re-expressed coefficients below are local slopes at the original centered reference values; they are not globally constant effects. The original polynomial was verified algebraically.',
  'The full joint model also controls for absolute GPA, attendance, the language gap and the other grades. Changing peer-relative GPA while holding absolute GPA fixed also changes the peer comparison. Use the separate and same-sample pair regressions to distinguish these conditioning choices; the math-gap identity alone does not explain every difference.'
)
notes <- c(
  'Sample and outcome are unchanged: students whose grade-8 school offers grade 9, with an explicit regular or complementary grade-9 SAE application as the binary outcome. Main tables keep all 313,813 students; the focused pair table restricts to observed GPA and math gap within each grade and holds that sample fixed across its three regressions.',
  'All regressions are OLS linear probability models with standard errors clustered by grade-8 origin school. Coefficients and SEs are in percentage points. Academic scaling and median imputation retain the original voluntary-training moments; each included academic measure has its own missingness indicator.',
  'Common controls: cohort-by-region, sector, gender, age and square/missingness, grade-4 versus grade-8 school match/missingness, parent-record indicator, survey year, fraction of applicable questions answered and year-specific complete-survey indicators. The new specifications use survey PCs in place of the 39 separate answer coefficients. No other academic measure enters a one-at-a-time regression.',
  'PCA is computed separately within each survey year on complete respondents in this voluntary sample, using all the selected items actually asked that year: 19, 29, 20 and 16 items. Each item is centered and scaled among complete respondents; PCA uses their correlation matrix. No answers are imputed for PCA and application outcomes do not enter its construction.',
  'PC scores have mean zero and SD one among complete respondents of that year. PC1 is oriented toward favorable answers; its loadings are all positive. Scores for incomplete or unavailable questionnaires are zero with completeness controls. Year-specific slopes avoid assuming that different yearly questionnaires measure an identical construct.',
  'The main specification uses PC1. A prespecified sensitivity includes PCs 1-3 separately by year; PC2 and PC3 are contrasts with year-specific meanings, not additional common satisfaction indices. Their signs are oriented so their largest absolute loading is positive. Full loadings and explained variance are saved.',
  sprintf('The four PC1 coefficients in the all-academic model are jointly significant: school-clustered F(%s,%s) = %.3f, p = %.3g.',num(wald$df1),num(wald$df2),wald$F,wald$p_value),
  'Inference conditions on the estimated PCA scores; it does not incorporate loading-estimation uncertainty. Complete respondents may differ from incomplete respondents. These are conditional descriptive associations, not causal effects. Annual grade-8 measures can postdate application. Geography remains deferred.',
  sprintf('Verification: %s regression checks passed, plus PCA orthogonality, eigenvector, variance and score-standardization checks for each year. The grade-8 pair clustered covariance was independently reconstructed. All pair samples and one-at-a-time formulas were checked.',num(nrow(checks)))
)
stopifnot(all(loadings[component==1,loading]>0))
sections <- list('One grade-specific measure at a time'=one,'All academic measures together, with the same PCA controls'=joint,
  'GPA versus math gap: identical students within each grade'=pair_table,'Quadratic terms in the identical-sample comparison'=pair_sq,
  'Survey PCA coefficients'=pca_table,'Sensitivity: first three PCs, with all academic controls'=pca3_table,
  'Questions with the largest PC1 loadings'=loading_table,'Observed correlations in the pair samples'=cor_table,
  'Original regression re-expressed in current GPA and baseline achievement'=original_table)
mdtable <- function(z) c(paste0('| ',paste(names(z),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
  apply(z,1,function(r) paste0('| ',paste(r,collapse=' | '),' |')))
report_dir <- file.path(root,'output/reports/sae_participation')
md <- c('# GPA, baseline achievement and parent-survey PCA','',paste(key,collapse='\n\n'),'','## Reading GPA and the math-gap coefficient','',paste(interpretation,collapse='\n\n'),
  '','![Separate versus joint GPA and math-gap coefficients](../../figures/sae_participation/separate_gpa_math_gap.png)','')
for(s in names(sections)) md <- c(md,paste0('## ',s),'',mdtable(sections[[s]]),'')
md <- c(md,'## Definitions and checks','',paste0('- ',notes),'','[Original all-variable regression](voluntary_ols.html)')
writeLines(md,file.path(report_dir,'voluntary_separate_pca.md'))
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
htmltable <- function(z) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(z)),'</th>',collapse=''),'</tr></thead><tbody>',
  paste(apply(z,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
body <- paste0(vapply(names(sections),function(s) paste0('<h2>',esc(s),'</h2>',htmltable(sections[[s]])),character(1)),collapse='')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>GPA, SIMCE gaps and survey PCA</title>',
  '<style>body{font:16px/1.55 system-ui,sans-serif;color:#243347;background:#f5f7fa;margin:0}main{max-width:1200px;margin:32px auto;padding:36px;background:white;border-radius:12px}h1{font-size:31px;line-height:1.2}h2{font-size:22px;margin-top:34px}table{border-collapse:collapse;width:100%;font-size:14px}th{background:#e9eff4;text-align:left}td,th{padding:9px;border-bottom:1px solid #dde3ea}td:not(:first-child){font-variant-numeric:tabular-nums;white-space:nowrap}.table{overflow-x:auto}.notes{font-size:14px}li{margin-bottom:10px}img{max-width:100%;height:auto}.tag{color:#216b86;font-size:12px;font-weight:700;text-transform:uppercase}</style></head><body><main>',
  '<div class="tag">October 10, 2026 · Voluntary applications · Separate and joint OLS</div><h1>GPA, baseline achievement and parent-survey PCA</h1>',
  paste0('<p>',esc(key),'</p>',collapse=''),'<h2>Reading GPA and the math-gap coefficient</h2>',paste0('<p>',esc(interpretation),'</p>',collapse=''),
  '<img src="../../figures/sae_participation/separate_gpa_math_gap.png" alt="Separate and joint GPA/math-gap coefficients on identical samples">',body,
  '<h2>Definitions and checks</h2><ul class="notes">',paste0('<li>',esc(notes),'</li>',collapse=''),'</ul><p><a href="voluntary_ols.html">Original all-variable regression</a></p></main></body></html>')
writeLines(html,file.path(report_dir,'voluntary_separate_pca.html'))
cat(paste(key,collapse='\n'),'\n'); print(pair_table); print(pca_table)
