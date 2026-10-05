source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(ggplot2))
vol_out <- file.path(out,'voluntary')
report_dir <- file.path(root,'output/reports/sae_participation')
figure_dir <- file.path(root,'output/figures/sae_participation')
dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
metrics <- fread(file.path(vol_out,'performance.csv'))
sample <- fread(file.path(vol_out,'sample.csv'))
cal <- fread(file.path(vol_out,'calibration.csv'))
tree <- fread(file.path(vol_out,'tree_validation.csv'))
checks <- fread(file.path(vol_out,'verification.csv'))
stopifnot(all(checks$passed))
labels <- c(no_application='Predict no application for everyone',training_rate='Training application rate',
  pooled_full='Previous pooled full model',context='Voluntary logit: context',admin='+ Grades and attendance 5-8',
  gaps='+ SIMCE-relative gaps',availability='+ Parent availability and school match',full='+ Parent answers (full logit)',
  before_grade8='Full logit excluding grade-8 measures',tree_full='Decision tree: all predictors',
  tree_before_grade8='Decision tree excluding grade-8 measures')
num <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
fmt <- function(x,k=3) ifelse(is.na(x),'--',formatC(x,digits=k,format='f'))
main <- metrics[match(names(labels),model)]
classification <- main[model!='training_rate',.(Model=unname(labels[model]),`Accuracy (%)`=fmt(100*accuracy),
  `Applicants detected`=num(applicants_detected),`False positives`=num(false_positives),
  `Net errors avoided`=num(net_errors_avoided),`Applicant recall (%)`=fmt(100*recall,2),`Precision (%)`=fmt(100*precision,2))]
ranking <- main[model!='no_application',.(Model=unname(labels[model]),AUC=fmt(auc),
  `Average precision`=fmt(average_precision),Brier=fmt(brier,5),
  `Top 10%: application rate (%)`=fmt(100*top_decile_precision,1),
  `Top 10%: share of applicants (%)`=fmt(100*top_decile_recall,1),`Top 10%: lift`=fmt(top_decile_lift,2))]
ranking[Model==labels[['training_rate']],c('Top 10%: application rate (%)','Top 10%: share of applicants (%)','Top 10%: lift'):=list('--','--','--')]
full <- metrics[model=='full']; early <- metrics[model=='before_grade8']; rule <- metrics[model=='no_application']
pooled <- metrics[model=='pooled_full']; availability <- metrics[model=='availability']; tree_full <- metrics[model=='tree_full']
tree_early <- metrics[model=='tree_before_grade8']
key <- c(
  sprintf('We restrict the exercise to %s students whose grade-8 origin offers grade 9. %s applied through SAE (%.2f%%). The unchanged school split has %s training and %s held-out students.',
    num(sum(sample$students)),num(sum(sample$applicants)),100*sum(sample$applicants)/sum(sample$students),
    num(sum(sample[split=='train',students])),num(rule$n)),
  sprintf('Among held-out students, predicting no application for everyone is %.3f%% accurate. The full logit fitted specifically on this group is %.3f%% accurate, detecting %s applicants and falsely flagging %s nonapplicants: %s net errors avoided. The paired accuracy difference is %+.3f percentage points (school-clustered 95%% CI [%+.3f, %+.3f]).',
    100*rule$accuracy,100*full$accuracy,num(full$applicants_detected),num(full$false_positives),num(full$net_errors_avoided),
    full$gain_pp,full$gain_ci_low_pp,full$gain_ci_high_pp),
  sprintf('Ranking is more informative than a binary cutoff: the full voluntary logit has AUC %.3f, compared with %.3f for the earlier pooled full model. Its highest-risk 10%% have an application rate of %.1f%% and contain %.1f%% of all held-out applicants (%.2fx the overall rate).',
    full$auc,pooled$auc,100*full$top_decile_precision,100*full$top_decile_recall,full$top_decile_lift),
  sprintf('Excluding grade-8 annual grades, attendance and gaps gives AUC %.3f, accuracy %.3f%% and %s net errors avoided. Parent answers change Brier loss from %.5f to %.5f after holding response availability and school matching constant.',
    early$auc,100*early$accuracy,num(early$net_errors_avoided),availability$brier,full$brier),
  sprintf('The decision tree uses complexity selected within training schools. With all predictors it achieves AUC %.3f, accuracy %.3f%% and %s net errors avoided. All alternatives are shown; no model is chosen by its test accuracy.',
    tree_full$auc,100*tree_full$accuracy,num(tree_full$net_errors_avoided)),
  sprintf('The tree excluding grade-8 measures detects %s applicants and falsely flags %s nonapplicants: %s net errors avoided and %.3f%% accuracy. Its gain of %+.3f percentage points has a school-clustered 95%% interval of [%+.3f, %+.3f], so it is small and statistically uncertain.',
    num(tree_early$applicants_detected),num(tree_early$false_positives),num(tree_early$net_errors_avoided),100*tree_early$accuracy,
    tree_early$gain_pp,tree_early$gain_ci_low_pp,tree_early$gain_ci_high_pp)
)
by_cohort <- fread(file.path(vol_out,'performance_by_cohort.csv'))
cohort_table <- by_cohort[model %in% c('full','before_grade8','tree_full','tree_before_grade8'),.(Cohort=cohort_gr8,
  Model=unname(labels[model]),Students=num(n),`Application rate (%)`=fmt(100*applicants/n,2),AUC=fmt(auc),
  `Accuracy gain (pp)`=fmt(gain_pp),`Net errors avoided`=num(net_errors_avoided))]
plot_data <- cal[model %in% c('full','before_grade8','tree_full','tree_before_grade8')]
plot_data[,model_label:=factor(unname(labels[model]),levels=unname(labels[c('full','before_grade8','tree_full','tree_before_grade8')]))]
plot_data[,decile:=11L-bin]
long <- melt(plot_data,id.vars=c('model_label','decile'),measure.vars=c('observed','predicted'),variable.name='Series',value.name='Rate')
long[,Series:=ifelse(Series=='observed','Observed application rate','Mean predicted probability')]
chart <- ggplot(long,aes(decile,Rate,color=Series,linetype=Series))+
  geom_line(linewidth=.75)+geom_point(size=1.8)+
  geom_hline(yintercept=rule$applicants/rule$n,color='#8d99a6',linetype='dotted')+
  facet_wrap(~model_label,ncol=2)+scale_x_continuous(breaks=c(1,5,10))+
  scale_y_continuous(labels=function(x) paste0(round(100*x),'%'))+
  scale_color_manual(values=c('Observed application rate'='#176b87','Mean predicted probability'='#cd7629'))+
  labs(title='Predicting applications where grade 9 is offered',
    subtitle='Held-out origin schools; deciles rank pupils within each model',
    x='Predicted-risk decile (10 = highest)',y='Application rate / predicted probability',
    caption='Dotted line: overall held-out application rate. Full-year grade-8 measures make those models retrospective.',color=NULL,linetype=NULL)+
  theme_minimal(base_size=12)+theme(legend.position='bottom',panel.grid.minor=element_blank(),plot.caption=element_text(hjust=0,size=9))
ggsave(file.path(figure_dir,'voluntary_risk_deciles.png'),chart,width=11,height=7,dpi=160,bg='white')
notes <- c(
  'Voluntary denotes an application by a student whose current school offers grade 9 in SAE A1. It does not establish the reason for applying or an eventual school change. Unresolved and terminal origins are excluded from this exercise.',
  'The outcome is an explicit grade-9 application in either regular or complementary rounds in the first observed grade-8 year (2017-2020). The school split is unchanged from the earlier exercise. This follow-up uses an already-examined holdout, so it is exploratory and is not an independent confirmatory test.',
  'Each logit is re-estimated only within the voluntary group. Predictors and quadratic effects follow the earlier definitions. Context includes cohort-by-region, school sector, gender and age. Missingness indicators remain; medians and scaling are re-estimated from voluntary training pupils only. School continuation itself is constant and omitted.',
  'Classification uses a fixed 0.5 cutoff and equal error costs. Net errors avoided equals correctly detected applicants minus falsely flagged nonapplicants. A negative value is worse than predicting no application for everyone. Paired confidence intervals cluster at origin school and condition on the fitted models.',
  'The tree is CART regression on the binary outcome: leaf means are application probabilities and splits/pruning use squared error. Maximum depth is 8, minimum leaf size is 200, and there is no class reweighting or surrogate splitting. Complexity candidates are 0.01, 0.005, 0.001, 0.0005 and 0.0001. A 20% school split within original training chooses highest validation accuracy, then lowest Brier loss, then the simpler tree. The chosen complexity is refitted on all original training pupils. Inner imputation is learned only from inner training. Tree predictors omit redundant squared terms, which do not add information.',
  'AUC measures ranking. Average precision summarizes precision across recall levels with equal-score groups handled together. Top-decile counts select the 10% highest predicted probabilities; student ID breaks ties independently of outcomes. Lift divides their application rate by the overall held-out rate. Decile plots use nearly equal-sized rank groups.',
  'The all-nonapplicant classification benchmark and the training-rate probability benchmark have identical binary predictions. Brier loss and average precision use probabilities directly. Classification improvement is not interchangeable with better probability ranking.',
  'Full-year grade-8 predictors can postdate application. Models excluding them are useful sensitivities, but annual origin records still do not establish application-date enrollment. No geography, new data sources, actual-move outcome, or causal interpretation is introduced.',
  sprintf('Verification passed %s checks, including same-sample school splits, convergence, saved-model reproduction, independent confusion counts and independently computed AUC. Selected tree complexity and full coefficients are saved with the analysis.',num(nrow(checks)))
)
mdtable <- function(z) c(paste0('| ',paste(names(z),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
  apply(z,1,function(r) paste0('| ',paste(r,collapse=' | '),' |')))
writeLines(c('# Predicting the voluntary SAE application margin','',paste(key,collapse='\n\n'),
  '','## Beating the no-application rule','',mdtable(classification),
  '','## Probability prediction and identifying likely applicants','',mdtable(ranking),
  '','![Voluntary application risk deciles](../../figures/sae_participation/voluntary_risk_deciles.png)',
  '','## Results by cohort','',mdtable(cohort_table),
  '','## Definitions and validation','',paste0('- ',notes),
  '','[Continuation benchmark](continuation_benchmark.md) | [Original report](report.md)'),file.path(report_dir,'voluntary_margin.md'))
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
htmltable <- function(z) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(z)),'</th>',collapse=''),'</tr></thead><tbody>',
  paste(apply(z,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Voluntary SAE applications</title>',
  '<style>body{font:16px/1.6 system-ui,sans-serif;background:#f5f7fa;color:#243347;margin:0}main{max-width:1160px;margin:40px auto;padding:36px;background:white;border-radius:12px}h1{font-size:32px;line-height:1.2}h2{margin-top:34px;font-size:23px}.tag{font-size:12px;text-transform:uppercase;letter-spacing:.08em;color:#216b86;font-weight:700}table{width:100%;border-collapse:collapse;font-size:13px}th{text-align:left;background:#e9eff4}td,th{padding:10px;border-bottom:1px solid #dde3ea}td:not(:first-child){font-variant-numeric:tabular-nums}.table{overflow-x:auto}li{margin-bottom:12px}.notes{font-size:14px}img{max-width:100%;height:auto}</style></head><body><main>',
  '<div class="tag">October 5, 2026 · Schools offering grade 9 · Exploratory follow-up</div><h1>Predicting voluntary SAE applications</h1>',
  paste0('<p>',esc(key),'</p>',collapse=''),'<h2>Beating the no-application rule</h2>',htmltable(classification),
  '<h2>Probability prediction and identifying likely applicants</h2>',htmltable(ranking),
  '<img src="../../figures/sae_participation/voluntary_risk_deciles.png" alt="Observed and predicted application rates by risk decile">',
  '<h2>Results by cohort</h2>',htmltable(cohort_table),'<h2>Definitions and validation</h2><ul class="notes">',
  paste0('<li>',esc(notes),'</li>',collapse=''),'</ul><p><a href="continuation_benchmark.html">Continuation benchmark</a> · <a href="report.html">Original report</a></p></main></body></html>')
writeLines(html,file.path(report_dir,'voluntary_margin.html'))
cat(paste(key,collapse='\n'),'\n')
print(tree[selected==TRUE])
