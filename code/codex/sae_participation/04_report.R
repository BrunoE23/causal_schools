source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(ggplot2))
report_dir <- file.path(root,'output/reports/sae_participation')
figure_dir <- file.path(root,'output/figures/sae_participation')
dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
metrics <- fread(file.path(out,'predictive_performance.csv'))
ct <- fread(file.path(out,'coefficients.csv'))
cal <- fread(file.path(out,'calibration.csv'))
sample <- fread(file.path(out,'sample_summary.csv'))
cw <- fread('code/codex/sae_participation/parent_crosswalk.csv')
frame <- readRDS(file.path(out,'model_frame.rds'))
dd <- frame$data; ti <- frame$test
pred <- readRDS(file.path(out,'heldout_predictions.rds'))
stopifnot(identical(dd$mrun[ti],pred$mrun))
labels <- c(context_only='Demographics, sector, cohort and region',baseline='+ School continuation',
            admin_grades5_8='+ Grades and attendance, grades 5-8',plus_simce_gaps='+ Grades minus baseline SIMCE z-scores',
            plus_parent_availability='+ Parent response and school-match indicators',plus_parent_answers='+ Parent answers',
            grades5_7_parent_answers='Full model excluding grade 8 annual measures')
overall <- metrics[group_type=='overall'][match(names(labels),model)]
overall[,label:=unname(labels[model])]
overall[,brier_improvement_previous:=shift(brier)-brier]
overall[model=='grades5_7_parent_answers',brier_improvement_previous:=NA_real_]
audit_write(overall,'report_model_comparison')

# Fixed model; permute only observed answers within survey year, school group,
# and whether the grade-4 and grade-8 schools match. Missingness stays unchanged.
# This is a held-out model-reliance diagnostic, not causal or unique importance.
set.seed(20261006)
test <- dd[ti]
y <- test$applied_sae
p <- pred$plus_parent_answers
eta <- qlogis(pmin(pmax(p,1e-12),1-1e-12))
loss <- function(p) { p<-pmin(pmax(p,1e-12),1-1e-12); -mean(y*log(p)+(1-y)*log1p(-p)) }
base_loss <- loss(p)
importance <- list()
for(feature in paste0('parent_',cw$feature)) {
  target_term <- paste0(feature,'__z')
  b <- ct[model=='plus_parent_answers' & term==target_term]
  if(nrow(b)!=1L) next
  observed <- which(!is.na(test[[feature]]))
  groups <- split(observed,interaction(test$simce_year[observed],test$continuation_group[observed],
                   ifelse(is.na(test$same_simce_origin[observed]),-1,test$same_simce_origin[observed]),drop=TRUE))
  x <- test[[target_term]]; changes <- numeric(10L)
  for(k in seq_len(10L)) {
    xp <- x
    for(ix in groups) if(length(ix)>1L) xp[ix] <- x[ix[sample.int(length(ix))]]
    changes[k] <- loss(plogis(eta+b$estimate*(xp-x)))-base_loss
  }
  importance[[feature]] <- data.table(feature=feature,label=cw$label[match(sub('^parent_','',feature),cw$feature)],
    observed_test=length(observed),coefficient_log_odds_per_train_sd=b$estimate,se_cluster_school=b$se_cluster_school,p_value=b$p_value,
    mean_logloss_increase=mean(changes),permutation_sd=sd(changes))
}
imp <- rbindlist(importance)
imp[,p_bh:=p.adjust(p_value,method='BH')]
setorder(imp,-mean_logloss_increase)
audit_write(imp,'parent_predictive_importance')

theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face='bold'),plot.margin=margin(15,18,15,15)))
main <- overall[model!='grades5_7_parent_answers']
main[,label:=factor(label,levels=rev(main$label))]
g1 <- ggplot(main,aes(auc,label))+geom_segment(aes(x=.5,xend=auc,yend=label),color='#c3cbd6',linewidth=1.2)+
  geom_point(color='#216b86',size=3.4)+geom_text(aes(label=sprintf('%.3f',auc)),hjust=-.3,size=3.8)+
  scale_x_continuous(limits=c(.5,1),breaks=seq(.5,1,.1))+labs(title='How much does each predictor block add?',
  subtitle='Same held-out schools and students in every model',x='AUC (0.5 = chance ranking; 1 = perfect ranking)',y=NULL)
ggsave(file.path(figure_dir,'predictive_performance.png'),g1,width=10,height=4.7,dpi=180,bg='white')
cal_plot <- cal[model %in% c('baseline','plus_simce_gaps','plus_parent_answers')]
cal_plot[,model:=factor(model,levels=c('baseline','plus_simce_gaps','plus_parent_answers'),labels=c('School continuation baseline','Plus administrative history and SIMCE','Plus parent answers'))]
g2 <- ggplot(cal_plot,aes(predicted,observed,color=model))+geom_abline(slope=1,intercept=0,color='grey70',linetype='dashed')+
  geom_line(linewidth=.7)+geom_point(size=2)+coord_equal(xlim=c(0,1),ylim=c(0,1))+
  scale_color_manual(values=c('#8a6574','#b8772e','#216b86'))+
  labs(title='Calibration on held-out schools',subtitle='Each point is a bin of predicted application probabilities',x='Mean predicted probability',y='Observed application rate',color=NULL)+theme(legend.position='bottom')
ggsave(file.path(figure_dir,'calibration.png'),g2,width=8,height=6.5,dpi=180,bg='white')
by_school <- sample[analysis_eligible==1,.(students=sum(students),applicants=sum(applicants)),by=continuation_group]
by_school[,rate:=applicants/students]
top <- head(imp,12)

fmt <- function(x,d=3) ifelse(is.na(x),'',formatC(x,format='f',digits=d))
num <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
comparison <- data.table(Model=overall$label,AUC=fmt(overall$auc,5),Brier=fmt(overall$brier,6),
  `Brier R2`=fmt(overall$brier_r2,4),`Log loss`=fmt(overall$log_loss,6))
rates <- data.table(`School continuation`=by_school$continuation_group,Students=num(by_school$students),`Application rate`=paste0(fmt(100*by_school$rate,1),'%'))
parent_table <- data.table(Question=top$label,`Observed test answers`=num(top$observed_test),
  `Log-odds / SD`=fmt(top$coefficient_log_odds_per_train_sd),`Clustered SE`=fmt(top$se_cluster_school),
  `Held-out log-loss increase x1000`=fmt(top$mean_logloss_increase*1000,3))
bygroup <- metrics[group_type=='school' & model %in% c('baseline','plus_simce_gaps','plus_parent_answers')]
group_table <- data.table(Group=bygroup$group,Model=unname(labels[bygroup$model]),N=num(bygroup$n),AUC=fmt(bygroup$auc),Brier=fmt(bygroup$brier,4))
bycohort <- metrics[group_type=='cohort' & model=='plus_parent_answers'][order(group)]
cohort_table <- data.table(Cohort=bycohort$group,N=num(bycohort$n),AUC=fmt(bycohort$auc),Brier=fmt(bycohort$brier,4),
  `Observed rate`=fmt(bycohort$prevalence),`Predicted rate`=fmt(bycohort$predicted_mean))
parent_groups <- dcast(metrics[group_type %in% c('cohort','same_school') & model %in% c('plus_parent_availability','plus_parent_answers')],
                       group_type+group~model,value.var='brier')
parent_groups[,brier_reduction:=plus_parent_availability-plus_parent_answers]
audit_write(parent_groups,'parent_increment_by_group')
parent_group_table <- data.table(Group=ifelse(parent_groups$group_type=='cohort',paste('Cohort',parent_groups$group),
  ifelse(parent_groups$group=='1','Same grade-4 and grade-8 school','Different grade-4 and grade-8 school')),
  `Brier without answer content`=fmt(parent_groups$plus_parent_availability,6),
  `Brier with answer content`=fmt(parent_groups$plus_parent_answers,6),
  `Reduction x1000`=fmt(parent_groups$brier_reduction*1000,3))
baseline_brier <- overall[model=='baseline',brier]
full_brier <- overall[model=='plus_parent_answers',brier]
availability_brier <- overall[model=='plus_parent_availability',brier]
key <- c(
  sprintf('The analysis includes %s students in the 2017-2020 grade-8 cohorts within SAE-covered origin regions. The fixed school split assigns %s students to training and %s to testing.',num(nrow(dd)),num(length(frame$train)),num(length(ti))),
  sprintf('Adding grades, attendance, SIMCE comparisons and parent information to school continuation reduces held-out Brier error by %.1f%% (%.4f to %.4f).',100*(1-full_brier/baseline_brier),baseline_brier,full_brier),
  sprintf('Parent answer content changes Brier error from %.4f to %.4f, after holding questionnaire response/matching information constant. This is a %.2f%% relative reduction.',availability_brier,full_brier,100*(1-full_brier/availability_brier)),
  sprintf('Excluding grade-8 annual grades, attendance and SIMCE gaps gives AUC %.3f and Brier %.4f.',overall[model=='grades5_7_parent_answers',auc],overall[model=='grades5_7_parent_answers',brier])
)
notes <- c(
  'Primary outcome: at least one explicit grade-9 application in regular or complementary SAE rounds in the grade-8 cohort year. Automatically appended continuity rows do not establish a choice. Off-platform/regularization activity is not measured.',
  'School continuation uses that process\'s A1 offerings. An observed grade-8 origin with no secondary offering is the ends-at-8 group. Early rollout menus omit some nonentry grades. Schools absent from A1 are unresolved, not assumed terminal. The 2018 Nuble boundary change is harmonized for coverage.',
  'The main exercise is retrospective: annual grade-8 performance and attendance can postdate application. The model excluding those measures still uses annual origin-school records, so it is not a validated application-date forecast.',
  'Grades, attendance and their peer-relative z-scores enter separately for grades 5-8, with squares. Math and language gaps equal grade z-score minus the baseline SIMCE z-score among the same later schoolmates. These are relative positions, not learning gains. SIMCE first uses national test-year normalization. Undefined peer SDs remain missing.',
  'All model comparisons use identical pupils and a fixed 80/20 split by grade-8 origin school across cohorts. Median imputation and predictor scaling use training data only. Context controls are cohort-by-region, sector, gender and age. Schools with missing continuation information remain in the model.',
  'The 39 parent questions/constructs were chosen from the source questionnaires before estimating outcome associations. They describe the grade-4 school. Wording and availability vary by year; exact codes and wording are saved in the parent dictionary. Higher valid ratings are favorable; the 2013 agreement scale and 2016 mistreatment response are reversed.',
  'The parent table ranks model reliance: average increase in test log loss when one observed answer is shuffled within survey-year, continuation and school-match strata (10 repetitions). This is exploratory held-out permutation importance, not a unique contribution, causal effect, or significance test. Correlated questions may substitute for one another.',
  'Parent coefficients are jointly conditional log-odds per training-sample SD, with model-derived origin-school-clustered SEs. Full coefficients and BH-adjusted parent p-values are saved. No causal interpretation is assigned.',
  'All student-school histories are retained. The model selects one annual record using the project attendance/promotion hierarchy and the latest observed year for each grade. Within-year movers may have a different school at application. Missing SIMCE/questionnaire information does not exclude students. Zero attendance is retained as a valid percentage.',
  'Brier R2 compares squared probability error to predicting the training prevalence for everyone. Subgroup AUC is particularly important because the school-continuation distinction strongly predicts overall participation. Small Other SAE offering groups have unstable metrics.',
  'This is a first fixed-specification result on one held-out-school split; it does not establish stability across alternative splits, future cohorts, or functional forms. No distance/comuna analysis or separate realized-school-change model is included.'
)
md_table <- function(d) c(paste0('| ',paste(names(d),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(d)),collapse=' | '),' |'),apply(d,1,function(r) paste0('| ',paste(r,collapse=' | '),' |')))
md <- c('# Predicting grade-8 SAE participation','',
  'Primary classification benchmark: [Can we beat the continuation rule?](continuation_benchmark.md). This compares accuracy and mistakes corrected against predicting no application wherever grade 9 is offered.','',
  'Follow-up: [Predicting voluntary applications](voluntary_margin.md) refits the models among students whose schools offer grade 9.','',
  paste0(key,collapse='\n\n'),'',
  '## Application rates by school continuation','',md_table(rates),'','## Held-out prediction','',md_table(comparison),'',
  '![Predictive performance](../../figures/sae_participation/predictive_performance.png)','',
  '## Performance within school groups','',md_table(group_table),'','## Full model by cohort','',md_table(cohort_table),'',
  '## Increment from parent answer content by group','',md_table(parent_group_table),'',
  '## Parent questions with greatest held-out model reliance','',md_table(parent_table),'',
  '## Interpretation and construction','',paste0('- ',notes),'',
  'Reproducible code: `code/codex/sae_participation/`. Data and diagnostics: `data/clean/sae_participation/`.')
writeLines(md,file.path(report_dir,'report.md'),useBytes=TRUE)
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
html_table <- function(d) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(d)),'</th>',collapse=''),'</tr></thead><tbody>',
  paste(apply(d,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Predicting SAE participation</title>',
 '<style>body{font:16px/1.6 system-ui,sans-serif;color:#243347;background:#f5f7fa;margin:0}main{max-width:1100px;margin:40px auto;padding:36px;background:white;border-radius:12px}h1{font-size:34px;line-height:1.2}h2{margin-top:40px;font-size:23px}.tag{color:#216b86;letter-spacing:.08em;font-size:12px;font-weight:700;text-transform:uppercase}table{border-collapse:collapse;width:100%;font-size:14px}th{text-align:left;background:#e9eff4}td,th{padding:10px;border-bottom:1px solid #dde3ea}td:not(:first-child){font-variant-numeric:tabular-nums}.table{overflow-x:auto}img{max-width:100%;height:auto}.notes{font-size:14px;color:#4c5a6b}li{margin-bottom:12px}.lede{font-size:18px}footer{margin-top:35px;font-size:13px;color:#596678}</style></head><body><main>',
 '<div class="tag">Research note · October 5, 2026 · Retrospective prediction</div><h1>Who applies through SAE in grade 8?</h1>',
 '<p><strong>Primary classification benchmark:</strong> <a href="continuation_benchmark.html">Can we beat the continuation rule?</a> This compares accuracy and mistakes corrected against predicting no application wherever grade 9 is offered.</p>',
 '<p><strong>Follow-up:</strong> <a href="voluntary_margin.html">Predicting voluntary applications</a> refits the models among students whose schools offer grade 9.</p>',
 paste0('<p class="lede">',esc(key),'</p>',collapse=''),'<h2>Application rates by school continuation</h2>',html_table(rates),
 '<h2>What each predictor block adds</h2>',html_table(comparison),'<img src="../../figures/sae_participation/predictive_performance.png" alt="Held-out AUC by model">',
 '<h2>Prediction within school groups</h2>',html_table(group_table),'<h2>Full model by cohort</h2>',html_table(cohort_table),
 '<h2>Parent answer content by group</h2><p>Positive reductions indicate improved prediction; negative reductions indicate worse prediction on this holdout.</p>',html_table(parent_group_table),
 '<img src="../../figures/sae_participation/calibration.png" alt="Calibration of predictions">',
 '<h2>Which parent answers contribute to prediction?</h2><p>Exploratory ranking of the fitted model\'s reliance on observed answers. Positive coefficients associate more favorable answers with higher application odds, conditional on the other predictors.</p>',html_table(parent_table),
 '<h2>Interpretation and construction</h2><ul class="notes">',paste0('<li>',esc(notes),'</li>',collapse=''),'</ul>',
 '<footer>Reproducible code: code/codex/sae_participation/<br>Derived data and full diagnostics: data/clean/sae_participation/</footer></main></body></html>')
writeLines(html,file.path(report_dir,'report.html'),useBytes=TRUE)
cat(paste(key,collapse='\n'),'\n')
print(overall[,.(model,auc,brier,brier_r2,log_loss)])
print(head(imp,12))
