# Evaluate Bruno's deterministic continuation rule on the existing heldout split.
# No re-estimation, threshold tuning, or model selection using test outcomes.
source('code/codex/sae_participation/common.R')
suppressPackageStartupMessages(library(fixest))
pred <- readRDS(file.path(out,'heldout_predictions.rds'))
known_groups <- c('Ends at 8 in SAE offerings','Offers grade 9')
excluded <- pred[!continuation_group %in% known_groups,
                 .(students=.N,applicants=sum(applied_sae)),by=continuation_group]
d <- copy(pred[continuation_group %in% known_groups])
d[,rule:=as.integer(continuation_group=='Ends at 8 in SAE offerings')]
split_counts <- fread(file.path(out,'split_summary.csv'))
applicant_composition <- split_counts[,.(students=sum(students),applicants=sum(applicants)),by=continuation_group]
applicant_composition[,share_all_applicants:=applicants/sum(applicants)]
applicant_composition[,share_known_applicants:=NA_real_]
applicant_composition[continuation_group %in% known_groups,share_known_applicants:=applicants/sum(applicants)]
setorder(applicant_composition,-applicants)
audit_write(applicant_composition,'applicant_composition_by_continuation')
train <- split_counts[split=='train' & continuation_group %in% known_groups,
                  .(train_students=sum(students),train_applicants=sum(applicants)),by=continuation_group]
train[,group_probability:=train_applicants/train_students]
d[train,on='continuation_group',group_probability:=i.group_probability]
stopifnot(!anyDuplicated(d$mrun),all(is.finite(d$group_probability)))
labels <- c(rule='Your continuation rule',baseline='Existing continuation + context logit',
  admin_grades5_8='+ Grades and attendance, grades 5-8',plus_simce_gaps='+ SIMCE-relative achievement gaps',
  plus_parent_availability='+ Parent availability and school match',plus_parent_answers='+ Parent answers (full model)',
  grades5_7_parent_answers='Full model excluding grade-8 annual measures')
safe_div <- function(a,b) if(b>0) a/b else NA_real_
summarize_classification <- function(z,m) {
  y <- z$applied_sae; b <- z$rule; h <- as.integer(z[[m]]>=.5)
  correct <- h==y; base_correct <- b==y
  fixed <- sum(!base_correct & correct); introduced <- sum(base_correct & !correct)
  delta <- as.integer(correct)-as.integer(base_correct)
  low <- high <- se <- 0
  if(any(delta!=0)) {
    # Paired student-level difference, actual intercept-only regression with
    # standard errors clustered by the heldout origin school.
    fit <- feols(delta~1,data=data.frame(delta,origin_rbd=z$origin_rbd),vcov=~origin_rbd,notes=FALSE)
    ci <- confint(fit,level=.95)
    se <- sqrt(vcov(fit)[1,1]); low <- ci[1,1]; high <- ci[1,2]
    stopifnot(abs(unname(coef(fit)[1])-mean(delta))<1e-12)
  }
  tp <- sum(h==1 & y==1); tn <- sum(h==0 & y==0)
  fp <- sum(h==1 & y==0); fn <- sum(h==0 & y==1)
  stopifnot(fixed-introduced==sum(correct)-sum(base_correct),tp+tn+fp+fn==nrow(z))
  data.table(model=m,students=nrow(z),schools=uniqueN(z$origin_rbd),accuracy=mean(correct),errors=sum(!correct),
    errors_fixed=fixed,errors_introduced=introduced,net_errors_avoided=fixed-introduced,
    accuracy_gain_pp=100*mean(delta),gain_se_pp=100*se,gain_ci_low_pp=100*low,gain_ci_high_pp=100*high,
    relative_error_reduction=safe_div(fixed-introduced,sum(!base_correct)),
    tp=tp,tn=tn,fp=fp,fn=fn,applicant_recall=safe_div(tp,tp+fn),applicant_precision=safe_div(tp,tp+fp),
    predicted_exceptions=sum(h!=b),true_exceptions=sum(y!=b),exception_recall=safe_div(fixed,sum(y!=b)),
    exception_precision=safe_div(fixed,sum(h!=b)))
}
results <- rbindlist(lapply(names(labels),function(m) {
  rbindlist(list(summarize_classification(d,m)[,group:='All known continuation'],
    rbindlist(lapply(known_groups,function(g) summarize_classification(d[continuation_group==g],m)[,group:=g]))))
}))
prob_models <- c('rule','group_probability',setdiff(names(labels),'rule'))
prob_results <- rbindlist(lapply(prob_models,function(m) {
  p <- d[[m]]; y <- d$applied_sae
  # No clipping to pretend the hard rule has finite log loss when it errs.
  logloss <- if(any((p==0 & y==1)|(p==1 & y==0))) Inf else -mean(ifelse(y==1,log(p),log1p(-p)))
  data.table(model=m,students=nrow(d),brier=mean((y-p)^2),log_loss=logloss,
    brier_skill_vs_hard_rule=1-mean((y-p)^2)/mean((y-d$rule)^2),
    brier_skill_vs_group_probability=1-mean((y-p)^2)/mean((y-d$group_probability)^2))
}))
audit_write(results,'continuation_benchmark_classification')
audit_write(prob_results,'continuation_benchmark_probability')
audit_write(excluded,'continuation_benchmark_exclusions')
audit_write(train,'continuation_benchmark_training_rates')
audit_write(d[,.(mrun,origin_rbd,cohort_gr8,continuation_group,applied_sae,rule,
  full_prediction=as.integer(plus_parent_answers>=.5),full_probability=plus_parent_answers)],'continuation_benchmark_predictions')

# Independent confusion-table and paired-count checks on every comparison.
checks <- rbindlist(lapply(names(labels),function(m) {
  h <- as.integer(d[[m]]>=.5); tab <- table(factor(d$applied_sae,levels=0:1),factor(h,levels=0:1))
  r <- results[model==m & group=='All known continuation']
  data.table(model=m,passed=abs(sum(diag(tab))/sum(tab)-r$accuracy)<1e-12 &&
    r$net_errors_avoided==sum(diag(tab))-sum(d$rule==d$applied_sae) &&
    r$errors_fixed+r$errors_introduced==sum(h!=d$rule) &&
    all(c(r$tn,r$fp,r$fn,r$tp)==c(tab[1,1],tab[1,2],tab[2,1],tab[2,2])))
}))
checks <- rbind(checks,data.table(model='hard_rule_brier_equals_error_rate',passed=
  abs(prob_results[model=='rule',brier]-(1-results[model=='rule' & group=='All known continuation',accuracy]))<1e-12))
stopifnot(all(checks$passed))
audit_write(checks,'continuation_benchmark_verification')

report_dir <- file.path(root,'output/reports/sae_participation')
dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
num <- function(x) format(x,big.mark=',',scientific=FALSE,trim=TRUE)
fmt <- function(x,k=3) formatC(x,digits=k,format='f')
main <- results[group=='All known continuation']
table_main <- data.table(Model=unname(labels[main$model]),`Accuracy (%)`=fmt(main$accuracy*100),
  Errors=num(main$errors),`Fixed versus your rule`=num(main$errors_fixed),`Introduced`=num(main$errors_introduced),
  `Net avoided`=num(main$net_errors_avoided),`Gain (pp)`=fmt(main$accuracy_gain_pp))
sub <- results[model=='plus_parent_answers' & group!='All known continuation']
base <- results[model=='rule' & group!='All known continuation']
table_groups <- data.table(Group=sub$group,Students=num(sub$students),
  `Your rule (%)`=fmt(100*base$accuracy[match(sub$group,base$group)]),`Full model (%)`=fmt(100*sub$accuracy),
  `Fixed`=num(sub$errors_fixed),`Introduced`=num(sub$errors_introduced),`Net avoided`=num(sub$net_errors_avoided),
  `Exceptions detected (%)`=fmt(100*sub$exception_recall,2))
table_applicants <- data.table(Group=applicant_composition$continuation_group,
  Applicants=num(applicant_composition$applicants),
  `Share of all applicants (%)`=fmt(100*applicant_composition$share_all_applicants,2),
  `Share within resolved groups (%)`=ifelse(is.na(applicant_composition$share_known_applicants),'--',
    fmt(100*applicant_composition$share_known_applicants,2)))
composition_note <- sprintf('Across the full training and held-out analysis sample, %s students applied for grade 9 in the 2017-2020 grade-8 cohorts. %.1f%% come from schools ending at grade 8, %.1f%% from schools offering grade 9 ("voluntary" applications), and %.1f%% from unresolved/other offerings. Restricting to the two resolved groups gives %.1f%% school-ending and %.1f%% voluntary. These are unique students with explicit regular or complementary applications, not confirmed school changes; the figures cover the defined grade-8 cohort sample in SAE-covered origin regions, not every raw SAE grade-9 applicant.',
  num(sum(applicant_composition$applicants)),
  100*applicant_composition[continuation_group==known_groups[1],share_all_applicants],
  100*applicant_composition[continuation_group==known_groups[2],share_all_applicants],
  100*applicant_composition[!continuation_group %in% known_groups,sum(share_all_applicants)],
  100*applicant_composition[continuation_group==known_groups[1],share_known_applicants],
  100*applicant_composition[continuation_group==known_groups[2],share_known_applicants])
fr <- main[model=='plus_parent_answers']; rr <- main[model=='rule']; nr <- main[model=='grades5_7_parent_answers']
stay <- sub[group=='Offers grade 9']; terminal <- sub[group=='Ends at 8 in SAE offerings']
key <- c(
  sprintf('Your rule predicts no SAE application where grade 9 is offered, and an application where the school ends at grade 8 in SAE offerings. It correctly classifies %s of %s held-out students (%.3f%%).',num(rr$students-rr$errors),num(rr$students),100*rr$accuracy),
  sprintf('At a fixed probability threshold of 0.5, the full model reaches %.3f%% accuracy: +%.3f percentage points. It fixes %s of the rule\'s mistakes and introduces %s new mistakes, avoiding %s errors net (%.2f%% of the rule\'s errors).',100*fr$accuracy,fr$accuracy_gain_pp,num(fr$errors_fixed),num(fr$errors_introduced),num(fr$net_errors_avoided),100*fr$relative_error_reduction),
  sprintf('Of the %s net corrections, %s are at schools ending at grade 8 and %s at schools offering grade 9. The model barely improves binary classification of applications where staying is possible.',num(fr$net_errors_avoided),num(terminal$net_errors_avoided),num(stay$net_errors_avoided)),
  sprintf('Without grade-8 annual grades, attendance and SIMCE gaps, accuracy is %.3f%%: only %s net errors avoided. The retrospective grade-8 information supplies almost all of the classification gain.',100*nr$accuracy,num(nr$net_errors_avoided))
)
notes <- c(
  sprintf('The comparison uses the same %s held-out students with either confirmed grade-9 SAE offerings or an observed grade-8 origin with no secondary SAE offering. It excludes %s students with unresolved offerings and %s in the small Other SAE offering group. These pupils cannot be assigned your rule unambiguously from the present classification.',num(nrow(d)),num(sum(excluded[continuation_group=='Not resolved in SAE offerings']$students)),num(sum(excluded[continuation_group=='Other SAE offering']$students))),
  'The target remains SAE application, not actual subsequent school continuation. A nonapplication is not proof that a student stays, and an application is not proof that a student moves.',
  'All learned models and predictions are unchanged. Classification uses a prespecified 0.5 cutoff and equal costs for false positives and false negatives. No cutoff or model was chosen to maximize accuracy on this already-inspected holdout.',
  sprintf('The paired full-model accuracy gain has an origin-school-clustered 95%% confidence interval of [%.3f, %.3f] percentage points, conditional on the fixed trained models. This comes from an intercept-only regression of each pupil\'s change in correctness. It does not include training-sample or model-selection uncertainty.',fr$gain_ci_low_pp,fr$gain_ci_high_pp),
  'For probability quality, a fairer school-only comparator assigns each continuation group its training-set application rate. It makes the same binary predictions as your rule at 0.5, but avoids treating application probabilities as literally zero or one. Probability-score comparisons are saved separately; the hard rule has infinite log loss when it makes a mistake.',
  'The previous overall AUC mostly rewarded separating schools with and without continuation. It did not establish accurate identification of exceptions to this rule. The primary comparison here is held-out accuracy and net mistakes avoided, with the two school groups reported separately.',
  'This is still retrospective when grade-8 annual measures are included. Geography remains deferred. Any new model or threshold development should use training/validation data; further confirmatory claims will need a fresh holdout because the current test results have now been examined.'
)
mdtable <- function(z) c(paste0('| ',paste(names(z),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
  apply(z,1,function(r) paste0('| ',paste(r,collapse=' | '),' |')))
writeLines(c('# Beating the continuation rule','',paste(key,collapse='\n\n'),'','## Same-student comparison','',mdtable(table_main),
  '','## Where the full model helps','',mdtable(table_groups),
  '','## Where applicants come from (full analysis sample)','',composition_note,'',mdtable(table_applicants),
  '','## Definitions and interpretation','',paste0('- ',notes)),
  file.path(report_dir,'continuation_benchmark.md'))
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
htmltable <- function(z) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(z)),'</th>',collapse=''),'</tr></thead><tbody>',
  paste(apply(z,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Beating the continuation rule</title>',
  '<style>body{font:16px/1.6 system-ui,sans-serif;background:#f5f7fa;color:#243347;margin:0}main{max-width:1120px;margin:40px auto;padding:36px;background:white;border-radius:12px}h1{font-size:32px;line-height:1.2}h2{margin-top:34px;font-size:23px}.tag{font-size:12px;text-transform:uppercase;letter-spacing:.08em;color:#216b86;font-weight:700}table{width:100%;border-collapse:collapse;font-size:13px}th{text-align:left;background:#e9eff4}td,th{padding:10px;border-bottom:1px solid #dde3ea}td:not(:first-child){font-variant-numeric:tabular-nums}.table{overflow-x:auto}li{margin-bottom:12px}.notes{font-size:14px}</style></head><body><main>',
  '<div class="tag">October 5, 2026 · Same held-out schools · Fixed 0.5 cutoff</div><h1>Can we beat the continuation rule?</h1>',
  paste0('<p>',esc(key),'</p>',collapse=''),'<h2>Same-student comparison</h2>',htmltable(table_main),
  '<h2>Where the full model helps</h2>',htmltable(table_groups),
  '<h2>Where applicants come from (full analysis sample)</h2><p>',esc(composition_note),'</p>',htmltable(table_applicants),
  '<h2>Definitions and interpretation</h2><ul class="notes">',
  paste0('<li>',esc(notes),'</li>',collapse=''),'</ul><p><a href="report.html">Original probability-prediction report</a></p></main></body></html>')
writeLines(html,file.path(report_dir,'continuation_benchmark.html'))
cat(paste(key,collapse='\n'),'\n'); print(table_groups); print(fr[,.(accuracy_gain_pp,gain_ci_low_pp,gain_ci_high_pp)])
