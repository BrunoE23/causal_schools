suppressPackageStartupMessages(library(data.table))
root <- normalizePath('.',winslash='/')
pair <- 'va_2017_2020__sae_2018_2020'
dest <- file.path(root,'data/clean/empirical_bayes_school_va',pair,'by_continuation')
table_dir <- file.path(root,'output/tables/empirical_bayes_school_va',pair)
report_dir <- file.path(root,'output/reports/empirical_bayes_school_va')
dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
r <- fread(file.path(dest,'results.csv'))
rates <- fread(file.path(dest,'assignment_risk_rates.csv'))
ev <- fread(file.path(dest,'expected_va_by_group.csv'))
account <- fread(file.path(dest,'sample_accounting.csv'))
checks <- fread(file.path(dest,'verification.csv'))
replication <- fread(file.path(dest,'paper_replication.csv'))
ev_checks <- fread(file.path(dest,'expected_va_verification.csv'))
stopifnot(all(checks$passed),all(replication$passed),all(ev_checks$passed))
groups <- c('school_ending','voluntary')
group_labels <- c(school_ending='School ending',voluntary='Voluntary')
specs <- unique(r[,.(spec,label)])
fmt <- function(x,digits=3L) sprintf(paste0('%.',digits,'f'),x)
num <- function(x) format(x,big.mark=',',trim=TRUE,scientific=FALSE)
stars <- function(p) ifelse(p<.01,'***',ifelse(p<.05,'**',ifelse(p<.10,'*','')))
pval <- function(p) ifelse(p<.001,'<0.001',fmt(p))
result_table <- function(kind) rbindlist(lapply(groups,function(g) {
  z <- r[sample==g][match(specs$spec,spec)]
  values <- switch(kind,
    first_stage=paste0(fmt(z$fs_beta),stars(z$fs_p_value),' (',fmt(z$fs_se),')'),
    pass_through=paste0(fmt(z$beta),stars(z$p_value_equal_zero),' (',fmt(z$se),')'),
    N=num(z$n_obs),F=fmt(z$fs_f,1),p0=pval(z$p_value_equal_zero),p1=pval(z$p_value_equal_one))
  cbind(data.table(Group=unname(group_labels[g])),as.data.table(setNames(as.list(values),specs$label)))
}))
expected_table <- function(scope_name,coverage=FALSE) rbindlist(lapply(groups,function(g) {
  z <- ev[sample==g & scope==scope_name][match(specs$spec,spec)]
  stopifnot(nrow(z)==7L,!anyNA(z$spec))
  values <- if(coverage) paste0(fmt(100*z$mean_covered_mass,1),'%') else fmt(z$display_mean)
  cbind(data.table(Group=unname(group_labels[g])),as.data.table(setNames(as.list(values),specs$label)))
}))
fs <- result_table('first_stage'); ps <- result_table('pass_through')
ns <- result_table('N'); fstats <- result_table('F')
expected_all <- expected_table('All applicants'); expected_iv <- expected_table('IV estimation sample')
covered <- expected_table('All applicants',TRUE)
risk_table <- rates[match(groups,sample),.(Group=unname(group_labels[sample]),
  `All applicants`=num(applicants),`With assignment risk`=num(applicants_at_risk),
  `Fraction with risk`=paste0(fmt(100*risk_share,2),'%'),`Probability coverage`=paste0(fmt(100*probability_observed/applicants,1),'%'))]
tables <- list(assignment_risk=risk_table,first_stage=fs,pass_through=ps,N=ns,first_stage_F=fstats,
  expected_all_applicants=expected_all,expected_iv_sample=expected_iv,covered_probability=covered,
  p_psi_zero=result_table('p0'),p_psi_one=result_table('p1'))
for(nm in names(tables)) fwrite(tables[[nm]],file.path(dest,paste0('table_',nm,'.csv')))
unknown <- rates[sample=='unresolved_other']
notes <- c(
  'First-stage and pass-through specifications are the paper\'s Tables 4 and 5, estimated separately within each origin-school group. Both use timely applicants with assignment risk in 2018-2020; school VA is estimated on the original 2017-2020 sample and is held fixed across groups.',
  'The first stage regresses attended-school EB VA on first-round offered-school EB VA. The pass-through regression instruments attended VA with offered VA. Both control for expected VA in that outcome, cohort indicators, grade-4 math and verbal scores, gender indicators and age indicators. Outcome-specific missingness and exam-taking restrictions match the main paper. No-offer instrument values and all VA normalizations are unchanged.',
  'Parentheses contain heteroskedasticity-robust standard errors, as in the main paper. Stars use the original normal-reference tests: *** p<0.01, ** p<0.05, * p<0.10. First-stage F is the squared robust t statistic for the single excluded instrument. These subgroup point estimates alone do not establish a statistically significant difference between groups.',
  'Assignment-risk fractions use all applicants in each Table 2 subgroup before complete-control and outcome restrictions. School ending means the grade-8 origin has no SAE offering above grade 8; voluntary means it offers grade 9. These operational labels describe continuation availability, not observed motives or actual moves.',
  sprintf('%s applicants have unresolved/other origin offerings, including %s with assignment risk. They are excluded from both subgroup rows. The original pooled results include them when eligible.',num(unknown$applicants),num(unknown$applicants_at_risk)),
  'Expected VA is the mean of sum_j p_ij times the original EB VA_j, without recentering within either group. The main expected-VA table uses all subgroup applicants; the sensitivity uses the exact outcome-specific IV estimation samples. Math and verbal expected VA use outcome-SD units. Binary-outcome expected VA is multiplied by 100 and expressed in percentage points. Pass-through and first-stage coefficients are not rescaled.',
  'Expected VA reflects portfolios, priorities and assignment probabilities together; it is not a pure measure of ranked preferences. As in the main paper, unmatched outcomes and school-options without measured VA contribute zero, and probability mass is not renormalized. The coverage table shows the mean assignment probability on schools with measured VA, so differences in coverage can be assessed.',
  sprintf('Verification: all %s original pooled coefficient/SE/F/N statistics reproduce the paper; all seven reconstructed expected-VA measures match the cached main inputs for 162,633 risk applicants. Subgroup counts exactly partition each original outcome-specific estimation sample, allowing for unresolved/other origins. Independent matrix calculations reproduce 2SLS and first-stage robust SEs for math in both groups.',nrow(replication)))
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
htmltable <- function(z) paste0('<div class="table"><table><thead><tr>',paste0('<th>',esc(names(z)),'</th>',collapse=''),
  '</tr></thead><tbody>',paste(apply(z,1,function(v) paste0('<tr>',paste0('<td>',esc(v),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
section <- function(title,z,note='') paste0('<h2>',title,'</h2><p>',esc(note),'</p>',htmltable(z))
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Main results by school continuation</title>',
  '<style>body{font:15px/1.5 system-ui,sans-serif;color:#243347;background:#f5f7fa;margin:0}main{max-width:1400px;margin:28px auto;background:white;padding:32px;border-radius:10px}h1{font-size:29px}h2{font-size:21px;margin-top:30px}.table{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:10px;border-bottom:1px solid #dde3ea;text-align:right;vertical-align:top;font-variant-numeric:tabular-nums}th{background:#e9eff4}td{white-space:nowrap}th:first-child,td:first-child{text-align:left}details{padding:14px;border:1px solid #dde3ea;border-radius:6px;margin-top:20px}summary{font-weight:600;cursor:pointer}li{margin:8px 0}.notes{font-size:13px}</style></head><body><main>',
  '<h1>Main results: school-ending versus voluntary applicants</h1><p>The seven paper outcomes, with separate group rows and unchanged VA measures and controls.</p>',
  section('Assignment risk among all applicants',risk_table),
  section('First-stage coefficients',fs,'Coefficient (robust SE).'),
  section('Pass-through coefficients',ps,'Coefficient (robust SE).'),
  section('Mean expected VA: all applicants',expected_all,'Math and verbal: outcome SD. All other columns: percentage points. The same original VA scale is used for both groups.'),
  '<details><summary>Estimation sample sizes, first-stage F and pass-through tests</summary>',
  section('N',ns),section('First-stage F',fstats),section('p-value: pass-through = 0',tables$p_psi_zero),section('p-value: pass-through = 1',tables$p_psi_one),'</details>',
  '<details><summary>Expected VA in the exact IV samples and probability coverage</summary>',
  section('Mean expected VA: exact IV estimation sample',expected_iv,'Sample sizes are outcome-specific and match the first-stage/pass-through tables.'),
  section('Probability on schools with measured VA: all applicants',covered),'</details>',
  '<h2>Definitions and verification</h2><ul class="notes">',paste0('<li>',esc(notes),'</li>',collapse=''),'</ul></main></body></html>')
writeLines(html,file.path(report_dir,'main_by_continuation.html'))
mdtable <- function(z) c(paste0('| ',paste(names(z),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(z)),collapse=' | '),' |'),
  apply(z,1,function(v) paste0('| ',paste(v,collapse=' | '),' |')))
md <- c('# Main results by school continuation','')
for(nm in names(tables)) md <- c(md,paste0('## ',gsub('_',' ',nm,fixed=TRUE)),'',mdtable(tables[[nm]]),'')
writeLines(c(md,'## Definitions and verification','',paste0('- ',notes)),file.path(report_dir,'main_by_continuation.md'))
tex_header <- c(' & \\multicolumn{2}{c}{Achievement} & \\multicolumn{3}{c}{Postsecondary access} & \\multicolumn{2}{c}{Within higher-education choice} \\\\',
  '\\cmidrule(lr){2-3} \\cmidrule(lr){4-6} \\cmidrule(lr){7-8}',
  ' & Math & Verbal & Exam taking & Fin. aid app. & Higher-ed enrollment & High-premium inst. & High-premium field \\\\')
tex_row <- function(label,values) paste0(label,' & ',paste(values,collapse=' & '),' \\\\')
tex_p <- function(p) ifelse(p<.001,'$<0.001$',fmt(p))
make_tex <- function(kind) {
  first <- kind=='first_stage'
  body <- unlist(lapply(groups,function(g) {
    z <- r[sample==g][match(specs$spec,spec)]
    b <- if(first) z$fs_beta else z$beta; std <- if(first) z$fs_se else z$se
    p <- if(first) z$fs_p_value else z$p_value_equal_zero
    c(tex_row(group_labels[g],paste0(fmt(b),stars(p))),tex_row('',paste0('(',fmt(std),')')),
      if(first) tex_row('First-stage F',fmt(z$fs_f,1)) else c(
        tex_row('$p$: $\\psi=0$',tex_p(z$p_value_equal_zero)),tex_row('$p$: $\\psi=1$',tex_p(z$p_value_equal_one))),
      tex_row('N',num(z$n_obs)),'\\addlinespace')
  }))
  tex <- c('\\begin{table}[!htbp]','\\centering',paste0('\\caption{',if(first) 'First-stage' else 'Pass-through',' coefficients by origin-school continuation}'),
    paste0('\\label{tab:',kind,'-by-continuation}'),'\\resizebox{\\textwidth}{!}{%','\\begin{tabular}{lccccccc}','\\toprule',tex_header,'\\midrule',body,
    '\\bottomrule','\\end{tabular}','}','\\par\\medskip\\footnotesize','\\begin{minipage}{\\textwidth}',
    paste0('\\textit{Notes:} ',paste(notes[1:5],collapse=' ')),'\\end{minipage}','\\end{table}')
  writeLines(tex,file.path(table_dir,paste0(kind,'_by_continuation.tex')))
}
make_tex('first_stage'); make_tex('pass_through')
extra <- c('\\begin{table}[!htbp]','\\centering','\\caption{Expected value added by applicant continuation group}',
  '\\label{tab:expected-va-by-continuation}','\\resizebox{\\textwidth}{!}{%','\\begin{tabular}{lccccccc}','\\toprule',tex_header,'\\midrule',
  apply(expected_all,1,function(x) tex_row(x[1],x[-1])),'\\bottomrule','\\end{tabular}','}',
  '\\par\\medskip\\footnotesize','\\begin{minipage}{\\textwidth}',paste0('\\textit{Notes:} ',paste(notes[c(4,6,7)],collapse=' ')),
  '\\end{minipage}','\\end{table}')
writeLines(extra,file.path(table_dir,'expected_va_by_continuation.tex'))
risk_tex <- c('\\begin{table}[!htbp]','\\centering','\\caption{Assignment risk by applicant continuation group}',
  '\\label{tab:assignment-risk-by-continuation}','\\begin{tabular}{lrrr}','\\toprule',
  ' & Applicants & With risk & Share with risk \\\\','\\midrule',
  vapply(groups,function(g) {z<-rates[sample==g];tex_row(group_labels[g],c(num(z$applicants),num(z$applicants_at_risk),paste0(fmt(100*z$risk_share,2),'\\%')))},character(1)),
  '\\bottomrule','\\end{tabular}','\\par\\medskip\\footnotesize','\\begin{minipage}{\\textwidth}',
  paste0('\\textit{Notes:} ',notes[4]),'\\end{minipage}','\\end{table}')
writeLines(risk_tex,file.path(table_dir,'assignment_risk_by_continuation.tex'))
print(risk_table);print(fs);print(ps);print(expected_all)
