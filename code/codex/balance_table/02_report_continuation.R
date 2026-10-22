# Render the Table 2 continuation variant from the verified small summary files.
suppressPackageStartupMessages(library(data.table))
root <- normalizePath('.',winslash='/')
clean <- file.path(root,'data/clean/balance_table_continuation')
tables <- file.path(root,'output/tables/balance_table')
reports <- file.path(root,'output/reports/balance_table')
stats <- fread(file.path(clean,'stats.csv'))
counts <- fread(file.path(clean,'counts.csv'))
coverage <- fread(file.path(clean,'coverage.csv'))
matches <- fread(file.path(clean,'matches.csv'))
compare <- fread(file.path(clean,'paper_replication.csv'))
attendance <- fread(file.path(clean,'nonprivate_attendance.csv'))
stopifnot(nrow(compare)==45L,all(compare$passed))
stopifnot(setequal(attendance$group, counts$group), !anyDuplicated(attendance$group))
groups <- counts$group
labels <- setNames(counts$label,groups)
specs <- unique(stats[,.(var,label)],by='var')
unresolved <- counts[group=='g_sae_app',students]-sum(counts[group %in% c('g_ending','g_voluntary'),students])
fmt <- function(value,var) if(is.na(value)) '---' else sprintf(if(var %in%
  c('edad_alu','income_decile_imputed','mother_educ_years_imputed')) '%.1f' else '%.3f',value)
rows <- list()
add <- function(label,var_name=NULL,block_name=NULL,section=NULL) {
  cells <- if(!is.null(var_name)) vapply(groups,function(g) fmt(stats[group==g & var==var_name,mean],var_name),character(1))
    else if(!is.null(block_name)) vapply(groups,function(g) sprintf('%.1f%%',100*matches[group==g & block==block_name,match]),character(1))
    else rep('',5)
  rows[[length(rows)+1L]] <<- data.table(section=if(is.null(section)) '' else section,label,
    broad=cells[1],applicants=cells[2],risk=cells[3],ending=cells[4],voluntary=cells[5])
}
add('Student characteristics',section='Student characteristics')
add('Female','female'); add('Metropolitan Region (Santiago)','in_rm'); add('Age','edad_alu')
add('Baseline SIMCE variables',section='Baseline SIMCE variables')
add('Math 4th grade (z)','z_sim_mat_4to'); add('Verbal 4th grade (z)','z_sim_leng_4to')
add('Income decile','income_decile_imputed'); add('Mother education years','mother_educ_years_imputed')
add('% matched all SIMCE vars',block_name='SIMCE')
add('Middle school',section='Middle school')
add('GPA (z)','z_gpa_middle_mean'); add('Attendance (z)','z_att_middle_mean')
add('% matched all middle-school vars',block_name='Middle school')
add('Outcomes',section='Outcomes')
for(v in c('z_year_math_max','z_year_leng_max','admission_exam_taker','higher_ed_enrolled_m1','high_inst_m1','high_paying_field_m1'))
  add(specs[var==v,label],v)
display <- rbindlist(rows)
attendance_cells <- vapply(groups,function(g) sprintf('%.2f%%',100*attendance[group==g,nonprivate_share]),character(1))
display <- rbind(display, data.table(section='',label='Attends non-private high school',
  broad=attendance_cells[1],applicants=attendance_cells[2],risk=attendance_cells[3],ending=attendance_cells[4],voluntary=attendance_cells[5]))
footer <- as.data.table(setNames(lapply(groups,function(g) c(format(counts[group==g,students],big.mark=',',trim=TRUE),
  if(g=='g_broad') '---' else sprintf('%.2f%%',counts[group==g,percent_applicants]),
  sprintf('%.1f%%',counts[group==g,percent_broad]))),names(display)[3:7]))
footer[,`:=`(section='',label=c('N','Share of SAE applicants','Percent of broad panel'))]
setcolorder(footer,names(display))
display <- rbindlist(list(display,footer),use.names=TRUE)
stopifnot(nrow(display)==25L,
  abs(sum(counts[group %in% c('g_ending','g_voluntary'),students])+unresolved-counts[group=='g_sae_app',students])<1e-10)
fwrite(display,file.path(clean,'display_table.csv'))
notes <- c('The first three columns reproduce Table 2. The broad panel contains cohorts 2017-2020 with an observed high school and age 12-16. SAE applicants are timely 2018-2020 grade-9 applicants; assignment risk requires maximum simulated assignment probability below one.',
  'The two new columns are subsets of the SAE-applicant column. School ending means the grade-8 origin has no offering above grade 8 in the contemporaneous SAE offering file. Voluntary means that origin offers grade 9. The classification concerns continuation availability, not an observed motive or actual school change.',
  sprintf('%s applicants (%.2f%%) have an unresolved origin offering or another offering pattern. They remain in the overall applicant column and, when at risk, in the risk column, but enter neither new column.',format(unresolved,big.mark=',',trim=TRUE),100*unresolved/counts[group=='g_sae_app',students]),
  'Share of SAE applicants uses all 294,198 timely applicants as its denominator; the broad-panel cell is not applicable. The two continuation groups exclude the 1.62% with unresolved/other origins. Non-private high-school attendance uses the most-attended high school and the same private-paid sector indicator as the applicant-priority comparison; the share is among students with an observed sector classification, with coverage saved separately.',
  'Cells report means among available observations, using the same variable definitions and normalization as Table 2. Block match rates and group sizes are retained. Differences, standard errors, and joint tests are omitted.')
esc <- function(x) {x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
html_rows <- vapply(seq_len(nrow(display)),function(i) {
  z<-display[i]
  if(nzchar(z$section)) return(paste0('<tr class="section"><th colspan="6">',esc(z$label),'</th></tr>'))
  paste0('<tr><th>',esc(z$label),'</th>',paste0('<td>',esc(unlist(z[,3:7,with=FALSE])),'</td>',collapse=''),'</tr>')
},character(1))
source('code/codex/balance_table/04_lottery_results_section.R')
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Table 2: applicant continuation groups</title>',
  '<style>body{font:15px/1.5 system-ui,sans-serif;background:#f5f7fa;color:#243347;margin:0}main{max-width:1180px;margin:28px auto;background:white;padding:30px;border-radius:10px}h1{font-size:27px}.table{overflow-x:auto}table{width:100%;border-collapse:collapse;font-size:14px}th,td{padding:8px;border-bottom:1px solid #e1e5ea;text-align:right;font-variant-numeric:tabular-nums}th:first-child{text-align:left}thead th{background:#e8eef3;vertical-align:bottom}.section th{text-align:left;background:#f4f6f8;padding-top:13px}tbody th{font-weight:400}p.notes{font-size:13px}@media print{main{padding:0;margin:0}body{background:white}tr{break-inside:avoid}}</style></head><body><main><h1>Table 2: applicants by school continuation</h1><p>Means only, with the paper\'s original samples and variables.</p><div class="table"><table><thead><tr><th></th>',
  paste0('<th>',esc(labels),'</th>',collapse=''),'</tr></thead><tbody>',paste(html_rows,collapse=''),
  '</tbody></table></div>',paste0('<p class="notes">',esc(notes),'</p>',collapse=''),lottery_html,'</main></body></html>')
writeLines(html,file.path(reports,'table2_continuation.html'))
tex_escape <- function(x) gsub('%','\\%',x,fixed=TRUE)
tex_rows <- vapply(seq_len(nrow(display)),function(i) {
  z<-display[i]
  if(nzchar(z$section)) return(paste0('\\midrule\n\\multicolumn{6}{l}{\\textit{',z$label,'}} \\\\[2pt]'))
  paste0(paste(tex_escape(unlist(z[,2:7,with=FALSE])),collapse=' & '),' \\\\')
},character(1))
tex <- c('\\begin{table}[H]','\\centering','\\caption{Balance across samples and applicant continuation groups}',
  '\\label{tab:balance_samples_continuation}','\\scriptsize','\\setlength{\\tabcolsep}{4pt}',
  '\\begin{tabular}{@{}lccccc@{}}','\\toprule',
  ' & \\shortstack{Broad Panel\\\\(All HS)} & \\shortstack{SAE\\\\Applicants} & \\shortstack{SAE Applicants\\\\w/ risk} & \\shortstack{SAE Applicants\\\\School ends at 8} & \\shortstack{SAE Applicants\\\\Voluntary} \\\\',
  ' & (1) & (2) & (3) & (4) & (5) \\\\',tex_rows,'\\bottomrule','\\end{tabular}',
  '\\par\\medskip\\footnotesize','\\begin{minipage}{\\textwidth}',
  paste0('\\textit{Notes:} ',paste(tex_escape(notes),collapse=' ')),'\\end{minipage}','\\end{table}')
writeLines(tex,file.path(tables,'balance_table_continuation.tex'))
writeLines(capture.output(sessionInfo()),file.path(clean,'session_info.txt'))
print(counts); print(coverage); print(display)
message('Verified all 45 original Table 2 means, Ns and variable match rates.')
