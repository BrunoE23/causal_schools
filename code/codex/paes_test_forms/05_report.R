suppressPackageStartupMessages({library(data.table);library(ggplot2)})
out <- 'data/clean/paes_test_forms'
report <- 'output/reports/paes_test_forms'
fig <- 'output/figures/paes_test_forms'
dir.create(report,recursive=TRUE,showWarnings=FALSE)
dir.create(fig,recursive=TRUE,showWarnings=FALSE)
j <- fread(file.path(out,'joint_tests.csv'))
p <- fread(file.path(out,'pairwise_contrasts.csv'))
l <- fread(file.path(out,'largest_score_gaps.csv'))
a <- fread(file.path(out,'sample_audit.csv'))
v <- fread(file.path(out,'verification.csv'))
stopifnot(all(v$passed))
main <- merge(l[sitting=='REG' & specification=='adjusted'],
  j[sitting=='REG' & specification=='adjusted' & outcome=='score',.(year,subject,n,schools,joint_p=p_value,joint_holm=p_holm)],by=c('year','subject'))
main[, `:=`(ci_lo=fifelse(estimate_b_minus_a>=0,lower_simultaneous95,-upper_simultaneous95),
  ci_hi=fifelse(estimate_b_minus_a>=0,upper_simultaneous95,-lower_simultaneous95))]
subject_order <- c('CL','M1','M2','HCS','CBIO','CFIS','CQUI','CTP')
main[,subject_order:=match(subject,subject_order)]
setorder(main,year,subject_order)
fwrite(main,file.path(out,'report_main.csv'))
focus <- main[subject %in% c('CL','M1')]
focus[,row:=paste(year,label,paste0('form ',advantaged_form,' minus ',other_form),sep=' | ')]
focus[,row:=factor(row,levels=rev(row))]
chart <- ggplot(focus,aes(x=gap,y=row,color=joint_holm<.05))+
  geom_vline(xintercept=0,color='#aab4bc',linetype=2)+
  geom_errorbar(aes(xmin=ci_lo,xmax=ci_hi),orientation='y',width=.15,linewidth=.8)+
  geom_point(size=3)+scale_color_manual(values=c('FALSE'='#8a969f','TRUE'='#007f88'),guide='none')+
  labs(title='Within-year score differences between PAES forms',
    subtitle='Separate comparisons for each admissions year and subject; regular sittings',
    x='Adjusted score difference (PAES points)',y=NULL,
    caption='Largest fitted form gap in each subject/year. Intervals cover all 102 regular-sitting pairwise contrasts jointly (95%, Bonferroni).\nSchool fixed effects; prior grades/ranking, sex, age, graduation year and educational track controls. School-clustered SEs.\nTeal: joint form test passes Holm correction across 32 year-subject tests. Estimates are associations; assignment is not verified.')+
  theme_minimal(base_size=12)+theme(plot.title=element_text(face='bold',size=20),
    plot.subtitle=element_text(margin=margin(b=16)),panel.grid.major.y=element_blank(),
    plot.caption=element_text(hjust=0,size=9,lineheight=1.25),plot.margin=margin(18,22,14,18))
ggsave(file.path(fig,'reading_math_form_gaps.png'),chart,width=12.5,height=6.8,dpi=180,bg='white')
esc <- function(x) {x<-gsub('&','&amp;',as.character(x),fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
tbl <- function(d) paste0('<div class="scroll"><table><thead><tr>',paste0('<th>',esc(names(d)),'</th>',collapse=''),
  '</tr></thead><tbody>',paste(apply(d,1,function(r) paste0('<tr>',paste0('<td>',esc(r),'</td>',collapse=''),'</tr>')),collapse=''),'</tbody></table></div>')
fp <- function(x) ifelse(x<.001,formatC(x,format='e',digits=1),sprintf('%.3f',x))
fn <- function(x) format(x,big.mark=',',trim=TRUE,scientific=FALSE)
display <- main[,.(Year=year,Test=label,N=fn(n),Comparison=paste(advantaged_form,'minus',other_form),
  `Adjusted gap`=sprintf('%.2f',gap),`Simultaneous 95% CI`=sprintf('[%.2f, %.2f]',ci_lo,ci_hi),
  `Joint p`=fp(joint_p),`Joint Holm p`=fp(joint_holm))]
rob <- dcast(l[sitting=='REG' & subject=='CL'],year~specification,value.var='gap')
for(nm in setdiff(names(rob),'year')) set(rob,j=nm,value=round(rob[[nm]],2))
setnames(rob,c('year','adjusted','adjusted_current_graduates','raw_same_sample','school_only'),
  c('Year','Fully adjusted','Current graduates, adjusted','Raw, same sample','School effects only'))
bal <- j[specification=='balance_school',.(Tests=.N,`p < .05`=sum(p_value<.05),`Holm p < .05`=sum(p_holm<.05)),by=.(Sitting=sitting,Characteristic=outcome)]
winter <- merge(l[sitting=='INV'&specification=='adjusted'],j[sitting=='INV'&specification=='adjusted'&outcome=='score',.(year,subject,n,joint_holm=p_holm)],by=c('year','subject'))
winter <- winter[,.(Year=year,Test=label,N=fn(n),Comparison=paste(advantaged_form,'minus',other_form),Gap=sprintf('%.2f',gap),`Joint Holm p`=fp(joint_holm))]
coverage <- a[,.(Sitting=sitting,Year=year,Test=label,`Valid form and score`=fn(valid_form_score),
  `Complete controls`=fn(complete_controls),`Within-school support`=fn(within_school_supported),
  `Retention %`=sprintf('%.1f',100*within_school_supported/valid_form_score))]
headline <- main[year==2026&subject=='CL']
annual <- main[,.(Tests=.N,`Significant after correction`=sum(joint_holm<.05),
  `Subjects with variation`=paste(label[joint_holm<.05],collapse='; ')),by=.(Year=year)]
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>PAES test form analysis</title><style>body{font:16px/1.55 system-ui,sans-serif;background:#f5f7f8;color:#20323c;margin:0}main{max-width:1120px;margin:auto;background:white;padding:44px}h1{font-size:38px;line-height:1.15}h2{margin-top:40px}p{max-width:950px}a{color:#007f88}.lead{font-size:21px}.note{padding:18px 22px;border-left:4px solid #007f88;background:#eef7f7}.scroll{overflow:auto}table{border-collapse:collapse;font-size:13px;width:100%;font-variant-numeric:tabular-nums}th,td{padding:9px 10px;border-bottom:1px solid #dce3e7;text-align:left;white-space:nowrap}th{background:#eef3f5}img{width:100%;height:auto}code{background:#eef3f5;padding:2px 4px}details{margin:22px 0}summary{cursor:pointer;font-weight:650}.muted{color:#61727c}li{margin-bottom:8px}@media(max-width:700px){main{padding:22px}h1{font-size:30px}}</style><main>',
  '<div class="muted">Research diagnostic &middot; 24 October 2026</div><h1>Do scores vary across PAES forms within a year?</h1>',
  '<p class="lead">Yes: separate within-year comparisons find small, statistically detectable differences. ',sum(main$joint_holm<.05),
  ' of 32 regular-sitting year-subject tests reject equal mean scores after Holm correction.</p>',
  '<p>Each test asks whether all form coefficients are equal within one year, sitting and subject. No test asks whether a form is always better, and no common form effect is imposed across years. The multiple-testing correction is conservative across all 32 separate tests.</p>',tbl(annual),
  '<div class="note"><strong>Largest estimated gap:</strong> Reading, admissions 2026 (sat in late 2025), form 103 versus 106: <strong>+',sprintf('%.2f',headline$gap),
  ' points</strong>, or ',sprintf('%.3f',abs(headline$estimate_sd)),
  ' SD. The simultaneous 95% interval is [',sprintf('%.2f',headline$ci_lo),', ',sprintf('%.2f',headline$ci_hi),
  ']. This uses ',fn(headline$n),' students from ',fn(headline$schools),' attended schools.</div>',
  '<p>These differences remain in the reported, equated scores. Baseline balance and robustness are consistent with a form advantage, but the available files do not establish randomized assignment. The estimates are conditional associations, not a verified causal effect or a claim that every student benefits equally.</p>',
  '<img src="../../figures/paes_test_forms/reading_math_form_gaps.png" alt="Estimated largest Reading and Math M1 form gaps, with simultaneous intervals">',
  '<h2>What was tested</h2><p>Eight tests/modules in each regular admissions process, 2023-2026: Reading, M1, M2, History, and four separate science modules. A supplementary analysis covers PAES winter 2024-2026. Winter admissions 2023 was PDT and is excluded. Form codes are local to a year, sitting and test; do not pool a code across years.</p>',
  '<p>Each form field (<code>FORMA_REG_*</code> or <code>FORMA_INV_*</code>) is paired with its same-sitting <code>*_REG_ACTUAL</code> or <code>*_INV_ACTUAL</code> score. Science modules use the corresponding science score and module-specific positive form identifier. No best-of-sittings score is used. Correct-answer counts are a secondary outcome and are never used as controls for the score outcome: doing so would condition on a consequence of test form.</p>',
  '<h2>Adjusted comparisons, regular PAES</h2><p>The table shows the highest-minus-lowest fitted form contrast within each year/test. Because these pairs were selected from the data, their intervals adjust jointly for all 102 pairwise regular contrasts. The joint test uses all forms, with Holm correction across 32 year-subject tests. Selecting the largest fitted difference tends to exaggerate its point estimate; the simultaneous intervals account for pair selection.</p>',tbl(display),
  '<h2>Do the within-year results survive adjustment?</h2><p>For Reading, the direction of the largest contrast stays the same within each year across the four specifications below. Restricting to current graduates gives a 2026 within-year difference of 8.45 points.</p>',tbl(rob),
  '<p>Across all subjects, 12 of 32 current-graduate adjusted tests remain significant after their own Holm correction. No M2 or chemistry year passes the primary regular-sitting correction. M1 does in 2023-2025, but not 2026. This is a test of average score differences; equality of averages would not establish equality throughout the score distribution.</p>',
  '<h2>Baseline balance</h2><p>Within attended schools, form assignments do not show systematic differences in high-school GPA, sex, age, or current-graduate status. No joint baseline test survives Holm correction across all 128 regular-sitting balance tests (or the 72 winter tests). This supports comparability on observed characteristics; it cannot establish balance on unobserved characteristics or replace assignment documentation.</p>',tbl(bal),
  '<h2>Model and inference</h2><ul>',
  '<li>Separate OLS regression by admissions year, sitting, and subject/module; reported score on form indicators plus attended-school fixed effects.</li>',
  '<li>Controls: cubic high-school GPA, cubic ranking score, cubic age, sex, graduation-year indicators, and educational-track indicators. Continuous controls are centered/scaled only for numerical stability; outcomes remain PAES points.</li>',
  '<li>Keep positive form codes and scores 100-1000; positive attended-school RBD; GPA 4-7; ranking score 100-1000; age 14-90; valid recorded sex and graduation year 1930 through the sitting calendar year. Complete-case analysis. Keep schools with at least two observed forms for the relevant sample. No SAE, application or university-enrollment restriction.</li>',
  '<li>Raw, school-only, and fully adjusted specifications use the same sample. The current-graduate robustness restricts graduation year to admissions year minus one and reapplies within-school form support.</li>',
  '<li>Regression-derived standard errors cluster by attended school, using the finite-sample correction in fixest. Wald joint tests use F(q, G-1); pairwise intervals use t(G-1). School effects and clustering do not substitute for test-center or test-room information, which is unavailable here.</li>',
  '<li>The complete coefficient covariance can be numerically singular in nuisance directions involving sparse control categories. Its tiny negative eigenvalues are at machine-rounding scale (relative magnitude below 2.4e-16). No covariance regularization is applied; every tested form block is positive definite.</li></ul>',
  '<h2>Supplementary winter results</h2><p>Only 1 of 24 adjusted winter tests survives Holm correction: biology in admissions 2024. Smaller science samples lose substantial coverage when requiring within-school support; interpret winter estimates as applying to the retained sample.</p>',tbl(winter),
  '<details><summary>Sample accounting</summary><p>Regular samples retain 91.5%-99.2% of takers with valid form and score; winter samples retain 51.4%-94.0%. Three positive-form records have invalid scores and are excluded (one regular M1 in 2023, two winter Reading in 2026). Birth month/year provides age; for winter the age control is measured at December of the same year, a uniform shift absorbed by the cubic specification.</p>',tbl(coverage),'</details>',
  '<h2>Validation and reproducibility</h2><p>All ',nrow(v),' validation checks pass. An independent within-school QR regression and cluster sandwich reproduce the headline 2026 Reading coefficients and form standard errors. Source counts, sample accounting, score/form alignment, multiple-testing corrections and simultaneous intervals are checked. Raw inputs and other project analyses were preserved.</p>',
  '<p><a href="../../../code/codex/paes_test_forms/README.md">Run instructions</a> &middot; ',
  '<a href="../../../data/clean/paes_test_forms/joint_tests.csv">All joint tests</a> &middot; ',
  '<a href="../../../data/clean/paes_test_forms/pairwise_contrasts.csv">All pairwise contrasts</a> &middot; ',
  '<a href="../../../data/clean/paes_test_forms/form_descriptives.csv">Form-level descriptive statistics</a> &middot; ',
  '<a href="../../../data/clean/paes_test_forms/verification.csv">Validation results</a></p>',
  '<h2>Institutional interpretation and sources</h2><p>DEMRE explicitly states that the exact score depends on the form and that forms are equated: ',
  '<a href="https://www.portaldemre.demre.cl/proceso-admision/factores-seleccion/tabla-transformacion-puntajes">official score transformation explanation</a>. ',
  'Its <a href="https://portaldemre.demre.cl/investigacion/documentos/informes/2024-informe-resultados-paes-invierno-admision-2024.pdf">winter 2024 results report</a> describes IRT-based calibration/equating and the forms by subject. ',
  'The <a href="https://www.portaldemre.demre.cl/publicaciones/pdf/manuales/p2024-manual-aplicacion-paes-regular.pdf">regular application manual</a> identifies form, folio, room and desk assignment on candidate labels; the materials inspected do not establish a random assignment mechanism.</p>',
  '<p>Local raw files: <code>A_INSCRITOS_PUNTAJES*PAES*MRUN.csv</code> for 2023-2026 under the project Box raw-data directory; exact paths and metadata are in ',
  '<a href="../../../data/clean/paes_test_forms/source_manifest.csv">source_manifest.csv</a>. The corresponding 2023 and 2026 dictionaries define the form codes and same-sitting scores. ',
  '<code>LOCAL_EDUCACIONAL</code> is an educational-establishment identifier; it was not treated as a test center.</p>',
  '<div class="note"><strong>Conclusion:</strong> The data support small average form advantages, especially in Reading. Establishing a causal instrument would additionally require the assignment rule and, ideally, test-center/room identifiers. These results do not measure university-admission effects.</div>',
  '</main></html>')
writeLines(html,file.path(report,'report.html'),useBytes=TRUE)
message('Report saved: ',file.path(report,'report.html'))
