# Format the direct-IV / RSS-predicted-gain validation ratio matrix.
suppressPackageStartupMessages(library(data.table))
repo_wd<-Sys.getenv('CAUSAL_SCHOOLS_REPO_WD',unset=getwd())
out_dir<-file.path(repo_wd,'output/tables/rss_debiased_varcov')
input_path<-file.path(out_dir,'unshrunk_cross_outcome_iv_rss_validation.csv')
x<-fread(input_path)
order_keys<-c('math','verbal','exam','highinst','highpay','income','aid')
labels<-c('Math','Verbal','Exam taking','High-premium inst.','High-premium field','Log income','Fin. aid app.')
x[,validation_ratio:=direct_iv/predicted_causal_gain]
x[,`:=`(from_order=match(from_key,order_keys),to_order=match(to_key,order_keys))]
setorder(x,from_order,to_order)
fwrite(x[,.(from_key,from_label,to_key,to_label,direct_iv,predicted_causal_gain,validation_ratio,direct_se,n_obs)],file.path(out_dir,'unshrunk_cross_outcome_iv_rss_validation_ratios.csv'))
fmt<-function(z){ifelse(is.finite(z),sprintf('%.2f',z),'--')}
rows<-unlist(lapply(order_keys,function(k){
  z<-x[from_key==k][match(order_keys,to_key)]
  paste0(labels[match(k,order_keys)],' & ',paste(fmt(z$validation_ratio),collapse=' & '),' \\\\')
}))
tex<-c('\\begin{table}[!htbp]','\\centering','\\caption{Ratio of direct cross-outcome IV gains to RSS-implied causal gains}','\\label{tab:unshrunk-cross-iv-rss-validation-ratios}','\\resizebox{\\textwidth}{!}{%','\\begin{tabular}{lccccccc}','\\toprule',paste0('Row school VA & ',paste(labels,collapse=' & '),' \\\\'),'\\midrule',rows,'\\bottomrule','\\end{tabular}}','\\par\\medskip','\\footnotesize','\\begin{minipage}{\\textwidth}','Notes: Each cell divides the direct unshrunk cross-outcome IV estimate by the corresponding RSS-implied causal prediction, defined as the RSS projection from the row VA to the column VA multiplied by the column outcome\'s unshrunk same-outcome IV pass-through. A ratio of one denotes exact agreement, zero denotes no direct IV gain despite a nonzero prediction, and a negative ratio denotes opposite signs. Diagonal entries equal one mechanically because their prediction uses the same same-outcome IV coefficient. Ratios can be large when the predicted gain is close to zero; the underlying levels and standard errors are reported in Table~\\ref{tab:unshrunk-cross-iv-rss-validation}.','\\end{minipage}','\\end{table}')
writeLines(tex,file.path(out_dir,'unshrunk_cross_outcome_iv_rss_validation_ratios.tex'))
print(x[,.(from_label,to_label,direct_iv,predicted_causal_gain,validation_ratio)])
