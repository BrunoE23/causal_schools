library(data.table); library(fixest)
out <- 'data/clean/paes_test_forms'
j <- fread(file.path(out,'joint_tests.csv')); p <- fread(file.path(out,'pairwise_contrasts.csv'))
l <- fread(file.path(out,'largest_score_gaps.csv')); a <- fread(file.path(out,'sample_audit.csv'))
print(j[,.(tests=.N,significant=sum(p_value<.05),significant_holm=sum(p_holm<.05)),by=.(sitting,specification,outcome)])
print(l[sitting=='REG' & subject=='CL',.(year,specification,gap,advantaged_form,other_form)])
print(j[specification=='balance_school' & p_holm<.05])
print(p[specification=='balance_school' & sitting=='REG' & year==2026 & subject=='CL'])
print(a[,.(min_retention=min(within_school_supported/valid_form_score),max_retention=max(within_school_supported/valid_form_score)),by=sitting])
