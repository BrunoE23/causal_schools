# Describe the previously reported ranking; do not rerank from a new app cache.
source("code/codex/school_preference_va/common.R")
source(file.path(code_dir, "school_types.R"))
types <- read_school_types()
directory_path <- attr(types, "source_path")
ranking_path <- file.path(root, "output/tables/applicant_priority_comparison/top_100_sae_destination_schools.csv")
a <- fread(ranking_path)
check("existing top100 unique school and rank", nrow(a) == 100L && !anyDuplicated(a$rbd) &&
  identical(sort(a$rank), seq_len(100L)))
a <- merge(a, types, by = "rbd", all.x = TRUE, sort = FALSE)
setorder(a, rank)
check("every ranked school has observed directory classification", !anyNA(a$school_type) && nrow(a) == 100L)
check("mutually exclusive TP subtypes reproduce anyTP flag", all(a$mixed_hc_tp + a$tp_only == a$tp_school))
categories <- c("HC only", "Mixed HC/TP", "TP only", "Artistic only", "Other/no regular secondary")
summary <- rbindlist(lapply(c(10L, 20L, 50L, 100L), function(n) {
  z <- a[rank <= n]
  out <- z[, .(schools = .N, whole_school_enrollment = sum(whole_school_enrollment)), by = school_type]
  out <- merge(data.table(school_type = categories), out, by = "school_type", all.x = TRUE, sort = FALSE)
  out[is.na(schools), `:=`(schools = 0L, whole_school_enrollment = 0)]
  out[, `:=`(top_n = n, share_schools = schools / n,
    share_enrollment = whole_school_enrollment / sum(whole_school_enrollment))]
  out[match(categories, school_type)]
}))
check("each topN distribution sums to N and one", all(summary[, .(ok = sum(schools) == top_n[1L] &&
  abs(sum(share_schools) - 1) < 1e-12 && abs(sum(share_enrollment) - 1) < 1e-12), by = top_n]$ok))
fwrite(types, file.path(clean_dir, "school_type_classification.csv"))
fwrite(a, file.path(clean_dir, "top100_school_types.csv"))
fwrite(summary, file.path(clean_dir, "top_school_type_distribution.csv"))
fwrite(manifest_rows(c(ranking_path, directory_path)), file.path(clean_dir, "top_school_type_manifest.csv"))

display <- summary[schools > 0L, .(`Top schools` = top_n, Classification = school_type,
  Schools = schools, `Share of schools` = sprintf("%.2f%%", 100 * share_schools),
  `Enrollment (all grades)` = fmt_n(whole_school_enrollment),
  `Share of enrollment` = sprintf("%.2f%%", 100 * share_enrollment))]
fwrite(summary, file.path(table_dir, "top_school_type_distribution.csv"))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Types of most demanded schools</title>',
  '<style>body{font:16px/1.5 system-ui;margin:32px;color:#172536}main{max-width:1100px;margin:auto}',
  'table{border-collapse:collapse;width:100%}th,td{padding:10px;text-align:right;border-bottom:1px solid #ddd}',
  'th:nth-child(2),td:nth-child(2){text-align:left}thead{background:#edf2f6}.table{overflow:auto}</style></head><body><main>',
  '<h1>School types among the most demanded SAE destinations</h1>',
  '<p>Uses the previously reported top100 ranking by distinct paper-sample applicants listing each school, 2018-2020. ',
  'The schools are not reranked. Classification uses regular secondary offerings in the 2024 Directorio: ',
  'HC310 and TP410/510/610/710/810. Mixed schools offer both; TP-only schools offer TP without HC. ',
  'These labels allow lower grades in the same establishment.</p>', html_table(display),
  '<p>School shares use the indicated topN as denominator. Enrollment shares use MAT_TOTAL summed only over those ',
  'schools. MAT_TOTAL includes all grades, not just secondary students; the directory does not identify pupils ',
  'taking each track within a mixed school. Classification is fixed in 2024 and does not establish historical ',
  'offerings at the application date. All ranked schools match; no active-status restriction changes the earlier ranking.</p>',
  '</main></body></html>')
writeLines(html, file.path(report_dir, "top_school_types.html"), useBytes = TRUE)
save_checks("top_school_type_verification.csv")
print(display)
