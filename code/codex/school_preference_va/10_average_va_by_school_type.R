# Student-weighted school VA means: the user selected 2024 Directorio enrollment.
# No demand model is refitted. Use one common four-VA-complete school population.
source("code/codex/school_preference_va/common.R")
source(file.path(code_dir, "school_types.R"))
types <- read_school_types()
directory_path <- attr(types, "source_path")
normalization_path <- file.path(clean_dir, "va_normalization.csv")
norm <- fread(normalization_path)[match(specs$standardized, standardized_variable)]
check("same four VA normalization parameters as preference models",
  identical(norm$standardized_variable, specs$standardized) && all(norm$reference_sd > 0))
v <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome",
  "controlled_value_added_eb_centered_student"))
v <- v[analysis_sample == "All" & outcome %chin% specs$outcome]
check("canonical EB VA source unique by school and outcome", !anyDuplicated(v[, .(school_rbd, outcome)]))
v <- dcast(v, school_rbd ~ outcome, value.var = "controlled_value_added_eb_centered_student")
setnames(v, c("school_rbd", specs$outcome), c("rbd", specs$raw))
for (k in seq_len(nrow(specs))) v[, (specs$standardized[k]) :=
  (get(specs$raw[k]) - norm$reference_mean[k]) / norm$reference_sd[k]]

# This national universe matches the preceding Directorio distribution, including
# private-paid establishments. MAT_TOTAL covers every grade in the school.
national <- types[directory_status == 1L & directory_has_enrollment == 1L &
  (hc_school == 1L | tp_school == 1L | artistic_school == 1L)]
check("national schools unique and enrollment weights positive", !anyDuplicated(national$rbd) &&
  all(is.finite(national$whole_school_enrollment) & national$whole_school_enrollment > 0))
national <- merge(national, v, by = "rbd", all.x = TRUE, sort = FALSE)
national[, complete_four_va := rowSums(is.finite(as.matrix(.SD))) == length(specs$raw), .SDcols = specs$raw]
categories <- c("HC only", "Mixed HC/TP", "TP only", "Artistic only")
check("national universe has only the four regular secondary types", all(national$school_type %chin% categories))
coverage <- national[, .(all_schools = .N, all_enrollment = sum(whole_school_enrollment),
  complete_schools = sum(complete_four_va), complete_enrollment = sum(whole_school_enrollment[complete_four_va])),
  by = school_type]
coverage[, `:=`(school_coverage = complete_schools / all_schools,
  enrollment_coverage = complete_enrollment / all_enrollment)]
coverage <- coverage[match(categories, school_type)]

summarize <- function(d, population) rbindlist(lapply(seq_len(nrow(specs)), function(k) {
  d[, {
    raw_mean <- weighted.mean(get(specs$raw[k]), whole_school_enrollment)
    standardized_mean <- weighted.mean(get(specs$standardized[k]), whole_school_enrollment)
    .(schools = .N, enrollment = sum(whole_school_enrollment), raw_mean = raw_mean,
      standardized_mean = standardized_mean,
      normalization_identity_error = abs(standardized_mean -
        (raw_mean - norm$reference_mean[k]) / norm$reference_sd[k]))
  }, by = school_type][, `:=`(population = population, outcome = specs$outcome[k],
    label = specs$label[k], raw_unit = if (k == 1L) "Student test-score SD" else "Probability",
    raw_display_mean = raw_mean * if (k == 1L) 1 else 100,
    raw_display_unit = if (k == 1L) "Student test-score SD" else "Percentage points")]
}))
complete <- national[complete_four_va == TRUE]
means <- summarize(complete, "National active regular secondary schools")
check("weighted mean normalization identity", max(means$normalization_identity_error) < 1e-12)
check("identical school and student denominators across VA outcomes", all(means[,
  .(ok = uniqueN(schools) == 1L && uniqueN(enrollment) == 1L), by = school_type]$ok))

# Retain a clearly separate comparison on preference-model destination support.
# The primary national means above do not impose the model's 2501-school support.
model_schools <- unique(as.data.table(readRDS(file.path(clean_dir, "estimation_data.rds")))[, .(rbd)])
model_means <- summarize(complete[rbd %in% model_schools$rbd], "Preference destinations active in 2024")
fwrite(means, file.path(clean_dir, "enrollment_weighted_va_by_school_type.csv"))
fwrite(model_means, file.path(clean_dir, "enrollment_weighted_va_model_support.csv"))
fwrite(coverage, file.path(clean_dir, "enrollment_weighted_va_coverage.csv"))
fwrite(manifest_rows(c(va_path, directory_path, normalization_path)),
  file.path(clean_dir, "enrollment_weighted_va_manifest.csv"))

display <- function(x, value) {
  a <- dcast(x, school_type + schools + enrollment ~ label, value.var = value)
  a <- a[match(categories[categories %chin% a$school_type], school_type)]
  setcolorder(a, c("school_type", "schools", "enrollment", specs$label))
  for (name in specs$label) set(a, j = name, value = sprintf("%.3f", a[[name]]))
  a[, `:=`(schools = fmt_n(schools), enrollment = fmt_n(enrollment))]
  setnames(a, c("school_type", "schools", "enrollment"), c("School type", "Schools", "Enrollment (all grades)"))
  a
}
z_display <- display(means, "standardized_mean")
raw_display <- display(means, "raw_display_mean")
coverage_display <- copy(coverage)
coverage_display[, `:=`(school_coverage = sprintf("%.2f%%", 100 * school_coverage),
  enrollment_coverage = sprintf("%.2f%%", 100 * enrollment_coverage))]
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>Enrollment-weighted VA by school type</title>',
  '<style>body{font:16px/1.5 system-ui;margin:32px;color:#172536}main{max-width:1250px;margin:auto}',
  'table{border-collapse:collapse;width:100%}th,td{padding:10px;text-align:right;border-bottom:1px solid #ddd}',
  'th:first-child,td:first-child{text-align:left}thead{background:#edf2f6}.table{overflow:auto}</style></head><body><main>',
  '<h1>Enrollment-weighted average school VA by type</h1>',
  '<p>National 2024 functioning schools with enrollment offering regular secondary education, including private-paid schools. ',
  'Use existing All-sample pooled 2017-2020 EB VAs. Each school is weighted by 2024 Directorio MAT_TOTAL. ',
  'These weights include all grades; they are not secondary-only or track-specific enrollment counts. ',
  'The primary table uses all nationally available schools, rather than only SAE study destinations.</p>',
  '<h2>Same pooled school-SD units as preference models</h2>', html_table(z_display),
  '<p>Use the unchanged means and SDs across the preference model\'s 2,501 unique schools. No within-type or ',
  'enrollment-weighted restandardization is performed. Positive values indicate VA above that reference mean, ',
  'not necessarily a positive raw VA. These are descriptive means of school indices.</p>',
  '<h2>Original VA units</h2>', html_table(raw_display),
  '<p>Math is in student test-score SDs. Exam taking, high-premium field and high-premium institution are in ',
  'percentage points. These retain the existing VA source populations and student-centered normalization; ',
  'institution and field VAs are estimated among exam takers. They are not outcome prevalence rates.</p>',
  '<h2>Common four-VA coverage</h2>', html_table(coverage_display),
  '<p>Require all four VAs to be observed at a school, using identical school and enrollment denominators ',
  'across outcomes. No missing VA is replaced by zero. Artistic-only results involve two schools. ',
  'Mixed means regular HC310 plus TP410/510/610/710/810; TP-only means TP without regular HC. ',
  'Lower grades in the establishment are allowed. The classification is fixed in 2024.</p>',
  '<h2>Separate comparison: preference-model destination support</h2>', html_table(display(model_means, "standardized_mean")),
  '<p>This comparison restricts the same complete, active schools to destination RBDs in the preference model, ',
  'using the same weights and VA normalization. It is separate from the requested national table.</p>',
  '</main></body></html>')
writeLines(html, file.path(report_dir, "enrollment_weighted_va_by_school_type.html"), useBytes = TRUE)
fwrite(means, file.path(table_dir, "enrollment_weighted_va_by_school_type.csv"))
save_checks("enrollment_weighted_va_verification.csv")
print(z_display)
print(raw_display)
print(coverage)
