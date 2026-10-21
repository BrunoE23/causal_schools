# Unadjusted projected-income gender gaps in the paper's broad panel.
suppressPackageStartupMessages(library(data.table))
setDTthreads(min(4L, getDTthreads()))
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
source_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
clean_dir <- file.path(repo_wd, "data/clean/projected_income_gender")
report_dir <- file.path(repo_wd, "output/reports/projected_income_gender")
table_dir <- file.path(repo_wd, "output/tables/projected_income_gender")
for (p in c(clean_dir, report_dir, table_dir)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
keep <- c("mrun", "cohort_gr8", "GEN_ALU", "EDAD_ALU", "most_time_RBD", "COD_SIES_m1",
  "log_program_income_full_clp_m1", "program_income_full_clp_m1")
header <- names(fread(source_path, nrows = 0L))
stopifnot(all(keep %in% header))
fwrite(data.table(path = source_path, bytes = file.info(source_path)$size, selected_columns = length(keep),
  strategy = "Select eight master columns; immediately restrict to the paper broad-panel definition"),
  file.path(clean_dir, "source_manifest.csv"))
message("Reading only income, gender and sample-definition columns from the paper master...")
d <- fread(source_path, select = keep, na.strings = c("", "NA"))
d <- d[cohort_gr8 %in% 2017:2020 & is.finite(most_time_RBD) & most_time_RBD > 0 &
  is.finite(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16]
stopifnot(!anyNA(d$mrun), !anyDuplicated(d$mrun))
checks <- data.table(check = character(), passed = logical())
check <- function(label, passed) {
  checks <<- rbind(checks, data.table(check = label, passed = isTRUE(passed)))
  if (!isTRUE(passed)) stop(label)
  message("PASS: ", label)
}
check("Broad panel reproduces the paper's 931547 students", nrow(d) == 931547L)
d[, enrolled := !is.na(COD_SIES_m1) & nzchar(trimws(as.character(COD_SIES_m1))) &
  !trimws(as.character(COD_SIES_m1)) %in% c("NA", "0")]
d[, female := as.integer(GEN_ALU == 2L)]
d[, gender := fifelse(GEN_ALU == 1L, "Men", fifelse(GEN_ALU == 2L, "Women", NA_character_))]
coverage <- d[, .(students = .N, income_observed = sum(is.finite(log_program_income_full_clp_m1)),
  enrolled = sum(enrolled)), by = gender]
fwrite(coverage, file.path(clean_dir, "coverage.csv"))
d <- d[!is.na(gender) & is.finite(log_program_income_full_clp_m1) &
  is.finite(program_income_full_clp_m1) & program_income_full_clp_m1 > 0]
check("Stored log income equals the log of projected CLP income",
  max(abs(d$log_program_income_full_clp_m1 - log(d$program_income_full_clp_m1))) < 1e-10)
check("Non-enrollees retain the existing 553553 CLP income floor",
  all(abs(d[enrolled == FALSE, program_income_full_clp_m1] - 553553) < 1e-8))
samples <- list(with_imputation = d, enrolled_only = d[enrolled == TRUE])
means <- list(); gaps <- list()
for (sample_name in names(samples)) {
  x <- samples[[sample_name]]
  means[[sample_name]] <- x[, .(N = .N, mean_log_income = mean(log_program_income_full_clp_m1),
    sd_log_income = sd(log_program_income_full_clp_m1),
    mean_income_clp = mean(program_income_full_clp_m1),
    geometric_mean_income_clp = exp(mean(log_program_income_full_clp_m1)),
    enrollment_share = mean(enrolled)), by = gender][, sample := sample_name]
  # The female coefficient is exactly the unadjusted women-minus-men gap.
  fit <- lm(log_program_income_full_clp_m1 ~ female, data = x, x = TRUE, y = TRUE)
  X <- fit$x; e <- residuals(fit); N <- nrow(X); K <- ncol(X)
  bread <- solve(crossprod(X))
  V <- N / (N - K) * bread %*% crossprod(X * as.numeric(e)) %*% bread
  gap <- unname(coef(fit)["female"]); se <- sqrt(V["female", "female"])
  cval <- qnorm(.975)
  gaps[[sample_name]] <- data.table(sample = sample_name, N = N,
    gap_women_minus_men_log = gap, std_error_hc1 = se,
    p_value = 2 * pnorm(-abs(gap / se)), lower95_log = gap - cval * se, upper95_log = gap + cval * se,
    women_relative_to_men_pct = 100 * expm1(gap),
    lower95_pct = 100 * expm1(gap - cval * se), upper95_pct = 100 * expm1(gap + cval * se))
  women_mean <- mean(x[gender == "Women", log_program_income_full_clp_m1])
  men_mean <- mean(x[gender == "Men", log_program_income_full_clp_m1])
  check(paste(sample_name, "regression gap equals the difference in group means"), abs(gap - (women_mean - men_mean)) < 1e-10)
  check(paste(sample_name, "has finite regression-derived robust uncertainty"), is.finite(se) && se > 0)
}
means <- rbindlist(means); gaps <- rbindlist(gaps)
setcolorder(means, c("sample", "gender", setdiff(names(means), c("sample", "gender"))))
saveRDS(d, file.path(clean_dir, "analysis_data.rds"))
fwrite(means, file.path(clean_dir, "means.csv"))
fwrite(gaps, file.path(clean_dir, "gender_gaps.csv"))
table <- merge(dcast(means, sample ~ gender, value.var = c("N", "mean_log_income")), gaps, by = "sample")
table <- table[match(c("with_imputation", "enrolled_only"), sample)]
fwrite(table, file.path(table_dir, "projected_income_by_gender.csv"))
fwrite(checks, file.path(clean_dir, "verification.csv"))

labels <- c(with_imputation = "All students, including non-enrollee income floor", enrolled_only = "HE enrollees only")
rows <- vapply(seq_len(nrow(table)), function(i) {
  x <- table[i]
  sprintf('<tr><th>%s</th><td>%.4f</td><td>%.4f</td><td>%.4f</td><td>%.2f%%</td><td>%s</td></tr>',
    labels[x$sample], x$mean_log_income_Men, x$mean_log_income_Women, x$gap_women_minus_men_log,
    x$women_relative_to_men_pct, format(x$N, big.mark = ",", trim = TRUE))
}, character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Projected income by gender</title><style>body{max-width:1000px;margin:38px auto;padding:0 22px;font:16px/1.55 system-ui;color:#243143}h1{font-size:27px}table{border-collapse:collapse;width:100%;margin:24px 0}th,td{padding:12px;border-bottom:1px solid #dbe0e6;text-align:right}th:first-child{text-align:left}thead th{background:#f3f6f9}.note{font-size:14px;color:#526171}</style></head><body>',
  '<h1>Projected income by gender</h1><p>Unadjusted comparisons in the paper&rsquo;s broad panel: grade-8 cohorts 2017&ndash;2020, ages 12&ndash;16, with an observed attended high school.</p>',
  '<table><thead><tr><th>Sample</th><th>Men: mean log income</th><th>Women: mean log income</th><th>Women &minus; men</th><th>Relative gap</th><th>Students</th></tr></thead><tbody>',
  rows, '</tbody></table>',
  '<p class="note">The outcome is the master&rsquo;s log_program_income_full_clp_m1: natural log projected monthly income assigned to the first observed HE program. Non-enrollees receive the existing CLP 553,553 floor. Enrollees retain the paper&rsquo;s hierarchical MiFuturo program-income predictions in both rows.</p>',
  '<p class="note">The relative gap is 100 &times; [exp(mean log income of women &minus; mean log income of men) &minus; 1], comparing geometric means. These projections describe enrollment and program sorting; they do not measure observed individual wages or within-program gender wage differences.</p>',
  '<p class="note">The broad panel contains 931,547 students. Income is observed for all; one record with unknown gender is excluded, leaving 931,546 students, of whom 625,616 have observed HE enrollment. The descriptive comparison imposes no exam-taking or baseline-control restriction.</p>',
  '<p><a href="../../tables/projected_income_gender/projected_income_by_gender.csv">Full table and robust confidence intervals (CSV)</a></p></body></html>')
writeLines(html, file.path(report_dir, "projected_income_by_gender.html"), useBytes = TRUE)
message("Finished: ", nrow(checks), " checks passed.")
print(means)
print(gaps)
