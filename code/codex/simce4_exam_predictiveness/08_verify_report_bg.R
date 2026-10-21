# Independently verify the saved score decomposition and report the two OLS models.
# Run after 07_construct_regress_bg.R. This script never changes the source data.
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
clean_dir <- file.path(repo_wd, "data/clean/simce4_exam_predictiveness/bg_decomposition")
report_dir <- file.path(repo_wd, "output/reports/simce4_exam_predictiveness")
table_dir <- file.path(repo_wd, "output/tables/simce_prediction")
required_files <- c("student_scores.rds", "coefficients.csv", "regression_summary.csv",
  "prediction_diagnostics.csv", "sample_flow.csv", "source_manifest.csv", "fold_diagnostics.csv",
  "second_stage_models.rds", "covariance_matrices.rds", "score_normalization.csv", "prediction_roster.rds")
missing_files <- required_files[!file.exists(file.path(clean_dir, required_files))]
if (length(missing_files)) stop("Run the construction script first. Missing: ", paste(missing_files, collapse = ", "))

checks <- list()
check <- function(label, condition, detail = "") {
  ok <- isTRUE(condition)
  checks[[length(checks) + 1L]] <<- data.table(check = label, passed = ok, detail = as.character(detail))
  if (!ok) {
    fwrite(rbindlist(checks), file.path(clean_dir, "verification_checks.csv"))
    stop(label, if (nzchar(detail)) paste0(": ", detail) else "", call. = FALSE)
  }
  message("PASS: ", label)
}
same_num <- function(x, y, tolerance = 1e-9) {
  length(x) == length(y) && all(is.finite(x)) && all(is.finite(y)) &&
    all(abs(as.numeric(x) - as.numeric(y)) <= tolerance)
}

d <- as.data.table(readRDS(file.path(clean_dir, "student_scores.rds")))
coefs <- fread(file.path(clean_dir, "coefficients.csv"))
summary <- fread(file.path(clean_dir, "regression_summary.csv"))
prediction <- fread(file.path(clean_dir, "prediction_diagnostics.csv"), colClasses = list(character = "cohort_gr8"))
flow <- fread(file.path(clean_dir, "sample_flow.csv"))
manifest <- fread(file.path(clean_dir, "source_manifest.csv"))
fold_diagnostics <- fread(file.path(clean_dir, "fold_diagnostics.csv"))
normalization <- fread(file.path(clean_dir, "score_normalization.csv"))
saved_models <- readRDS(file.path(clean_dir, "second_stage_models.rds"))
saved_covariance <- readRDS(file.path(clean_dir, "covariance_matrices.rds"))
needed <- c("mrun", "cohort_gr8", "psu_year", "most_time_rbd_middle", "fold", "actual_score",
  "b_i", "g_i", "he_applied", "premium_field", "timely_exam_taker", "observed_simce8", "year_1st_app")
check("Saved score data contain all required variables", all(needed %in% names(d)))
check("The common second-stage sample has one row per student", nrow(d) > 0L && !anyNA(d$mrun) && !anyDuplicated(d$mrun))
check("Both SIMCE8 cohorts enter the sample", setequal(unique(d$cohort_gr8), c(2017L, 2019L)))
check("Entrance exams are observed in the expected admission year", all(d$psu_year == d$cohort_gr8 + 5L) && all(d$timely_exam_taker == TRUE))
check("Every retained student has observed grade-8 SIMCE", all(d$observed_simce8 == TRUE))
check("All score components, outcomes and school clusters are observed", all(complete.cases(d[, .(actual_score, b_i, g_i,
  he_applied, premium_field, most_time_rbd_middle)])) && all(is.finite(as.matrix(d[, .(actual_score, b_i, g_i)]))))
check("The five folds are deterministic student-ID folds", setequal(d$fold, 1:5) &&
  all(d$fold == as.integer(abs(d$mrun) %% 5L) + 1L))
check("Actual score equals predicted score plus residual", same_num(d$actual_score, d$b_i + d$g_i, 1e-12))
check("Both second-stage outcomes are binary", all(d$he_applied %in% c(0L, 1L)) && all(d$premium_field %in% c(0L, 1L)))
check("HE application exactly reuses the master's first-application indicator", all(d$he_applied == as.integer(!is.na(d$year_1st_app))))
check("Premium-field enrollment exactly reuses the master's existing outcome", same_num(d$premium_field, d$high_paying_field_m1, 0))
check("Non-enrollees retain zero premium-field enrollment", all(d[is.na(COD_SIES_m1) |
  trimws(as.character(COD_SIES_m1)) %in% c("", "NA"), premium_field] == 0L))
check("Saved summaries have exactly the two requested outcomes", nrow(summary) == 2L && !anyDuplicated(summary$outcome) &&
  setequal(summary$outcome, c("he_applied", "premium_field")))
check("Coefficient rows are unique and contain only intercept, predicted score and residual", nrow(coefs) == 6L &&
  !anyDuplicated(coefs[, .(outcome, term)]) && setequal(coefs$outcome, c("he_applied", "premium_field")) &&
  setequal(coefs$term, c("(Intercept)", "b_i", "g_i")))
check("Both saved models and covariance matrices exist", all(c("he_applied", "premium_field") %in% names(saved_models)) &&
  all(c("he_applied", "premium_field") %in% names(saved_covariance)))
check("Source manifest, sample flow and fold diagnostics are nonempty", nrow(manifest) > 0L && nrow(flow) > 0L && nrow(fold_diagnostics) >= 5L)
roster <- as.data.table(readRDS(file.path(clean_dir, "prediction_roster.rds")))
check("Prediction roster preserves one row per baseline-complete student", !anyDuplicated(roster$mrun) &&
  nrow(roster) == flow[cohort_gr8 == "All" & grepl("^4\\.", stage), N])
eligible_roster <- roster[is.finite(b_i) & is.finite(g_i) & !is.na(premium_field)]
check("The final sample exactly retains finite predictions and observed existing field outcomes", identical(d$mrun, eligible_roster$mrun) &&
  same_num(d$premium_field, eligible_roster$high_paying_field_m1, 0) &&
  same_num(d$he_applied, as.integer(!is.na(eligible_roster$year_1st_app)), 0))
check("Missing cross-fitted predictions correspond exactly to unseen fixed-effect levels",
  all(!is.finite(roster$b_i) == roster$unseen_fixed_effect) &&
  sum(!is.finite(roster$b_i)) == sum(fold_diagnostics$N_missing_prediction))
check("Sample flow has unique cohort-stage records and correct final counts", !anyDuplicated(flow[, .(cohort_gr8, stage)]) &&
  flow[cohort_gr8 == "All" & grepl("^6\\.", stage), N] == nrow(d) &&
  all(vapply(c(2017L, 2019L), function(y) flow[cohort_gr8 == as.character(y) & grepl("^6\\.", stage), N] ==
    sum(d$cohort_gr8 == y), logical(1))))
rm(eligible_roster, roster)
if ("path" %in% names(manifest)) {
  check("Recorded source files still exist", all(file.exists(manifest$path)))
  if ("bytes" %in% names(manifest)) check("Recorded source file sizes remain unchanged",
    same_num(file.info(manifest$path)$size, manifest$bytes, 0))
}
check("One score normalization record is saved per expected admission year", nrow(normalization) == 2L &&
  !anyDuplicated(normalization$psu_year) && setequal(normalization$psu_year, c(2022L, 2024L)))
for (year in c(2022L, 2024L)) {
  z <- d[psu_year == year]
  n <- normalization[psu_year == year]
  math_z <- (z$math_max - n$math_mean) / n$math_sd
  verbal_z <- (z$leng_max - n$verbal_mean) / n$verbal_sd
  composite <- ((math_z + verbal_z) / 2 - n$composite_mean) / n$composite_sd
  check(paste(year, "actual score reproduces the saved within-year normalization"),
    all(c(n$math_sd, n$verbal_sd, n$composite_sd) > 0) && same_num(z$actual_score, composite, 1e-12))
}
check("Fold diagnostics cover all five held-out folds once", nrow(fold_diagnostics) == 5L &&
  !anyDuplicated(fold_diagnostics$fold) && setequal(fold_diagnostics$fold, 1:5) &&
  all(fold_diagnostics$N_test == fold_diagnostics$N_finite_prediction + fold_diagnostics$N_missing_prediction))
for (f in 1:5) {
  model_path <- file.path(clean_dir, "prediction_models", paste0("fold_", f, ".rds"))
  check(paste("Saved prediction model exists for fold", f), file.exists(model_path))
  prediction_fit <- readRDS(model_path)
  held_out <- d[fold == f][seq_len(min(50L, sum(d$fold == f)))]
  p <- as.numeric(predict(prediction_fit, newdata = held_out))
  check(paste("Saved fold", f, "model reproduces held-out baseline predictions"), same_num(p, held_out$b_i, 1e-10))
  rm(prediction_fit)
}

expected_prediction <- rbindlist(lapply(c("All", "2017", "2019"), function(y) {
  z <- if (y == "All") d else d[cohort_gr8 == as.integer(y)]
  data.table(cohort_gr8 = y, N = nrow(z),
    r_squared = 1 - sum((z$actual_score - z$b_i)^2) / sum((z$actual_score - mean(z$actual_score))^2),
    prediction_correlation = cor(z$actual_score, z$b_i))
}))
check("Prediction diagnostics cover the pooled and separate cohorts", nrow(prediction) == 3L &&
  !anyDuplicated(prediction$cohort_gr8) && setequal(prediction$cohort_gr8, expected_prediction$cohort_gr8))
for (y in expected_prediction$cohort_gr8) {
  expected <- expected_prediction[cohort_gr8 == y]
  reported <- prediction[cohort_gr8 == y]
  check(paste("Prediction diagnostics independently reproduce for", y), same_num(reported$N, expected$N, 0) &&
    same_num(reported$r_squared, expected$r_squared) && same_num(reported$prediction_correlation, expected$prediction_correlation))
}

# Recover the exact 3-by-3 cluster sandwich from scores, without using the
# covariance function or regression summary produced by the construction script.
X <- cbind(`(Intercept)` = 1, b_i = d$b_i, g_i = d$g_i)
cluster <- d$most_time_rbd_middle
G <- uniqueN(cluster)
N <- nrow(d)
K <- ncol(X)
check("The second-stage design is identified", G > 1L && N > K && qr(X)$rank == K)
bread <- solve(crossprod(X))
correction <- G / (G - 1) * (N - 1) / (N - K)
expected_terms <- colnames(X)
for (outcome_name in c("he_applied", "premium_field")) {
  y <- d[[outcome_name]]
  beta <- as.vector(bread %*% crossprod(X, y)); names(beta) <- expected_terms
  residual <- y - as.vector(X %*% beta)
  scores_by_school <- rowsum(X * residual, group = cluster, reorder = FALSE)
  V <- correction * bread %*% crossprod(scores_by_school) %*% bread
  dimnames(V) <- list(expected_terms, expected_terms)
  se <- sqrt(diag(V))
  p_value <- 2 * pt(abs(beta / se), df = G - 1L, lower.tail = FALSE)
  critical <- qt(.975, df = G - 1L)
  reported <- coefs[outcome == outcome_name][match(expected_terms, term)]
  model <- saved_models[[outcome_name]]
  stored_v <- saved_covariance[[outcome_name]][expected_terms, expected_terms, drop = FALSE]
  s <- summary[outcome == outcome_name]
  r2 <- 1 - sum(residual^2) / sum((y - mean(y))^2)
  difference <- beta["b_i"] - beta["g_i"]
  difference_variance <- V["b_i", "b_i"] + V["g_i", "g_i"] - 2 * V["b_i", "g_i"]
  difference_p <- 2 * pt(abs(difference / sqrt(difference_variance)), df = G - 1L, lower.tail = FALSE)
  check(paste(outcome_name, "saved regression is precisely outcome ~ b_i + g_i"), inherits(model, "lm") &&
    identical(names(coef(model)), expected_terms) && same_num(coef(model), beta, 1e-8) && nobs(model) == N)
  check(paste(outcome_name, "coefficients reproduce from the common student sample"), same_num(reported$estimate, beta, 1e-8))
  check(paste(outcome_name, "middle-school clustered covariance independently reproduces"), same_num(stored_v, V, 1e-10) &&
    all(is.finite(V)) && min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) > 0)
  check(paste(outcome_name, "SEs, p-values and 95% intervals use t(G-1)"), same_num(reported$std_error, se, 1e-9) &&
    same_num(reported$p_value, p_value, 1e-8) && same_num(reported$lower95, beta - critical * se, 1e-8) &&
    same_num(reported$upper95, beta + critical * se, 1e-8))
  check(paste(outcome_name, "N, school clusters, mean, R-squared and coefficient-equality test reproduce"),
    s$N == N && s$middle_schools == G && same_num(s$mean_outcome, mean(y)) &&
    same_num(s$r_squared, r2) && same_num(s$b_equals_g_p, difference_p, 1e-8))
}

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
outcome_labels <- c(he_applied = "Centralized HE application", premium_field = "High-premium field enrollment")
display_table <- copy(coefs)
display_table[, outcome_label := unname(outcome_labels[outcome])]
display_table[, `:=`(estimate_pp = 100 * estimate, std_error_pp = 100 * std_error,
  lower95_pp = 100 * lower95, upper95_pp = 100 * upper95)]
display_table <- merge(display_table, summary, by = "outcome", all.x = TRUE, sort = FALSE)
fwrite(display_table, file.path(table_dir, "predicted_score_and_gap.csv"))

escape_html <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
fmt_n <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digits = 1L) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_p <- function(x) if (x < .001) "&lt;0.001" else sprintf("%.3f", x)
outcomes <- names(outcome_labels)
row_html <- function(label, values, class = "") paste0('<tr class="', class, '"><th scope="row">', label,
  '</th>', paste0("<td>", values, "</td>", collapse = ""), "</tr>")
coefficient_cell <- function(outcome_name, term_name) {
  x <- coefs[outcome == outcome_name & term == term_name]
  sprintf('%.2f<span class="se">(%.2f)</span>', 100 * x$estimate, 100 * x$std_error)
}
summary_values <- function(column) vapply(outcomes, function(y) summary[outcome == y][[column]], numeric(1))
table_rows <- c(
  row_html("Predicted score, <i>b</i><sub>i</sub>", vapply(outcomes, coefficient_cell, character(1), term_name = "b_i")),
  row_html("Score residual, <i>g</i><sub>i</sub>", vapply(outcomes, coefficient_cell, character(1), term_name = "g_i")),
  row_html("Students", fmt_n(summary_values("N")), "section"),
  row_html("Middle-school clusters", fmt_n(summary_values("middle_schools"))),
  row_html("Outcome mean", fmt_pct(summary_values("mean_outcome"))),
  row_html("Regression R<sup>2</sup>", sprintf("%.3f", summary_values("r_squared"))),
  row_html("<i>p</i>: coefficient on <i>b</i> equals coefficient on <i>g</i>",
    vapply(summary_values("b_equals_g_p"), fmt_p, character(1)))
)
pooled <- prediction[cohort_gr8 == "All"]
by_cohort <- prediction[cohort_gr8 != "All"]
setorder(by_cohort, cohort_gr8)
prediction_text <- paste0("Cross-fitted score prediction explains <strong>", fmt_pct(pooled$r_squared),
  "</strong> of observed score variation overall (", paste0(by_cohort$cohort_gr8, ": ", fmt_pct(by_cohort$r_squared), collapse = "; "), ").")
application_years <- sort(unique(d$year_1st_app[!is.na(d$year_1st_app)]))
application_year_text <- if (length(application_years)) paste0(" Observed first applications in this sample span ",
  min(application_years), "&ndash;", max(application_years), ".") else ""
sample_text <- paste0("Of ", fmt_n(flow[cohort_gr8 == "All" & grepl("^2\\.", stage), N]),
  " timely exam takers, ", fmt_n(flow[cohort_gr8 == "All" & grepl("^3\\.", stage), N]),
  " have both grade-8 SIMCE scores and ", fmt_n(flow[cohort_gr8 == "All" & grepl("^4\\.", stage), N]),
  " have complete baseline controls. Cross-fitting excludes ", fmt_n(sum(fold_diagnostics$N_missing_prediction)),
  " students whose held-out fixed-effect levels are absent from training; no further students are lost to missing HE outcomes.")
flow_headers <- paste0("<th>", escape_html(names(flow)), "</th>", collapse = "")
flow_rows <- vapply(seq_len(nrow(flow)), function(i) paste0("<tr>", paste0("<td>",
  escape_html(unlist(flow[i], use.names = FALSE)), "</td>", collapse = ""), "</tr>"), character(1))
html <- c('<!doctype html>', '<html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>Predicted entrance-exam score and score residual</title>',
  '<style>body{max-width:960px;margin:42px auto;padding:0 24px;color:#20242a;font:16px/1.55 system-ui,sans-serif}h1{font-size:27px;line-height:1.25;margin-bottom:12px}p{margin:14px 0}.subtitle,.note{color:#525c66}.subtitle{font-size:15px}table{width:100%;border-collapse:collapse;margin:24px 0}th,td{padding:12px 15px;border-bottom:1px solid #e0e4e8}thead th{border-bottom:2px solid #687788;text-align:right}thead th:first-child,tbody th{text-align:left}tbody th{font-weight:500}td{text-align:right;font-variant-numeric:tabular-nums}.se{display:block;font-size:14px;color:#596473;margin-top:3px}.section th,.section td{border-top:2px solid #b4bdc6}.note{font-size:14px}details{margin:25px 0}summary{cursor:pointer;font-weight:600}.flow{font-size:12px;overflow:auto}.flow th,.flow td{padding:7px}.flow th{text-align:left}a{color:#265dad}.pass{color:#276339}</style>',
  '</head><body>', '<h1>Predicted entrance-exam score and score residual</h1>',
  paste0('<p class="subtitle">Grade-8 cohorts 2017 and 2019 &middot; ', fmt_n(N), ' exam takers &middot; Five-fold cross-fitted predictions</p>'),
  '<p>The observed math&ndash;verbal entrance-exam composite is split into its baseline prediction, <i>b</i><sub>i</sub>, and the residual, <i>g</i><sub>i</sub> = actual score &minus; <i>b</i><sub>i</sub>. Each outcome is regressed jointly on these two components and an intercept.</p>',
  paste0('<p>', prediction_text, '</p>'),
  '<table aria-label="Outcome regressions on predicted entrance-exam score and score residual"><thead><tr><th></th>',
  paste0('<th scope="col">', outcome_labels, '</th>', collapse = ""), '</tr></thead><tbody>', table_rows, '</tbody></table>',
  '<p class="note"><strong>Units:</strong> percentage-point changes for a one-unit increase in the relevant score component. Both components retain the units of the actual score: one within-admission-year standard deviation of the math&ndash;verbal composite. They are not standardized separately. Parentheses contain middle-school clustered standard errors; inference is conditional on the cross-fitted predictions.</p>',
  '<p class="note"><strong>Prediction:</strong> fifths of students are held out in turn. Predictors include cubic grade-4 and grade-8 SIMCE math and reading scores, parental background, middle-school GPA and attendance, and cohort, sex, age, residence-comuna, middle-years-observed and middle-school fixed effects. No high-school identifiers enter the prediction. Actual entrance scores refer to timely admission years 2022 (PDT) and 2024 (PAES), standardized within admission year.</p>',
  paste0('<p class="note"><strong>Outcomes:</strong> centralized HE application is the master data&rsquo;s regular-application indicator, observed in 2021&ndash;2026 and derived from its first-application-year variable; it is neither financial-aid application nor application to every IP/CFT.',
    application_year_text, ' High-premium field is the existing indicator at first observed higher-education enrollment in 2022&ndash;2025, with non-enrollment coded zero. Follow-up differs across cohorts. Both regressions use the same students, all with observed SIMCE8 and timely entrance-exam scores.</p>'),
  paste0('<p class="note"><strong>Coverage:</strong> ', sample_text, '</p>'),
  '<p class="note"><strong>Interpretation:</strong> these are conditional associations. The residual is everything in the actual score left unexplained by measured baseline predictors; it is not a causal estimate of a high school&rsquo;s contribution. The second-stage specification contains only an intercept, <i>b</i><sub>i</sub> and <i>g</i><sub>i</sub>.</p>',
  '<details><summary>Sample accounting and saved results</summary><div class="flow"><table><thead><tr>', flow_headers,
  '</tr></thead><tbody>', flow_rows, '</tbody></table></div>',
  '<p><a href="../../tables/simce_prediction/predicted_score_and_gap.csv">Regression estimates and confidence intervals (CSV)</a></p>',
  '</details>',
  paste0('<p class="note pass">Independent verification: ', length(checks) + 1L, ' checks passed, including exact score decomposition and reconstruction of the clustered covariance matrices.</p>'),
  '</body></html>')
report_path <- file.path(report_dir, "predicted_score_and_gap.html")
writeLines(html, report_path, useBytes = TRUE)
check("The report and companion table are saved", file.exists(report_path) &&
  file.exists(file.path(table_dir, "predicted_score_and_gap.csv")))
fwrite(rbindlist(checks), file.path(clean_dir, "verification_checks.csv"))
message("Done: ", report_path)
