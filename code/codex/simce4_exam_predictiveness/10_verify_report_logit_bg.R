# Verify and report logistic regressions using the existing cross-fitted scores.
# Run after 09_fit_logit_bg.R. The saved baseline predictions are never refitted.
suppressPackageStartupMessages(library(data.table))
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
score_dir <- file.path(repo_wd, "data/clean/simce4_exam_predictiveness/bg_decomposition")
clean_dir <- file.path(score_dir, "logit")
report_dir <- file.path(repo_wd, "output/reports/simce4_exam_predictiveness")
table_dir <- file.path(repo_wd, "output/tables/simce_prediction")
required <- c("models.rds", "covariance_matrices.rds", "coefficients.csv", "marginal_effects.csv",
  "marginal_covariance_matrices.rds", "model_summary.csv", "source_preservation.csv")
missing <- required[!file.exists(file.path(clean_dir, required))]
if (length(missing)) stop("Run 09_fit_logit_bg.R first. Missing: ", paste(missing, collapse = ", "))
checks <- list()
check <- function(label, condition) {
  ok <- isTRUE(condition)
  checks[[length(checks) + 1L]] <<- data.table(check = label, passed = ok)
  if (!ok) {
    fwrite(rbindlist(checks), file.path(clean_dir, "verification_checks.csv"))
    stop(label, call. = FALSE)
  }
  message("PASS: ", label)
}
same_num <- function(x, y, tolerance = 1e-9) length(x) == length(y) &&
  all(is.finite(x)) && all(is.finite(y)) && all(abs(as.numeric(x) - as.numeric(y)) <= tolerance)

d <- as.data.table(readRDS(file.path(score_dir, "student_scores.rds")))
models <- readRDS(file.path(clean_dir, "models.rds"))
covariances <- readRDS(file.path(clean_dir, "covariance_matrices.rds"))
ame_covariances <- readRDS(file.path(clean_dir, "marginal_covariance_matrices.rds"))
coefs <- fread(file.path(clean_dir, "coefficients.csv"))
ames <- fread(file.path(clean_dir, "marginal_effects.csv"))
model_summary <- fread(file.path(clean_dir, "model_summary.csv"))
preservation <- fread(file.path(clean_dir, "source_preservation.csv"))
prediction <- fread(file.path(score_dir, "prediction_diagnostics.csv"), colClasses = list(character = "cohort_gr8"))
flow <- fread(file.path(score_dir, "sample_flow.csv"))
fold_diagnostics <- fread(file.path(score_dir, "fold_diagnostics.csv"))
outcomes <- c("he_applied", "premium_field")
terms <- c("(Intercept)", "b_i", "g_i")
effect_terms <- c("b_i", "g_i")
check("The existing common sample has one row per student", nrow(d) == 233787L && !anyDuplicated(d$mrun) && !anyNA(d$mrun))
check("Both grade-8 cohorts and timely entrance-exam years remain unchanged", setequal(d$cohort_gr8, c(2017L, 2019L)) &&
  all(d$psu_year == d$cohort_gr8 + 5L) && all(d$timely_exam_taker == 1L) && all(d$observed_simce8 == 1L))
check("Predicted and residual components preserve the original score units", same_num(d$actual_score, d$b_i + d$g_i, 1e-12))
check("Both binary outcomes retain the master definitions", all(d$he_applied %in% c(0L, 1L)) &&
  all(d$premium_field %in% c(0L, 1L)) && all(d$he_applied == as.integer(!is.na(d$year_1st_app))) &&
  same_num(d$premium_field, d$high_paying_field_m1, 0))
check("Saved results contain exactly the two requested logistic models", setequal(names(models), outcomes) &&
  setequal(names(covariances), outcomes) && setequal(names(ame_covariances), outcomes) &&
  nrow(model_summary) == 2L && !anyDuplicated(model_summary$outcome) && setequal(model_summary$outcome, outcomes))
check("Saved logit coefficients and marginal effects have unique complete keys", nrow(coefs) == 6L &&
  !anyDuplicated(coefs[, .(outcome, term)]) && setequal(coefs$outcome, outcomes) && setequal(coefs$term, terms) &&
  nrow(ames) == 4L && !anyDuplicated(ames[, .(outcome, term)]) &&
  setequal(ames$outcome, outcomes) && setequal(ames$term, effect_terms))
check("Every preserved input and previous result retains its hash", nrow(preservation) > 0L &&
  !anyDuplicated(preservation$path) && all(file.exists(preservation$path)) &&
  identical(preservation$md5_before, preservation$md5_after) &&
  identical(unname(tools::md5sum(preservation$path)), preservation$md5_before))
score_path <- normalizePath(file.path(score_dir, "student_scores.rds"), winslash = "/")
check("The unchanged-hash manifest includes the original score dataset", score_path %in%
  normalizePath(preservation$path, winslash = "/"))

X <- cbind(`(Intercept)` = 1, b_i = d$b_i, g_i = d$g_i)
N <- nrow(X); K <- ncol(X); G <- uniqueN(d$most_time_rbd_middle)
critical <- qt(.975, df = G - 1L)
small_sample <- G / (G - 1) * (N - 1) / (N - K)
check("The logistic design is identified and all regressors are observed", qr(X)$rank == K && G > 1L && all(is.finite(X)))
log_one_plus_exp <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
reconstructed <- list()
for (outcome_name in outcomes) {
  fit <- models[[outcome_name]]
  y <- d[[outcome_name]]
  beta <- coef(fit)[terms]
  eta <- as.vector(X %*% beta)
  p <- plogis(eta)
  w <- p * (1 - p)
  information <- crossprod(X * sqrt(w))
  bread <- solve(information)
  scores <- rowsum(X * (y - p), d$most_time_rbd_middle, reorder = FALSE)
  V <- small_sample * bread %*% crossprod(scores) %*% bread
  dimnames(V) <- list(terms, terms)
  se <- sqrt(diag(V))
  p_value <- 2 * pt(abs(beta / se), df = G - 1L, lower.tail = FALSE)
  ll <- sum(y * eta - log_one_plus_exp(eta))
  null_p <- mean(y)
  null_ll <- sum(y * log(null_p) + (1 - y) * log1p(-null_p))
  summary_row <- model_summary[outcome == outcome_name]
  reported <- coefs[outcome == outcome_name][match(terms, term)]
  stored_v <- covariances[[outcome_name]][terms, terms, drop = FALSE]
  check(paste(outcome_name, "is a converged binomial-logit model of only b and g"), inherits(fit, "glm") &&
    fit$family$family == "binomial" && fit$family$link == "logit" && isTRUE(fit$converged) &&
    identical(names(coef(fit)), terms) && nobs(fit) == N &&
    same_num(fit$y, y, 0) && same_num(fit$prior.weights, rep(1, N), 0))
  check(paste(outcome_name, "fitted probabilities and score equation reproduce"), same_num(fitted(fit), p, 1e-10) &&
    all(p > 0 & p < 1) && max(abs(colSums(X * (y - p)))) < 1e-4)
  check(paste(outcome_name, "likelihood, null likelihood and McFadden R-squared reproduce"),
    same_num(summary_row$log_likelihood, ll, 1e-7) && same_num(as.numeric(logLik(fit)), ll, 1e-7) &&
    same_num(summary_row$null_log_likelihood, null_ll, 1e-7) &&
    same_num(summary_row$mcfadden_r2, 1 - ll / null_ll, 1e-10))
  check(paste(outcome_name, "middle-school cluster sandwich reproduces"),
    same_num(stored_v, V, 1e-10) && min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) > 0)
  check(paste(outcome_name, "coefficients, odds ratios and t(G-1) inference reproduce"), same_num(reported$estimate, beta) &&
    same_num(reported$odds_ratio, exp(beta)) && same_num(reported$std_error, se) &&
    same_num(reported$p_value, p_value, 1e-8) && same_num(reported$lower95, beta - critical * se) &&
    same_num(reported$upper95, beta + critical * se))

  average_weight <- mean(w)
  ame <- beta[effect_terms] * average_weight
  derivative_s <- colMeans(X * (w * (1 - 2 * p)))
  gradient <- t(vapply(effect_terms, function(term_name) {
    unit <- as.numeric(terms == term_name)
    average_weight * unit + beta[term_name] * derivative_s
  }, numeric(K)))
  dimnames(gradient) <- list(effect_terms, terms)
  marginal_V <- gradient %*% V %*% t(gradient)
  marginal_se <- sqrt(diag(marginal_V))
  marginal_p <- 2 * pt(abs(ame / marginal_se), df = G - 1L, lower.tail = FALSE)
  ame_function <- function(b) {
    prob <- plogis(as.vector(X %*% b))
    b[effect_terms] * mean(prob * (1 - prob))
  }
  finite_difference <- vapply(seq_along(beta), function(k) {
    step <- rep(0, K); step[k] <- 1e-6
    (ame_function(beta + step) - ame_function(beta - step)) / 2e-6
  }, numeric(length(effect_terms)))
  reported_ame <- ames[outcome == outcome_name][match(effect_terms, term)]
  stored_ame_V <- ame_covariances[[outcome_name]][effect_terms, effect_terms, drop = FALSE]
  check(paste(outcome_name, "analytic marginal-effect gradient matches finite differences"), same_num(gradient, finite_difference, 1e-8))
  check(paste(outcome_name, "average marginal effects and full delta-method covariance reproduce"),
    same_num(reported_ame$estimate, ame) && same_num(stored_ame_V, marginal_V, 1e-10) &&
    min(eigen(marginal_V, symmetric = TRUE, only.values = TRUE)$values) > 0)
  check(paste(outcome_name, "marginal-effect SEs and t(G-1) inference reproduce"), same_num(reported_ame$std_error, marginal_se) &&
    same_num(reported_ame$p_value, marginal_p, 1e-8) &&
    same_num(reported_ame$lower95, ame - critical * marginal_se) &&
    same_num(reported_ame$upper95, ame + critical * marginal_se))
  contrast <- c(0, 1, -1)
  difference_se <- sqrt(as.numeric(t(contrast) %*% V %*% contrast))
  beta_equal_p <- 2 * pt(abs((beta["b_i"] - beta["g_i"]) / difference_se), df = G - 1L, lower.tail = FALSE)
  ame_difference_se <- sqrt(marginal_V[1, 1] + marginal_V[2, 2] - 2 * marginal_V[1, 2])
  ame_equal_p <- 2 * pt(abs((ame[1] - ame[2]) / ame_difference_se), df = G - 1L, lower.tail = FALSE)
  check(paste(outcome_name, "common sample, cluster count, means, iterations and equality tests reproduce"),
    summary_row$N == N && summary_row$middle_schools == G && same_num(summary_row$mean_outcome, mean(y)) &&
    summary_row$iterations == fit$iter && same_num(summary_row$b_equals_g_p, beta_equal_p, 1e-8) &&
    same_num(summary_row$ame_b_equals_g_p, ame_equal_p, 1e-8))
  reconstructed[[outcome_name]] <- list(ame = ame, covariance = marginal_V)
}

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
labels <- c(he_applied = "Centralized HE application", premium_field = "High-premium field enrollment")
table_data <- merge(copy(ames), model_summary, by = "outcome", all.x = TRUE, sort = FALSE)
table_data[, outcome_label := unname(labels[outcome])]
table_data[, `:=`(estimate_pp = estimate * 100, std_error_pp = std_error * 100,
  lower95_pp = lower95 * 100, upper95_pp = upper95 * 100)]
fwrite(table_data, file.path(table_dir, "predicted_score_and_gap.csv"))
coefficient_table <- merge(copy(coefs), model_summary, by = "outcome", all.x = TRUE, sort = FALSE)
coefficient_table[, outcome_label := unname(labels[outcome])]
fwrite(coefficient_table, file.path(table_dir, "predicted_score_and_gap_logit_coefficients.csv"))

escape_html <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
fmt_n <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digits = 1L) paste0(formatC(100 * x, format = "f", digits = digits), "%")
fmt_p <- function(x) if (x < .001) "&lt;0.001" else sprintf("%.3f", x)
row_html <- function(label, values, class = "") paste0('<tr class="', class, '"><th scope="row">', label,
  '</th>', paste0("<td>", values, "</td>", collapse = ""), "</tr>")
effect_cell <- function(outcome_name, term_name) {
  x <- ames[outcome == outcome_name & term == term_name]
  sprintf('%.2f<span class="se">(%.2f)</span>', 100 * x$estimate, 100 * x$std_error)
}
summary_values <- function(column) vapply(outcomes, function(y) model_summary[outcome == y][[column]], numeric(1))
rows <- c(
  row_html("Predicted score, <i>b</i><sub>i</sub>", vapply(outcomes, effect_cell, character(1), term_name = "b_i")),
  row_html("Score residual, <i>g</i><sub>i</sub>", vapply(outcomes, effect_cell, character(1), term_name = "g_i")),
  row_html("Students", fmt_n(summary_values("N")), "section"),
  row_html("Middle-school clusters", fmt_n(summary_values("middle_schools"))),
  row_html("Outcome mean", fmt_pct(summary_values("mean_outcome"))),
  row_html("McFadden pseudo-R<sup>2</sup>", sprintf("%.3f", summary_values("mcfadden_r2"))),
  row_html("<i>p</i>: marginal effect of <i>b</i> equals marginal effect of <i>g</i>",
    vapply(summary_values("ame_b_equals_g_p"), fmt_p, character(1)))
)
raw_rows <- c(lapply(terms, function(term_name) row_html(if (term_name == "(Intercept)") "Intercept" else
  if (term_name == "b_i") "Predicted score, <i>b</i><sub>i</sub>" else "Score residual, <i>g</i><sub>i</sub>",
  vapply(outcomes, function(y) {
    z <- coefs[outcome == y & term == term_name]
    sprintf('%.3f<span class="se">(%.3f); OR %.3f</span>', z$estimate, z$std_error, z$odds_ratio)
  }, character(1)))), list(row_html("<i>p</i>: logit coefficient on <i>b</i> equals coefficient on <i>g</i>",
    vapply(summary_values("b_equals_g_p"), fmt_p, character(1)))))
pooled <- prediction[cohort_gr8 == "All"]
by_cohort <- prediction[cohort_gr8 != "All"]; setorder(by_cohort, cohort_gr8)
prediction_text <- paste0("The existing cross-fitted score prediction explains <strong>", fmt_pct(pooled$r_squared),
  "</strong> of observed score variation (", paste0(by_cohort$cohort_gr8, ": ", fmt_pct(by_cohort$r_squared), collapse = "; "),
  "). Baseline predictions and the student sample are unchanged from the linear regressions.")
ame_gaps <- vapply(outcomes, function(y) ames[outcome == y & term == "b_i", estimate] -
  ames[outcome == y & term == "g_i", estimate], numeric(1))
interpretation_text <- paste0("In the logistic specification, the predicted component has the larger association with both outcomes: average-marginal-effect differences of ",
  sprintf("%.2f", 100 * ame_gaps[1]), " percentage points for centralized HE application and ",
  sprintf("%.2f", 100 * ame_gaps[2]), " points for high-premium-field enrollment. The logit coefficient-equality tests give <i>p</i> ",
  fmt_p(model_summary[outcome == "he_applied", b_equals_g_p]), " and ",
  fmt_p(model_summary[outcome == "premium_field", b_equals_g_p]), ", respectively.")
flow_headers <- paste0("<th>", escape_html(names(flow)), "</th>", collapse = "")
flow_rows <- vapply(seq_len(nrow(flow)), function(i) paste0("<tr>", paste0("<td>",
  escape_html(unlist(flow[i], use.names = FALSE)), "</td>", collapse = ""), "</tr>"), character(1))
html <- c('<!doctype html>', '<html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>Logistic models: predicted entrance-exam score and residual</title>',
  '<style>body{max-width:960px;margin:42px auto;padding:0 24px;color:#20242a;font:16px/1.55 system-ui,sans-serif}h1{font-size:27px;line-height:1.25;margin-bottom:12px}p{margin:14px 0}.subtitle,.note{color:#525c66}.subtitle{font-size:15px}table{width:100%;border-collapse:collapse;margin:24px 0}th,td{padding:12px 15px;border-bottom:1px solid #e0e4e8}thead th{border-bottom:2px solid #687788;text-align:right}thead th:first-child,tbody th{text-align:left}tbody th{font-weight:500}td{text-align:right;font-variant-numeric:tabular-nums}.se{display:block;font-size:14px;color:#596473;margin-top:3px}.section th,.section td{border-top:2px solid #b4bdc6}.note{font-size:14px}details{margin:25px 0}summary{cursor:pointer;font-weight:600}.flow{font-size:12px;overflow:auto}.flow th,.flow td{padding:7px}.flow th{text-align:left}a{color:#265dad}.pass{color:#276339}</style>',
  '</head><body>', '<h1>Predicted entrance-exam score and residual: logistic models</h1>',
  paste0('<p class="subtitle">Grade-8 cohorts 2017 and 2019 &middot; ', fmt_n(N), ' exam takers &middot; Average marginal effects</p>'),
  '<p>The observed math&ndash;verbal entrance-exam composite is split into its baseline prediction, <i>b</i><sub>i</sub>, and its residual, <i>g</i><sub>i</sub> = actual score &minus; <i>b</i><sub>i</sub>. Each binary outcome is estimated using a logit with an intercept and these two score components.</p>',
  paste0('<p>', prediction_text, '</p>'),
  '<table aria-label="Logistic average marginal effects"><thead><tr><th></th>',
  paste0('<th scope="col">', labels, '</th>', collapse = ""), '</tr></thead><tbody>', rows, '</tbody></table>',
  paste0('<p>', interpretation_text, '</p>'),
  '<p class="note"><strong>Units:</strong> average marginal effects in percentage points for a one-unit increase in the indicated component, holding the other component fixed. Both retain the units of the actual score: one within-admission-year standard deviation of the math&ndash;verbal composite. They are not standardized separately. Parentheses contain delta-method standard errors from the middle-school clustered covariance; inference uses <i>t</i> with clusters minus one degrees of freedom and is conditional on the existing cross-fitted predictions.</p>',
  '<p class="note"><strong>Prediction:</strong> five-fold predictions use cubic grade-4 and grade-8 SIMCE math and reading, parental background, middle-school GPA and attendance, and cohort, sex, age, residence-comuna, middle-years-observed and middle-school fixed effects. No high-school identifiers enter. Actual scores refer to timely admission years 2022 (PDT) and 2024 (PAES), standardized within admission year.</p>',
  '<p class="note"><strong>Outcomes:</strong> centralized HE application is the master data&rsquo;s regular-application indicator, observed in 2021&ndash;2026. High-premium field is the existing indicator at first observed HE enrollment in 2022&ndash;2025, with non-enrollment coded zero. Cohort follow-up differs. Both regressions use the same students.</p>',
  paste0('<p class="note"><strong>Coverage:</strong> ', fmt_n(flow[cohort_gr8 == "All" & grepl("^2\\.", stage), N]),
    ' timely exam takers; ', fmt_n(flow[cohort_gr8 == "All" & grepl("^3\\.", stage), N]),
    ' with both SIMCE8 scores; ', fmt_n(flow[cohort_gr8 == "All" & grepl("^4\\.", stage), N]),
    ' with complete baseline controls. The existing cross-fit excludes ', fmt_n(sum(fold_diagnostics$N_missing_prediction)),
    ' students with fixed-effect levels absent from training. No additional students are excluded by the logistic models.</p>'),
  '<p class="note"><strong>Interpretation:</strong> the estimates are conditional associations. The score residual includes all score variation unexplained by measured baseline predictors; it is not a causal estimate of high-school effectiveness. McFadden pseudo-R<sup>2</sup> is not a share of outcome variance explained.</p>',
  '<details><summary>Logit coefficients and odds ratios</summary><table><thead><tr><th></th>',
  paste0('<th scope="col">', labels, '</th>', collapse = ""), '</tr></thead><tbody>', unlist(raw_rows), '</tbody></table>',
  '<p class="note">Entries are logit coefficients, with clustered SEs in parentheses and odds ratios (OR) below. A one-unit increase has OR = exp(coefficient).</p>',
  '<p><a href="../../tables/simce_prediction/predicted_score_and_gap_logit_coefficients.csv">Logit coefficients and confidence intervals (CSV)</a></p></details>',
  '<details><summary>Sample accounting and saved results</summary><div class="flow"><table><thead><tr>', flow_headers,
  '</tr></thead><tbody>', flow_rows, '</tbody></table></div>',
  '<p><a href="../../tables/simce_prediction/predicted_score_and_gap.csv">Average marginal effects and confidence intervals (CSV)</a> &middot; ',
  '<a href="predicted_score_and_gap_lpm.html">Earlier linear-probability regressions</a></p></details>',
  paste0('<p class="note pass">Independent verification: ', length(checks) + 1L,
    ' checks passed, including unchanged source hashes, the likelihood, clustered covariance and marginal-effect derivatives.</p>'),
  '</body></html>')
report_path <- file.path(report_dir, "predicted_score_and_gap.html")
writeLines(trimws(html, which = "right"), report_path, useBytes = TRUE)
check("The logistic report and two companion tables are saved", file.exists(report_path) &&
  file.exists(file.path(table_dir, "predicted_score_and_gap.csv")) &&
  file.exists(file.path(table_dir, "predicted_score_and_gap_logit_coefficients.csv")))
fwrite(rbindlist(checks), file.path(clean_dir, "verification_checks.csv"))
message("Done: ", report_path)
