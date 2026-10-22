# Four-column report from the saved common-sample peer preference models.
source("code/codex/school_preference_va/four_specifications_common.R")
all_schools <- "--all-schools" %in% commandArgs(trailingOnly = TRUE)
scope <- if (all_schools) "peer_populations_all_schools" else "peer_populations"
out <- file.path(base_clean_dir, scope)
tables <- file.path(base_table_dir, scope)
dir.create(tables, recursive = TRUE, showWarnings = FALSE)
co <- fread(file.path(out, "coefficients.csv"))[auxiliary == FALSE]
ms <- fread(file.path(out, "model_summary.csv"))[auxiliary == FALSE]
blocks <- fread(file.path(out, "block_comparisons.csv"))
normalization <- fread(file.path(out, "peer_normalization.csv"))
verification <- fread(file.path(out, "verification.csv"))
check("All saved model checks passed", nrow(verification) > 0 && all(verification$passed))
order <- c("D", "D_applicants", "D_offers", "D_attendees")
headings <- c("D: same sample", "+ Applicant peers", "+ Offered peers", "+ Attendee peers")
expected <- if (all_schools) fread(file.path(out, "preflight_coverage.csv"))[2L] else data.table(rankings = 13037L, school_pairs = 36276L, schools = 99L)
check("Four main models use the identical approved sample", nrow(ms) == 4L && setequal(ms$model, order) &&
  all(ms$applicants == expected$rankings) && all(ms$school_pairs == expected$school_pairs) && all(ms$schools == expected$schools))
metrics <- c("parents_education", "simce_baseline", "female", "income_decile_imputed")
va_labels <- c(z_va_math = "Math VA", z_va_exam = "Exam-taking VA", z_va_highfield = "High-premium-field VA", z_va_highinst = "High-premium-institution VA")
peer_labels <- c(parents_education = "Peer parents' education", simce_baseline = "Peer baseline SIMCE", female = "Peer female share", income_decile_imputed = "Peer income decile")
rows <- data.table(term = c(va_terms, paste0("peer_", metrics), "log1p_distance_main", school_type_terms),
  label = c(unname(va_labels[va_terms]), unname(peer_labels[metrics]), "log(1 + distance in km)", "Mixed HC/TP", "TP only"),
  section = c(rep("Value added", 4L), rep("Peer population", 4L), rep("Distance and school type", 3L)))
observed_fees <- fee_specs[term %in% co$term]
rows <- rbind(rows, observed_fees[, .(term, label = paste(ifelse(field == "PAGO_MENSUAL", "Monthly", "Enrollment"), label), section = "Fees (reference: free)")])
export <- rbindlist(lapply(order, function(m) {
  z <- co[model == m]
  if (m != "D") {
    g <- sub("^D_", "", m)
    z[, term := sub(paste0("^peer_", g, "_"), "peer_", term)]
  }
  merge(rows, z, by = "term", all.x = TRUE, sort = FALSE)[, model := m]
}))
check("Every main coefficient is represented exactly once", export[is.finite(estimate), .N] == nrow(co))
fwrite(export, file.path(tables, "peer_populations_coefficients.csv"))
fwrite(ms, file.path(tables, "peer_populations_sample.csv"))
fwrite(blocks, file.path(tables, "peer_populations_block_comparison.csv"))

# Save collinearity diagnostics on the same 99 equally weighted destination schools.
d <- as.data.table(readRDS(file.path(out, "estimation_data.rds")))
school <- unique(d, by = "rbd")
correlations <- rbindlist(lapply(c("applicants", "offers", "attendees"), function(g) {
  vars <- c(va_terms, paste0("peer_", g, "_", metrics))
  cmat <- cor(as.matrix(school[, ..vars]))
  z <- as.data.table(as.table(cmat)); setnames(z, c("variable1", "variable2", "correlation"))
  z[, group := g]; z
}))
fwrite(correlations, file.path(out, "school_attribute_correlations.csv"))
parent_income_cor <- correlations[grepl("parents_education$", variable1) & grepl("income_decile_imputed$", variable2), correlation]

fmt <- function(x) formatC(x, format = "f", digits = 3L, big.mark = ",")
fmt_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)
fmt_p <- function(x) ifelse(x < .001, "&lt;0.001", sprintf("%.3f", x))
esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); gsub("<", "&lt;", x, fixed = TRUE) }
coefficient_cell <- function(m, t) {
  z <- export[model == m & term == t]
  if (!nrow(z) || !is.finite(z$estimate)) return("&mdash;")
  paste0(fmt(z$estimate), '<span class="se">(', fmt(z$std_error), ')</span>')
}
body <- vapply(seq_len(nrow(rows)), function(i) {
  section <- if (i == 1L || rows$section[i] != rows$section[i - 1L]) paste0('<tr class="section"><th colspan="5">', rows$section[i], '</th></tr>') else ""
  paste0(section, '<tr><th>', esc(rows$label[i]), '</th>', paste0('<td>', vapply(order, coefficient_cell, character(1), t = rows$term[i]), '</td>', collapse = ""), '</tr>')
}, character(1))
summary_row <- function(label, values) paste0('<tr><th>', label, '</th>', paste0('<td>', values, '</td>', collapse = ""), '</tr>')
model_index <- match(order, ms$model)
ms <- ms[model_index]
footer <- c(summary_row("Applicants / complete rankings", fmt_n(ms$applicants)), summary_row("Applicant-school pairs", fmt_n(ms$school_pairs)),
  summary_row("Distinct schools", fmt_n(ms$schools)), summary_row("Log likelihood", fmt(ms$log_likelihood)),
  summary_row("Joint peer-block p-value", c("&mdash;", fmt_p(blocks$peer_wald_p[match(c("applicants", "offers", "attendees"), blocks$group)]))),
  summary_row("Joint VA-block p-value", c("&mdash;", fmt_p(blocks$va_wald_p[match(c("applicants", "offers", "attendees"), blocks$group)]))))
comparison <- vapply(seq_len(nrow(blocks)), function(i) {
  z <- blocks[i]
  paste0('<tr><th>', c(applicants = "Applicant peers", offers = "Offered peers", attendees = "Attendee peers")[z$group], '</th><td>',
    sprintf("%.1f", z$loglik_gain_peers_given_VA), '</td><td>', sprintf("%.1f", z$loglik_gain_VA_given_peers), '</td></tr>')
}, character(1))
normalization_text <- paste(sprintf("%s: %.3f %s", unname(peer_labels[normalization$metric]), normalization$reference_sd,
  c(parents_education = "years", simce_baseline = "student z-score units", female = "share units", income_decile_imputed = "deciles")[normalization$metric]), collapse = "; ")
coverage_text <- if (all_schools) paste0('<p><b>Common sample:</b> ', fmt_n(expected$rankings), ' complete submitted rankings, ', fmt_n(expected$school_pairs),
  ' school alternatives and ', fmt_n(expected$schools), ' destination schools. Extending the peer measures to the whole observed school population retains ',
  sprintf('%.2f%%', 100 * expected$rankings / 273184), ' of original D rankings. All 2,481 original D destinations have complete attendee measures; 25 lack complete applicant measures and 12 lack complete offer measures. Exclude entire lists containing an incomplete school, leaving 5,247 rankings out (1.92%). D is re-estimated on exactly the same retained sample as the other columns.</p>') else
  '<p><b>Common sample:</b> 13,037 complete submitted rankings, 36,276 school alternatives and 99 destination schools. This is 4.8% of the original D sample: every listed school must belong to the supplied 100-school composition file. Lists are never shortened. D is re-estimated on this restricted sample; it is not the original full-sample coefficient column.</p>'
fee_note <- if (all_schools) 'All ten original D fee indicators are retained in every column.' else 'Five original fee indicators are identically zero here and are omitted from every column; every other D regressor is retained.'
peer_gains_larger <- all(blocks$loglik_gain_peers_given_VA > blocks$loglik_gain_VA_given_peers)
va_gains_larger <- all(blocks$loglik_gain_VA_given_peers > blocks$loglik_gain_peers_given_VA)
fit_interpretation <- if (peer_gains_larger) 'The four peer characteristics improve in-sample ranking fit more than the four VAs in all three versions.' else if (va_gains_larger)
  'The four VAs improve in-sample ranking fit more than the four peer characteristics in all three versions.' else 'The relative incremental fit of peers and VA varies with the peer population.'
peer_simce_positive <- all(co[grepl('^peer_.*_simce_baseline$', term), estimate] > 0)
results_text <- paste0('<p><b>Reading the results:</b> ', if (peer_simce_positive) 'Higher peer baseline SIMCE is positively associated with ranking a school earlier in all three versions. ' else '',
  fit_interpretation, ' These comparisons describe conditional associations; they do not establish causal willingness to pay for peers or VA.</p>')
if (all_schools) {
  check("All original fee controls remain in every full-school model", all(vapply(order, function(m) all(active_fee_terms %in% co[model == m, term]), logical(1))))
  get_b <- function(m, t) co[model == m & term == t, estimate]
  results_text <- c(results_text, paste0('<p>Adding attendee peers changes the math-VA coefficient from ', fmt(get_b('D', 'z_va_math')), ' to ',
    fmt(get_b('D_attendees', 'z_va_math')), ' and exam-taking VA from ', fmt(get_b('D', 'z_va_exam')), ' to ', fmt(get_b('D_attendees', 'z_va_exam')),
    '. Premium-field VA remains positive (', fmt(get_b('D_attendees', 'z_va_highfield')), '). The negative conditional income-decile coefficients do not imply that families dislike affluent peers: income, parent education and baseline achievement enter jointly and overlap strongly.</p>'))
}
cohort_note <- if (all_schools) 'The expanded measures reproduce all values of the original 100-school input. Available school-cohort means are equally weighted over 2018-2020; group-specific cohort counts and baseline-data coverage remain in the source tables. Missing school-group measures are not imputed.' else
  'Use the supplied mean_equal_cohort columns: equally weight available school-cohort means in 2018-2020. Attendees have all three cohorts; applicant and offer groups sometimes have fewer. Retain source cohort-count and missing-data coverage fields. There is no additional imputation and no reconstruction of peer populations.'
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>School preferences: value added and peers</title><style>body{font:16px/1.5 system-ui,sans-serif;color:#18283b;margin:30px}main{max-width:1150px;margin:auto}h1{font-size:27px}.intro{background:#f1f5f9;padding:13px 17px}.wrap{overflow:auto}table{border-collapse:collapse;width:100%;font-size:14px;margin:22px 0}th,td{padding:8px 12px;text-align:right;border-bottom:1px solid #e1e6ec;vertical-align:top}th:first-child{text-align:left}tbody th{font-weight:400}thead th{background:#eff3f8}.se{display:block;font-size:12px;color:#637389}.section th{background:#fafbfd;font-weight:650;border-bottom:1px solid #acb9c8;padding-top:15px}tfoot{font-weight:600}.note{font-size:13px;color:#526174}details{margin:24px 0}summary{cursor:pointer;font-weight:600}a{color:#245d9b}</style></head><body><main>',
  '<h1>School preferences: value added and peers</h1>',
  '<p class="intro"><b>D:</b> Distance + fees + four VAs + mixed HC/TP and TP-only indicators.<br>The other three columns add the same four peer characteristics, measured among probability-weighted applicants, first-round offer recipients, or all most-attended-school attendees.</p>',
  coverage_text,
  '<div class="wrap"><table><thead><tr><th>Attribute</th>', paste0('<th>', headings, '</th>', collapse = ""), '</tr></thead><tbody>', body,
  '</tbody><tfoot>', footer, '</tfoot></table></div>',
  paste0('<p class="note">Exact submitted-list rank-ordered logit, with student-clustered standard errors in parentheses. D contains the four existing EB VAs and no PAES mean or peer interactions. Free is the fee reference. ', fee_note, '</p>'),
  results_text,
  '<details><summary>Incremental fit of peers versus VA</summary><p class="note">Each block has four coefficients. Peer gains compare D + peers with D. VA gains compare D + peers with the same model omitting only its four VAs; three auxiliary nested fits are saved for this purpose. These are in-sample ranking-fit comparisons, not causal willingness-to-pay measures or out-of-sample predictions.</p>',
  '<table><thead><tr><th>Peer population</th><th>Gain from peers, given VA</th><th>Gain from VA, given peers</th></tr></thead><tbody>', comparison, '</tbody></table></details>',
  '<details><summary>Peer definitions, normalization and coverage</summary>',
  paste0('<p class="note">', cohort_note, '</p>'),
  '<p class="note">Applicant peers use simulated admission-probability weights. Offered peers include all recorded first-round offers, including guaranteed offers. Attendee peers include all master-universe students in grade-8 cohorts 2018-2020 whose most-attended HS is the destination, including non-applicants and without an age restriction. The strictly stochastic offer supplement is not used.</p>',
  paste0('<p class="note">All peer columns share the same normalization: equally weighted attendee-school means and SDs over the ', fmt_n(expected$schools), ' estimation destinations. A one-unit peer coefficient means ', esc(normalization_text), '. Existing VA normalization is unchanged from D, so raw coefficient magnitudes are not a standalone comparison of peer and VA importance.</p>'),
  paste0('<p class="note">Peer parent education and income decile are strongly correlated across schools (correlations ', sprintf("%.2f", min(parent_income_cor)), '&ndash;', sprintf("%.2f", max(parent_income_cor)), '). Individual peer coefficients condition on the other three peer measures and can have unstable signs. Peer composition comes from the same 2018-2020 populations and is potentially endogenous; these are conditional ranking associations.</p>'),
  '</details>',
  paste0('<p class="note"><a href="../../tables/school_preference_va/', scope, '/peer_populations_coefficients.csv">Coefficients and intervals (CSV)</a> &middot; <a href="../../tables/school_preference_va/', scope, '/peer_populations_sample.csv">Sample (CSV)</a> &middot; <a href="../../tables/school_preference_va/', scope, '/peer_populations_block_comparison.csv">Incremental fit (CSV)</a></p>'),
  '</main></body></html>')
path <- file.path(report_dir, if (all_schools) "with_peer_populations_all_schools.html" else "with_peer_populations.html")
writeLines(html, path, useBytes = TRUE)
check("Four requested headings are ordered correctly", grepl(paste0('<th>', headings, '</th>', collapse = ""), paste(html, collapse = ""), fixed = TRUE))
fwrite(checks, file.path(out, "report_verification.csv"))
message("Saved ", path)
