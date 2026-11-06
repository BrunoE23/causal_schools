suppressPackageStartupMessages({library(data.table); library(fixest)})
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "math_va_cross_effects_eb")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
setFixest_nthreads(2)
reference <- fread(file.path(out, "math_eb_va_cross_effects.csv"))
instrument <- "z_math_adj_eb"
expected <- "expected_math_adj_eb"
controls <- c("cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU")
specs <- data.table(va_key = c("math", "field", "institution"),
  outcome = c("z_year_math_max", "high_paying_field_m1", "high_inst_m1"),
  attended_va = c("d_math_adj_eb", "d_highpay_adj_eb", "d_highinst_adj_eb"),
  label = c("Math VA", "High-premium field VA", "High-premium institution VA"))
cols <- unique(c("mrun", "timely_sae", "any_risk", "sae_proceso", "most_time_RBD", "rbd_treated_1R",
  "admission_exam_taker", controls, instrument, expected, specs$attended_va, specs$outcome[-1L]))
# Select original main-IV columns only from the saved 138 MB frame.
d <- fread(file.path(repo, "data", "clean", "peer_lottery_variation", "main_regression_df_with_peer_shocks.csv"),
  select = cols, na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(nrow(d) == 162633L, !anyDuplicated(d$mrun), all(d$timely_sae == 1L), all(d$any_risk == 1L),
          all(d$sae_proceso %in% 2018:2020))

# Reproduce both preceding second-stage samples before changing the dependent
# variable from the student's binary outcome to the attended school's EB VA.
eligible <- lapply(reference$outcome, function(y) {
  needed <- c(y, "d_math_adj_eb", instrument, expected, controls, "admission_exam_taker")
  d$admission_exam_taker == 1L & complete.cases(d[, ..needed])
})
stopifnot(length(eligible) == 2L, identical(eligible[[1]], eligible[[2]]),
          all(vapply(eligible, sum, integer(1)) == reference$n_obs))
reg <- d[which(eligible[[1]])]
stopifnot(all(complete.cases(reg[, specs$attended_va, with = FALSE])))
fwrite(reg[, .(mrun)], file.path(out, "cross_kappa_student_sample.csv"))

# Validate every dependent variable against the current R/lfe shrunk VA source,
# and validate the offered-school math VA instrument against that same run.
va <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
  "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
audit <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]
  v <- va[analysis_sample == "All" & outcome == s$outcome]
  stopifnot(!anyDuplicated(v$school_rbd))
  attended <- v$controlled_value_added_eb_centered_student[match(reg$most_time_RBD, v$school_rbd)]
  gap <- max(abs(attended - reg[[s$attended_va]]))
  stopifnot(!anyNA(attended), gap < 1e-10)
  if (s$va_key == "math") {
    offered <- v$controlled_value_added_eb_centered_student[match(reg$rbd_treated_1R, v$school_rbd)]
    offered[is.na(reg$rbd_treated_1R) | reg$rbd_treated_1R == 0] <- 0
    stopifnot(!anyNA(offered), max(abs(offered - reg[[instrument]])) < 1e-10)
  }
  data.table(va_key = s$va_key, max_attended_va_gap = gap, n_students = nrow(reg))
}))
fwrite(audit, file.path(out, "cross_kappa_eb_source_validation.csv"))

rhs <- paste(c(instrument, expected, "factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to",
               "factor(GEN_ALU)", "factor(EDAD_ALU)"), collapse = " + ")
results <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]
  model <- feols(as.formula(paste0(s$attended_va, " ~ ", rhs)), data = reg, vcov = "hetero", notes = FALSE)
  k <- unname(coef(model)[instrument]); uncertainty <- unname(se(model)[instrument])
  stopifnot(nobs(model) == nrow(reg), is.finite(k), uncertainty > 0)
  data.table(attended_va_key = s$va_key, attended_va_label = s$label, dependent_variable = s$attended_va,
    offered_va_instrument = instrument, risk_control = expected, kappa = k, se = uncertainty,
    p_value = 2 * pnorm(-abs(k / uncertainty)), ci_low = k - 1.96 * uncertainty,
    ci_high = k + 1.96 * uncertainty, n_obs = nobs(model), wald_f = (k / uncertainty)^2,
    change_for_0_1_math_va_offer = 0.1 * k,
    dependent_units = if (s$va_key == "math") "Math-score SD" else "Probability units of school VA",
    vcov = "fixest hetero, default finite-sample correction")
}))
own <- results[attended_va_key == "math"]
stopifnot(all(abs(own$kappa - reference$first_stage_beta) < 1e-10),
          all(abs(own$se - reference$first_stage_se) < 1e-10), all(own$n_obs == reference$n_obs))
fwrite(results, file.path(out, "math_offer_cross_kappa_eb.csv"))
print(results)
