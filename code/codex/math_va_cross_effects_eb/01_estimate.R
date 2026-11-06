suppressPackageStartupMessages({library(data.table); library(fixest)})
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "math_va_cross_effects_eb")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
setFixest_nthreads(2)

# The 138 MB saved current main-IV frame includes left-joined peer diagnostics.
# Read original main-IV columns only; peer variables never enter this analysis.
input <- file.path(repo, "data", "clean", "peer_lottery_variation", "main_regression_df_with_peer_shocks.csv")
specs <- data.table(spec = c("math_adj_eb", "highpay_adj_eb", "highinst_adj_eb"),
  outcome = c("z_year_math_max", "high_paying_field_m1", "high_inst_m1"),
  label = c("Math", "High-premium field", "High-premium institution"))
specs[, `:=`(treatment = paste0("d_", spec), instrument = paste0("z_", spec), expected = paste0("expected_", spec))]
controls <- c("cohort_gr8", "z_sim_mat_4to", "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU")
needed <- unique(c("mrun", "student_id", "sae_proceso", "timely_sae", "any_risk", "admission_exam_taker",
  "most_time_RBD", "rbd_treated_1R", controls, specs$outcome, specs$treatment, specs$instrument, specs$expected))
d <- fread(input, select = needed, na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(nrow(d) == 162633L, !anyDuplicated(d$mrun),
  !anyDuplicated(d[, .(student_id, sae_proceso)]), all(d$timely_sae == 1L), all(d$any_risk == 1L),
  all(d$sae_proceso %in% 2018:2020))
for (o in specs$outcome[-1L]) stopifnot(all(d[[o]][!is.na(d[[o]])] %in% 0:1))

# Verify the precise EB run: current unified R/lfe All-sample school values.
# This is distinct from the older saved Stata EB files used for earlier simple
# correlations. Never import the raw or unshrunk column as a treatment.
va_path <- file.path(repo, "output", "intermediate", "empirical_bayes_school_va", "highered_2017_2020",
                     "eb_school_rbd_observational_values_all_outcomes.csv")
va <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
audit <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]
  v <- va[analysis_sample == "All" & outcome == s$outcome]
  stopifnot(!anyDuplicated(v$school_rbd))
  attended <- v$controlled_value_added_eb_centered_student[match(d$most_time_RBD, v$school_rbd)]
  offered <- v$controlled_value_added_eb_centered_student[match(d$rbd_treated_1R, v$school_rbd)]
  offered[is.na(d$rbd_treated_1R) | d$rbd_treated_1R == 0] <- 0
  gap_d <- max(abs(attended - d[[s$treatment]]), na.rm = TRUE)
  gap_z <- max(abs(offered - d[[s$instrument]]), na.rm = TRUE)
  stopifnot(identical(is.na(attended), is.na(d[[s$treatment]])),
    identical(is.na(offered), is.na(d[[s$instrument]])), gap_d < 1e-10, gap_z < 1e-10)
  data.table(spec = s$spec, max_attended_gap = gap_d, max_offered_gap = gap_z,
    school_value_column = "controlled_value_added_eb_centered_student", source = va_path)
}))
fwrite(audit, file.path(out, "eb_school_value_source_validation.csv"))

fit_iv <- function(y, label, s) {
  required <- c(y, s$treatment, s$instrument, s$expected, controls, "admission_exam_taker")
  reg <- d[admission_exam_taker == 1L & complete.cases(d[, ..required])]
  rhs <- paste(c("factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to",
    "factor(GEN_ALU)", "factor(EDAD_ALU)", s$expected), collapse = " + ")
  model <- feols(as.formula(paste0(y, " ~ ", rhs, " | 0 | ", s$treatment, " ~ ", s$instrument)),
                 data = reg, vcov = "hetero", notes = FALSE)
  fs <- feols(as.formula(paste0(s$treatment, " ~ ", s$instrument, " + ", rhs)),
              data = reg, vcov = "hetero", notes = FALSE)
  rf <- feols(as.formula(paste0(y, " ~ ", s$instrument, " + ", rhs)),
              data = reg, vcov = "hetero", notes = FALSE)
  term <- paste0("fit_", s$treatment)
  b <- unname(coef(model)[term]); uncertainty <- unname(se(model)[term])
  fs_b <- unname(coef(fs)[s$instrument]); fs_se <- unname(se(fs)[s$instrument])
  rf_b <- unname(coef(rf)[s$instrument]); rf_se <- unname(se(rf)[s$instrument])
  stopifnot(nobs(model) == nrow(reg), nobs(fs) == nrow(reg), nobs(rf) == nrow(reg),
    is.finite(b), uncertainty > 0, abs(b - rf_b / fs_b) < 1e-9)
  data.table(predictor = s$spec, outcome = y, outcome_label = label,
    treatment = s$treatment, instrument = s$instrument, risk_control = s$expected,
    beta = b, se = uncertainty, p_value = 2 * pnorm(-abs(b / uncertainty)),
    ci_low = b - 1.96 * uncertainty, ci_high = b + 1.96 * uncertainty, n_obs = nobs(model),
    first_stage_beta = fs_b, first_stage_se = fs_se, first_stage_f = (fs_b / fs_se)^2,
    reduced_form_beta = rf_b, reduced_form_se = rf_se,
    outcome_mean = mean(reg[[y]]), vcov = "fixest hetero, default finite-sample correction")
}

# First reproduce the three same-outcome current main IV estimates, including SEs.
own <- rbindlist(lapply(seq_len(nrow(specs)), function(i) fit_iv(specs$outcome[i], specs$label[i], specs[i])))
main <- fread(file.path(repo, "output", "tables", "empirical_bayes_school_va", "va_2017_2020__sae_2018_2020", "main_results.csv"))
parity <- merge(own, main[spec %in% specs$spec, .(predictor = spec, reference_beta = beta, reference_se = se, reference_n = n_obs)], by = "predictor")
stopifnot(nrow(parity) == 3L, all(parity$n_obs == parity$reference_n),
  max(abs(parity$beta - parity$reference_beta)) < 1e-8,
  max(abs(parity$se - parity$reference_se)) < 1e-8)
fwrite(parity, file.path(out, "current_main_iv_parity.csv"))

# Requested cross effects: only the outcome changes; keep the EB math index,
# offered-school EB math instrument and matching expected EB math risk control.
results <- rbindlist(lapply(2:3, function(i) fit_iv(specs$outcome[i], specs$label[i], specs[1])))
fwrite(results, file.path(out, "math_eb_va_cross_effects.csv"))
fwrite(data.table(input_frame = input, n_timely_at_risk = nrow(d),
                  n_exam_takers = sum(d$admission_exam_taker == 1L, na.rm = TRUE)), file.path(out, "input_sample_coverage.csv"))
print(parity[, .(predictor, beta, reference_beta, se, reference_se, n_obs, reference_n)])
print(results)
