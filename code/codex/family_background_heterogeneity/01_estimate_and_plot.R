suppressPackageStartupMessages({library(data.table); library(fixest); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "family_background_heterogeneity")
fig <- file.path(repo, "output", "figures", "family_background_heterogeneity")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
setFixest_nthreads(2)
log_msg <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

specs <- data.table(
  spec = c("math_adj_eb", "leng_adj_eb", "highinst_adj_eb", "highpay_adj_eb", "program_income_adj_eb"),
  outcome = c("z_year_math_max", "z_year_leng_max", "high_inst_m1", "high_paying_field_m1", "log_program_income_clp_m1"),
  label = c("Math", "Verbal", "High-premium inst.", "High-premium field", "Log proj. income")
)
specs[, `:=`(treatment = paste0("d_", spec), instrument = paste0("z_", spec), expected = paste0("expected_", spec))]
controls <- c("cohort_gr8", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to")

# This is the saved current main-IV frame plus left-joined peer diagnostics.
# Read only original main-IV columns; no peer variable enters a regression/filter.
frame_path <- Sys.getenv("FAMILY_HET_REGRESSION_DF", file.path(repo, "data", "clean",
  "peer_lottery_variation", "main_regression_df_with_peer_shocks.csv"))
needed <- unique(c("student_id", "mrun", "sae_proceso", "timely_sae", "any_risk",
  "admission_exam_taker", "most_time_RBD", "rbd_treated_1R", controls,
  specs$outcome, "log_program_income_full_clp_m1", specs$treatment, specs$instrument, specs$expected))
log_msg("Reading selected columns of saved main-IV frame")
dt <- fread(frame_path, select = needed, na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(!anyDuplicated(dt$mrun), !anyDuplicated(dt, by = c("student_id", "sae_proceso")),
  all(dt$sae_proceso %in% 2018:2020), all(dt$timely_sae == 1L), all(dt$any_risk == 1L))
stopifnot(identical(is.na(dt$log_program_income_clp_m1), is.na(dt$log_program_income_full_clp_m1)),
  max(abs(dt$log_program_income_clp_m1 - dt$log_program_income_full_clp_m1), na.rm = TRUE) < 1e-10)

# Verify attended/offered school VA comes from the same current unified R EB file.
va <- fread(file.path(repo, "output", "intermediate", "empirical_bayes_school_va",
  "highered_2017_2020", "eb_school_rbd_observational_values_all_outcomes.csv"),
  select = c("school_rbd", "analysis_sample", "outcome", "controlled_value_added_eb_centered_student"))
source_audit <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]
  v <- va[analysis_sample == "All" & outcome == s$outcome]
  stopifnot(!anyDuplicated(v$school_rbd))
  attended <- v$controlled_value_added_eb_centered_student[match(dt$most_time_RBD, v$school_rbd)]
  offered <- v$controlled_value_added_eb_centered_student[match(dt$rbd_treated_1R, v$school_rbd)]
  offered[is.na(dt$rbd_treated_1R) | dt$rbd_treated_1R == 0] <- 0
  gap_d <- max(abs(attended - dt[[s$treatment]]), na.rm = TRUE)
  gap_z <- max(abs(offered - dt[[s$instrument]]), na.rm = TRUE)
  stopifnot(identical(is.na(attended), is.na(dt[[s$treatment]])),
            identical(is.na(offered), is.na(dt[[s$instrument]])), gap_d < 1e-10, gap_z < 1e-10)
  data.table(spec = s$spec, max_attended_gap = gap_d, max_offered_gap = gap_z)
}))
fwrite(source_audit, file.path(out, "school_value_source_validation.csv"))

log_msg("Reading family background from selected universe columns")
background <- fread(file.path(root, "data", "clean", "univ_gr8_df.csv"),
  select = c("mrun", "income_mid_imputed", "income_decile_imputed", "father_educ_years_imputed",
             "mother_educ_years_imputed"), na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(!anyDuplicated(background$mrun))
n_before <- nrow(dt)
dt <- merge(dt, background, by = "mrun", all.x = TRUE, sort = FALSE)
stopifnot(nrow(dt) == n_before)
dt[, parent_education_years := rowMeans(cbind(father_educ_years_imputed, mother_educ_years_imputed), na.rm = TRUE)]
dt[!is.finite(parent_education_years), parent_education_years := NA_real_]
make_rank_quintile <- function(x) {
  ans <- rep(NA_integer_, length(x))
  keep <- is.finite(x)
  n <- sum(keep)
  if (n) ans[keep] <- pmin(5L, as.integer(ceiling(5 * frank(x[keep], ties.method = "average") / n)))
  ans
}
# Same pooled-rank/tie rule as the reference achievement-heterogeneity figure.
# Define bins once before outcome-specific complete-case/exam-taker filters.
dt[, `:=`(income_quintile = make_rank_quintile(income_mid_imputed),
           education_quintile = make_rank_quintile(parent_education_years))]
bins <- rbindlist(list(
  dt[!is.na(income_quintile), .(n_students = .N, min_value = min(income_mid_imputed),
    max_value = max(income_mid_imputed), mean_value = mean(income_mid_imputed)), by = .(quintile = income_quintile)][, background := "income"],
  dt[!is.na(education_quintile), .(n_students = .N, min_value = min(parent_education_years),
    max_value = max(parent_education_years), mean_value = mean(parent_education_years)), by = .(quintile = education_quintile)][, background := "education"]
))
stopifnot(nrow(bins) == 10L, setequal(bins[background == "income", quintile], 1:5),
  setequal(bins[background == "education", quintile], 1:5))
setorder(bins, background, quintile)
fwrite(bins, file.path(out, "quintile_definitions.csv"))
fwrite(dt[, .(n_students = .N, n_income = sum(!is.na(income_quintile)),
  n_education = sum(!is.na(education_quintile)),
  n_one_parent_only = sum(rowSums(!is.na(cbind(father_educ_years_imputed, mother_educ_years_imputed))) == 1L))],
  file.path(out, "background_coverage.csv"))

run_iv <- function(x, s, background = "pooled", quintile = 0L) {
  required <- c(controls, s$outcome, s$treatment, s$instrument, s$expected, "admission_exam_taker")
  x <- x[complete.cases(x[, ..required]) & admission_exam_taker == 1L]
  stopifnot(nrow(x) > 0L, uniqueN(x[[s$instrument]]) > 1L)
  rhs <- paste(c("factor(cohort_gr8)", "z_sim_mat_4to", "z_sim_leng_4to",
    "factor(GEN_ALU)", "factor(EDAD_ALU)", s$expected), collapse = " + ")
  model <- feols(as.formula(paste0(s$outcome, " ~ ", rhs, " | 0 | ",
    s$treatment, " ~ ", s$instrument)), data = x, vcov = "hetero", notes = FALSE)
  term <- paste0("fit_", s$treatment)
  stopifnot(term %in% names(coef(model)))
  b <- unname(coef(model)[term])
  uncertainty <- unname(se(model)[term])
  fs <- feols(as.formula(paste0(s$treatment, " ~ ", s$instrument, " + ", rhs)),
    data = x, vcov = "hetero", notes = FALSE)
  fs_f <- unname((coef(fs)[s$instrument] / se(fs)[s$instrument])^2)
  data.table(background = background, quintile = quintile, spec = s$spec, label = s$label,
    beta = b, se = uncertainty, ci_low = b - 1.96 * uncertainty, ci_high = b + 1.96 * uncertainty,
    p_value = 2 * pnorm(-abs(b / uncertainty)), n_obs = nobs(model), fs_f = fs_f)
}
log_msg("Checking pooled IV estimates against current saved main results")
pooled <- rbindlist(lapply(seq_len(nrow(specs)), function(i) run_iv(dt, specs[i])))
main <- fread(file.path(repo, "output", "tables", "empirical_bayes_school_va",
  "va_2017_2020__sae_2018_2020", "main_results.csv"))
parity <- merge(pooled, main[spec %in% specs$spec, .(spec, reference_beta = beta, reference_se = se, reference_n = n_obs)], by = "spec")
stopifnot(nrow(parity) >= 4L, all(parity$n_obs == parity$reference_n),
          max(abs(parity$beta - parity$reference_beta)) < 1e-8,
          max(abs(parity$se - parity$reference_se)) < 1e-8)
fwrite(parity, file.path(out, "pooled_main_iv_parity.csv"))
fwrite(pooled, file.path(out, "pooled_estimates.csv"))

log_msg("Fitting income and parental-education quintile IV regressions")
results <- rbindlist(lapply(c("income", "education"), function(bg) {
  q_col <- paste0(bg, "_quintile")
  rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    rbindlist(lapply(1:5, function(q) run_iv(dt[get(q_col) == q], specs[i], bg, q)))
  }))
}))
stopifnot(nrow(results) == 50L, all(is.finite(results$beta)), all(results$se > 0))
fwrite(results, file.path(out, "family_background_heterogeneity_estimates.csv"))
fwrite(dt[, .(mrun, student_id, sae_proceso, income_mid_imputed, income_quintile,
  father_educ_years_imputed, mother_educ_years_imputed, parent_education_years, education_quintile)],
  file.path(out, "student_background_groups.csv"))
counts <- results[, .(n_grouped = sum(n_obs)), by = .(background, spec)]
# Grouped regressions partition each outcome's background-observed complete cases.
checks <- rbindlist(lapply(c("income", "education"), function(bg) {
  rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    s <- specs[i]
    cols <- c(controls, s$outcome, s$treatment, s$instrument, s$expected, "admission_exam_taker", paste0(bg, "_quintile"))
    data.table(background = bg, spec = s$spec,
      n_expected = dt[complete.cases(dt[, ..cols]) & admission_exam_taker == 1L, .N])
  }))
}))
checks <- merge(counts, checks, by = c("background", "spec"))
stopifnot(all(checks$n_grouped == checks$n_expected))
fwrite(checks, file.path(out, "group_sample_partition_checks.csv"))

colors <- c("Math" = "#1B4D89", "Verbal" = "#B6422E", "High-premium inst." = "#1B7F5A",
            "High-premium field" = "#7A4EA3", "Log proj. income" = "#A15C1B")
results[, label := factor(label, levels = specs$label)]
results[, x := quintile + rep(c(-0.24, -0.12, 0, 0.12, 0.24), each = 5L, times = 2L)]
yrange <- range(c(results$ci_low, results$ci_high, 1), finite = TRUE)
make_plot <- function(bg) {
  title <- if (bg == "income") "Heterogeneity by family income" else "Heterogeneity by parental education"
  xlabel <- if (bg == "income") "Family income quintile" else "Average parental education quintile"
  subtitle <- if (bg == "income") "Imputed Grade 4 SIMCE family income" else "Average of mother's and father's imputed education years"
  xlabels <- c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")
  if (bg == "education") {
    b <- bins[background == bg][order(quintile)]
    xlabels <- paste0(xlabels, "\n", format(b$min_value, trim = TRUE), "-", format(b$max_value, trim = TRUE), " years")
  }
  ggplot(results[background == bg], aes(x = x, y = beta, color = label, shape = label)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey65", linewidth = 0.45) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.05, linewidth = 0.5) +
    geom_point(size = 2.6) +
    scale_color_manual(values = colors) + scale_shape_manual(values = c(16, 17, 15, 18, 8)) +
    scale_x_continuous(breaks = 1:5, labels = xlabels, limits = c(0.5, 5.5)) +
    coord_cartesian(ylim = yrange) +
    labs(title = title, subtitle = subtitle, x = xlabel,
      y = expression(paste("Estimated EB pass-through ", theta^{EB})), color = NULL, shape = NULL,
      caption = "Quintiles in the pooled lottery frame; ties stay together. Current main-IV sample rules and controls; robust 95% confidence intervals.") +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
      legend.position = "top", legend.text = element_text(size = 10),
      plot.caption = element_text(hjust = 0, size = 8.5, margin = margin(t = 12)),
      axis.title.x = element_text(margin = margin(t = 12)),
      plot.margin = margin(14, 16, 12, 14))
}
for (bg in c("income", "education")) {
  p <- make_plot(bg)
  stub <- paste0("family_", bg, "_quintile_heterogeneity")
  ggsave(file.path(fig, paste0(stub, ".png")), p, width = 11, height = 7, dpi = 220, bg = "white")
  ggsave(file.path(fig, paste0(stub, ".pdf")), p, width = 11, height = 7, bg = "white")
}
print(bins)
print(pooled)
print(results[, .(background, label, quintile, beta, se, n_obs, fs_f)])
log_msg("Saved two figures and regression diagnostics")
