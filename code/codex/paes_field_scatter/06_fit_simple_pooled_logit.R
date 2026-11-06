suppressPackageStartupMessages(library(data.table))
repo <- normalizePath(getwd(), winslash = "/")
base <- file.path(repo, "data", "clean", "paes_field_scatter", "broad_panel", "by_gender")
out <- file.path(base, "simple_pooled_logit")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
d <- as.data.table(readRDS(file.path(base, "gender_scatter_input.rds")))
stopifnot(nrow(d) == 2L * 645902L, !anyDuplicated(d[, .(mrun, outcome)]),
          all(d$enrollment %in% 0:1), all(is.finite(d$math_score)))
labels <- c(high_paying_field_m1 = "High-premium field", high_inst_m1 = "High-premium institution")
# Descriptive DGP calibration: pool genders, with no gender controls, school
# effects, VA stratification, or other covariates. Score units are pooled
# broad-panel within-test-year standard deviations, as in the preceding figures.
results <- rbindlist(lapply(names(labels), function(o) {
  x <- d[outcome == o]
  model <- glm(enrollment ~ math_score, family = binomial(link = "logit"), data = x)
  p <- as.numeric(predict(model, type = "response"))
  stopifnot(model$converged, nobs(model) == 645902L, all(is.finite(coef(model))),
            all(p >= 0 & p <= 1), abs(mean(p) - mean(x$enrollment)) < 1e-8)
  data.table(outcome = o, label = labels[[o]], n_students = nobs(model),
    alpha_H = unname(coef(model)[1]), beta_Y_log_odds_per_sd = unname(coef(model)[2]),
    odds_ratio_per_sd = exp(unname(coef(model)[2])),
    average_marginal_effect_pp_per_sd = 100 * mean(p * (1 - p)) * unname(coef(model)[2]),
    observed_choice_pct = 100 * mean(x$enrollment))
}))
fwrite(results, file.path(out, "pooled_logit_coefficients.csv"))
print(results)
