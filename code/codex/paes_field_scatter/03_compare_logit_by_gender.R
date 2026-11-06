suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
parent_out <- file.path(repo, "data", "clean", "paes_field_scatter", "broad_panel", "by_gender")
input_path <- file.path(parent_out, "gender_scatter_input.rds")
if (!file.exists(input_path)) {
  # Rebuild using the exact preceding sample/outcome definitions and parity checks.
  source(file.path(repo, "code", "codex", "paes_field_scatter", "02_plot_broad_panel_by_gender.R"),
         local = new.env(parent = globalenv()))
}
out <- file.path(parent_out, "logit_comparison")
fig <- file.path(repo, "output", "figures", "paes_field_scatter", "broad_panel", "by_gender", "logit_comparison")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
x <- as.data.table(readRDS(input_path))
bins <- fread(file.path(parent_out, "gender_score_bins.csv"))
reference <- fread(file.path(parent_out, "gender_linear_fit_summary.csv"))
stopifnot(nrow(x) == 2L * 645902L, all(x$enrollment %in% 0:1), all(is.finite(x$math_score)),
          !anyDuplicated(x[, .(mrun, outcome)]))
labels <- c(high_paying_field_m1 = "High-premium field", high_inst_m1 = "High-premium institution")

# A common student-level holdout across outcomes; stratify by gender, never by Y.
# Test observations are not used to fit either validation model.
students <- unique(x[, .(mrun, gender)])
setorder(students, gender, mrun)
set.seed(20261106)
students[, fold := sample(rep_len(1:5, .N)), by = gender]
x[students, on = "mrun", fold := i.fold]
stopifnot(!anyNA(x$fold))
fwrite(students, file.path(out, "student_validation_folds.csv"))
fwrite(x[, .(n_students = .N, n_events = sum(enrollment)), by = .(outcome, gender, fold)],
       file.path(out, "validation_sample_counts.csv"))

fits <- curves <- metrics <- list()
for (o in names(labels)) for (g in c("Male", "Female")) {
  z <- x[outcome == o & gender == g]
  id <- paste(o, g, sep = "_")
  b <- bins[outcome == o & gender == g]
  r <- reference[outcome == o & gender == g]
  stopifnot(nrow(z) == r$n_students, sum(b$n_students) == nrow(z),
    abs(mean(z$enrollment) * 100 - r$mean_enrollment_pct) < 1e-9)
  linear <- lm(enrollment ~ math_score, data = z)
  logit <- glm(enrollment ~ math_score, family = binomial(link = "logit"), data = z)
  stopifnot(logit$converged, nobs(logit) == nrow(z),
    abs(coef(linear)[[1]] * 100 - r$intercept_pp) < 1e-9,
    abs(coef(linear)[[2]] * 100 - r$slope_pp_per_sd) < 1e-9)
  for (model_name in c("Linear", "Logit")) {
    model <- if (model_name == "Linear") linear else logit
    prediction <- as.numeric(predict(model, type = "response"))
    fits[[paste(id, model_name)]] <- data.table(outcome = o, panel = labels[[o]], gender = g,
      model = model_name, n_students = nrow(z), intercept = unname(coef(model)[1]),
      slope = unname(coef(model)[2]), in_sample_mse = mean((z$enrollment - prediction)^2),
      min_fitted = min(prediction), max_fitted = max(prediction),
      fraction_outside_unit_interval = mean(prediction < 0 | prediction > 1))
    grid <- data.table(math_score = seq(min(b$mean_math_score), max(b$mean_math_score), length.out = 400))
    grid[, `:=`(fitted_pct = 100 * as.numeric(predict(model, newdata = grid, type = "response")),
                outcome = o, panel = labels[[o]], gender = g, model = model_name)]
    curves[[paste(id, model_name)]] <- grid
  }
  train <- z[fold != 1L]
  test <- z[fold == 1L]
  validation_linear <- lm(enrollment ~ math_score, data = train)
  validation_logit <- glm(enrollment ~ math_score, family = binomial(link = "logit"), data = train)
  stopifnot(validation_logit$converged)
  p_linear <- as.numeric(predict(validation_linear, newdata = test))
  p_logit <- as.numeric(predict(validation_logit, newdata = test, type = "response"))
  stopifnot(all(is.finite(p_linear)), all(p_logit >= 0 & p_logit <= 1))
  loss_linear <- (test$enrollment - p_linear)^2
  loss_logit <- (test$enrollment - p_logit)^2
  # Raw linear predictions are left intact, including values outside [0,1].
  # Same 0/1 outcome and squared-error loss for a comparable predictive metric.
  metrics[[id]] <- data.table(outcome = o, panel = labels[[o]], gender = g,
    n_train = nrow(train), n_test = nrow(test), linear_mse = mean(loss_linear),
    logit_mse = mean(loss_logit), logit_mse_reduction_pct = 100 * (1 - mean(loss_logit) / mean(loss_linear)),
    linear_fraction_outside_unit_interval = mean(p_linear < 0 | p_linear > 1),
    linear_min_prediction = min(p_linear), linear_max_prediction = max(p_linear),
    logit_min_prediction = min(p_logit), logit_max_prediction = max(p_logit))
}
fits <- rbindlist(fits)
curves <- rbindlist(curves)
metrics <- rbindlist(metrics)
fwrite(fits, file.path(out, "full_sample_fit_summary.csv"))
fwrite(curves, file.path(out, "fitted_curves.csv"))
fwrite(metrics, file.path(out, "heldout_prediction_comparison.csv"))
for (v in c("bins", "curves")) {
  a <- get(v)
  a[, `:=`(panel = factor(panel, levels = unname(labels)), gender = factor(gender, levels = c("Male", "Female")))]
}
curves[, model := factor(model, levels = c("Logit", "Linear"))]
p <- ggplot(bins, aes(mean_math_score, enrollment_share_pct, color = gender, shape = gender)) +
  geom_point(size = 2.0, alpha = 0.8) +
  geom_line(data = curves, aes(x = math_score, y = fitted_pct, linetype = model), linewidth = 0.9) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = c(Male = "#244F78", Female = "#BC4B37")) +
  scale_shape_manual(values = c(Male = 16, Female = 17)) +
  scale_linetype_manual(values = c(Logit = "solid", Linear = "22")) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(v) paste0(v, "%")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Logit versus linear fit, by gender",
    subtitle = "Broad panel, Grade 8 cohorts 2017-2020 | Including private schools | Same samples and score scale as preceding figure",
    x = "PAES/PDT math score (standard deviations within test year)", y = "Enrollment share",
    color = NULL, shape = NULL, linetype = NULL,
    caption = paste0("Points: gender-specific means in 50 common score-rank bins. Solid: logit. Dashed: OLS. Curves fitted to individuals, without controls.\n",
      "First enrollment recorded in 2022-2025; non-enrollees coded zero. Scores standardized within test year using the pooled broad panel.")) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 18, color = "#172B40"),
    plot.subtitle = element_text(size = 10, color = "#475569", margin = margin(b = 14)),
    strip.background = element_rect(fill = "#F0F3F6", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom", panel.spacing = grid::unit(1.4, "lines"),
    plot.caption = element_text(hjust = 0, size = 9, color = "#5B6470", margin = margin(t = 10)),
    plot.margin = margin(16, 18, 12, 14))
ggsave(file.path(fig, "paes_math_logit_vs_linear_by_gender.png"), p, width = 13.5, height = 6.8, dpi = 220, bg = "white")
ggsave(file.path(fig, "paes_math_logit_vs_linear_by_gender.pdf"), p, width = 13.5, height = 6.8, bg = "white")
print(metrics)
print(fits)
