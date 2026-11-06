suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
out <- file.path(repo, "data", "clean", "paes_field_scatter")
fig <- file.path(repo, "output", "figures", "paes_field_scatter")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)

# Only selected columns of the saved 138 MB main-frame/peer-diagnostics file.
# No peer variables, IV controls, or baseline-completeness filter are used.
source <- fread(file.path(repo, "data", "clean", "peer_lottery_variation",
                         "main_regression_df_with_peer_shocks.csv"),
  select = c("mrun", "sae_proceso", "timely_sae", "any_risk", "z_year_math_max",
             "high_paying_field_m1", "higher_ed_enrolled_m1"),
  na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(!anyDuplicated(source$mrun), all(source$timely_sae == 1L),
          all(source$any_risk == 1L), all(source$sae_proceso %in% 2018:2020))
d <- source[is.finite(z_year_math_max) & !is.na(high_paying_field_m1)]
stopifnot(all(d$high_paying_field_m1 %in% 0:1),
          all(d[higher_ed_enrolled_m1 == 0L, high_paying_field_m1] == 0L))
d[, `:=`(math_score = z_year_math_max, high_premium_field_pct = 100 * high_paying_field_m1)]

# Fit to every individual observation, not the displayed bin means.
model <- lm(high_premium_field_pct ~ math_score, data = d)
stats <- data.table(n_students = nrow(d), n_source = nrow(source),
  n_excluded_missing_score_or_outcome = nrow(source) - nrow(d),
  intercept_pp = unname(coef(model)["(Intercept)"]),
  slope_pp_per_sd = unname(coef(model)["math_score"]),
  r_squared = summary(model)$r.squared,
  correlation = cor(d$math_score, d$high_premium_field_pct),
  pct_high_premium_field = mean(d$high_premium_field_pct),
  pct_higher_ed_enrolled = 100 * mean(d$higher_ed_enrolled_m1, na.rm = TRUE))
d[, bin := pmin(50L, as.integer(ceiling(50 * frank(math_score, ties.method = "average") / .N)))]
bins <- d[, .(n_students = .N, mean_math_score = mean(math_score),
               pct_high_premium_field = mean(high_premium_field_pct),
               min_math_score = min(math_score), max_math_score = max(math_score)), by = bin]
setorder(bins, bin)
stopifnot(sum(bins$n_students) == nrow(d), nrow(bins) == 50L,
  abs(weighted.mean(bins$pct_high_premium_field, bins$n_students) - mean(d$high_premium_field_pct)) < 1e-10)
line <- data.table(math_score = seq(min(bins$mean_math_score), max(bins$mean_math_score), length.out = 300L))
line[, fitted_pct := as.numeric(predict(model, newdata = line))]
fit_label <- sprintf("N = %s  |  Slope = %.1f percentage points per SD  |  R-squared = %.3f",
                     format(nrow(d), big.mark = ",", trim = TRUE), stats$slope_pp_per_sd, stats$r_squared)

base_theme <- theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 18, color = "#172B40"),
        plot.subtitle = element_text(color = "#475569", size = 10.5, margin = margin(b = 14)),
        plot.caption = element_text(hjust = 0, color = "#5B6470", size = 9, margin = margin(t = 12)),
        axis.title = element_text(size = 12), plot.margin = margin(16, 18, 12, 14))
p <- ggplot(bins, aes(x = mean_math_score, y = pct_high_premium_field)) +
  geom_point(color = "#244F78", size = 2.7, alpha = 0.9) +
  geom_line(data = line, aes(x = math_score, y = fitted_pct), color = "#BC4B37", linewidth = 0.9) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "PAES math score and high-premium field enrollment", subtitle = fit_label,
       x = "PAES/PDT math score (standard deviations within test year)",
       y = "High-premium field enrollment share",
       caption = paste0("Points: means in 50 score-rank bins, preserving ties. Red line: OLS using every student.\n",
         "At-risk SAE applicants, processes 2018-2020. First enrollment in 2022-2025; non-enrollees coded zero. No additional controls.")) + base_theme
ggsave(file.path(fig, "paes_math_high_premium_field_binscatter.png"), p, width = 10, height = 6.5, dpi = 220, bg = "white")
ggsave(file.path(fig, "paes_math_high_premium_field_binscatter.pdf"), p, width = 10, height = 6.5, bg = "white")

# Retain a literal individual-level scatter as an additional view of the binary outcome.
raw_line <- data.table(math_score = seq(min(d$math_score), max(d$math_score), length.out = 300L))
raw_line[, fitted_pct := as.numeric(predict(model, newdata = raw_line))]
raw_plot <- ggplot(d, aes(x = math_score, y = high_premium_field_pct)) +
  geom_point(color = "#244F78", alpha = 0.015, size = 0.7) +
  geom_line(data = raw_line, aes(y = fitted_pct), color = "#BC4B37", linewidth = 0.9) +
  scale_y_continuous(breaks = c(0, 100), labels = c("0", "1")) +
  labs(title = "PAES math score and high-premium field enrollment", subtitle = fit_label,
       x = "PAES/PDT math score (standard deviations within test year)",
       y = "High-premium field enrollment indicator",
       caption = "Individual student outcomes and the same OLS line. At-risk SAE applicants; non-enrollees coded zero. No additional controls.") + base_theme
ggsave(file.path(fig, "paes_math_high_premium_field_individual_scatter.png"), raw_plot,
       width = 10, height = 6.5, dpi = 220, bg = "white")
fwrite(bins, file.path(out, "score_bins.csv"))
fwrite(stats, file.path(out, "linear_fit_summary.csv"))
print(stats)
