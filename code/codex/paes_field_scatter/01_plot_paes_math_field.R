suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
sample_type <- Sys.getenv("SCATTER_SAMPLE", "at_risk")
stopifnot(sample_type %in% c("at_risk", "broad_panel"))
out <- file.path(repo, "data", "clean", "paes_field_scatter")
fig <- file.path(repo, "output", "figures", "paes_field_scatter")
if (sample_type == "broad_panel") {
  out <- file.path(out, "broad_panel")
  fig <- file.path(fig, "broad_panel")
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)

# Only selected columns of the saved 138 MB main-frame/peer-diagnostics file.
# No peer variables, IV controls, or baseline-completeness filter are used.
if (sample_type == "at_risk") {
  source <- fread(file.path(repo, "data", "clean", "peer_lottery_variation",
                         "main_regression_df_with_peer_shocks.csv"),
  select = c("mrun", "sae_proceso", "timely_sae", "any_risk", "z_year_math_max",
             "high_paying_field_m1", "higher_ed_enrolled_m1"),
  na.strings = c("", "NA"), showProgress = FALSE)
stopifnot(!anyDuplicated(source$mrun), all(source$timely_sae == 1L),
          all(source$any_risk == 1L), all(source$sae_proceso %in% 2018:2020))
  sample_label <- "At-risk SAE applicants, processes 2018-2020"
} else {
  root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
  # Selected columns of the roughly 1 GB universe; no baseline-completeness filter.
  source <- fread(file.path(root, "data", "clean", "univ_gr8_df.csv"),
    select = c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD", "psu_year", "math_max",
      "COD_SIES_m1", "field_reclassified_m1", "f_science_m1", "f_law_m1", "f_eng_m1"),
    na.strings = c("", "NA"), showProgress = FALSE)
  source <- source[cohort_gr8 %in% 2017:2020 & most_time_RBD > 0 & EDAD_ALU %between% c(12, 16)]
  setkey(source, mrun)
  source <- source[!duplicated(mrun)]
  stopifnot(nrow(source) == 931547L, !anyNA(source$mrun))
  source[, COD_SIES_m1 := trimws(as.character(COD_SIES_m1))]
  source[COD_SIES_m1 %chin% c("", "NA"), COD_SIES_m1 := NA_character_]
  source[, `:=`(matriculated_m1 = !is.na(COD_SIES_m1), higher_ed_enrolled_m1 = as.integer(!is.na(COD_SIES_m1)))]
  program_info <- as.data.table(readRDS(file.path(root, "data", "clean", "program_info_22-25.rds")))
  stopifnot(!anyDuplicated(program_info$COD_SIES))
  source <- merge(source, program_info[, .(COD_SIES_m1 = as.character(COD_SIES),
    AREA_CARRERA_GENERICA_m1 = AREA_CARRERA_GENERICA)], by = "COD_SIES_m1", all.x = TRUE, sort = FALSE)

  # Reuse the exact existing high-premium-field constructor without executing
  # its income-model or export pipeline. No new field classification is invented.
  helper_env <- new.env(parent = globalenv())
  helper_names <- c("normalize_text", "high_paying_medicine_plus_area_keys", "derive_high_paying_field")
  expressions <- parse(file.path(repo, "code", "codex", "mifuturo_matricula_income",
                                  "03_construct_person_level_income_outcomes.R"))
  for (e in expressions) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.symbol(e[[2]]) &&
        as.character(e[[2]]) %in% helper_names) eval(e, envir = helper_env)
  }
  stopifnot(all(vapply(helper_names, exists, logical(1), envir = helper_env, inherits = FALSE)))
  source <- helper_env$derive_high_paying_field(source, "_m1")

  # Check the reconstructed outcome against the existing at-risk scatter input.
  reference <- fread(file.path(repo, "data", "clean", "peer_lottery_variation",
    "main_regression_df_with_peer_shocks.csv"), select = c("mrun", "high_paying_field_m1"), showProgress = FALSE)
  check <- merge(source[, .(mrun, reconstructed = high_paying_field_m1)], reference, by = "mrun")
  stopifnot(nrow(check) == nrow(reference),
    identical(is.na(check$reconstructed), is.na(check$high_paying_field_m1)),
    all(check$reconstructed == check$high_paying_field_m1, na.rm = TRUE))
  fwrite(data.table(n_matched = nrow(check), n_outcome_disagreements = sum(check$reconstructed != check$high_paying_field_m1, na.rm = TRUE)),
         file.path(out, "at_risk_outcome_parity.csv"))

  # The broad-panel version uses broad-panel test-year means/SDs, rather than
  # the SAE-only reference population implicit in the earlier main-IV frame.
  source[, z_year_math_max := NA_real_]
  source[!is.na(psu_year) & is.finite(math_max) & math_max > 0,
         z_year_math_max := (math_max - mean(math_max)) / sd(math_max), by = psu_year]
  score_audit <- source[is.finite(z_year_math_max), .(n_students = .N,
    raw_mean = mean(math_max), raw_sd = sd(math_max), z_mean = mean(z_year_math_max),
    z_sd = sd(z_year_math_max)), by = psu_year]
  stopifnot(all(abs(score_audit$z_mean) < 1e-10), all(abs(score_audit$z_sd - 1) < 1e-10))
  fwrite(score_audit, file.path(out, "score_standardization_by_year.csv"))
  sample_label <- "Broad panel, Grade 8 cohorts 2017-2020 (including private schools)"
}
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
         sample_label, ". First enrollment in 2022-2025; non-enrollees coded zero. No additional controls.")) + base_theme
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
       caption = paste0("Individual student outcomes and the same OLS line. ", sample_label,
                        "; non-enrollees coded zero. No additional controls.")) + base_theme
ggsave(file.path(fig, "paes_math_high_premium_field_individual_scatter.png"), raw_plot,
       width = 10, height = 6.5, dpi = 220, bg = "white")
fwrite(bins, file.path(out, "score_bins.csv"))
fwrite(stats, file.path(out, "linear_fit_summary.csv"))
print(stats)
