suppressPackageStartupMessages({library(data.table); library(ggplot2)})
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
out <- file.path(repo, "data", "clean", "paes_field_scatter", "broad_panel", "by_gender")
fig <- file.path(repo, "output", "figures", "paes_field_scatter", "broad_panel", "by_gender")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)

# Column-selective read of the roughly 1 GB universe; same broad-panel sample.
d <- fread(file.path(root, "data", "clean", "univ_gr8_df.csv"),
  select = c("mrun", "cohort_gr8", "EDAD_ALU", "GEN_ALU", "most_time_RBD", "psu_year", "math_max",
    "COD_SIES_m1", "field_reclassified_m1", "f_science_m1", "f_law_m1", "f_eng_m1",
    "COD_INST_m1", "TIPO_INST_1_m1", "NOMB_INST_m1"),
  na.strings = c("", "NA"), showProgress = FALSE)
d <- d[cohort_gr8 %in% 2017:2020 & most_time_RBD > 0 & EDAD_ALU %between% c(12, 16)]
setkey(d, mrun)
d <- d[!duplicated(mrun)]
stopifnot(nrow(d) == 931547L, !anyNA(d$mrun))
d[, COD_SIES_m1 := trimws(as.character(COD_SIES_m1))]
d[COD_SIES_m1 %chin% c("", "NA"), COD_SIES_m1 := NA_character_]
d[, matriculated_m1 := !is.na(COD_SIES_m1)]
info <- as.data.table(readRDS(file.path(root, "data", "clean", "program_info_22-25.rds")))
stopifnot(!anyDuplicated(info$COD_SIES))
d <- merge(d, info[, .(COD_SIES_m1 = as.character(COD_SIES), AREA_CARRERA_GENERICA_m1 = AREA_CARRERA_GENERICA)],
           by = "COD_SIES_m1", all.x = TRUE, sort = FALSE)

# Import canonical outcome helpers only; never execute the income-model pipeline.
helpers <- new.env(parent = globalenv())
helpers$high_inst_fe_cutoff <- 0.1
helper_names <- c("required_file", "normalize_text", "high_paying_medicine_plus_area_keys",
                  "derive_high_paying_field", "read_institution_effects", "derive_high_institution")
expressions <- parse(file.path(repo, "code", "codex", "mifuturo_matricula_income",
                               "03_construct_person_level_income_outcomes.R"))
for (e in expressions) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.symbol(e[[2]]) &&
      as.character(e[[2]]) %in% helper_names) eval(e, envir = helpers)
}
stopifnot(all(vapply(helper_names, exists, logical(1), envir = helpers, inherits = FALSE)))
d <- helpers$derive_high_paying_field(d, "_m1")
d[, institution_key := paste(as.character(suppressWarnings(as.integer(COD_INST_m1))),
                             helpers$normalize_text(TIPO_INST_1_m1),
                             helpers$normalize_text(NOMB_INST_m1), sep = " || ")]
effects <- helpers$read_institution_effects(file.path(repo, "output", "tables", "mifuturo_matricula_income",
                                                      "mifuturo_income_fe_institution_effects.csv"))
stopifnot(!anyDuplicated(effects$institution_key))
effects <- effects[, .(institution_key, mifuturo_institution_fe_log_clp_centered_m1 = mifuturo_institution_fe_log_clp_centered)]
d <- merge(d, effects, by = "institution_key", all.x = TRUE, sort = FALSE)
d <- helpers$derive_high_institution(d, "_m1")

# Both binaries must equal the existing current main outcomes on the shared students.
reference <- fread(file.path(repo, "data", "clean", "peer_lottery_variation",
  "main_regression_df_with_peer_shocks.csv"), select = c("mrun", "high_paying_field_m1", "high_inst_m1"),
  showProgress = FALSE)
check <- merge(d[, .(mrun, field = high_paying_field_m1, institution = high_inst_m1)], reference, by = "mrun")
stopifnot(nrow(check) == nrow(reference))
parity <- rbindlist(lapply(c("field", "institution"), function(o) {
  v <- if (o == "field") "high_paying_field_m1" else "high_inst_m1"
  stopifnot(identical(is.na(check[[o]]), is.na(check[[v]])), all(check[[o]] == check[[v]], na.rm = TRUE))
  data.table(outcome = o, n_matched = nrow(check), n_disagreements = sum(check[[o]] != check[[v]], na.rm = TRUE))
}))
fwrite(parity, file.path(out, "main_outcome_parity.csv"))

# Reuse the preceding broad-panel standardization exactly, pooled across genders.
score_stats <- fread(file.path(dirname(out), "score_standardization_by_year.csv"))
stopifnot(!anyDuplicated(score_stats$psu_year))
d <- merge(d, score_stats[, .(psu_year, raw_mean, raw_sd)], by = "psu_year", all.x = TRUE, sort = FALSE)
d[, math_score := NA_real_]
d[!is.na(psu_year) & is.finite(math_max) & math_max > 0 & is.finite(raw_sd) & raw_sd > 0,
  math_score := (math_max - raw_mean) / raw_sd]
score_check <- d[is.finite(math_score), .(n = .N, z_mean = mean(math_score), z_sd = sd(math_score)), by = psu_year]
score_check <- merge(score_check, score_stats[, .(psu_year, n_students)], by = "psu_year")
stopifnot(all(score_check$n == score_check$n_students), all(abs(score_check$z_mean) < 1e-10),
          all(abs(score_check$z_sd - 1) < 1e-10))
fwrite(score_check, file.path(out, "score_reference_validation.csv"))
coverage <- data.table(n_broad_panel = nrow(d), n_observed_math = sum(is.finite(d$math_score)),
  n_missing_or_other_gender_with_score = d[is.finite(math_score), sum(!GEN_ALU %in% c(1L, 2L))])
fwrite(coverage, file.path(out, "gender_coverage.csv"))
print(coverage)
# Verify the preceding pooled plot before excluding unknown gender values.
old_fit <- fread(file.path(dirname(out), "linear_fit_summary.csv"))
pooled_field <- lm(I(100 * high_paying_field_m1) ~ math_score, data = d[is.finite(math_score)])
pooled_check <- data.table(n_students = nobs(pooled_field),
  slope_pp_per_sd = unname(coef(pooled_field)["math_score"]), r_squared = summary(pooled_field)$r.squared)
fwrite(pooled_check, file.path(out, "preceding_pooled_fit_validation.csv"))
stopifnot(nobs(pooled_field) == old_fit$n_students,
  abs(unname(coef(pooled_field)["math_score"]) - old_fit$slope_pp_per_sd) < 1e-9,
  abs(summary(pooled_field)$r.squared - old_fit$r_squared) < 1e-10)
d <- d[is.finite(math_score) & GEN_ALU %in% c(1L, 2L)]
d[, gender := factor(fifelse(GEN_ALU == 1L, "Male", "Female"), levels = c("Male", "Female"))]
# Common pooled-score bins for both genders and outcomes; no sex-specific rescaling.
d[, bin := pmin(50L, as.integer(ceiling(50 * frank(math_score, ties.method = "average") / .N)))]
stopifnot(uniqueN(d$bin) == 50L)
labels <- c("high_paying_field_m1" = "High-premium field", "high_inst_m1" = "High-premium institution")
long <- melt(d, id.vars = c("mrun", "gender", "math_score", "bin", "matriculated_m1"),
             measure.vars = names(labels), variable.name = "outcome", value.name = "enrollment")
long <- long[!is.na(enrollment)]
stopifnot(all(long$enrollment %in% 0:1), all(long[matriculated_m1 == FALSE, enrollment] == 0L))
long[, `:=`(enrollment_pct = 100 * enrollment,
             panel = factor(as.character(outcome), levels = names(labels), labels = unname(labels)))]
bins <- long[, .(n_students = .N, mean_math_score = mean(math_score),
                 enrollment_share_pct = mean(enrollment_pct)), by = .(outcome, panel, gender, bin)]
stopifnot(nrow(bins) == 200L, sum(bins$n_students) == nrow(long))
fits <- list()
lines <- list()
for (o in names(labels)) for (g in c("Male", "Female")) {
  x <- long[outcome == o & gender == g]
  model <- lm(enrollment_pct ~ math_score, data = x)
  id <- paste(o, g, sep = "_")
  fits[[id]] <- data.table(outcome = o, panel = labels[[o]], gender = g, n_students = nrow(x),
    intercept_pp = unname(coef(model)["(Intercept)"]),
    slope_pp_per_sd = unname(coef(model)["math_score"]), r_squared = summary(model)$r.squared,
    mean_enrollment_pct = mean(x$enrollment_pct))
  b <- bins[outcome == o & gender == g]
  grid <- data.table(math_score = seq(min(b$mean_math_score), max(b$mean_math_score), length.out = 250L))
  grid[, `:=`(fitted_pct = as.numeric(predict(model, newdata = grid)), outcome = o,
              panel = labels[[o]], gender = g)]
  lines[[id]] <- grid
}
fits <- rbindlist(fits)
lines <- rbindlist(lines)
lines[, `:=`(panel = factor(panel, levels = unname(labels)), gender = factor(gender, levels = c("Male", "Female")))]
for (o in names(labels)) for (g in c("Male", "Female")) {
  b <- bins[outcome == o & gender == g]
  f <- fits[outcome == o & gender == g]
  stopifnot(sum(b$n_students) == f$n_students,
    abs(weighted.mean(b$enrollment_share_pct, b$n_students) - f$mean_enrollment_pct) < 1e-10)
}

colors <- c("Male" = "#244F78", "Female" = "#BC4B37")
p <- ggplot(bins, aes(x = mean_math_score, y = enrollment_share_pct, color = gender, shape = gender)) +
  geom_point(size = 2.1, alpha = 0.85) +
  geom_line(data = lines, aes(x = math_score, y = fitted_pct), linewidth = 0.85) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = colors) + scale_shape_manual(values = c("Male" = 16, "Female" = 17)) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(z) paste0(z, "%")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "PAES math and higher-education choices, by gender",
       subtitle = "Broad panel, Grade 8 cohorts 2017-2020 | Including private schools | Common score scale across genders",
       x = "PAES/PDT math score (standard deviations within test year)", y = "Enrollment share",
       color = NULL, shape = NULL,
       caption = paste0("Points: gender-specific means within 50 common score-rank bins. Lines: separate OLS fits to all individuals, without controls.\n",
         "First enrollment recorded in 2022-2025; non-enrollees coded zero. Score standardization uses the pooled broad panel.")) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 18, color = "#172B40"),
        plot.subtitle = element_text(size = 10.5, color = "#475569", margin = margin(b = 14)),
        strip.background = element_rect(fill = "#F0F3F6", color = NA),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom", panel.spacing = grid::unit(1.4, "lines"),
        plot.caption = element_text(hjust = 0, size = 9, color = "#5B6470", margin = margin(t = 10)),
        plot.margin = margin(16, 18, 12, 14))
ggsave(file.path(fig, "paes_math_field_institution_by_gender.png"), p, width = 13.5, height = 6.8, dpi = 220, bg = "white")
ggsave(file.path(fig, "paes_math_field_institution_by_gender.pdf"), p, width = 13.5, height = 6.8, bg = "white")
for (o in names(labels)) {
  # Filtering both layers preserves exactly the combined figure's fits and bins.
  single <- ggplot(bins[outcome == o], aes(x = mean_math_score, y = enrollment_share_pct, color = gender, shape = gender)) +
    geom_point(size = 2.4, alpha = 0.85) +
    geom_line(data = lines[outcome == o], aes(x = math_score, y = fitted_pct), linewidth = 0.85) +
    scale_color_manual(values = colors) + scale_shape_manual(values = c("Male" = 16, "Female" = 17)) +
    scale_y_continuous(breaks = seq(0, 100, 20), labels = function(z) paste0(z, "%")) +
    coord_cartesian(ylim = c(0, 100)) + do.call(labs, as.list(p$labels)) + p$theme +
    labs(title = paste0("PAES math and ", tolower(labels[[o]]), " enrollment, by gender"))
  stub <- if (o == "high_paying_field_m1") "field" else "institution"
  ggsave(file.path(fig, paste0("paes_math_", stub, "_by_gender.png")), single, width = 11, height = 6.8, dpi = 220, bg = "white")
}
fwrite(fits, file.path(out, "gender_linear_fit_summary.csv"))
fwrite(bins, file.path(out, "gender_score_bins.csv"))
fwrite(coverage, file.path(out, "gender_coverage.csv"))
print(fits)
print(coverage)
