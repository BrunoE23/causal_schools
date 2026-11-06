suppressPackageStartupMessages({library(data.table); library(ggplot2)})
repo <- normalizePath(getwd(), winslash = "/")
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/xd-br/Dropbox/causal_schools")
base <- file.path(repo, "data", "clean", "paes_field_scatter", "broad_panel", "by_gender")
grouping <- Sys.getenv("VA_GROUPING", "three_groups")
stopifnot(grouping %in% c("three_groups", "quintiles", "terciles"))
variant <- switch(grouping, quintiles = "raw_stata_va_quintiles",
                  terciles = "raw_stata_va_terciles", "raw_stata_va_groups")
quantile_col <- if (grouping == "terciles") "va_tercile" else "va_quintile"
n_quantiles <- if (grouping == "terciles") 3L else 5L
quantile_label <- if (grouping == "terciles") "Terciles" else "Quintiles"
input_path <- file.path(base, "gender_scatter_input.rds")
if (!file.exists(input_path)) {
  source(file.path(repo, "code", "codex", "paes_field_scatter", "02_plot_broad_panel_by_gender.R"),
         local = new.env(parent = globalenv()))
}
out <- file.path(base, variant)
fig <- file.path(repo, "output", "figures", "paes_field_scatter", "broad_panel", "by_gender", variant)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
setDTthreads(2)
labels <- c(high_paying_field_m1 = "High-premium field", high_inst_m1 = "High-premium institution")
groups <- switch(grouping, quintiles = paste0("Q", 1:5), terciles = paste0("T", 1:3), c("Q1", "Q2-Q4", "Q5"))
colors <- setNames(if (grouping == "quintiles") {
  c("#BC4B37", "#C18A32", "#79838F", "#3C8E89", "#244F78")
} else c("#BC4B37", "#79838F", "#244F78"), groups)
legend_labels <- c(paste0(groups[1], " (lowest VA)"), groups[-c(1L, length(groups))],
                   paste0(tail(groups, 1), " (highest VA)"))

x <- as.data.table(readRDS(input_path))
x[, outcome := as.character(outcome)]
stopifnot(nrow(x) == 2L * 645902L, !anyDuplicated(x[, .(mrun, outcome)]),
          all(x$enrollment %in% 0:1), all(is.finite(x$math_score)))

# Only four columns of the roughly 1 GB universe are needed for school linkage.
# Reproduce the preceding broad-panel filter/deduplication before matching IDs.
lookup <- fread(file.path(root, "data", "clean", "univ_gr8_df.csv"),
  select = c("mrun", "cohort_gr8", "EDAD_ALU", "most_time_RBD"),
  na.strings = c("", "NA"), showProgress = FALSE)
lookup <- lookup[cohort_gr8 %in% 2017:2020 & EDAD_ALU %between% c(12, 16) & most_time_RBD > 0]
setkey(lookup, mrun)
lookup <- lookup[!duplicated(mrun), .(mrun, school_rbd = most_time_RBD)]
stopifnot(nrow(lookup) == 931547L, !anyNA(lookup$mrun))
x <- merge(x, lookup, by = "mrun", all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(x$school_rbd), nrow(x) == 2L * 645902L)

# Use the saved pre-centering, pre-EB Stata estimates authorized earlier.
# Each school has equal weight; quintiles are fixed before score/gender filtering.
keys <- c(high_paying_field_m1 = "highpay", high_inst_m1 = "highinst")
va <- rbindlist(lapply(names(keys), function(o) {
  path <- file.path(repo, "output", "tables", "empirical_bayes_school_va",
                    paste0("stata_va_eb_input_", keys[[o]], ".csv"))
  v <- fread(path)[analysis_sample == "All" & is.finite(va_raw)]
  stopifnot(all(v$outcome == o), !anyDuplicated(v$school_rbd), all(v$n_students > 0),
            diff(range(v$va_raw - v$va_centered)) < 1e-6)
  eb <- fread(file.path(dirname(path), paste0("stata_eb_school_values_", keys[[o]], ".csv")))
  check <- merge(v, eb[analysis_sample == "All"],
    by = c("school_rbd", "analysis_sample", "outcome"), suffixes = c("", "_eb"))
  stopifnot(nrow(check) == nrow(v), nrow(check) == nrow(eb[analysis_sample == "All"]),
    all(check$n_students == check$n_students_eb), max(abs(check$va_raw - check$va_raw_eb)) < 1e-6)
  v[, .(school_rbd, outcome, va_raw, n_va_students = n_students, source_path = path)]
}))
va[, (quantile_col) := pmin(n_quantiles,
  as.integer(ceiling(n_quantiles * frank(va_raw, ties.method = "average") / .N))), by = outcome]
if (grouping %in% c("quintiles", "terciles")) {
  va[, va_group := paste0(if (grouping == "terciles") "T" else "Q", get(quantile_col))]
} else {
  va[, va_group := fifelse(va_quintile == 1L, "Q1", fifelse(va_quintile == 5L, "Q5", "Q2-Q4"))]
}
stopifnot(!anyDuplicated(va[, .(school_rbd, outcome)]))
quintiles <- va[, .(n_schools = .N, min_va = min(va_raw), max_va = max(va_raw),
                   mean_va = mean(va_raw)), by = c("outcome", quantile_col)]
stopifnot(nrow(quintiles) == 2L * n_quantiles)
setorderv(quintiles, c("outcome", quantile_col))
fwrite(va, file.path(out, "school_va_group_assignments.csv"))
fwrite(quintiles, file.path(out, paste0("school_", quantile_col, "_definitions.csv")))

x <- merge(x, va[, c("school_rbd", "outcome", "va_raw", quantile_col, "va_group"), with = FALSE],
           by = c("school_rbd", "outcome"), all.x = TRUE, sort = FALSE)
stopifnot(nrow(x) == 2L * 645902L)
coverage <- x[, .(n_preceding_sample = .N, n_va_matched = sum(!is.na(va_group)),
                  n_missing_school_va = sum(is.na(va_group))), by = .(outcome, gender)]
fwrite(coverage, file.path(out, "school_va_match_coverage.csv"))
print(coverage)
x <- x[!is.na(va_group)]
x[, `:=`(va_group = factor(va_group, levels = groups),
          panel = factor(outcome, levels = names(labels), labels = unname(labels)),
          gender = factor(gender, levels = c("Male", "Female")))]
stopifnot(!anyDuplicated(x[, .(mrun, outcome)]))
counts <- x[, .(n_students = .N, n_schools = uniqueN(school_rbd), n_events = sum(enrollment),
                enrollment_share_pct = 100 * mean(enrollment)), by = .(outcome, panel, gender, va_group)]
stopifnot(nrow(counts) == 4L * length(groups))
partition <- merge(counts[, .(n_group_total = sum(n_students)), by = .(outcome, gender)],
                   coverage, by = c("outcome", "gender"))
stopifnot(all(partition$n_group_total == partition$n_va_matched))
fwrite(counts, file.path(out, "group_sample_counts.csv"))
fwrite(partition, file.path(out, "sample_partition_checks.csv"))
saveRDS(x, file.path(out, "classified_individual_input.rds"))

# The preceding 50 pooled-score bins are retained, with separate cell means.
bins <- x[, .(n_students = .N, mean_math_score = mean(math_score),
              enrollment_share_pct = 100 * mean(enrollment)), by = .(outcome, panel, gender, va_group, bin)]
stopifnot(all(bins$bin %in% 1:50), sum(bins$n_students) == nrow(x))
fits <- curves <- list()
for (o in names(labels)) for (g in c("Male", "Female")) for (q in groups) {
  z <- x[outcome == o & gender == g & va_group == q]
  b <- bins[outcome == o & gender == g & va_group == q]
  stopifnot(nrow(z) == sum(b$n_students),
    abs(weighted.mean(b$enrollment_share_pct, b$n_students) - 100 * mean(z$enrollment)) < 1e-9,
    uniqueN(z$enrollment) == 2L)
  model <- glm(enrollment ~ math_score, family = binomial(link = "logit"), data = z)
  fitted <- as.numeric(predict(model, type = "response"))
  stopifnot(model$converged, nobs(model) == nrow(z), all(is.finite(coef(model))),
            all(fitted >= 0 & fitted <= 1))
  id <- paste(o, g, q, sep = "_")
  fits[[id]] <- data.table(outcome = o, panel = labels[[o]], gender = g, va_group = q,
    n_students = nrow(z), n_schools = uniqueN(z$school_rbd),
    intercept_log_odds = unname(coef(model)[1]), slope_log_odds_per_sd = unname(coef(model)[2]),
    mean_enrollment_pct = 100 * mean(z$enrollment), mse = mean((z$enrollment - fitted)^2))
  grid <- data.table(math_score = seq(min(b$mean_math_score), max(b$mean_math_score), length.out = 350))
  grid[, `:=`(fitted_pct = 100 * as.numeric(predict(model, newdata = grid, type = "response")),
              outcome = o, panel = labels[[o]], gender = g, va_group = q)]
  curves[[id]] <- grid
}
fits <- rbindlist(fits)
curves <- rbindlist(curves)
curves[, `:=`(panel = factor(panel, levels = unname(labels)), gender = factor(gender, levels = c("Male", "Female")),
              va_group = factor(va_group, levels = groups))]
fwrite(fits, file.path(out, "logit_fit_summary.csv"))
fwrite(bins, file.path(out, "group_score_bins.csv"))
fwrite(curves, file.path(out, "fitted_curves.csv"))

annotations <- copy(counts)
annotations[, `:=`(x_label = min(bins$mean_math_score) - 0.03,
                    y_label = 97 - 6.5 * (as.integer(va_group) - 1L),
                    n_label = paste0(as.character(va_group), ": N = ", format(n_students, big.mark = ",", trim = TRUE)))]
p <- ggplot(bins, aes(mean_math_score, enrollment_share_pct, color = va_group)) +
  geom_point(size = 1.4, alpha = 0.5) +
  geom_line(data = curves, aes(x = math_score, y = fitted_pct), linewidth = 0.95) +
  geom_text(data = annotations, aes(x_label, y_label, label = n_label), hjust = 0, size = 3.6,
            show.legend = FALSE) +
  facet_grid(panel ~ gender) +
  scale_color_manual(values = colors, breaks = groups, labels = legend_labels) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(v) paste0(v, "%")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "PAES math and enrollment, by school VA and gender",
    subtitle = paste0("Broad panel, including private schools | ", quantile_label,
                       " of the corresponding outcome's saved raw Stata school VA"),
    x = "PAES/PDT math score (standard deviations within test year)", y = "Enrollment share", color = NULL,
    caption = paste0("Lines: separate individual-level logit fits, without controls. Points: cell means within the preceding 50 common score bins. N counts students.\n",
      quantile_label, " give equal weight to schools and use common cutoffs across genders. Raw VA before centering/EB; students without school VA excluded.\n",
      "First enrollment recorded in 2022-2025; non-enrollees coded zero. Descriptive associations; VA estimated using these cohorts' outcomes.")) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 18, color = "#172B40"),
    plot.subtitle = element_text(size = 10.5, color = "#475569", margin = margin(b = 13)),
    strip.background = element_rect(fill = "#F0F3F6", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom", panel.spacing = grid::unit(1.3, "lines"),
    plot.caption = element_text(hjust = 0, size = 9, color = "#5B6470", margin = margin(t = 10)),
    plot.margin = margin(16, 18, 12, 14))
ggsave(file.path(fig, "paes_math_logit_by_va_group_and_gender.png"), p, width = 13.5, height = 10, dpi = 220, bg = "white")
ggsave(file.path(fig, "paes_math_logit_by_va_group_and_gender.pdf"), p, width = 13.5, height = 10, bg = "white")
for (o in names(labels)) {
  # All layers retain the combined figure's estimates, bins, scales and counts.
  single <- p
  single$data <- bins[outcome == o]
  single$layers[[2]]$data <- curves[outcome == o]
  single$layers[[3]]$data <- annotations[outcome == o]
  single <- single + facet_wrap(~gender, nrow = 1) +
    labs(title = paste0("PAES math and ", tolower(labels[[o]]), ", by school VA and gender"))
  stub <- if (o == "high_paying_field_m1") "field" else "institution"
  ggsave(file.path(fig, paste0("paes_math_", stub, "_logit_by_va_group_and_gender.png")),
         single, width = 13.5, height = 6.8, dpi = 220, bg = "white")
}
print(quintiles)
print(counts)
