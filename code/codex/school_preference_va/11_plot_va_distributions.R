# Exact enrollment-weighted VA distributions for the four school types.
# CDFs retain the two artistic-school atoms instead of smoothing two schools.
source("code/codex/school_preference_va/common.R")
source(file.path(code_dir, "school_types.R"))
suppressPackageStartupMessages(library(ggplot2))
figure_dir <- file.path(root, "output/figures", task)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
types <- read_school_types()
directory_path <- attr(types, "source_path")
normalization_path <- file.path(clean_dir, "va_normalization.csv")
norm <- fread(normalization_path)[match(specs$standardized, standardized_variable)]
v <- fread(va_path, select = c("school_rbd", "analysis_sample", "outcome",
  "controlled_value_added_eb_centered_student"))
v <- v[analysis_sample == "All" & outcome %chin% specs$outcome]
check("VA source unique by school and outcome", !anyDuplicated(v[, .(school_rbd, outcome)]))
v <- dcast(v, school_rbd ~ outcome, value.var = "controlled_value_added_eb_centered_student")
setnames(v, c("school_rbd", specs$outcome), c("rbd", specs$raw))
for (k in seq_len(nrow(specs))) v[, (specs$standardized[k]) :=
  (get(specs$raw[k]) - norm$reference_mean[k]) / norm$reference_sd[k]]
d <- types[directory_status == 1L & directory_has_enrollment == 1L &
  (hc_school == 1L | tp_school == 1L | artistic_school == 1L)]
d <- merge(d, v, by = "rbd", all.x = TRUE, sort = FALSE)
d[, complete_four_va := rowSums(is.finite(as.matrix(.SD))) == length(va_terms), .SDcols = va_terms]
d <- d[complete_four_va == TRUE]
check("one school per row and positive enrollment weights", !anyDuplicated(d$rbd) &&
  all(is.finite(d$whole_school_enrollment) & d$whole_school_enrollment > 0))
categories <- c("HC only", "Mixed HC/TP", "TP only", "Artistic only")
check("four requested school types present", setequal(d$school_type, categories))
counts <- d[, .(schools = .N, enrollment = sum(whole_school_enrollment)), by = school_type]
check("artistic group has exactly two observed schools", counts[school_type == "Artistic only", schools] == 2L)
long <- melt(d[, c("rbd", "school_type", "whole_school_enrollment", va_terms), with = FALSE],
  id.vars = c("rbd", "school_type", "whole_school_enrollment"), variable.name = "term", value.name = "va_z")
long[, `:=`(outcome = specs$outcome[match(term, specs$standardized)],
  dimension = specs$label[match(term, specs$standardized)])]

# Combine ties before calculating the exact cumulative enrolled-pupil mass.
cdf <- long[, .(enrollment_mass = sum(whole_school_enrollment)), by = .(school_type, dimension, va_z)]
setorder(cdf, school_type, dimension, va_z)
cdf[, `:=`(enrollment_share = enrollment_mass / sum(enrollment_mass),
  cumulative_share = cumsum(enrollment_mass) / sum(enrollment_mass)), by = .(school_type, dimension)]
check("all sixteen weighted CDFs are valid", cdf[, .(ok = all(diff(va_z) > 0) &&
  all(diff(cumulative_share) >= 0) && abs(tail(cumulative_share, 1L) - 1) < 1e-12 &&
  abs(sum(enrollment_share) - 1) < 1e-12), by = .(school_type, dimension)][, .N == 16L && all(ok)])
quantiles <- cdf[, {
  probs <- c(.1, .25, .5, .75, .9)
  .(probability = probs, va_z = vapply(probs, function(p) va_z[which(cumulative_share >= p)[1L]], numeric(1)))
}, by = .(school_type, dimension)]
means <- long[, .(standardized_mean = weighted.mean(va_z, whole_school_enrollment),
  schools = .N, enrollment = sum(whole_school_enrollment)), by = .(school_type, outcome, dimension)]
previous <- fread(file.path(clean_dir, "enrollment_weighted_va_by_school_type.csv"))
compare <- merge(means, previous[, .(school_type, outcome, previous_mean = standardized_mean,
  previous_schools = schools, previous_enrollment = enrollment)], by = c("school_type", "outcome"))
check("plots reproduce the previous mean table and population", nrow(compare) == 16L &&
  max(abs(compare$standardized_mean - compare$previous_mean)) < 1e-12 &&
  all(compare$schools == compare$previous_schools) && all(compare$enrollment == compare$previous_enrollment))

# Add only zero/one boundary segments; all actual jumps remain exact school VAs.
bounds <- cdf[, .(lower = min(va_z) - .04 * diff(range(va_z)),
  upper = max(va_z) + .04 * diff(range(va_z))), by = dimension]
ends <- merge(unique(cdf[, .(school_type, dimension)]), bounds, by = "dimension")
plot_data <- rbindlist(list(cdf[, .(school_type, dimension, va_z, cumulative_share)],
  ends[, .(school_type, dimension, va_z = lower, cumulative_share = 0)],
  ends[, .(school_type, dimension, va_z = upper, cumulative_share = 1)]))
setorder(plot_data, school_type, dimension, va_z)
plot_data[, `:=`(school_type = factor(school_type, levels = categories),
  dimension = factor(dimension, levels = specs$label))]
art_points <- copy(cdf[school_type == "Artistic only"])
art_points[, `:=`(school_type = factor(school_type, levels = categories),
  dimension = factor(dimension, levels = specs$label))]
palette <- c("HC only" = "#0072B2", "Mixed HC/TP" = "#009E73", "TP only" = "#D55E00", "Artistic only" = "#CC79A7")
line_types <- c("HC only" = "solid", "Mixed HC/TP" = "solid", "TP only" = "solid", "Artistic only" = "longdash")
legend_labels <- setNames(paste0(categories, " (", fmt_n(counts$schools[match(categories, counts$school_type)]), " schools)"), categories)
figure <- ggplot(plot_data, aes(va_z, cumulative_share, colour = school_type, linetype = school_type)) +
  geom_vline(xintercept = 0, linewidth = .35, colour = "#AAB2BD", linetype = "dotted") +
  geom_step(direction = "hv", linewidth = .85) +
  geom_point(data = art_points, size = 1.7, show.legend = FALSE) +
  facet_wrap(~dimension, ncol = 2L, scales = "free_x") +
  scale_color_manual(values = palette, labels = legend_labels, drop = FALSE) +
  scale_linetype_manual(values = line_types, labels = legend_labels, drop = FALSE) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(0, 1, .25),
    limits = c(0, 1), expand = expansion(mult = c(.015, .015))) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(title = "School VA distributions by school type",
    subtitle = "Exact cumulative distributions, weighted by 2024 school enrollment",
    x = "School VA (same pooled school-SD units as preference models)",
    y = "Share of enrollment at or below each VA", colour = NULL, linetype = NULL,
    caption = paste0("National active secondary schools, including private schools: ", fmt_n(nrow(d)),
      " schools; ", fmt_n(sum(d$whole_school_enrollment)), " pupils across all grades.\n",
      "Same four-VA-complete sample and fixed normalization as the preceding means. Artistic: only 2 schools; no smoothing.")) +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, colour = "#475569", margin = margin(b = 14)),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_line(linewidth = .25, colour = "#EDF0F3"),
    panel.grid.major.y = element_line(linewidth = .3, colour = "#DFE5EB"),
    panel.spacing = grid::unit(1.2, "lines"), legend.position = "bottom", legend.box = "horizontal",
    legend.text = element_text(size = 11), legend.key.width = grid::unit(1.6, "cm"),
    plot.caption = element_text(hjust = 0, colour = "#475569", size = 9.5, margin = margin(t = 10)),
    axis.title = element_text(size = 11), plot.margin = margin(14, 18, 12, 12)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))
base <- file.path(figure_dir, "enrollment_weighted_va_cdfs_by_school_type")
ggsave(paste0(base, ".png"), figure, width = 12.5, height = 8.8, dpi = 220, device = ragg::agg_png, bg = "white")
ggsave(paste0(base, ".pdf"), figure, width = 12.5, height = 8.8, device = grDevices::cairo_pdf, bg = "white")
for (k in seq_len(nrow(specs))) {
  label <- specs$label[k]
  individual <- figure + plot_data[dimension == label] + facet_wrap(~dimension, ncol = 1L, scales = "free_x")
  # Replace the artistic layer data too so a single panel is genuinely single.
  individual$layers[[3L]]$data <- art_points[dimension == label]
  ggsave(file.path(figure_dir, paste0("enrollment_weighted_va_cdf_", specs$suffix[k], ".png")),
    individual, width = 9, height = 6.8, dpi = 220, device = ragg::agg_png, bg = "white")
}
fwrite(cdf, file.path(clean_dir, "enrollment_weighted_va_ecdf.csv"))
fwrite(quantiles, file.path(clean_dir, "enrollment_weighted_va_quantiles.csv"))
fwrite(long, file.path(clean_dir, "enrollment_weighted_va_distribution_schools.csv"))
fwrite(manifest_rows(c(va_path, directory_path, normalization_path)), file.path(clean_dir, "va_distribution_manifest.csv"))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>School VA distributions</title>',
  '<style>body{font:16px/1.6 system-ui;margin:30px;color:#172536}main{max-width:1300px;margin:auto}',
  'img{max-width:100%;height:auto}a{color:#175d99}</style></head><body><main>',
  '<h1>Enrollment-weighted VA distributions by school type</h1>',
  '<p>The four groups are HC-only, mixed HC/TP, TP-only and artistic-only schools. ',
  'Use the same national population, four jointly observed EB VAs, normalization and 2024 whole-school enrollment ',
  'weights as the preceding mean table. Private-paid schools are included. Enrollment covers all grades.</p>',
  '<p>At a given VA on the horizontal axis, each curve gives the share of that type\'s enrollment ',
  'in schools with VA at or below that value. A distribution farther to the right has higher VA. ',
  'The artistic group has only two schools: its jumps and marked points show their actual enrollment masses. ',
  'No kernel, bandwidth, tail trimming or interpolation is used.</p>',
  '<img src="../../figures/', task, '/enrollment_weighted_va_cdfs_by_school_type.png" alt="Four VA panels with four school-type cumulative distributions each">',
  '<p><a href="../../figures/', task, '/enrollment_weighted_va_cdfs_by_school_type.pdf">Download vector PDF</a> | ',
  '<a href="enrollment_weighted_va_by_school_type.html">Mean values and coverage</a></p>',
  '</main></body></html>')
writeLines(html, file.path(report_dir, "va_distributions_by_school_type.html"), useBytes = TRUE)
check("figure exports exist and nonempty", all(file.info(paste0(base, c(".png", ".pdf")))$size > 0))
save_checks("va_distribution_verification.csv")
message("Saved combined and individual VA distribution plots: ", base)
print(dcast(quantiles[probability %in% c(.1, .5, .9)], school_type + probability ~ dimension, value.var = "va_z"))
