# User correction: density curves, with artistic schools dropped from plots.
source("code/codex/school_preference_va/common.R")
suppressPackageStartupMessages(library(ggplot2))
figure_dir <- file.path(root, "output/figures", task)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
input <- file.path(clean_dir, "enrollment_weighted_va_distribution_schools.csv")
d <- fread(input)
categories <- c("HC only", "Mixed HC/TP", "TP only")
d <- d[school_type %chin% categories]
check("exactly three requested school types", setequal(d$school_type, categories))
check("unique school dimension observations and valid weights", !anyDuplicated(d[, .(rbd, dimension)]) &&
  all(is.finite(d$va_z)) && all(is.finite(d$whole_school_enrollment) & d$whole_school_enrollment > 0))
counts <- unique(d[, .(rbd, school_type, whole_school_enrollment)])[, .(schools = .N,
  enrollment = sum(whole_school_enrollment)), by = school_type]
weighted_quantile <- function(x, w, p) {
  idx <- order(x); x <- x[idx]; w <- w[idx] / sum(w)
  x[which(cumsum(w) >= p)[1L]]
}

# A shared bandwidth per outcome permits direct comparison of the three curves.
# Weighted Silverman rule: .9*min(weighted SD,weighted IQR/1.34)*n_eff^(-1/5).
# n_eff is a smoothing-rule input, not an inferential sample-size adjustment.
bandwidths <- d[, {
  w <- whole_school_enrollment / sum(whole_school_enrollment)
  mu <- sum(w * va_z)
  weighted_sd <- sqrt(sum(w * (va_z - mu)^2))
  weighted_iqr <- weighted_quantile(va_z, w, .75) - weighted_quantile(va_z, w, .25)
  n_eff <- 1 / sum(w^2)
  h <- .9 * min(weighted_sd, weighted_iqr / 1.34) * n_eff^(-.2)
  .(bandwidth = h, weighted_sd = weighted_sd, weighted_iqr = weighted_iqr, n_eff = n_eff,
    lower = min(va_z) - 5 * h, upper = max(va_z) + 5 * h)
}, by = dimension]
check("positive finite shared bandwidths", nrow(bandwidths) == 4L &&
  all(is.finite(bandwidths$bandwidth) & bandwidths$bandwidth > 0))
curves <- diagnostics <- list()
for (k in seq_len(nrow(bandwidths))) {
  dim_name <- bandwidths$dimension[k]
  h <- bandwidths$bandwidth[k]
  grid <- seq(bandwidths$lower[k], bandwidths$upper[k], length.out = 4096L)
  for (g in categories) {
    x <- d[dimension == dim_name & school_type == g]
    w <- x$whole_school_enrollment / sum(x$whole_school_enrollment)
    f <- numeric(length(grid))
    # Direct Gaussian mixture avoids FFT/binning approximations to the weights.
    for (i in seq_len(nrow(x))) f <- f + w[i] * dnorm(grid, mean = x$va_z[i], sd = h)
    area <- sum(diff(grid) * (head(f, -1L) + tail(f, -1L)) / 2)
    key <- paste(dim_name, g)
    curves[[key]] <- data.table(dimension = dim_name, school_type = g,
      va_z = grid, density = f, bandwidth = h)
    diagnostics[[key]] <- data.table(dimension = dim_name, school_type = g,
      schools = nrow(x), enrollment = sum(x$whole_school_enrollment), bandwidth = h,
      grid_points = length(grid), integrated_density = area, weight_sum = sum(w))
  }
}
curves <- rbindlist(curves)
diagnostics <- rbindlist(diagnostics)
check("all twelve normalized nonnegative densities", nrow(diagnostics) == 12L &&
  all(is.finite(curves$density) & curves$density >= 0) &&
  all(abs(diagnostics$integrated_density - 1) < 1e-6) && all(abs(diagnostics$weight_sum - 1) < 1e-12))
check("identical smoothing across types within each dimension", all(diagnostics[, uniqueN(bandwidth), by = dimension]$V1 == 1L))
curves[, `:=`(school_type = factor(school_type, levels = categories), dimension = factor(dimension, levels = specs$label))]
palette <- c("HC only" = "#0072B2", "Mixed HC/TP" = "#009E73", "TP only" = "#D55E00")
legend_labels <- setNames(paste0(categories, " (", fmt_n(counts$schools[match(categories, counts$school_type)]), " schools)"), categories)
figure <- ggplot(curves, aes(va_z, density, colour = school_type, fill = school_type)) +
  geom_area(position = "identity", alpha = .10, linewidth = 0) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linewidth = .35, colour = "#AAB2BD", linetype = "dotted") +
  facet_wrap(~dimension, ncol = 2L, scales = "free") +
  scale_color_manual(values = palette, labels = legend_labels) +
  scale_fill_manual(values = palette, labels = legend_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, .06))) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(title = "School VA densities by school type",
    subtitle = "Weighted by 2024 school enrollment | Artistic schools excluded",
    x = "School VA (same pooled school-SD units as preference models)", y = "Density", colour = NULL, fill = NULL,
    caption = paste0("National active secondary schools, including private schools: ", fmt_n(sum(counts$schools)),
      " schools; ", fmt_n(sum(counts$enrollment)), " pupils across all grades.\n",
      "Gaussian kernel; common bandwidth within each panel; each curve integrates to 1. VA sample and normalization unchanged.")) +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, colour = "#475569", margin = margin(b = 14)),
    strip.text = element_text(face = "bold", size = 12), panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .3, colour = "#E5EAF0"),
    panel.spacing = grid::unit(1.2, "lines"), legend.position = "bottom", legend.text = element_text(size = 11),
    legend.key.width = grid::unit(1.2, "cm"), plot.caption = element_text(hjust = 0, size = 9.5,
      colour = "#475569", margin = margin(t = 10)), plot.margin = margin(14, 18, 12, 12))
base <- file.path(figure_dir, "enrollment_weighted_va_densities_by_school_type")
ggsave(paste0(base, ".png"), figure, width = 12.5, height = 8.8, dpi = 220, device = ragg::agg_png, bg = "white")
ggsave(paste0(base, ".pdf"), figure, width = 12.5, height = 8.8, device = grDevices::cairo_pdf, bg = "white")
for (k in seq_len(nrow(specs))) {
  label <- specs$label[k]
  p <- figure + curves[dimension == label] + facet_wrap(~dimension, ncol = 1L, scales = "free")
  ggsave(file.path(figure_dir, paste0("enrollment_weighted_va_density_", specs$suffix[k], ".png")),
    p, width = 9, height = 6.8, dpi = 220, device = ragg::agg_png, bg = "white")
}
fwrite(curves, file.path(clean_dir, "enrollment_weighted_va_density_curves.csv"))
fwrite(bandwidths, file.path(clean_dir, "va_density_bandwidths.csv"))
fwrite(diagnostics, file.path(clean_dir, "va_density_diagnostics.csv"))
fwrite(manifest_rows(input), file.path(clean_dir, "va_density_manifest.csv"))
html <- paste0('<!doctype html><html><head><meta charset="utf-8"><title>School VA densities</title>',
  '<style>body{font:16px/1.6 system-ui;margin:30px;color:#172536}main{max-width:1300px;margin:auto}',
  'img{max-width:100%;height:auto}a{color:#175d99}</style></head><body><main>',
  '<h1>Enrollment-weighted VA densities by school type</h1>',
  '<p>HC-only, mixed HC/TP and TP-only schools; artistic schools are excluded as requested. ',
  'Same 2024 whole-school enrollment weights, four-VA-complete national sample and fixed VA normalization as before. ',
  'Private-paid schools are included, and enrollment includes all grades.</p>',
  '<img src="../../figures/', task, '/enrollment_weighted_va_densities_by_school_type.png" alt="Four VA panels with three overlapping density curves each">',
  '<p>Each curve is an enrollment-weighted Gaussian kernel density and integrates to one. ',
  'The bandwidth is shared across types within each dimension:0.9 times the smaller of the pooled weighted SD ',
  'and weighted IQR/1.34, times effective school count to power -1/5. Effective school count is ',
  '(sum of enrollment weights) squared divided by the sum of squared weights; it is used only for graphical smoothing. ',
  'No school VA is trimmed. The grid covers all observed values plus five bandwidths in both tails. ',
  'No demand model, mean or VA estimate is changed.</p>',
  '<p><a href="../../figures/', task, '/enrollment_weighted_va_densities_by_school_type.pdf">Open density PDF</a> | ',
  '<a href="enrollment_weighted_va_by_school_type.html">Mean values and coverage</a></p></main></body></html>')
writeLines(html, file.path(report_dir, "va_distributions_by_school_type.html"), useBytes = TRUE)
check("density figure exports exist", all(file.info(paste0(base, c(".png", ".pdf")))$size > 0))
save_checks("va_density_verification.csv")
print(bandwidths)
message("Saved density PDF: ", paste0(base, ".pdf"))
