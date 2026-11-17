# Filled, stacked regional histograms using the existing VA plotting samples.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
base <- file.path(repo, "data/clean/lottery_variation_by_region")
out <- file.path(base, "histograms")
fig <- file.path(repo, "output/figures/lottery_variation_by_region/histogram")
report <- file.path(repo, "output/reports/lottery_variation_by_region")
for (p in c(out, fig, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
specs <- data.table(measure = c("math", "field", "institution"),
  title = c("Math", "High-premium field", "High-premium institution"),
  unit = c("test-score SD", "percentage points", "percentage points"), scale = c(1, 100, 100))
groups <- c("Always", "Sometimes", "Never")
colors <- c(Always = "#0072B2", Sometimes = "#E69F00", Never = "#D55E00")
n_bins <- 30L
inputs <- file.path(base, paste0("va_", specs$measure), "school_plot_data.csv")
stopifnot(all(file.exists(inputs)), all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
frames <- lapply(inputs, function(p) fread(p)[order(rbd)])
shared <- c("rbd", "region_code", "region", "region_order", "classification", "years_observed", "years_with_variation", "va_observed")
stopifnot(all(vapply(frames, function(x) identical(x[, ..shared], frames[[1L]][, ..shared]), logical(1))))
regions <- unique(frames[[1L]][, .(region_order, region_code, region)])[order(region_order)]
stopifnot(nrow(regions) == 16L, setequal(regions$region_code, 1:16))
fwrite(data.table(path = inputs, md5 = hashes), file.path(out, "input_manifest.csv"))
audits <- list()
plot_specs <- list()

for (i in seq_len(nrow(specs))) {
  spec <- specs[i]
  d <- frames[[i]]
  a <- copy(d[va_observed == TRUE])
  stopifnot(nrow(d) == 2529L, nrow(a) == 2504L, !anyDuplicated(d$rbd),
    all(is.finite(a$va)), all(abs(a$va - a$va_original * spec$scale) < 1e-12),
    identical(as.integer(table(factor(a$classification, levels = groups))), c(2194L, 296L, 14L)))
  # Same 30 bins in every region; include the upper endpoint in the final bin.
  edges <- seq(min(a$va), max(a$va), length.out = n_bins + 1L)
  width <- edges[2L] - edges[1L]
  a[, bin := cut(va, breaks = edges, right = FALSE, include.lowest = TRUE, labels = FALSE)]
  stopifnot(!anyNA(a$bin), all(a$bin >= 1L & a$bin <= n_bins))
  counts <- a[, .(schools = .N), by = .(region_code, classification, bin)]
  bins <- regions[, CJ(classification = groups, bin = seq_len(n_bins), sorted = FALSE),
                  by = .(region_order, region_code, region)]
  bins <- merge(bins, counts, by = c("region_code", "classification", "bin"), all.x = TRUE)
  bins[is.na(schools), schools := 0L]
  coverage <- d[, .(regional_schools = sum(va_observed), classified = .N), by = region_code]
  bins <- merge(bins, coverage, by = "region_code", all.x = TRUE)
  bins[, `:=`(left = edges[bin], right = edges[bin + 1L],
    center = (edges[bin] + edges[bin + 1L]) / 2, share = schools / regional_schools)]
  # The denominator is all plotted schools in the region, not the color group.
  checks <- bins[, .(histogram_schools = sum(schools), share_sum = sum(share),
    regional_schools = unique(regional_schools)), by = .(region_code, region)]
  expected <- a[, .(schools = .N), by = .(region_code, classification)][order(region_code, classification)]
  actual <- bins[, .(schools = sum(schools)), by = .(region_code, classification)][schools > 0][order(region_code, classification)]
  stopifnot(sum(bins$schools) == nrow(a), identical(expected, actual),
    all(checks$histogram_schools == checks$regional_schools), all(abs(checks$share_sum - 1) < 1e-12))
  labels <- bins[, .(schools = sum(schools)), by = .(region_code, region, classification)]
  peak <- max(bins[, .(share = sum(share)), by = .(region_code, bin)]$share)
  labels[, `:=`(x = edges[1L] + diff(range(edges)) * c(.04, .39, .76)[match(classification, groups)],
    y = peak * 1.13, label = paste0(c("A", "S", "N")[match(classification, groups)], ": ", schools))]
  setorder(bins, region_order, bin, classification)
  fwrite(bins, file.path(out, paste0(spec$measure, "_bin_counts.csv")))
  fwrite(a[, .(rbd, region_code, classification, va, bin)], file.path(out, paste0(spec$measure, "_school_bins.csv")))
  for (obj in c("bins", "labels")) {
    z <- get(obj)
    z[, `:=`(region = factor(region, levels = regions$region), classification = factor(classification, levels = groups))]
    assign(obj, z)
  }
  make_plot <- function(region_id = NULL) {
    bb <- if (is.null(region_id)) bins else bins[region_code == region_id]
    ll <- if (is.null(region_id)) labels else labels[region_code == region_id]
    title <- if (is.null(region_id)) paste0(spec$title, " value added by school region") else
      paste0(regions[region_code == region_id, region], ": ", tolower(spec$title), " value added")
    subtitle <- if (is.null(region_id)) "EB-adjusted VA, cohorts 2017-2020 | 2,504 schools | Lottery variation in SAE 2018-2020" else
      sprintf("EB-adjusted VA, cohorts 2017-2020 | %s of %s regional schools with VA | SAE 2018-2020",
        coverage[region_code == region_id, regional_schools], coverage[region_code == region_id, classified])
    p <- ggplot(bb, aes(center, share, fill = classification)) +
      geom_col(width = width, position = position_stack(reverse = TRUE), color = "white", linewidth = .10) +
      geom_vline(xintercept = 0, color = "#596777", linetype = "dashed", linewidth = .30) +
      geom_text(data = ll, aes(x, y, label = label, color = classification), inherit.aes = FALSE,
        hjust = 0, size = 2.8, fontface = "bold", show.legend = FALSE) +
      scale_fill_manual(values = colors, limits = groups, breaks = groups, drop = FALSE) +
      scale_color_manual(values = colors, limits = groups, guide = "none") +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
      scale_y_continuous(breaks = scales::breaks_pretty(n = 4)(c(0, peak)), labels = scales::label_percent(accuracy = 1)) +
      coord_cartesian(xlim = range(edges), ylim = c(0, peak * 1.25), expand = FALSE) +
      labs(title = title, subtitle = subtitle, x = paste0(spec$title, " VA (", spec$unit, ")"),
        y = "Share of schools in region", fill = "Lottery variation",
        caption = paste0("Stacked bars: share of all schools with VA in that region. Each school has equal weight; all bars together sum to 100%.\n",
          "30 equal-width bins and common axes across regions. A = Always, S = Sometimes, N = Never; labels give school counts.\n",
          "Groups refer to observed SAE offering years. All group sizes are included. Nationally, 25 classified schools lack VA.")) +
      theme_minimal(base_size = 11, base_family = "sans") +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E9EDF1", linewidth = .3),
        strip.text = element_text(face = "bold", size = 10.5, color = "#233143"),
        panel.spacing = grid::unit(1.15, "lines"),
        plot.title = element_text(face = "bold", size = 19, color = "#233143"),
        plot.subtitle = element_text(size = 10.5, color = "#526171", margin = margin(b = 10)),
        plot.caption = element_text(size = 9, color = "#526171", hjust = 0, margin = margin(t = 10)),
        legend.position = "top", legend.justification = "left", legend.title = element_text(size = 10),
        legend.key.size = grid::unit(.9, "lines"), legend.margin = margin(0, 0, 5, 0), plot.margin = margin(14, 18, 12, 12))
    if (is.null(region_id)) p <- p + facet_wrap(vars(region), ncol = 4, drop = FALSE)
    if (!is.null(region_id)) p <- p + theme(plot.title = element_text(size = 17), plot.caption = element_text(size = 8))
    p
  }
  destination <- file.path(fig, spec$measure)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  overview <- make_plot()
  ggsave(file.path(destination, "all_regions.png"), overview, width = 15, height = 12, dpi = 220, bg = "white")
  ggsave(file.path(destination, "all_regions.svg"), overview, width = 15, height = 12, device = grDevices::svg, bg = "white")
  for (region_id in regions$region_code) ggsave(file.path(destination, sprintf("region_%02d.png", region_id)),
    make_plot(region_id), width = 8.8, height = 5.9, dpi = 180, bg = "white")
  audits[[i]] <- cbind(measure = spec$measure, checks)
  plot_specs[[i]] <- data.table(measure = spec$measure, bins = n_bins, bin_width = width,
    x_min = edges[1L], x_max = tail(edges, 1L), y_max = peak * 1.25, plotted_schools = nrow(a),
    missing_va = nrow(d) - nrow(a), weighting = "one per school", denominator = "all plotted schools in region")
  message("Saved ", spec$measure, ": 16 regional histograms and overview; all 2,504 schools accounted for.")
}

fwrite(rbindlist(audits), file.path(out, "region_verification.csv"))
fwrite(rbindlist(plot_specs), file.path(out, "plot_specifications.csv"))
paths <- unlist(lapply(specs$measure, function(k) file.path(fig, k,
  c("all_regions.png", "all_regions.svg", sprintf("region_%02d.png", regions$region_code)))))
stopifnot(length(paths) == 54L, all(file.exists(paths)), identical(unname(tools::md5sum(inputs)), hashes))
fwrite(data.table(check = c("Identical school sample and classifications across all three measures",
  "Original VA units retained", "Every observed school assigned to exactly one bin",
  "Bin counts reproduce regional and group counts", "Shares sum to one within each region",
  "48 regional PNGs and three PNG/SVG overviews exist", "Cached source inputs unchanged"), passed = TRUE),
  file.path(out, "verification.csv"))
rel <- "../../figures/lottery_variation_by_region/histogram/"
overviews <- vapply(seq_len(nrow(specs)), function(i) paste0('<section id="', specs$measure[i], '"><h2>', specs$title[i], ' VA</h2>',
  '<p class="note">Units: ', specs$unit[i], '. <a href="', rel, specs$measure[i], '/all_regions.svg">Vector overview</a></p>',
  '<a href="', rel, specs$measure[i], '/all_regions.png"><img src="', rel, specs$measure[i], '/all_regions.png" alt="',
  specs$title[i], ' VA histograms in all 16 regions"></a></section>'), character(1))
regional <- vapply(seq_len(nrow(regions)), function(j) paste0('<details><summary>', regions$region[j], '</summary>',
  paste(vapply(seq_len(nrow(specs)), function(i) paste0('<figure><figcaption>', specs$title[i], ' VA</figcaption><a href="',
    rel, specs$measure[i], '/', sprintf('region_%02d.png', regions$region_code[j]), '"><img loading="lazy" src="',
    rel, specs$measure[i], '/', sprintf('region_%02d.png', regions$region_code[j]), '" alt="', specs$title[i], ' VA histogram in ',
    regions$region[j], '"></a></figure>'), character(1)), collapse = ''), '</details>'), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Regional VA histograms</title>',
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1500px;margin:28px auto;padding:0 24px}h1{font-size:30px}h2{margin-top:36px}nav{display:flex;gap:22px;flex-wrap:wrap;padding:15px 0}a{color:#0072b2}img{width:100%;height:auto}figure{margin:20px 0}figcaption,summary{font-weight:650}summary{cursor:pointer;padding:14px;background:#eef3f6;margin-top:12px}.note{font-size:14px;color:#526171}.legend{display:flex;gap:24px;flex-wrap:wrap;font-weight:600}.always{color:#0072B2}.sometimes{color:#B57700}.never{color:#D55E00}</style></head><body>',
  '<h1>Value-added histograms across all 16 school regions</h1><p>The same <b>2,504 schools</b> appear in all three measures: 2,194 always, 296 sometimes, and 14 never have observed lottery variation.</p>',
  '<div class="legend"><span class="always">Blue: always</span><span class="sometimes">Orange: sometimes</span><span class="never">Red: never</span></div>',
  '<nav><a href="#math">Math</a><a href="#field">High-premium field</a><a href="#institution">High-premium institution</a><a href="#by-region">Individual regions</a></nav>',
  '<p>Each bar is the share of all schools with VA in that region whose VA falls in the bin. Its colored segments show the lottery categories. All bars together sum to 100% within each region; a color\'s total share equals its fraction of schools in that region. Each school has equal weight.</p>',
  '<p class="note">There are 30 equal-width bins spanning the full observed VA range, with common bin boundaries and axis scales across regions within each measure. All group sizes are included. Math uses test-score SD; field and institution use percentage points. These histograms use the whole region as the denominator; the previous density curves normalized separately within each lottery group.</p>',
  '<p class="note">The 2017-2020 All-sample EB-adjusted observational VA values and their original centering are unchanged. The same 25 classified schools lack VA. Lottery variation means at least one simulated grade-9 applicant with 0 &lt; p &lt; 1 in a school-year, after summing programs within school (1e-10 endpoint tolerance). Always/sometimes/never refers to observed SAE offering years in 2018-2020; Metropolitana enters in 2019. <a href="report.html">Classification details</a>.</p>',
  overviews, '<section id="by-region"><h2>All three measures within each region</h2><p>Expand a region; click any image to view its full resolution.</p>', regional, '</section></body></html>')
writeLines(html, file.path(report, "histogram_all_three.html"), useBytes = TRUE)
message("Saved combined histogram gallery. All count, share, support, unit, and source-integrity checks passed.")
