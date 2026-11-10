# Assemble the three completed VA measures; verify identical school support.
suppressPackageStartupMessages(library(data.table))
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
base <- file.path(repo, "data/clean/lottery_variation_by_region")
report <- file.path(repo, "output/reports/lottery_variation_by_region")
fig <- file.path(repo, "output/figures/lottery_variation_by_region")
keys <- c("math", "field", "institution")
titles <- c("Math VA", "High-premium field VA", "High-premium institution VA")
units <- c("Test-score standard deviations", "Percentage points", "Percentage points")
frames <- lapply(keys, function(k) fread(file.path(base, paste0("va_", k), "school_plot_data.csv"))[order(rbd)])
checks <- lapply(keys, function(k) fread(file.path(base, paste0("va_", k), "verification.csv")))
specs <- rbindlist(lapply(keys, function(k) fread(file.path(base, paste0("va_", k), "plot_specification.csv"))))
shared <- c("rbd", "region_code", "region", "classification", "years_observed", "years_with_variation", "va_observed")
stopifnot(all(vapply(checks, function(x) all(x$passed), logical(1))),
  all(vapply(frames, function(x) identical(x[, ..shared], frames[[1L]][, ..shared]), logical(1))),
  all(specs$plotted_schools == 2504L), all(specs$missing_va == 25L))
for (i in seq_along(keys)) stopifnot(all(abs(frames[[i]][va_observed == TRUE, va - va_original * c(1, 100, 100)[i]]) < 1e-12))
regions <- unique(frames[[1L]][, .(region_order, region_code, region)])[order(region_order)]
paths <- unlist(lapply(keys, function(k) c(file.path(fig, k, c("all_regions.png", "all_regions.svg")),
  file.path(fig, k, sprintf("region_%02d.png", regions$region_code)), file.path(report, paste0("va_", k, ".html")))))
stopifnot(nrow(regions) == 16L, all(file.exists(paths)))
fwrite(specs, file.path(base, "all_va_plot_specifications.csv"))
fwrite(data.table(check = c("Identical school IDs and lottery categories across all three measures",
  "Identical VA availability across measures", "All measure-level checks pass", "Probability VAs scaled to percentage points",
  "48 regional images and all overviews/galleries exist"), passed = TRUE), file.path(base, "all_va_gallery_verification.csv"))
rel <- "../../figures/lottery_variation_by_region/"
overview <- vapply(seq_along(keys), function(i) paste0('<section id="', keys[i], '"><h2>', titles[i], '</h2><p class="note">', units[i],
  '. <a href="va_', keys[i], '.html">Open all 16 individual plots</a> | <a href="', rel, keys[i], '/all_regions.svg">Vector overview</a></p>',
  '<a href="', rel, keys[i], '/all_regions.png"><img loading="lazy" src="', rel, keys[i], '/all_regions.png" alt="', titles[i], ' distributions in all 16 regions"></a></section>'), character(1))
region_plots <- vapply(seq_len(nrow(regions)), function(j) paste0('<details><summary>', regions$region[j], '</summary>',
  paste(vapply(seq_along(keys), function(i) paste0('<figure><figcaption>', titles[i], '</figcaption><a href="', rel, keys[i], '/', sprintf('region_%02d.png', regions$region_code[j]), '">',
    '<img loading="lazy" src="', rel, keys[i], '/', sprintf('region_%02d.png', regions$region_code[j]), '" alt="', titles[i], ' in ', regions$region[j], '"></a></figure>'), character(1)), collapse = ''), '</details>'), character(1))
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Three VA distributions across 16 regions</title>',
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1500px;margin:28px auto;padding:0 24px}h1{font-size:30px}h2{margin-top:36px;font-size:25px}nav{display:flex;gap:22px;flex-wrap:wrap;padding:15px 0}a{color:#0072b2}img{width:100%;height:auto}figure{margin:20px 0}figcaption,summary{font-weight:650}summary{cursor:pointer;padding:14px;background:#eef3f6;margin-top:12px}.note{font-size:14px;color:#526171}.legend{display:flex;gap:24px;flex-wrap:wrap;font-weight:600}.always{color:#0072B2}.sometimes{color:#B57700}.never{color:#D55E00}section{scroll-margin-top:20px}</style></head><body>',
  '<h1>Value-added distributions across all 16 school regions</h1><p>Three EB-adjusted measures: math, high-premium field, and high-premium institution. The same <b>2,504 schools</b> appear in every set: 2,194 always, 296 sometimes, and 14 never have observed lottery variation.</p>',
  '<div class="legend"><span class="always">Blue: always</span><span class="sometimes">Orange: sometimes</span><span class="never">Red: never</span></div>',
  '<nav><a href="#math">Math</a><a href="#field">High-premium field</a><a href="#institution">High-premium institution</a><a href="#by-region">Compare all three within a region</a></nav>',
  '<p class="note">School weights are equal. Each curve is a distribution within a region and lottery group, normalized to area one. Groups with fewer than five schools have ticks only. Axes and smoothing are common across regions within each VA measure; each measure retains its own units and bandwidth. Math is in test-score SD; the two enrollment VAs are in percentage points.</p>',
  '<p class="note">All measures use the paper\'s 2017-2020 All-sample EB-adjusted observational school values. No high-math restriction or regional recentering. The same 25 classified schools lack each VA measure. Lottery groups use the earlier SAE 2018-2020 classification over observed offering years; Metropolitana enters in 2019. <a href="report.html">Classification and year-coverage details</a>.</p>',
  overview, '<section id="by-region"><h2>Compare the three measures within a region</h2><p>Expand a region to see its three individual plots. Click any image to open the full-resolution file.</p>', region_plots,
  '</section></body></html>')
writeLines(html, file.path(report, "va_all_three.html"), useBytes = TRUE)
message("Verified common 2,504-school support; saved gallery with all 48 regional plots.")
