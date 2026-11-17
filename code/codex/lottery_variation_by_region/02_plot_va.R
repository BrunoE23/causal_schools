# Regional EB VA distributions, colored by the saved lottery persistence groups.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
setDTthreads(4L)
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
repo <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = getwd())
data_root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
measure <- if (length(args)) args[1L] else "math"
specs <- data.table(measure = c("math", "field", "institution"),
  outcome = c("z_year_math_max", "high_paying_field_m1", "high_inst_m1"),
  title = c("Math", "High-premium field", "High-premium institution"),
  unit = c("test-score SD", "percentage points", "percentage points"), scale = c(1, 100, 100))
selected_measure <- measure
spec <- specs[measure == selected_measure]
stopifnot(nrow(spec) == 1L)
base <- file.path(repo, "data/clean/lottery_variation_by_region")
out <- file.path(base, paste0("va_", measure))
fig <- file.path(repo, "output/figures/lottery_variation_by_region", measure)
report <- file.path(repo, "output/reports/lottery_variation_by_region")
for (p in c(out, fig, report)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
class_path <- file.path(base, "school_classification.csv")
va_path <- file.path(data_root, "data/clean/empirical_bayes_school_va/cohorts_2017_2020/eb_school_rbd_observational_values.csv")
inputs <- c(class_path, va_path)
stopifnot(all(file.exists(inputs)), all(file.info(inputs)$size < 100e6))
hashes <- unname(tools::md5sum(inputs))
fwrite(data.table(path = inputs, bytes = file.info(inputs)$size, md5 = hashes), file.path(out, "input_manifest.csv"))
d <- fread(class_path)
stopifnot(nrow(d) == 2529L, !anyDuplicated(d$rbd), setequal(d$region_code, 1:16))
v <- fread(va_path, select = c("outcome", "school_rbd", "analysis_sample", "controlled_value_added_eb_centered_student"))
v <- v[analysis_sample == "All" & outcome == spec$outcome]
setnames(v, c("school_rbd", "controlled_value_added_eb_centered_student"), c("rbd", "va_original"))
stopifnot(!anyDuplicated(v$rbd))
d <- merge(d, v[, .(rbd, va_original)], by = "rbd", all.x = TRUE, sort = FALSE)
d[, `:=`(va_observed = is.finite(va_original), va = va_original * spec$scale)]
groups <- c("Always", "Sometimes", "Never")
colors <- c(Always = "#0072B2", Sometimes = "#E69F00", Never = "#D55E00")
linetypes <- c(Always = "solid", Sometimes = "longdash", Never = "dotted")
region_map <- unique(d[, .(region_order, region_code, region)])[order(region_order)]
region_map[region_code == 1, region := "Tarapac\u00e1"]
region_map[region_code == 5, region := "Valpara\u00edso"]
region_map[region_code == 8, region := "Biob\u00edo"]
region_map[region_code == 9, region := "La Araucan\u00eda"]
region_map[region_code == 11, region := "Ays\u00e9n"]
region_map[region_code == 14, region := "Los R\u00edos"]
region_map[region_code == 16, region := "\u00d1uble"]
d[, region := region_map$region[match(region_code, region_map$region_code)]]
counts <- d[, .(classified = .N, plotted = sum(va_observed), missing_va = sum(!va_observed)),
            by = .(region_order, region_code, region, classification)]
grid <- region_map[, .(classification = groups), by = .(region_order, region_code, region)]
counts <- merge(grid, counts, by = c("region_order", "region_code", "region", "classification"), all.x = TRUE)
for (k in c("classified", "plotted", "missing_va")) set(counts, which(is.na(counts[[k]])), k, 0L)
setorder(counts, region_order, classification)
fwrite(counts, file.path(out, "coverage_by_region_group.csv"))
fwrite(d[va_observed == FALSE], file.path(out, "schools_without_va.csv"))
fwrite(d, file.path(out, "school_plot_data.csv"))
a <- d[va_observed == TRUE]
stopifnot(nrow(a) > 0, uniqueN(a$region_code) == 16L, !anyDuplicated(a$rbd),
  sum(counts$plotted) == nrow(a), sum(counts$missing_va) == nrow(d) - nrow(a))
# Shared bandwidth and original VA scale: no within-region recentering or scaling.
bandwidth <- bw.nrd0(a$va)
stopifnot(is.finite(bandwidth), bandwidth > 0)
xrange <- range(a$va) + c(-1, 1) * 4 * bandwidth
density_min_n <- 5L
curves <- a[, {
  if (.N < density_min_n) NULL else {
    z <- density(va, bw = bandwidth, kernel = "gaussian", from = xrange[1L], to = xrange[2L], n = 1024L)
    .(x = z$x, density = z$y)
  }
}, by = .(region_code, region, classification)]
areas <- curves[, .(area = sum(diff(x) * (head(density, -1L) + tail(density, -1L)) / 2)),
                by = .(region_code, classification)]
stopifnot(all(abs(areas$area - 1) < .005))
expected_curves <- counts[plotted >= density_min_n, .(region_code, classification)]
stopifnot(nrow(areas) == nrow(expected_curves),
  nrow(merge(areas, expected_curves, by = c("region_code", "classification"))) == nrow(expected_curves))
ymax <- max(curves$density) * 1.08
rugs <- copy(a)
rugs[, rug_y := -ymax * (.035 + .045 * (match(classification, groups) - 1L))]
rugs[, rug_yend := rug_y - .025 * ymax]
labels <- copy(counts)
labels[, `:=`(x = xrange[1L] + diff(xrange) * c(.07, .40, .76)[match(classification, groups)],
  y = ymax * 1.06, label = paste0(c("A", "S", "N")[match(classification, groups)], ": ", plotted))]
subtitle <- sprintf("EB-adjusted VA, cohorts 2017-2020 | %s schools | Colors: lottery variation in SAE 2018-2020",
                    format(nrow(a), big.mark = ",", trim = TRUE))
for (obj in c("curves", "rugs", "labels")) {
  z <- get(obj)
  z[, `:=`(region = factor(region, levels = region_map$region), classification = factor(classification, levels = groups))]
  assign(obj, z)
}
fwrite(curves, file.path(out, "density_curves.csv"))
fwrite(areas, file.path(out, "density_integral_checks.csv"))
fwrite(data.table(measure, bandwidth, density_min_n, x_min = xrange[1L], x_max = xrange[2L],
  density_max = ymax, plotted_schools = nrow(a), missing_va = nrow(d) - nrow(a), weighting = "one per school"),
  file.path(out, "plot_specification.csv"))

make_plot <- function(region_id = NULL) {
  dc <- if (is.null(region_id)) curves else curves[region_code == region_id]
  rr <- if (is.null(region_id)) rugs else rugs[region_code == region_id]
  ll <- if (is.null(region_id)) labels else labels[region_code == region_id]
  title <- if (is.null(region_id)) paste0(spec$title, " value added by school region") else
    paste0(region_map[region_code == region_id, region], ": ", tolower(spec$title), " value added")
  plot_subtitle <- if (is.null(region_id)) subtitle else sprintf(
    "EB-adjusted VA, cohorts 2017-2020 | %s of %s regional schools with VA | SAE 2018-2020",
    format(counts[region_code == region_id, sum(plotted)], big.mark = ",", trim = TRUE),
    format(counts[region_code == region_id, sum(classified)], big.mark = ",", trim = TRUE))
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "#AEB9C4", linewidth = .3) +
    geom_vline(xintercept = 0, color = "#C4CBD2", linetype = "dashed", linewidth = .35) +
    geom_area(data = dc, aes(x, density, fill = classification, group = classification), stat = "identity", position = "identity", alpha = .055, show.legend = FALSE) +
    geom_line(data = dc, aes(x, density, color = classification, linetype = classification), linewidth = .72, show.legend = TRUE) +
    geom_segment(data = rr[classification != "Never"], aes(x = va, xend = va, y = rug_y, yend = rug_yend, color = classification), linewidth = .3, alpha = .55, show.legend = FALSE) +
    geom_segment(data = rr[classification == "Never"], aes(x = va, xend = va, y = rug_y, yend = rug_yend, color = classification), linewidth = .85, show.legend = FALSE) +
    geom_text(data = ll, aes(x, y, label = label, color = classification), hjust = 0, size = 2.7, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = colors, breaks = groups, limits = groups, drop = FALSE) +
    scale_fill_manual(values = colors, breaks = groups, limits = groups, drop = FALSE) +
    scale_linetype_manual(values = linetypes, breaks = groups, limits = groups, drop = FALSE) +
    guides(fill = "none", linetype = "none", colour = guide_legend(
      override.aes = list(colour = unname(colors), linetype = unname(linetypes), alpha = 1))) +
    scale_x_continuous(limits = xrange, breaks = scales::breaks_pretty(n = 5), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(-.17 * ymax, 1.15 * ymax), breaks = scales::breaks_pretty(n = 4)(c(0, ymax)), expand = expansion(mult = 0)) +
    labs(title = title, subtitle = plot_subtitle, x = paste0(spec$title, " VA (", spec$unit, ")"),
      y = "Density within lottery group", color = "Lottery variation", linetype = "Lottery variation",
      caption = paste0("A = Always, S = Sometimes, N = Never; labels give school counts with VA. Each school has equal weight.\n",
        "Curves use a common bandwidth and integrate to 1 within each group. Groups with fewer than 5 schools show ticks only.\n",
        "Ticks show individual schools (top to bottom: A, S, N). Same axes throughout. Nationally, ", nrow(d) - nrow(a), " schools lack VA.")) +
    theme_minimal(base_size = 11, base_family = "sans") +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E9EDF1", linewidth = .3),
      strip.text = element_text(face = "bold", size = 10.5, color = "#233143"),
      panel.spacing = grid::unit(1.15, "lines"),
      plot.title = element_text(face = "bold", size = 19, color = "#233143"),
      plot.subtitle = element_text(size = 10.5, color = "#526171", margin = margin(b = 10)),
      plot.caption = element_text(size = 9, color = "#526171", hjust = 0, margin = margin(t = 10)),
      legend.position = "top", legend.justification = "left", legend.title = element_text(size = 10),
      legend.margin = margin(0, 0, 5, 0), plot.margin = margin(14, 18, 12, 12))
  if (is.null(region_id)) p <- p + facet_wrap(vars(region), ncol = 4, drop = FALSE)
  if (!is.null(region_id)) p <- p + theme(plot.title = element_text(size = 17), plot.caption = element_text(size = 8))
  p
}
overview <- make_plot()
ggsave(file.path(fig, "all_regions.png"), overview, width = 15, height = 12, dpi = 220, bg = "white")
ggsave(file.path(fig, "all_regions.svg"), overview, width = 15, height = 12, device = grDevices::svg, bg = "white")
files <- character(nrow(region_map))
for (j in seq_len(nrow(region_map))) {
  code <- region_map$region_code[j]
  files[j] <- sprintf("region_%02d.png", code)
  ggsave(file.path(fig, files[j]), make_plot(code), width = 8.8, height = 5.9, dpi = 180, bg = "white")
}
stopifnot(length(files) == 16L, all(file.exists(file.path(fig, files))),
  identical(unname(tools::md5sum(inputs)), hashes))
fwrite(data.table(check = c("One plotted row per school", "All 16 regions", "VA coverage accounts for all classified schools",
  "Only groups with at least five schools receive curves", "Densities integrate to one", "16 regional PNGs exist", "Inputs unchanged"), passed = TRUE),
  file.path(out, "verification.csv"))
rel <- paste0("../../figures/lottery_variation_by_region/", measure, "/")
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  paste0('<title>', spec$title, ' VA by region and lottery variation</title>'),
  '<style>body{font:16px/1.5 system-ui;color:#233143;max-width:1600px;margin:28px auto;padding:0 24px}h1{font-size:28px}.grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:22px}img{width:100%;height:auto}figure{margin:0}a{color:#0072b2}.note{color:#526171;font-size:14px}@media(max-width:1000px){.grid{grid-template-columns:1fr}}</style></head><body>',
  paste0('<h1>', spec$title, ' VA by region and lottery variation</h1><p>', nrow(a), ' of ', nrow(d), ' classified schools have an estimate. Blue: always; orange: sometimes; red: never.</p>'),
  '<p class="note">School weights are equal. The VA measure is the paper\'s EB-adjusted 2017-2020 All-sample estimate, with its original student-centered normalization. VA is not re-estimated, re-centered within region, or conditioned on high math scores. Categories and school regions are unchanged from the preceding distribution.</p>',
  '<p class="note">Each density integrates to one within its region and lottery group. Colors therefore compare distribution shapes, not group shares. Every school appears as a tick; groups with fewer than five schools have ticks only. Absent groups are labeled zero. Common bandwidth and axis scales apply to every plot.</p>',
  '<p class="note">Always/sometimes/never is defined over observed grade-9-offering years in SAE 2018-2020. Metropolitana enters in 2019; the preceding report documents year coverage and the 37 schools observed only once. These are adjusted observational VA estimates, not separately estimated lottery effects.</p>',
  paste0('<p><a href="', rel, 'all_regions.png">High-resolution overview</a> | <a href="', rel, 'all_regions.svg">Vector overview (SVG)</a> | <a href="../../../data/clean/lottery_variation_by_region/va_', measure, '/coverage_by_region_group.csv">Coverage and counts</a></p>'),
  paste0('<img src="', rel, 'all_regions.png" alt="Sixteen regional VA distributions colored by lottery variation">'),
  '<h2>Individual regional plots</h2><div class="grid">',
  vapply(seq_len(nrow(region_map)), function(j) paste0('<figure><a href="', rel, files[j], '"><img loading="lazy" src="', rel, files[j], '" alt="', region_map$region[j], ' VA distribution"></a></figure>'), character(1)),
  '</div></body></html>')
writeLines(html, file.path(report, paste0("va_", measure, ".html")), useBytes = TRUE)
print(counts[, .(classified = sum(classified), plotted = sum(plotted), missing_va = sum(missing_va)), by = classification])
message("Saved 16 regional plots, overview PNG/SVG, and HTML gallery. Bandwidth = ", signif(bandwidth, 5))
