# Explore high-VA thresholds without assigning binary indicators.
# Run from the repository root with Rscript.
suppressPackageStartupMessages({library(data.table); library(ggplot2)})
input <- 'output/tables/empirical_bayes_school_va'
figdir <- 'output/figures/high_va_cutoffs'
datadir <- 'data/clean/high_va_cutoffs'
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
dir.create(datadir, recursive = TRUE, showWarnings = FALSE)
keys <- c('math', 'language', 'highinst', 'highpay', 'program_income_full')
labels <- c('Math (test SD)', 'Verbal (test SD)', 'High-premium institution (probability)',
            'High-premium field (probability)', 'Projected income (log points)')
d <- rbindlist(lapply(seq_along(keys), function(i) {
  x <- fread(file.path(input, paste0('stata_eb_school_values_', keys[i], '.csv')))
  x <- x[analysis_sample == 'All']
  stopifnot(nrow(x) > 0, !anyDuplicated(x$school_rbd),
            all(is.finite(x$va_eb_centered)), all(x$n_students > 0))
  x[, .(school_rbd, measure = labels[i], key = keys[i],
        va = va_eb_centered, raw_va = va_centered, n_students)]
}))
d[, measure := factor(measure, levels = labels)]
both <- rbind(copy(d)[, `:=`(weight = 1, weighting = 'Equal school weights')],
              copy(d)[, `:=`(weight = n_students, weighting = 'Student weights')])
both[, weighting := factor(weighting, levels = c('Equal school weights', 'Student weights'))]
wq <- function(x, w, p) {o <- order(x); x[o][which(cumsum(w[o])/sum(w) >= p)[1]]}
stats <- both[, .(schools = .N, students = sum(n_students),
                  minimum = min(va), p25 = wq(va, weight, .25),
                  median = wq(va, weight, .5), p75 = wq(va, weight, .75),
                  maximum = max(va), share_above_zero = sum(weight[va > 0])/sum(weight)),
              by = .(key, measure, weighting)]
fwrite(stats, file.path(datadir, 'distribution_summary.csv'))
refs <- melt(stats, id.vars = c('measure', 'weighting'),
             measure.vars = c('median', 'p75'), variable.name = 'reference', value.name = 'value')
refs <- rbind(refs, unique(both[, .(measure, weighting)])[, `:=`(reference = 'zero', value = 0)])
# Explicit weighted bin masses; identical breaks across weighting schemes.
histograms <- both[, {
  breaks <- seq(min(va), max(va), length.out = 46)
  bin <- cut(va, breaks, include.lowest = TRUE, labels = FALSE)
  mass <- as.numeric(tapply(weight, factor(bin, levels = 1:45), sum))
  mass[is.na(mass)] <- 0
  .(x = head(breaks, -1), xend = tail(breaks, -1), share = mass/sum(weight))
}, by = .(measure, weighting)]
stopifnot(all(abs(histograms[, sum(share), by = .(measure, weighting)]$V1 - 1) < 1e-10))
colors <- c(zero = '#333333', median = '#0072B2', p75 = '#D55E00')
base <- theme_minimal(base_size = 12) + theme(legend.position = 'bottom',
  panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold'),
  plot.caption = element_text(hjust = 0, size = 9))
p <- ggplot(histograms) + geom_rect(aes(xmin = x, xmax = xend, ymin = 0, ymax = share),
  fill = '#80AFC1', color = 'white', linewidth = .15) +
  geom_vline(data = refs, aes(xintercept = value, color = reference), linetype = 'dashed', linewidth = .55) +
  facet_wrap(vars(measure, weighting), ncol = 2, scales = 'free') +
  scale_color_manual(values = colors, breaks = c('zero', 'median', 'p75'),
    labels = c('Zero', 'Median', '75th percentile')) +
  scale_y_continuous(labels = function(x) paste0(round(100*x), '%')) +
  labs(title = 'Where does high value added begin?', subtitle = 'Five primary EB-shrunken measures | All-sample school estimates | 45 equal-width bins',
       x = 'School value added', y = 'Share within bin', color = NULL,
       caption = 'Each row uses the same schools and bin boundaries. Student weights use outcome-specific regression counts.\nReference lines are candidate conventions, not estimated natural cutoffs. Full observed ranges are shown.') + base
ggsave(file.path(figdir, 'primary_five_distributions.png'), p, width = 13, height = 15, dpi = 160)
ggsave(file.path(figdir, 'primary_five_distributions.pdf'), p, width = 13, height = 15)
# Ordered school values expose gaps without depending on histogram bins.
ranked <- copy(d)[order(measure, va)]
ranked[, percentile := 100 * (seq_len(.N) - .5)/.N, by = measure]
q <- ggplot(ranked, aes(percentile, va)) + geom_line(color = '#0072B2') +
  geom_hline(yintercept = 0, color = 'grey50', linetype = 'dashed') +
  facet_wrap(vars(measure), ncol = 2, scales = 'free_y') +
  labs(title = 'Ordered school VA: look for jumps or separate groups',
       subtitle = 'Equal school weights | EB-shrunken estimates | Full observed ranges',
       x = 'School percentile', y = 'School value added') + base
ggsave(file.path(figdir, 'ordered_school_values.png'), q, width = 12, height = 10, dpi = 160)
print(stats)
