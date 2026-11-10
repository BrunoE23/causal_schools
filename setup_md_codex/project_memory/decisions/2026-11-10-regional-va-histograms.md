# Regional VA histograms by lottery persistence

The user requested filled histogram bars in place of the preceding density
plots for all three VA measures and all 16 school regions. This changes the
display only; reuse the cached school plotting data and the same 2,504 schools
(2,194 always, 296 sometimes, 14 never) with 25 missing VA estimates. Retain
original EB-adjusted observational VA values, units, school regions, and the
SAE 2018-2020 classification over observed offering years.

Use filled stacked bars, with blue always, orange sometimes, and red never.
Normalize each bin segment by all plotted schools in its region. The complete
histogram sums to one within a region, and each color's total mass equals the
group's regional share. This makes both VA distributions and lottery-category
shares visible. It differs from the earlier density normalization within each
region-group, so document that denominator change in every plot and the gallery.

Use 30 equal-width bins over the full pooled observed range separately for
each VA measure. Hold bin boundaries and both axes fixed across regions within
each measure. Include both extreme observed values, with a closed final upper
endpoint; do not trim tails or recenter VA. All school weights are equal and
all group sizes are represented, including single observations. Math remains
in test-score SD; premium-field and premium-institution VA are in percentage
points. No high-math restriction or new estimation is imposed.

Save separate histogram outputs to preserve the earlier density figures.
Deliver three overviews, 48 individual regional PNGs, and a combined gallery.
Verify school support and unit conversions across measures, one bin per school,
group and regional count identities, regional shares summing to one, and
unchanged source hashes. Preserve bin assignments and bin counts in clean data.

Completed checks pass for all 48 region-measure distributions. The gallery has
51 images and 106 valid local file references; all three SVG overviews parse.
All three overviews and individual plots for Arica y Parinacota (institution)
and La Araucania (math) were visually inspected. Titles, counts, legends, and
captions fit, and sparse categories remain represented as colored bar segments.
