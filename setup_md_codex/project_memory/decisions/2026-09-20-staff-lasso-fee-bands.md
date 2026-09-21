# Add school fee-band indicators to joint staff Lasso

Bruno asked to include annual cost as matricula + 9 * mensualidad. Inspection of
the official 2024 school-directory codebook (page 3) established that both
PAGO_MATRICULA and PAGO_MENSUAL are self-reported ranges, not exact peso amounts.
After the distinction was explained, Bruno explicitly chose indicators.

## Approved extension

Use the same 2024 directory as the existing school context, joined by RBD:
C:/Users/brunem/Box/causal_schools/data/raw/school_directory/2024/20240912_Directorio_Oficial_EE_2024_20240430_WEB.csv.
The local reference is ER_Directorio_Oficial_EE_WEB.pdf in that folder.

For EACH fee, use free as the omitted reference and add five paid-band indicators:
CLP 1,000-10,000; 10,001-25,000; 25,001-50,000; 50,001-100,000; above 100,000.
Add a separate no-information indicator. Do not treat no information as free,
do not use the category codes as continuous prices, do not choose arbitrary
midpoints/top-code amounts, and do not construct an annual-cost scalar. The
monthly multiplier nine does not change membership in these source fee bands.

Both school-only and joint models receive the same 12 indicators, bringing raw
candidates to 175 (39 school, 43 teacher, 46 orientador, 47 leadership).
All previous predictors, outcomes, samples, penalty grids, random seeds and
folds are retained. Rerun inner/outer tuning and full refits for all 12 outcomes
and both penalty rules. Keep filenames stable and update the same report.
Preserve the immediately preceding aggregate performance table separately as
performance_before_fee_bands.csv for a reproducible before/after comparison.

The raw directory contains all 3,682 analysis schools with unique RBDs.
Enrollment-fee counts, ordered free then increasing paid bands then unknown:
2,749 / 324 / 7 / 10 / 36 / 420 / 136.
Monthly-fee counts in the same order:
2,493 / 3 / 27 / 145 / 311 / 566 / 137.
Sparse bands remain explicit; Lasso selection may be unstable for rare groups.
Bands are a 2024 snapshot, not retrospective annual prices or total school cost.

Source hashes, exact category mappings, mutually exclusive non-reference
indicators, unchanged old predictors/folds and all previous empirical checks
are verified. This extends (and updates counts/results from)
2026-09-20-staff-lasso-va.md; the interpretation remains predictive, not causal.

## Rerun findings

Full joint designs have 306 nonconstant columns; school-only designs have 37.
Main one-SE out-of-fold joint/school-only R2 after adding fee bands:
math 0.465/0.401, language 0.477/0.442, exam taking 0.609/0.462, HE enrollment
0.334/0.277, HP institution 0.598/0.510, full projected income 0.458/0.424,
HP field 0.100/0.106. Fee bands improve prediction for most outcomes, but
the staff block's incremental gain shrinks for several outcomes because the
baseline also improves. For math the incremental gain changes from 0.081 to
0.065; for full projected income from 0.043 to 0.034. These are predictive
comparisons, not causal cost effects or a decomposition of staff quality.

The main math joint fit selects above-CLP-100,000 enrollment fees (+0.215) and
monthly fees (+0.142). Full projected income selects the top monthly band
(+0.170) and top enrollment band (+0.052). All coefficients are outcome SD per
predictor SD, including indicators; they are not raw free-versus-paid gaps.

Verification passed for all 169,348 held-out predictions, 288 fitted-model
optimality checks and 48 performance rows, with unchanged source hashes.
Independent prediction reconstruction differs by at most 1.20e-14; maximum
KKT violation is 1.52e-5 (tolerance 2e-4). Synthetic preprocessing tests and
the LaTeX table compile check passed. Rebuilding with the raw directory's
UTF-8 BOM encoding leaves every predictor and fold unchanged.
