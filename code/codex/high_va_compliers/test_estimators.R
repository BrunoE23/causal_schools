suppressPackageStartupMessages({library(data.table); library(fixest)})
source('code/codex/high_va_compliers/helpers.R')
# An exact population with unequal offer probabilities and known compliance
# types. Integer frequency expansion produces exact randomization within type/q.
cells <- CJ(q = c(.1, .2, .4, .6, .8, .9), type = c('always', 'never', 'complier'), Z = 0:1)
cells[, X := fifelse(type == 'complier', 2 + 3*q, fifelse(type == 'always', 9, -2))]
cells[, D := as.integer(type == 'always' | (type == 'complier' & Z == 1))]
cells[, n := as.integer(round(100 * fifelse(Z == 1, q, 1 - q)))]
s <- cells[rep(seq_len(nrow(cells)), cells$n)]
truth <- cells[type == 'complier' & Z == 1, weighted.mean(X, q*(1-q))]
for (state in 0:1) {
  a <- ratio_mean(s$X, s$D, s$Z, s$q, state)
  b <- fit_spline_mean(s$X, s$D, s$Z, s$q, state)
  stopifnot(abs(a$estimate - truth) < 1e-10, abs(b$estimate - truth) < 1e-9)
}
# Verify the sandwich against the corresponding no-intercept IV regression.
s[, `:=`(w = Z-q, product = X*D)]
m <- feols(product ~ 0 | D ~ w, data = s, vcov = 'hetero')
a <- ratio_mean(s$X, s$D, s$Z, s$q)
stopifnot(abs(coef(m)[1] - a$estimate) < 1e-10,
          abs(se(m)[1] - a$se) < 1e-10)
# Unknown schools must retain unknown mass; unmatched is explicitly low offer.
p <- data.table(student_id = c(1,1,1,2,2),
  school_id = c('10_a', '20_b', 'unmatched', '10_a', '99_c'), prob = c(.3,.2,.5,.4,.6))
h <- data.table(school_rbd = c(10,20), high_va_math = c(1L,0L))
r <- aggregate_high_probability(p, h, 'math')
stopifnot(all(r$total_mass == 1), r[student_id == 1, q_math] == .3,
          r[student_id == 1, unknown_math] == 0, r[student_id == 2, unknown_math] == .6)
cat('PASS: known complier means, spline consistency, robust SE, and probability coverage.\n')
