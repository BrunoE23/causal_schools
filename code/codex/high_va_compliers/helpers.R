# All uncertainty estimates condition on the saved school classifications and q.
ratio_mean <- function(x, d, z, q, state = 1L) {
  w <- z - q
  treatment <- if (state == 1L) d else 1 - d
  a <- w * treatment * x
  b <- w * treatment
  n <- length(x)
  if (n < 3L || abs(sum(b)) < 1e-10) stop('Insufficient first-stage denominator.')
  estimate <- sum(a) / sum(b)
  # Heteroskedastic moment sandwich with n/(n-1) correction.
  # The signed denominator handles the untreated-state first stage.
  se <- sqrt(n / (n - 1) * sum((a - estimate * b)^2)) / abs(sum(b))
  data.table::data.table(estimate = estimate, se = se,
    ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se,
    denominator = mean(b), status = 'ok')
}

fit_spline_mean <- function(x, d, z, q, state = 1L) {
  t <- if (state == 1L) d else 1 - d
  dat <- data.frame(product = x * t, treatment = t, z = z)
  if (length(unique(q)) < 5L) stop('Fewer than five distinct q values for spline.')
  basis <- splines::ns(q, df = 4, intercept = FALSE)
  colnames(basis) <- paste0('s', seq_len(ncol(basis)))
  dat <- cbind(dat, basis)
  rhs <- paste(colnames(basis), collapse = ' + ')
  model <- fixest::feols(as.formula(paste('product ~', rhs, '| treatment ~ z')),
    data = dat, vcov = 'hetero', notes = FALSE)
  tab <- fixest::coeftable(model)
  if (!'fit_treatment' %in% rownames(tab)) stop('Spline IV treatment coefficient unidentified.')
  estimate <- tab['fit_treatment', 1]
  se <- tab['fit_treatment', 2]
  data.table::data.table(estimate = estimate, se = se,
    ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se,
    denominator = NA_real_, status = 'ok')
}

safe_fit <- function(fun, ...) {
  tryCatch(fun(...), error = function(e) data.table::data.table(
    estimate = NA_real_, se = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    denominator = NA_real_, status = conditionMessage(e)))
}

# Full support is necessary: never renormalize probabilities over matched schools.
aggregate_high_probability <- function(prob, schools, keys) {
  stopifnot(all(is.finite(prob$prob)), all(prob$prob >= 0 & prob$prob <= 1))
  prob <- data.table::copy(prob)
  prob[, school_rbd := suppressWarnings(as.numeric(sub('_.*$', '', school_id)))]
  unmatched <- tolower(trimws(prob$school_id)) == 'unmatched'
  prob[, outside := unmatched]
  for (k in keys) {
    col <- paste0('high_va_', k)
    h <- schools[[col]][match(prob$school_rbd, schools$school_rbd)]
    h[unmatched] <- 0L
    prob[, (col) := h]
  }
  # Explicit named columns keep one record per applicant.
  ans <- prob[, .(total_mass = sum(prob)), by = student_id]
  for (k in keys) {
    col <- paste0('high_va_', k)
    s <- prob[, .(q = sum(prob * data.table::fifelse(is.na(get(col)), 0, get(col))),
                  unknown = sum(prob[is.na(get(col))])), by = student_id]
    data.table::setnames(s, c('q', 'unknown'), c(paste0('q_', k), paste0('unknown_', k)))
    ans <- merge(ans, s, by = 'student_id', sort = FALSE)
  }
  ans
}
