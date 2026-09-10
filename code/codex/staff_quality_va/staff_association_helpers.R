q_z <- function(x) {
  scale <- sd(x)
  if (!is.finite(scale) || scale <= 0) stop("Cannot standardize a constant/missing component.")
  (x - mean(x)) / scale
}

q_fit_indices <- function(wide, role) {
  components <- c("prior_role_years", "school_role_spell_years", "university_share",
                   if (role == "counselor") "teaching_title_share" else "hs_teaching_title_share")
  eligible <- wide$N_ACTIVE_YEARS >= 3L & complete.cases(wide[, ..components])
  eligible[is.na(eligible)] <- FALSE
  x <- as.matrix(wide[eligible, ..components])
  if (nrow(x) < 100L) stop("Too few complete schools for the prespecified ", role, " index; review measurement coverage.")
  means <- colMeans(x); sds <- apply(x, 2, sd)
  if (any(!is.finite(sds) | sds == 0)) stop("Uninformative core component; review before using VA.")
  z <- sweep(sweep(x, 2, means, "-"), 2, sds, "/")
  career_raw <- rowMeans(z[, 1:2, drop = FALSE])
  credentials_raw <- rowMeans(z[, 3:4, drop = FALSE])
  career <- q_z(career_raw); credentials <- q_z(credentials_raw)
  balanced_raw <- (career + credentials) / 2
  balanced <- q_z(balanced_raw)
  pca <- prcomp(z, center = FALSE, scale. = FALSE)
  direction <- if (cor(pca$x[, 1], balanced) < 0) -1 else 1
  pc1 <- q_z(direction * pca$x[, 1])
  for (field in c("career_index", "credentials_index", "balanced_index", "pca_index")) {
    set(wide, j = field, value = NA_real_)
  }
  wide[which(eligible), c("career_index", "credentials_index", "balanced_index", "pca_index") :=
         .(career, credentials, balanced, pc1)]
  params <- data.table(ROLE = role, COMPONENT = components, N_FIT = nrow(x), CENTER = means, SCALE = sds,
    BLOCK = rep(c("career", "credentials"), each = 2), WITHIN_BLOCK_WEIGHT = .5, BLOCK_WEIGHT = .5,
    BLOCK_SCALE = rep(c(sd(career_raw), sd(credentials_raw)), each = 2), BALANCED_SCALE = sd(balanced_raw))
  loadings <- data.table(ROLE = role, COMPONENT = components, PC1_LOADING = direction * pca$rotation[, 1],
    PC2_LOADING = pca$rotation[, 2], PC3_LOADING = pca$rotation[, 3], PC4_LOADING = pca$rotation[, 4])
  diagnostics <- data.table(ROLE = role, N_SCHOOLS = nrow(wide), N_FIT = nrow(x),
    PC1_VARIANCE_SHARE = pca$sdev[1]^2/sum(pca$sdev^2), PC2_VARIANCE_SHARE = pca$sdev[2]^2/sum(pca$sdev^2),
    COR_BALANCED_PC1 = cor(balanced, pc1), COR_CAREER_CREDENTIALS = cor(career, credentials),
    PC1_HAS_MIXED_LOADING_SIGNS = any(loadings$PC1_LOADING < 0) && any(loadings$PC1_LOADING > 0))
  component_cor <- as.data.table(as.table(cor(z)))
  setnames(component_cor, c("COMPONENT_1", "COMPONENT_2", "CORRELATION"))
  component_cor[, ROLE := role]
  list(wide = wide, parameters = params, loadings = loadings, diagnostics = diagnostics, component_correlations = component_cor)
}

q_weighted_cor <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]/sum(w[ok])
  if (length(x) < 3) return(NA_real_)
  dx <- x - sum(w*x); dy <- y - sum(w*y)
  den <- sqrt(sum(w*dx^2)*sum(w*dy^2))
  if (!is.finite(den) || den == 0) return(NA_real_)
  sum(w*dx*dy)/den
}

q_adjusted_association <- function(dt) {
  required <- c("X", "Y", "N_VA_STUDENTS", "COD_DEPE", "COD_REG_RBD", "RURAL_RBD", "HAS_TP_OR_ARTISTIC", "HAS_BASIC")
  dt <- copy(dt[complete.cases(dt[, ..required]) & is.finite(X) & is.finite(Y) & N_VA_STUDENTS > 0])
  empty <- list(N_ADJUSTED = nrow(dt), ADJUSTED_BETA_SD = NA_real_, ADJUSTED_SE_HC1 = NA_real_,
                ADJUSTED_P = NA_real_, ADJUSTED_CI_LOW = NA_real_, ADJUSTED_CI_HIGH = NA_real_,
                ADJUSTED_PARTIAL_R = NA_real_, ADJUSTED_STATUS = "insufficient_variation")
  if (nrow(dt) < 30L || sd(dt$X) == 0 || sd(dt$Y) == 0) return(empty)
  dt[, `:=`(X_Z = q_z(X), Y_Z = q_z(Y), LOG_N = log(N_VA_STUDENTS))]
  factors <- setdiff(required, c("X", "Y", "N_VA_STUDENTS"))
  factors <- factors[vapply(factors, function(f) uniqueN(dt[[f]]) > 1L, logical(1L))]
  controls <- c(if (sd(dt$LOG_N) > 0) c("LOG_N", "I(LOG_N^2)"),
                if (length(factors)) paste0("factor(", factors, ")"))
  form <- as.formula(paste("Y_Z ~ X_Z", if (length(controls)) paste("+", paste(controls, collapse = " + ")) else ""))
  fit <- lm(form, data = dt)
  covariance <- sandwich::vcovHC(fit, type = "HC1")
  beta <- unname(coef(fit)["X_Z"]); se <- sqrt(covariance["X_Z", "X_Z"])
  df <- df.residual(fit)
  p <- if (is.finite(se) && se > 0) 2*pt(-abs(beta/se), df = df) else NA_real_
  crit <- qt(.975, df)
  control_formula <- if (length(controls)) paste(controls, collapse = " + ") else "1"
  rx <- residuals(lm(as.formula(paste("X_Z ~", control_formula)), data = dt))
  ry <- residuals(lm(as.formula(paste("Y_Z ~", control_formula)), data = dt))
  list(N_ADJUSTED = nrow(dt), ADJUSTED_BETA_SD = beta, ADJUSTED_SE_HC1 = se, ADJUSTED_P = p,
       ADJUSTED_CI_LOW = beta - crit*se, ADJUSTED_CI_HIGH = beta + crit*se,
       ADJUSTED_PARTIAL_R = cor(rx, ry), ADJUSTED_STATUS = if (is.finite(se)) "estimated" else "se_unavailable")
}

q_correlations <- function(dt, adjusted = TRUE) {
  ok <- is.finite(dt$X) & is.finite(dt$Y)
  x <- dt$X[ok]; y <- dt$Y[ok]
  row <- list(N = length(x), PEARSON_R = NA_real_, PEARSON_P = NA_real_, PEARSON_CI_LOW = NA_real_,
               PEARSON_CI_HIGH = NA_real_, SPEARMAN_R = NA_real_, STUDENT_WEIGHTED_R = NA_real_,
               UNSHRUNK_VA_R = NA_real_, N_UNSHRUNK = 0L, STATUS = "insufficient_variation")
  if (length(x) >= 30L && sd(x) > 0 && sd(y) > 0) {
    test <- cor.test(x, y, method = "pearson")
    raw_ok <- is.finite(dt$X) & is.finite(dt$Y_RAW)
    row <- list(N = length(x), PEARSON_R = unname(test$estimate), PEARSON_P = test$p.value,
      PEARSON_CI_LOW = test$conf.int[1], PEARSON_CI_HIGH = test$conf.int[2],
      # Canonical precision avoids artificial tie-breaking from CSV/log rounding.
      # Pearson, regressions and index construction remain unrounded.
      SPEARMAN_R = cor(as.numeric(sprintf("%.10g", x)), as.numeric(sprintf("%.10g", y)), method = "spearman"),
      STUDENT_WEIGHTED_R = q_weighted_cor(dt$X, dt$Y, dt$N_VA_STUDENTS),
      UNSHRUNK_VA_R = if (sum(raw_ok) >= 30L && sd(dt$Y_RAW[raw_ok]) > 0 && sd(dt$X[raw_ok]) > 0)
        cor(dt$X[raw_ok], dt$Y_RAW[raw_ok]) else NA_real_,
      N_UNSHRUNK = sum(raw_ok), STATUS = "estimated")
  }
  if (adjusted) row <- c(row, q_adjusted_association(dt))
  as.data.table(row)
}
