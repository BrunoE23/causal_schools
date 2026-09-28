###############################################################################
# RD analysis: HE enrollment at the PSU/PAES scholarship score cutoff
#
# Mirrors script 16 (any-scholarship take-up) but with HE enrollment
# as the outcome. Produces:
#   (1) Main RD jump via rdrobust
#   (2) Continuous VA interaction table (feols, 5 specs)
#   (3) Binary gamma3 at p50 and p75
#
# Outputs
# -------
#   output/tables/becas_creditos/he_enrollment_rd_va_interaction_{pts,sd}.{tex,csv}
#   output/tables/becas_creditos/he_enrollment_rd_gamma3_binary.csv
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(haven); library(fixest); library(rdrobust)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
clean_dir     <- file.path(data_wd, "data", "clean")
eb_dir        <- file.path(clean_dir, "empirical_bayes_school_va", "cohorts_2017_2020")
table_dir     <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

RD_CONTROLS <- c(
  "factor(cohort_gr8)", "factor(GEN_ALU)", "factor(EDAD_ALU)",
  "z_sim_mat_4to", "z_sim_leng_4to", "factor(income_decile)"
)
poly_terms <- function(rv) c(rv, paste0("I(above_cutoff * ", rv, ")"))

va_specs <- data.table(
  spec       = c("math",            "verbal",          "highinst",     "highfield",            "progincome"),
  eb_outcome = c("z_year_math_max", "z_year_leng_max", "high_inst_m1", "high_paying_field_m1", "log_program_income_clp_m1"),
  label      = c("Math VA",         "Verbal VA",       "Hi-inst. VA",  "Hi-field VA",          "Proj. income VA")
)

# ── Universe ──────────────────────────────────────────────────────────────────
message("Reading universe.")
universe <- fread(file.path(clean_dir, "univ_gr8_df.csv"),
  select = c("MRUN", "math_max", "leng_max", "psu_year", "most_time_RBD",
             "cohort_gr8", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to",
             "income_decile", "COD_SIES_m1"),
  na.strings = c("", "NA"))
universe <- universe[math_max > 0 & leng_max > 0]
universe[, `:=`(
  mrun_key    = key_from_numeric(MRUN),
  rbd         = as.numeric(most_time_RBD),
  psu_average = (math_max + leng_max) / 2,
  cutoff_yr   = fifelse(psu_year <= 2022L, 500L, 510L)
)]
universe[, `:=`(
  psu_centered_pts = psu_average - cutoff_yr,
  above_cutoff     = as.integer(psu_average >= cutoff_yr),
  enrolled_he      = as.integer(!is.na(COD_SIES_m1))
)]
universe[!is.na(psu_year), psu_sd_yr := sd(psu_average), by = psu_year]
universe[, psu_centered_sd := (psu_average - cutoff_yr) / psu_sd_yr]

# ── EB school VA ──────────────────────────────────────────────────────────────
message("Reading EB school values.")
eb_raw <- fread(file.path(eb_dir, "eb_school_rbd_observational_values.csv"),
                na.strings = c("", "NA"))
eb_raw <- eb_raw[analysis_sample == "All" & outcome %chin% va_specs$eb_outcome,
  .(rbd = as.numeric(school_rbd), outcome,
    va  = controlled_value_added_eb_centered_student)]
eb_wide <- dcast(eb_raw, rbd ~ outcome, value.var = "va")
setnames(eb_wide, va_specs$eb_outcome, paste0("school_va_", va_specs$spec))
universe <- merge(universe, eb_wide, by = "rbd", all.x = TRUE)

va_cols <- paste0("school_va_", va_specs$spec)
for (col in va_cols) {
  s <- sd(universe[[col]], na.rm = TRUE)
  if (!is.na(s) && s > 0) set(universe, j = col, value = universe[[col]] / s)
}

# ── 1. Main RD jump (rdrobust) ────────────────────────────────────────────────
message("\n=== Main RD: HE enrollment at the eligibility cutoff ===")
for (rv in c("psu_centered_pts", "psu_centered_sd")) {
  d <- universe[!is.na(enrolled_he) & !is.na(get(rv))]
  r <- rdrobust(y = d$enrolled_he, x = d[[rv]], c = 0,
                kernel = "triangular", vce = "hc1")
  cat(sprintf("\n[%s]  h = +/-%.1f\n  Jump = %.4f  (robust SE = %.4f, p = %.4f)\n  95%% CI: [%.4f, %.4f]\n",
    rv, mean(r$bws["h",]),
    r$coef["Conventional",], r$se["Robust",],
    r$pv["Robust",], r$ci["Robust",1], r$ci["Robust",2]))
}

# ── 2. Bandwidth (MSE-optimal on enrolled_he) ─────────────────────────────────
get_h <- function(rv) {
  d <- universe[!is.na(enrolled_he) & !is.na(get(rv))]
  r <- rdrobust(y = d$enrolled_he, x = d[[rv]], c = 0,
                kernel = "triangular", vce = "hc1")
  h <- mean(r$bws["h",])
  message(sprintf("  h [%s] = +/-%.3f", rv, h))
  h
}
message("\nBandwidth selection.")
h_pts <- get_h("psu_centered_pts")
h_sd  <- get_h("psu_centered_sd")

# ── 3. Continuous VA interaction (feols) ──────────────────────────────────────
run_rd_spec <- function(dt, spec_row, bw, rv) {
  va_col <- paste0("school_va_", spec_row$spec)
  pt     <- poly_terms(rv)
  needed <- c("enrolled_he", va_col, rv, "above_cutoff",
              "cohort_gr8", "GEN_ALU", "EDAD_ALU",
              "z_sim_mat_4to", "z_sim_leng_4to", "income_decile")
  reg_dt <- dt[abs(dt[[rv]]) <= bw & complete.cases(dt[, ..needed])]
  if (nrow(reg_dt) < 100L) return(NULL)

  inter_term <- paste0("above_cutoff:", va_col)
  rhs <- paste(c("above_cutoff", va_col, inter_term, pt, RD_CONTROLS), collapse = " + ")
  m   <- feols(as.formula(paste0("enrolled_he ~ ", rhs)),
               data = reg_dt, vcov = "hetero", notes = FALSE)

  coefs <- coef(m); ses <- se(m)
  pull  <- function(nm) {
    hit <- names(coefs)[grepl(nm, names(coefs), fixed = TRUE)]
    if (!length(hit)) return(list(b = NA_real_, se = NA_real_))
    list(b = coefs[[hit[1]]], se = ses[[hit[1]]])
  }
  above <- pull("above_cutoff"); inter <- pull(inter_term)
  list(spec = spec_row$spec, label = spec_row$label, bw_used = bw,
       n_obs = nrow(reg_dt), outcome_mean = mean(reg_dt$enrolled_he, na.rm = TRUE),
       above_b = above$b, above_se = above$se,
       inter_b = inter$b,  inter_se = inter$se)
}

run_all <- function(h, rv, tag) {
  message(sprintf("Continuous VA interaction [%s, bw=+/-%.1f].", tag, h))
  rbindlist(lapply(seq_len(nrow(va_specs)), function(i) {
    r <- run_rd_spec(universe, va_specs[i], bw = h, rv = rv)
    if (!is.null(r)) as.data.table(r)[, version := tag]
  }), use.names = TRUE, fill = TRUE)
}

res_pts <- run_all(h_pts, "psu_centered_pts", "pts")
res_sd  <- run_all(h_sd,  "psu_centered_sd",  "sd")

pval <- function(b, se) 2 * pnorm(-abs(b / se))
stars <- function(p) fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.10, "*", "")))

cat("\n=== Continuous VA interaction (pts) ===\n")
print(res_pts[, .(spec, n_obs,
  gamma1  = round(above_b, 4), se1 = round(above_se, 4),
  gamma3  = round(inter_b, 4), se3 = round(inter_se, 4),
  t3      = round(inter_b / inter_se, 2),
  p3      = round(pval(inter_b, inter_se), 4),
  sig     = stars(pval(inter_b, inter_se))
)])

# ── 4. Binary gamma3 at p50 and p75 ──────────────────────────────────────────
run_binary <- function(percentile, rv = "psu_centered_pts", h = h_pts) {
  cutoff_q <- percentile / 100
  rbindlist(lapply(seq_len(nrow(va_specs)), function(i) {
    va_col <- paste0("school_va_", va_specs$spec[i])
    dt <- universe[abs(get(rv)) <= h & !is.na(enrolled_he) &
                   !is.na(get(rv)) & !is.na(get(va_col))]
    school_vals <- unique(dt[, .(rbd, va = get(va_col))])
    q_val <- quantile(school_vals$va, cutoff_q)
    dt[, hi_va := as.integer(get(va_col) >= q_val)]

    needed <- c("enrolled_he", "above_cutoff", "hi_va", rv,
                "cohort_gr8", "GEN_ALU", "EDAD_ALU",
                "z_sim_mat_4to", "z_sim_leng_4to", "income_decile")
    dt <- dt[complete.cases(dt[, ..needed])]

    rhs <- paste(c("above_cutoff", "hi_va", "above_cutoff:hi_va",
                   rv, paste0("I(above_cutoff * ", rv, ")"),
                   RD_CONTROLS), collapse = " + ")
    m <- feols(as.formula(paste0("enrolled_he ~ ", rhs)),
               data = dt, vcov = "hetero", notes = FALSE)

    coefs <- coef(m); ses <- se(m)
    pull <- function(nm) {
      hit <- names(coefs)[grepl(nm, names(coefs), fixed = TRUE)]
      if (!length(hit)) return(c(NA_real_, NA_real_))
      c(coefs[[hit[1]]], ses[[hit[1]]])
    }
    g1 <- pull("above_cutoff"); g3 <- pull("above_cutoff:hi_va")
    data.table(spec = va_specs$spec[i], label = va_specs$label[i],
               split = percentile, n_obs = nrow(dt),
               g1_b = g1[1], g1_se = g1[2],
               g3_b = g3[1], g3_se = g3[2],
               g3_t = g3[1] / g3[2],
               g3_p = 2 * pnorm(-abs(g3[1] / g3[2])))
  }), use.names = TRUE)
}

res_p50 <- run_binary(50)
res_p75 <- run_binary(75)

fmt <- function(res, split_label) {
  cat(sprintf("\n=== Binary gamma3 (%s split, HE enrollment) ===\n", split_label))
  cat(sprintf("  lo group = %s  |  hi group = %s\n\n",
    if (split_label == "p50") "below median" else "bottom 75%",
    if (split_label == "p50") "above median" else "top 25%"))
  cat(sprintf("%-20s  %6s  %7s  %9s %6s %6s %5s\n",
    "VA measure", "N", "gamma1", "gamma3", "SE", "t", "p"))
  cat(strrep("-", 72), "\n")
  for (i in seq_len(nrow(res))) {
    r <- res[i]
    cat(sprintf("%-20s  %6s  %7.4f  %9.4f %6.4f %6.2f %5.3f %s\n",
      r$label, format(r$n_obs, big.mark = ","),
      r$g1_b, r$g3_b, r$g3_se, r$g3_t, r$g3_p, stars(r$g3_p)))
  }
}

fmt(res_p50, "p50")
fmt(res_p75, "p75")

# ── 5. Save ───────────────────────────────────────────────────────────────────
fwrite(res_pts, file.path(table_dir, "he_enrollment_rd_va_interaction_pts.csv"))
fwrite(res_sd,  file.path(table_dir, "he_enrollment_rd_va_interaction_sd.csv"))
fwrite(rbindlist(list(res_p50, res_p75)),
       file.path(table_dir, "he_enrollment_rd_gamma3_binary.csv"))
message("\nDone.")
