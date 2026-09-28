###############################################################################
# Beca Bicentenario RD: does school value added moderate scholarship take-up?
#
# Design
# ------
#   Running variable : psu_average = (math_max + leng_max) / 2
#   Cutoff           : 500 pts (PSU, exam year <= 2022)
#                      510 pts (PAES, exam year >= 2023)
#   Polynomial       : local linear, separate slopes each side of cutoff
#   Bandwidth        : MSE-optimal via rdrobust (triangular kernel, HC1)
#
# Specification
# -------------
#   beca_bicentenario_i =
#       gamma_0
#     + gamma_1 * above_c
#     + gamma_2 * VA_{s(i)}^{m,EB}
#     + gamma_3 * (above_c x VA_{s(i)}^{m,EB})
#     + h(S_i - c)          [local linear, kink at c]
#     + X_i'Gamma
#     + eps_i
#
#   Outcome is Beca Bicentenario take-up across ALL 5 specs.
#   Each spec differs only in which school EB VA measure is interacted.
#   gamma_1: jump in take-up at the PSU cutoff
#   gamma_3: does higher school VA amplify the take-up response?
#
# Two running-variable versions are estimated and saved:
#   pts : raw PSU/PAES points (cutoff = 0 after centering)
#   sd  : within-exam-year SD units
#
# Outputs
# -------
#   output/tables/becas_creditos/bicentenario_rd_va_interaction_pts.{tex,csv}
#   output/tables/becas_creditos/bicentenario_rd_va_interaction_sd.{tex,csv}
#   output/tables/becas_creditos/bicentenario_rd_va_sensitivity.csv
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(fixest)
  library(rdrobust)
})


# ── Paths ─────────────────────────────────────────────────────────────────────
data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
clean_dir        <- file.path(data_wd, "data", "clean")
eb_dir           <- file.path(clean_dir, "empirical_bayes_school_va", "cohorts_2017_2020")
universe_path    <- file.path(clean_dir, "univ_gr8_df.csv")
assignments_path <- file.path(clean_dir, "asignaciones.dta")
eb_values_path   <- file.path(eb_dir, "eb_school_rbd_observational_values.csv")
table_dir        <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ── Parameters ────────────────────────────────────────────────────────────────
CUTOFF_PSU  <- 500L   # PSU scale (exam year <= 2022)
CUTOFF_PAES <- 510L   # PAES scale (exam year >= 2023)
RD_CONTROLS <- c(
  "factor(cohort_gr8)", "factor(GEN_ALU)", "factor(EDAD_ALU)",
  "z_sim_mat_4to", "z_sim_leng_4to", "factor(income_decile)"
)
poly_terms <- function(rv) c(rv, paste0("I(above_cutoff * ", rv, ")"))

# ── VA specs ──────────────────────────────────────────────────────────────────
# Outcome is always beca_bicentenario take-up.
# Each spec interacts above_cutoff with a different school EB VA measure.
va_specs <- data.table(
  spec       = c("math",            "verbal",          "highinst",      "highfield",            "progincome"),
  eb_outcome = c("z_year_math_max", "z_year_leng_max", "high_inst_m1",  "high_paying_field_m1", "log_program_income_clp_m1"),
  label      = c("Math VA",         "Verbal VA",       "Hi-inst. VA",   "Hi-field VA",          "Proj. income VA")
)

# ── Helpers ───────────────────────────────────────────────────────────────────
key_from_numeric <- function(x) as.character(as.numeric(x))
fmt_est  <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
fmt_se   <- function(x) ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
stars    <- function(p) fifelse(
  is.na(p), "",
  fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.10, "*", "")))
)
fmt_star <- function(x, p) paste0(fmt_est(x), stars(p))
pval     <- function(b, se) 2 * pnorm(-abs(b / se))

# ── 1. Universe ───────────────────────────────────────────────────────────────
message("Reading universe.")
universe <- fread(
  universe_path,
  select = c(
    "MRUN", "student_id", "cohort_gr8", "most_time_RBD",
    "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to",
    "math_max", "leng_max", "psu_year", "income_decile"
  ),
  na.strings = c("", "NA")
)
universe[, `:=`(
  mrun_key      = key_from_numeric(MRUN),
  student_id    = as.numeric(student_id),
  most_time_RBD = as.numeric(most_time_RBD),
  psu_average   = (math_max + leng_max) / 2
)]
universe <- universe[math_max > 0 & leng_max > 0]

# ── 2. Beca Bicentenario receipt ──────────────────────────────────────────────
message("Reading benefit assignments.")
assignments <- as.data.table(
  read_dta(assignments_path, col_select = c("mrun", "beneficio_beca_fscu"))
)
assignments[, benefit := toupper(trimws(beneficio_beca_fscu))]
bbic_ids <- unique(key_from_numeric(assignments[benefit == "BBIC", mrun]))
bjgm_ids <- unique(key_from_numeric(assignments[benefit == "BJGM", mrun]))
universe[, beca_any := as.integer(mrun_key %chin% union(bbic_ids, bjgm_ids))]

# ── 3. EB school value added ──────────────────────────────────────────────────
message("Reading EB school values.")
eb_raw <- fread(eb_values_path, na.strings = c("", "NA"))
eb_raw <- eb_raw[
  analysis_sample == "All" &
    outcome %chin% va_specs$eb_outcome,
  .(school_rbd = as.numeric(school_rbd), outcome,
    va = controlled_value_added_eb_centered_student)
]
eb_wide <- dcast(eb_raw, school_rbd ~ outcome, value.var = "va")
setnames(eb_wide, va_specs$eb_outcome, paste0("school_va_", va_specs$spec))
universe <- merge(
  universe, eb_wide,
  by.x = "most_time_RBD", by.y = "school_rbd",
  all.x = TRUE, sort = FALSE
)

# ── 4. RD variables ───────────────────────────────────────────────────────────
universe[, cutoff_yr := fifelse(psu_year <= 2022L, CUTOFF_PSU, CUTOFF_PAES)]
universe[, above_cutoff := as.integer(psu_average >= cutoff_yr)]

# Running variable version 1: raw pts
universe[, psu_centered_pts := psu_average - cutoff_yr]

# Running variable version 2: within-exam-year SD units
universe[!is.na(psu_year), psu_sd_yr := sd(psu_average), by = psu_year]
universe[, psu_centered_sd := (psu_average - cutoff_yr) / psu_sd_yr]

# Standardise school VA columns to unit SD (coefficients = effect per 1-SD of VA)
va_cols <- paste0("school_va_", va_specs$spec)
for (col in va_cols) {
  s <- sd(universe[[col]], na.rm = TRUE)
  if (!is.na(s) && s > 0) set(universe, j = col, value = universe[[col]] / s)
}

# ── 5. Bandwidth selection via rdrobust ───────────────────────────────────────
# Outcome is the same (beca_bicentenario) for all specs — one call per rv version
setFixest_nthreads(0)

get_h <- function(rv) {
  message("rdrobust bandwidth selection [rv = ", rv, "].")
  rdr_data <- universe[!is.na(beca_any) & !is.na(universe[[rv]])]
  rdr <- rdrobust(y = rdr_data$beca_any, x = rdr_data[[rv]], c = 0,
                  kernel = "triangular", vce = "hc1")
  h <- mean(rdr$bws["h", ])
  message("  Optimal bandwidth: +/-", round(h, 3))
  h
}

h_pts <- get_h("psu_centered_pts")
h_sd  <- get_h("psu_centered_sd")

# ── 6. Estimation ─────────────────────────────────────────────────────────────
run_rd_spec <- function(dt, spec_row, bw, rv = "psu_centered_pts") {
  va_col <- paste0("school_va_", spec_row$spec)
  pt     <- poly_terms(rv)

  needed <- c("beca_any", va_col, rv, "above_cutoff",
              "cohort_gr8", "GEN_ALU", "EDAD_ALU", "z_sim_mat_4to", "z_sim_leng_4to",
              "income_decile")
  reg_dt <- dt[abs(dt[[rv]]) <= bw & complete.cases(dt[, ..needed])]

  if (nrow(reg_dt) < 100L) {
    warning("Too few observations for spec: ", spec_row$spec)
    return(NULL)
  }

  inter_term <- paste0("above_cutoff:", va_col)
  rhs <- paste(c("above_cutoff", va_col, inter_term, pt, RD_CONTROLS), collapse = " + ")
  m <- feols(as.formula(paste0("beca_any ~ ", rhs)), data = reg_dt,
             vcov = "hetero", notes = FALSE)

  coefs <- coef(m)
  ses   <- se(m)
  pull  <- function(nm) {
    hit <- names(coefs)[grepl(nm, names(coefs), fixed = TRUE)]
    if (!length(hit)) return(list(b = NA_real_, se = NA_real_))
    list(b = coefs[[hit[1]]], se = ses[[hit[1]]])
  }

  above <- pull("above_cutoff")
  inter <- pull(inter_term)

  list(
    spec         = spec_row$spec,
    label        = spec_row$label,
    bw_used      = bw,
    n_obs        = nrow(reg_dt),
    outcome_mean = mean(reg_dt$beca_any, na.rm = TRUE),
    above_b  = above$b, above_se = above$se,
    inter_b  = inter$b, inter_se = inter$se
  )
}

run_all_specs <- function(h, rv, label) {
  message("Running VA specs [", label, "] (bw = +/-", round(h, 3), ").")
  rbindlist(lapply(seq_len(nrow(va_specs)), function(i) {
    message("  VA spec: ", va_specs$spec[i])
    r <- run_rd_spec(universe, va_specs[i], bw = h, rv = rv)
    if (!is.null(r)) as.data.table(r)[, version := label]
  }), use.names = TRUE, fill = TRUE)
}

results_pts <- run_all_specs(h_pts, "psu_centered_pts", "pts")
results_sd  <- run_all_specs(h_sd,  "psu_centered_sd",  "sd")

# ── 7. LaTeX table ────────────────────────────────────────────────────────────
make_rd_table <- function(res, tex_path, csv_path, caption, label) {
  n_cols <- nrow(res)
  align  <- paste(rep("c", n_cols), collapse = "")

  row2 <- function(label_str, b_col, se_col) {
    p <- pval(res[[b_col]], res[[se_col]])
    c(
      paste0(label_str, " & ", paste(fmt_star(res[[b_col]], p), collapse = " & "), " \\\\"),
      paste0("        & ",    paste(fmt_se(res[[se_col]]),    collapse = " & "), " \\\\")
    )
  }

  latex <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{l", align, "}"),
    "\\toprule",
    paste0(" & ", paste(res$label, collapse = " & "), " \\\\"),
    "\\midrule",
    row2("Above cutoff ($\\hat{\\gamma}_1$)",              "above_b", "above_se"),
    row2("Above $\\times$ School VA ($\\hat{\\gamma}_3$)", "inter_b", "inter_se"),
    "\\midrule",
    paste0("$N$ & ", paste(format(res$n_obs, big.mark = ","), collapse = " & "), " \\\\"),
    paste0("Take-up mean & ", paste(sprintf("%.3f", res$outcome_mean), collapse = " & "), " \\\\"),
    paste0("Bandwidth & ", paste(sprintf("%.1f", res$bw_used), collapse = " & "), " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\medskip\\footnotesize",
    "\\begin{minipage}{\\textwidth}",
    paste0(
      "Notes: Outcome is take-up of any need-based scholarship (Beca Bicentenario or Beca Juan Gomez Millas). ",
      "Running variable $S_i$ is the average of math and verbal admission-test scores, ",
      "centered at the eligibility cutoff (500 pts for PSU years $\\leq$2022; 510 pts for PAES years $\\geq$2023). ",
      "Bandwidth selected by MSE-optimal rdrobust (triangular kernel). ",
      "Each column interacts above-cutoff eligibility with a different school EB value-added measure (standardized to unit SD). ",
      "Local linear polynomial with separate slopes on each side of the cutoff. ",
      "Controls: cohort, gender, age, grade-4 SIMCE math and language. ",
      "Heteroskedasticity-robust standard errors in parentheses. ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )

  writeLines(latex, tex_path)
  fwrite(res, csv_path)
  message("Wrote: ", tex_path)
  message("Wrote: ", csv_path)
}

for (ver in list(list(res = results_pts, tag = "pts", cap_note = "raw PSU/PAES points"),
                 list(res = results_sd,  tag = "sd",  cap_note = "within-year SD units"))) {
  make_rd_table(
    ver$res,
    tex_path = file.path(table_dir, paste0("any_scholarship_rd_va_interaction_", ver$tag, ".tex")),
    csv_path = file.path(table_dir, paste0("any_scholarship_rd_va_interaction_", ver$tag, ".csv")),
    caption  = paste0("School Value Added and Need-Based Scholarship Take-up at the Eligibility Cutoff",
                      " (running variable: ", ver$cap_note, ")"),
    label    = paste0("tab:any-scholarship-rd-va-interaction-", ver$tag)
  )
  print(ver$res[, .(spec, bw_used, n_obs, above_b, above_se, inter_b, inter_se)])
}

# ── 8. Bandwidth sensitivity ──────────────────────────────────────────────────
BW_FACTORS <- c(0.5, 0.75, 1.0, 1.25, 1.5)
message("Running bandwidth sensitivity (factors: ", paste(BW_FACTORS, collapse = ", "), ").")

run_sensitivity <- function(h, rv, label) {
  rbindlist(lapply(BW_FACTORS, function(fac) {
    rbindlist(lapply(seq_len(nrow(va_specs)), function(i) {
      r <- run_rd_spec(universe, va_specs[i], bw = h * fac, rv = rv)
      if (is.null(r)) return(NULL)
      as.data.table(r)[, `:=`(bw_factor = fac, h_optimal = h, version = label)]
    }), use.names = TRUE, fill = TRUE)
  }), use.names = TRUE, fill = TRUE)
}

sensitivity <- rbindlist(list(
  run_sensitivity(h_pts, "psu_centered_pts", "pts"),
  run_sensitivity(h_sd,  "psu_centered_sd",  "sd")
), use.names = TRUE)

sens_path <- file.path(table_dir, "any_scholarship_rd_va_sensitivity.csv")
fwrite(sensitivity, sens_path)
message("Wrote: ", sens_path)
print(sensitivity[, .(version, bw_factor, bw_used, spec, n_obs, above_b, inter_b, inter_se)])
message("Done.")
