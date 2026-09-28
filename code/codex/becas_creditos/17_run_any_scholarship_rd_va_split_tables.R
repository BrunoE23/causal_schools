###############################################################################
# RD VA split tables: any scholarship take-up, by school VA group
#
# For each of the 5 EB VA measures, splits schools at p50 (and separately
# at p75) and runs rdrobust within each half.  Reports the jump (gamma_1)
# for each group.
#
# Outputs
# -------
#   output/tables/becas_creditos/any_scholarship_rd_p50_split.{tex,csv}
#   output/tables/becas_creditos/any_scholarship_rd_p75_split.{tex,csv}
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(haven); library(rdrobust)
})

data_wd <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", unset = "C:/Users/brunem/Box/causal_schools")
repo_wd <- Sys.getenv("CAUSAL_SCHOOLS_REPO_WD", unset = "C:/Users/brunem/Research/causal_schools")
clean_dir      <- file.path(data_wd, "data", "clean")
eb_dir         <- file.path(clean_dir, "empirical_bayes_school_va", "cohorts_2017_2020")
table_dir      <- file.path(repo_wd, "output", "tables", "becas_creditos")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

key_from_numeric <- function(x) as.character(as.numeric(x))

va_specs <- data.table(
  spec       = c("math",            "verbal",          "highinst",     "highfield",            "progincome"),
  eb_outcome = c("z_year_math_max", "z_year_leng_max", "high_inst_m1", "high_paying_field_m1", "log_program_income_clp_m1"),
  label      = c("Math VA",         "Verbal VA",       "Hi-inst. VA",  "Hi-field VA",          "Proj. income VA")
)

# ── Universe ──────────────────────────────────────────────────────────────────
message("Reading universe.")
universe <- fread(file.path(clean_dir, "univ_gr8_df.csv"),
  select = c("MRUN", "math_max", "leng_max", "psu_year", "most_time_RBD", "income_decile"),
  na.strings = c("", "NA"))
universe <- universe[math_max > 0 & leng_max > 0]
universe[, `:=`(
  mrun_key    = key_from_numeric(MRUN),
  rbd         = as.numeric(most_time_RBD),
  psu_average = (math_max + leng_max) / 2,
  cutoff_yr   = fifelse(psu_year <= 2022L, 500L, 510L)
)]
universe[, psu_centered := psu_average - cutoff_yr]

# ── Any scholarship outcome ───────────────────────────────────────────────────
message("Reading benefit assignments.")
asgn <- as.data.table(read_dta(file.path(clean_dir, "asignaciones.dta"),
  col_select = c("mrun", "beneficio_beca_fscu")))
asgn[, benefit := toupper(trimws(beneficio_beca_fscu))]
any_ids <- union(
  key_from_numeric(asgn[benefit == "BBIC", mrun]),
  key_from_numeric(asgn[benefit == "BJGM", mrun])
)
universe[, beca_any := as.integer(mrun_key %chin% any_ids)]

# ── EB school VA — all 5 outcomes ─────────────────────────────────────────────
message("Reading EB school values.")
eb_raw <- fread(file.path(eb_dir, "eb_school_rbd_observational_values.csv"),
                na.strings = c("", "NA"))
eb_raw <- eb_raw[
  analysis_sample == "All" & outcome %chin% va_specs$eb_outcome,
  .(rbd = as.numeric(school_rbd), outcome,
    va  = controlled_value_added_eb_centered_student)
]
eb_wide <- dcast(eb_raw, rbd ~ outcome, value.var = "va")
setnames(eb_wide, va_specs$eb_outcome, paste0("school_va_", va_specs$spec))
universe <- merge(universe, eb_wide, by = "rbd", all.x = TRUE)

# ── MSE-optimal bandwidth on full sample ─────────────────────────────────────
message("rdrobust bandwidth on full sample (outcome = beca_any).")
rdr_full <- rdrobust(
  y = universe[!is.na(beca_any) & !is.na(psu_centered)]$beca_any,
  x = universe[!is.na(beca_any) & !is.na(psu_centered)]$psu_centered,
  c = 0, kernel = "triangular", vce = "hc1"
)
h_opt <- mean(rdr_full$bws["h", ])
message(sprintf("  h = +/- %.1f pts", h_opt))

# ── Split-table estimation ────────────────────────────────────────────────────
run_split <- function(percentile) {
  cutoff_q <- percentile / 100

  rbindlist(lapply(seq_len(nrow(va_specs)), function(i) {
    va_col <- paste0("school_va_", va_specs$spec[i])
    dt <- universe[!is.na(beca_any) & !is.na(psu_centered) & !is.na(get(va_col))]

    # Split on school-level VA percentile (unique school values)
    school_vals <- unique(dt[, .(rbd, va = get(va_col))])
    q_val <- quantile(school_vals$va, cutoff_q)

    rd_one <- function(sub, label) {
      if (nrow(sub) < 100) return(NULL)
      sub_c <- sub[!is.na(income_decile)]
      covs_mat <- model.matrix(~ factor(income_decile), data = sub_c)[, -1, drop = FALSE]
      r <- tryCatch(
        rdrobust(y = sub_c$beca_any, x = sub_c$psu_centered, c = 0,
                 h = h_opt, covs = covs_mat, kernel = "triangular", vce = "hc1"),
        error = function(e) NULL
      )
      if (is.null(r)) return(NULL)
      data.table(
        spec         = va_specs$spec[i],
        label        = va_specs$label[i],
        group        = label,
        n_obs        = nrow(sub_c),
        outcome_mean = mean(sub_c$beca_any, na.rm = TRUE),
        jump         = r$coef["Conventional", ],
        se_rob       = r$se["Robust", ],
        pval         = r$pv["Robust", ],
        ci_lo        = r$ci["Robust", 1],
        ci_hi        = r$ci["Robust", 2]
      )
    }

    lo_label <- if (percentile == 50) "Below median" else "Bottom 75%"
    hi_label <- if (percentile == 50) "Above median" else "Top 25%"

    rbindlist(list(
      rd_one(dt[get(va_col) <  q_val], lo_label),
      rd_one(dt[get(va_col) >= q_val], hi_label)
    ), use.names = TRUE)
  }), use.names = TRUE)
}

message("Running p50 split.")
res_p50 <- run_split(50)
message("Running p75 split.")
res_p75 <- run_split(75)

cat("\n=== p50 split ===\n"); print(res_p50[, .(label, group, n_obs, jump=round(jump,4), se_rob=round(se_rob,4), pval=round(pval,4))])
cat("\n=== p75 split ===\n"); print(res_p75[, .(label, group, n_obs, jump=round(jump,4), se_rob=round(se_rob,4), pval=round(pval,4))])

# ── LaTeX table builder ───────────────────────────────────────────────────────
fmt_est  <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
fmt_se   <- function(x) ifelse(is.na(x), "", paste0("(", sprintf("%.3f", x), ")"))
stars    <- function(p) fifelse(is.na(p), "",
             fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.10, "*", ""))))
fmt_star <- function(x, p) paste0(fmt_est(x), stars(p))

make_split_table <- function(res, lo_label, hi_label, tex_path, csv_path, caption, tab_label) {

  wide <- dcast(res, spec + label ~ group,
    value.var = c("jump", "se_rob", "pval", "n_obs", "outcome_mean"))

  lo_j  <- paste0("jump_",         lo_label)
  lo_se <- paste0("se_rob_",       lo_label)
  lo_p  <- paste0("pval_",         lo_label)
  lo_n  <- paste0("n_obs_",        lo_label)
  hi_j  <- paste0("jump_",         hi_label)
  hi_se <- paste0("se_rob_",       hi_label)
  hi_p  <- paste0("pval_",         hi_label)
  hi_n  <- paste0("n_obs_",        hi_label)

  nrows <- nrow(wide)

  rows <- unlist(lapply(seq_len(nrows), function(i) {
    r <- wide[i]
    c(
      paste0(r$label, " & ",
        fmt_star(r[[lo_j]], r[[lo_p]]), " & ",
        fmt_star(r[[hi_j]], r[[hi_p]]), " \\\\"),
      paste0(" & ",
        fmt_se(r[[lo_se]]), " & ",
        fmt_se(r[[hi_se]]), " \\\\"),
      paste0(" & \\multicolumn{1}{c}{\\textit{N} = ", format(r[[lo_n]], big.mark=","),
             "} & \\multicolumn{1}{c}{\\textit{N} = ", format(r[[hi_n]], big.mark=","),
             "} \\\\[4pt]")
    )
  }))

  latex <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", tab_label, "}"),
    "\\begin{tabular}{lcc}",
    "\\toprule",
    paste0(" & ", lo_label, " & ", hi_label, " \\\\"),
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\medskip\\footnotesize",
    "\\begin{minipage}{\\textwidth}",
    paste0(
      "Notes: Outcome is take-up of any need-based scholarship (Beca Bicentenario or Beca Juan Gomez Millas). ",
      "Each row uses a different school EB value-added measure to split schools at the indicated percentile. ",
      "RD jump ($\\hat{\\gamma}_1$) estimated via rdrobust using the MSE-optimal bandwidth from the full sample ",
      "(h = ", sprintf("%.1f", h_opt), " pts, triangular kernel). ",
      "Robust standard errors in parentheses. ",
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

make_split_table(
  res_p50, "Below median", "Above median",
  tex_path  = file.path(table_dir, "any_scholarship_rd_p50_split.tex"),
  csv_path  = file.path(table_dir, "any_scholarship_rd_p50_split.csv"),
  caption   = "Scholarship Take-up RD Jump by School VA Group (Median Split)",
  tab_label = "tab:any-scholarship-rd-p50-split"
)

make_split_table(
  res_p75, "Bottom 75%", "Top 25%",
  tex_path  = file.path(table_dir, "any_scholarship_rd_p75_split.tex"),
  csv_path  = file.path(table_dir, "any_scholarship_rd_p75_split.csv"),
  caption   = "Scholarship Take-up RD Jump by School VA Group (Top Quartile Split)",
  tab_label = "tab:any-scholarship-rd-p75-split"
)

message("Done.")
