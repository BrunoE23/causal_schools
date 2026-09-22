suppressPackageStartupMessages({
  library(data.table)
})

find_existing_path <- function(env_var, candidates, label) {
  candidates <- c(Sys.getenv(env_var), candidates)
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (!length(candidates)) stop("Could not find ", label, ".")
  candidates[[1]]
}

data_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_DATA_WD",
  c("C:/Users/brunem/Box/causal_schools", "C:/Users/xd-br/Box/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools", "C:/Users/xd-br/Dropbox/causal_schools"),
  "data_wd"
)
repo_wd <- find_existing_path(
  "CAUSAL_SCHOOLS_REPO_WD",
  c(getwd(), "C:/Users/brunem/Research/causal_schools"),
  "repo_wd"
)

clean_eb_dir <- file.path(data_wd, "data/clean/empirical_bayes_school_va")
universe_path <- file.path(data_wd, "data/clean/univ_gr8_df.csv")
output_dir <- file.path(repo_wd, "output/tables/compliance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

pairs <- data.table(
  pair = c("maximum_data", "no_overlap"),
  label = c("VA 2017--2020; SAE 2018--2020", "VA 2017--2018; SAE 2019--2020"),
  input_tag = c("va_2017_2020__sae_2018_2020", "va_2017_2018__sae_2019_2020")
)

specs <- data.table(
  spec = c("math_adj_eb", "leng_adj_eb", "exam_adj_eb", "highinst_adj_eb",
           "highpay_adj_eb", "program_income_adj_eb", "anypost_adj_eb"),
  label = c("Math", "Language", "Exam taking", "High-premium institution",
            "High-premium field", "Program income", "Benefits/credit application"),
  outcome = c("z_year_math_max", "z_year_leng_max", "admission_exam_taker", "high_inst_m1",
              "high_paying_field_m1", "log_program_income_clp_m1", "any_postulacion"),
  require_exam_taker = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
)

covariates <- fread(
  universe_path,
  select = c("student_id", "simce4_math_decile", "income_decile_imputed"),
  na.strings = c("", "NA"), showProgress = FALSE
)
covariates[, student_id := as.numeric(student_id)]
if (anyDuplicated(covariates$student_id)) stop("Universe is not unique by student_id.")

summarize_compliance <- function(dt, panel, row_label, row_order) {
  dt[, .(
    panel = panel,
    row_label = row_label,
    row_order = row_order,
    n_students = .N,
    n_offered = sum(positive_offer),
    offer_rate = mean(positive_offer),
    attend_offer_rate_all = mean(attended_offer),
    take_up_given_offer = if (sum(positive_offer) > 0) mean(attended_offer[positive_offer]) else NA_real_
  )]
}

make_pair <- function(pair_name, pair_label, input_tag) {
  input_path <- file.path(clean_eb_dir, input_tag, "scalar_iv_regression_df.csv")
  if (!file.exists(input_path)) stop("Missing regression data: ", input_path)
  dt <- fread(input_path, na.strings = c("", "NA"), showProgress = FALSE)
  dt[, student_id := as.numeric(student_id)]
  dt <- merge(dt, covariates, by = "student_id", all.x = TRUE, sort = FALSE)
  dt[, positive_offer := !is.na(rbd_treated_1R) & rbd_treated_1R > 0]
  dt[, attended_offer := positive_offer & !is.na(most_time_RBD) & most_time_RBD == rbd_treated_1R]
  dt[, gender_label := fcase(GEN_ALU == 1, "Male", GEN_ALU == 2, "Female", default = "Other/missing")]
  dt[, math_quintile := fifelse(
    !is.na(simce4_math_decile) & simce4_math_decile >= 1 & simce4_math_decile <= 10,
    ceiling(simce4_math_decile / 2), NA_real_
  )]
  dt[, income_quintile := fifelse(
    !is.na(income_decile_imputed) & income_decile_imputed >= 1 & income_decile_imputed <= 10,
    ceiling(income_decile_imputed / 2), NA_real_
  )]

  out <- summarize_compliance(dt, "Overall", "All lottery-risk students", 1L)
  out <- rbind(out, dt[, summarize_compliance(.SD, "SAE cohort", as.character(.BY$sae_proceso), .BY$sae_proceso), by = sae_proceso][, sae_proceso := NULL])
  out <- rbind(out, dt[, summarize_compliance(.SD, "Gender", .BY$gender_label, match(.BY$gender_label, c("Female", "Male", "Other/missing"))), by = gender_label][, gender_label := NULL])
  out <- rbind(out, dt[!is.na(math_quintile), summarize_compliance(.SD, "Grade-4 math quintile", paste0("Q", .BY$math_quintile), .BY$math_quintile), by = math_quintile][, math_quintile := NULL])
  out <- rbind(out, dt[!is.na(income_quintile), summarize_compliance(.SD, "Baseline income quintile", paste0("Q", .BY$income_quintile), .BY$income_quintile), by = income_quintile][, income_quintile := NULL])

  outcome_rows <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    s <- specs[i]
    needed <- c(s$outcome, paste0("d_", s$spec), paste0("z_", s$spec),
                paste0("expected_", s$spec), "cohort_gr8", "z_sim_mat_4to",
                "z_sim_leng_4to", "GEN_ALU", "EDAD_ALU")
    if (s$require_exam_taker) needed <- c(needed, "admission_exam_taker")
    reg <- dt[complete.cases(dt[, ..needed])]
    if (s$require_exam_taker) reg <- reg[admission_exam_taker == 1L]
    summarize_compliance(reg, "Main-table outcome sample", s$label, i)
  }))
  out <- rbind(out, outcome_rows, use.names = TRUE)
  out[, `:=`(pair = pair_name, pair_label = pair_label)]
  panel_levels <- c("Overall", "SAE cohort", "Gender", "Grade-4 math quintile",
                    "Baseline income quintile", "Main-table outcome sample")
  out[, panel_order := match(panel, panel_levels)]
  setorder(out, panel_order, row_order)
  out[, panel_order := NULL]
  setcolorder(out, c("pair", "pair_label", "panel", "row_label", "row_order",
                     "n_students", "n_offered", "offer_rate",
                     "attend_offer_rate_all", "take_up_given_offer"))

  csv_path <- file.path(output_dir, paste0("offer_take_up_", pair_name, ".csv"))
  tex_path <- file.path(output_dir, paste0("offer_take_up_", pair_name, ".tex"))
  fwrite(out, csv_path)

  lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{First-round offer take-up: ", pair_label, "}"),
    paste0("\\label{tab:offer-takeup-", gsub("_", "-", pair_name), "}"),
    "\\begin{tabular}{llrrrrr}", "\\toprule",
    "Panel & Group & N & Offered & Offer rate & Attend offer/all & Take-up $\\mid$ offer \\\\",
    "\\midrule"
  )
  panels <- unique(out$panel)
  for (panel_name in panels) {
    block <- out[panel == panel_name]
    for (i in seq_len(nrow(block))) {
      lines <- c(lines, paste0(
        if (i == 1) panel_name else "", " & ", block$row_label[i], " & ",
        format(block$n_students[i], big.mark = ","), " & ",
        format(block$n_offered[i], big.mark = ","), " & ",
        sprintf("%.1f\\%%", 100 * block$offer_rate[i]), " & ",
        sprintf("%.1f\\%%", 100 * block$attend_offer_rate_all[i]), " & ",
        sprintf("%.1f\\%%", 100 * block$take_up_given_offer[i]), " \\\\")
      )
    }
    if (panel_name != tail(panels, 1)) lines <- c(lines, "\\addlinespace")
  }
  lines <- c(
    lines, "\\bottomrule", "\\end{tabular}", "\\par\\medskip", "\\footnotesize",
    "\\begin{minipage}{0.98\\textwidth}",
    paste0(
      "Notes: Offered indicates a positive first-round SAE offer. Attend offer indicates that the student's most-time high school after grade 8 equals the first-round offered school. The unconditional attendance rate treats students without a first-round offer as non-takers; the final column is conditional on receiving a positive offer. Math and income quintiles are formed from baseline deciles. The sample contains timely applicants with nondegenerate simulated assignment risk in the stated SAE cohorts."
    ),
    "\\end{minipage}", "\\end{table}"
  )
  writeLines(lines, tex_path)
  out
}

all_results <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
  make_pair(pairs$pair[i], pairs$label[i], pairs$input_tag[i])
}))
fwrite(all_results, file.path(output_dir, "offer_take_up_all_pairs.csv"))
print(all_results[panel == "Overall"])
