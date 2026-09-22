suppressPackageStartupMessages(library(data.table))

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

apps_path <- file.path(data_wd, "data/clean/sae_grade9_unique_proceso.RData")
clean_eb_dir <- file.path(data_wd, "data/clean/empirical_bayes_school_va")
output_dir <- file.path(repo_wd, "output/tables/compliance")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

apps_env <- new.env()
load(apps_path, envir = apps_env)
if (!exists("sae_apps_grade9", envir = apps_env, inherits = FALSE)) {
  stop("Expected sae_apps_grade9 in ", apps_path)
}
apps <- as.data.table(apps_env$sae_apps_grade9)
apps <- apps[
  preferencia_postulante %in% 1:3,
  .(mrun = as.numeric(mrun), sae_proceso = as.integer(sae_proceso),
    rank = as.integer(preferencia_postulante), choice_rbd = as.numeric(rbd))
]
if (anyDuplicated(apps[, .(mrun, sae_proceso, rank)])) {
  stop("Application data are not unique by student, process, and rank.")
}
apps_wide <- dcast(apps, mrun + sae_proceso ~ rank, value.var = "choice_rbd")
setnames(apps_wide, c("1", "2", "3"), paste0("choice_", 1:3))

pairs <- data.table(
  pair = c("maximum_data", "no_overlap"),
  label = c("VA 2017--2020; SAE 2018--2020", "VA 2017--2018; SAE 2019--2020"),
  input_tag = c("va_2017_2020__sae_2018_2020", "va_2017_2018__sae_2019_2020")
)
if (!identical(Sys.getenv("RUN_NO_OVERLAP", unset = "0"), "1")) {
  pairs <- pairs[pair == "maximum_data"]
}

make_pair <- function(pair_name, pair_label, input_tag) {
  input_path <- file.path(clean_eb_dir, input_tag, "scalar_iv_regression_df.csv")
  dt <- fread(
    input_path,
    select = c("mrun", "sae_proceso", "rbd_treated_1R", "most_time_RBD"),
    na.strings = c("", "NA"), showProgress = FALSE
  )
  dt[, mrun := as.numeric(mrun)]
  dt[, sae_proceso := as.integer(sae_proceso)]
  dt[, rbd_treated_1R := as.numeric(rbd_treated_1R)]
  dt[, most_time_RBD := as.numeric(most_time_RBD)]
  if (anyDuplicated(dt[, .(mrun, sae_proceso)])) {
    stop("Regression sample is not unique by student and process: ", input_tag)
  }
  dt <- merge(dt, apps_wide, by = c("mrun", "sae_proceso"), all.x = TRUE, sort = FALSE)
  dt[, offer_status := fifelse(
    !is.na(rbd_treated_1R) & rbd_treated_1R > 0,
    "Recorded offer", "No recorded offer"
  )]

  rank_rows <- rbindlist(lapply(1:3, function(k) {
    choice_col <- paste0("choice_", k)
    eligible <- dt[!is.na(get(choice_col))]
    ans <- eligible[, .(
      n = .N,
      share_attend = mean(!is.na(most_time_RBD) & most_time_RBD == get(choice_col))
    ), by = offer_status]
    ans[, rank_order := k]
    ans[, choice := paste0("Choice ", k)]
    ans
  }))

  dt[, attends_top3 := (!is.na(choice_1) & most_time_RBD == choice_1) |
       (!is.na(choice_2) & most_time_RBD == choice_2) |
       (!is.na(choice_3) & most_time_RBD == choice_3)]
  any_top3 <- dt[!is.na(choice_1), .(
    n = .N, share_attend = mean(attends_top3, na.rm = TRUE)
  ), by = offer_status]
  any_top3[, rank_order := 4L]
  any_top3[, choice := "Any of top 3"]

  long <- rbind(rank_rows, any_top3, use.names = TRUE)
  shares <- dcast(long, rank_order + choice ~ offer_status, value.var = "share_attend")
  counts <- dcast(long, rank_order + choice ~ offer_status, value.var = "n")
  setnames(shares, c("No recorded offer", "Recorded offer"), c("share_no_offer", "share_offer"))
  setnames(counts, c("No recorded offer", "Recorded offer"), c("n_no_offer", "n_offer"))
  out <- merge(shares, counts, by = c("rank_order", "choice"), sort = FALSE)
  out[, difference_offer_minus_no_offer := share_offer - share_no_offer]
  out[, pair := pair_name]
  out[, pair_label := pair_label]
  setcolorder(out, c(
    "pair", "pair_label", "rank_order", "choice", "n_offer", "share_offer",
    "n_no_offer", "share_no_offer", "difference_offer_minus_no_offer"
  ))
  setorder(out, rank_order)

  csv_path <- file.path(output_dir, paste0("application_rank_take_up_", pair_name, ".csv"))
  tex_path <- file.path(output_dir, paste0("application_rank_take_up_", pair_name, ".tex"))
  fwrite(out, csv_path)

  lines <- c(
    "\\begin{table}[!htbp]", "\\centering",
    paste0("\\caption{Eventual school by SAE application rank: ", pair_label, "}"),
    paste0("\\label{tab:application-rank-takeup-", gsub("_", "-", pair_name), "}"),
    "\\begin{tabular}{lrrrrr}", "\\toprule",
    " & \\multicolumn{2}{c}{Recorded offer} & \\multicolumn{2}{c}{No recorded offer} & Difference \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "Application position & N & Share & N & Share & Offer $-$ no offer \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(out))) {
    lines <- c(lines, paste0(
      out$choice[i], " & ", format(out$n_offer[i], big.mark = ","), " & ",
      sprintf("%.1f\\%%", 100 * out$share_offer[i]), " & ",
      format(out$n_no_offer[i], big.mark = ","), " & ",
      sprintf("%.1f\\%%", 100 * out$share_no_offer[i]), " & ",
      sprintf("%+.1f pp", 100 * out$difference_offer_minus_no_offer[i]), " \\\\"
    ))
  }
  lines <- c(
    lines, "\\bottomrule", "\\end{tabular}", "\\par\\medskip", "\\footnotesize",
    "\\begin{minipage}{0.95\\textwidth}",
    "Notes: Each row reports the share whose most-time high school after grade 8 equals the indicated position in the student's SAE application list. Rank-specific denominators include students who submitted a school at that rank. The top-three denominator includes students with a first choice and counts attendance at any available choice among positions 1--3. Recorded offer means a positive school identifier in the regular first-round SAE assignment record.",
    "\\end{minipage}", "\\end{table}"
  )
  writeLines(lines, tex_path)
  out
}

results <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
  make_pair(pairs$pair[i], pairs$label[i], pairs$input_tag[i])
}))
fwrite(results, file.path(output_dir, "application_rank_take_up_all_pairs.csv"))
print(results)
