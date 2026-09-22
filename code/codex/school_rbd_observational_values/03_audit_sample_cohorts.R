# Read only the columns needed to audit cohort eligibility, without fitting VA.
library(data.table)
exprs <- parse("code/codex/school_rbd_observational_values/01_construct_school_rbd_values.R")
config <- new.env()
needed <- c("baseline_score_vars", "baseline_score_poly_terms",
            "baseline_cpad_control_vars", "middle_school_control_vars", "control_vars")
for (expr in exprs) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% needed) eval(expr, config)
}
root <- Sys.getenv("CAUSAL_SCHOOLS_DATA_WD", "C:/Users/brunem/Box/causal_schools")
cols <- unique(c(config$control_vars, "most_time_rbd_middle", "most_time_RBD", "math_max", "leng_max"))
d <- fread(file.path(root, "data/clean/univ_gr8_df.csv"), select = cols,
           na.strings = c("", "NA"), showProgress = FALSE, nThread = 2)
d[, valid := !is.na(most_time_RBD) & most_time_RBD > 0 &
    !is.na(EDAD_ALU) & EDAD_ALU >= 12 & EDAD_ALU <= 16]
d[, complete := complete.cases(d[, c(config$control_vars, "most_time_rbd_middle"), with = FALSE])]
print(d[, .(universe = .N, complete_controls = sum(valid & complete),
            exam_takers = sum(valid & complete & (!is.na(math_max) | !is.na(leng_max)))),
        by = cohort_gr8][order(cohort_gr8)])
