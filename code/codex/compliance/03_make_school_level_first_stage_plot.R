suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
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

input_tag <- Sys.getenv(
  "SCHOOL_FIRST_STAGE_INPUT_TAG",
  unset = "va_2017_2020__sae_2018_2020"
)
top_n_schools <- as.integer(Sys.getenv("SCHOOL_FIRST_STAGE_TOP_N", unset = "100"))
if (is.na(top_n_schools) || top_n_schools <= 0) {
  stop("SCHOOL_FIRST_STAGE_TOP_N must be a positive integer.")
}

apps_path <- file.path(data_wd, "data/clean/sae_grade9_unique_proceso.RData")
iv_path <- file.path(
  data_wd,
  "data/clean/empirical_bayes_school_va",
  input_tag,
  "scalar_iv_regression_df.csv"
)
table_dir <- file.path(repo_wd, "output/tables/compliance")
figure_dir <- file.path(repo_wd, "output/figures/compliance")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading IV sample: ", iv_path)
iv <- fread(
  iv_path,
  select = c("mrun", "sae_proceso", "timely_sae", "rbd_treated_1R",
             "most_time_RBD", "any_risk"),
  na.strings = c("", "NA"),
  showProgress = FALSE
)
iv[, `:=`(
  mrun = as.numeric(mrun),
  sae_proceso = as.integer(sae_proceso),
  timely_sae = as.integer(timely_sae),
  rbd_treated_1R = as.numeric(rbd_treated_1R),
  most_time_RBD = as.numeric(most_time_RBD),
  any_risk = as.integer(any_risk)
)]
iv <- iv[
  timely_sae == 1L &
    any_risk == 1L &
    !is.na(most_time_RBD) & most_time_RBD > 0
]
if (anyDuplicated(iv[, .(mrun, sae_proceso)])) {
  stop("IV sample is not unique by mrun and sae_proceso.")
}

message("Reading SAE applications: ", apps_path)
apps_env <- new.env()
load(apps_path, envir = apps_env)
if (!exists("sae_apps_grade9", envir = apps_env, inherits = FALSE)) {
  stop("Expected sae_apps_grade9 in ", apps_path)
}
apps <- as.data.table(apps_env$sae_apps_grade9)
apps <- apps[
  ,
  .(
    mrun = as.numeric(mrun),
    sae_proceso = as.integer(sae_proceso),
    rbd_target = as.numeric(rbd)
  )
]
apps <- unique(apps[!is.na(rbd_target) & rbd_target > 0])

student_school <- merge(apps, iv, by = c("mrun", "sae_proceso"), all = FALSE, sort = FALSE)
student_school[, offered_target := as.integer(!is.na(rbd_treated_1R) & rbd_treated_1R == rbd_target)]
student_school[, attended_target := as.integer(!is.na(most_time_RBD) & most_time_RBD == rbd_target)]

diff_stats <- function(y, z) {
  n_offer <- sum(z == 1L)
  n_no_offer <- sum(z == 0L)
  p_offer <- if (n_offer > 0) mean(y[z == 1L]) else NA_real_
  p_no_offer <- if (n_no_offer > 0) mean(y[z == 0L]) else NA_real_
  first_stage <- p_offer - p_no_offer
  influence <- numeric(length(y))
  if (n_offer > 0) influence[z == 1L] <- (y[z == 1L] - p_offer) / n_offer
  if (n_no_offer > 0) influence[z == 0L] <- -(y[z == 0L] - p_no_offer) / n_no_offer
  se <- if (n_offer > 0 && n_no_offer > 0) sqrt(sum(influence^2)) else NA_real_
  data.table(
    n_applications = length(y),
    n_offers = n_offer,
    n_no_offers = n_no_offer,
    attendance_if_offered = p_offer,
    attendance_if_not_offered = p_no_offer,
    first_stage = first_stage,
    se = se,
    ci_low = first_stage - 1.96 * se,
    ci_high = first_stage + 1.96 * se
  )
}

school_first_stage <- student_school[
  ,
  diff_stats(attended_target, offered_target),
  by = .(school_rbd = rbd_target)
]
school_first_stage[, first_stage_pp := 100 * first_stage]
school_first_stage[, se_pp := 100 * se]
school_first_stage[, ci_low_pp := 100 * ci_low]
school_first_stage[, ci_high_pp := 100 * ci_high]
school_first_stage[, baseline_attendance_pp := 100 * attendance_if_not_offered]
school_first_stage[, offered_attendance_pp := 100 * attendance_if_offered]
school_first_stage[, estimable := !is.na(first_stage)]
setorder(school_first_stage, -n_applications, school_rbd)

all_csv_path <- file.path(table_dir, "school_level_offer_attendance_first_stage.csv")
top_csv_path <- file.path(table_dir, "school_level_offer_attendance_first_stage_top100.csv")
summary_csv_path <- file.path(table_dir, "school_level_offer_attendance_first_stage_summary.csv")
plot_path <- file.path(figure_dir, "school_level_offer_attendance_first_stage_top100.png")
fwrite(school_first_stage, all_csv_path)

make_weighted_summary <- function(dt, label) {
  data.table(
    sample = label,
    n_schools = nrow(dt),
    n_applications = sum(dt$n_applications),
    share_student_school_applications = sum(dt$n_applications) / nrow(student_school),
    weighted_attendance_if_not_offered = weighted.mean(
      dt$attendance_if_not_offered,
      w = dt$n_applications,
      na.rm = TRUE
    ),
    weighted_attendance_if_offered = weighted.mean(
      dt$attendance_if_offered,
      w = dt$n_applications,
      na.rm = TRUE
    ),
    weighted_first_stage = weighted.mean(
      dt$first_stage,
      w = dt$n_applications,
      na.rm = TRUE
    )
  )
}

all_estimable_schools <- school_first_stage[estimable == TRUE & n_offers > 0 & n_no_offers > 0]

top_schools <- school_first_stage[
  estimable == TRUE & n_offers > 0 & n_no_offers > 0
][
  order(-n_applications, school_rbd)
][
  seq_len(min(.N, top_n_schools))
]
top_schools[, plot_order := frank(-n_applications, ties.method = "first")]
top_n_applications <- sum(top_schools$n_applications)
top_application_share <- top_n_applications / nrow(student_school)
top_weighted_first_stage_pp <- weighted.mean(
  top_schools$first_stage_pp,
  w = top_schools$n_applications,
  na.rm = TRUE
)
top_weighted_baseline_pp <- weighted.mean(
  top_schools$baseline_attendance_pp,
  w = top_schools$n_applications,
  na.rm = TRUE
)
top_weighted_offered_pp <- weighted.mean(
  top_schools$offered_attendance_pp,
  w = top_schools$n_applications,
  na.rm = TRUE
)
all_n_applications <- sum(all_estimable_schools$n_applications)
all_weighted_first_stage_pp <- weighted.mean(
  all_estimable_schools$first_stage_pp,
  w = all_estimable_schools$n_applications,
  na.rm = TRUE
)
all_weighted_baseline_pp <- weighted.mean(
  all_estimable_schools$baseline_attendance_pp,
  w = all_estimable_schools$n_applications,
  na.rm = TRUE
)
all_weighted_offered_pp <- weighted.mean(
  all_estimable_schools$offered_attendance_pp,
  w = all_estimable_schools$n_applications,
  na.rm = TRUE
)
fwrite(top_schools, top_csv_path)

summary_dt <- rbindlist(list(
  make_weighted_summary(all_estimable_schools, "All estimable applied schools"),
  make_weighted_summary(top_schools, paste0("Top ", nrow(top_schools), " applied schools"))
))
summary_dt[, `:=`(
  share_student_school_applications_pct = 100 * share_student_school_applications,
  weighted_attendance_if_not_offered_pct = 100 * weighted_attendance_if_not_offered,
  weighted_attendance_if_offered_pct = 100 * weighted_attendance_if_offered,
  weighted_first_stage_pp = 100 * weighted_first_stage
)]
fwrite(summary_dt, summary_csv_path)

plot_dt <- copy(top_schools)
plot_dt[, school_label := factor(school_rbd, levels = school_rbd[order(-n_applications, school_rbd)])]
plot_points <- melt(
  plot_dt,
  id.vars = c("school_rbd", "school_label", "n_applications"),
  measure.vars = c("baseline_attendance_pp", "offered_attendance_pp"),
  variable.name = "offer_status",
  value.name = "attendance_pp"
)
plot_points[, offer_status := fifelse(
  offer_status == "offered_attendance_pp",
  "Offered school",
  "Not offered school"
)]
annotation_x <- plot_dt$school_label[
  min(nrow(plot_dt), max(1L, floor(0.78 * nrow(plot_dt))))
]
first_stage_label <- paste0(
  "Application-weighted first stage\n",
  "schools shown: ",
  sprintf("%.1f", top_weighted_first_stage_pp),
  " pp"
)

p <- ggplot(plot_dt, aes(x = school_label)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_segment(
    aes(
      y = baseline_attendance_pp,
      yend = offered_attendance_pp,
      xend = school_label
    ),
    color = "grey70",
    linewidth = 0.45
  ) +
  geom_point(
    data = plot_points,
    aes(y = attendance_pp, color = offer_status),
    size = 2.2,
    alpha = 0.9
  ) +
  scale_color_manual(
    name = "First-round offer status",
    values = c("Not offered school" = "#7a7a7a", "Offered school" = "#1f5d8c")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3.2, alpha = 1))
  ) +
  annotate(
    "label",
    x = annotation_x,
    y = 96,
    label = first_stage_label,
    hjust = 0.5,
    vjust = 1,
    size = 3.3,
    linewidth = 0.25,
    fill = "white",
    color = "grey20"
  ) +
  labs(
    x = "Applied schools, ordered by number of applications",
    y = "Share attending the applied school (percentage points)",
    title = "Attendance at applied schools by first-round offer status",
    subtitle = paste0(
      nrow(plot_dt), " largest applied schools; ",
      format(top_n_applications, big.mark = ","), " student-school applications represented (",
      sprintf("%.1f", 100 * top_application_share), "% of all). ",
      "\nPopulation-weighted rates, all estimable applications (N = ",
      format(all_n_applications, big.mark = ","),
      "): not offered ",
      sprintf("%.1f", all_weighted_baseline_pp),
      "%, offered ",
      sprintf("%.1f", all_weighted_offered_pp),
      "%, first stage ",
      sprintf("%.1f", all_weighted_first_stage_pp),
      " pp"
    ),
    caption = "Segment length is the school-level first stage."
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave(plot_path, p, width = 10, height = 7, dpi = 300)

message("Wrote: ", all_csv_path)
message("Wrote: ", top_csv_path)
message("Wrote: ", summary_csv_path)
message("Wrote: ", plot_path)
message("Student-school rows: ", nrow(student_school))
message("Schools with applications: ", nrow(school_first_stage))
message("Schools in plot: ", nrow(top_schools))
message("Applications in plot: ", top_n_applications)
message("Share of student-school applications in plot: ", sprintf("%.1f%%", 100 * top_application_share))
message("Application-weighted first stage in plot: ", sprintf("%.1f pp", top_weighted_first_stage_pp))
print(summary_dt[, .(
  sample,
  n_schools,
  n_applications,
  share_student_school_applications_pct,
  weighted_attendance_if_not_offered_pct,
  weighted_attendance_if_offered_pct,
  weighted_first_stage_pp
)])
print(top_schools[, .(
  school_rbd,
  n_applications,
  n_offers,
  attendance_if_offered,
  attendance_if_not_offered,
  first_stage
)][1:min(.N, 10)])





