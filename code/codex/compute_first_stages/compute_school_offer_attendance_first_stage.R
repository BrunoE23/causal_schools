suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

data_wd <- "C:/Users/xd-br/Dropbox/causal_schools"

samples_path <- file.path(data_wd, "data/clean/samples.RData")
treatment_path <- file.path(data_wd, "data/clean/treatment_1R.RData")
tracking_path <- file.path(data_wd, "data/clean/tracking_clean_wide.RData")
output_path <- file.path(data_wd, "data/clean/first_stage_school_offer_attendance.csv")

load(samples_path)
load(treatment_path)
load(tracking_path)

# br_code is a course-level school identifier nested within an applied school rbd.
# For school-level first stages, merge offers at the br_code level and then collapse
# to one row per student-process-school using any first-round offer within the school.

mean_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

school_applications <- sample_students %>%
  select(mrun, proceso, rbd, br_code) %>%
  left_join(
    all_treatments %>% select(mrun, br_code, offered_spot_1R),
    by = c("mrun", "br_code")
  ) %>%
  rename(rbd_target = rbd) %>%
  group_by(mrun, proceso, rbd_target) %>%
  summarise(
    offered_spot_1R = as.integer(any(offered_spot_1R == 1, na.rm = TRUE)),
    n_br_codes = n(),
    .groups = "drop"
  )

tracking_target <- tracking_window_wide %>%
  select(mrun, proceso, rbd_target, in_target_school_t_0, in_target_school_t_3)

first_stage_school <- school_applications %>%
  left_join(tracking_target, by = c("mrun", "proceso", "rbd_target")) %>%
  group_by(rbd = rbd_target) %>%
  summarise(
    n_apps = n(),
    n_offers_1r = sum(offered_spot_1R == 1, na.rm = TRUE),
    prop_attend_y1_given_offer = mean_or_na(in_target_school_t_0[offered_spot_1R == 1]),
    prop_attend_y4_given_offer = mean_or_na(in_target_school_t_3[offered_spot_1R == 1]),
    .groups = "drop"
  ) %>%
  arrange(rbd)

write_csv(first_stage_school, output_path)

cat("Wrote:", output_path, "\n")
cat("Schools:", nrow(first_stage_school), "\n")
cat("Applications after school-level collapse:", nrow(school_applications), "\n")
cat("Total first-round offers:", sum(first_stage_school$n_offers_1r, na.rm = TRUE), "\n")
