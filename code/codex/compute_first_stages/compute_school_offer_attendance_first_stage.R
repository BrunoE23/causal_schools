suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

data_wd <- "C:/Users/xd-br/Dropbox/causal_schools"

samples_path <- file.path(data_wd, "data/clean/sae_grade9_unique_proceso.RData")
treatment_path <- file.path(data_wd, "data/clean/treatment_1R_v2.RData")
rbd_universe_path <- file.path(data_wd, "data/clean/rbd_universe.csv")
output_path <- file.path(data_wd, "data/clean/first_stage_school_offer_attendance.csv")

load(samples_path)
load(treatment_path)
rbd_universe <- read_csv(rbd_universe_path, show_col_types = FALSE) %>% 
  select(-`...1`)

# br_code is a course-level school identifier nested within an applied school rbd.
# For school-level first stages, merge offers at the br_code level and then collapse
# to one row per student-process-school using first-round offers to that target school.

mean_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

plot_first_stage_distribution <- function(df,
                                          first_stage_col,
                                          year_label,
                                          min_apps = 200,
                                          min_offers = 100) {
  sample_schools <- df %>%
    filter(n_apps > min_apps, n_offers_1r > min_offers) %>%
    select(
      rbd,
      first_stage_y1,
      first_stage_y4,
      n_apps,
      n_offers_1r,
      prop_attend_y1_given_offer,
      prop_attend_y4_given_offer
    ) %>%
    arrange(.data[[first_stage_col]])

  ggplot(sample_schools, aes(x = .data[[first_stage_col]])) +
    geom_histogram(
      bins = 28,
      color = "#16324f",
      fill = "#6fa8dc",
      linewidth = 0.4,
      alpha = 0.9
    ) +
    geom_vline(
      xintercept = mean(sample_schools[[first_stage_col]], na.rm = TRUE),
      color = "#b13a2f",
      linewidth = 0.9
    ) +
    geom_vline(
      xintercept = median(sample_schools[[first_stage_col]], na.rm = TRUE),
      color = "#1f6f50",
      linetype = "dashed",
      linewidth = 0.9
    ) +
    labs(
      title = paste0("School-Level First Stage in Year ", year_label),
      subtitle = paste0(
        "Sample restricted to schools with more than ",
        min_apps,
        " applications and more than ",
        min_offers,
        " first-round offers (N = ",
        nrow(sample_schools),
        ")"
      ),
      x = "Offer effect on attendance in target school",
      y = "Number of schools",
      caption = "Solid red line = mean, dashed green line = median"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#16324f"),
      plot.subtitle = element_text(color = "#355070"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#d9e2ec", linewidth = 0.35),
      panel.grid.major.y = element_line(color = "#e8eef3", linewidth = 0.35),
      plot.caption = element_text(color = "#5c6770")
    )
}

school_applications <- sae_apps_grade9 %>%
  select(mrun, sae_proceso, rbd, br_code) %>%
  left_join(
    all_treatments %>% select(mrun, br_code, offered_spot_1R, rbd_treated_1R),
    by = c("mrun", "br_code")
  ) %>%
  rename(rbd_target = rbd) %>%
  group_by(mrun, sae_proceso, rbd_target) %>%
  summarise(
#    any_offer_1R = as.integer(any(offered_spot_1R == 1, na.rm = TRUE)),
    offered_target_school_1R = as.integer(any(rbd_treated_1R == rbd_target, na.rm = TRUE)),
    n_br_codes = n(),
    .groups = "drop"
  )

rbd_target_attendance <- school_applications %>%
  left_join(
    rbd_universe %>% select(MRUN, RBD_rel1, RBD_rel4),
    by = c("mrun" = "MRUN")
  ) %>%
  mutate(
    in_target_school_t_1 = as.integer(RBD_rel1 == rbd_target),
    in_target_school_t_4 = as.integer(RBD_rel4 == rbd_target)
  ) %>%
  select(-RBD_rel1, -RBD_rel4)

first_stage_school <- rbd_target_attendance %>%
  group_by(rbd = rbd_target) %>%
  summarise(
    n_apps = n(),
    n_offers_1r = sum(offered_target_school_1R == 1, na.rm = TRUE),
  #  n_offers_target_school_1r = sum(offered_target_school_1R == 1, na.rm = TRUE),
    prop_attend_y1_given_offer = mean_or_na(in_target_school_t_1[offered_target_school_1R == 1]),
    prop_attend_y4_given_offer = mean_or_na(in_target_school_t_4[offered_target_school_1R == 1]),
    prop_attend_y1_given_no_offer = mean_or_na(in_target_school_t_1[offered_target_school_1R == 0]),
    prop_attend_y4_given_no_offer = mean_or_na(in_target_school_t_4[offered_target_school_1R == 0]),
    first_stage_y1 =  prop_attend_y1_given_offer - prop_attend_y1_given_no_offer,
    first_stage_y4 =  prop_attend_y4_given_offer - prop_attend_y4_given_no_offer,
    .groups = "drop"
  ) %>%
  arrange(rbd)

write_csv(first_stage_school, output_path)

cat("Wrote:", output_path, "\n")
cat("Schools:", nrow(first_stage_school), "\n")
cat("Applications after school-level collapse:", nrow(school_applications), "\n")
#cat("Total first-round offers to any school:", sum(first_stage_school$n_any_offers_1r, na.rm = TRUE), "\n")
cat("Total first-round offers to target school:", sum(first_stage_school$n_offers_1r, na.rm = TRUE), "\n")

first_stage_school %>% 
  filter(n_apps > 200 & n_offers_1r > 100)


first_stage_plot_y1 <- plot_first_stage_distribution(
  first_stage_school,
  first_stage_col = "first_stage_y1",
  year_label = 1
)

first_stage_plot_y4 <- plot_first_stage_distribution(
  first_stage_school,
  first_stage_col = "first_stage_y4",
  year_label = 4
)

print(first_stage_plot_y1)
print(first_stage_plot_y4)


