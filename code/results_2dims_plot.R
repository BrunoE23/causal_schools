####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


setwd(data_wd)
#####################################

library(tidyverse)


load("./data/clean/final_data.RData")
load("./data/clean/all_apps.RData")

sample_type <- "P1"
sample_type <- "V2"

effects_schools <- read_csv(paste0("./data/clean/effects_schools_long_", sample_type, ".csv"))

effects_schools <- effects_schools %>% 
  filter(!(is.na(school_id)))

FS_schools <- effects_schools %>% 
  filter(outcome == "n_years_rbd_target_post") %>% 
  mutate(FS = beta/4) %>% 
  select(school_id, group, FS)

effects_schools_late<- effects_schools %>% 
  left_join(FS_schools, by = c("group", "school_id")) %>% 
          mutate(beta = beta/FS)



#hist(FS_schools_all$beta)
#summary(FS_schools_all$beta)

effects_schools %>% 
  group_by(outcome) %>% 
  summarize(n_sig = sum(sig))


table(effects_schools$outcome)



summ_effects <- effects_schools %>% 
  #filter(outcome == "math_max") %>% 
  filter(group == "male" | group =="female") %>% 
  group_by(outcome, group) %>% 
  summarize(mean = mean(beta),
            sd  = sd(beta),
  )


effects_schools %>% 
  filter(outcome == "female") %>% 
  filter(group == "male") %>% 
  pull(beta) %>% 
  sd()


effects_schools %>% 
  filter(outcome == "avg_stem_share") %>% 
  filter(group == "all") %>% 
  pull(beta) %>% 
  sd()




effects_schools %>% 
  filter(outcome == "leng_max") %>% 
  filter(group == "female") %>% 
  pull(beta) %>% 
  mean()

effects_schools %>% 
  filter(outcome == "leng_max") %>% 
  filter(group == "male") %>% 
  pull(beta) %>% 
  mean()




effects_schools %>% 
  filter(outcome == "avg_stem_share") %>% 
  filter(group == "female") %>% 
  pull(beta) %>% 
  sd()

effects_schools %>% 
  filter(outcome == "avg_stem_share") %>% 
  filter(group == "male") %>% 
  pull(beta) %>% 
  sd()





####################PLOTS
#Plot: 2 vars, 1 sample 

outcome1<- "math_max"
outcome2<- "avg_stem_share"


plot_2vars <- function(outcome1, outcome2, this_group = "all", data = effects_schools,
                       my_xlab = outcome1, my_ylab = outcome2) {

  effects_wide <- data %>%
    filter(group == this_group, outcome %in% c(outcome1, outcome2)) %>%
    select(outcome, school_id, beta, sig) %>% 
    filter(!(is.na(school_id))) %>% 
    pivot_wider(
      names_from   = outcome,
      values_from  = c(beta, sig),
      names_glue   = "{.value}_{outcome}"
    ) 

  x_sig <- (paste0("sig_",outcome1))
  y_sig <- (paste0("sig_",outcome2))
  
  effects_wide <- effects_wide %>% 
    mutate(
      sig_group = case_when(
        .data[[x_sig]] == 1 & .data[[y_sig]] == 1 ~ "Significant in both axes",
        .data[[x_sig]] == 1 & .data[[y_sig]] == 0 ~ "Significant in x-axis",
        .data[[x_sig]] == 0 & .data[[y_sig]] == 1 ~ "Significant in y-axis",
        TRUE ~ "Insignificant",
      ),
      sig_group = factor(
        sig_group,
        levels = c("Significant in both axes",
                   "Significant in x-axis",
                   "Significant in y-axis",
                   "Insignificant")
      )
    )

x_var <- (paste0("beta_",outcome1))
y_var <- (paste0("beta_",outcome2))




comparison_data_v1 <- final_data
comparison_data_v2 <- final_data

if(outcome1 %in% colnames(all_applications)) {comparison_data_v1 <- all_applications} 
if(outcome2 %in% colnames(all_applications)) {comparison_data_v2 <- all_applications} 


sample_sd_xvar <- sd(comparison_data_v1[[outcome1]], na.rm = TRUE)
sample_sd_yvar <- sd(comparison_data_v2[[outcome2]], na.rm = TRUE)



# collect scaled values from both axes
vals <- c(
  effects_wide[[x_var]] / sample_sd_xvar,
  effects_wide[[y_var]] / sample_sd_yvar
)

# symmetric limit based on max absolute value
m <- min(quantile(abs(vals), 0.99, na.rm = TRUE), 1.5)
lims <- c(-m, m)


#sample_mean_xvar <- mean(comparison_data_v1[[outcome1]], na.rm = TRUE)
#sample_mean_yvar <- mean(comparison_data_v2[[outcome2]], na.rm = TRUE)




effects_wide %>% 
  ggplot(aes(
    x = .data[[x_var]] / sample_sd_xvar,
    y = .data[[y_var]] / sample_sd_yvar,
    color = sig_group
  )) +
  geom_point() +
  geom_smooth(
    
    data = subset(effects_wide, sig_group != "Insignificant"),  # 👈 only significant
    aes(
      x = .data[[x_var]] / sample_sd_xvar,
      y = .data[[y_var]] / sample_sd_yvar,
      color = NULL
    ),  
     # <-- disables color grouping for the line
    method = "lm", 
    se = FALSE
  ) +
  coord_cartesian(xlim = lims, ylim = lims) +
  theme_minimal() +
  theme(legend.position = "bottom")  +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = my_xlab,
    y = my_ylab,
    title = "Scatter plot of school effects",
    subtitle = "Effects are as fractions of the population s.d."
  )
}


plot_2samples <- function(this_outcome, group1 ="male", group2 ="female", data = effects_schools) {
  
  x_var <- (paste0("beta_",group1))
  y_var <- (paste0("beta_",group2))
  
  effects_wide <- data %>% 
    filter(group %in% c(group1, group2)) %>% 
    filter(outcome == this_outcome) %>% 
    select(group, school_id, beta, sig) %>% 
    filter(!(is.na(school_id))) %>% 
    pivot_wider(
      names_from   = group,
      values_from  = c(beta, sig),
      names_glue   = "{.value}_{group}"
    ) %>% 
    filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
  
  g1_sig <- (paste0("sig_",group1))
  g2_sig <- (paste0("sig_",group2))
  
  effects_wide <- effects_wide %>% 
    mutate(
      sig_group = case_when(
        .data[[g1_sig]] == 1 & .data[[g2_sig]] == 1 ~ "Significant in both groups",
        .data[[g1_sig]] == 1 & .data[[g2_sig]] == 0 ~ paste("Significant in", group1, "sample"), 
        .data[[g1_sig]] == 0 & .data[[g2_sig]] == 1 ~ paste("Significant in", group2, "sample"),
        TRUE ~ "Insignificant"
      ),
      sig_group = factor(
        sig_group,
        levels = c("Significant in both groups",
                   paste("Significant in", group1, "sample"),
                   paste("Significant in", group2, "sample"),
                   "Insignificant")
      )
    )
  
  
  comparison_data<- final_data
  if(this_outcome %in% colnames(all_applications)) {comparison_data <- all_applications} 
  
  
  sample_sd <- sd(comparison_data[[this_outcome]], na.rm = TRUE)
  
  
  
  # collect scaled values from both axes
  vals <- c(
    effects_wide[[x_var]] / sample_sd,
    effects_wide[[y_var]] / sample_sd
  )
  
  # symmetric limit based on max absolute value
  m <- min(quantile(abs(vals), 0.99, na.rm = TRUE), 1.5)
  lims <- c(-m, m)
  
  effects_wide %>% 
    ggplot(aes(
      x = .data[[x_var]] / sample_sd,
      y = .data[[y_var]] / sample_sd,
      color = sig_group
    )) +
    geom_point() +
    geom_smooth(aes(color = NULL),   # <-- disables color grouping for the line
                method = "lm", 
                se = FALSE
    ) +
    coord_cartesian(xlim = lims, ylim = lims) +
    theme_minimal() +
    theme(legend.position = "bottom")  +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50")
}



plot_2vars("math_max", "leng_max", my_xlab = "Effect on math exam", my_ylab = "Effect on verbal exam")
ggsave(paste0(code_output_wd, "/output/figures/2dim_math_lang", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)


plot_2vars("math_max", "avg_stem_share", my_xlab = "Effect on math exam", my_ylab = "Effect on app. STEM content")
ggsave(paste0(code_output_wd, "/output/figures/2dim_math_stem", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)


plot_2vars("math_max", "avg_stem_share", data = effects_schools_late, my_xlab = "Effect on math exam", my_ylab = "Effect on app. STEM content")
ggsave(paste0(code_output_wd, "/output/figures/2dim_math_stem_late_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)





plot_2vars("math_max", "took_only_science", my_xlab = "Effect on math exam", my_ylab = "Effect on taking only science exam")
ggsave(paste0(code_output_wd, "/output/figures/2dim_math_sci", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)



plot_2vars("math_max", "leng_max", data = effects_schools_late)


plot_2vars("math_max", "female")
plot_2vars("math_max", "female", data = effects_schools_late)


plot_2vars("math_max", "avg_stem_share")
plot_2vars("math_max", "avg_stem_share", data = effects_schools_late)


plot_2vars("all",  "avg_stem_share", "took_only_science")
plot_2vars("all", "avg_stem_share", "took_only_history")
plot_2vars("all",  "avg_stem_share", "took_both")


plot_2vars("male",  "avg_stem_share", "took_only_science")
plot_2vars("male", "avg_stem_share", "took_only_history")
plot_2vars("male",  "avg_stem_share", "took_both")



plot_2vars("all", "math_max", "prop_engineer2")
plot_2vars("all", "math_max", "prop_health2")
plot_2vars("all", "math_max", "prop_engineer2")

plot_2vars("male", "math_max", "prop_health2")
plot_2vars("female", "math_max", "prop_health2")


plot_2vars("male", "math_max", "avg_stem_share")
plot_2vars("female", "math_max", "avg_stem_share")


plot_2vars("male", "math_max", "prop_engineer2")
plot_2vars("female", "math_max", "prop_engineer2")


plot_2vars("all", "math_max", "prop_engineer2")
plot_2vars("all", "math_max", "prop_science2")
plot_2vars("all", "math_max", "prop_health2")
plot_2vars("all", "math_max", "prop_technology2")


plot_2vars("female", "math_max", "prop_engineer2")
plot_2vars("female", "math_max", "prop_science2")
plot_2vars("female", "math_max", "prop_health2")
plot_2vars("female", "math_max", "prop_technology2")


plot_2vars("female", "prop_engineer2", "prop_science2")


plot_2vars("all", "leng_max", "completed_psu")
plot_2vars("male", "leng_max", "completed_psu")
plot_2vars("female", "leng_max", "completed_psu")

plot_2vars("all", "math_max", "completed_psu")
plot_2vars("male", "math_max", "completed_psu")
plot_2vars("female", "math_max", "completed_psu")


plot_2vars("all", "math_max", "leng_max")
plot_2vars("male", "math_max", "leng_max")
plot_2vars("female", "math_max", "leng_max")



plot_2vars("all", "math_max", "took_only_science")
plot_2vars("male", "math_max", "took_only_science")
plot_2vars("female", "math_max", "took_only_science")

plot_2vars("all", "math_max", "took_only_history")
plot_2vars("male", "math_max", "took_only_history")
plot_2vars("female", "math_max", "took_only_history")

plot_2vars("all", "leng_max", "n_repeat_grado_post")


plot_2vars("all", "math_max", "avg_stem_share")
plot_2vars("male", "math_max", "avg_stem_share")
plot_2vars("female", "math_max", "avg_stem_share")


plot_2vars("all", "took_only_science", "avg_stem_share")
plot_2vars("male", "took_science", "avg_stem_share")
plot_2vars("female", "took_science", "avg_stem_share")


plot_2vars("all", "math_max", "n_years_rbd_target_post")
plot_2vars("male", "math_max", "n_years_rbd_target_post")
plot_2vars("female", "math_max", "n_years_rbd_target_post")


plot_2vars("all", "leng_max", "n_years_rbd_target_post")
plot_2vars("male", "leng_max", "n_years_rbd_target_post")
plot_2vars("female", "leng_max", "n_years_rbd_target_post")

this_outcome = "math_max"
group1 = "male"
group2 = "female"



plot_2samples("math_max", "male", "female")
plot_2samples("math_max", "male", "female", effects_schools_late)


plot_2samples("avg_stem_share", "male", "female")
plot_2samples("avg_stem_share", "male", "female", effects_schools_late)


plot_2samples("avg_stem_share", "male", "female")
plot_2samples("avg_stem_share", "all", "female")
plot_2samples("avg_stem_share", "all", "male")

plot_2samples("took_only_science", "male", "female")
plot_2samples("took_only_history", "male", "female")
plot_2samples("took_both", "male", "female")

plot_2samples("prop_engineer2")
plot_2samples("prop_science2")
plot_2samples("prop_health2")
plot_2samples("prop_technology2")


#plot_2samples("female", "male", "female")
plot_2samples("math_max", "ab_med", "be_med")
plot_2samples("math_max", "be_med", "ab_med")

plot_2samples("leng_max", "ab_med", "be_med")
plot_2samples("leng_max", "male", "female")


plot_2samples("leng_max", "male", "female")