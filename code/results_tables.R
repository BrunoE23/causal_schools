####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


setwd(data_wd)
#####################################

library(tidyverse)
library(gt)

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


all_applications <- all_applications %>% 
        select(-graduated_hs, completed_psu)

summary(final_data$math_max)
summary(final_data$leng_max)

prop.table(table(final_data$completed_psu))


comp_data <- NULL

for (outcome in unique(effects_schools$outcome)) {
  
  comparison_data <- final_data
  if(outcome %in% colnames(all_applications)) {comparison_data <- all_applications} 
  
  
  comp_sd_var <- sd(comparison_data[[outcome]], na.rm = TRUE)
  comp_mean_var <- mean(comparison_data[[outcome]], na.rm = TRUE)
  
  comp_data <- dplyr::bind_rows(
    comp_data,
    tibble::tibble(outcome = outcome, comp_mean_var = comp_mean_var, comp_sd_var = comp_sd_var)
  )  
}


order_table_rows<- c("n_years_rbd_target_post", "n_repeat_grado_post", "n_school_changes_post", 
                     #"completed_psu", 
                     "leng_max", 'math_max', 
                     'took_only_history', "took_only_science", "took_both", 
                     "avg_stem_share",
                     #"prop_health1", "prop_technology1", "prop_science1", 
                     'prop_health2', "prop_technology2", "prop_science2", "prop_engineer2")


summ_table <- effects_schools %>% 
  filter(group == "all") %>% 
  filter(outcome %in% order_table_rows) %>%
  mutate(outcome = factor( outcome, levels = order_table_rows)) %>% 
  group_by(outcome) %>% 
  summarize(
          n_schools_sig = sum(sig == 1),
          avg_beta = mean(beta[sig == 1]),
            sd_beta = sd(beta[sig ==1])) %>% 
  left_join(comp_data, by = "outcome") %>% 
  mutate(beta_sd_in_pop_sd = sd_beta / comp_sd_var)  %>% 
  mutate(outcome = recode(outcome,
                          n_years_rbd_target_post = "N. years in target school",
                          n_repeat_grado_post     = "N. repeated grades, post-treatment",
                          n_school_changes_post   = "N. school changes, post-treatment",
                          
                          completed_psu           = "Finished core higher ed. exam",
                          math_max                = "Score in math exam",
                          leng_max                = "Score in verbal exam",
                          
                          
                          took_only_history     = "Took only history exam",
                          took_only_science     = "Took only science exam",
                          took_both             = "Took both science and history exams",
                          
                          avg_stem_share        = "STEM content in application",
                          
                          
                          prop_health2           = "Prop. application in health",
                          prop_technology2       = "Prop. application in technology",
                          prop_science2          = "Prop. application in sciences, math",
                          prop_engineer2         = "Prop. application in engineering",
                          
  ))
 

#library(knitr)
#library(kableExtra)

latex_code <- kable(
  summ_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  caption = "Summary of School-Level Effects by Outcome",
  col.names = c("Outcome", "\\# ($\\beta$) Sig.", "Mean($\\beta$)", "SD($\\beta$)",
                "Mean Var.", "SD Var.", "SD($\\beta$)/SD(Pop)")
) |>
  add_header_above(c(" " = 1, "(1)"=1, "(2)"=1, "(3)"=1, "(4)"=1, "(5)"=1, "(6)"=1),
                   escape = FALSE) |>
  add_header_above(c(" " = 1,
                     "Sig School Effects"=3,
                     "Pop. Stats"=2,
                     "(6): (3)/(5)"=1),
                   escape = FALSE) |>
  column_spec(1, width = "3.5cm") |>
  kable_styling(latex_options = c("hold_position","booktabs","striped"))


writeLines(latex_code, paste0(code_output_wd, "/output/tables/effects_summary_", sample_type, ".tex"))





