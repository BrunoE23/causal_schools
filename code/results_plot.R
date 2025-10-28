####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


setwd(data_wd)
#####################################

library(tidyverse)


effects_schools <- read_csv("./data/clean/effects_schools_long.csv")

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
  filter(outcome == "math_max") %>% 
  filter(group == "female") %>% 
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


plot_2vars <- function(this_group = "all", outcome1, outcome2) {

effects_wide <- effects_schools %>% 
  filter(group == this_group) %>% 
  filter(outcome %in% c(outcome1, outcome2)) %>% 
  pivot_wider(
        names_from = outcome,
        values_from = beta:sig
  ) 

y_var <- (paste0("beta_",outcome2))
x_var <- (paste0("beta_",outcome1))

effects_wide %>% 
  ggplot(aes_string(x=x_var, y = y_var)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

}


plot_2vars("all", "leng_max", "completed_psu")
plot_2vars("male", "leng_max", "completed_psu")
plot_2vars("female", "leng_max", "completed_psu")

plot_2vars("all", "math_max", "completed_psu")
plot_2vars("male", "math_max", "completed_psu")
plot_2vars("female", "math_max", "completed_psu")


plot_2vars("all", "math_max", "leng_max")
plot_2vars("male", "math_max", "leng_max")
plot_2vars("female", "math_max", "leng_max")



plot_2vars("all", "math_max", "took_science")
plot_2vars("male", "math_max", "took_science")
plot_2vars("female", "math_max", "took_science")

plot_2vars("all", "math_max", "avg_stem_share")
plot_2vars("male", "math_max", "avg_stem_share")
plot_2vars("female", "math_max", "avg_stem_share")


plot_2vars("all", "took_science", "avg_stem_share")
plot_2vars("male", "took_science", "avg_stem_share")
plot_2vars("female", "took_science", "avg_stem_share")


plot_2vars("all", "math_max", "n_years_rbd_target_post")
plot_2vars("male", "math_max", "n_years_rbd_target_post")
plot_2vars("female", "math_max", "n_years_rbd_target_post")


plot_2vars("all", "leng_max", "n_years_rbd_target_post")
plot_2vars("male", "leng_max", "n_years_rbd_target_post")
plot_2vars("female", "leng_max", "n_years_rbd_target_post")

plot_2samples <- function(this_outcome, group1, group2) {
  
  effects_wide <- effects_schools %>% 
    filter(group %in% c(group1, group2)) %>% 
    filter(outcome == this_outcome) %>% 
    pivot_wider(
      names_from = group,
      values_from = beta:sig
    ) 
  
  x_var <- (paste0("beta_",group1))
  y_var <- (paste0("beta_",group2))
  
  effects_wide %>% 
    ggplot(aes_string(x=x_var, y = y_var)) + geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = FALSE) 
  
}


plot_2samples("avg_stem_share", "male", "female")


#plot_2samples("female", "male", "female")
plot_2samples("math_max", "male", "female")
plot_2samples("math_max", "ab_med", "be_med")
plot_2samples("math_max", "be_med", "ab_med")

plot_2samples("leng_max", "ab_med", "be_med")
plot_2samples("leng_max", "male", "female")


plot_2samples("leng_max", "male", "female")