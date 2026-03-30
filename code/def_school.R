####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

universe <- read_csv("data/clean/universe.csv") %>% 
  select(-...1)  %>% 
  mutate(MRUN = as.character(MRUN))


load("./data/clean/tracking_univ8gr.RData")

#TODO:Note change
#if sae_proceso == 2017, this means kid applies in 2017 to enter in 2018.
#I want 2017 to show up as 0, 2018 to show up as 1, and 2019 as 2 

tracking_window <- tracking_all %>%  
  left_join(universe, by = "MRUN") %>% 
  mutate(year_rel_cohort8 = AGNO - cohort_gr8,
         year_rel_coh_lab = paste0("t_", year_rel_cohort8), 
         year_rel_coh_lab = str_replace_all(year_rel_coh_lab, "t_-(\\d+)", "t_min\\1")
)


