####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

dollar_clp_conversion <- 913

library(tidyverse)

load("./data/clean/samples.RData")
load("./data/clean/treatment_1R.RData")
load("./data/clean/stem_outcome.RData")

#Academic controls and school of graduation, only for first year applying
load("./data/clean/psu_students.RData")

load("./data/clean/sae_2017_19_stud_controls.RData")
load("./data/clean/tracking_clean_wide.RData")
load("./data/clean/simce_4to.Rdata")

length(unique(sample_students$rbd))
  
colnames(tracking_window_wide)

tracking_window_small <- tracking_window_wide %>% 
        select(proceso, mrun, rbd_target, 
               n_years_rbd_target_post, n_school_changes_post, n_repeat_grado_post,
               z_GPA_t_min4, z_GPA_t_min3, z_GPA_t_min2, z_GPA_t_min1, 
               z_ATT_t_min4, z_ATT_t_min3, z_ATT_t_min2, z_ATT_t_min1
        )
               


final_data <-   sample_students %>%
  rename(rbd_target = rbd) %>% 
  left_join(sae_stud_info, by = c("mrun", "proceso")) %>%
  left_join(tracking_window_small, by = c("mrun", "proceso", "rbd_target")) %>%
    left_join(all_treatments, by = c("br_code", "mrun"))  %>%
  left_join(students_apps, by = "mrun") %>%
  rename(grad_rbd_psu = RBD) %>% 
  mutate(female = as.integer(ifelse(es_mujer == 0, 0, 1))) %>% 
  mutate(male   = as.integer(ifelse(es_mujer == 0, 1, 0))) %>% 
  mutate(gender = as.factor(ifelse(es_mujer == 0, "Male", "Female"))) %>% 
  mutate(took_only_science = as.integer(ifelse(hist_max == 0 & scien_max > 0  , 1, 0))) %>% 
  mutate(took_only_history = as.integer(ifelse(hist_max > 0 & scien_max == 0  , 1, 0))) %>% 
  mutate(took_both = as.integer(ifelse(hist_max > 0 & scien_max > 0  , 1, 0))) %>% 
    mutate(leng_math_total = math_max + leng_max) %>% 
  mutate(graduated_hs   = as.integer(ifelse(!(is.na(PROM_NOTAS)), 1, 0))) %>% 
  mutate(registered_psu = as.integer(ifelse(!(is.na(FECHA_NACIMIENTO)), 1, 0))) %>% 
  mutate(completed_psu = as.integer(ifelse(leng_math_total> 0,  1, 0))) %>% 
  #   left_join(socioecon_controls,   by = "mrun")  %>%
  left_join(stem_outcome,   by = "mrun")  %>%
  left_join(simce_4to,   by = "mrun")  %>%
  mutate(avg_income_y4_usd = avg_income_y4 / dollar_clp_conversion) %>%  
  mutate(graduated_from_applied_school = (grad_rbd_psu == rbd_target)) 

table(final_data$offered_spot_anyR)
table(final_data$rbd_treated)

length(unique(final_data$rbd_treated))

#final_data %>% 
#  filter(completed_psu == 1) %>% 
#  filter(registered_psu == 0) %>% 
#  filter(is.na(RBD)) %>% 
#  View()

save(final_data,  file = "./data/clean/final_data.Rdata")

haven::write_dta(final_data, path = "./data/clean/final_data.dta")



all_applications <- students_apps  %>% 
  rename(grad_rbd_psu = RBD) %>% 
  mutate(female = as.integer(ifelse(COD_SEXO == 2, 1, 0))) %>% 
  mutate(male   = as.integer(ifelse(COD_SEXO == 1, 1, 0))) %>% 
  mutate(gender = as.factor(ifelse(COD_SEXO == 1, "Male", "Female"))) %>% 
  mutate(took_only_science = as.integer(ifelse(hist_max == 0 & scien_max > 0  , 1, 0))) %>% 
  mutate(took_only_history = as.integer(ifelse(hist_max > 0 & scien_max == 0  , 1, 0))) %>% 
  mutate(took_both = as.integer(ifelse(hist_max > 0 & scien_max > 0  , 1, 0))) %>% 
  mutate(leng_math_total = math_max + leng_max) %>% 
  mutate(graduated_hs   = as.integer(ifelse(!(is.na(PROM_NOTAS)), 1, 0))) %>% 
  mutate(registered_psu = as.integer(ifelse(!(is.na(FECHA_NACIMIENTO)), 1, 0))) %>% 
  mutate(completed_psu = as.integer(ifelse(leng_math_total > 0,  1, 0))) %>% 
  #   left_join(socioecon_controls,   by = "mrun")  %>%
  left_join(stem_outcome,   by = "mrun") %>% 
  left_join(simce_4to, by = "mrun")




save(all_applications,  file = "./data/clean/all_apps.Rdata")
haven::write_dta(all_applications, path = "./data/clean/all_apps.dta")



