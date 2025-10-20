####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)

load("./data/clean/samples.RData")
load("./data/clean/treatment_1R.RData")
load("./data/clean/stem_outcome.RData")

#Academic controls and school of graduation, only for first year applying
load("./data/clean/psu_students.RData")
load("./data/clean/sae_2017_19_stud_controls.RData")
load("./data/clean/tracking_clean_wide.RData")

rm(students_apps_demre)


final_data <-   sample_students %>%
  rename(rbd_target = rbd) %>% 
  left_join(sae_stud_info, by = c("mrun", "proceso")) %>%
  left_join(tracking_window_wide, by = c("mrun", "proceso", "rbd_target")) %>%
    left_join(all_treatments, by = c("br_code", "mrun"))  %>%
  left_join(students_apps, by = "mrun") %>%
  rename(grad_rbd_psu = RBD) %>% 
  mutate(female = as.factor(ifelse(es_mujer == 0, 0, 1))) %>% 
  mutate(male   = as.factor(ifelse(es_mujer == 0, 1, 0))) %>% 
  mutate(gender = as.factor(ifelse(es_mujer == 0, "Male", "Female"))) %>% 
  mutate(took_science = as.factor(ifelse(scien_max == 0, "No", "Yes")),
         gender_science = factor(paste(gender, took_science, sep = "_"))
         ) %>%
  mutate(took_history = as.factor(ifelse(hist_max == 0, "No", "Yes"))) %>% 
  mutate(took_both = as.factor(ifelse(hist_max > 0 & scien_max > 0  , "Yes", "No"))) %>% 
    mutate(leng_math_total = math_max + leng_max) %>% 
  mutate(graduated_hs   = as.factor(ifelse(!(is.na(PROM_NOTAS)), 1, 0))) %>% 
  mutate(registered_psu = as.factor(ifelse(!(is.na(FECHA_NACIMIENTO)), 1, 0))) %>% 
  mutate(completed_psu = as.factor(ifelse(leng_math_total> 0,  1, 0))) %>% 
  #   left_join(socioecon_controls,   by = "mrun")  %>%
  left_join(stem_outcome,   by = "mrun")  %>%
  mutate(graduated_from_applied_school = (grad_rbd_psu == rbd_target)) %>% 
  mutate(rbd_admitido = factor(rbd_admitido)) 



#final_data %>% 
#  filter(completed_psu == 1) %>% 
#  filter(registered_psu == 0) %>% 
#  filter(is.na(RBD)) %>% 
#  View()

save(final_data,  file = "./data/clean/final_data.Rdata")

haven::write_dta(final_data, path = "./data/clean/final_data.dta")


stop_code()

# Check how many students I have gender for 
table(final_data$gender)
table(final_data$COD_SEXO)


#Check for people who never register for psu 

table(is.na(final_data$avg_stem_share), is.na(final_data$PROM_NOTAS))


### preliminary regs
library(broom)
library(fixest)


table(final_data$graduated_from_applied_school, final_data$offered_spot_1R)


#1st stage
toy_1st_stage <- lm(data = final_data, graduated_from_applied_school ~ offered_spot_1R  + br_code)
toy_1st_df <- tidy(toy_1st_stage)



test_1st<- feols(fml = graduated_from_applied_school ~  0 + offered_spot_1R +  PROM_NOTAS+ PTJE_RANKING+ math_max + year_1st_app |  br_code, data = final_data)
test_1st_df <- tidy(test_1st)

sum(test_1st_df$statistic > 2)


#exam_outcomes
toy_math <- lm(data = final_data, math_max  ~ offered_spot_1R + br_code)
toy_math_df <- tidy(toy_math)

toy_leng <- lm(data = final_data, leng_max  ~ offered_spot_1R + br_code)
toy_leng_df <- tidy(toy_leng)



final_data %>% 
#  filter(rbd_admitido == 0) %>% 
  pull(avg_stem_share) %>% 
  mean(na.rm = TRUE)

toy_main_noT <- lm(data = final_data, avg_stem_share  ~ PROM_NOTAS+ PTJE_RANKING+ math_max + i(year_1st_app))
test_main_noT_df <- tidy(toy_main_noT)


#main_outcome
test_main<- feols(fml = avg_stem_share ~  rbd_admitido +  PROM_NOTAS+ PTJE_RANKING+ math_max + year_1st_app |  br_code, data = final_data)
test_main_df <- tidy(test_main)

test_main_df %>% 
  arrange(-abs(statistic)) %>% 
  View()


toy_main <- lm(data = final_data, avg_stem_share ~  rbd_admitido +  PROM_NOTAS+ PTJE_RANKING+ math_max +  year_1st_app + br_code)
toy_main_df <- tidy(toy_main)


coef_test(toy_main, vcov = "CR2", cluster = final_data$br_code)





library(clubSandwich)



length(unique(final_data$rbd_admitido))

toy_main_df %>%
  filter(grepl("^rbd", term)) %>%
  arrange(-abs(statistic)) %>% 
  View()

toy_main_df %>%  
  arrange(-abs(statistic)) %>% 
filter(abs(statistic)>=2) %>% 
  View()


toy_main_df %>%
  filter(grepl("^rbd", term)) %>%
  pull(statistic) %>%
     hist()

#add controls and hope for the best? 

library(car)

alias(toy_main)

# Get all terms that start with rbd
rbd_terms <- grep("^rbd", names(coef(toy_main)), value = TRUE)

"rbd_admitido1" %in% rbd_terms

# Build hypotheses automatically
hypotheses <- paste(rbd_terms, "= 0")

# Run joint test
linearHypothesis(toy_main, hypotheses)


length(unique(final_data$rbd_admitido))

toy_main2 <- lm(data = final_data, n_stem_low ~ offered_spot_1R + br_code)
toy_main2_df <- tidy(toy_main2)



