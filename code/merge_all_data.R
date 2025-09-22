
library(tidyverse)
setwd("C:/Users/xd-br/Desktop/PhD/Research/Education_Chile")


load("./data_clean/samples.RData")
load("./data_clean/treatment_1R.RData")
load("./data_clean/stem_outcome.RData")

#Academic controls and school of graduation, only for first year applying
load("./data_clean/psu_students.RData")
rm(students_apps_demre)


final_data <-   sample_students %>%
  left_join(all_treatments, by = c("br_code", "mrun"))  %>%
  left_join(students_apps, by = "mrun") %>%
  #   left_join(socioecon_controls,   by = "mrun")  %>%
  left_join(stem_outcome,   by = "mrun")  %>%
  mutate(graduated_from_applied_school = (RBD == rbd)) %>% 
  mutate(rbd_admitido = factor(rbd_admitido)) %>% 
  mutate()

save(final_data,  file = "./data_clean/final_data.Rdata")

haven::write_dta(final_data, path = "./data_clean/final_data.dta")


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



