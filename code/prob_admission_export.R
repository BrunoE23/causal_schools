####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

setwd(data_wd)
#####################################

library(tidyverse)



##### Combining all DA_probs
####################################

#Read all 4 db
probs_2018 <- read_csv("./data/clean/DA_probs/DA_probs_2018.csv") %>% 
  select(-...1)

probs_2019 <- read_csv("./data/clean/DA_probs/DA_probs_2019.csv") %>% 
  select(-...1)


probs_2020 <- read_csv("./data/clean/DA_probs/DA_probs_2020.csv") %>% 
  select(-...1)


probs_2021 <- read_csv("./data/clean/DA_probs/DA_probs_2021.csv") %>% 
  select(-...1)


#Make the unmatched also have a year to avoid some confusion
probs_2018 <- probs_2018 %>% 
  mutate(school_id = ifelse(school_id == "unmatched", "unmatched_2018", school_id))

probs_2019 <- probs_2019 %>% 
  mutate(school_id = ifelse(school_id == "unmatched", "unmatched_2018", school_id))


probs_2020 <- probs_2020 %>% 
  mutate(school_id = ifelse(school_id == "unmatched", "unmatched_2020", school_id))

probs_2021 <- probs_2021 %>% 
  mutate(school_id = ifelse(school_id == "unmatched", "unmatched_2021", school_id))



#Combine them
probs_all <- rbind(probs_2018,
                                      probs_2019,
                   probs_2020,
                   probs_2021)  %>% 
  mutate(rbd_prob = sub("_.*", "", school_id)) %>% 
  mutate(prob_r = round(prob, 2)) %>% 
  mutate(year = str_extract(school_id, "\\d{4}$"))  
  
  
  probs_all %>% 
  pull(student_id) %>% 
  unique() %>% 
  length()


  #1% appears more than once, suggests repeating grade
  probs_all |>
    dplyr::summarise(n = dplyr::n(), .by = c(student_id, rbd_prob)) |>
    dplyr::filter(n > 1L) %>% 
    View()
  
  
#Filtering first for the first application in the cycle
probs_unique <- probs_all %>% 
  select(student_id, rbd_prob, prob_r, year) %>%
  group_by(student_id, rbd_prob) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup() 


#Computing individual risk 
probs_unique <- probs_unique %>% 
  group_by(student_id) %>% 
  mutate(any_risk = ifelse(max(prob_r) == 1.0, 0, 1)) %>% 
  ungroup()

#5% has approximately 0s computed
probs_unique %>% 
  filter(prob_r == 0)

prop.table(table(probs_unique$any_risk))



#TODO: Maybe add a school filter to focus in fewer schools; avoid making the df unnecessarily large


#Make it wide 
probs_wide <- probs_unique %>% 
  pivot_wider(
    names_from = rbd_prob,
    values_from = prob_r,
    names_prefix = "prob_"
  )

#Fill with zeros and compute the binaries 
probs_wide <- probs_wide %>%
  mutate(across(-c(student_id, any_risk), ~ tidyr::replace_na(., 0)))

#Get the binaries
probs_wide <- probs_wide %>%
  mutate(
    across(
      starts_with("prob_"),
      ~ as.integer(. == 0),
      .names = "{sub('prob_', 'iszero_', .col)}"
    )
  )

#Save 
write_csv(probs_wide, "./data/clean/DA_probs/probs_columns_wide.csv")
#haven::write_dta(probs_wide, "./data/clean/DA_probs/probs_columns_wide.dta")

