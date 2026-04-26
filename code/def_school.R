####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)

universe <- read_csv("data/clean/universe.csv") %>% 
  select(-...1)  %>% 
  mutate(MRUN = as.character(MRUN))


load("./data/clean/tracking_univ8gr.RData")

#TODO:Note change
#if sae_proceso == 2017, this means kid applies in 2017 to enter 1M in 2018.
#I want 2017 to show up as 0, 2018 to show up as 1, and 2019 as 2 

tracking_window <- tracking_all %>%  
  left_join(universe, by = "MRUN") %>% 
  mutate(year_rel_cohort8 = AGNO - cohort_gr8,
         year_rel_coh_lab = paste0("t_", year_rel_cohort8), 
         year_rel_coh_lab = str_replace_all(year_rel_coh_lab, "t_-(\\d+)", "t_min\\1")
)  %>% 
  filter(year_rel_cohort8 > 0 & year_rel_cohort8 <= 4) %>% 
  select(MRUN, RBD, year_rel_cohort8, year_rel_coh_lab,
         ASISTENCIA, SIT_FIN)

rm(tracking_all)

#############Getting rid of students showing up in multiple schools the same year

check <- tracking_window %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  mutate(n_school_year_entries = n())  %>% 
  filter(n_school_year_entries > 1)
#179k repeated cases.

  
#Drop asistencia ==0 if multiple school years for that student
tracking_window_clean <- tracking_window %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  mutate(n_school_year_entries = n()) %>% 
  ungroup() %>% 
  filter(!(n_school_year_entries > 1 & ASISTENCIA == 0)) %>% 
  select(-n_school_year_entries)


check2 <- tracking_window_clean %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  mutate(n_school_year_entries = n())  %>% 
  filter(n_school_year_entries > 1)
#139 rep. cases remain

#Drop failed class year if multiple school years for a student that passed at least one class year.
tracking_window_clean <-  tracking_window_clean %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  mutate(n_school_year_entries = n(),
         n_school_year_passed  = sum(SIT_FIN == "P")) %>% 
  ungroup()  %>% 
  filter(!(n_school_year_entries > 1 & n_school_year_passed > 0 & SIT_FIN != "P")) %>% 
  select(-n_school_year_entries)

check2 <- tracking_window_clean %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  mutate(n_school_year_entries = n())  %>% 
  filter(n_school_year_entries > 1)
  #56 rep. cases remain

#Reasonable last resort: Pick highest attendance.    
tracking_window_clean <-  tracking_window_clean %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  mutate(n_school_year_entries = n()) %>% 
  filter(ASISTENCIA == max(ASISTENCIA)) %>% 
  ungroup() %>% 
  select(-(c(n_school_year_entries,n_school_year_passed )))
#%>% 
#  group_by(MRUN, AGNO, sae_proceso) %>% 
#  mutate(n_school_year_entries = n()) 

#12 cases remain
final_ties <- tracking_window_clean %>% 
  group_by(MRUN, year_rel_cohort8) %>% 
  filter(n() > 1) %>%
  ungroup()
  

#Try to break ties by looking at other years
resolved <- final_ties %>%
  group_by(MRUN, year_rel_cohort8) %>%
  mutate(
    chosen_RBD = {
      current_mrun <- first(MRUN)
      current_year <- first(year_rel_cohort8)
      current_rbds <- RBD
      
      other_years <- tracking_window_clean %>%
        filter(MRUN == current_mrun,
               year_rel_cohort8 != current_year)
      
      counts <- other_years %>%
        filter(RBD %in% current_rbds) %>%
        count(RBD, name = "n")
      
      if (nrow(counts) == 0) {
        NA_real_
      } else {
        counts %>%
          arrange(desc(n)) %>%
          slice(1) %>%
          pull(RBD)
      }
    }
  ) %>%
  ungroup()


resolved <- resolved %>%
  group_by(MRUN, year_rel_cohort8) %>%
  summarise(chosen_RBD = first(chosen_RBD), .groups = "drop")

tracking_window_final <- tracking_window_clean %>%
  left_join(resolved, by = c("MRUN", "year_rel_cohort8")) %>%
  filter(is.na(chosen_RBD) | RBD == chosen_RBD) %>%
  select(-chosen_RBD)

#check_final <- tracking_window_final %>% 
#  group_by(MRUN, year_rel_cohort8) %>% 
#  mutate(n_school_year_entries = n())  %>% 
#  filter(n_school_year_entries > 1)


first_last <- tracking_window_final %>%
  group_by(MRUN) %>%
  summarise(
    RBD_rel1 = first(RBD[year_rel_cohort8 == 1], default = NA),
    RBD_rel4 = first(RBD[year_rel_cohort8 == 4], default = NA),
    .groups = "drop"
  )

#Takes a bit to run; Chat has some suggestions
most_time <- tracking_window_final %>%
  group_by(MRUN, RBD) %>%
  summarise(
    n_years = n(),                       # number of years in that RBD
    last_year = max(year_rel_cohort8),   # latest year in that RBD
    .groups = "drop"
  ) %>%
  group_by(MRUN) %>%
  # first pick RBDs with max years
  slice_max(n_years, n = 1, with_ties = TRUE) %>%
  # then break ties using latest year
  slice_max(last_year, n = 1) %>%
  summarise(most_time_RBD = first(RBD), .groups = "drop")


rbd_attended <- left_join(first_last, most_time, by = "MRUN") %>% 
  mutate(first_last_same = (RBD_rel1 == RBD_rel4))

prop.table(table(rbd_attended$first_last_same, useNA = "ifany"))

write.csv(rbd_attended, "./data/clean/rbd_universe.csv")