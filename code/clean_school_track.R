####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)
library(tidyr)

#read samples to get student universe
load("./data/clean/samples.RData")
load("./data/clean/tracking_all.RData")


#Goals: 
#1) Control for pre-SAE achievement 
#2) Have a sense of how many schools are associated with a certain target school
#3) Have a sense of which type of schools might put up spots in SAE? 

#Suppose a student that applies in year 7 and year 9 


tracking_test <- tracking_all %>%  
  left_join(students_small, by = "MRUN" ,  multiple = "all", relationship = "many-to-many") %>% 
   mutate(year_rel_sae_change = AGNO - sae_proceso,
          year_rel_sae_lab = paste0("t", year_rel_sae_change))


#a student tracked in a row, might have multiple processes in SAE (either same year or across years)
#a student who applied in SAE should have multiple years tracked
  

tracking_window <- tracking_test %>% 
  filter(year_rel_sae_change >= -4 & year_rel_sae_change <= 4) %>% 
  select(RBD, COD_GRADO, AGNO, MRUN, sae_proceso,
         COD_COM_ALU, NOM_COM_ALU,
         PROM_GRAL, ASISTENCIA, 
         SIT_FIN, SIT_FIN_R,
         school_grade_avg_GPA, school_grade_avg_ATT,
         school_grade_sd_GPA,  school_grade_sd_ATT,
         pctl_school_grade,
         sae_proceso,
         year_rel_sae_change, year_rel_sae_lab) %>% 
  unique() %>% 
  arrange(MRUN, AGNO)
        

##Cleaning data

tracking_window_clean <- tracking_window %>% 
  group_by(MRUN, AGNO, sae_proceso) %>% 
  mutate(n_school_year_entries = n()) %>% 
  ungroup() %>% 
  #145k repeated cases.
  #Drop asistencia ==0 if multiple school years for that student
  filter(!(n_school_year_entries > 1 & ASISTENCIA == 0)) %>% 
  select(-n_school_year_entries) %>% 
  group_by(MRUN, AGNO, sae_proceso) %>% 
  mutate(n_school_year_entries = n(),
         n_school_year_passed  = sum(SIT_FIN == "P")) %>% 
  ungroup() %>% 
#58 cases remain
#Drop failed class year if multiple school years for a student that passed at least one class year.
  filter(!(n_school_year_entries > 1 & n_school_year_passed > 0 & SIT_FIN != "P")) %>% 
  select(-n_school_year_entries) %>% 
    group_by(MRUN, AGNO, sae_proceso) %>% 
    mutate(n_school_year_entries = n()) %>% 
#10 repeated cases remain: Pick highest attendance of the 20    
    filter(ASISTENCIA == max(ASISTENCIA)) %>% 
      ungroup() %>% 
    select(-n_school_year_entries) 
     #%>% 
    #  group_by(MRUN, AGNO, sae_proceso) %>% 
    #  mutate(n_school_year_entries = n()) 
    

#tracking_window_clean %>% 
#  filter(n_school_year_entries > 1) %>% 
#  View()
  

value_cols <- setdiff(
  names(tracking_window_clean),
  c("MRUN", "AGNO", "rel", "suffix")
)

# df
tracking_window_clean %>%
pivot_wider(
  id_cols = MRUN,
  names_from = suffix,
  values_from = all_of(value_cols),
  names_glue = "{.value}_{suffix}"
)
}



  