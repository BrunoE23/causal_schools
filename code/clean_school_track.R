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



students_small <- sample_students %>% 
  select(mrun, rbd, cod_nivel, proceso) %>% 
  rename(rbd_target = rbd,
         cod_nivel_target = cod_nivel,
         sae_proceso = proceso,
         MRUN = mrun) %>% 
  mutate(MRUN = as.character(MRUN)) %>%
  # mutate(year_track = proceso - 1) %>% 
  unique()

#Goals: 
#1) Control for pre-SAE achievement 
#2) Have a sense of how many schools are associated with a certain target school
#3) Have a sense of which type of schools might put up spots in SAE? 

#Suppose a student that applies in year 7 and year 9 


tracking_window <- tracking_all %>%  
  left_join(students_small, by = "MRUN" ,  multiple = "all", relationship = "many-to-many") %>% 
   mutate(year_rel_sae_change = AGNO - sae_proceso,
          year_rel_sae_lab = paste0("t", year_rel_sae_change))



rm(tracking_all)  
gc()

#a student tracked in a row, might have multiple processes in SAE (either same year or across years)
#a student who applied in SAE should have multiple years tracked
  

tracking_window <- tracking_window %>% 
  filter(year_rel_sae_change >= -4 & year_rel_sae_change <= 4) %>% 
  select(RBD, COD_GRADO, AGNO, MRUN, sae_proceso,
         COD_COM_ALU, NOM_COM_ALU, COD_REG_ALU,
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
    select(-(c(n_school_year_entries,n_school_year_passed )))
#%>% 
#  group_by(MRUN, AGNO, sae_proceso) %>% 
#  mutate(n_school_year_entries = n()) 


flag_var_change <- function(x) {
  change <- x != dplyr::lag(x)
  change[is.na(change)] <- FALSE  # first obs has no previous
  as.integer(change)              # 1 if changed, 0 otherwise
}


flag_var_repeat <- function(x) {
  repeated <- x == dplyr::lag(x)
  repeated[is.na(repeated)] <- FALSE  # first obs has no previous
  as.integer(repeated)              # 1 if same, 0 otherwise
}



#Now that data is clean, we can compute some across periods variables
tracking_window_clean<- tracking_window_clean %>% 
   mutate(z_GPA = (PROM_GRAL - school_grade_avg_GPA)/school_grade_sd_GPA ,
          z_ATT = (ASISTENCIA - school_grade_avg_ATT)/school_grade_sd_ATT ) %>% 
   arrange(MRUN, sae_proceso, year_rel_sae_change) %>% 
  group_by(MRUN, sae_proceso) %>% 
        mutate(region_change = flag_var_change(COD_REG_ALU),
               comuna_change = flag_var_change(COD_COM_ALU),
               school_change = flag_var_change(RBD),
               repeat_grado  = flag_var_repeat(COD_GRADO)) %>% 
  ungroup()
       
  

#tracking_window_clean %>% 
#  filter(n_school_year_entries > 1) %>% 
#  View()
  
names(tracking_window_clean)


value_cols <- setdiff(
  names(tracking_window_clean),
  c("MRUN", "sae_proceso", "AGNO", "year_rel_sae_change", "year_rel_sae_lab")
)

tracking_window_wide <- tracking_window_clean %>%
pivot_wider(
  id_cols =  c(MRUN, sae_proceso),
  names_from = year_rel_sae_lab,
  values_from = all_of(value_cols),
  names_glue = "{.value}_{year_rel_sae_lab}"
)


tracking_window_wide <- tracking_window_wide %>%
  mutate(
    n_school_changes = rowSums(across(starts_with("school_change_")), na.rm = TRUE),
    n_comuna_changes = rowSums(across(starts_with("comuna_change_")), na.rm = TRUE),
    n_region_changes = rowSums(across(starts_with("region_change_")), na.rm = TRUE),
    n_repeat_grado   = rowSums(across(starts_with("repeat_grado_")), na.rm = TRUE),
    
    ever_changed_school = as.integer(n_school_changes > 0),
    ever_changed_comuna = as.integer(n_comuna_changes > 0),
    ever_changed_region = as.integer(n_region_changes > 0),
    ever_repeat_grado   = as.integer(n_repeat_grado   > 0)
  )  

round(prop.table(table(tracking_window_wide$n_school_changes)),3)
round(prop.table(table(tracking_window_wide$n_comuna_changes)),3)
round(prop.table(table(tracking_window_wide$n_region_changes)),3)

save( tracking_window_wide, file = "./data/clean/tracking_clean_wide.RData")

