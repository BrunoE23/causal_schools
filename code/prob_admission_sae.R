####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


library(tidyverse)



###############
table(sae_apps_2018_DA$prioridad_hermano)

sae_apps_2018_DA %>% 
  filter(rbd == 125) %>% 
  #  filter(!(is.na(orden_pie))) %>% 
#  arrange(rbd) %>% 
  View()


#All es_pie == 1 are a subset of prioridad_matriculado == 1 
sae_apps_2018_DA %>% 
  filter(es_pie == 1) %>% 
  filter(prioridad_matriculado == 0) %>% 
  arrange(rbd) %>% 
#  arrange(mrun, preferencia_postulante) %>% 
  View()

#There are about a thousand ppl w orden_pie
sae_apps_2018_DA %>% 
  filter(!(is.na(orden_pie))) %>% 
  filter(prioridad_matriculado == 0) %>% 
  arrange(rbd) %>% 
  #  arrange(mrun, preferencia_postulante) %>% 
  View()


table(oferta_2018$tiene_orden_pie)

oferta_2018 %>% 
  filter(rbd == 4) %>% 
  View()


table(sae_apps_2018_DA$orden_pie)


sae_apps_2017_DA <- read_csv2("./data/raw/2017/SAE_2017/C1_Postulaciones_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  rename(student_id = mrun, 
         school_id = br_code,
         student_pref = preferencia_postulante ) %>% 
  mutate(priority_level = case_when(
#          agregada_por_continuidad == 1 ~ 5L,
          prioridad_matriculado == 1 ~ 4L,
          prioridad_hermano == 1 ~ 3L,
          prioridad_hijo_funcionario == 1 ~ 2L,
          prioridad_exalumno == 1 ~ 1L,
          TRUE ~ 0L)) %>% 
  select(student_id, school_id, student_pref, priority_level)



oferta_2017 <- read_csv2("./data/raw/2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)


oferta_2017_DA <- read_csv2("./data/raw/2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  select(br_code, vacantes_regular) %>% 
  rename(  school_id = br_code,
           spots_available = vacantes_regular)

oferta_2018 <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) 


oferta_2018_DA <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  select(br_code, vacantes_regular) %>% 
  rename(  school_id = br_code,
           spots_available = vacantes_regular)


sae_apps_2018_DA <- read_csv2("./data/raw/2018/SAE_2018/C1_Postulaciones_etapa_regular_2018_Admisión_2019_PUBL.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  rename(student_id = mrun, 
         school_id = br_code,
         student_pref = preferencia_postulante ) %>% 
  mutate(priority_level = case_when(
#    agregada_por_continuidad == 1 ~ 5L,
    prioridad_matriculado == 1 ~ 4L,
    prioridad_hermano == 1 ~ 3L,
    prioridad_hijo_funcionario == 1 ~ 2L,
    prioridad_exalumno == 1 ~ 1L,
    TRUE ~ 0L)) %>% 
  select(student_id, school_id, student_pref, priority_level)


sae_apps_2020 <- read_csv2("./data/raw/2020/SAE_2020/C1_Postulaciones_etapa_regular_2020_Admisión_2021_PUBL.csv") %>% 
  mutate(proceso = 2020) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)

sae_apps_2020 %>% 
  filter(!(is.na(orden_pie))) %>% 
  View()
#################################


#### Chile DA 
Run_school_cl_DA <-    function(school_db, apps_db, 
                             seed = NULL, print = FALSE, time = FALSE) {
  
  
  
  
  if(time == TRUE) {
    start_time <- Sys.time()
  }
  
  #Create new Databases:
  
  #A copy of school_db where I can update spots remaining
  #  school_cur_spots <- school_db %>% 
  #    mutate(cur_spots_available = spots_available)
  
  if (!is.null(seed)) set.seed(seed)
  
  
  #A DB to keep track of rejections and w lottery tickets
  current_apps <- apps_db %>% 
    mutate(rejected = 0L) %>% 
    group_by(school_id) %>% 
    mutate(lottery_ticket = sample(1:n())) %>% 
    ungroup()
  
  #A final student list 
  school_offers <- apps_db %>% 
    select(student_id) %>% 
    unique() 
  
  n_rejected = -999
  
  #change for a while  
  while (n_rejected != 0) {  
    
    n_rejected = 0
    
    #Matched students
    #  matched_students <- school_offers %>% 
    #    filter(school_offer != 0L) %>% 
    #    pull(student_id)
    
    
    #Compute the current proposal depending on the rejections.
    
    #Get top non rejected preference
    #Re-compute from rejected
    current_apps <- current_apps %>% 
      group_by(student_id) %>% 
      mutate(
        current_proposal = if (any(rejected == 0)) {
          as.integer(
            rejected == 0 & 
              student_pref == min(student_pref[rejected == 0])
          )
        } else {
          0L
        }
      ) %>% 
      ungroup()
    
    cur_app_proposal <- current_apps %>% 
      filter(current_proposal == 1)
    
    
    #next round
    #  if (nrow(cur_unmatched_students) == 0) {next}
    
    #List of schools where there are students applying 
    schools_target <-   cur_app_proposal %>% 
      pull(school_id) %>% 
      unique()
    
    
    schools_iterate <- school_db %>% 
      filter(school_id %in% schools_target)
    
    #DB of schools to assign spots
    #Just to make iteration quicker
    #  schools_to_assign_spots <- school_cur_spots %>% 
    #  filter(cur_spots_available > 0) %>% 
    #  filter(school_id  %in% schools_target)
    
    
    #next round
    #  if (nrow(schools_to_assign_spots) == 0) {next}
    
    
    
    #For each spot, assign spots for highest tier students.
    for (i in 1:nrow(schools_iterate)) {
      
      spot_id          <- schools_iterate$school_id[i]
      N_spots_available  <- schools_iterate$spots_available[i]
      
      
      #Consider students applying to school and order
      
      #For priority, higher is better;
      #For Lottery, higher is bad. 
      ordered_cur_school <- cur_app_proposal %>% 
        filter(school_id == spot_id) %>% 
        arrange(desc(priority_level), lottery_ticket) 
      
      
      
      rejected_cur_school <-  ordered_cur_school %>% 
        slice(-N_spots_available) %>% 
        pull(student_id)
      
      if (length(rejected_cur_school) > 0 & (print == TRUE)) {
        print(paste("student", rejected_cur_school, "rejected from school", spot_id))
      }
      
      n_rejected <- n_rejected + length(rejected_cur_school)
      
      #    provisional_cur_school <-  ordered_cur_school %>% 
      #      slice(N_spots_available)
      
      #Send rejections!
      current_apps  <- current_apps %>% 
        mutate( rejected = 
                  ifelse( current_proposal == 1 & 
                            school_id == spot_id & 
                            student_id %in% rejected_cur_school  
                          , 1, rejected)
        )
      
      #End of a school spot analysis            
    }
    #End of within school spot loop
    
    if (print == TRUE)  print(paste("Number of rejections this round:", n_rejected))
    
    
    #End of across schools spots loop  (a round)
  }
  #End of all rounds
  
  if (is.character(school_db$school_id)) {
    unmatched_value <- "unmatched"
  } else if (is.integer(school_db$school_id)) {
    unmatched_value <- -99L
  } else {
    unmatched_value <- -99
  }
  
  school_offers <-    cur_app_proposal %>% 
    filter(rejected == 0) %>% 
    select(student_id, school_id) %>% 
    left_join(school_offers, ., by = "student_id")   %>% 
    mutate(school_id = replace_na(school_id, unmatched_value))  
  
  
  if(time == TRUE) {
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(end_time - start_time, units = "secs")
    
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
    
  }
  
  
  return(school_offers)
  
}




#It takes 25 seconds to do one real DA in 2017 (smaller sample, lots of prob = 1)
Run_school_DA(apps_db = sae_apps_2017_DA, 
              school_db = oferta_2017_DA, 
              print = FALSE, time = TRUE)


#2 mins
short_sae_17_5_reps <-   Loop_DA(apps_db = sae_apps_2017_DA, 
                                 school_db = oferta_2017_DA, 
                                 5,
                                 time = TRUE)
short_sae_17_5_reps %>% 
  filter(prob < 1)


short_sae_17_60_reps <-   Loop_DA(apps_db = sae_apps_2017_DA, 
                                  school_db = oferta_2017_DA, 
                                  60,
                                  time = TRUE)

#33 min to run 60 as expected
short_sae_17_60_reps %>% 
  filter(prob < 1)

#It takes 2 seconds to do one real DA w GPT code
#It used to be 3.5 min
Run_school_DA(apps_db = sae_apps_2018_DA, 
              school_db = oferta_2018_DA, 
              print = FALSE, time = TRUE, seed = 123)

#fast approach: takes 0.2 s 
Run_school_DA_fast(apps_db = sae_apps_2018_DA, 
              school_db = oferta_2018_DA, 
              print = FALSE, time = TRUE, seed = 123)

#I can now take about 3 minutes to do 100 repeats
#It used to take 163 seconds min to do 5 repeats

sae_18_100_reps <-   Loop_DA(apps_db = sae_apps_2018_DA, 
                                 school_db = oferta_2018_DA, 
                                 100,
                                 time = TRUE)

#It took 55 seconds to do 100 repeats
#10 min to do 1k !?
sae_18_100_reps_fast <-   Loop_DA_fast(apps_db = sae_apps_2018_DA, 
                             school_db = oferta_2018_DA, 
                             100,
                             time = TRUE)

sae_18_10_reps_fast <-   Loop_DA_fast(apps_db = sae_apps_2018_DA, 
                                       school_db = oferta_2018_DA, 
                                       10, seed = 123, 
                                       time = TRUE)

sae_18_10_reps_fast_b <-   Loop_DA_fast(apps_db = sae_apps_2018_DA, 
                                      school_db = oferta_2018_DA, 
                                      10, seed = 123, 
                                      time = TRUE)

head(sae_18_10_reps_fast_b)


sae_18_100_reps %>% 
  filter(prob < 1)


sae_18_5_reps %>% 
  filter(school_id  == "unmatched")


results_2018 <- read_csv2("./data/raw/2018/SAE_2018/D1_Resultados_etapa_regular_2018_Admisión_2019_PUBL.csv") %>%
  mutate(proceso = 2018) %>%
  filter(cod_nivel == 9)


results_2018 %>% 
  filter(is.na(rbd_admitido))