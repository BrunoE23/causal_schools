####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"
data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


library(tidyverse)


setwd(code_output_wd)
source("./code/prob_admission_v4.R")
setwd(data_wd)



prep_apps_db <- function(year = NULL) {
path = "./data/raw/YYY/SAE_YYY/C1_Postulaciones_etapa_regular_YYY_Admisión_YYZ_PUBL.csv"
path <- str_replace_all(string = path, pattern = "YYY", paste0(year)) 
path <- str_replace(string = path, pattern = "YYZ", paste0(year+1))  


path2 <-"./data/raw/YYY/SAE_YYY/B1_Postulantes_etapa_regular_YYY_Admisión_YYZ_PUBL.csv"
path2 <- str_replace_all(string = path2, pattern = "YYY", paste0(year)) 
path2 <- str_replace(string = path2, pattern = "YYZ", paste0(year+1))  


  sae_apps_year <- read_csv2(path) %>% 
    mutate(proceso = year) %>% 
    mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
    filter(cod_nivel == 9) %>% 
    rename(student_id = mrun, 
           school_id = br_code,
           student_pref = preferencia_postulante,
           is_pie = es_pie,
           academic_order_transition = orden_alta_exigencia_transicion) %>% 
    mutate(priority_level = case_when(
      #    agregada_por_continuidad == 1 ~ 5L,
      prioridad_matriculado == 1 ~ 4L,
      prioridad_hermano == 1 ~ 3L,
      prioridad_hijo_funcionario == 1 ~ 2L,
      prioridad_exalumno == 1 ~ 1L,
      TRUE ~ 0L)) %>% 
    select(student_id, school_id, student_pref, priority_level, 
           is_pie, orden_pie , academic_order_transition ,
           loteria_original)
  
  sae_stud_info <- read_csv2(path2)  %>% 
    filter(cod_nivel== 9) %>% 
    select(mrun, prioritario, alto_rendimiento) %>% 
    rename(student_id = mrun,
           is_prioritario = prioritario,
           is_high_achv   = alto_rendimiento)
  
  sae_apps_all <- sae_apps_year %>% 
    left_join(sae_stud_info, by = "student_id")

  
  return(sae_apps_all)
  
  }
prep_spots_db <- function(year = NULL) {
  
  path = "./data/raw/YYY/SAE_YYY/A1_Oferta_Establecimientos_etapa_regular_YYY_Admisión_YYZ.csv"
  path <- str_replace_all(string = path, pattern = "YYY", paste0(year)) 
  path <- str_replace(string = path, pattern = "YYZ", paste0(year+1))  
  
  
  oferta_db <- read_csv2(path) %>% 
    mutate(proceso = year) %>% 
    mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
    filter(cod_nivel == 9) %>% 
    select(br_code, vacantes_pie , vacantes_prioritarios , vacantes_alta_exigencia_t, vacantes_alta_exigencia_r,  vacantes_regular) %>% 
    rename(  school_id = br_code,
             regular_spots         =  vacantes_regular,
             pie_spots             =  vacantes_pie, 
             prioritario_spots     =  vacantes_prioritarios,
             achievement_spots     =  vacantes_alta_exigencia_t,
             achievement_spots_reg =  vacantes_alta_exigencia_r,
    )
  
  
  
  
}
results_db <- function(year = NULL) {

  
  path = "./data/raw/YYY/SAE_YYY/D1_Resultados_etapa_regular_YYY_Admisión_YYZ_PUBL.csv"
  path <- str_replace_all(string = path, pattern = "YYY", paste0(year)) 
  path <- str_replace(string = path, pattern = "YYZ", paste0(year+1))  
  
  
  results_db <- read_csv2(path) %>%
    mutate(proceso = year) %>%
    filter(cod_nivel == 9) %>% 
    rename(rbd = rbd_admitido,
           cod_curso = cod_curso_admitido) %>% 
    mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
    mutate(br_code = ifelse(is.na(rbd), "unmatched", br_code)) %>% 
    select(mrun, br_code) %>% 
    rename(school_id_actual = br_code,
           student_id = mrun)
  
  return(results_db)
  
}


get_results <- function(year) {
  
  school_db_name <- paste0("schools_DA_", year)
  apps_db_name   <- paste0("apps_DA_", year)
  
  my_DA_results <- Run_school_DA_fast_CL_v4(
    school_db = get(school_db_name),
    apps_db = get(apps_db_name),
#    achievement_rule = "transition",
#    pie_rule = "school_order_if_available",
#    quota_order = c("pie", "achievement", "prioritario", "regular", "continuity"),
#    unused_quota_rule = "roll_to_regular",
#    special_priority_rule = "same_as_regular",
    use_loteria_original = TRUE,
    print = FALSE,
    time = TRUE
  )
  
  return(my_DA_results)
}
compare_results <- function(year) {
  
  message("Comparing results with my own computation of year ", year)
  
  results_db_name   <- paste0("results_DA_", year)
  
  compare_results <- left_join(get_results(year), get(results_db_name), by = "student_id") %>% 
    mutate(same_assignment = as.integer(school_id == school_id_actual))
  
  
  print(table(compare_results$same_assignment))
  print(prop.table(table(compare_results$same_assignment)))
  
  return(compare_results)
  
}

get_probs <- function(year, reps = 10) {

  school_db_name <- paste0("schools_DA_", year)
  apps_db_name   <- paste0("apps_DA_", year)
  
  
probs <-  Loop_DA_fast(
            get(school_db_name),
            get(apps_db_name),
            reps, 
            time = TRUE)


return(probs)
    
}


<<<<<<< HEAD
=======
#No achievements_spots_reg on my 
>>>>>>> 7e2e1f83606a6740fc6730efe3ec8ae2458ae785
#(sum(schools_DA_2018$achievement_spots_reg))
#(sum(schools_DA_2019$achievement_spots_reg))
#(sum(schools_DA_2020$achievement_spots_reg))
#(sum(schools_DA_2021$achievement_spots_reg))



#####2018 block
###################################

apps_DA_2018 <- prep_apps_db(2018)
schools_DA_2018 <- prep_spots_db(2018)
results_DA_2018 <- results_db(2018)



comp_results_2018 <- compare_results(2018)
#99.47%

probs_2018  <- get_probs(2018, 100)


#How many students are at (risk) ? 
student_risk <- probs_2018 %>% 
  group_by(student_id) %>% 
  mutate(risk = ifelse(max(prob) == 1.0, 0, 1))

#100 replications in 3.25 minutes3

####################################

#####2019 block
###################################
apps_DA_2019 <- prep_apps_db(2019)
schools_DA_2019 <- prep_spots_db(2019)
results_DA_2019 <- results_db(2019)


comp_results_2019 <- compare_results(2019)
#99.2%
###################################

#####2020 block
###################################
apps_DA_2020 <- prep_apps_db(2020)
schools_DA_2020 <- prep_spots_db(2020)
results_DA_2020 <- results_db(2020)


comp_results_2020 <- compare_results(2020)
#99.12%
###################################

#####2021 block
###################################
apps_DA_2021 <- prep_apps_db(2021)
schools_DA_2021 <- prep_spots_db(2021)
results_DA_2021 <- results_db(2021)


comp_results_2021 <- compare_results(2021)
#99.38%
###################################




##### Prob Computation
####################################

<<<<<<< HEAD
#Took like 90 min in small laptop
=======
#5660 seconds in small laptop (1h30)
>>>>>>> 7e2e1f83606a6740fc6730efe3ec8ae2458ae785
set.seed(233)
probs_2018  <- get_probs(2018, 1000)
write.csv(probs_2018, "./data/clean/DA_probs/DA_probs_2018.csv")

<<<<<<< HEAD
#Never run in small laptop
#3812 seconds
set.seed(283)
=======

>>>>>>> 7e2e1f83606a6740fc6730efe3ec8ae2458ae785
probs_2019  <- get_probs(2019, 1000)
write.csv(probs_2019, "./data/clean/DA_probs/DA_probs_2019.csv")


#3000 seconds on big laptop (50 min)
set.seed(253)
probs_2020  <- get_probs(2020, 1000)
write.csv(probs_2020, "./data/clean/DA_probs/DA_probs_2020.csv")

<<<<<<< HEAD

set.seed(263)
=======
>>>>>>> 7e2e1f83606a6740fc6730efe3ec8ae2458ae785
probs_2021  <- get_probs(2021, 1000)
write.csv(probs_2021, "./data/clean/DA_probs/DA_probs_2021.csv")




