####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

#IMPORTANT REMINDER: SAE 2017 takes place in 2016 and the spot allows you to
#get a spot in a different school in the academic year of 2017. 
#Obviously it is the same for other years.


#packages
library(tidyverse)


#read spots available
oferta_2017 <- read_csv2("./data/raw/2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>% 
  mutate(cod_sede = NA_integer_)

sum(oferta_2017$vacantes_regular)

oferta_2018 <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 

sum(oferta_2018$vacantes_regular)


oferta_2019 <- read_csv2("./data/raw/2019/SAE_2019/A1_Oferta_Establecimientos_etapa_regular_2019_Admisión_2020.csv") %>% 
  mutate(proceso = 2019) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 

sum(oferta_2019$vacantes_regular)


sae_oferta <- rbind(oferta_2019, oferta_2018, oferta_2017)
rm(oferta_2019, oferta_2018, oferta_2017)

## Apps 

sae_apps_2017 <- read_csv2("./data/raw/2017/SAE_2017/C1_Postulaciones_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9)

sae_apps_2018 <- read_csv2("./data/raw/2018/SAE_2018/C1_Postulaciones_etapa_regular_2018_Admisión_2019_PUBL.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9)

sae_apps_2019 <- read_csv2("./data/raw/2019/SAE_2019/C1_Postulaciones_etapa_regular_2019_Admisión_2020_PUBL.csv") %>% 
  mutate(proceso = 2019) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9)




#Filter out prioridad 
sae_apps <- rbind(sae_apps_2017, sae_apps_2018, sae_apps_2019) %>% 
  filter(agregada_por_continuidad == 0,
         prioridad_matriculado == 0,
         prioridad_hermano == 0,
         prioridad_hijo_funcionario == 0,
         prioridad_exalumno == 0) %>% 
  select(-c(agregada_por_continuidad, 
            prioridad_matriculado, prioridad_hermano,
            prioridad_hijo_funcionario, prioridad_exalumno))


rm(sae_apps_2017, sae_apps_2018, sae_apps_2019)


## Student controls

sae_stud_info_2017 <- read_csv2("./data/raw/2017/SAE_2017/B1_Postulantes_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  select(-(c(cod_com,nom_com))) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>% 
  mutate(proceso = 2017) 
  
sae_stud_info_2018 <- read_csv2("./data/raw/2018/SAE_2018/B1_Postulantes_etapa_regular_2018_Admisión_2019_PUBL.csv")  %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>% 
    mutate(proceso = 2018) 


sae_stud_info_2019 <- read_csv2("./data/raw/2019/SAE_2019/B1_Postulantes_etapa_regular_2019_Admisión_2020_PUBL.csv") %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>% 
  mutate(proceso = 2019) 


sae_stud_info <- rbind(sae_stud_info_2017,
                       sae_stud_info_2018,
                       sae_stud_info_2019) %>% 
  select(-(cod_nivel))

rm(sae_stud_info_2017,
   sae_stud_info_2018,
   sae_stud_info_2019)

#sae_apps <- left_join(sae_apps, sae_stud_info, by = "mrun", multiple = "all")

sae_stud_info %>% 
  group_by(mrun, proceso) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))
  
#Saving
length(unique(sae_apps$mrun))
length(unique(sae_stud_info$mrun))


save(sae_oferta, sae_apps, file = "./data/clean/sae_2017_19.RData")
save(sae_stud_info, file = "./data/clean/sae_2017_19_stud_controls.RData")




hist(sae_apps$loteria_original)
#Why is this not a uniform distribution? 
#are people randomized into integer numbers depending on the number of spots ?


# Identify oversubscribed spots 

cupos_p_curso <- sae_oferta %>% 
  group_by(rbd, cod_nivel, cod_curso, proceso) %>% 
  summarize(n_cupos = sum(vacantes_regular))  %>% 
  filter(n_cupos > 0)  
#  mutate(more_30_cupos = ifelse(n_cupos >= 30,1,0),
#         more_50_cupos = ifelse(n_cupos >= 50,1,0),
#  )

# table(cupos_p_curso$more_30_cupos)
# table(cupos_p_curso$more_50_cupos)


apps_p_curso <- sae_apps %>% 
  filter(preferencia_postulante <= 5) %>% 
  group_by(rbd, cod_nivel, cod_curso, proceso) %>% 
  summarize(n_apps = n())

summ_cursos <- left_join(cupos_p_curso, apps_p_curso, by = c("rbd", "cod_nivel", "cod_curso", "proceso")) %>% 
  mutate(exc_apps = n_apps - n_cupos,
         ratio_apps = n_apps / n_cupos) %>% 
  arrange(-(exc_apps)) %>%  
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso))


#Select sample of spots

summ_cursos %>%  
  select(rbd, cod_nivel) %>% 
  unique() %>% 
  nrow()

sample_cursos <- summ_cursos %>% 
  filter(n_cupos >= 50) %>% 
  filter(ratio_apps >= 2) %>% 
  arrange(rbd, cod_nivel, proceso) %>% 
  ungroup()


sample_cursos %>%  
  select(rbd, cod_nivel,proceso) %>% 
  unique() %>% 
  nrow()

length(unique(sample_cursos$rbd))


sample_selected_cursos <- summ_cursos %>% 
  filter(n_cupos >= 100) %>% 
  filter(ratio_apps >= 5) %>% 
  arrange(rbd, cod_nivel, proceso) %>% 
  ungroup()


######################
## Create data frame 
#######################


#For each spot, append all students that applied 

sample_students <- sample_cursos %>% 
  select(br_code, n_cupos, n_apps, exc_apps, ratio_apps) %>% 
  left_join(sae_apps, multiple = "all",
            by = "br_code") %>% 
  filter(preferencia_postulante <= 5) 

#sae_apps %>% 
#  filter(preferencia_postulante <= 5) %>%  
#  filter(br_code %in% sample_cursos$br_code) %>% 
#  pull(mrun) %>% 
#  unique %>% 
#  length()


save(sample_students, sample_cursos, file = "./data/clean/samples.RData")

