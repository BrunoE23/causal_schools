
setwd("C:/Users/xd-br/Desktop/PhD/Research/Education_Chile")
library(tidyverse)


#read spots available
oferta_2017 <- read_csv2("./2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>% 
  mutate(cod_sede = NA_integer_)

oferta_2018 <- read_csv2("./2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 

oferta_2019 <- read_csv2("./2019/SAE_2019/A1_Oferta_Establecimientos_etapa_regular_2019_Admisión_2020.csv") %>% 
  mutate(proceso = 2019) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 


sae_oferta <- rbind(oferta_2019, oferta_2018, oferta_2017)
rm(oferta_2019, oferta_2018, oferta_2017)

## Apps 

sae_apps_2017 <- read_csv2("./2017/SAE_2017/C1_Postulaciones_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9)

sae_apps_2018 <- read_csv2("./2018/SAE_2018/C1_Postulaciones_etapa_regular_2018_Admisión_2019_PUBL.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_nivel, "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9)

sae_apps_2019 <- read_csv2("./2019/SAE_2019/C1_Postulaciones_etapa_regular_2019_Admisión_2020_PUBL.csv") %>% 
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

save(sae_oferta, sae_apps, file = "./data_clean/sae_2017_19.RData")


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

sample_cursos <- summ_cursos %>% 
  filter(n_cupos >= 50) %>% 
  filter(ratio_apps >= 2) %>% 
  arrange(rbd, cod_nivel, proceso) %>% 
  ungroup()


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


save(sample_students, sample_cursos, file = "./data_clean/samples.RData")

