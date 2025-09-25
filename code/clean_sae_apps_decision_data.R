setwd("C:/Users/xd-br/Desktop/PhD/Research/causal_schools")

library(tidyverse)


load("./data/clean/samples.RData")


results_2017 <- read_csv2("./data/raw/2017/SAE_2017/D1_Resultados_etapa_regular_2017_Admisión_2018_PUBL.csv") %>%
  mutate(proceso = 2017) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>%
  mutate(br_code = paste0(as.character(rbd_admitido), "_", cod_nivel, "_", cod_curso_admitido, "_", proceso)) 

results_2018 <- read_csv2("./data/raw/2018/SAE_2018/D1_Resultados_etapa_regular_2018_Admisión_2019_PUBL.csv") %>%
  mutate(proceso = 2018) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>%
  mutate(br_code = paste0(as.character(rbd_admitido), "_", cod_nivel, "_", cod_curso_admitido, "_", proceso)) 


results_2019 <- read_csv2("./data/raw/2019/SAE_2019/D1_Resultados_etapa_regular_2019_Admisión_2020_PUBL.csv") %>%
  mutate(proceso = 2019) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) %>%
  mutate(br_code = paste0(as.character(rbd_admitido), "_", cod_nivel, "_", cod_curso_admitido, "_", proceso)) 


#Simple framework: every applicant taken vs every applicant not taken
#Simple framework: every applicant taken vs every applicant 

offered_spots <- function(results, year,
                          sample = sample_students) {
  
  x_1R <- results  %>% 
    select(mrun, br_code, respuesta_postulante, rbd_admitido) %>% 
    mutate(offered_spot_1R = 1)
  
  #    x_2R <- results  %>% 
  #       select(mrun, br_code, respuesta_postulante_post_lista_espera) %>% 
  #       mutate(offered_spot_2R = 1)
  
  
  treatment  <-  sample %>% 
    filter(proceso == year) %>% 
    left_join(x_1R, by = c("mrun", "br_code")) %>% 
    #       left_join(x_2R, by = c("mrun", "br_code")) %>% 
    mutate(offered_spot_1R = ifelse(is.na(respuesta_postulante), 0, offered_spot_1R)) %>%
    #        mutate(offered_spot_2R = ifelse(is.na(respuesta_postulante_2R), 0, offered_spot_2R))  
    select(mrun, br_code, offered_spot_1R, rbd_admitido, respuesta_postulante)
  
  return(treatment)
  
}

treatment_2017 <- offered_spots(results_2017, 2017)
treatment_2018 <- offered_spots(results_2018, 2018)
treatment_2019 <- offered_spots(results_2019, 2019)

all_treatments <- rbind(treatment_2017, treatment_2018, treatment_2019) %>% 
  mutate(rbd_admitido = ifelse(is.na(rbd_admitido),0, rbd_admitido))

#all_treatments %>% 
#  filter(rbd_admitido == 0)

rm(treatment_2017, treatment_2018, treatment_2019)

save(all_treatments, file = "./data/clean/treatment_1R.RData")


