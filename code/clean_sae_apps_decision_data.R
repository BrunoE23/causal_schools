####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


#Datawd (Dropbox) 
setwd(data_wd)
#####################################


library(tidyverse)


load("./data/clean/samples.RData")


results_2017 <- read_csv2("./data/raw/2017/SAE_2017/D1_Resultados_etapa_regular_2017_Admisión_2018_PUBL.csv") %>%
  mutate(proceso = 2017) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 


results_2018 <- read_csv2("./data/raw/2018/SAE_2018/D1_Resultados_etapa_regular_2018_Admisión_2019_PUBL.csv") %>%
  mutate(proceso = 2018) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 

results_2019 <- read_csv2("./data/raw/2019/SAE_2019/D1_Resultados_etapa_regular_2019_Admisión_2020_PUBL.csv") %>%
  mutate(proceso = 2019) %>% 
  filter(cod_nivel >= 7 & cod_nivel <= 9) 

#Simple framework: every applicant taken vs every applicant not taken
#Simple framework: every applicant taken vs every applicant 


offered_spots <- function(results, year,
                          sample = sample_students) {
  
  x_12R <- results  %>% 
    select(mrun, rbd_admitido, rbd_admitido_post_resp) 
  
  #    x_2R <- results  %>% 
  #       select(mrun, br_code, respuesta_postulante_post_lista_espera) %>% 
  #       mutate(offered_spot_2R = 1)
  
#  sample_students
  treatment  <-  sample %>% 
    filter(proceso == year) %>% 
    left_join(x_12R, by = c("mrun")) %>% 
    #       left_join(x_2R, by = c("mrun", "br_code")) %>% 
    mutate(offered_spot_1R = replace_na(as.numeric(rbd == rbd_admitido), 0),
           offered_spot_2R = replace_na(as.numeric(rbd == rbd_admitido_post_resp), 0),
           offered_spot_anyR =   pmax(offered_spot_1R, offered_spot_2R)) %>% 
  select(mrun, n_cupos, n_apps,  br_code, preferencia_postulante,
               offered_spot_1R, offered_spot_2R, offered_spot_anyR, 
                        rbd_admitido, rbd_admitido_post_resp)
  
  return(treatment)
  
}


treatment_2017 <- offered_spots(results_2017, 2017)
treatment_2018 <- offered_spots(results_2018, 2018)
treatment_2019 <- offered_spots(results_2019, 2019)



test_treatments <- rbind(treatment_2017, treatment_2018, treatment_2019)



prob_treatment_1R <- test_treatments %>% 
  arrange(br_code, preferencia_postulante) %>% 
  group_by(br_code, preferencia_postulante) %>% 
  summarize(prob_treatment = mean(offered_spot_1R, na.rm = TRUE),
            n_students = n(), 
            n_offers = sum(offered_spot_1R == 1), 
            n_cupos = max(n_cupos),
            n_apps = max(n_apps),
            prop_cupos = n_offers / n_cupos,
            prop_apps = n_students / n_apps) %>% 
  arrange(br_code, preferencia_postulante) %>%
  group_by(br_code) %>% 
  mutate(
    cumm_cupos = cumsum(prop_cupos)
  ) 


prob_treatment_1R %>% 
  filter(prob_treatment == 1) %>% 
  pull(cumm_cupos) %>% 
  summary()

prob_treatment_1R %>% 
  filter(preferencia_postulante == 3) %>% 
  pull(cumm_cupos) %>% 
  summary()

prob_treatment_1R %>% 
  filter(preferencia_postulante == 5) %>% 
  pull(cumm_cupos) %>% 
  summary()



prob_treatment_2R <- test_treatments %>% 
  group_by(br_code, preferencia_postulante) %>% 
  summarize(prob_treatment = mean(offered_spot_2R, na.rm = TRUE),
            n_students = n())



all_treatments <- rbind(treatment_2017, treatment_2018, treatment_2019) %>% 
  mutate(rbd_admitido = ifelse(is.na(rbd_admitido),0, rbd_admitido)) %>% 
  select(-c(preferencia_postulante,n_cupos, n_apps))

#all_treatments %>% 
#  filter(rbd_admitido == 0)

rm(treatment_2017, treatment_2018, treatment_2019)

save(all_treatments, file = "./data/clean/treatment_1R.RData")


