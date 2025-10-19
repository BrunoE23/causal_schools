####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################



library(tidyverse)
library(readxl)

load("./data/clean/psu_applications.RData")
rm(students_apps_demre)


#oferta_2020 <- read_xlsx("./2020/PSU2020/PostulaciónySelección_Admisión2020/Libro_CódigosADM2020_ArchivoD.xlsx", sheet = 3) %>% 
#  mutate(stem_share = `%_MATE` + `%_CIEN`,
#         stem_proxy_low = ifelse(stem_share > 45, 1L, 0L),
#         stem_proxy_high = ifelse(stem_share > 50, 1L, 0L)) %>% 
#  rename(PROCESO = AGNO_ACAD) %>% 
#  select(PROCESO, CODIGO, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high)


oferta_2024 <- read_csv2("./data/raw/2024/PAES-2024-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2024_REV.csv") %>%
  mutate(stem_share = rowSums(across(c(CIEN, M1, M2)), na.rm = TRUE),
         stem_proxy_low = ifelse(stem_share >= 40, 1L, 0L),
         stem_proxy_high = ifelse(stem_share > 40, 1L, 0L)) %>% 
  mutate(PROCESO = 2024) %>%
  rename(COD_CARRERA_PREF = COD_CARRERA) %>%
  select(PROCESO, COD_CARRERA_PREF, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high)


hist(oferta_2024$stem_share)
prop.table(table(oferta_2024$stem_share))



#oferta_combined <- rbind(oferta_2024, oferta_2020) %>%
#                   select(-PROCESO) %>%
#                    unique() %>%
#                    arrange(CODIGO)


  stem_outcome <- college_apps %>% 
        filter(ORDEN_PREF <= 5) %>%
        filter(TIPO_PREF == "REGULAR") %>%
        left_join(oferta_2024, by = "COD_CARRERA_PREF") %>%
        group_by(mrun) %>%
  #Only first application
    filter(year == min(year)) %>%
    summarize(avg_stem_share = mean(stem_share),
              n_stem_low  = sum(stem_proxy_low),
              n_stem_high = sum(stem_proxy_high),
              only_stem_low  = min(stem_proxy_low),
              only_stem_high = min(stem_proxy_high),
              year_1st_app = min(year)
    )
  
  
  save(stem_outcome, file = "./data/clean/stem_outcome.RData")
  