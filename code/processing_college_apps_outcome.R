####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) setwd(data_wd)
#####################################

library(tidyverse)
library(readxl)

load("./data/clean/psu_applications.RData")


#oferta_2020 <- read_xlsx("./2020/PSU2020/PostulaciónySelección_Admisión2020/Libro_CódigosADM2020_ArchivoD.xlsx", sheet = 3) %>% 
#  mutate(stem_share = `%_MATE` + `%_CIEN`,
#         stem_proxy_low = ifelse(stem_share > 45, 1L, 0L),
#         stem_proxy_high = ifelse(stem_share > 50, 1L, 0L)) %>% 
#  rename(PROCESO = AGNO_ACAD) %>% 
#  select(PROCESO, CODIGO, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high)



#colnames(program_info_2024)

program_info_2024 <-    read_csv2("./data/raw/2024/Matricula-Ed-Superior-2024/20250729_Matrícula_Ed_Superior_2024_PUBL_MRUN.csv",
            locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%  
  rename_with(~ toupper(.x)) %>% 
      select(CODIGO_UNICO, 
             TIPO_INST_1, TIPO_INST_2 , TIPO_INST_3, COD_INST, NOMB_INST,
             COD_SEDE, NOMB_SEDE,
             COD_CARRERA, NOMB_CARRERA, 
             DUR_TOTAL_CARR,
             REGION_SEDE,  PROVINCIA_SEDE,  COMUNA_SEDE, 
             NIVEL_GLOBAL,
             NIVEL_CARRERA_1, NIVEL_CARRERA_2,
             CODIGO_DEMRE,
             AREA_CONOCIMIENTO,
             CINE_F_97_AREA_AREA, CINE_F_97_SUBAREA, 
             AREA_CARRERA_GENERICA, 
             CINE_F_13_AREA, CINE_F_13_SUBAREA, 
             ACREDITADA_CARR, ACREDITADA_INST
             ) %>% 
  unique()  %>% 
  filter(NIVEL_CARRERA_2 %in% c("Carreras Profesionales", "Carreras Técnicas")) %>% 
  mutate(science1    = as.integer(AREA_CONOCIMIENTO == "Ciencias Básicas"),
         technology1 = as.integer(AREA_CONOCIMIENTO == "Tecnología"),
         health1     = as.integer(AREA_CONOCIMIENTO == "Salud"),
         engineer2   = as.integer(CINE_F_13_AREA == "Ingeniería, Industria y Construcción"),
         science2    = as.integer(CINE_F_13_AREA == "Ciencias naturales, matemáticas y estadística"),
         technology2 = as.integer(CINE_F_13_AREA == "Tecnología de la Información y la Comunicación (TIC)"),
         health2     = as.integer(CINE_F_13_AREA == "Salud y Bienestar")
  ) %>% 
  rename(COD_SIES = CODIGO_UNICO) %>% 
  select(COD_SIES, 
         NOMB_CARRERA, TIPO_INST_1, NOMB_INST, COD_INST,AREA_CARRERA_GENERICA,
         science1, technology1, health1, engineer2, science2, technology2,health2,
         ACREDITADA_CARR, ACREDITADA_INST)
         
length(unique(program_info_2024$COD_SIES))


mifuturo_recent <- readxl::read_xlsx(
  "./data/raw/mifuturo/Buscador_Empleabilidad_ingresos_2025_2026_SIES.xlsx",
  sheet = 2) %>%  
  rename(NOMB_INST             = `Nombre de institución`, 
         AREA_CARRERA_GENERICA = `Nombre carrera genérica`,
         TIPO_INST_1           = `Tipo de institución`,
         NOMB_CARRERA           = `Nombre carrera (del título)`,
         
         employment_1st_year   = `Empleabilidad 1er año`,
         employment_2nd_year   = `Empleabilidad 2° año`,
         income_4th_year       = `Ingreso Promedio al 4° año`
  ) %>% 
  mutate(TIPO_INST_1 = ifelse(TIPO_INST_1 == "Insitutos Profesionales", 
                              "Institutos Profesionales", 
                              TIPO_INST_1)) %>% 
  select(NOMB_CARRERA, NOMB_INST, AREA_CARRERA_GENERICA, TIPO_INST_1,
         employment_1st_year, employment_2nd_year, income_4th_year
  )

#Append employment data
program_info_2024 <-  left_join(program_info_2024,
                                mifuturo_recent, 
                                by = c("NOMB_CARRERA", 
                                       "NOMB_INST",
                                       "AREA_CARRERA_GENERICA",
                                       "TIPO_INST_1"))



oferta_2024 <- read_csv2("./data/raw/2024/PAES-2024-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2024_REV.csv") %>%
  mutate(stem_share = rowSums(across(c(CIEN, M1, M2)), na.rm = TRUE),
         stem_proxy_low = ifelse(stem_share >= 40, 1L, 0L),
         stem_proxy_high = ifelse(stem_share > 40, 1L, 0L)) %>% 
  mutate(PROCESO = 2024) %>%
  rename(COD_CARRERA_PREF = COD_CARRERA) %>%
  select(PROCESO, COD_CARRERA_PREF, COD_SIES, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high) %>% 
  left_join(program_info_2024, by = "COD_SIES")



hist(oferta_2024$stem_share, breaks = 20)
prop.table(table(oferta_2024$stem_share))


                         
#oferta_combined <- rbind(oferta_2024, oferta_2020) %>%
#                   select(-PROCESO) %>%
#                    unique() %>%
#                    arrange(CODIGO)

colnames(oferta_2024)



  stem_outcome <- college_apps %>% 
        filter(ORDEN_PREF <= 5) %>%
        filter(TIPO_PREF == "REGULAR") %>%
        left_join(oferta_2024, by = "COD_CARRERA_PREF") %>%
        group_by(mrun) %>%
  #Only first application
    filter(year == min(year)) %>%
    summarize(avg_stem_share = mean(stem_share),
              
              across(
                c(science1, technology1, health1, engineer2, science2, technology2, health2),
                ~ mean(.x, na.rm = TRUE),                 # proportion=mean for 0/1
                .names = "prop_{.col}"),
                
              n_stem_low  = sum(stem_proxy_low),
              n_stem_high = sum(stem_proxy_high),
              only_stem_low  = min(stem_proxy_low),
              only_stem_high = min(stem_proxy_high),
              year_1st_app = min(year)
    )
  
  
  
  save(stem_outcome, file = "./data/clean/stem_outcome.RData")
  
  
  
  
