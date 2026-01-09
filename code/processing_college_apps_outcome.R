####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
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
  select(#NOMB_CARRERA, 
    NOMB_INST, AREA_CARRERA_GENERICA, TIPO_INST_1,
         employment_1st_year, employment_2nd_year, income_4th_year
  ) 
#%>% 
#mutate(NOMB_CARRERA = stringr::str_to_upper(NOMB_CARRERA))



mifuturo_recent_clean <- mifuturo_recent %>%
  mutate(
    # --- employment: coerce to character, replace "n/a"/"s/i", then parse numeric
    employment_1st_year = as.character(employment_1st_year),
    employment_2nd_year = as.character(employment_2nd_year),
    
    employment_1st_year = na_if(employment_1st_year, "n/a"),
    employment_1st_year = na_if(employment_1st_year, "s/i"),
    employment_2nd_year = na_if(employment_2nd_year, "n/a"),
    employment_2nd_year = na_if(employment_2nd_year, "s/i"),
    
    employment_1st_year = parse_number(employment_1st_year, locale = locale(decimal_mark = ".")),
    employment_2nd_year = parse_number(employment_2nd_year, locale = locale(decimal_mark = ".")),
    
    # --- income: keep as character, replace "n/a"/"s/i"
    income_4th_year = as.character(income_4th_year),
    income_4th_year = na_if(income_4th_year, "n/a"),
    income_4th_year = na_if(income_4th_year, "s/i")
  )


parse_income_midpoint_clp <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("n/a", "s/i", "", "NA")] <- NA_character_
  
  extract_amounts <- function(s) {
    if (is.na(s)) return(numeric(0))
    s2 <- str_squish(str_replace_all(s, "\\$", ""))
    
    pat <- "(\\d+)\\s+mill[oó]n(?:es)?(?:\\s+(\\d+)\\s+mil)?|(\\d+)\\s+mil"
    m <- str_match_all(s2, pat)[[1]]
    if (nrow(m) == 0) return(numeric(0))
    
    out <- numeric(0)
    for (i in seq_len(nrow(m))) {
      mil1  <- m[i, 2]
      milk  <- m[i, 3]
      onlyk <- m[i, 4]
      if (!is.na(mil1)) {
        out <- c(out, as.numeric(mil1) * 1e6 + ifelse(is.na(milk), 0, as.numeric(milk) * 1e3))
      } else if (!is.na(onlyk)) {
        out <- c(out, as.numeric(onlyk) * 1e3)
      }
    }
    out
  }
  
  vapply(x, function(s) {
    if (is.na(s)) return(NA_real_)
    
    if (s == "Sobre $3 millones 500 mil") return(4e6)
    
    amts <- extract_amounts(s)
    if (length(amts) >= 2) mean(amts[1:2]) else NA_real_
  }, numeric(1))
}

mifuturo_recent_clean <- mifuturo_recent_clean %>%
  mutate(income_4th_year_mid_clp = parse_income_midpoint_clp(income_4th_year))

fe_reg_wage <- lm(data = mifuturo_recent_clean, income_4th_year_mid_clp ~ factor(NOMB_INST) + factor(AREA_CARRERA_GENERICA))
summary(fe_reg_wage)$r.squared
nobs(fe_reg_wage)

sum(is.na(mifuturo_recent_clean$income_4th_year_mid_clp))
sum(is.na(mifuturo_recent_clean$employment_1st_year))
sum(is.na(mifuturo_recent_clean$employment_2nd_year))

fe_reg_employ <- lm(data = mifuturo_recent_clean, employment_1st_year ~ factor(NOMB_INST) + factor(AREA_CARRERA_GENERICA))
summary(fe_reg_employ)$r.squared
nobs(fe_reg_employ)

fe_reg_employ2 <- lm(data = mifuturo_recent_clean, employment_2nd_year ~ factor(NOMB_INST) + factor(AREA_CARRERA_GENERICA))
summary(fe_reg_employ2)$r.squared
nobs(fe_reg_employ2)

#Append employment data
program_info_2024_joint <-  left_join(program_info_2024,
                                mifuturo_recent_clean, 
                                by = c("NOMB_INST",
                                       "AREA_CARRERA_GENERICA",
                                       "TIPO_INST_1"))

#sum(program_info_2024$NOMB_CARRERA %in% mifuturo_recent$NOMB_CARRERA)
sum(program_info_2024$NOMB_INST %in% mifuturo_recent$NOMB_INST)
sum(program_info_2024$AREA_CARRERA_GENERICA %in% mifuturo_recent$AREA_CARRERA_GENERICA)
sum(program_info_2024$TIPO_INST_1 %in% mifuturo_recent$TIPO_INST_1)

#TODO: See and think on how to fix this NAs 
sum(is.na(program_info_2024_joint$income_4th_year))
sum(is.na(program_info_2024_joint$income_4th_year_clp))


oferta_2024 <- read_csv2("./data/raw/2024/PAES-2024-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2024_REV.csv") %>%
  mutate(stem_share = rowSums(across(c(CIEN, M1, M2)), na.rm = TRUE),
         stem_proxy_low = ifelse(stem_share >= 40, 1L, 0L),
         stem_proxy_high = ifelse(stem_share > 40, 1L, 0L)) %>% 
  mutate(PROCESO = 2024) %>%
  rename(COD_CARRERA_PREF = COD_CARRERA) %>%
  select(PROCESO, COD_CARRERA_PREF, COD_SIES, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high) %>% 
  left_join(program_info_2024_joint, by = "COD_SIES")



hist(oferta_2024$stem_share, breaks = 20)
prop.table(table(oferta_2024$stem_share))


hist(oferta_2024$income_4th_year_mid_clp, breaks = 20)

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
                
              n_stem_low  = sum(stem_proxy_low, na.rm = TRUE),
              n_stem_high = sum(stem_proxy_high, na.rm = TRUE),
              only_stem_low  = min(stem_proxy_low),
              only_stem_high = min(stem_proxy_high),
              year_1st_app = min(year),
              #,
              avg_employ_y1 = mean(employment_1st_year, na.rm = TRUE),   
              avg_employ_y2 = mean(employment_2nd_year, na.rm = TRUE),   
              avg_income_y4 = mean(income_4th_year_mid_clp, na.rm = TRUE),       
              
                            
    )
  
  sum(is.na(stem_outcome$avg_income_y4))
  
  save(stem_outcome, file = "./data/clean/stem_outcome.RData")
  