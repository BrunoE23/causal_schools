setwd("C:/Users/xd-br/Desktop/PhD/Research/Education_Chile")

library(tidyverse)

#Reading student outcome data

students_apps2025 <- read_csv2("./2025/PAES_2025_Inscritos_Puntajes/A_INSCRITOS_PUNTAJES_PAES_2025_PUB_MRUN.csv") %>%
  mutate(year = 2025)  %>% 
  rename(mrun = MRUN) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL,
         GRUPO_DEPENDENCIA = DEPENDENCIA) %>% 
  filter(ANYO_DE_EGRESO == (year - 1)) %>% 
  filter(PROM_NOTAS != 0) %>% 
  mutate(leng_max   = CLEC_MAX,
         math_max   = MATE1_MAX,
         hist_max   = HCSOC_MAX,
         scien_max  = CIEN_MAX) %>% 
  select(mrun, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, 
         CODIGO_REGION_EGRESO, CODIGO_COMUNA_EGRESO)


students_apps2024 <- read_csv2("./2024/PAES-2024-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PAES_2024_PUB_MRUN.csv") %>%
  mutate(year = 2024)  %>% 
  rename(mrun = MRUN) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL,
         GRUPO_DEPENDENCIA = DEPENDENCIA) %>% 
  filter(ANYO_DE_EGRESO == (year - 1)) %>% 
  filter(PROM_NOTAS != 0) %>% 
  mutate(leng_max   = CLEC_MAX,
         math_max   = MATE1_MAX,
         hist_max   = HCSOC_MAX,
         scien_max  = CIEN_MAX) %>% 
  select(mrun, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, 
         CODIGO_REGION_EGRESO, CODIGO_COMUNA_EGRESO)


students_apps2023 <- read_csv2("./2023/PAES2023-Inscritos-Puntajes-1/A_INSCRITOS_PUNTAJES_2023_PAES_PUB_MRUN.csv") %>%
  mutate(year = 2023)  %>% 
  rename(mrun = MRUN) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL,
         GRUPO_DEPENDENCIA = DEPENDENCIA) %>% 
  filter(ANYO_DE_EGRESO == (year - 1)) %>% 
  filter(PROM_NOTAS != 0) %>% 
  mutate(leng_max   = CLEC_MAX,
         math_max   = MATE1_MAX,
         hist_max   = HCSOC_MAX,
         scien_max  = CIEN_MAX) %>% 
  select(mrun, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, 
         CODIGO_REGION_EGRESO, CODIGO_COMUNA_EGRESO)


students_apps2022 <- read_csv2("./2022/PTU2022-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PDT_2022_PUB_MRUN.csv") %>%
  mutate(year = 2022)  %>% 
  rename(mrun = MRUN) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL,
         GRUPO_DEPENDENCIA = DEPENDENCIA) %>% 
  filter(ANYO_DE_EGRESO == (year - 1)) %>% 
  filter(PROM_NOTAS != 0) %>% 
  mutate(leng_max  = pmax(CLEC_ACTUAL, CLEC_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, 
         CODIGO_REGION_EGRESO, CODIGO_COMUNA_EGRESO)

students_apps2021 <- read_csv2("./2021/PTU2021-Inscritos-Puntajes/A_INSCRITOS_PUNTAJES_PDT_2021_PUB_MRUN.csv") %>%
  mutate(year = 2021)  %>% 
  rename(mrun = MRUN) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL,
         GRUPO_DEPENDENCIA = DEPENDENCIA) %>% 
  filter(ANYO_DE_EGRESO == (year - 1)) %>% 
  filter(PROM_NOTAS != 0) %>% 
  mutate(leng_max  = pmax(CLEC_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, 
         CODIGO_REGION_EGRESO, CODIGO_COMUNA_EGRESO)

#socioecon_2021 <- read_csv2("./2021/Prueba-de-Transicion-Universitaria-2021-Datos-socioeconomicos/B_SOCIOECONOMICO_DOMICILIO_PDT_2021_PUB_MRUN.csv")

#Combining apps 

students_apps <- rbind(students_apps2021, 
                       students_apps2022, 
                       students_apps2023,
                       students_apps2024,
                       students_apps2025)

rm(students_apps2021, 
   students_apps2022, 
   students_apps2023,
   students_apps2024,
   students_apps2025)


# 5 odd MRUN ? 
odd_mrun <- students_apps %>%
  group_by(mrun) %>%
  summarize(n_shows = n()) %>%
  filter(n_shows > 1)

students_apps <- students_apps %>%
  filter(!(mrun %in% odd_mrun$mrun))



#############################################
#Demre years

students_apps2020 <- read_csv2("./2020/PSU2020/Rinden_Admisión2020/ArchivoC_Adm2020.csv") %>%
  rename(mrun_demre = ID_aux) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL) %>% 
  filter(SITUACION_EGRESO == 1) %>% 
  mutate(leng_max  = pmax(LENG_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun_demre, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA) %>% 
  mutate(year = 2020)





students_apps2019 <- read_csv2("./2019/PSU2019/Rinden_Admisión2019/ArchivoC_Adm2019.csv") %>%
  rename(mrun_demre = ID_aux) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL) %>% 
  filter(SITUACION_EGRESO == 1) %>% 
  mutate(leng_max  = pmax(LENG_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun_demre, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA) %>% 
  mutate(year = 2019)


students_apps2018 <- read_csv2("./2018/PSU2018/Rinden_Admisión2018/ArchivoC_Adm2018.csv") %>%
  rename(mrun_demre = ID_aux) %>% 
  rename(PROM_NOTAS = PROMEDIO_NOTAS,
         RAMA = RAMA_EDUCACIONAL) %>% 
  filter(SITUACION_EGRESO == 1) %>% 
  mutate(leng_max  = pmax(LENG_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun_demre, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA) %>% 
  mutate(year = 2018)


students_apps2017 <- read_csv2("./2017/PSU2017/Rinden_Admisión2017/ArchivoC_Adm2017.csv") %>%
  rename(mrun_demre = ID_aux) %>% 
  filter(SITUACION_EGRESO == 1) %>% 
  mutate(leng_max  = pmax(LENG_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun_demre, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA) %>% 
  mutate(year = 2017)

students_apps2016 <- read_csv2("./2016/PSU2016/Rinden_Admisión2016/ArchivoC_Adm2016.csv") %>%
  rename(mrun_demre = ID_aux) %>% 
  filter(SITUACION_EGRESO == 1) %>% 
  mutate(leng_max  = pmax(LENG_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun_demre, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA)%>% 
  mutate(year = 2016)

students_apps2015 <- read_csv2("./2015/PSU2015/Rinden_Admisión2015/ArchivoC_Adm2015.csv") %>%
  rename(mrun_demre = ID_aux) %>% 
  filter(SITUACION_EGRESO == 1) %>% 
  mutate(leng_max  = pmax(LENG_ACTUAL, LENG_ANTERIOR, na.rm = TRUE),
         math_max  = pmax(MATE_ACTUAL, MATE_ANTERIOR, na.rm = TRUE),
         hist_max  = pmax(HCSO_ACTUAL, HCSO_ANTERIOR, na.rm = TRUE),
         scien_max = pmax(CIEN_ACTUAL, CIEN_ANTERIOR, na.rm = TRUE)) %>% 
  select(mrun_demre, RBD, 
         PROM_NOTAS, PTJE_NEM, PTJE_RANKING,
         leng_max, math_max, hist_max, scien_max,
         RAMA, GRUPO_DEPENDENCIA, CODIGO_REGION, CODIGO_COMUNA) %>% 
  mutate(year = 2015)


students_apps_demre <- rbind(students_apps2015, 
                             students_apps2016, 
                             students_apps2017,
                             students_apps2018,
                             students_apps2019,
                             students_apps2020)

rm(students_apps2015, 
   students_apps2016, 
   students_apps2017,
   students_apps2018,
   students_apps2019,
   students_apps2020)



save(students_apps, students_apps_demre, file = "./data_clean/psu_students.RData")


########################

#Reading college app data
# and making it long data


college_apps2025 <- read_csv2("./2025/PAES_2025_Postulantes_Totales/C_POSTULANTES_SELECCION_PAES_2025_PUB_MRUN.csv") %>%
  rename(mrun = MRUN) %>% 
  rename(year = ANYO_PROCESO) %>% 
  arrange(mrun, ORDEN_PREF) %>% 
  select(        mrun,
                 year,
                 SEXO,
                 ORDEN_PREF,
                 COD_CARRERA_PREF,
                 ESTADO_PREF,
                 PTJE_PREF,
                 LUGAR_PREF,
                 TIPO_PREF )


college_apps2024 <- read_csv2("./2024/PAES-2024-Postulantes-Totales/C_POSTULANTES_SELECCION_PAES_2024_PUB_MRUN.csv") %>%
  rename(mrun = MRUN) %>% 
  rename(year = ANYO_PROCESO) %>% 
  arrange(mrun, ORDEN_PREF) %>% 
  select(        mrun,
                 year,
                 SEXO,
                 ORDEN_PREF,
                 COD_CARRERA_PREF,
                 ESTADO_PREF,
                 PTJE_PREF,
                 LUGAR_PREF,
                 TIPO_PREF )

#colnames(college_apps2024)
#table(college_apps2024$TIPO_PREF)

#2024 is already long!


cast_applications_long <- function(x) {
  
  x_long <-   x %>% 
    rename(mrun = MRUN) %>% 
    rename(year = ANYO_PROCESO) %>% 
    
    #keep only variables as of 2024 
    select(
      mrun,
      year,
      matches("PREF")
    ) %>%
    # Where all the pivoting occurs:
    pivot_longer(
      cols = matches("^(COD_CARRERA_PREF|ESTADO_PREF|PTJE_PREF|LUGAR_PREF)_\\d{2}$"), 
      names_to = c(".value", "ORDEN_PREF"),
      names_pattern = "(.*)_(\\d+)"
    ) %>%
    mutate(
      ORDEN_PREF = as.integer(ORDEN_PREF),
      # Make sure types are sensible; tweak if your data types differ
      PTJE_PREF = suppressWarnings(as.numeric(PTJE_PREF))
    ) %>%
    # Add columns that exist in 2024 (fill as NA for now)
    mutate(
      SEXO = NA_character_,
      TIPO_PREF = "REGULAR"
    ) %>%
    # Reorder & rename to match 2024 exactly
    select(
      mrun,
      year,
      SEXO,
      ORDEN_PREF,
      COD_CARRERA_PREF,
      ESTADO_PREF,
      PTJE_PREF,
      LUGAR_PREF,
      TIPO_PREF
    ) %>% 
    filter(COD_CARRERA_PREF != 0)
  
  return(x_long)
  
}

college_apps2023_long <- cast_applications_long(
  read_csv2("./2023/PAES2023-Postulantes-Regulares/C_POSTULANTES_SELECCION_2023_PAES_PUB_MRUN.csv"))



#2023 is now long! 

college_apps2022_long <- cast_applications_long(
  read_csv2("./2022/PTU2022-Postulantes-Seleccion-Regular/C_POSTULANTES_SELECCION_PDT_2022_PUB_MRUN.csv"))

#2022 is now long! 


college_apps2021_long <- cast_applications_long(
  read_csv2("./2021/PTU2021-Postulantes-Seleccion-Regular/C_POSTULANTES_SELECCION_PDT_2021_PUB_MRUN.csv"))

#2021 is now long! 


college_apps <- rbind(college_apps2025,
                      college_apps2024,
                      college_apps2023_long,
                      college_apps2022_long,
                      college_apps2021_long)


rm(college_apps2025,
   college_apps2024,
   college_apps2023_long,
   college_apps2022_long,
   college_apps2021_long)


####
#Demre years
####################

cast_applications_long_demre <- function(x) {
  
  x_long <- x %>%
    rename(mrun = ID_aux) %>%
    select(mrun, year, matches("PREF")) %>%
    pivot_longer(
      cols = matches("^(COD_CARRERA_PREF|ESTADO_PREF|PTJE_PREF|LUGAR_PREF)_\\d{2}(_(BEA|PACE))?$"),
      names_to = c("VAR", "ORDEN_PREF", "TIPO_PREF"),
      names_pattern = "^(COD_CARRERA_PREF|ESTADO_PREF|PTJE_PREF|LUGAR_PREF)_(\\d{2})(?:_(BEA|PACE))?$",
      values_to = "value",
      values_drop_na = TRUE,
      # 🔧 key fix: coerce mixed types to a common type during gather
      values_transform = list(value = as.character)
    ) %>%
    mutate(ORDEN_PREF = as.integer(ORDEN_PREF)) %>%
    pivot_wider(names_from = VAR, values_from = value) %>%
    mutate(
      TIPO_PREF  = if_else(TIPO_PREF == "", "REGULAR", TIPO_PREF),
      #       # ensure columns exist / have sensible types
      LUGAR_PREF = if (!"LUGAR_PREF" %in% names(.)) NA_character_ else LUGAR_PREF,
      ESTADO_PREF = as.character(ESTADO_PREF),
      COD_CARRERA_PREF = suppressWarnings(as.numeric(COD_CARRERA_PREF)),
      PTJE_PREF = suppressWarnings(as.numeric(PTJE_PREF)),
      # POND_ACAD_PREF = suppressWarnings(as.numeric(POND_ACAD_PREF)),
      SEXO = if (!"SEXO" %in% names(.)) NA_character_ else as.character(SEXO)
    ) %>%
    filter(!is.na(COD_CARRERA_PREF) & COD_CARRERA_PREF != 0) %>%
    select(
      mrun,
      year,
      SEXO,
      ORDEN_PREF,
      COD_CARRERA_PREF,
      ESTADO_PREF,
      PTJE_PREF,
      LUGAR_PREF,
      TIPO_PREF
    )  
  
  
  return(x_long)
  
}


college_apps2020_long <- read_csv2("./2020/PSU2020/PostulaciónySelección_Admisión2020/ArchivoD_Adm2020.csv") %>%
  mutate(year = 2020) %>% 
  cast_applications_long_demre()

college_apps2019_long <- read.csv("./2019/PSU2019/PostulaciónySelección_Admisión2019/ArchivoD_Adm2019.csv", sep = ";" , dec = c(",", ".")) %>%
  mutate(year = 2019) %>% 
  cast_applications_long_demre()


college_apps2018_long <- read.csv("./2018/PSU2018/PostulaciónySelección_Admisión2018/ArchivoD_Adm2018.csv", sep = ";" , dec = c(",", ".")) %>%
  mutate(year = 2018) %>% 
  rename_with(~ str_replace(.x, "^PTJE_PREF", "PTJE_PREF_"),
              starts_with("PTJE_PREF")) %>% 
  cast_applications_long_demre()


college_apps2017_long <- read.csv("./2017/PSU2017/Postulación_Admisión2017/ArchivoD_Adm2017.csv", sep = ";" , dec = c(",", ".")) %>%
  mutate(year = 2017) %>% 
  rename_with(~ str_replace(.x, "^PTJE_PREF", "PTJE_PREF_"),
              starts_with("PTJE_PREF")) %>% 
  cast_applications_long_demre()

college_apps2016_long <- read.csv("./2016/PSU2016/Postulación_Admisión2016/ArchivoD_Adm2016.csv", sep = ";" , dec = c(",", ".")) %>%
  mutate(year = 2016) %>% 
  rename_with(~ str_replace(.x, "^PTJE_PREF", "PTJE_PREF_"),
              starts_with("PTJE_PREF")) %>% 
  cast_applications_long_demre()

college_apps2015_long <- read.csv("./2015/PSU2015/Postulación_Admisión2015/ArchivoD_Adm2015.csv", sep = ";" , dec = c(",", ".")) %>%
  mutate(year = 2015) %>% 
  rename_with(~ str_replace(.x, "^PTJE_PREF", "PTJE_PREF_"),
              starts_with("PTJE_PREF")) %>% 
  cast_applications_long_demre()


college_apps_demre <- rbind(college_apps2020_long,
                            college_apps2019_long,
                            college_apps2018_long,
                            college_apps2017_long,
                            college_apps2016_long,
                            college_apps2015_long)

rm(college_apps2020_long,
   college_apps2019_long,
   college_apps2018_long,
   college_apps2017_long,
   college_apps2016_long,
   college_apps2015_long)


save(college_apps, college_apps_demre, file = "./data_clean/psu_applications.RData")





