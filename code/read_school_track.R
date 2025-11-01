####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)
library(readr)
library(tidyr)

#read samples to get student universe
load("./data/clean/samples.RData")

sum(sample_cursos$n_cupos[sample_cursos$proceso == 2019])

students_small <- sample_students %>% 
  select(mrun, rbd, cod_nivel, proceso) %>% 
  rename(rbd_target = rbd,
         cod_nivel_target = cod_nivel,
         sae_proceso = proceso,
         MRUN = mrun) %>% 
  mutate(MRUN = as.character(MRUN)) %>%
  # mutate(year_track = proceso - 1) %>% 
  unique()


sample_students_unique <- sample_students %>% 
  pull(mrun) %>% 
  unique()

length(sample_students_unique)


repeated_students_in_sample <- students_small %>% 
  group_by(MRUN, sae_proceso) %>% 
  mutate(n_apps_p_proceso = n()) %>% 
  arrange(desc(n_apps_p_proceso), MRUN)   %>%  
  ungroup()

repeated_students_in_sample %>% 
  select(MRUN, n_apps_p_proceso) %>% 
  unique() %>% 
  pull(n_apps_p_proceso) %>% 
  table()


#function 

read_tracking_data <- function(year) {
  
  folder_path <- paste0("./data/raw/student_tracking/", year, "/Rendimiento-", year)
  
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("❌ No CSV files found in folder: ", folder_path)
  } else if (length(csv_files) > 1) {
    stop("❌ Multiple CSV files found in folder: ", paste(csv_files, collapse = ", "))
  }
  
  
  
  tracking_year <-   read_csv2(csv_files[1])
  names(tracking_year) <- toupper(names(tracking_year))
  
  message("✅ Successfully read: ", csv_files[1])
  
  tracking_year <- tracking_year %>%
    filter(!is.na(SIT_FIN)) %>%
    select(dplyr::any_of(c(
      # Year
      "AGNO",
      # School vars
      "RBD","NOM_RBD","COD_REG_RBD","COD_PRO_RBD","COD_COM_RBD","NOM_COM_RBD",
      "COD_DEPE","COD_DEPE2","RURAL_RBD",
      # Program/level
      "COD_ENSE","COD_ENSE2",
      # Grade vars
      "COD_GRADO","LET_CUR","COD_JOR",
      # Student vars
      "MRUN","GEN_ALU","FEC_NAC_ALU","EDAD_ALU", # EDAD_ALU will be kept only if present
      "COD_COM_ALU","NOM_COM_ALU", "COD_REG_ALU",
      # Outcomes
      "PROM_GRAL","ASISTENCIA","SIT_FIN","SIT_FIN_R"
    ))) %>%
    group_by(RBD, COD_GRADO) %>% 
    mutate(
      school_grade_avg_GPA = mean(PROM_GRAL[PROM_GRAL != 0], na.rm = TRUE),
      school_grade_sd_GPA  = sd(PROM_GRAL[PROM_GRAL != 0], na.rm = TRUE),
      
      school_grade_avg_ATT = mean(ASISTENCIA[ASISTENCIA != 0], na.rm = TRUE),
      school_grade_sd_ATT  = sd(ASISTENCIA[ASISTENCIA != 0], na.rm = TRUE),
      
      # dplyr::percent_rank() returns values in [0,1]; singleton groups give NaN
      pctl_school_grade = if (n() == 1) 0.5 else dplyr::percent_rank(PROM_GRAL),
      pctl_school_grade = round(100 * pctl_school_grade, 1)  # 0–100 scale
    ) %>%
    ungroup() %>% 
    # Prevent silent mismatches in `%in%` due to types
    mutate(MRUN = as.character(MRUN)) %>%
    filter(MRUN %in% as.character(stats::na.omit(sample_students_unique)))
  
  gc()
  
  
  #Coverage rate:
  message(paste0("Fraction of students tracked in year ", year, ": ", round(sum(sample_students_unique %in% tracking_year$MRUN)/length(sample_students_unique), 3)))
  
  return(tracking_year)
  
}


read_matricula_data <- function(year) {
  
  folder_path <- paste0("./data/raw/student_tracking/", year, "/Rendimiento-", year)
  
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("❌ No CSV files found in folder: ", folder_path)
  } else if (length(csv_files) > 1) {
    stop("❌ Multiple CSV files found in folder: ", paste(csv_files, collapse = ", "))
  }
  
  
  
  matricula_year <-   read_csv2(csv_files[1])
  names(matricula_year) <- toupper(names(matricula_year))
  
  message("✅ Successfully read: ", csv_files[1])
  
  matricula_year <- matricula_year %>%
    filter(!is.na(SIT_FIN)) %>%
    select(dplyr::any_of(c(
      # Year
      "AGNO",
      # School vars
      "RBD","NOM_RBD","COD_REG_RBD","COD_PRO_RBD","COD_COM_RBD","NOM_COM_RBD",
      "COD_DEPE","COD_DEPE2","RURAL_RBD",
      # Program/level
      "COD_ENSE","COD_ENSE2",
      # Grade vars
      "COD_GRADO","LET_CUR","COD_JOR",
      # Student vars
      "MRUN","GEN_ALU","FEC_NAC_ALU","EDAD_ALU", # EDAD_ALU will be kept only if present
      "COD_COM_ALU","NOM_COM_ALU", "COD_REG_ALU",
      # Outcomes
      "PROM_GRAL","ASISTENCIA","SIT_FIN","SIT_FIN_R"
    ))) %>%
    group_by(RBD, COD_GRADO) %>% 
    mutate(
      school_grade_avg_GPA = mean(PROM_GRAL[PROM_GRAL != 0], na.rm = TRUE),
      school_grade_sd_GPA  = sd(PROM_GRAL[PROM_GRAL != 0], na.rm = TRUE),
      
      school_grade_avg_ATT = mean(ASISTENCIA[ASISTENCIA != 0], na.rm = TRUE),
      school_grade_sd_ATT  = sd(ASISTENCIA[ASISTENCIA != 0], na.rm = TRUE),
      
      # dplyr::percent_rank() returns values in [0,1]; singleton groups give NaN
      pctl_school_grade = if (n() == 1) 0.5 else dplyr::percent_rank(PROM_GRAL),
      pctl_school_grade = round(100 * pctl_school_grade, 1)  # 0–100 scale
    ) %>%
    ungroup() %>% 
    # Prevent silent mismatches in `%in%` due to types
    mutate(MRUN = as.character(MRUN)) 
 #   filter(MRUN %in% as.character(stats::na.omit(sample_students_unique)))
  
  gc()
  
  
  #Coverage rate:
#  message(paste0("Fraction of students tracked in year ", year, ": ", round(sum(sample_students_unique %in% tracking_year$MRUN)/length(sample_students_unique), 3)))
  
  return(matricula_year)
  
}


students_sae2020 <- sample_students %>% 
  filter(proceso == 2020) %>% 
  pull(mrun) %>% 
  unique()
x


round(sum(students_sae2020 %in% tracking_year$MRUN)/length(students_sae2020), 3))


#years pre SAE
tracking_2013 <- read_tracking_data(2013)
tracking_2014 <- read_tracking_data(2014)
tracking_2015 <- read_tracking_data(2015)
tracking_2016 <- read_tracking_data(2016)

#during SAE years
tracking_2017 <- read_tracking_data(2017)
tracking_2018 <- read_tracking_data(2018)
tracking_2019 <- read_tracking_data(2019)

#Post SAE years
tracking_2020 <- read_tracking_data(2020)
tracking_2021 <- read_tracking_data(2021)
tracking_2022 <- read_tracking_data(2022)
tracking_2023 <- read_tracking_data(2023)
tracking_2024 <- read_tracking_data(2024)


#test2016 <- tracking_2016 %>% 
#  filter(COD_GRADO == 8) %>% 
#  mutate(mrun = as.double(MRUN)) %>% 
#  mutate(in_sae_app_2017 = (mrun %in% sae_apps_2017$mrun))

#test2017 <-tracking_2017 %>% 
#  filter(COD_GRADO == 8) %>% 
#  mutate(mrun = as.double(MRUN)) %>% 
#  mutate(in_sae_app_2017 = (mrun %in% sae_apps_2017$mrun))

table(mat_2022$in_st_school)

mat_2022<-  read_matricula_data(2022) %>% 
  mutate(in_st_school = as.numeric(RBD %in% sample_cursos$rbd))

mat_2021<-  read_matricula_data(2020) %>% 
  mutate(in_st_school = as.numeric(RBD %in% sample_cursos$rbd))

mat_2020<-  read_matricula_data(2020)%>% 
  mutate(in_st_school = as.numeric(RBD %in% sample_cursos$rbd))


table(mat_2020$COD_GRADO, mat_2020$in_st_school)
  
#Q1: How many students of mat_2021 are in my sample ?
table(mat_2022$COD_GRADO)
table(mat_2022$COD_ENSE2)

media_2022 <- mat_2022 %>% 
  filter(COD_ENSE2 >= 5) %>% 
#  filter(COD_GRADO == 4) %>% 
 mutate(in_st_school = RBD %in% sample_cursos$rbd)


seniors_2022 <- mat_2022 %>% 
  filter(COD_ENSE2 >= 5) %>% 
  filter(COD_GRADO == 4) %>% 
mutate(in_st_school = RBD %in% sample_cursos$rbd)

seniors_2021 <- mat_2021 %>% 
  filter(COD_ENSE2 >= 5) %>% 
  filter(COD_GRADO == 4) 


fresh_2021 <- mat_2021 %>% 
  filter(COD_ENSE2 >= 5) %>% 
  filter(COD_GRADO == 1) 

prop(table(fresh_2021$in_st_school))


media_2020 <- mat_2020 %>% 
  filter(COD_ENSE2 >= 5) %>% 
#  filter(COD_GRADO == 4) %>% 
  mutate(in_st_school = RBD %in% sample_cursos$rbd)


seniors_2020 <- mat_2020 %>% 
  filter(COD_ENSE2 >= 5) %>% 
  filter(COD_GRADO == 4) %>% 
mutate(in_st_school = RBD %in% sample_cursos$rbd)


prop.table(table(seniors_2021$in_st_school))
prop.table(table(seniors_2022$in_st_school))
prop.table(table(seniors_2020$in_st_school))

prop.table(table(seniors_2021$in_st_school))
prop.table(table(seniors_2022$in_st_school))
prop.table(table(mat_2020$in_st_school))



sample_students_unique_2017 <- sample_students %>% 
  filter(proceso == 2017) %>% 
  pull(mrun) %>% 
  unique()

length(sample_students_unique_2017) / nrow(seniors_2021)
length(sample_students_unique_2017) / nrow(seniors_2022)

sum(sample_students_unique_2017 %in% seniors_2022$MRUN)/length(sample_students_unique_2017)
sum(sample_students_unique_2017 %in% seniors_2021$MRUN)/length(sample_students_unique_2017)
sum(sample_students_unique_2017 %in% seniors_2020$MRUN)/length(sample_students_unique_2017)

sum(sample_students_unique_2017 %in% mat_2022$MRUN)/length(sample_students_unique_2017)
sum(sample_students_unique_2017 %in% mat_2021$MRUN)/length(sample_students_unique_2017)
sum(sample_students_unique_2017 %in% mat_2020$MRUN)/length(sample_students_unique_2017)



#Putting them all together
years <- 2024:2013  # adjust as needed

tracking_all <- do.call(
  bind_rows,
  lapply(years, function(y) get(paste0("tracking_", y)))
)


do.call(
  rm,
  lapply(years, function(y) paste0("tracking_", y))
)


save( tracking_all, file = "./data/clean/tracking_all.RData")

