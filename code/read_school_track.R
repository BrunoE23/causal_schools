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


repeated_students_in_sample <- students_small %>% 
  group_by(MRUN) %>% 
  mutate(n_apps_in_sample = n()) %>% 
  arrange(desc(n_apps_in_sample), MRUN)   %>%  
  ungroup()

repeated_students_in_sample %>% 
  select(MRUN, n_apps_in_sample) %>% 
  unique() %>% 
  pull(n_apps_in_sample) %>% 
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
      "COD_COM_ALU","NOM_COM_ALU",
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

