####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


#universe of students: 
#everyone in grado 8 in the years 2017 (would belong to SAE 2017) #psu in 2021 (proceso/mat 2022)
#everyone in grado 8 in the years 2018 ("" SAE 2018) (proceso/mat 2023)
#everyone in grado 8 in the years 2019 ("" SAE 2019)(proceso/mat 2024)
#everyone in grado 8 in the years 2020 ("" SAE 2020)  (proceso/mat 2025)


#I restricted back to having a SIT_FIN, but I think this avoids repetitions.

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

  

mat_2017_gr8 <-  read_matricula_data(2017) %>% 
  filter(COD_GRADO == 8) %>% 
  select(MRUN, AGNO,
         GEN_ALU, FEC_NAC_ALU, EDAD_ALU, # EDAD_ALU will be kept only if present
         COD_COM_ALU,NOM_COM_ALU, COD_REG_ALU)


mat_2018_gr8 <-  read_matricula_data(2018) %>% 
  filter(COD_GRADO == 8) %>% 
  select(MRUN, AGNO,
         GEN_ALU, FEC_NAC_ALU, EDAD_ALU, # EDAD_ALU will be kept only if present
         COD_COM_ALU,NOM_COM_ALU, COD_REG_ALU)


mat_2019_gr8 <-  read_matricula_data(2019) %>% 
  filter(COD_GRADO == 8) %>% 
  select(MRUN, AGNO,
         GEN_ALU, FEC_NAC_ALU, EDAD_ALU, # EDAD_ALU will be kept only if present
         COD_COM_ALU,NOM_COM_ALU, COD_REG_ALU)


mat_2020_gr8 <-  read_matricula_data(2020) %>% 
  filter(COD_GRADO == 8) %>% 
  select(MRUN, AGNO,
         GEN_ALU, FEC_NAC_ALU, EDAD_ALU, # EDAD_ALU will be kept only if present
         COD_COM_ALU,NOM_COM_ALU, COD_REG_ALU)

#rbind
mat_gr8 <- rbind(mat_2017_gr8,
                 mat_2018_gr8,
                 mat_2019_gr8,
                 mat_2020_gr8)

#Out of the 1million of rows, 105k are repeated.
first_8gr <- mat_gr8 %>% 
  select(MRUN, AGNO) %>% 
  group_by(MRUN) %>% 
  filter(AGNO == min (AGNO))  %>% 
  ungroup() %>% 
  distinct() %>% 
  rename(cohort_gr8 = AGNO) 

write.csv(first_8gr, "data/clean/universe.csv")

#Final N = 944.510

first_8gr_controls <- mat_gr8 %>% 
  group_by(MRUN) %>% 
  filter(AGNO == min (AGNO))  %>% 
  ungroup() %>% 
  distinct() %>% 
  rename(cohort_gr8 = AGNO) 

write.csv(first_8gr_controls, "data/clean/universe_controls.csv")




####Assign a school to each of these kids. 

universe_MRUN <- first_8gr  %>% 
  pull(MRUN)

#Just changed the final filter to universe_MRUN
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
    filter(MRUN %in% as.character(stats::na.omit(universe_MRUN)))
  
  gc()
  
  
  #Coverage rate:
  message(paste0("Fraction of students tracked in year ", year, ": ", round(sum(universe_MRUN %in% tracking_year$MRUN)/length(universe_MRUN), 3)))
  
  return(tracking_year)
  
}



tracking_2017 <- read_tracking_data(2017)
tracking_2018 <- read_tracking_data(2018)
tracking_2019 <- read_tracking_data(2019)
tracking_2020 <- read_tracking_data(2020)
tracking_2021 <- read_tracking_data(2021)
tracking_2022 <- read_tracking_data(2022)
tracking_2023 <- read_tracking_data(2023)
tracking_2024 <- read_tracking_data(2024)


years <- 2017:2024  # adjust as needed

tracking_all <- do.call(
  bind_rows,
  lapply(years, function(y) get(paste0("tracking_", y)))
)



save( tracking_all, file = "./data/clean/tracking_univ8gr.RData")


