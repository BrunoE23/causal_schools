####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)
library(readxl)


#Simce 4to basico

#Parent survey
read_simce4b_cpad <- function(year) {
  
  folder_path <- paste0(
    "./data/raw/simce/Simce cuarto básico ",
    year,
    " - Versión privada/Archivos XLS (XLSX)"
  )
  
  if (!dir.exists(folder_path)) {
    stop("❌ Folder not found: ", folder_path)
  }
  
  # Find the CPAD file for that year (tweak pattern if your naming differs)
  xlsx_files <- list.files(
    path = folder_path,
    pattern = paste0("simce4b", year, ".*cpad.*\\.xlsx$"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(xlsx_files) == 0) {
    stop("❌ No CPAD XLSX files found in folder: ", folder_path)
  } else if (length(xlsx_files) > 1) {
    stop("❌ Multiple CPAD XLSX files found: ", paste(xlsx_files, collapse = ", "))
  }

  # pick the column name (string) for each year
  income_code <- dplyr::case_when(
    year == 2013 ~ "cpad_p09",
    year == 2014 ~ "cpad_p06",
    year == 2015 ~ "cpad_p10",
    year == 2016 ~ "cpad_p10",
    TRUE ~ NA_character_
  )
  
  if (is.na(income_code)) {
    stop("❌ No income_code mapping for year = ", year)
  }
  
  readxl::read_xlsx(xlsx_files[[1]]) %>%
    dplyr::mutate(
      income = .data[[income_code]],
      simce_year = agno
    ) %>%
    dplyr::select(idalumno, income, simce_year) %>% 
    dplyr::mutate(
      income_mid = dplyr::case_when(
        income == 1  ~  50000,
        income == 2  ~ 150000,
        income == 3  ~ 250000,
        income == 4  ~ 350000,
        income == 5  ~ 450000,
        income == 6  ~ 550000,
        income == 7  ~ 700000,
        income == 8  ~ 900000,
        income == 9  ~ 1100000,
        income == 10 ~ 1300000,
        income == 11 ~ 1500000,
        income == 12 ~ 1700000,
        income == 13 ~ 1900000,
        income == 14 ~ 2100000,
        income == 15 ~ 2300000,
        TRUE ~ NA_real_
      )
    )
  
  
}


cpad_2013 <- read_simce4b_cpad(2013)
cpad_2014 <- read_simce4b_cpad(2014)
cpad_2015 <- read_simce4b_cpad(2015)
cpad_2016 <- read_simce4b_cpad(2016)




# SIMCE 4B student-level scores
read_simce4b_alu <- function(year) {
  
  folder_path <- paste0(
    "./data/raw/simce/Simce cuarto básico ",
    year,
    " - Versión privada/Archivos XLS (XLSX)"
  )
  
  if (!dir.exists(folder_path)) {
    stop("❌ Folder not found: ", folder_path)
  }
  
  # Find the ALU MRUN file for that year
  xlsx_files <- list.files(
    path = folder_path,
    pattern = paste0("simce4b", year, ".*alu.*mrun.*\\.xlsx$"),
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(xlsx_files) == 0) {
    stop("❌ No ALU-MRUN XLSX files found in folder: ", folder_path)
  } else if (length(xlsx_files) > 1) {
    stop("❌ Multiple ALU-MRUN XLSX files found: ", paste(xlsx_files, collapse = ", "))
  }
  
  df <- readxl::read_xlsx(xlsx_files[[1]]) %>%
    dplyr::mutate(simce_year = agno)
  
  # If MRUN column name varies, standardize it (optional but helpful)
  if (!("mrun" %in% names(df)) && ("MRUN" %in% names(df))) {
    df <- df %>% dplyr::rename(mrun = MRUN)
  }
  

  df %>%
    dplyr::select(
      idalumno, mrun, simce_year,
      ptje_lect4b_alu,
      ptje_mate4b_alu,
    )
}



simce_alu_2013 <- read_simce4b_alu(2013)
simce_alu_2014 <- read_simce4b_alu(2014)
simce_alu_2015 <- read_simce4b_alu(2015)
simce_alu_2016 <- read_simce4b_alu(2016)


simce_2013 <- left_join(simce_alu_2013, cpad_2013, by = c("idalumno", "simce_year")) %>% 
  mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
         ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = scale(ptje_mate4b_alu), 
         z_sim_leng_4to = scale(ptje_lect4b_alu))

sum(is.na(simce_2013$ptje_lect4b_alu))
sum(is.na(simce_2013$ptje_mate4b_alu))
sum(is.na(simce_2013$income_mid))

simce_2014 <- left_join(simce_alu_2014, cpad_2014, by = c("idalumno", "simce_year")) %>% 
mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
       ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = scale(ptje_mate4b_alu), 
         z_sim_leng_4to = scale(ptje_lect4b_alu))

sum(is.na(simce_2014$ptje_lect4b_alu))
sum(is.na(simce_2014$ptje_mate4b_alu))
sum(is.na(simce_2014$income_mid))

simce_2015 <- left_join(simce_alu_2015, cpad_2015, by = c("idalumno", "simce_year")) %>% 
mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
       ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = scale(ptje_mate4b_alu), 
         z_sim_leng_4to = scale(ptje_lect4b_alu))

sum(is.na(simce_2015$ptje_lect4b_alu))
sum(is.na(simce_2015$ptje_mate4b_alu))
sum(is.na(simce_2015$income_mid))


simce_2016 <- left_join(simce_alu_2016, cpad_2016, by = c("idalumno", "simce_year")) %>% 
  mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
       ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = scale(ptje_mate4b_alu), 
         z_sim_leng_4to = scale(ptje_lect4b_alu))

sum(is.na(simce_2016$ptje_lect4b_alu))
sum(is.na(simce_2016$ptje_mate4b_alu))
sum(is.na(simce_2016$income_mid))

#### TODO: Some imputation, maybe based on school avg or median; for simce I can include GPA?

simce_4to <- rbind(simce_2013, 
                   simce_2014, 
                   simce_2015, 
                   simce_2016) %>% 
          mutate(income_decile = ntile(income_mid, 10))



save( simce_4to, file = "./data/clean/simce_4to.Rdata")
