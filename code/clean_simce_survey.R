####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)
library(readxl)


#Simce 4to basico

cpad_var_name <- function(year, var) {
  dplyr::case_when(
    var == "income" & year == 2013 ~ "cpad_p09",
    var == "income" & year == 2014 ~ "cpad_p06",
    var == "income" & year %in% c(2015, 2016) ~ "cpad_p10",
    var == "father_educ" & year == 2014 ~ "cpad_p04",
    var == "mother_educ" & year == 2014 ~ "cpad_p05",
    var == "father_educ" & year %in% c(2013, 2015, 2016) ~ "cpad_p07",
    var == "mother_educ" & year %in% c(2013, 2015, 2016) ~ "cpad_p08",
    var == "father_indigenous" & year == 2014 ~ "cpad_p07_01",
    var == "mother_indigenous" & year == 2014 ~ "cpad_p07_02",
    var == "father_indigenous" & year %in% c(2015, 2016) ~ "cpad_p09_01",
    var == "mother_indigenous" & year %in% c(2015, 2016) ~ "cpad_p09_02",
    var == "sala_cuna" & year == 2013 ~ "cpad_p10",
    var == "jardin" & year == 2013 ~ "cpad_p11",
    var == "prekinder" & year == 2013 ~ "cpad_p12",
    var == "kinder" & year == 2013 ~ "cpad_p13",
    var == "sala_cuna" & year == 2014 ~ "cpad_p08",
    var == "jardin" & year == 2014 ~ "cpad_p09",
    var == "prekinder" & year == 2014 ~ "cpad_p10",
    var == "kinder" & year == 2014 ~ "cpad_p11",
    var == "sala_cuna" & year %in% c(2015, 2016) ~ "cpad_p11",
    var == "jardin" & year %in% c(2015, 2016) ~ "cpad_p12",
    var == "prekinder" & year %in% c(2015, 2016) ~ "cpad_p13",
    var == "kinder" & year %in% c(2015, 2016) ~ "cpad_p14",
    TRUE ~ NA_character_
  )
}

get_cpad_var <- function(data, year, var) {
  col <- cpad_var_name(year, var)
  if (is.na(col) || !(col %in% names(data))) {
    return(rep(NA_real_, nrow(data)))
  }
  as.numeric(data[[col]])
}

clean_yes_no <- function(x) {
  dplyr::case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}

clean_parent_education_years <- function(x, year) {
  # Codes are year-specific in 2014: the first category is 1st grade
  # rather than "no schooling". Post-secondary categories are approximate.
  if (year == 2014) {
    return(dplyr::case_when(
      x %in% 1:8 ~ as.numeric(x),
      x == 9 ~ 9,
      x == 10 ~ 10,
      x == 11 ~ 11,
      x %in% c(12, 13) ~ 12,
      x == 14 ~ 13,
      x %in% c(15, 16) ~ 14,
      x == 17 ~ 16,
      x == 18 ~ 18,
      x == 19 ~ 21,
      TRUE ~ NA_real_
    ))
  }

  dplyr::case_when(
    x == 1 ~ 0,
    x %in% 2:9 ~ as.numeric(x - 1),
    x == 10 ~ 9,
    x == 11 ~ 10,
    x == 12 ~ 11,
    x %in% c(13, 14) ~ 12,
    x == 15 ~ 13,
    x %in% c(16, 17) ~ 14,
    x == 18 ~ 16,
    x == 19 ~ 18,
    x == 20 ~ 21,
    TRUE ~ NA_real_
  )
}

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

  income_code <- cpad_var_name(year, "income")
  
  if (is.na(income_code)) {
    stop("❌ No income_code mapping for year = ", year)
  }
  
  cpad_raw <- readxl::read_xlsx(xlsx_files[[1]])

  cpad_raw %>%
    dplyr::mutate(
      income = .data[[income_code]],
      simce_year = agno,
      father_educ = get_cpad_var(cpad_raw, year, "father_educ"),
      mother_educ = get_cpad_var(cpad_raw, year, "mother_educ"),
      father_educ_years = clean_parent_education_years(father_educ, year),
      mother_educ_years = clean_parent_education_years(mother_educ, year),
      father_indigenous = clean_yes_no(
        get_cpad_var(cpad_raw, year, "father_indigenous")
      ),
      mother_indigenous = clean_yes_no(
        get_cpad_var(cpad_raw, year, "mother_indigenous")
      ),
      sala_cuna = clean_yes_no(get_cpad_var(cpad_raw, year, "sala_cuna")),
      jardin = clean_yes_no(get_cpad_var(cpad_raw, year, "jardin")),
      prekinder = clean_yes_no(get_cpad_var(cpad_raw, year, "prekinder")),
      kinder = clean_yes_no(get_cpad_var(cpad_raw, year, "kinder"))
    ) %>%
    dplyr::select(
      idalumno,
      income,
      simce_year,
      father_educ,
      mother_educ,
      father_educ_years,
      mother_educ_years,
      father_indigenous,
      mother_indigenous,
      sala_cuna,
      jardin,
      prekinder,
      kinder
    ) %>%
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
      simce_rbd_4to = rbd,
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
  mutate(z_sim_mat_4to  = as.numeric(scale(ptje_mate4b_alu)), 
         z_sim_leng_4to = as.numeric(scale(ptje_lect4b_alu)))

sum(is.na(simce_2013$ptje_lect4b_alu))
sum(is.na(simce_2013$ptje_mate4b_alu))
sum(is.na(simce_2013$income_mid))

simce_2014 <- left_join(simce_alu_2014, cpad_2014, by = c("idalumno", "simce_year")) %>% 
mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
       ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = as.numeric(scale(ptje_mate4b_alu)), 
         z_sim_leng_4to = as.numeric(scale(ptje_lect4b_alu)))

sum(is.na(simce_2014$ptje_lect4b_alu))
sum(is.na(simce_2014$ptje_mate4b_alu))
sum(is.na(simce_2014$income_mid))

simce_2015 <- left_join(simce_alu_2015, cpad_2015, by = c("idalumno", "simce_year")) %>% 
mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
       ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = as.numeric(scale(ptje_mate4b_alu)), 
         z_sim_leng_4to = as.numeric(scale(ptje_lect4b_alu)))

sum(is.na(simce_2015$ptje_lect4b_alu))
sum(is.na(simce_2015$ptje_mate4b_alu))
sum(is.na(simce_2015$income_mid))


simce_2016 <- left_join(simce_alu_2016, cpad_2016, by = c("idalumno", "simce_year")) %>% 
  mutate(ptje_mate4b_alu = as.numeric(ptje_mate4b_alu),
       ptje_lect4b_alu = as.numeric(ptje_lect4b_alu)) %>% 
  mutate(z_sim_mat_4to  = as.numeric(scale(ptje_mate4b_alu)), 
         z_sim_leng_4to = as.numeric(scale(ptje_lect4b_alu)))

sum(is.na(simce_2016$ptje_lect4b_alu))
sum(is.na(simce_2016$ptje_mate4b_alu))
sum(is.na(simce_2016$income_mid))

#### TODO: Some imputation, maybe based on school avg or median; for simce I can include GPA?

simce_4to <- rbind(simce_2013, 
                   simce_2014, 
                   simce_2015, 
                   simce_2016) %>% 
          mutate(income_decile = ntile(income_mid, 10))

#First year simce only
simce_4to <- simce_4to %>% 
     select(-idalumno)  %>% 
     group_by(mrun) %>% 
     filter(simce_year == min(simce_year)) %>% 
     ungroup()

#Drop simce if none of them have scores. If any scores, pick highest avg 
simce_4to <- simce_4to %>%
  group_by(mrun) %>%
  mutate(
    all_missing = all(is.na(ptje_lect4b_alu) & is.na(ptje_mate4b_alu)),
    avg_score = rowMeans(cbind(ptje_lect4b_alu, ptje_mate4b_alu), na.rm = TRUE)
  ) %>%
  filter(!all_missing) %>%
  slice_max(avg_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-all_missing, -avg_score)


save( simce_4to, file = "./data/clean/simce_4to.Rdata")
