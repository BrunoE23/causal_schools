####################################
find_existing_path <- function(candidates, label) {
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) == 0) {
    stop("Could not find ", label, ". Update candidates.", call. = FALSE)
  }

  candidates[[1]]
}

data_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Dropbox/causal_schools",
    "C:/Users/brunem/Dropbox/causal_schools"
  ),
  "data_wd"
)
code_output_wd <- find_existing_path(
  c(
    "C:/Users/xd-br/Desktop/PhD/Research/causal_schools",
    "C:/Users/brunem/Research/causal_schools"
  ),
  "code_output_wd"
)

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
})


# This currently uses full SIES Matricula-Ed-Superior 2022-2025. PAES
# Matricula 2026 is not a substitute for full SIES Matricula-Ed-Superior 2026.


#I am only gonna save students in their first year, 
#and thus keep a track of people who ever entered
#I can also add a variable for `timely entering`.


matricula_cols <- c(
  "MRUN", "ANIO_ING_CARR_ORI", "SEM_ING_CARR_ORI", "ANIO_ING_CARR_ACT",
  "FORMA_INGRESO", "NIVEL_CARRERA_2",
  "CODIGO_UNICO", "COD_CARRERA", "CODIGO_DEMRE",
  "NOMB_CARRERA", "TIPO_INST_1", "NOMB_INST", "COD_INST",
  "AREA_CARRERA_GENERICA",
  "REGION_SEDE", "PROVINCIA_SEDE", "COMUNA_SEDE",
  "CINE_F_97_AREA_AREA", "CINE_F_97_SUBAREA",
  "ACREDITADA_CARR", "ACREDITADA_INST", "ACRE_INST_ANIO"
)

read_matricula_cols <- function(file_path, requested_cols) {
  header <- names(fread(file_path, nrows = 0, encoding = "UTF-8"))
  lookup <- setNames(header, toupper(header))
  missing_cols <- setdiff(requested_cols, names(lookup))

  if (length(missing_cols) > 0) {
    stop(
      "Missing matricula columns in ", file_path, ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  fread(
    file_path,
    select = unname(lookup[requested_cols]),
    encoding = "UTF-8",
    showProgress = TRUE
  ) %>%
    rename_with(~ toupper(.x))
}

# Read data
load_student_mat_info <- function(year_input) {
  
  file_path <- paste0(
    "./data/raw/", year_input,
    "/Matricula-Ed-Superior-", year_input,
    "/20250729_Matrícula_Ed_Superior_", year_input,
    "_PUBL_MRUN.csv"
  )
  
  read_matricula_cols(file_path, matricula_cols) %>%
    filter(NIVEL_CARRERA_2 %in% c("Carreras Profesionales", 
                                  "Carreras Técnicas")) %>%
    
    rename(COD_SIES = CODIGO_UNICO) %>%
    
    select(
           #Student vars
           MRUN, ANIO_ING_CARR_ORI, SEM_ING_CARR_ORI, ANIO_ING_CARR_ACT, FORMA_INGRESO, 
           #Carrera vars
           COD_SIES, COD_CARRERA, CODIGO_DEMRE, 
           NOMB_CARRERA, TIPO_INST_1, NOMB_INST, COD_INST,
           AREA_CARRERA_GENERICA,
           REGION_SEDE, PROVINCIA_SEDE, COMUNA_SEDE, 
           CINE_F_97_AREA_AREA, CINE_F_97_SUBAREA, 
           ACREDITADA_CARR, ACREDITADA_INST, ACRE_INST_ANIO) %>%
    
    mutate(
      year_info = year_input,
      ACREDITADA_CARR = str_squish(ACREDITADA_CARR),
      ACREDITADA_INST = str_squish(ACREDITADA_INST),
      ACRE_INST_ANIO = as.numeric(ACRE_INST_ANIO),
      program_accredited = case_when(
        ACREDITADA_CARR == "ACREDITADA" ~ 1L,
        ACREDITADA_CARR == "NO ACREDITADA" ~ 0L,
        TRUE ~ NA_integer_
      ),
      institution_accredited = case_when(
        ACREDITADA_INST == "ACREDITADA" ~ 1L,
        ACREDITADA_INST %in% c("NO ACREDITADA", "BAJO TUTELA") ~ 0L,
        TRUE ~ NA_integer_
      ),
      program_certified_years = case_when(
        program_accredited == 1L ~ ACRE_INST_ANIO,
        program_accredited == 0L ~ 0,
        TRUE ~ NA_real_
      ),
      
        
      #my implementation of Campos et al. 2026
      field_reclassified = case_when(
        
        # Keep Veterinaria
        CINE_F_97_SUBAREA == "Veterinaria" ~ "Health and Welfare",
        
        # Drop remaining Agriculture and Services
        CINE_F_97_AREA_AREA %in% c("Agricultura", "Servicios") ~ NA_character_,
        
        # Science
        CINE_F_97_AREA_AREA == "Ciencias" ~ "Science",
        
        # Teaching
        CINE_F_97_AREA_AREA == "Educación" ~ "Teaching",
        
        # Humanities and Arts
        CINE_F_97_AREA_AREA == "Humanidades y Artes" ~ "Humanities and Arts",
        
        # Engineering
        CINE_F_97_AREA_AREA == "Ingeniería, Industria y Construcción" ~
          "Engineering, Manufacturing and Construction",
        
        # Social Sciences / Business / Law
        CINE_F_97_AREA_AREA == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
          CINE_F_97_SUBAREA == "Derecho" ~ "Law",
        
        CINE_F_97_AREA_AREA == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
          CINE_F_97_SUBAREA == "Enseñanza Comercial y Administración" ~ "Business",
        
        CINE_F_97_AREA_AREA == "Ciencias Sociales, Enseñanza Comercial y Derecho" &
          CINE_F_97_SUBAREA %in% c(
            "Ciencias Sociales y del Comportamiento",
            "Periodismo e Información"
          ) ~ "Social Sciences",
        
        # Medicine vs Health and Welfare
        CINE_F_97_AREA_AREA == "Salud y Servicios Sociales" &
          CINE_F_97_SUBAREA == "Medicina" ~ "Medicine",
        
        CINE_F_97_AREA_AREA == "Salud y Servicios Sociales" ~ "Health and Welfare",
        
        TRUE ~ NA_character_
      ),
      
      f_science   = as.integer(field_reclassified == "Science"),
      f_social    = as.integer(field_reclassified == "Social Sciences"),
      f_business  = as.integer(field_reclassified == "Business"),
      f_law       = as.integer(field_reclassified == "Law"),
      f_teaching  = as.integer(field_reclassified == "Teaching"),
      f_humarts   = as.integer(field_reclassified == "Humanities and Arts"),
      f_eng       = as.integer(field_reclassified == "Engineering, Manufacturing and Construction"),
      f_medicine  = as.integer(field_reclassified == "Medicine"),
      f_health    = as.integer(field_reclassified == "Health and Welfare")
    )
  
}


# rbind 
student_mat_all <- bind_rows(
  load_student_mat_info(2022),
  load_student_mat_info(2023),
  load_student_mat_info(2024),
  load_student_mat_info(2025),
)


#consider the earliest ingreso
student_mat_first <- student_mat_all %>% 
#ingresos only
    filter(year_info == ANIO_ING_CARR_ORI) %>% 
#if multiple ingresos get the first one 
    group_by(MRUN) %>% 
    arrange(ANIO_ING_CARR_ORI, SEM_ING_CARR_ORI, .by_group = TRUE) %>% 
  slice_head(n = 1) %>% 
  ungroup()
  
#And the last ingreso
student_mat_last <- student_mat_all %>% 
  #ingresos only
  filter(year_info == ANIO_ING_CARR_ORI) %>% 
  #if multiple ingresos get the last one 
  group_by(MRUN) %>% 
  arrange(ANIO_ING_CARR_ORI, SEM_ING_CARR_ORI, .by_group = TRUE) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

length(unique(student_mat_first$MRUN))


rm(student_mat_all)


#TODO: append mifuturo if easy 


# format 
student_mat_1st_small <- student_mat_first %>% 
    select(MRUN, year_info, 
           COD_SIES, COD_CARRERA, CODIGO_DEMRE, 
           NOMB_CARRERA, TIPO_INST_1, NOMB_INST, COD_INST,
           ACREDITADA_CARR, ACREDITADA_INST, ACRE_INST_ANIO,
           program_accredited, institution_accredited,
           program_certified_years,
           field_reclassified,
           #field binaries
           f_science ,
           f_social     ,
           f_business  ,
           f_law        ,
           f_teaching   ,
           f_humarts    ,
           f_eng        ,
           f_medicine ,
           f_health ) %>% 
  rename_with(~ paste0(., "_m1"), -MRUN) 
      



student_mat_last_small <- student_mat_last %>% 
  select(MRUN, year_info, 
         COD_SIES, COD_CARRERA, CODIGO_DEMRE, 
         NOMB_CARRERA, TIPO_INST_1, NOMB_INST, COD_INST,
         ACREDITADA_CARR, ACREDITADA_INST, ACRE_INST_ANIO,
         program_accredited, institution_accredited,
         program_certified_years,
         field_reclassified,
         #field binaries
         f_science ,
         f_social     ,
         f_business  ,
         f_law        ,
         f_teaching   ,
         f_humarts    ,
         f_eng        ,
         f_medicine ,
         f_health ) %>% 
  rename_with(~ paste0(., "_ml"), -MRUN)


#export
dir.create("./data/clean/mat_ingresos_22-25", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/clean/mat_ingresos_22-24", recursive = TRUE, showWarnings = FALSE)

write.csv(student_mat_1st_small,  "./data/clean/mat_ingresos_22-25/mat_1st_ing.csv", row.names = FALSE)
write.csv(student_mat_last_small, "./data/clean/mat_ingresos_22-25/mat_last_ing.csv", row.names = FALSE)

# Legacy alias: older scripts still point to this folder, but the content is
# 2022-2025 after the current SIES update.
write.csv(student_mat_1st_small,  "./data/clean/mat_ingresos_22-24/mat_1st_ing.csv", row.names = FALSE)
write.csv(student_mat_last_small, "./data/clean/mat_ingresos_22-24/mat_last_ing.csv", row.names = FALSE)


