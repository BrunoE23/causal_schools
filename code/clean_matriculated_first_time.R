####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


#I will do now matriculas 2022-2025 which encompass my first 3 cohorts
#I am waiting for matricula 2026 data to complete the 4 cohorts.


#I am only gonna save students in their first year, 
#and thus keep a track of people who ever entered
#I can also add a variable for `timely entering`.


# Read data
load_student_mat_info <- function(year_input) {
  
  file_path <- paste0(
    "./data/raw/", year_input,
    "/Matricula-Ed-Superior-", year_input,
    "/20250729_Matrícula_Ed_Superior_", year_input,
    "_PUBL_MRUN.csv"
  )
  
  read_csv2(file_path,
            locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
    
    rename_with(~ toupper(.x)) %>%
    

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
           ACREDITADA_CARR, ACREDITADA_INST) %>%
    
    mutate(
        year_info = year_input,
      
        
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
  load_student_mat_info(2024)
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
write.csv(student_mat_1st_small,  "./data/clean/mat_ingresos_22-24/mat_1st_ing.csv")
write.csv(student_mat_last_small, "./data/clean/mat_ingresos_22-24/mat_last_ing.csv")


