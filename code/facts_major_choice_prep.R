####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


dollar_clp_conversion <- 913

library(tidyverse)


load("./data/clean/psu_students.RData")
load("./data/clean/psu_applications.RData")


#There are still some differences per year on score scales
students_apps %>% 
  group_by(year) %>% 
  group_split() %>% 
  lapply(summary)


##### Field data on each program coming from enrollment data 

load_program_info <- function(year_input) {
  
  file_path <- paste0(
    "./data/raw/", year_input,
    "/Matricula-Ed-Superior-", year_input,
    "/20250729_Matrícula_Ed_Superior_", year_input,
    "_PUBL_MRUN.csv"
  )
  
  read_csv2(file_path,
            locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
    
    rename_with(~ toupper(.x)) %>%
    
    select(CODIGO_UNICO, 
           TIPO_INST_1, TIPO_INST_2 , TIPO_INST_3, COD_INST, NOMB_INST,
           COD_SEDE, NOMB_SEDE,
           COD_CARRERA, NOMB_CARRERA, 
           DUR_TOTAL_CARR,
           REGION_SEDE, PROVINCIA_SEDE, COMUNA_SEDE, 
           NIVEL_GLOBAL,
           NIVEL_CARRERA_1, NIVEL_CARRERA_2,
           CODIGO_DEMRE,
           AREA_CONOCIMIENTO,
           CINE_F_97_AREA_AREA, CINE_F_97_SUBAREA, 
           AREA_CARRERA_GENERICA, 
           CINE_F_13_AREA, CINE_F_13_SUBAREA, 
           ACREDITADA_CARR, ACREDITADA_INST
    ) %>%
    
    distinct() %>%
    
    filter(NIVEL_CARRERA_2 %in% c("Carreras Profesionales", 
                                  "Carreras Técnicas")) %>%
    

    
    rename(COD_SIES = CODIGO_UNICO) %>%
    
    select(COD_SIES, COD_CARRERA, CODIGO_DEMRE, 
           NOMB_CARRERA, TIPO_INST_1, NOMB_INST, COD_INST,
           AREA_CARRERA_GENERICA,
           REGION_SEDE, PROVINCIA_SEDE, COMUNA_SEDE, 
           AREA_CONOCIMIENTO, CINE_F_13_AREA,
           ACREDITADA_CARR, ACREDITADA_INST) %>%
    
    mutate(
      field_merged = case_when(
        CINE_F_13_AREA %in% c(
          "Ingeniería, Industria y Construcción",
          "Ciencias naturales, matemáticas y estadística",
          "Tecnología de la Información y la Comunicación (TIC)"
        ) ~ "STEM, non health",
        TRUE ~ CINE_F_13_AREA
      ),
      year_info = year_input,
      #   science1    = as.integer(AREA_CONOCIMIENTO == "Ciencias Básicas"),
      #    technology1 = as.integer(AREA_CONOCIMIENTO == "Tecnología"),
      #    health1     = as.integer(AREA_CONOCIMIENTO == "Salud"),
      #   engineer2   = as.integer(CINE_F_13_AREA == "Ingeniería, Industria y Construcción"),
      #  science2    = as.integer(CINE_F_13_AREA == "Ciencias naturales, matemáticas y estadística"),
      #  technology2 = as.integer(CINE_F_13_AREA == "Tecnología de la Información y la Comunicación (TIC)"),
      stem_bin   = as.integer(field_merged == "STEM, non health"),
      health_bin = as.integer(field_merged == "Salud y Bienestar")
        )
  
}

#program_info_2022 <- load_program_info(2022)
#program_info_2023 <- load_program_info(2023)
#program_info_2024 <- load_program_info(2024)

program_info_all <- bind_rows(
  load_program_info(2022),
  load_program_info(2023),
  load_program_info(2024)
) %>% 
  group_by(COD_SIES) %>% 
  filter(year_info == max(year_info)) %>% 
  ungroup() 

saveRDS(program_info_all, "./data/clean/program_info_22-24.rds")

program_info_all <- readRDS("./data/clean/program_info_22-24.rds")


program_info_all %>% 
  filter(CODIGO_DEMRE == 0) %>% 
  View()


#DB above only has COD_SIES e.g. (I70S1C3J1V2) and not COD_CARRERA (e.g 11001)

map_codes_2024 <- read_csv2("./data/raw/2024/PAES-2024-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2024_REV.csv") %>% 
  select(COD_CARRERA, COD_SIES,
         NEM, RANKING, CLEC, M1, HSCO, CIEN, M2) %>% 
  rename(COD_CARRERA_PREF = COD_CARRERA) %>% 
  mutate(year_info = 2024)

map_codes_2025 <- read_csv("./data/raw/2025/PAES-2025-Oferta-Definitiva-Programas/OFERTA_DEFINITIVA_PROGRAMAS_PAES_2025.csv") %>% 
  select(COD_CARRERA, COD_SIES,
         NEM, RANKING, CLEC, M1, HSCO, CIEN, M2) %>% 
  rename(COD_CARRERA_PREF = COD_CARRERA) %>% 
  mutate(year_info = 2025)

map_codes_all<- bind_rows(
  map_codes_2024,
  map_codes_2025
) %>% 
  group_by(COD_CARRERA_PREF) %>% 
  filter(year_info == max(year_info)) %>% 
  ungroup() 


saveRDS(map_codes_all, "./data/clean/oferta_codes_24_25.rds")
map_codes_all <- readRDS ("./data/clean/oferta_codes_24_25.rds")


apps_field <- college_apps %>%  
  left_join(map_codes_all, by = "COD_CARRERA_PREF") %>% 
  left_join(program_info_all, by = "COD_SIES") %>% 
 #filter(ORDEN_PREF <= 10) %>%
 filter(TIPO_PREF == "REGULAR") %>% 
  group_by(mrun) %>%
  #Only first application per person
  filter(year == min(year)) %>% 
  ungroup()


#122 programs with missing data for fields out of the 2233 unique programs people apply to 
#130k apps out of 6.56 M is like a 2%
#Weird that it happens, but I guess a trivial fraction all in all.

apps_field %>% 
  filter(is.na(COD_SIES)) %>% 
#  pull(year) %>% 
#  table()
 select(COD_CARRERA_PREF) %>% 
 unique() %>% 
  View()  


mapping_check <- apps_field %>%
  distinct(AREA_CARRERA_GENERICA, field_merged) %>%
  group_by(AREA_CARRERA_GENERICA) %>%
  summarise(n_fields = n()) %>%
  filter(n_fields > 1)


mapping_check


apps_field %>% 
  distinct(NOMB_CARRERA,AREA_CARRERA_GENERICA, field_merged) %>%
  filter(AREA_CARRERA_GENERICA %in% mapping_check$AREA_CARRERA_GENERICA)

#These might be up for reclassification.
#BIOINGENIERIA MEDICA sounds a bit healthy. 
#INGENIERIA CIVIL EN TRANSPORTE podria no ser Servicios.



tab_apps_p_person <- apps_field %>% 
  group_by(mrun, year) %>% 
  summarize(n_apps_person = max(ORDEN_PREF)) %>% 
  ungroup() 

table(tab_apps_p_person$year, tab_apps_p_person$n_apps_person)

#Finding: Until 2022 people could only rank up to 10 programs.
#Since 2023 people rank up to 20. 

tab_apps_p_person %>% 
  filter(year >= 2023) %>% 
  pull(n_apps_person) %>% 
  table() %>% 
  prop.table() %>% 
  cumsum()

#74% rank 10 choices or less after 2023; 85% rank up 14 or less. 
#Let me stay with 10. 

apps_top10 <- apps_field %>%
  filter(ORDEN_PREF <= 10)

apps_top10 %>%
  distinct(mrun, field_merged) %>% 
  group_by(mrun) %>%
  summarise(n_fields = n_distinct(field_merged)) %>%
  summarise(
    prop_single_fields = sum(n_fields == 1)/sum(n_fields >= 1), 
    prop_2_fields      = sum(n_fields == 2)/sum(n_fields >= 1), 
    prop_3_fields      = sum(n_fields == 3)/sum(n_fields >= 1), 
    mean_fields = mean(n_fields)  
    )



#46% apply to only  1 fields in the first 10 apps.
#76% apply to up to 2 fields in the first 10 apps.
#91% apply to up to 3 fields in the first 10 apps.



apps_top10 %>%
  distinct(mrun, AREA_CARRERA_GENERICA) %>% 
  group_by(mrun) %>%
  summarise(n_majors = n_distinct(AREA_CARRERA_GENERICA)) %>%
  summarise(
    prop_single_majors = sum(n_majors == 1)/sum(n_majors >= 1), 
    prop_2_majors      = sum(n_majors == 2)/sum(n_majors >= 1), 
    prop_3_majors      = sum(n_majors == 3)/sum(n_majors >= 1), 
    prop_4_majors      = sum(n_majors == 4)/sum(n_majors >= 1), 
    prop_5_majors      = sum(n_majors == 5)/sum(n_majors >= 1), 
    mean_majors        = mean(n_majors)
  )

#25% apply to        1 majors in the first 10 apps.
#47% apply to up to  2 majors in the first 10 apps.
#65% apply to up to  3 majors in the first 10 apps.
#78% apply to up to  4 majors in the first 10 apps.
#47% apply to up to  5 majors in the first 10 apps.



#Chat code: 




# --- 2) Person x Field binary matrix ---
field_wide <- apps_top10 %>% 
  distinct(mrun, field_merged) %>% 
  mutate(v = 1L) %>%
  pivot_wider(
    names_from  = field_merged,
    values_from = v,
    values_fill = 0L
  )

field_mat <- as.matrix(field_wide[, -1, drop = FALSE])  # drop MRUN

# --- 3) Co-occurrence counts ---
co_occ <- t(field_mat) %*% field_mat
diag_counts <- diag(co_occ)

# --- 4) Jaccard similarity: |A∩B| / |A∪B| ---
union_mat <- outer(diag_counts, diag_counts, "+") - co_occ
jaccard <- co_occ / union_mat
diag(jaccard) <- 1

# --- 5) Extract UNIQUE field pairs (upper triangle only, no diagonal) ---
rn <- rownames(jaccard)
cn <- colnames(jaccard)

upper_idx <- which(upper.tri(jaccard), arr.ind = TRUE)

pairs <- tibble(
  field_a = rn[upper_idx[, "row"]],
  field_b = cn[upper_idx[, "col"]],
  jaccard = jaccard[upper_idx],
  both    = co_occ[upper_idx],
  n_a     = diag_counts[upper_idx[, "row"]],
  n_b     = diag_counts[upper_idx[, "col"]]
) %>%
  arrange(desc(jaccard))

print(head(pairs, 30))  # top 30 candidate merges
View(pairs)

# --- 6) Heatmap (optional; can be slow if many fields) ---
heat_df <- as.data.frame(as.table(jaccard))
colnames(heat_df) <- c("field_a", "field_b", "jaccard")

ggplot(heat_df, aes(field_a, field_b, fill = jaccard)) +
  geom_tile() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Field Co-Application Similarity (Jaccard), Top 10 Preferences",
    x = "field_merged",
    y =  "field_merged",
    fill = "Jaccard"
  )


################ Get Cutoff scores



cutoff_scores <- college_apps %>% 
#solo seleccionados and regular admission
    filter(ESTADO_PREF == 24) %>%  
      group_by(COD_CARRERA_PREF, year, TIPO_PREF) %>% 
    filter(PTJE_PREF == min(PTJE_PREF))  %>% 
    ungroup() %>% 
    select(year, COD_CARRERA_PREF, PTJE_PREF, TIPO_PREF) %>% 
    rename(PTJE_corte = PTJE_PREF)
  
    
saveRDS(cutoff_scores, "./data/clean/cutoff_scores.rds")




################ Check Spatial data 

library(sf)

comunas_info <- read_sf("./data/raw/spatial/chl_admin_boundaries/chl_admin3.shp") %>% 
  select(adm3_name, adm3_pcode, center_lat, center_lon) %>%
  rename(CODIGO_COMUNA = adm3_pcode,
         NOMBRE_COMUNA = adm3_name,
         LAT = center_lat,
         LON = center_lon)  %>% 
    mutate(
      CODIGO_COMUNA = str_remove(CODIGO_COMUNA, "^CL")
         )

comunas_info <- st_set_geometry(comunas_info, NULL)
         
saveRDS(comunas_info, "./data/clean/comuna_info.RDS")




