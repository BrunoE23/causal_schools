####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


setwd(data_wd)
#####################################

#IMPORTANT REMINDER: SAE 2017 takes place in 2017 and the spot allows you to
#get a spot in a different school in the academic year of 2018. 
#Obviously it is the same for other years.

#Kids in SAE 2017 are in grade 8 in 2017,
#Show up in grade              9 in 2018
#                              12 in 2021

#Kids in SAE 2020 are in grade 8 in 2020.
#Kids in SAE 2020 are in grade 9 in 2021.
#Kids in SAE 2020 are in grade 12 in 2025.

#packages
library(tidyverse)


#read spots available
#4 regions?
oferta_2017 <- read_csv2("./data/raw/2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  mutate(cod_sede = NA_integer_)

sum(oferta_2017$vacantes_regular)

#all country?
oferta_2018 <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) 

sum(oferta_2018$vacantes_regular)


oferta_2019 <- read_csv2("./data/raw/2019/SAE_2019/A1_Oferta_Establecimientos_etapa_regular_2019_Admisión_2020.csv") %>% 
  mutate(proceso = 2019) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) 

sum(oferta_2019$vacantes_regular)


oferta_2020 <- read_csv2("./data/raw/2020/SAE_2020/A1_Oferta_Establecimientos_etapa_regular_2020_Admisión_2021.csv") %>% 
  mutate(proceso = 2020) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) 



sum(oferta_2020$vacantes_regular)


sae_oferta <- rbind(oferta_2020, oferta_2019, oferta_2018, oferta_2017)
rm(oferta_2020, oferta_2019, oferta_2018, oferta_2017)

## Apps 

sae_apps_2017 <- read_csv2("./data/raw/2017/SAE_2017/C1_Postulaciones_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)

sae_apps_2018 <- read_csv2("./data/raw/2018/SAE_2018/C1_Postulaciones_etapa_regular_2018_Admisión_2019_PUBL.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd), "_",  cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)

sae_apps_2019 <- read_csv2("./data/raw/2019/SAE_2019/C1_Postulaciones_etapa_regular_2019_Admisión_2020_PUBL.csv") %>% 
  mutate(proceso = 2019) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)


sae_apps_2020 <- read_csv2("./data/raw/2020/SAE_2020/C1_Postulaciones_etapa_regular_2020_Admisión_2021_PUBL.csv") %>% 
  mutate(proceso = 2020) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)


#table(sae_apps_2019$rbd)
#table(sae_apps_2020$rbd)


#Filter out prioridad 
sae_apps <- rbind(sae_apps_2017, sae_apps_2018, sae_apps_2019,sae_apps_2020) %>% 
  filter(agregada_por_continuidad == 0,
         prioridad_matriculado == 0,
         prioridad_hermano == 0,
         prioridad_hijo_funcionario == 0,
         prioridad_exalumno == 0) %>% 
  select(-c(agregada_por_continuidad, 
            prioridad_matriculado, prioridad_hermano,
            prioridad_hijo_funcionario, prioridad_exalumno))


table(sae_apps$proceso)

rm(sae_apps_2017, sae_apps_2018, sae_apps_2019, sae_apps_2020 )

top_2_apps <- sae_apps %>%  
  filter(preferencia_postulante <= 2) %>%  
  select(proceso , mrun, preferencia_postulante, rbd, everything()) %>% 
  arrange(proceso, mrun, preferencia_postulante)  %>% 
  group_by(proceso, mrun) %>%  
  summarise(
    rbd_top2_code = paste(unique(rbd), collapse = "_"), 
    n_rbd         = length(unique(rbd)),
    rbd_1         = unique(rbd)[1],
    rbd_2         = unique(rbd)[2])    %>% 
    ungroup() 

top2lists <- top_2_apps %>% 
  filter(n_rbd > 1) %>% 
  group_by(rbd_top2_code) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


#length(unique(sae_apps$rbd))


#sae_apps %>%  
#  filter(preferencia_postulante <= 3) %>%  
#  filter(rbd %in% sample_rbd$rbd) %>% 
#  group_by(mrun)


## Student controls

sae_stud_info_2017 <- read_csv2("./data/raw/2017/SAE_2017/B1_Postulantes_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  select(-(c(cod_com,nom_com))) %>% 
  filter(cod_nivel== 9) %>% 
  mutate(proceso = 2017) 
  
sae_stud_info_2018 <- read_csv2("./data/raw/2018/SAE_2018/B1_Postulantes_etapa_regular_2018_Admisión_2019_PUBL.csv")  %>% 
  filter(cod_nivel== 9) %>% 
    mutate(proceso = 2018) 


sae_stud_info_2019 <- read_csv2("./data/raw/2019/SAE_2019/B1_Postulantes_etapa_regular_2019_Admisión_2020_PUBL.csv") %>% 
  filter(cod_nivel== 9) %>% 
  mutate(proceso = 2019) 

sae_stud_info_2020 <- read_csv2("./data/raw/2020/SAE_2020/B1_Postulantes_etapa_regular_2020_Admisión_2021_PUBL.csv") %>% 
  filter(cod_nivel== 9) %>% 
  mutate(proceso = 2020) 


sae_stud_info <- rbind(sae_stud_info_2017,
                       sae_stud_info_2018,
                       sae_stud_info_2019,
                       sae_stud_info_2020) %>% 
  select(-(cod_nivel))

rm(sae_stud_info_2017,
   sae_stud_info_2018,
   sae_stud_info_2019,
   sae_stud_info_2020)


#sae_apps <- left_join(sae_apps, sae_stud_info, by = "mrun", multiple = "all")

#sae_stud_info %>% 
#  group_by(mrun, proceso) %>% 
#  summarize(n = n()) %>% 
#  arrange(desc(n))
  
#Saving
length(unique(sae_apps$mrun))
length(unique(sae_stud_info$mrun))


save(sae_oferta, sae_apps, file = "./data/clean/sae_2017_19.RData")
save(sae_stud_info, file = "./data/clean/sae_2017_19_stud_controls.RData")




hist(sae_apps$loteria_original)
#Why is this not a uniform distribution? 
#are people randomized into integer numbers depending on the number of spots ?


# Identify oversubscribed spots 

cupos_p_curso <- sae_oferta %>% 
  group_by(rbd, cod_nivel, cod_curso, proceso) %>% 
  summarize(n_cupos = sum(vacantes_regular))  %>% 
  ungroup() %>% 
  filter(n_cupos > 0)  
#  mutate(more_30_cupos = ifelse(n_cupos >= 30,1,0),
#         more_50_cupos = ifelse(n_cupos >= 50,1,0),
#  )

# table(cupos_p_curso$more_30_cupos)
# table(cupos_p_curso$more_50_cupos)


#This is what determines whether a school is oversubscribbed or not.
#I Will only count top 3 applications
apps_p_curso <- sae_apps %>% 
  filter(preferencia_postulante <= 3) %>% 
  group_by(rbd, cod_nivel, cod_curso, proceso) %>% 
  summarize(n_apps = n()) %>% 
  ungroup()

summ_cursos <- left_join(cupos_p_curso, apps_p_curso, by = c("rbd", "cod_nivel", "cod_curso", "proceso")) %>% 
  mutate(exc_apps = pmax(n_apps - n_cupos, 0) ,
         ratio_apps = n_apps / n_cupos) %>% 
  arrange(-(exc_apps)) %>%  
  mutate(br_code = paste0(as.character(rbd), "_", cod_curso, "_", proceso))


#summ_cursos %>%  
#  select(rbd, cod_nivel) %>% 
#  unique() %>% 
#  nrow()

#summary(summ_cursos)

#sample_cursos <- summ_cursos %>% 
#  filter(n_cupos >= 200) %>% 
#  filter(ratio_apps >= 2) %>% 
#  arrange(rbd, cod_nivel, proceso) %>% 
#  ungroup()

#summ_cursos %>% 
#  group_by(rbd) %>% 
#  summarize(n_proc = n()) %>% 
#  arrange(desc(n_proc))


#Power calculation by school
power_cursos <- summ_cursos %>% 
  group_by(rbd, proceso) %>% 
  mutate(n_treated = sum(n_cupos), 
       n_controls = sum(exc_apps)) %>% 
  ungroup() %>% 
  select(rbd, proceso, n_treated, n_controls)  


summary(power_cursos)

#library(dplyr)

# MDE per rbd with clustering across proceso (clusters).
# Assumes each row is one (rbd, proceso) with counts n_treated, n_controls.
# Uses: Var( mean_T ) = σ^2 * [ Σ_j nTj * DE_Tj ] / (Σ_j nTj)^2  and similarly for controls,
# where DE_Tj = 1 + (nTj - 1)*rho and DE_Cj = 1 + (nCj - 1)*rho.

mde_by_rbd_clustered <- function(df,
                                 rho,
                                 alpha = 0.05,
                                 power = 0.80,
                                 small_sample = TRUE) {
  
  z_beta <- qnorm(power)
  
  df %>%
    group_by(rbd) %>%
    summarise(
      G          = n_distinct(proceso),               # number of clusters for this rbd
      nT_total   = sum(n_treated,  na.rm = TRUE),
      nC_total   = sum(n_controls, na.rm = TRUE),
      # Sums needed for variance pieces:
      sum_nT_DET = sum(n_treated  * (1 + (n_treated  - 1) * rho), na.rm = TRUE),
      sum_nC_DEC = sum(n_controls * (1 + (n_controls - 1) * rho), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Small-sample df ≈ G - 2 (min 1). Large-sample uses z.
      df_ss   = pmax(G - 2, 1),
      crit    = if (small_sample) qt(1 - alpha/2, df = df_ss) else qnorm(1 - alpha/2),
      se_fact = sqrt( (sum_nT_DET / (nT_total^2)) + (sum_nC_DEC / (nC_total^2)) ),
      MDE_d   = (crit + z_beta) * se_fact
    ) %>%
    select(rbd, G, nT_total, nC_total, MDE_d, se_fact, crit, df_ss)
}

# ---- Example usage ----
# df <- your data frame with columns: rbd, proceso, n_treated, n_controls, br_code
# Choose an ICC:
rho <- 0.01

mde_rbd <- mde_by_rbd_clustered(power_cursos, rho = rho, alpha = 0.05, power = 0.80, small_sample = FALSE)
mde_rbd


# If you want MDE in outcome units:
# sd_outcome <- 10
# mde_rbd <- mde_rbd %>% mutate(MDE_units = MDE_d * sd_outcome)


hist(mde_rbd$MDE_d, breaks = 50)


#Sample of schools 
rbd_mde_filter <- mde_rbd %>% 
              filter(MDE_d <= 0.35)


#To investigate in the future
rbd_mde_filter %>% filter( G == 2)



#sample_cursos <- summ_cursos %>% 
#  filter(n_cupos >= 200) %>% 
#  filter(ratio_apps >= 2) %>% 
#  arrange(rbd, cod_nivel, proceso) %>% 
#  ungroup()


sample_cursos <- summ_cursos %>% 
  filter(rbd %in% rbd_mde_filter$rbd) 


#sample_selected_cursos <- summ_cursos %>% 
#  filter(n_cupos >= 100) %>% 
#  filter(ratio_apps >= 5) %>% 
#  arrange(rbd, cod_nivel, proceso) %>% 
#  ungroup()


#table(sample_selected_cursos$rbd)

######################
## Create data frame 
#######################


#For each spot, append all students that applied 

sample_students <- sample_cursos %>% 
  select(br_code, n_cupos, n_apps) %>% 
  left_join(sae_apps, multiple = "all",
            by = "br_code") %>% 
  filter(preferencia_postulante <= 3) 



sample_students_all <- sample_cursos %>% 
  select(br_code, n_cupos, n_apps) %>% 
  left_join(sae_apps, multiple = "all",
            by = "br_code")

#table(sample_students_all$proceso)

#sample_students_top3 <- sample_cursos %>% 
#  select(br_code, n_cupos, n_apps) %>% 
#  left_join(sae_apps, multiple = "all",
#            by = "br_code") %>% 
#  filter(preferencia_postulante <= 3) 

#sae_apps %>% 
#  filter(preferencia_postulante <= 5) %>%  
#  filter(br_code %in% sample_cursos$br_code) %>% 
#  pull(mrun) %>% 
#  unique %>% 
#  length()


save(rbd_mde_filter, file = "./data/clean/rbd_mde_sample.RData")
save(sample_students, sample_cursos, file = "./data/clean/samples.RData")
save(sample_students_all, file = "./data/clean/sample_students_nontop5.RData")

