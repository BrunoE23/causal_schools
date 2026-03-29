####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


library(tidyverse)



###############
table(sae_apps_2018_DA$prioridad_hermano)

sae_apps_2018_DA %>% 
  filter(rbd == 125) %>% 
  #  filter(!(is.na(orden_pie))) %>% 
#  arrange(rbd) %>% 
  View()


#All es_pie == 1 are a subset of prioridad_matriculado == 1 
sae_apps_2018_DA %>% 
  filter(es_pie == 1) %>% 
  filter(prioridad_matriculado == 0) %>% 
  arrange(rbd) %>% 
#  arrange(mrun, preferencia_postulante) %>% 
  View()

#There are about a thousand ppl w orden_pie
sae_apps_2018_DA %>% 
  filter(!(is.na(orden_pie))) %>% 
  filter(prioridad_matriculado == 0) %>% 
  arrange(rbd) %>% 
  #  arrange(mrun, preferencia_postulante) %>% 
  View()


table(oferta_2018$tiene_orden_pie)

oferta_2018 %>% 
  filter(rbd == 4) %>% 
  View()


table(sae_apps_2018_DA$orden_pie)

sae_apps_2017_DA %>% 
  filter(agregada_por_continuidad == 1) %>% 
  filter(prioridad_matriculado == 0 )

sae_apps_2017_DA <- read_csv2("./data/raw/2017/SAE_2017/C1_Postulaciones_etapa_regular_2017_Admisión_2018_PUBL.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  rename(student_id = mrun, 
         school_id = br_code,
         student_pref = preferencia_postulante ) %>% 
  mutate(priority_level = case_when(
#          agregada_por_continuidad == 1 ~ 5L,
          prioridad_matriculado == 1 ~ 4L,
          prioridad_hermano == 1 ~ 3L,
          prioridad_hijo_funcionario == 1 ~ 2L,
          prioridad_exalumno == 1 ~ 1L,
          TRUE ~ 0L)) %>% 
  select(student_id, school_id, student_pref, priority_level)



oferta_2017 <- read_csv2("./data/raw/2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)


oferta_2017_DA <- read_csv2("./data/raw/2017/SAE_2017/A1_Oferta_Establecimientos_etapa_regular_2017_Admisión_2018.csv") %>% 
  mutate(proceso = 2017) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  select(br_code, vacantes_regular) %>% 
  rename(  school_id = br_code,
           spots_available = vacantes_regular)

oferta_2018 <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) 


sum(oferta_2017$vacantes_alta_exigencia_r)

#####2018 block

oferta_2018_DA <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  select(br_code, vacantes_pie , vacantes_prioritarios , vacantes_alta_exigencia_t, vacantes_regular) %>% 
  rename(  school_id = br_code,
           regular_spots     =  vacantes_regular,
           pie_spots         = vacantes_pie, 
           prioritario_spots = vacantes_prioritarios,
           achievement_spots = vacantes_alta_exigencia_t)

sae_apps_2018_DA_lo  %>% 
    filter(agregada_por_continuidad == 1) %>% 
  filter(prioridad_matriculado == 0) 
  

sae_apps_2018_DA_lo <- read_csv2("./data/raw/2018/SAE_2018/C1_Postulaciones_etapa_regular_2018_Admisión_2019_PUBL.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  rename(student_id = mrun, 
         school_id = br_code,
         student_pref = preferencia_postulante,
         is_pie = es_pie,
         academic_order_transition = orden_alta_exigencia_transicion) %>% 
  mutate(priority_level = case_when(
#    agregada_por_continuidad == 1 ~ 5L,
    prioridad_matriculado == 1 ~ 4L,
    prioridad_hermano == 1 ~ 3L,
    prioridad_hijo_funcionario == 1 ~ 2L,
    prioridad_exalumno == 1 ~ 1L,
    TRUE ~ 0L)) %>% 
  select(student_id, school_id, student_pref, priority_level, 
         is_pie, orden_pie , academic_order_transition ,
         loteria_original)

sae_stud_info_2018 <- read_csv2("./data/raw/2018/SAE_2018/B1_Postulantes_etapa_regular_2018_Admisión_2019_PUBL.csv")  %>% 
  filter(cod_nivel== 9) %>% 
  select(mrun, prioritario, alto_rendimiento) %>% 
  rename(student_id = mrun,
          is_prioritario = prioritario,
          is_high_achv   = alto_rendimiento)

sae_apps_2018_DA_lo <- sae_apps_2018_DA_lo %>% 
  left_join(sae_stud_info_2018, by = "student_id")

############# 2019 block


oferta_2019_DA <- read_csv2("./data/raw/2019/SAE_2019/A1_Oferta_Establecimientos_etapa_regular_2019_Admisión_2020.csv") %>% 
  mutate(proceso = 2019) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  select(br_code, vacantes_pie , vacantes_prioritarios , vacantes_alta_exigencia_t, vacantes_regular) %>% 
  rename(  school_id = br_code,
           regular_spots     =  vacantes_regular,
           pie_spots         = vacantes_pie, 
           prioritario_spots = vacantes_prioritarios,
           achievement_spots = vacantes_alta_exigencia_t)

oferta_2018_DA_old <- read_csv2("./data/raw/2018/SAE_2018/A1_Oferta_Establecimientos_etapa_regular_2018_Admisión_2019.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  select(br_code, vacantes_regular) %>% 
  rename(  school_id = br_code,
           spots_available = vacantes_regular)

sae_apps_2018_DA_lo  %>% 
  filter(agregada_por_continuidad == 1) %>% 
  filter(prioridad_matriculado == 0) 

sae_apps_2018_DA_lo  %>% 
  filter(agregada_por_continuidad == 1) %>% 
  filter(prioridad_matriculado == 0) 


sae_apps_2018_DA_lo <- read_csv2("./data/raw/2018/SAE_2018/C1_Postulaciones_etapa_regular_2018_Admisión_2019_PUBL.csv") %>% 
  mutate(proceso = 2018) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9) %>% 
  rename(student_id = mrun, 
         school_id = br_code,
         student_pref = preferencia_postulante,
         is_pie = es_pie,
         academic_order_transition = orden_alta_exigencia_transicion) %>% 
  mutate(priority_level = case_when(
    #    agregada_por_continuidad == 1 ~ 5L,
    prioridad_matriculado == 1 ~ 4L,
    prioridad_hermano == 1 ~ 3L,
    prioridad_hijo_funcionario == 1 ~ 2L,
    prioridad_exalumno == 1 ~ 1L,
    TRUE ~ 0L)) %>% 
  select(student_id, school_id, student_pref, priority_level, 
         is_pie, orden_pie , academic_order_transition ,
         loteria_original)

sae_stud_info_2018 <- read_csv2("./data/raw/2018/SAE_2018/B1_Postulantes_etapa_regular_2018_Admisión_2019_PUBL.csv")  %>% 
  filter(cod_nivel== 9) %>% 
  select(mrun, prioritario, alto_rendimiento) %>% 
  rename(student_id = mrun,
         is_prioritario = prioritario,
         is_high_achv   = alto_rendimiento)

sae_apps_2018_DA_lo <- sae_apps_2018_DA_lo %>% 
  left_join(sae_stud_info_2018, by = "student_id")









#####################

(sae_apps_2018_DA_lo) %>% 
#  filter(is_pie == 1) %>% 
  filter(!is.na(orden_pie)) %>% 
  View()


#Pareciera que high_achv tiene que ver con orden_alta_exigecia_r
(sae_apps_2018_DA_lo) %>% 
    filter(is_high_achv == 1) %>% 
  filter(!is.na(orden_alta_exigencia_transicion)) %>% 
  select(student_id) %>%  
  unique() %>% 
  View()




is_pie

sae_apps_2018_DA <- sae_apps_2018_DA_lo %>% 
  select(-loteria_original)


#2000 apps have orden_pie
sae_apps_2018_DA_lo %>% 
  filter(!is.na(orden_pie))

#6700 apps have orden_alta_exigencia_transicion
sae_apps_2018_DA_lo %>% 
  filter(!is.na(orden_alta_exigencia_transicion))



sae_apps_2020 <- read_csv2("./data/raw/2020/SAE_2020/C1_Postulaciones_etapa_regular_2020_Admisión_2021_PUBL.csv") %>% 
  mutate(proceso = 2020) %>% 
  mutate(br_code = paste0(as.character(rbd), "_", cod_curso, "_", proceso)) %>% 
  filter(cod_nivel == 9)

sae_apps_2020 %>% 
  filter(!(is.na(orden_pie))) %>% 
  View()
#################################



#It takes 25 seconds to do one real DA in 2017 (smaller sample, lots of prob = 1)
Run_school_DA(apps_db = sae_apps_2017_DA, 
              school_db = oferta_2017_DA, 
              print = FALSE, time = TRUE)


#2 mins
short_sae_17_5_reps <-   Loop_DA(apps_db = sae_apps_2017_DA, 
                                 school_db = oferta_2017_DA, 
                                 5,
                                 time = TRUE)
short_sae_17_5_reps %>% 
  filter(prob < 1)


short_sae_17_60_reps <-   Loop_DA(apps_db = sae_apps_2017_DA, 
                                  school_db = oferta_2017_DA, 
                                  60,
                                  time = TRUE)

#33 min to run 60 as expected
short_sae_17_60_reps %>% 
  filter(prob < 1)

#It takes 2 seconds to do one real DA w GPT code
#It used to be 3.5 min
Run_school_DA(apps_db = sae_apps_2018_DA, 
              school_db = oferta_2018_DA, 
              print = FALSE, time = TRUE, seed = 123)

#fast approach: takes 0.2 s 
 Run_school_DA_fast(apps_db = sae_apps_2018_DA, 
              school_db = oferta_2018_DA, 
              print = FALSE, time = TRUE, seed = 123)

 
 
#I can now take about 3 minutes to do 100 repeats
#It used to take 163 seconds min to do 5 repeats

sae_18_100_reps <-   Loop_DA(apps_db = sae_apps_2018_DA, 
                                 school_db = oferta_2018_DA, 
                                 100,
                                 time = TRUE)

#It took 55 seconds to do 100 repeats
#10 min to do 1k !?
sae_18_100_reps_fast <-   Loop_DA_fast(apps_db = sae_apps_2018_DA, 
                             school_db = oferta_2018_DA, 
                             100,
                             time = TRUE)

sae_18_10_reps_fast <-   Loop_DA_fast(apps_db = sae_apps_2018_DA, 
                                       school_db = oferta_2018_DA, 
                                       10, seed = 123, 
                                       time = TRUE)

sae_18_10_reps_fast_b <-   Loop_DA_fast(apps_db = sae_apps_2018_DA, 
                                      school_db = oferta_2018_DA, 
                                      10, seed = 123, 
                                      time = TRUE)

head(sae_18_10_reps_fast_b)


sae_18_100_reps %>% 
  filter(prob < 1)


sae_18_5_reps %>% 
  filter(school_id  == "unmatched")


results_2018_DA <- read_csv2("./data/raw/2018/SAE_2018/D1_Resultados_etapa_regular_2018_Admisión_2019_PUBL.csv") %>%
  mutate(proceso = 2018) %>%
  filter(cod_nivel == 9) %>% 
  rename(rbd = rbd_admitido,
         cod_curso = cod_curso_admitido) %>% 
  mutate(br_code = paste0(as.character(rbd),  "_", cod_curso, "_", proceso)) %>% 
  mutate(br_code = ifelse(is.na(rbd), "unmatched", br_code)) %>% 
  select(mrun, br_code) %>% 
  rename(school_id_actual = br_code,
         student_id = mrun)


results_2018_DA %>% 
  filter(school_id_actual == "unmatched")

test_2018_seed <- Run_school_DA_fast(apps_db = sae_apps_2018_DA, 
              school_db = oferta_2018_DA_old,  
              print = FALSE, time = TRUE, seed = 321)

head(test_2018_seed)

test_2018_seed %>% 
  filter(school_id == "unmatched")


test_2018_lo <- Run_school_DA(apps_db = sae_apps_2018_DA_lo, 
                                school_db = oferta_2018_DA,  
                                print = FALSE, time = TRUE)


#Naive test: how far am I with a random seed versus the actual outcome 

compare_results_2018 <- left_join(results_2018_DA, test_2018_seed, by = "student_id") %>% 
                        mutate(same_assignment = as.integer(school_id == school_id_actual))

prop.table(table(compare_results_2018$same_assignment))


compare_results_2018_lo <- left_join(results_2018_DA, test_2018_lo, by = "student_id") %>% 
  mutate(same_assignment = as.integer(school_id == school_id_actual))

prop.table(table(compare_results_2018_lo$same_assignment))


#lo did not help at all! 
#only possible explanation is at least 15% of students are prioritarios? 

sae_stud_info_2018 <- read_csv2("./data/raw/2018/SAE_2018/B1_Postulantes_etapa_regular_2018_Admisión_2019_PUBL.csv")  %>% 
  filter(cod_nivel== 9) %>% 
  mutate(proceso = 2018) 

table(sae_stud_info_2018$prioritario)

table(sae_stud_info_2018$alto_rendimiento)


#Try version with some guess for quotas.
test_2018_CL_lo <- Run_school_DA_CL(
  school_db = oferta_2018_DA,
  apps_db = sae_apps_2018_DA_lo,
  quota_order = c("pie", "achievement", "prioritario", "regular"),
  unused_quota_rule = "roll_to_regular",
  special_priority_rule = "same_as_regular",
  use_loteria_original = TRUE,
  print = TRUE,
  time = TRUE
)

compare_results_2018_CL <- left_join(results_2018_DA, test_2018_CL_lo, by = "student_id") %>% 
  mutate(same_assignment = as.integer(school_id == school_id_actual))

prop.table(table(compare_results_2018_CL$same_assignment))

#96.8% !!




#Try version with academic transition
test_2018_CL_lo_trans <- Run_school_DA_fast_CL(
  school_db = oferta_2018_DA,
  apps_db = sae_apps_2018_DA_lo,
  achievement_rule = "transition",
  quota_order = c("pie", "achievement", "prioritario", "regular"),
  unused_quota_rule = "roll_to_regular",
  special_priority_rule = "same_as_regular",
  use_loteria_original = TRUE,
  print = TRUE,
  time = TRUE
)

compare_results_2018_CL_t <- left_join(results_2018_DA, test_2018_CL_lo_trans, by = "student_id") %>% 
  mutate(same_assignment = as.integer(school_id == school_id_actual))

(table(compare_results_2018_CL_t$same_assignment))
prop.table(table(compare_results_2018_CL_t$same_assignment))

#99.15% !!

#Only missing implementing orden_pie value.
sae_apps_2018_DA_lo %>% 
  filter(!(is.na(orden_pie))) %>% 
  filter(is_pie == 1)




#Try version with academic transition + PIE_order (v3)
test_2018_CL_lo_trans_pie <- Run_school_DA_fast_CL(
  school_db = oferta_2018_DA,
  apps_db = sae_apps_2018_DA_lo,
  achievement_rule = "transition",
  pie_rule = "school_order_if_available",
  quota_order = c("pie", "achievement", "prioritario", "regular"),
  unused_quota_rule = "roll_to_regular",
  special_priority_rule = "same_as_regular",
  use_loteria_original = TRUE,
  print = TRUE,
  time = TRUE
)

compare_results_2018_CL_t_p <- left_join(results_2018_DA, test_2018_CL_lo_trans_pie, by = "student_id") %>% 
  mutate(same_assignment = as.integer(school_id == school_id_actual))

(table(compare_results_2018_CL_t_p$same_assignment))
prop.table(table(compare_results_2018_CL_t_p$same_assignment))

#99.32% !!

wrong_match <- compare_results_2018_CL_t_p %>% 
  filter(same_assignment == 0) %>% 
  rename(my_da_school_id = school_id)

window <- sae_apps_2018_DA_lo %>% 
  inner_join(wrong_match, by = "student_id") %>% 
  arrange(student_id, student_pref)

View(window)

window %>% 
  filter(priority_level == 4) %>% 
  filter(my_da_school_id == "unmatched") %>% 
  select(student_id, school_id, my_da_school_id, priority_level  ) %>% 
  write.csv("obvious_issue.csv")



check_priority4_spots <- window %>%
  filter(priority_level == 4,
         my_da_school_id == "unmatched") %>%
  distinct(student_id, school_id, my_da_school_id, student_pref, priority_level) %>%
  left_join(
    oferta_2018_DA %>%
      select(school_id,
             pie_spots,
             achievement_spots,
             prioritario_spots,
             regular_spots),
    by = "school_id"
  ) %>%
  mutate(
    total_spots = coalesce(pie_spots, 0) +
      coalesce(achievement_spots, 0) +
      coalesce(prioritario_spots, 0) +
      coalesce(regular_spots, 0),
    has_any_spots = total_spots > 0,
    has_regular_spots = coalesce(regular_spots, 0) > 0
  )

check_priority4_spots %>% 
  filter(has_any_spots == FALSE) %>% 
  View()


#Ensure continuidad (v4)
test_2018_CL_lo_trans_pie_v4 <- Run_school_DA_fast_CL_v4(
  school_db = oferta_2018_DA,
  apps_db = sae_apps_2018_DA_lo,
  achievement_rule = "transition",
  pie_rule = "school_order_if_available",
  quota_order = c("pie", "achievement", "prioritario", "regular", "continuity"),
  unused_quota_rule = "roll_to_regular",
  special_priority_rule = "same_as_regular",
  use_loteria_original = TRUE,
  print = FALSE,
  time = TRUE
)

compare_results_2018_CL_t_p_ct <- left_join(results_2018_DA, test_2018_CL_lo_trans_pie_v4, by = "student_id") %>% 
  mutate(same_assignment = as.integer(school_id == school_id_actual))

(table(compare_results_2018_CL_t_p_ct$same_assignment))
prop.table(table(compare_results_2018_CL_t_p_ct$same_assignment))

#99.47% !!

wrong_match <- compare_results_2018_CL_t_p_ct %>% 
  filter(same_assignment == 0) %>% 
  rename(my_da_school_id = school_id)

window <- sae_apps_2018_DA_lo %>% 
  inner_join(wrong_match, by = "student_id") %>% 
  arrange(student_id, student_pref)

View(window)

window %>% 
  filter(!is.na(academic_order_transition)) %>% 
  View()
#  filter(my_da_school_id == "unmatched") %>% 
  write.csv("obvious_issue.csv")





check_priority4_spots <- window %>%
  filter(priority_level == 4,
         my_da_school_id == "unmatched") %>%
  distinct(student_id, school_id, my_da_school_id, student_pref, priority_level) %>%
  left_join(
    oferta_2018_DA %>%
      select(school_id,
             pie_spots,
             achievement_spots,
             prioritario_spots,
             regular_spots),
    by = "school_id"
  ) %>%
  mutate(
    total_spots = coalesce(pie_spots, 0) +
      coalesce(achievement_spots, 0) +
      coalesce(prioritario_spots, 0) +
      coalesce(regular_spots, 0),
    has_any_spots = total_spots > 0,
    has_regular_spots = coalesce(regular_spots, 0) > 0
  )

check_priority4_spots %>% 
  filter(has_any_spots == FALSE) 