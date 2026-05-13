####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

setwd(data_wd)
#####################################

library(tidyverse)

income_imputation_min_n <- 15L

impute_income_decile <- function(df, min_cell_n = income_imputation_min_n) {
  imputation_steps <- list(
    school_comuna_year = c("simce_rbd_4to", "COD_COM_ALU", "simce_year"),
    school_comuna = c("simce_rbd_4to", "COD_COM_ALU"),
    school_year = c("simce_rbd_4to", "simce_year"),
    school = c("simce_rbd_4to")
  )

  df <- df %>%
    mutate(
      income_mid_observed = income_mid,
      income_mid_imputed = income_mid,
      income_decile_observed = income_decile,
      income_mid_missing = as.integer(is.na(income_mid)),
      income_mid_impute_source = if_else(
        is.na(income_mid),
        NA_character_,
        "observed"
      ),
      income_mid_impute_n = if_else(
        is.na(income_mid),
        NA_integer_,
        1L
      ),
      has_baseline_simce_scores = !is.na(z_sim_mat_4to) & !is.na(z_sim_leng_4to)
    )

  donor_df <- df %>%
    filter(has_baseline_simce_scores, !is.na(income_mid))

  for (step_name in names(imputation_steps)) {
    group_vars <- imputation_steps[[step_name]]

    donor_medians <- donor_df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        income_mid_fill = median(income_mid, na.rm = TRUE),
        income_mid_fill_n = n(),
        .groups = "drop"
      ) %>%
      filter(income_mid_fill_n >= min_cell_n)

    df <- df %>%
      left_join(donor_medians, by = group_vars) %>%
      mutate(
        should_fill_income = has_baseline_simce_scores &
          is.na(income_mid_imputed) &
          !is.na(income_mid_fill),
        income_mid_imputed = if_else(
          should_fill_income,
          income_mid_fill,
          income_mid_imputed
        ),
        income_mid_impute_source = if_else(
          should_fill_income,
          step_name,
          income_mid_impute_source
        ),
        income_mid_impute_n = if_else(
          should_fill_income,
          as.integer(income_mid_fill_n),
          income_mid_impute_n
        )
      ) %>%
      select(-income_mid_fill, -income_mid_fill_n, -should_fill_income)
  }

  income_deciles <- df %>%
    filter(has_baseline_simce_scores, !is.na(income_mid_imputed)) %>%
    transmute(
      mrun,
      income_decile_imputed = ntile(income_mid_imputed, 10)
    )

  df %>%
    left_join(income_deciles, by = "mrun") %>%
    mutate(
      income_mid_was_imputed = as.integer(
        has_baseline_simce_scores &
          is.na(income_mid_observed) &
          !is.na(income_mid_imputed)
      ),
      income_decile_was_imputed = income_mid_was_imputed,
      income_decile_imputation_min_n = min_cell_n,
      income_mid_missing_after_impute = as.integer(
        has_baseline_simce_scores & is.na(income_mid_imputed)
      ),
      income_decile_missing_after_impute = as.integer(
        has_baseline_simce_scores & is.na(income_decile_imputed)
      )
    )
}



##### 
#Read all 

#universe  + controls
base <- read.csv("data/clean/universe_controls.csv") %>% 
  select(-X) %>% 
  mutate(mrun = MRUN) %>% 
  mutate(student_id = MRUN) %>% 
  select(mrun, MRUN, student_id, everything())

#sae_indicator
load("./data/clean/sae_binary_prep.RData")

#sae_controls
#load("./data/clean/sae_2017_19_stud_controls.RData")

#controls
load("./data/clean/simce_4to.Rdata")


#treatment?
schools_attended <- read.csv("./data/clean/rbd_universe.csv") %>% 
  select(-X)

load("./data/clean/offers_1R_p_proceso.RData")

offers_1R_first <- offers_1R_proceso %>% 
                   group_by(mrun) %>% 
                   filter(sae_proceso == min(sae_proceso)) %>% 
                   ungroup()

#table(offers_1R$rbd_treated_1R == 0)


#outcomes
#PSU scores 
load("./data/clean/psu_students.RData")


#Apps
load("./data/clean/stem_outcome.RData")



#matricula
mat_first<- read.csv("./data/clean/mat_ingresos_22-24/mat_1st_ing.csv")
mat_last <- read.csv("./data/clean/mat_ingresos_22-24/mat_last_ing.csv")




reg_df <- left_join(base, offers_1R_first, by = "mrun") %>% 
  mutate(timely_sae = ifelse(cohort_gr8 == sae_proceso, 1L, 0L)) %>% 
  left_join(simce_4to, by = "mrun") %>% 
  left_join(schools_attended, by = "MRUN") %>% 
  left_join(students_apps, by = "mrun") %>% 
  rename(psu_year = year) %>% 
  rename(grad_rbd_psu = RBD) %>% 
  mutate(timely_psu =  ifelse(cohort_gr8 + 4 == psu_year, 1L, 0L)) %>% 
  mutate(took_only_science = as.integer(ifelse(hist_max == 0 & scien_max > 0  , 1, 0))) %>% 
  mutate(took_only_history = as.integer(ifelse(hist_max > 0 & scien_max == 0  , 1, 0))) %>% 
  mutate(took_both = as.integer(ifelse(hist_max > 0 & scien_max > 0  , 1, 0))) %>% 
  mutate(leng_math_total = math_max + leng_max) %>% 
  #There are better measures of graduated HS probably ? 
  mutate(graduated_hs   = as.integer(ifelse(!(is.na(PROM_NOTAS)), 1, 0))) %>% 
  mutate(registered_psu = as.integer(ifelse(!(is.na(FECHA_NACIMIENTO)), 1, 0))) %>% 
  mutate(completed_psu = as.integer(ifelse(leng_math_total> 0,  1, 0))) %>% 
  left_join(stem_outcome, by = "mrun")  %>% 
  left_join(mat_first, by = "MRUN") %>% 
  left_join(mat_last, by = "MRUN") %>%
  impute_income_decile()
  

rm(list = setdiff(ls(), "reg_df"))
gc()

write.csv(reg_df, "data/clean/univ_gr8_df.csv", row.names = FALSE)
haven::write_dta(reg_df,    "data/clean/univ_gr8_df.dta")


#prob of treatment
#maybe this one I append on stata? 
#vector_probs <-  read.csv("./data/clean/DA_probs/probs_columns_wide.csv")





