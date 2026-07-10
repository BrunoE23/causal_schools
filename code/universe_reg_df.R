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

setwd(data_wd)
#####################################

library(tidyverse)

income_imputation_min_n <- 15L
baseline_cpad_imputation_vars <- c(
  "father_educ_years",
  "mother_educ_years",
  "father_indigenous",
  "mother_indigenous",
  "sala_cuna",
  "jardin",
  "prekinder",
  "kinder"
)

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
      income_mid_impute_level = if_else(
        is.na(income_mid),
        NA_integer_,
        0L
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
        income_mid_impute_level = if_else(
          should_fill_income,
          match(step_name, names(imputation_steps)),
          income_mid_impute_level
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

  df <- df %>%
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

  for (var in baseline_cpad_imputation_vars) {
    observed_var <- paste0(var, "_observed")
    imputed_var <- paste0(var, "_imputed")
    missing_var <- paste0(var, "_missing")
    source_var <- paste0(var, "_impute_source")
    level_var <- paste0(var, "_impute_level")
    n_var <- paste0(var, "_impute_n")
    was_imputed_var <- paste0(var, "_was_imputed")
    missing_after_var <- paste0(var, "_missing_after_impute")

    df[[observed_var]] <- df[[var]]
    df[[imputed_var]] <- df[[var]]
    df[[missing_var]] <- as.integer(is.na(df[[var]]))
    df[[source_var]] <- ifelse(is.na(df[[var]]), NA_character_, "observed")
    df[[level_var]] <- ifelse(is.na(df[[var]]), NA_integer_, 0L)
    df[[n_var]] <- ifelse(is.na(df[[var]]), NA_integer_, 1L)

    donor_df <- df %>%
      filter(has_baseline_simce_scores, !is.na(.data[[var]]))

    for (step_name in names(imputation_steps)) {
      group_vars <- imputation_steps[[step_name]]

      donor_medians <- donor_df %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          fill_value = median(.data[[var]], na.rm = TRUE),
          fill_n = n(),
          .groups = "drop"
        ) %>%
        filter(fill_n >= min_cell_n)

      df <- df %>%
        left_join(donor_medians, by = group_vars)

      should_fill <- df$has_baseline_simce_scores &
        is.na(df[[imputed_var]]) &
        !is.na(df$fill_value)

      df[[imputed_var]][should_fill] <- df$fill_value[should_fill]
      df[[source_var]][should_fill] <- step_name
      df[[level_var]][should_fill] <- match(step_name, names(imputation_steps))
      df[[n_var]][should_fill] <- as.integer(df$fill_n[should_fill])

      df <- df %>%
        select(-fill_value, -fill_n)
    }

    df[[was_imputed_var]] <- as.integer(
      df$has_baseline_simce_scores &
        is.na(df[[observed_var]]) &
        !is.na(df[[imputed_var]])
    )
    df[[missing_after_var]] <- as.integer(
      df$has_baseline_simce_scores & is.na(df[[imputed_var]])
    )
  }

  df
}



##### 
#Read all 

#universe  + controls
base <- read.csv("data/clean/universe_controls.csv") %>%
  select(-any_of(c("X", "...1"))) %>%
  mutate(mrun = MRUN) %>% 
  mutate(student_id = MRUN) %>% 
  select(mrun, MRUN, student_id, everything())

#sae_indicator
load("./data/clean/sae_binary_prep.RData")

#sae_controls
#load("./data/clean/sae_2017_19_stud_controls.RData")

#controls
load("./data/clean/simce_4to.Rdata")

simce8_math_heterogeneity_path <- "./data/clean/simce8_heterogeneity/cohort_2019_math_heterogeneity.csv"

simce8_math_heterogeneity <- tibble(
  mrun = numeric(),
  simce_year_8b = numeric(),
  ptje_mate8b_alu = numeric(),
  simce4_math_decile = numeric(),
  simce8_math_decile = numeric(),
  simce8_math_quintile = numeric(),
  simce8_vs_4to_math_decile_change = numeric(),
  simce8_math_decile_movement = character(),
  simce8_math_improved_gt1_decile = integer(),
  simce8_math_within1_decile = integer(),
  simce8_math_worsened_gt1_decile = integer()
)

if (file.exists(simce8_math_heterogeneity_path)) {
  simce8_math_heterogeneity <- read.csv(simce8_math_heterogeneity_path) %>%
    select(
      mrun,
      simce_year_8b,
      ptje_mate8b_alu,
      simce4_math_decile,
      simce8_math_decile,
      simce8_math_quintile,
      simce8_vs_4to_math_decile_change,
      simce8_math_decile_movement,
      simce8_math_improved_gt1_decile,
      simce8_math_within1_decile,
      simce8_math_worsened_gt1_decile
    ) %>%
    distinct(mrun, .keep_all = TRUE)
} else {
  warning(
    "SIMCE 8B heterogeneity file not found: ",
    simce8_math_heterogeneity_path,
    ". Run code/codex/simce8_heterogeneity/01_clean_simce8_2019.R first."
  )
}


#treatment?
schools_attended <- read.csv("./data/clean/rbd_universe.csv") %>% 
  select(-any_of(c("X", "...1")))

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
mat_last <- data.table::fread(
  if (dir.exists("./data/clean/mat_ingresos_22-25")) {
    "./data/clean/mat_ingresos_22-25/mat_last_ing.csv"
  } else {
    "./data/clean/mat_ingresos_22-24/mat_last_ing.csv"
  },
  na.strings = c("", "NA")
)

mat_first <- data.table::fread(
  if (dir.exists("./data/clean/mat_ingresos_22-25")) {
    "./data/clean/mat_ingresos_22-25/mat_1st_ing.csv"
  } else {
    "./data/clean/mat_ingresos_22-24/mat_1st_ing.csv"
  },
  na.strings = c("", "NA")
)

paes_matricula_2026_path <- "./data/clean/paes_2026_update/paes_matricula_2025_2026_mapped.rds"
paes_matricula_2026 <- tibble(MRUN = numeric(), paes_matriculated_2026 = integer())

if (file.exists(paes_matricula_2026_path)) {
  paes_matricula_2026 <- readRDS(paes_matricula_2026_path) %>%
    filter(ANYO_PROCESO == 2026) %>%
    mutate(
      MRUN = as.numeric(MRUN),
      PREFERENCIA = suppressWarnings(as.numeric(PREFERENCIA)),
      LUGAR_EN_LA_LISTA = suppressWarnings(as.numeric(LUGAR_EN_LA_LISTA))
    ) %>%
    group_by(MRUN) %>%
    arrange(PREFERENCIA, LUGAR_EN_LA_LISTA, CODIGO_CARRERA, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      MRUN,
      paes_matriculated_2026 = 1L,
      paes_cod_carrera_2026 = CODIGO_CARRERA,
      paes_cod_sies_2026 = COD_SIES,
      paes_nombre_carrera_2026 = NOMBRE_CARRERA,
      paes_sigla_universidad_2026 = SIGLA_UNIVERSIDAD,
      paes_matricula_source_2026 = "PAES-2026-Matricula"
    )
}




reg_df <- left_join(base, offers_1R_first, by = "mrun") %>% 
  mutate(timely_sae = ifelse(cohort_gr8 == sae_proceso, 1L, 0L)) %>% 
  left_join(simce_4to, by = "mrun") %>% 
  left_join(simce8_math_heterogeneity, by = "mrun") %>%
  left_join(schools_attended, by = "MRUN") %>% 
  left_join(students_apps, by = "mrun") %>% 
  rename(psu_year = year) %>% 
  rename(grad_rbd_psu = RBD) %>% 
  mutate(
    expected_psu_year = cohort_gr8 + 5L,
    timely_psu = ifelse(expected_psu_year == psu_year, 1L, 0L)
  ) %>%
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
  left_join(paes_matricula_2026, by = "MRUN") %>%
  mutate(
    paes_matriculated_2026 = replace_na(paes_matriculated_2026, 0L),
    paes_matricula_is_full_sies_2026 = 0L
  ) %>%
  impute_income_decile()
  

rm(list = setdiff(ls(), "reg_df"))
gc()

write.csv(reg_df, "data/clean/univ_gr8_df.csv", row.names = FALSE)

reg_df_stata <- reg_df %>%
  rename_with(
    ~ stringr::str_replace(.x, "_missing_after_impute$", "_miss_after_imp")
  )

haven::write_dta(reg_df_stata, "data/clean/univ_gr8_df.dta")


#prob of treatment
#maybe this one I append on stata? 
#vector_probs <-  read.csv("./data/clean/DA_probs/probs_columns_wide.csv")





