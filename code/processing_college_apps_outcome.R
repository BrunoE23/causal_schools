####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################

library(tidyverse)
library(readxl)

dollar_clp_conversion <- 913


load("./data/clean/psu_applications.RData")


oferta_2020 <- read_xlsx("./data/raw/2020/PSU2020/PostulaciónySelección_Admisión2020/Libro_CódigosADM2020_ArchivoD.xlsx", sheet = 3) %>% 
  mutate(stem_share = `%_MATE` + `%_CIEN`,
         stem_proxy_low = ifelse(stem_share > 45, 1L, 0L),
         stem_proxy_high = ifelse(stem_share > 50, 1L, 0L)) %>% 
  rename(PROCESO = AGNO_ACAD) %>% 
  select(PROCESO, CODIGO, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high)



#colnames(program_info_2024)

#Enrollment data, but has a lot of info I use.


program_info_all <- readRDS("./data/clean/program_info_22-24.rds") %>% 
  mutate(
    NOMB_INST = str_remove(NOMB_INST, 
                           "\\s*\\(\\*.*?\\)")
  )


#Data checks
# length(unique(program_info_2024$COD_SIES))
# 
# table(program_info_2024$AREA_CONOCIMIENTO, program_info_2024$CINE_F_13_AREA)
# table(program_info_2024$CINE_F_13_AREA)
# 
# program_info_2024 %>% 
#   filter(CINE_F_13_AREA == "Servicios") %>% 
#   pull(AREA_CARRERA_GENERICA) %>% 
#   table()
# 
# program_info_2024 %>% 
#   filter(AREA_CONOCIMIENTO == "Tecnología") %>% 
#   View()


mifuturo_recent <- readxl::read_xlsx(
  "./data/raw/mifuturo/Buscador_Empleabilidad_ingresos_2025_2026_SIES.xlsx",
  sheet = 2) %>%  
  rename(NOMB_INST             = `Nombre de institución`, 
         AREA_CARRERA_GENERICA = `Nombre carrera genérica`,
         TIPO_INST_1           = `Tipo de institución`,
         NOMB_CARRERA           = `Nombre carrera (del título)`,
         
         employment_1st_year   = `Empleabilidad 1er año`,
         employment_2nd_year   = `Empleabilidad 2° año`,
         income_4th_year       = `Ingreso Promedio al 4° año`
  ) %>% 
  mutate(TIPO_INST_1 = ifelse(TIPO_INST_1 == "Insitutos Profesionales", 
                              "Institutos Profesionales", 
                              TIPO_INST_1)) %>% 
  select(#NOMB_CARRERA, 
    NOMB_INST, AREA_CARRERA_GENERICA, TIPO_INST_1,
         employment_1st_year, employment_2nd_year, income_4th_year
  )  %>% 
  filter(!(is.na(AREA_CARRERA_GENERICA)))
#%>% 
#mutate(NOMB_CARRERA = stringr::str_to_upper(NOMB_CARRERA))



mifuturo_recent_clean <- mifuturo_recent %>%
  mutate(
    # --- employment: coerce to character, replace "n/a"/"s/i", then parse numeric
    employment_1st_year = as.character(employment_1st_year),
    employment_2nd_year = as.character(employment_2nd_year),
    
    employment_1st_year = na_if(employment_1st_year, "n/a"),
    employment_1st_year = na_if(employment_1st_year, "s/i"),
    employment_2nd_year = na_if(employment_2nd_year, "n/a"),
    employment_2nd_year = na_if(employment_2nd_year, "s/i"),
    
    employment_1st_year = parse_number(employment_1st_year, locale = locale(decimal_mark = ".")),
    employment_2nd_year = parse_number(employment_2nd_year, locale = locale(decimal_mark = ".")),
    
    # --- income: keep as character, replace "n/a"/"s/i"
    income_4th_year = as.character(income_4th_year),
    income_4th_year = na_if(income_4th_year, "n/a"),
    income_4th_year = na_if(income_4th_year, "s/i")
  )


parse_income_midpoint_clp <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("n/a", "s/i", "", "NA")] <- NA_character_
  
  extract_amounts <- function(s) {
    if (is.na(s)) return(numeric(0))
    s2 <- str_squish(str_replace_all(s, "\\$", ""))
    
    pat <- "(\\d+)\\s+mill[oó]n(?:es)?(?:\\s+(\\d+)\\s+mil)?|(\\d+)\\s+mil"
    m <- str_match_all(s2, pat)[[1]]
    if (nrow(m) == 0) return(numeric(0))
    
    out <- numeric(0)
    for (i in seq_len(nrow(m))) {
      mil1  <- m[i, 2]
      milk  <- m[i, 3]
      onlyk <- m[i, 4]
      if (!is.na(mil1)) {
        out <- c(out, as.numeric(mil1) * 1e6 + ifelse(is.na(milk), 0, as.numeric(milk) * 1e3))
      } else if (!is.na(onlyk)) {
        out <- c(out, as.numeric(onlyk) * 1e3)
      }
    }
    out
  }
  
  vapply(x, function(s) {
    if (is.na(s)) return(NA_real_)
    
    if (s == "Sobre $3 millones 500 mil") return(4e6)
    
    amts <- extract_amounts(s)
    if (length(amts) >= 2) mean(amts[1:2]) else NA_real_
  }, numeric(1))
}

mifuturo_recent_clean <- mifuturo_recent_clean %>%
  mutate(income_4th_year_mid_clp = parse_income_midpoint_clp(income_4th_year)) %>% 
  mutate(income_4th_year_mid_usd = income_4th_year_mid_clp / dollar_clp_conversion) %>%  
  mutate(log_income = log(income_4th_year_mid_usd))

table(is.na(mifuturo_recent_clean$log_income))

mifuturo_recent_clean %>% 
  filter(is.na(log_income)) %>% 
  View()
 

# mifuturo_recent_clean %>%
#   group_by(NOMB_INST, AREA_CARRERA_GENERICA) %>%
#   summarize(count = n()) %>% 
#   filter(count > 1) %>% 
#   ungroup()


#Predicting employment 
emp_reg_wage <- lm(data = mifuturo_recent_clean, employment_2nd_year ~ employment_1st_year)
summary(emp_reg_wage)$r.squared
nobs(emp_reg_wage)
###


###################################
######### Impute wages

# Ensure factors in the training data
train <- mifuturo_recent_clean %>%
  filter(!is.na(log_income)) %>%
  mutate(
    NOMB_INST = factor(NOMB_INST),
    AREA_CARRERA_GENERICA = factor(AREA_CARRERA_GENERICA)
  )

miss <- mifuturo_recent_clean %>%
  filter(is.na(log_income)) %>%
  mutate(
    NOMB_INST = factor(NOMB_INST, levels = levels(train$NOMB_INST)),
    AREA_CARRERA_GENERICA = factor(AREA_CARRERA_GENERICA,
                                   levels = levels(train$AREA_CARRERA_GENERICA))
  )


fe_reg_wage <- lm(log_income ~ NOMB_INST + AREA_CARRERA_GENERICA, data = train)

smear <- mean(exp(residuals(fe_reg_wage)))
# Predict only where both factors are in-support (not NA after re-leveling)

miss_supported <- miss %>% filter(!is.na(NOMB_INST), !is.na(AREA_CARRERA_GENERICA))
miss_supported$log_income_hat <- predict(fe_reg_wage, newdata = miss_supported)
miss_supported$income_hat <- exp(miss_supported$log_income_hat) * smear


b <- coef(fe_reg_wage)

# Build FULL vector of institution effects, including the omitted baseline as 0
inst_levels <- levels(train$NOMB_INST)
inst_fe <- setNames(rep(0, length(inst_levels)), inst_levels)

inst_idx <- grep("^NOMB_INST", names(b))  # works with formula "log_income ~ NOMB_INST + ..."
inst_names <- sub("^NOMB_INST", "", names(b)[inst_idx])
inst_fe[inst_names] <- b[inst_idx]

# Unweighted mean across institutions (each institution once)
c_inst <- mean(inst_fe)

# Centered institution effects
inst_fe_centered <- inst_fe - c_inst

# Adjust intercept so fitted values don't change
alpha_centered <- b["(Intercept)"] + c_inst


## Repeat for majors
maj_levels <- levels(train$AREA_CARRERA_GENERICA)
maj_fe <- setNames(rep(0, length(maj_levels)), maj_levels)

maj_fe_idx <- grep("^AREA_CARRERA_GENERICA", names(b))  # works with formula "log_income ~ NOMB_INST + ..."
maj_fe_names <- sub("^AREA_CARRERA_GENERICA", "", names(b)[maj_fe_idx])
maj_fe[maj_fe_names] <- b[maj_fe_idx]

sort(maj_fe)



mifuturo_imputed <- mifuturo_recent_clean %>%
  left_join(
    miss_supported %>% select(NOMB_INST , AREA_CARRERA_GENERICA , log_income_hat, income_hat),
    by = c("NOMB_INST", "AREA_CARRERA_GENERICA")
  ) %>%
  mutate(
    log_income_imp = ifelse(is.na(log_income), log_income_hat, log_income),
    income_imp     = ifelse(is.na(log_income), income_hat, exp(log_income))
  )




inst_effects_tbl <- tibble::tibble(
  NOMB_INST = names(inst_fe_centered),
  inst_fe = as.numeric(inst_fe_centered)
)


major_effects_tbl <- tibble::tibble(
  AREA_CARRERA_GENERICA = names(maj_fe),
  maj_fe = as.numeric(maj_fe)
)



saveRDS(inst_effects_tbl, "./data/clean/inst_fe.rds")
saveRDS(major_effects_tbl, "./data/clean/maj_fe.rds")

##########################


sum(is.na(mifuturo_recent_clean$income_4th_year_mid_clp))
sum(is.na(mifuturo_recent_clean$employment_1st_year))
sum(is.na(mifuturo_recent_clean$employment_2nd_year))



fe_reg_employ <- lm(data = mifuturo_recent_clean, employment_1st_year ~ factor(NOMB_INST) + factor(AREA_CARRERA_GENERICA))
summary(fe_reg_employ)$r.squared
nobs(fe_reg_employ)

fe_reg_employ2 <- lm(data = mifuturo_recent_clean, employment_2nd_year ~ factor(NOMB_INST) + factor(AREA_CARRERA_GENERICA))
summary(fe_reg_employ2)$r.squared
nobs(fe_reg_employ2)




#Append employment data
mifuturo_imputed <- mifuturo_imputed %>%
  mutate(AREA_CARRERA_GENERICA = str_squish(AREA_CARRERA_GENERICA))

program_info_all <- program_info_all %>%
  mutate(AREA_CARRERA_GENERICA = str_squish(AREA_CARRERA_GENERICA))

program_info_joint <-  left_join(program_info_all,
                                 mifuturo_imputed, 
                                by = c("NOMB_INST",
                                       "AREA_CARRERA_GENERICA",
                                       "TIPO_INST_1"))

setdiff(unique(program_info_all$AREA_CARRERA_GENERICA),
        unique(mifuturo_imputed$AREA_CARRERA_GENERICA))
        

setdiff(unique(mifuturo_imputed$AREA_CARRERA_GENERICA),
        unique(program_info_all$AREA_CARRERA_GENERICA))



setdiff(unique(program_info_all$NOMB_INST),
        unique(mifuturo_imputed$NOMB_INST))


setdiff(unique(mifuturo_imputed$NOMB_INST),
        unique(program_info_all$NOMB_INST))




program_info_joint %>% 
  filter(CODIGO_DEMRE ==0) %>% 
  filter(is.na(employment_1st_year)) %>% 
#  filter(TIPO_INST_1 == "Universidades") %>% 
  View()


#sum(program_info_2024$NOMB_CARRERA %in% mifuturo_recent$NOMB_CARRERA)
sum(program_info_2024$NOMB_INST %in% mifuturo_recent$NOMB_INST)
sum(program_info_2024$AREA_CARRERA_GENERICA %in% mifuturo_recent$AREA_CARRERA_GENERICA)
sum(program_info_2024$TIPO_INST_1 %in% mifuturo_recent$TIPO_INST_1)

#TODO: See and think on how to fix this NAs 
sum(is.na(program_info_2024_joint$income_4th_year))
sum(is.na(program_info_2024_joint$income_4th_year_clp))




oferta_codes_all_info <- readRDS("./data/clean/oferta_codes_24_25.rds") %>% 
  mutate(stem_share = rowSums(across(c(CIEN, M1, M2)), na.rm = TRUE),
         stem_proxy_low = ifelse(stem_share >= 40, 1L, 0L),
         stem_proxy_high = ifelse(stem_share > 40, 1L, 0L)) %>% 
  rename(year_oferta = year_info) %>%   
#  select(PROCESO, COD_CARRERA_PREF, COD_SIES, CARRERA, UNIVERSIDAD, stem_share,stem_proxy_low, stem_proxy_high) %>% 
  left_join(program_info_joint, by = "COD_SIES")


oferta_codes_all %>% 
  filter(is.na(income_imp)) %>% 
  View()

hist(oferta_codes_all$stem_share, breaks = 20)
prop.table(table(oferta_codes_all$stem_share))


hist(oferta_codes_all$income_imp, breaks = 20)

#oferta_combined <- rbind(oferta_2024, oferta_2020) %>%
#                   select(-PROCESO) %>%
#                    unique() %>%
#                    arrange(CODIGO)



########


apps_all_info <- college_apps %>% 
  filter(ORDEN_PREF <= 10) %>%
  filter(TIPO_PREF == "REGULAR") %>%
  left_join(oferta_codes_all_info, by = "COD_CARRERA_PREF") %>%
  group_by(mrun) %>%
  #Only first application per person
  filter(year == min(year)) %>% 
  ungroup()
  

#Of 4.1M apps I am missing income information for 700K.
#So my coverage is 83%. Not too shabby. 

apps_all_info %>% 
  filter(is.na(income_imp))

#If I dont limit to first 10 apps, I get 4.75M and I miss 800k. 
#Same rate 

#Of 4.1M apps I am missing  field information for 77k
#So my coverage is 98%. Pretty good!!  
apps_all_info %>% 
  filter(is.na(field_merged))


apps_all_info %>% 
  pull(COMUNA_SEDE) %>% 
  table()


#################################
#### Current person level outcome computation

  stem_outcome <- college_apps %>% 
        filter(ORDEN_PREF <= 5) %>%
        filter(TIPO_PREF == "REGULAR") %>%
        left_join(oferta_2024, by = "COD_CARRERA_PREF") %>%
        group_by(mrun) %>%
  #Only first application
    filter(year == min(year)) %>%
    summarize(avg_stem_share = mean(stem_share),
              
              across(
                c(stem_bin, health_bin),
                ~ mean(.x, na.rm = TRUE),           # proportion=mean for 0/1
                .names = "prop_{.col}"),
                
              n_stem_low  = sum(stem_proxy_low, na.rm = TRUE),
              n_stem_high = sum(stem_proxy_high, na.rm = TRUE),
              only_stem_low  = min(stem_proxy_low),
              only_stem_high = min(stem_proxy_high),
              year_1st_app = min(year),
              #,
              avg_employ_y1 = mean(employment_1st_year, na.rm = TRUE),   
              avg_employ_y2 = mean(employment_2nd_year, na.rm = TRUE),   
              avg_income_y4 = mean(income_4th_year_mid_clp, na.rm = TRUE),       
              
                            
    )
  
  sum(is.na(stem_outcome$avg_income_y4))
  
  save(stem_outcome, file = "./data/clean/stem_outcome.RData")
  