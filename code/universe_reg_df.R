####################################
#data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

setwd(data_wd)
#####################################

library(tidyverse)



##### 
#Read all 

#universe  + controls
base <- read.csv("data/clean/universe_controls.csv") %>% 
  select(-X) %>% 
  mutate(mrun = MRUN) %>% 
  mutate(student_id = MRUN) %>% 
  select(mrun, MRUN, student_id, everything())

#controls
load("./data/clean/simce_4to.Rdata")

#treatment?
schools_attended <- read.csv("./data/clean/rbd_universe.csv") %>% 
  select(-X)

#prob of treatment
#maybe this one I append on stata? 
#vector_probs <-  read.csv("./data/clean/DA_probs/probs_columns_wide.csv")

#sae_indicator

#sae_controls
load("./data/clean/sae_2017_19_stud_controls.RData")

#outcomes
#PSU
load("./data/clean/psu_students.RData")


#apps


#matricula
mat_first<- read.csv("./data/clean/mat_ingresos_22-24/mat_1st_ing.csv")
mat_last <- read.csv("./data/clean/mat_ingresos_22-24/mat_last_ing.csv")
