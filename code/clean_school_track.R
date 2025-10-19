####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


#read samples to get student universe
load("./data/clean/samples.RData")


students_small <- sample_students %>% 
  select(mrun, rbd, cod_nivel, proceso) %>% 
  rename(rbd_target = rbd)


students_unique <- sample_students %>% 
  pull(mrun) %>% 
  unique()


repeated_students_in_sample <- students_small %>% 
                                group_by(mrun) %>% 
                                mutate(n_apps_in_sample = n()) %>% 
                                 arrange(desc(n_apps_in_sample), mrun)   %>%  
                                ungroup()

repeated_students_in_sample %>% 
  select(mrun, n_apps_in_sample) %>% 
  unique() %>% 
  pull(n_apps_in_sample) %>% 
  table()



#follow students unique over time 


