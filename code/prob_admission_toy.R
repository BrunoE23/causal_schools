####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


library(tidyverse)



#Toy Example
#Page 8-9 of RDMD
#######################################


school_db <- data.frame(
    school_id = c(41,52,63), 
    school_name = c("a","b","c"),
    spots_available = c(1,1,1)
    )


apps_db <- data.frame(
  student_id     = c(1,1,2,2,3,4,4,5),
  school_id = c(41,52,41,52,41,63,41,63),
  student_pref     = c(1,2,1,2,1,1,2,1),
  priority_level   = c(0,0,0,1,0,0,0,1)
)




#### Run the algorithm once 

Run_school_DA <-    function(school_db, apps_db, 
                             seed = NULL, print = FALSE, time = FALSE) {

  
  if(time == TRUE) {
    start_time <- Sys.time()
  }
  
  #Create new Databases:

  #A copy of school_db where I can update spots remaining
#  school_cur_spots <- school_db %>% 
#    mutate(cur_spots_available = spots_available)
  
  if (!is.null(seed)) set.seed(seed)
  
  
  #A DB to keep track of rejections and w lottery tickets
  current_apps <- apps_db %>% 
    mutate(rejected = 0L) %>% 
    group_by(school_id) %>% 
    mutate(lottery_ticket = sample(1:n())) %>% 
    ungroup()
    
    #A final student list 
  school_offers <- apps_db %>% 
    select(student_id) %>% 
    unique() 
  
  round_rejected = -999

#change for a while  
while (round_rejected != 0) {  

  round_rejected = 0
  
  #Matched students
#  matched_students <- school_offers %>% 
#    filter(school_offer != 0L) %>% 
#    pull(student_id)
  

  #Compute the current proposal depending on the rejections.

  #Get top non rejected preference
  #Re-compute from rejected
  current_apps <- current_apps %>% 
    group_by(student_id) %>% 
    mutate(
      current_proposal = if (any(rejected == 0)) {
        as.integer(
          rejected == 0 & 
            student_pref == min(student_pref[rejected == 0])
        )
      } else {
        0L
      }
    ) %>% 
    ungroup()

  cur_app_proposal <- current_apps %>% 
    filter(current_proposal == 1)
  

#next round
#  if (nrow(cur_unmatched_students) == 0) {next}
  
  #List of schools where there are students applying 
 schools_target <-   cur_app_proposal %>% 
    pull(school_id) %>% 
    unique()
  
  
schools_iterate <- school_db %>% 
   filter(school_id %in% schools_target)
 
  #DB of schools to assign spots
  #Just to make iteration quicker
#  schools_to_assign_spots <- school_cur_spots %>% 
#  filter(cur_spots_available > 0) %>% 
#  filter(school_id  %in% schools_target)
    
  
  #next round
#  if (nrow(schools_to_assign_spots) == 0) {next}
  


#For each spot, assign spots for highest tier students.
for (i in 1:nrow(schools_iterate)) {

  spot_id          <- schools_iterate$school_id[i]
  N_spots_available  <- schools_iterate$spots_available[i]
  
  
#Consider students applying to school and order

  #For priority, higher is better;
  #For Lottery, higher is bad. 
      ordered_cur_school <- cur_app_proposal %>% 
        filter(school_id == spot_id) %>% 
        arrange(desc(priority_level), lottery_ticket) 
      
      
      n_reject <- max(nrow(ordered_cur_school) - N_spots_available, 0)
      
      rejected_cur_school <- ordered_cur_school %>% 
        slice_tail(n = n_reject) %>% 
        pull(student_id)
    
    if (n_reject > 0 & (print == TRUE)) {
    print(paste("student", rejected_cur_school, "rejected from school", spot_id))
    }
    
    round_rejected <- round_rejected + n_reject
    
#    provisional_cur_school <-  ordered_cur_school %>% 
#      slice(N_spots_available)
      
    #Send rejections!
 current_apps  <- current_apps %>% 
              mutate( rejected = 
                        ifelse( current_proposal == 1 & 
                                school_id == spot_id & 
                                student_id %in% rejected_cur_school  
                              , 1, rejected)
              )
    
        #End of a school spot analysis            
              }
  #End of within school spot loop
   
if (print == TRUE)  print(paste("Number of rejections this round:", n_rejected))


  #End of across schools spots loop  (a round)
  }
  #End of all rounds
  
  if (is.character(school_db$school_id)) {
    unmatched_value <- "unmatched"
  } else if (is.integer(school_db$school_id)) {
    unmatched_value <- -99L
  } else {
    unmatched_value <- -99
  }
  
  school_offers <-    cur_app_proposal %>% 
                      filter(rejected == 0) %>% 
                      select(student_id, school_id) %>% 
    left_join(school_offers, ., by = "student_id")   %>% 
    mutate(school_id = replace_na(school_id, unmatched_value))  
  
  
  if(time == TRUE) {
    end_time <- Sys.time()
  elapsed_seconds <- as.numeric(end_time - start_time, units = "secs")
  
  cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  
  }
  
  
  return(school_offers)
  
}


Run_school_DA(school_db, apps_db, print = FALSE, time = TRUE)

### Loop it K times 

Loop_DA <- function(school_db, apps_db, n_reps,
                    time = FALSE) {
  
  if(time == TRUE) {
    start_time <- Sys.time()
  }
  
  
  results <- replicate(
    n_reps,
    Run_school_DA(school_db, apps_db, print = FALSE),
    simplify = FALSE
  )
  

  results_df <- bind_rows(results, .id = "sim_id") %>% 
    mutate(
      school_id = ifelse(school_id == -99, "unmatched", as.character(school_id))
    )
  
  probs <- results_df %>% 
    group_by(student_id, school_id) %>% 
    summarise(prob = n() / n_reps, .groups = "drop")
  
  
  if(time == TRUE) {
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(end_time - start_time, units = "secs")
    
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
    
  }
  
  return(probs)
  
}


Loop_DA(school_db, apps_db, 1000, time = TRUE)

# Loop_DA(school_db, apps_db, 1000, time = TRUE)
#Elapsed time: 44.836 seconds
