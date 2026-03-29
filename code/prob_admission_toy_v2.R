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

#toy 1
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



#toy 2
school_db2 <- data.frame(
  school_id = c(41,52,63), 
  school_name = c("a","b","c"),
  spots_available = c(1,1,1)
)

apps_db2 <- data.frame(
  student_id     = c(1,2,2,2,3,3,4),
  school_id      = c(63,63,52,41,52,41,41),
  student_pref   = c(1,1,2,3,1,2,1),
  priority_level = c(0,0,0,0,0,0,0)
)



#### Run the algorithm once 
Run_school_DA <- function(school_db, apps_db, 
                          seed = NULL, print = FALSE, time = FALSE,
                          lottery_type = c("by_school", "single")) {
  
  lottery_type <- match.arg(lottery_type)
  
  if (time == TRUE) {
    start_time <- Sys.time()
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # Fixed applications table with lottery tickets
  if (lottery_type == "by_school") {
    apps_state <- apps_db %>%
      group_by(school_id) %>%
      mutate(lottery_ticket = sample.int(n())) %>%
      ungroup() %>%
      arrange(student_id, student_pref)
    
  } else if (lottery_type == "single") {
    student_lottery <- data.frame(
      student_id = sort(unique(apps_db$student_id)),
      lottery_ticket = sample.int(length(unique(apps_db$student_id)))
    )
    
    apps_state <- apps_db %>%
      left_join(student_lottery, by = "student_id") %>%
      arrange(student_id, student_pref)
  }
  
  school_offers <- apps_db %>% 
    select(student_id) %>% 
    unique()
  
  # Student-level pointer state
  student_state <- apps_db %>%
    group_by(student_id) %>%
    summarise(max_pref = max(student_pref), .groups = "drop") %>%
    mutate(cur_pref = 1L)
  
  round_rejected <- -999L
  
  while (round_rejected != 0) {  
    
    round_rejected <- 0L
    
    cur_app_proposal <- student_state %>%
      filter(cur_pref <= max_pref) %>%
      inner_join(
        apps_state,
        by = c("student_id", "cur_pref" = "student_pref")
      )
    
    if (nrow(cur_app_proposal) == 0) break
    
    cur_ranked <- cur_app_proposal %>%
      left_join(
        school_db %>% select(school_id, spots_available),
        by = "school_id"
      ) %>%
      group_by(school_id) %>%
      arrange(desc(priority_level), lottery_ticket, .by_group = TRUE) %>%
      mutate(rank_within_school = row_number()) %>%
      ungroup()
    
    rejected_ids <- cur_ranked %>%
      filter(rank_within_school > spots_available) %>%
      pull(student_id)
    
    round_rejected <- length(rejected_ids)
    
    if (round_rejected > 0) {
      rejected_rows <- student_state$student_id %in% rejected_ids
      student_state$cur_pref[rejected_rows] <- student_state$cur_pref[rejected_rows] + 1L
    }
    
    if (print == TRUE) {
      print(paste("Number of rejections this round:", round_rejected))
    }
  }
  
  if (is.character(school_db$school_id)) {
    unmatched_value <- "unmatched"
  } else if (is.integer(school_db$school_id)) {
    unmatched_value <- -99L
  } else {
    unmatched_value <- -99
  }
  
  final_proposals <- student_state %>%
    filter(cur_pref <= max_pref) %>%
    inner_join(
      apps_state,
      by = c("student_id", "cur_pref" = "student_pref")
    ) %>%
    select(student_id, school_id)
  
  school_offers <- school_offers %>%
    left_join(final_proposals, by = "student_id") %>%
    mutate(school_id = replace_na(school_id, unmatched_value))
  
  if (time == TRUE) {
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(end_time - start_time, units = "secs")
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  return(school_offers)
}


Run_school_DA_fast <- function(school_db, apps_db,
                               seed = NULL,
                               print = FALSE,
                               time = FALSE,
                               lottery_type = c("by_school", "single")) {
  
  lottery_type <- match.arg(lottery_type)
  
  if (time) {
    start_time <- proc.time()[["elapsed"]]
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # ---- basic checks ----
  req_school <- c("school_id", "spots_available")
  req_apps   <- c("student_id", "school_id", "student_pref", "priority_level")
  
  if (!all(req_school %in% names(school_db))) {
    stop("school_db must contain: school_id, spots_available")
  }
  if (!all(req_apps %in% names(apps_db))) {
    stop("apps_db must contain: student_id, school_id, student_pref, priority_level")
  }
  
  n_apps <- nrow(apps_db)
  if (n_apps == 0L) {
    out <- data.frame(student_id = unique(apps_db$student_id), school_id = numeric(0))
    return(out)
  }
  
  # ---- preserve output type for school_id ----
  school_id_col <- school_db$school_id
  if (is.factor(school_id_col)) school_id_col <- as.character(school_id_col)
  
  unmatched_value <- if (is.character(school_id_col)) {
    "unmatched"
  } else if (is.integer(school_id_col)) {
    -99L
  } else {
    -99
  }
  
  # ---- copy and normalize ids ----
  apps <- apps_db
  schools <- school_db
  
  if (is.factor(apps$school_id))    apps$school_id <- as.character(apps$school_id)
  if (is.factor(schools$school_id)) schools$school_id <- as.character(schools$school_id)
  
  # student coding
  student_vals <- sort(unique(apps$student_id))
  n_students   <- length(student_vals)
  student_idx  <- match(apps$student_id, student_vals)
  
  # school coding based on school_db
  school_vals <- schools$school_id
  n_schools   <- length(school_vals)
  school_idx  <- match(apps$school_id, school_vals)
  
  if (anyNA(school_idx)) {
    stop("Some apps_db$school_id values are not present in school_db$school_id")
  }
  
  capacity <- schools$spots_available
  if (length(capacity) != n_schools) stop("Invalid school capacities")
  
  # ---- sort apps once by student then preference ----
  ord_student_pref <- order(student_idx, apps$student_pref)
  
  student_idx   <- student_idx[ord_student_pref]
  school_idx    <- school_idx[ord_student_pref]
  priority      <- apps$priority_level[ord_student_pref]
  school_id_raw <- apps$school_id[ord_student_pref]
  
  # ---- lottery tickets ----
  # smaller lottery ticket is better
  lottery <- integer(n_apps)
  
  if ("loteria_original" %in% names(apps)) {
    x <- apps$loteria_original[ord_student_pref]
    
    if (anyNA(x)) {
      stop("apps_db$loteria_original contains missing values")
    }
    if (!is.numeric(x)) {
      stop("apps_db$loteria_original must be numeric/integer")
    }
    if (any(x != as.integer(x))) {
      stop("apps_db$loteria_original must contain integers")
    }
    
    lottery <- as.integer(x)
    
    if (lottery_type == "by_school") {
      school_rows <- split(seq_len(n_apps), school_idx)
      for (rows in school_rows) {
        vals <- lottery[rows]
        if (anyDuplicated(vals)) {
          stop("apps_db$loteria_original contains duplicate lottery values within a school")
        }
      }
    } else if (lottery_type == "single") {
      student_rows <- split(seq_len(n_apps), student_idx)
      student_lot <- vapply(student_rows, function(rows) {
        vals <- unique(lottery[rows])
        if (length(vals) != 1L) {
          stop("Under lottery_type = 'single', each student must have exactly one unique loteria_original across applications")
        }
        vals
      }, integer(1))
    }
    
  } else {
    if (lottery_type == "by_school") {
      school_rows <- split(seq_len(n_apps), school_idx)
      for (rows in school_rows) {
        lottery[rows] <- sample.int(length(rows))
      }
    } else if (lottery_type == "single") {
      student_lottery <- sample.int(n_students)
      lottery <- student_lottery[student_idx]
    }
  }
  
  # ---- student pointer structure ----
  app_counts <- tabulate(student_idx, nbins = n_students)
  start_pos  <- cumsum(c(1L, head(app_counts, -1L)))
  cur_pos    <- rep.int(1L, n_students)
  
  # ---- DA loop ----
  round_rejected <- -1L
  
  while (round_rejected != 0L) {
    
    active_students <- which(cur_pos <= app_counts)
    if (length(active_students) == 0L) break
    
    # current application row for each active student
    current_rows <- start_pos[active_students] + cur_pos[active_students] - 1L
    
    # rank current proposers within school:
    # school ascending, priority descending, lottery ascending
    ord_round <- order(
      school_idx[current_rows],
      -priority[current_rows],
      lottery[current_rows]
    )
    
    ranked_rows     <- current_rows[ord_round]
    ranked_students <- student_idx[ranked_rows]
    ranked_schools  <- school_idx[ranked_rows]
    
    # rank within school after sorting
    first_in_school <- !duplicated(ranked_schools)
    school_starts   <- which(first_in_school)
    school_lengths  <- diff(c(school_starts, length(ranked_schools) + 1L))
    
    rank_within_school <- sequence(school_lengths)
    school_for_block   <- ranked_schools[school_starts]
    cap_for_ranked     <- capacity[school_for_block]
    cap_expanded       <- rep.int(cap_for_ranked, school_lengths)
    
    rejected_mask <- rank_within_school > cap_expanded
    rejected_students <- ranked_students[rejected_mask]
    
    round_rejected <- length(rejected_students)
    
    if (print) {
      message("Number of rejections this round: ", round_rejected)
    }
    
    if (round_rejected > 0L) {
      cur_pos[rejected_students] <- cur_pos[rejected_students] + 1L
    }
  }
  
  # ---- final assignments ----
  final_active <- which(cur_pos <= app_counts)
  
  assigned_school <- vector(mode = typeof(unmatched_value), length = n_students)
  assigned_school[] <- unmatched_value
  
  if (length(final_active) > 0L) {
    final_rows <- start_pos[final_active] + cur_pos[final_active] - 1L
    
    if (is.character(unmatched_value)) {
      assigned_school[final_active] <- as.character(school_id_raw[final_rows])
    } else if (is.integer(unmatched_value)) {
      assigned_school[final_active] <- as.integer(school_id_raw[final_rows])
    } else {
      assigned_school[final_active] <- as.numeric(school_id_raw[final_rows])
    }
  }
  
  out <- data.frame(
    student_id = student_vals,
    school_id  = assigned_school,
    stringsAsFactors = FALSE
  )
  
  if (time) {
    elapsed_seconds <- proc.time()[["elapsed"]] - start_time
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  out
}


Loop_DA <- function(school_db, apps_db, n_reps,
                    time = FALSE,
                    lottery_type = c("by_school", "single")) {
  
  lottery_type <- match.arg(lottery_type)
  
  if(time == TRUE) {
    start_time <- Sys.time()
  }
  
  results <- replicate(
    n_reps,
    Run_school_DA(
      school_db,
      apps_db,
      print = FALSE,
      lottery_type = lottery_type
    ),
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


Loop_DA_fast <- function(school_db, apps_db, n_reps,
                         seed = NULL,
                         time = FALSE,
                         lottery_type = c("by_school", "single")) {
  
  lottery_type <- match.arg(lottery_type)
  
  if (time) {
    start_time <- proc.time()[["elapsed"]]
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  student_vals <- sort(unique(apps_db$student_id))
  
  school_id_col <- school_db$school_id
  if (is.factor(school_id_col)) school_id_col <- as.character(school_id_col)
  
  unmatched_value <- if (is.character(school_id_col)) {
    "unmatched"
  } else if (is.integer(school_id_col)) {
    -99L
  } else {
    -99
  }
  
  counts <- integer(0)
  
  for (r in seq_len(n_reps)) {
    
    sim_seed <- sample.int(.Machine$integer.max, 1L)
    
    sim_result <- Run_school_DA_fast(
      school_db = school_db,
      apps_db   = apps_db,
      seed      = sim_seed,
      print     = FALSE,
      time      = FALSE,
      lottery_type = lottery_type
    )
    
    school_chr <- as.character(sim_result$school_id)
    school_chr[is.na(school_chr)] <- "unmatched"
    school_chr[school_chr == as.character(unmatched_value)] <- "unmatched"
    
    keys <- paste(sim_result$student_id, school_chr, sep = "___")
    tab  <- table(keys)
    
    nm <- names(tab)
    if (length(counts) == 0L) {
      counts <- as.integer(tab)
      names(counts) <- nm
    } else {
      new_nm <- setdiff(nm, names(counts))
      if (length(new_nm) > 0L) {
        counts <- c(counts, setNames(integer(length(new_nm)), new_nm))
      }
      counts[nm] <- counts[nm] + as.integer(tab)
    }
  }
  
  key_names <- names(counts)
  parts <- strsplit(key_names, "___", fixed = TRUE)
  
  student_out <- vapply(parts, `[`, character(1), 1L)
  school_out  <- vapply(parts, `[`, character(1), 2L)
  
  if (is.integer(apps_db$student_id)) {
    student_out <- as.integer(student_out)
  } else if (is.numeric(apps_db$student_id)) {
    student_out <- as.numeric(student_out)
  }
  
  probs <- data.frame(
    student_id = student_out,
    school_id  = school_out,
    prob       = as.numeric(counts) / n_reps,
    stringsAsFactors = FALSE
  )
  
  probs <- probs[order(probs$student_id, probs$school_id), ]
  rownames(probs) <- NULL
  
  if (time) {
    elapsed_seconds <- proc.time()[["elapsed"]] - start_time
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  probs
}


Loop_DA_fast(school_db, apps_db, 10000, time = TRUE, lottery_type = "single")
Loop_DA_fast(school_db2, apps_db2, 10000, time = TRUE, lottery_type = "single")


expand_market <- function(school_db, apps_db, n_rep_types) {
  if (n_rep_types < 1) stop("n_rep_types must be >= 1")
  
  # map original students to 1..K types
  base_students <- sort(unique(apps_db$student_id))
  K <- length(base_students)
  
  apps_rep <- do.call(
    rbind,
    lapply(seq_len(n_rep_types), function(r) {
      tmp <- apps_db
      # new student ids: type k in replication r
      tmp$student_id <- match(tmp$student_id, base_students) + (r - 1L) * K
      tmp
    })
  )
  
  school_rep <- school_db
  school_rep$spots_available <- school_rep$spots_available * n_rep_types
  
  list(
    school_db = school_rep,
    apps_db = apps_rep
  )
}


Loop_DA_large_market <- function(school_db, apps_db,
                                 n_rep_types,
                                 n_sims = 10000,
                                 lottery_type = c("single", "by_school"),
                                 time = FALSE) {
  
  lottery_type <- match.arg(lottery_type)
  
  expanded <- expand_market(school_db, apps_db, n_rep_types)
  
  probs_big <- Loop_DA_fast(
    school_db = expanded$school_db,
    apps_db = expanded$apps_db,
    n_reps = n_sims,
    lottery_type = lottery_type,
    time = time
  )
  
  # collapse back to original "type"
  K <- length(unique(apps_db$student_id))
  probs_big$type_id <- ((as.integer(probs_big$student_id) - 1L) %% K) + 1L
  
  out <- probs_big %>%
    dplyr::group_by(type_id, school_id) %>%
    dplyr::summarise(prob = mean(prob), .groups = "drop")
  
  if ("school_name" %in% names(school_db)) {
    school_map <- school_db %>%
      dplyr::distinct(school_id, school_name) %>%
      dplyr::mutate(school_id = as.character(school_id))
    
    out <- out %>%
      dplyr::mutate(school_id_chr = as.character(school_id)) %>%
      dplyr::left_join(school_map, by = c("school_id_chr" = "school_id")) %>%
      dplyr::mutate(
        school_name = dplyr::if_else(school_id_chr == "unmatched", "unmatched", school_name)
      ) %>%
      dplyr::select(type_id, school_id, school_name, prob)
  }
  
  out[order(out$type_id, out$school_id), ]
}


# finite-market simulation
Loop_DA_fast(school_db2, apps_db2, 10000, lottery_type = "single", time = TRUE)

# replicated economy, e.g. n = 50 copies of each type
Loop_DA_large_market(school_db2, apps_db2,
                     n_rep_types = 10,
                     n_sims = 100000,
                     lottery_type = "single",
                     time = TRUE)
