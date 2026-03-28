library(tidyverse)

#######################################
# Proper DA (fast version)
#######################################

Run_school_DA <- function(school_db, apps_db,
                          seed = NULL,
                          print = FALSE,
                          time = FALSE,
                          lottery_type = c("by_school", "single")) {
  Run_school_DA_fast(
    school_db = school_db,
    apps_db = apps_db,
    seed = seed,
    print = print,
    time = time,
    lottery_type = lottery_type
  )
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
  
  # ---- integer coding ----
  student_vals <- sort(unique(apps$student_id))
  n_students   <- length(student_vals)
  student_idx0 <- match(apps$student_id, student_vals)
  
  school_vals <- schools$school_id
  n_schools   <- length(school_vals)
  school_idx0 <- match(apps$school_id, school_vals)
  
  if (anyNA(school_idx0)) {
    stop("Some apps_db$school_id values are not present in school_db$school_id")
  }
  
  capacity <- as.integer(schools$spots_available)
  if (length(capacity) != n_schools) stop("Invalid school capacities")
  
  # ---- sort apps once by student then preference ----
  ord_student_pref <- order(student_idx0, apps$student_pref)
  
  student_idx <- student_idx0[ord_student_pref]
  school_idx  <- school_idx0[ord_student_pref]
  priority    <- as.integer(apps$priority_level[ord_student_pref])
  school_id_raw <- apps$school_id[ord_student_pref]
  
  # ---- lottery tickets ----
  # smaller lottery ticket is better
  lottery <- integer(n_apps)
  
  if ("loteria_original" %in% names(apps)) {
    x <- apps$loteria_original[ord_student_pref]
    
    if (anyNA(x)) stop("apps_db$loteria_original contains missing values")
    if (!is.numeric(x)) stop("apps_db$loteria_original must be numeric/integer")
    if (any(x != as.integer(x))) stop("apps_db$loteria_original must contain integers")
    
    lottery <- as.integer(x)
    
    if (lottery_type == "by_school") {
      school_rows <- split(seq_len(n_apps), school_idx)
      for (rows in school_rows) {
        vals <- lottery[rows]
        if (anyDuplicated(vals)) {
          stop("apps_db$loteria_original contains duplicate lottery values within a school")
        }
      }
    } else {
      student_rows <- split(seq_len(n_apps), student_idx)
      invisible(vapply(student_rows, function(rows) {
        vals <- unique(lottery[rows])
        if (length(vals) != 1L) {
          stop("Under lottery_type = 'single', each student must have exactly one unique loteria_original across applications")
        }
        vals[1]
      }, integer(1)))
    }
    
  } else {
    if (lottery_type == "by_school") {
      school_rows <- split(seq_len(n_apps), school_idx)
      for (rows in school_rows) {
        lottery[rows] <- sample.int(length(rows))
      }
    } else {
      student_lottery <- sample.int(n_students)
      lottery <- student_lottery[student_idx]
    }
  }
  
  # ---- student pointer structure ----
  # app_counts[s] = how many choices student s listed
  app_counts <- tabulate(student_idx, nbins = n_students)
  start_pos  <- cumsum(c(1L, head(app_counts, -1L)))
  cur_pos    <- rep.int(1L, n_students)   # pointer within each student's ranked list
  
  # current tentative hold: for each student, are they held?
  held_student <- rep(FALSE, n_students)
  held_row_by_student <- rep.int(NA_integer_, n_students)
  
  round_rejected <- -1L
  
  # ---- proper DA loop ----
  while (round_rejected != 0L) {
    
    # students who are not currently held and still have a school to try
    proposers <- which(!held_student & cur_pos <= app_counts)
    if (length(proposers) == 0L) break
    
    proposal_rows <- start_pos[proposers] + cur_pos[proposers] - 1L
    proposal_schools <- school_idx[proposal_rows]
    
    # only schools receiving a new proposal need updating this round
    touched_schools <- unique(proposal_schools)
    
    new_rejected_students <- integer(0)
    
    for (s in touched_schools) {
      # currently held at school s
      held_rows_s <- held_row_by_student[held_student]
      if (length(held_rows_s) > 0L) {
        held_rows_s <- held_rows_s[school_idx[held_rows_s] == s]
      }
      
      # new proposals to school s this round
      prop_rows_s <- proposal_rows[proposal_schools == s]
      
      # rank held + new together
      comp_rows <- c(held_rows_s, prop_rows_s)
      
      ord_s <- order(
        -priority[comp_rows],
        lottery[comp_rows]
      )
      ranked_rows <- comp_rows[ord_s]
      
      cap_s <- capacity[s]
      keep_n <- min(cap_s, length(ranked_rows))
      kept_rows_s <- if (keep_n > 0L) ranked_rows[seq_len(keep_n)] else integer(0)
      rej_rows_s  <- if (keep_n < length(ranked_rows)) ranked_rows[(keep_n + 1L):length(ranked_rows)] else integer(0)
      
      # reset currently held students at this school
      if (length(held_rows_s) > 0L) {
        old_students <- student_idx[held_rows_s]
        held_student[old_students] <- FALSE
        held_row_by_student[old_students] <- NA_integer_
      }
      
      # set new held students at this school
      if (length(kept_rows_s) > 0L) {
        kept_students <- student_idx[kept_rows_s]
        held_student[kept_students] <- TRUE
        held_row_by_student[kept_students] <- kept_rows_s
      }
      
      # rejected students advance pointer
      if (length(rej_rows_s) > 0L) {
        rej_students <- unique(student_idx[rej_rows_s])
        new_rejected_students <- c(new_rejected_students, rej_students)
      }
    }
    
    new_rejected_students <- unique(new_rejected_students)
    round_rejected <- length(new_rejected_students)
    
    if (round_rejected > 0L) {
      cur_pos[new_rejected_students] <- cur_pos[new_rejected_students] + 1L
    }
    
    if (print) {
      message("Number of rejections this round: ", round_rejected)
    }
  }
  
  # ---- final assignments ----
  assigned_school <- vector(mode = typeof(unmatched_value), length = n_students)
  assigned_school[] <- unmatched_value
  
  final_students <- which(held_student)
  if (length(final_students) > 0L) {
    final_rows <- held_row_by_student[final_students]
    
    if (is.character(unmatched_value)) {
      assigned_school[final_students] <- as.character(school_id_raw[final_rows])
    } else if (is.integer(unmatched_value)) {
      assigned_school[final_students] <- as.integer(school_id_raw[final_rows])
    } else {
      assigned_school[final_students] <- as.numeric(school_id_raw[final_rows])
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
  
  if (time) {
    start_time <- Sys.time()
  }
  
  results <- replicate(
    n_reps,
    Run_school_DA(
      school_db = school_db,
      apps_db = apps_db,
      print = FALSE,
      lottery_type = lottery_type
    ),
    simplify = FALSE
  )
  
  school_id_col <- school_db$school_id
  if (is.factor(school_id_col)) school_id_col <- as.character(school_id_col)
  
  unmatched_value <- if (is.character(school_id_col)) {
    "unmatched"
  } else if (is.integer(school_id_col)) {
    -99L
  } else {
    -99
  }
  
  results_df <- bind_rows(results, .id = "sim_id") %>% 
    mutate(
      school_id = ifelse(as.character(school_id) == as.character(unmatched_value),
                         "unmatched",
                         as.character(school_id))
    )
  
  probs <- results_df %>% 
    count(student_id, school_id, name = "n") %>%
    mutate(
      prob = n / n_reps,
      type_id = student_id
    ) %>%
    select(type_id, school_id, prob)
  
  if ("school_name" %in% names(school_db)) {
    school_map <- school_db %>%
      distinct(school_id, school_name) %>%
      mutate(school_id = as.character(school_id))
    
    probs <- probs %>%
      mutate(school_id_chr = as.character(school_id)) %>%
      left_join(school_map, by = c("school_id_chr" = "school_id")) %>%
      mutate(
        school_name = if_else(school_id_chr == "unmatched", "unmatched", school_name)
      ) %>%
      select(type_id, school_id, school_name, prob)
  }
  
  if (time) {
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(end_time - start_time, units = "secs")
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  probs
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
    type_id   = student_out,
    school_id = school_out,
    prob      = as.numeric(counts) / n_reps,
    stringsAsFactors = FALSE
  )
  
  if ("school_name" %in% names(school_db)) {
    school_map <- school_db %>%
      distinct(school_id, school_name) %>%
      mutate(school_id = as.character(school_id))
    
    probs <- probs %>%
      mutate(school_id_chr = as.character(school_id)) %>%
      left_join(school_map, by = c("school_id_chr" = "school_id")) %>%
      mutate(
        school_name = if_else(school_id_chr == "unmatched", "unmatched", school_name)
      ) %>%
      select(type_id, school_id, school_name, prob)
  }
  
  probs <- probs[order(probs$type_id, probs$school_id), ]
  rownames(probs) <- NULL
  
  if (time) {
    elapsed_seconds <- proc.time()[["elapsed"]] - start_time
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  probs
}

expand_market <- function(school_db, apps_db, n_rep_types) {
  if (n_rep_types < 1) stop("n_rep_types must be >= 1")
  
  base_students <- sort(unique(apps_db$student_id))
  K <- length(base_students)
  
  apps_rep <- do.call(
    rbind,
    lapply(seq_len(n_rep_types), function(r) {
      tmp <- apps_db
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
  
  K <- length(unique(apps_db$student_id))
  
  probs_big <- probs_big %>%
    mutate(type_id = ((as.integer(type_id) - 1L) %% K) + 1L)
  
  out <- probs_big %>%
    group_by(type_id, school_id) %>%
    summarise(prob = mean(prob), .groups = "drop")
  
  if ("school_name" %in% names(school_db)) {
    school_map <- school_db %>%
      distinct(school_id, school_name) %>%
      mutate(school_id = as.character(school_id))
    
    out <- out %>%
      mutate(school_id_chr = as.character(school_id)) %>%
      left_join(school_map, by = c("school_id_chr" = "school_id")) %>%
      mutate(
        school_name = if_else(school_id_chr == "unmatched", "unmatched", school_name)
      ) %>%
      select(type_id, school_id, school_name, prob)
  } else {
    out <- out %>%
      select(type_id, school_id, prob)
  }
  
  out <- out[order(out$type_id, out$school_id), ]
  rownames(out) <- NULL
  
  out
}


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


####Tables 

table_toy1 <- Loop_DA_fast(
  school_db, apps_db, 100000,
  lottery_type = "single",
  time = TRUE
) %>%
  arrange(school_name, type_id)

table_SM <- Loop_DA_fast(
  school_db2, apps_db2, 100000,
  lottery_type = "single",
  time = TRUE
) %>%
  arrange(school_name, type_id)

table_LM <- Loop_DA_large_market(
  school_db2, apps_db2,
  n_rep_types = 20,
  n_sims = 100000,
  lottery_type = "single",
  time = TRUE
) %>%
  arrange(school_name, type_id)