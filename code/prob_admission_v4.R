Run_school_DA_fast_CL_v4 <- function(
    school_db,
    apps_db,
    use_loteria_original,
    seed = NULL,
    print = FALSE,
    time = FALSE,
    quota_order = c("pie", "achievement", "prioritario", "regular", "continuity"),
    unused_quota_rule = c("roll_to_regular", "waste"),
    special_priority_rule = c("same_as_regular", "lottery_only"),
    achievement_rule = c("transition", "regime"),
    pie_rule = c("school_order_if_available", "regular_rule")
) {
  
  if (time) start_time <- proc.time()[["elapsed"]]
  if (!is.null(seed)) set.seed(seed)
  
  unused_quota_rule <- match.arg(unused_quota_rule)
  special_priority_rule <- match.arg(special_priority_rule)
  achievement_rule <- match.arg(achievement_rule)
  pie_rule <- match.arg(pie_rule)
  
  # ---- checks ----
  req_school <- c(
    "school_id",
    "pie_spots",
    "prioritario_spots",
    "achievement_spots",
    "regular_spots"
  )
  req_apps <- c(
    "student_id",
    "school_id",
    "student_pref",
    "priority_level",
    "is_pie",
    "is_prioritario",
    "is_high_achv"
  )
  
  if (!all(req_school %in% names(school_db))) {
    stop("school_db must contain: school_id, pie_spots, prioritario_spots, achievement_spots, regular_spots")
  }
  if (!all(req_apps %in% names(apps_db))) {
    stop("apps_db must contain: student_id, school_id, student_pref, priority_level, is_pie, is_prioritario, is_high_achv")
  }
  if (achievement_rule == "transition" && !("academic_order_transition" %in% names(apps_db))) {
    stop("apps_db must contain academic_order_transition when achievement_rule = 'transition'")
  }
  if (pie_rule == "school_order_if_available" && !("orden_pie" %in% names(apps_db))) {
    stop("apps_db must contain orden_pie when pie_rule = 'school_order_if_available'")
  }
  
  apps <- as.data.frame(apps_db, stringsAsFactors = FALSE)
  schools <- as.data.frame(school_db, stringsAsFactors = FALSE)
  
  if (is.factor(apps$school_id)) apps$school_id <- as.character(apps$school_id)
  if (is.factor(schools$school_id)) schools$school_id <- as.character(schools$school_id)
  
  apps$is_pie <- as.integer(apps$is_pie)
  apps$is_prioritario <- as.integer(apps$is_prioritario)
  apps$is_high_achv <- as.integer(apps$is_high_achv)
  apps$priority_level <- as.integer(apps$priority_level)
  apps$student_pref <- as.integer(apps$student_pref)
  
  if ("academic_order_transition" %in% names(apps)) {
    if (!all(is.na(apps$academic_order_transition) |
             apps$academic_order_transition == as.integer(apps$academic_order_transition))) {
      stop("apps_db$academic_order_transition must be integer-valued when non-missing")
    }
    apps$academic_order_transition <- as.integer(apps$academic_order_transition)
  }
  
  if ("orden_pie" %in% names(apps)) {
    if (!all(is.na(apps$orden_pie) | apps$orden_pie == as.integer(apps$orden_pie))) {
      stop("apps_db$orden_pie must be integer-valued when non-missing")
    }
    apps$orden_pie <- as.integer(apps$orden_pie)
  }
  
  # preserve original school_id output type
  school_id_col <- school_db$school_id
  if (is.factor(school_id_col)) school_id_col <- as.character(school_id_col)
  unmatched_value <- if (is.character(school_id_col)) "unmatched" else if (is.integer(school_id_col)) -99L else -99
  
  student_vals <- sort(unique(apps$student_id))
  n_students_total <- length(student_vals)
  
  if (nrow(apps) == 0L) {
    out <- data.frame(student_id = student_vals, school_id = unmatched_value)
    return(out)
  }
  
  # ---- lottery on original school applications ----
  if (use_loteria_original && "loteria_original" %in% names(apps)) {
    x <- apps$loteria_original
    if (anyNA(x)) stop("apps_db$loteria_original contains missing values")
    if (!is.numeric(x)) stop("apps_db$loteria_original must be numeric/integer")
    if (any(x != as.integer(x))) stop("apps_db$loteria_original must contain integers")
    apps$lottery_ticket <- as.integer(x)
    
    split_rows <- split(seq_len(nrow(apps)), apps$school_id)
    for (rows in split_rows) {
      xschool <- apps$lottery_ticket[rows]
      xschool <- xschool[!is.na(xschool)]
      if (length(xschool) > 0L && anyDuplicated(xschool)) {
        stop("apps_db$loteria_original contains duplicate non-missing values within a school_id")
      }
    }
  } else {
    apps$lottery_ticket <- integer(nrow(apps))
    split_rows <- split(seq_len(nrow(apps)), apps$school_id)
    for (rows in split_rows) {
      apps$lottery_ticket[rows] <- sample.int(length(rows))
    }
  }
  
  # ---- validate school-specific orders where present ----
  if (achievement_rule == "transition") {
    split_rows <- split(seq_len(nrow(apps)), apps$school_id)
    for (rows in split_rows) {
      x <- apps$academic_order_transition[rows]
      x <- x[!is.na(x)]
      if (length(x) > 0L && anyDuplicated(x)) {
        stop("Duplicate academic_order_transition within at least one school_id")
      }
    }
  }
  
  if (pie_rule == "school_order_if_available") {
    split_rows <- split(seq_len(nrow(apps)), apps$school_id)
    for (rows in split_rows) {
      x <- apps$orden_pie[rows]
      x <- x[!is.na(x)]
      if (length(x) > 0L && anyDuplicated(x)) {
        stop("Duplicate orden_pie within at least one school_id")
      }
    }
  }
  
  # ---- expand schools into pseudo-schools ----
  qt <- c("pie", "achievement", "prioritario", "regular", "continuity")
  qcol <- c("pie_spots", "achievement_spots", "prioritario_spots", "regular_spots")
  
  if (!all(qt %in% quota_order)) {
    stop("quota_order must contain: pie, achievement, prioritario, regular, continuity")
  }
  
  school_list <- vector("list", length(qt))
  
  # standard quota pseudo-schools
  for (k in seq_along(qcol)) {
    idx <- schools[[qcol[k]]] > 0
    if (any(idx)) {
      school_list[[k]] <- data.frame(
        school_id = schools$school_id[idx],
        quota_type = qt[k],
        spots_available = as.integer(schools[[qcol[k]]][idx]),
        pseudo_school_id = paste0(schools$school_id[idx], "__", qt[k]),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # continuity pseudo-school: always create, effectively unlimited capacity
  school_list[[5]] <- data.frame(
    school_id = schools$school_id,
    quota_type = "continuity",
    spots_available = rep.int(n_students_total, nrow(schools)),
    pseudo_school_id = paste0(schools$school_id, "__continuity"),
    stringsAsFactors = FALSE
  )
  
  schools_expanded <- do.call(rbind, school_list)
  
  if (is.null(schools_expanded) || nrow(schools_expanded) == 0L) {
    out <- data.frame(student_id = student_vals, school_id = unmatched_value)
    return(out)
  }
  
  pseudo_vals <- schools_expanded$pseudo_school_id
  pseudo_capacity <- schools_expanded$spots_available
  names(pseudo_capacity) <- pseudo_vals
  
  pseudo_key <- paste(schools_expanded$school_id, schools_expanded$quota_type, sep = "___")
  pseudo_lookup <- schools_expanded$pseudo_school_id
  names(pseudo_lookup) <- pseudo_key
  
  quota_rank_map <- match(qt, quota_order)
  names(quota_rank_map) <- qt
  
  # ---- expand applications into eligible pseudo-school applications ----
  app_blocks <- vector("list", 5L)
  
  # regular: everyone
  app_blocks[[1]] <- data.frame(
    student_id = unname(apps$student_id),
    school_id = unname(apps$school_id),
    student_pref = unname(apps$student_pref),
    priority_level = unname(apps$priority_level),
    lottery_ticket = unname(apps$lottery_ticket),
    achievement_transition_order = NA_integer_,
    pie_order = NA_integer_,
    quota_type = "regular",
    quota_rank = quota_rank_map["regular"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  # pie
  if (pie_rule == "school_order_if_available") {
    idx <- "orden_pie" %in% names(apps) & !is.na(apps$orden_pie)
  } else {
    idx <- apps$is_pie == 1L
  }
  
  if (any(idx)) {
    app_blocks[[2]] <- data.frame(
      student_id = unname(apps$student_id[idx]),
      school_id = unname(apps$school_id[idx]),
      student_pref = unname(apps$student_pref[idx]),
      priority_level = unname(apps$priority_level[idx]),
      lottery_ticket = unname(apps$lottery_ticket[idx]),
      achievement_transition_order = NA_integer_,
      pie_order = if ("orden_pie" %in% names(apps)) unname(apps$orden_pie[idx]) else NA_integer_,
      quota_type = "pie",
      quota_rank = quota_rank_map["pie"],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  
  # achievement
  if (achievement_rule == "regime") {
    idx <- apps$is_high_achv == 1L
    if (any(idx)) {
      app_blocks[[3]] <- data.frame(
        student_id = unname(apps$student_id[idx]),
        school_id = unname(apps$school_id[idx]),
        student_pref = unname(apps$student_pref[idx]),
        priority_level = unname(apps$priority_level[idx]),
        lottery_ticket = unname(apps$lottery_ticket[idx]),
        achievement_transition_order = NA_integer_,
        pie_order = NA_integer_,
        quota_type = "achievement",
        quota_rank = quota_rank_map["achievement"],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  } else {
    idx <- !is.na(apps$academic_order_transition)
    if (any(idx)) {
      app_blocks[[3]] <- data.frame(
        student_id = unname(apps$student_id[idx]),
        school_id = unname(apps$school_id[idx]),
        student_pref = unname(apps$student_pref[idx]),
        priority_level = unname(apps$priority_level[idx]),
        lottery_ticket = unname(apps$lottery_ticket[idx]),
        achievement_transition_order = unname(apps$academic_order_transition[idx]),
        pie_order = NA_integer_,
        quota_type = "achievement",
        quota_rank = quota_rank_map["achievement"],
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  }
  
  # prioritario
  idx <- apps$is_prioritario == 1L
  if (any(idx)) {
    app_blocks[[4]] <- data.frame(
      student_id = unname(apps$student_id[idx]),
      school_id = unname(apps$school_id[idx]),
      student_pref = unname(apps$student_pref[idx]),
      priority_level = unname(apps$priority_level[idx]),
      lottery_ticket = unname(apps$lottery_ticket[idx]),
      achievement_transition_order = NA_integer_,
      pie_order = NA_integer_,
      quota_type = "prioritario",
      quota_rank = quota_rank_map["prioritario"],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  
  # continuity fallback: only priority_level == 4
  idx <- apps$priority_level == 4L
  if (any(idx)) {
    app_blocks[[5]] <- data.frame(
      student_id = unname(apps$student_id[idx]),
      school_id = unname(apps$school_id[idx]),
      student_pref = unname(apps$student_pref[idx]),
      priority_level = unname(apps$priority_level[idx]),
      lottery_ticket = unname(apps$lottery_ticket[idx]),
      achievement_transition_order = NA_integer_,
      pie_order = NA_integer_,
      quota_type = "continuity",
      quota_rank = quota_rank_map["continuity"],
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  
  apps_expanded <- do.call(rbind, app_blocks)
  
  # keep only pseudo-schools that exist
  keys <- paste(apps_expanded$school_id, apps_expanded$quota_type, sep = "___")
  keep <- keys %in% names(pseudo_lookup)
  apps_expanded <- apps_expanded[keep, , drop = FALSE]
  keys <- keys[keep]
  apps_expanded$pseudo_school_id <- unname(pseudo_lookup[keys])
  
  if (nrow(apps_expanded) == 0L) {
    out <- data.frame(student_id = student_vals, school_id = unmatched_value)
    return(out)
  }
  
  # priority used within pseudo-school
  if (special_priority_rule == "lottery_only") {
    apps_expanded$priority_for_match <- ifelse(
      apps_expanded$quota_type == "regular",
      apps_expanded$priority_level,
      0L
    )
  } else {
    apps_expanded$priority_for_match <- apps_expanded$priority_level
  }
  
  # order within each student: original pref then quota order
  ord <- order(apps_expanded$student_id, apps_expanded$student_pref, apps_expanded$quota_rank)
  apps_expanded <- apps_expanded[ord, , drop = FALSE]
  
  # integer coding
  student_index_vals <- sort(unique(apps_expanded$student_id))
  student_idx <- match(apps_expanded$student_id, student_index_vals)
  pseudo_index_vals <- pseudo_vals
  pseudo_idx <- match(apps_expanded$pseudo_school_id, pseudo_index_vals)
  
  # original school for collapsed output
  school_id_raw <- apps_expanded$school_id
  
  # expanded pref index per student
  app_counts <- tabulate(student_idx, nbins = length(student_index_vals))
  start_pos <- cumsum(c(1L, head(app_counts, -1L)))
  
  priority <- as.integer(apps_expanded$priority_for_match)
  lottery <- as.integer(apps_expanded$lottery_ticket)
  trans_order <- apps_expanded$achievement_transition_order
  pie_order <- apps_expanded$pie_order
  quota_type <- apps_expanded$quota_type
  
  # ---- DA core ----
  run_da_core <- function(capacity_vec, verbose = FALSE) {
    n_students <- length(student_index_vals)
    cur_pos <- rep.int(1L, n_students)
    held_rows <- integer(0)
    round_rejected <- -1L
    
    while (round_rejected != 0L) {
      held_students <- if (length(held_rows)) student_idx[held_rows] else integer(0)
      
      active_students <- which(cur_pos <= app_counts)
      if (length(held_students)) {
        active_students <- active_students[!(active_students %in% held_students)]
      }
      if (!length(active_students)) break
      
      current_rows <- start_pos[active_students] + cur_pos[active_students] - 1L
      competing_rows <- c(held_rows, current_rows)
      comp_school <- pseudo_idx[competing_rows]
      comp_quota <- quota_type[competing_rows]
      
      transition_mask <- (comp_quota == "achievement" & achievement_rule == "transition")
      pie_mask <- (comp_quota == "pie" &
                     pie_rule == "school_order_if_available" &
                     !is.na(pie_order[competing_rows]))
      
      # ranking key:
      # - achievement transition: use transition order
      # - pie with school order: use orden_pie
      # - otherwise: priority desc, lottery asc
      ord_round <- order(
        comp_school,
        ifelse(transition_mask | pie_mask, 0L, -priority[competing_rows]),
        ifelse(
          transition_mask,
          trans_order[competing_rows],
          ifelse(pie_mask, pie_order[competing_rows], lottery[competing_rows])
        ),
        lottery[competing_rows]
      )
      
      ranked_rows <- competing_rows[ord_round]
      ranked_school <- pseudo_idx[ranked_rows]
      
      first_in_school <- !duplicated(ranked_school)
      school_starts <- which(first_in_school)
      school_lengths <- diff(c(school_starts, length(ranked_school) + 1L))
      rank_within_school <- sequence(school_lengths)
      
      caps <- capacity_vec[ ranked_school[school_starts] ]
      cap_expanded <- rep.int(caps, school_lengths)
      
      keep_mask <- rank_within_school <= cap_expanded
      kept_rows <- ranked_rows[keep_mask]
      rejected_rows <- ranked_rows[!keep_mask]
      rejected_students <- unique(student_idx[rejected_rows])
      
      held_rows <- kept_rows
      round_rejected <- length(rejected_students)
      
      if (round_rejected > 0L) {
        cur_pos[rejected_students] <- cur_pos[rejected_students] + 1L
      }
      
      if (verbose) {
        message("Number of rejections this round: ", round_rejected)
      }
    }
    
    list(held_rows = held_rows, cur_pos = cur_pos)
  }
  
  cap1 <- as.integer(pseudo_capacity)
  res1 <- run_da_core(cap1, verbose = print)
  final_rows <- res1$held_rows
  
  # ---- optionally roll unused special quota seats into regular ----
  if (unused_quota_rule == "roll_to_regular") {
    filled_counts <- integer(length(pseudo_vals))
    if (length(final_rows)) {
      filled_tab <- tabulate(pseudo_idx[final_rows], nbins = length(pseudo_vals))
      filled_counts[] <- filled_tab
    }
    
    unused <- pmax(cap1 - filled_counts, 0L)
    
    quota_type_by_pseudo <- schools_expanded$quota_type
    school_by_pseudo <- schools_expanded$school_id
    
    extra_regular_by_school <- tapply(
      unused[quota_type_by_pseudo %in% c("pie", "achievement", "prioritario")],
      school_by_pseudo[quota_type_by_pseudo %in% c("pie", "achievement", "prioritario")],
      sum
    )
    
    cap2 <- cap1
    reg_idx <- which(quota_type_by_pseudo == "regular")
    if (length(reg_idx)) {
      extras <- extra_regular_by_school[school_by_pseudo[reg_idx]]
      extras[is.na(extras)] <- 0L
      cap2[reg_idx] <- cap2[reg_idx] + as.integer(extras)
    }
    
    res2 <- run_da_core(cap2, verbose = print)
    final_rows <- res2$held_rows
  }
  
  # ---- collapse back to original school_id ----
  assigned_student_idx <- if (length(final_rows)) student_idx[final_rows] else integer(0)
  assigned_school <- rep(unmatched_value, n_students_total)
  
  if (length(final_rows)) {
    matched_students <- student_index_vals[assigned_student_idx]
    matched_school <- school_id_raw[final_rows]
    pos <- match(matched_students, student_vals)
    assigned_school[pos] <- matched_school
  }
  
  out <- data.frame(
    student_id = student_vals,
    school_id = assigned_school,
    stringsAsFactors = FALSE
  )
  
  if (time) {
    elapsed_seconds <- proc.time()[["elapsed"]] - start_time
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  out
}



Loop_DA_fast <- function(
    school_db,
    apps_db,
    n_reps,
    seed = NULL,
    time = FALSE,
    quota_order = c("pie", "achievement", "prioritario", "regular", "continuity"),
    unused_quota_rule = c("roll_to_regular", "waste"),
    special_priority_rule = c("same_as_regular", "lottery_only"),
    achievement_rule = c("transition", "regime"),
    pie_rule = c("school_order_if_available", "regular_rule")
) {
  
  unused_quota_rule <- match.arg(unused_quota_rule)
  special_priority_rule <- match.arg(special_priority_rule)
  achievement_rule <- match.arg(achievement_rule)
  pie_rule <- match.arg(pie_rule)
  
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
    
    sim_result <- Run_school_DA_fast_CL_v4(
      school_db = school_db,
      apps_db = apps_db,
      use_loteria_original = FALSE,
      seed = sim_seed,
      print = FALSE,
      time = FALSE,
      quota_order = quota_order,
      unused_quota_rule = unused_quota_rule,
      special_priority_rule = special_priority_rule,
      achievement_rule = achievement_rule,
      pie_rule = pie_rule
    )
    
    school_chr <- as.character(sim_result$school_id)
    school_chr[is.na(school_chr)] <- "unmatched"
    school_chr[school_chr == as.character(unmatched_value)] <- "unmatched"
    
    keys <- paste(sim_result$student_id, school_chr, sep = "___")
    tab <- table(keys)
    
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
  
  if (is.integer(school_db$school_id)) {
    school_out <- ifelse(school_out == "unmatched", NA, school_out)
    school_out <- as.integer(school_out)
  } else if (is.numeric(school_db$school_id)) {
    school_out <- ifelse(school_out == "unmatched", NA, school_out)
    school_out <- as.numeric(school_out)
  }
  
  probs <- data.frame(
    student_id = student_out,
    school_id = school_out,
    prob = as.numeric(counts) / n_reps,
    stringsAsFactors = FALSE
  )
  
  if ("school_name" %in% names(school_db)) {
    school_map <- school_db %>%
      distinct(school_id, school_name) %>%
      mutate(school_id_chr = as.character(school_id))
    
    probs <- probs %>%
      mutate(school_id_chr = as.character(school_id)) %>%
      left_join(school_map, by = "school_id_chr") %>%
      mutate(
        school_name = if_else(school_id_chr == "unmatched", "unmatched", school_name)
      ) %>%
      select(student_id, school_id, school_name, prob)
  }
  
  probs <- probs[order(probs$student_id, probs$school_id), ]
  rownames(probs) <- NULL
  
  if (time) {
    elapsed_seconds <- proc.time()[["elapsed"]] - start_time
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  probs
}