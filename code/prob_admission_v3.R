Run_school_DA_CL <- function(
    school_db,
    apps_db,
    seed = NULL,
    print = FALSE,
    time = FALSE,
    quota_order = c("pie", "achievement", "prioritario", "regular"),
    unused_quota_rule = c("waste", "roll_to_regular"),
    use_loteria_original = TRUE,
    special_priority_rule = c("same_as_regular", "lottery_only")
) {
  
  if (time) start_time <- Sys.time()
  if (!is.null(seed)) set.seed(seed)
  
  unused_quota_rule   <- match.arg(unused_quota_rule)
  special_priority_rule <- match.arg(special_priority_rule)
  
  # ---- required columns ----
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
  
  # ---- copy / normalize ----
  schools <- school_db
  apps <- apps_db
  
  if (is.factor(schools$school_id)) schools$school_id <- as.character(schools$school_id)
  if (is.factor(apps$school_id))    apps$school_id    <- as.character(apps$school_id)
  
  # force 0/1 eligibility indicators
  apps$is_pie          <- as.integer(apps$is_pie)
  apps$is_prioritario  <- as.integer(apps$is_prioritario)
  apps$is_high_achv    <- as.integer(apps$is_high_achv)
  apps$priority_level  <- as.integer(apps$priority_level)
  apps$student_pref    <- as.integer(apps$student_pref)
  
  # preserve output type for original school_id
  school_id_col <- school_db$school_id
  if (is.factor(school_id_col)) school_id_col <- as.character(school_id_col)
  
  unmatched_value <- if (is.character(school_id_col)) {
    "unmatched"
  } else if (is.integer(school_id_col)) {
    -99L
  } else {
    -99
  }
  
  # all students in output
  student_vals <- sort(unique(apps$student_id))
  
  if (nrow(apps) == 0L) {
    out <- data.frame(student_id = student_vals, school_id = unmatched_value)
    return(out)
  }
  
  # ---- build one lottery per ORIGINAL school application ----
  # This is inherited by all subschools of the same school application.
  if (use_loteria_original && "loteria_original" %in% names(apps)) {
    if (anyNA(apps$loteria_original)) {
      stop("apps_db$loteria_original contains missing values")
    }
    if (!is.numeric(apps$loteria_original)) {
      stop("apps_db$loteria_original must be numeric/integer")
    }
    if (any(apps$loteria_original != as.integer(apps$loteria_original))) {
      stop("apps_db$loteria_original must contain integers")
    }
    apps$lottery_ticket <- as.integer(apps$loteria_original)
    
    # check uniqueness within original school
    dup_check <- apps |>
      dplyr::group_by(school_id) |>
      dplyr::summarise(has_dup = any(duplicated(lottery_ticket)), .groups = "drop")
    
    if (any(dup_check$has_dup)) {
      stop("apps_db$loteria_original contains duplicate values within at least one school_id")
    }
  } else {
    # generate one independent lottery per original school
    apps <- apps |>
      dplyr::group_by(school_id) |>
      dplyr::mutate(lottery_ticket = sample.int(dplyr::n())) |>
      dplyr::ungroup()
  }
  
  # ---- expand schools into quota-specific pseudo-schools ----
  schools_expanded <- schools |>
    dplyr::transmute(
      school_id,
      pie           = as.integer(pie_spots),
      achievement   = as.integer(achievement_spots),
      prioritario   = as.integer(prioritario_spots),
      regular       = as.integer(regular_spots)
    ) |>
    tidyr::pivot_longer(
      cols = c("pie", "achievement", "prioritario", "regular"),
      names_to = "quota_type",
      values_to = "spots_available"
    ) |>
    dplyr::filter(spots_available > 0L) |>
    dplyr::mutate(
      pseudo_school_id = paste0(school_id, "__", quota_type)
    )
  
  if (nrow(schools_expanded) == 0L) {
    out <- data.frame(student_id = student_vals, school_id = unmatched_value)
    return(out)
  }
  
  # ---- expand applications to all eligible subschools ----
  # Every student applies to regular.
  # Eligible students also apply to pie / achievement / prioritario.
  apps_expanded <- apps |>
    dplyr::mutate(
      pie         = is_pie,
      achievement = is_high_achv,
      prioritario = is_prioritario,
      regular     = 1L
    ) |>
    tidyr::pivot_longer(
      cols = c("pie", "achievement", "prioritario", "regular"),
      names_to = "quota_type",
      values_to = "eligible"
    ) |>
    dplyr::filter(eligible == 1L) |>
    dplyr::mutate(
      quota_rank = match(quota_type, quota_order)
    ) |>
    dplyr::filter(!is.na(quota_rank)) |>
    dplyr::inner_join(
      schools_expanded |>
        dplyr::select(school_id, quota_type, pseudo_school_id),
      by = c("school_id", "quota_type")
    ) |>
    dplyr::arrange(student_id, student_pref, quota_rank) |>
    dplyr::group_by(student_id) |>
    dplyr::mutate(expanded_pref = dplyr::row_number()) |>
    dplyr::ungroup()
  
  if (nrow(apps_expanded) == 0L) {
    out <- data.frame(student_id = student_vals, school_id = unmatched_value)
    return(out)
  }
  
  # ---- priority used inside each pseudo-school ----
  # regular always uses priority_level
  # special quotas can either use the same ladder or lottery only
  apps_expanded <- apps_expanded |>
    dplyr::mutate(
      priority_for_match = dplyr::case_when(
        quota_type == "regular" ~ priority_level,
        special_priority_rule == "same_as_regular" ~ priority_level,
        special_priority_rule == "lottery_only" ~ 0L,
        TRUE ~ priority_level
      )
    )
  
  # ---- helper: one DA run for a given capacity table ----
  run_da_once <- function(apps_state, schools_state, verbose = FALSE) {
    
    # sort applications once by student + expanded preference
    apps_state <- apps_state |>
      dplyr::arrange(student_id, expanded_pref)
    
    # student pointer state
    student_state <- apps_state |>
      dplyr::group_by(student_id) |>
      dplyr::summarise(max_pref = max(expanded_pref), .groups = "drop") |>
      dplyr::mutate(cur_pref = 1L)
    
    # tentative holds
    tentative_current <- apps_state[0, ]
    
    round_rejected <- -1L
    
    while (round_rejected != 0L) {
      
      held_ids <- tentative_current$student_id
      
      active_students <- student_state |>
        dplyr::filter(cur_pref <= max_pref, !(student_id %in% held_ids))
      
      if (nrow(active_students) == 0L) break
      
      # each active student proposes to current pseudo-school
      cur_app_proposal <- active_students |>
        dplyr::inner_join(
          apps_state,
          by = c("student_id", "cur_pref" = "expanded_pref")
        )
      
      # combine new proposals with previous tentative holds
      all_competing <- dplyr::bind_rows(tentative_current, cur_app_proposal) |>
        dplyr::left_join(
          schools_state |>
            dplyr::select(pseudo_school_id, spots_available),
          by = "pseudo_school_id"
        )
      
      # rank within each pseudo-school
      ranked <- all_competing |>
        dplyr::group_by(pseudo_school_id) |>
        dplyr::arrange(
          dplyr::desc(priority_for_match),
          lottery_ticket,
          .by_group = TRUE
        ) |>
        dplyr::mutate(rank_within_school = dplyr::row_number()) |>
        dplyr::ungroup()
      
      kept <- ranked |>
        dplyr::filter(rank_within_school <= spots_available) |>
        dplyr::select(names(tentative_current))
      
      rejected <- ranked |>
        dplyr::filter(rank_within_school > spots_available) |>
        dplyr::pull(student_id) |>
        unique()
      
      round_rejected <- length(rejected)
      tentative_current <- kept
      
      if (round_rejected > 0L) {
        student_state$cur_pref[match(rejected, student_state$student_id)] <-
          student_state$cur_pref[match(rejected, student_state$student_id)] + 1L
      }
      
      if (verbose) {
        message("Number of rejections this round: ", round_rejected)
      }
    }
    
    list(
      tentative = tentative_current,
      student_state = student_state
    )
  }
  
  # ---- first pass: fixed quota capacities ----
  da_pass1 <- run_da_once(
    apps_state = apps_expanded,
    schools_state = schools_expanded,
    verbose = print
  )
  
  final_tentative <- da_pass1$tentative
  final_schools_expanded <- schools_expanded
  
  # ---- optional second pass: release unused special quota seats to regular ----
  if (unused_quota_rule == "roll_to_regular") {
    
    fill_summary <- final_tentative |>
      dplyr::count(school_id, quota_type, name = "filled") |>
      dplyr::right_join(
        schools_expanded |>
          dplyr::select(school_id, quota_type, pseudo_school_id, spots_available),
        by = c("school_id", "quota_type")
      ) |>
      dplyr::mutate(
        filled = tidyr::replace_na(filled, 0L),
        unused = pmax(spots_available - filled, 0L)
      )
    
    extra_regular <- fill_summary |>
      dplyr::filter(quota_type != "regular") |>
      dplyr::group_by(school_id) |>
      dplyr::summarise(extra_regular = sum(unused), .groups = "drop")
    
    final_schools_expanded <- schools_expanded |>
      dplyr::left_join(extra_regular, by = "school_id") |>
      dplyr::mutate(
        extra_regular = tidyr::replace_na(extra_regular, 0L),
        spots_available = dplyr::if_else(
          quota_type == "regular",
          spots_available + extra_regular,
          spots_available
        )
      ) |>
      dplyr::select(-extra_regular)
    
    # rerun from scratch with enlarged regular quotas
    da_pass2 <- run_da_once(
      apps_state = apps_expanded,
      schools_state = final_schools_expanded,
      verbose = print
    )
    
    final_tentative <- da_pass2$tentative
  }
  
  # ---- collapse pseudo-school assignment back to original school_id ----
  final_proposals <- final_tentative |>
    dplyr::select(student_id, school_id) |>
    distinct()
  
  school_offers <- data.frame(
    student_id = student_vals,
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(final_proposals, by = "student_id")
  
  school_offers$school_id[is.na(school_offers$school_id)] <- unmatched_value
  
  if (time) {
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(end_time - start_time, units = "secs")
    cat(sprintf("Elapsed time: %.3f seconds\n", elapsed_seconds))
  }
  
  school_offers
}

Run_school_DA_fast_CL <- function(
    school_db,
    apps_db,
    seed = NULL,
    print = FALSE,
    time = FALSE,
    quota_order = c("pie", "achievement", "prioritario", "regular"),
    unused_quota_rule = c("waste", "roll_to_regular"),
    use_loteria_original = TRUE,
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
      if (anyDuplicated(apps$lottery_ticket[rows])) {
        stop("apps_db$loteria_original contains duplicate values within a school_id")
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
  qt <- c("pie", "achievement", "prioritario", "regular")
  qcol <- c("pie_spots", "achievement_spots", "prioritario_spots", "regular_spots")
  
  school_list <- vector("list", length(qt))
  for (k in seq_along(qt)) {
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
  app_blocks <- vector("list", 4L)
  
  # regular: everyone
  app_blocks[[1]] <- data.frame(
    student_id = apps$student_id,
    school_id = apps$school_id,
    student_pref = apps$student_pref,
    priority_level = apps$priority_level,
    lottery_ticket = apps$lottery_ticket,
    achievement_transition_order = NA_integer_,
    pie_order = NA_integer_,
    quota_type = "regular",
    quota_rank = quota_rank_map["regular"],
    stringsAsFactors = FALSE
  )
  
  # pie
  # If orden_pie exists, use non-missing orden_pie to define PIE participation.
  # Otherwise fall back to is_pie == 1.
  if ("orden_pie" %in% names(apps)) {
    idx <- !is.na(apps$orden_pie)
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
        student_id = apps$student_id[idx],
        school_id = apps$school_id[idx],
        student_pref = apps$student_pref[idx],
        priority_level = apps$priority_level[idx],
        lottery_ticket = apps$lottery_ticket[idx],
        achievement_transition_order = NA_integer_,
        pie_order = NA_integer_,
        quota_type = "achievement",
        quota_rank = quota_rank_map["achievement"],
        stringsAsFactors = FALSE
      )
    }
  } else if (achievement_rule == "transition") {
    idx <- !is.na(apps$academic_order_transition)
    if (any(idx)) {
      app_blocks[[3]] <- data.frame(
        student_id = apps$student_id[idx],
        school_id = apps$school_id[idx],
        student_pref = apps$student_pref[idx],
        priority_level = apps$priority_level[idx],
        lottery_ticket = apps$lottery_ticket[idx],
        achievement_transition_order = apps$academic_order_transition[idx],
        pie_order = NA_integer_,
        quota_type = "achievement",
        quota_rank = quota_rank_map["achievement"],
        stringsAsFactors = FALSE
      )
    }
  }
  
  # prioritario
  idx <- apps$is_prioritario == 1L
  if (any(idx)) {
    app_blocks[[4]] <- data.frame(
      student_id = apps$student_id[idx],
      school_id = apps$school_id[idx],
      student_pref = apps$student_pref[idx],
      priority_level = apps$priority_level[idx],
      lottery_ticket = apps$lottery_ticket[idx],
      achievement_transition_order = NA_integer_,
      pie_order = NA_integer_,
      quota_type = "prioritario",
      quota_rank = quota_rank_map["prioritario"],
      stringsAsFactors = FALSE
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
      unused[quota_type_by_pseudo != "regular"],
      school_by_pseudo[quota_type_by_pseudo != "regular"],
      sum
    )
    
    cap2 <- cap1
    reg_idx <- which(quota_type_by_pseudo == "regular")
    if (length(reg_idx)) {
      extras <- extra_regular_by_school[ school_by_pseudo[reg_idx] ]
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