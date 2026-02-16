####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"

#Datawd (Dropbox) 
setwd(data_wd)
#####################################


library(tidyverse)
library(geosphere)



#### Step 1:  Distance matrix 

program_info_most_recent <- readRDS("./data/clean/program_info_22-24.rds") %>% 
  mutate(COMUNA_SEDE = stri_trans_general(COMUNA_SEDE, "Latin-ASCII"))



comunas_info <- readRDS( "./data/clean/comuna_info.RDS")  %>% 
                  arrange(CODIGO_COMUNA)

# Extract coordinates matrix (lon, lat)
coords <- as.matrix(comunas_info[, c("LON", "LAT")])

# Distance matrix in meters
dist_matrix <- distm(coords, fun = distHaversine) / 1000  # convert to km

# Convert to long dataframe
dist_df <- as.data.frame(dist_matrix) %>%
  mutate(
    NOMBRE_COMUNA_EGRESO = comunas_info$NOMBRE_COMUNA,
    CODIGO_COMUNA_EGRESO = comunas_info$CODIGO_COMUNA,
    LAT_EGRESO = comunas_info$LAT,
    LON_EGRESO = comunas_info$LON
  ) %>%
  pivot_longer(
    cols = -c(NOMBRE_COMUNA_EGRESO, CODIGO_COMUNA_EGRESO, LAT_EGRESO, LON_EGRESO),
    names_to = "index_b",
    values_to = "distance_km"
  ) %>%
  mutate(
    index_b = as.integer(gsub("V", "", index_b)),
    NOMBRE_COMUNA_SEDE = comunas_info$NOMBRE_COMUNA[index_b],
    CODIGO_COMUNA_SEDE = comunas_info$CODIGO_COMUNA[index_b],
    LAT_SEDE = comunas_info$LAT[index_b],
    LON_SEDE = comunas_info$LON[index_b]
  ) %>%
  select(
    NOMBRE_COMUNA_EGRESO, CODIGO_COMUNA_EGRESO,
    NOMBRE_COMUNA_SEDE, CODIGO_COMUNA_SEDE,
    LAT_EGRESO, LON_EGRESO,
    LAT_SEDE, LON_SEDE,
    distance_km
  )


dist_sym <- dist_df %>%
  transmute(
    CODIGO_COMUNA_EGRESO = as.integer(CODIGO_COMUNA_EGRESO),
    CODIGO_COMUNA_SEDE   = as.integer(CODIGO_COMUNA_SEDE),
    distance_km          = as.numeric(distance_km)
  ) %>%
  as.data.table()

setkey(dist_sym, CODIGO_COMUNA_EGRESO, CODIGO_COMUNA_SEDE)


# sample 1000 pairs and check D(a,b) == D(b,a)
#set.seed(1)
#pairs <- dist_sym[sample(.N, 1000)][, .(a = CODIGO_COMUNA_EGRESO, b = CODIGO_COMUNA_SEDE, d = distance_km)]

#pairs[, d_rev := dist_sym[.(b, a), distance_km]]
#pairs[, max_abs_diff := max(abs(d - d_rev), na.rm = TRUE)]
#pairs[, max_abs_diff][1]




#students_apps_geo <- students_apps %>%  
#  left_join(comunas_info, by = "CODIGO_COMUNA_EGRESO")

#Missing 3k out of 977k only because they do not have a comuna egreso
#table(is.na(students_apps_geo$NOMBRE_COMUNA_EGRESO))



#program_comuna <- program_info_most_recent %>% 
#  select(COD_SIES, COMUNA_SEDE)  %>%  
#  left_join(comunas_info %>% 
#              rename(COMUNA_SEDE = NOMBRE_COMUNA_EGRESO), by = "COMUNA_SEDE")
 

#Im pretty sure there is a DB that has info about where students actually live,
#instead of their schools. But I will leave that for a later moment

#students_apps_geo %>% 
#  filter(is.na(NOMBRE_COMUNA_EGRESO)) %>% 
#  View()

###################################
#2) Construct feasible set 

#Only using 2024 and 2025



map_codes_all <- readRDS ("./data/clean/oferta_codes_24_25_all.rds") %>% 
          rename(year = year_info) 

mifuturo_imputed<- readRDS( "./data/clean/mifuturo_imputed.rds")

cutoff_scores <- readRDS( "./data/clean/cutoff_scores.rds") %>% 
  filter(TIPO_PREF == "REGULAR") 

all_info_programs  <-  map_codes_all %>% 
  left_join(program_info_most_recent, by = "COD_SIES") %>% 
  left_join(mifuturo_imputed, 
                                 by = c("NOMB_INST",
                                        "AREA_CARRERA_GENERICA",
                                        "TIPO_INST_1")) %>% 
  left_join(cutoff_scores, by = c("COD_CARRERA_PREF", "year")) %>% 
  left_join(comunas_info[,1:2] %>%  
              rename(COMUNA_SEDE = NOMBRE_COMUNA,
                     CODIGO_COMUNA_SEDE = CODIGO_COMUNA), by = "COMUNA_SEDE")


load("./data/clean/psu_students.RData")

students_apps_2425 <- students_apps %>% 
  filter(year >= 2024) %>% 
  filter(!is.na(CODIGO_COMUNA_EGRESO)) %>% 
  group_by(mrun) %>% 
  filter(year == min(year)) %>% 
  ungroup()
 
# One-time: create elective max score
students_apps_2425 <- as.data.table(students_apps_2425)
students_apps_2425[, elect_max := pmax(hist_max, scien_max, na.rm = TRUE)]
students_apps_2425[!is.finite(elect_max), elect_max := NA_real_]


#################################
############# Chat Code 
##############################

library(data.table)
alpha <- 0


build_D_cp <- function(dist_sym, comuna_codes, prog_comunas) {
  comuna_codes <- as.integer(comuna_codes)
  prog_comunas <- as.integer(prog_comunas)
  
  C <- length(comuna_codes)
  J <- length(prog_comunas)
  
  D_cp <- matrix(NA_real_, nrow = C, ncol = J)
  rownames(D_cp) <- as.character(comuna_codes)
  
  for (i in seq_len(C)) {
    ca <- comuna_codes[i]
    
    # keyed lookup for all program comunas at once
    tmp <- dist_sym[list(ca, prog_comunas), nomatch = 0L]
    
    # align to prog_comunas order
    pos <- match(prog_comunas, tmp$CODIGO_COMUNA_SEDE)
    dvec <- tmp$distance_km[pos]
    
    # if anything missing, set to Inf (or 0 if you prefer)
    dvec[is.na(dvec)] <- Inf
    
    D_cp[i, ] <- dvec
  }
  
  D_cp
}


make_objective_money_minus_distance <- function(D_cp, pro_dt,
                                                money_col = "income_imp",
                                                alpha_col = "alpha_dist",
                                                stu_comuna_col = "CODIGO_COMUNA_EGRESO") {
  
  money <- suppressWarnings(as.numeric(pro_dt[[money_col]]))
  money[is.na(money)] <- -Inf   # <-- critical: prevent max.col picking NA
  J <- length(money)
  
  function(stu_block, elig, Sb) {
    # stu_block is only the block of students
    stu_com <- as.character(as.integer(stu_block[[stu_comuna_col]]))  # length nb
    nb <- nrow(stu_block)
    
    alpha <- if (alpha_col %in% names(stu_block)) as.numeric(stu_block[[alpha_col]]) else rep(0, nb)
    
    # distances by indexing: nb × J
    D <- D_cp[stu_com, , drop = FALSE]
    
    # objective: money - alpha_i * distance_ij
    M <- matrix(money, nrow = nb, ncol = J, byrow = TRUE)
    Obj <- M - sweep(D, 1L, alpha, "*")
    
    Obj
  }
}





library(data.table)
library(readr)



# ============================================================
# Feasible sets + max money:
#   (1) among ALL eligible programs
#   (2) among eligible programs IN the student's application list
#
# Key fix added:
#   If program requires M2 (weight M2>0), student must have math2_max != 0
#   (Your convention: 0 means "did not take M2")
# ============================================================
library(data.table)
library(readr)

compute_feasible_sets_max_money <- function(
    students,
    programs,
    apps,
    strata_cols = "year",
    block_size = 20000L,
    
    id_col = "mrun",
    program_id_col = "COD_CARRERA_PREF",
    cutoff_col = "PTJE_corte",
    money_col = "income_imp",
    
    score_cols_K  = c("PTJE_NEM","PTJE_RANKING","leng_max","math_max","math2_max",
                      "hist_max","scien_max","elect_max"),
    
    weight_cols_base = c("NEM","RANKING","CLEC","M1","M2","HSCO","CIEN"),
    
    apps_id_col = "mrun",
    apps_year_col = "year",
    apps_program_col = "COD_CARRERA_PREF",
    apps_rank_col = "ORDEN_PREF",      # <-- NEW
    
    m2_score_col = "math2_max",
    
    print_outer = TRUE,
    print_block_progress = TRUE
) {
  start_time <- Sys.time()
  cat("Start time:", format(start_time), "\n")
  
  students <- as.data.table(copy(students))
  programs <- as.data.table(copy(programs))
  apps     <- as.data.table(copy(apps))
  
  stopifnot(all(strata_cols %in% names(students)))
  stopifnot(all(strata_cols %in% names(programs)))
  stopifnot(id_col %in% names(students))
  stopifnot(program_id_col %in% names(programs))
  stopifnot(cutoff_col %in% names(programs))
  stopifnot(money_col %in% names(programs))
  stopifnot(all(score_cols_K %in% names(students)))
  stopifnot(all(weight_cols_base %in% names(programs)))
  stopifnot(all(c(apps_id_col, apps_year_col, apps_program_col, apps_rank_col) %in% names(apps)))
  stopifnot(m2_score_col %in% names(students))
  
  setkeyv(students, strata_cols)
  setkeyv(programs, strata_cols)
  setkeyv(apps, c(apps_year_col, apps_id_col))
  
  strata_vals <- unique(students[, ..strata_cols])
  
  out <- vector("list", nrow(strata_vals))
  out_i <- 0L
  
  for (s in seq_len(nrow(strata_vals))) {
    key_row <- strata_vals[s]
    yr <- key_row[[strata_cols[1]]]
    
    if (print_outer) {
      cat("Starting stratum", s, "of", nrow(strata_vals),
          "=>", yr, "at", format(Sys.time()), "\n")
    }
    
    stu_s <- students[key_row, nomatch = 0, on = strata_cols]
    pro_s <- programs[key_row, nomatch = 0, on = strata_cols]
    if (nrow(stu_s) == 0L || nrow(pro_s) == 0L) next
    
    apps_y <- apps[.(yr), on = apps_year_col, nomatch = 0L]
    
    for (cc in score_cols_K) set(stu_s, j = cc, value = as.numeric(stu_s[[cc]]))
    for (cc in weight_cols_base) set(pro_s, j = cc, value = as.numeric(pro_s[[cc]]))
    set(pro_s, j = cutoff_col, value = as.numeric(pro_s[[cutoff_col]]))
    
    has_M2_all    <- !is.na(stu_s$math2_max)  & (stu_s$math2_max  != 0)
    has_HSCO_all  <- !is.na(stu_s$hist_max)   & (stu_s$hist_max   != 0)
    has_CIEN_all  <- !is.na(stu_s$scien_max)  & (stu_s$scien_max  != 0)
    has_ELECT_all <- has_HSCO_all | has_CIEN_all
    
    for (cc in score_cols_K) {
      ii <- which(is.na(stu_s[[cc]]))
      if (length(ii)) set(stu_s, i = ii, j = cc, value = 0)
    }
    for (cc in weight_cols_base) {
      ii <- which(is.na(pro_s[[cc]]))
      if (length(ii)) set(pro_s, i = ii, j = cc, value = 0)
    }
    
    pro_s <- pro_s[!is.na(get(cutoff_col))]
    if (nrow(pro_s) == 0L) next
    
    pro_s[, ELECT := 0]
    both_pos <- (pro_s$HSCO > 0) & (pro_s$CIEN > 0)
    if (any(both_pos)) {
      pro_s[both_pos, ELECT := HSCO]
      pro_s[both_pos, `:=`(HSCO = 0, CIEN = 0)]
    }
    
    weight_cols_K <- c("NEM","RANKING","CLEC","M1","M2","HSCO","CIEN","ELECT")
    
    Wm <- as.matrix(pro_s[, ..weight_cols_K])
    Wm[is.na(Wm)] <- 0
    Wm <- Wm / 100
    rs <- rowSums(Wm); rs[rs == 0] <- 1
    Wm <- Wm / rs
    pro_s[, (weight_cols_K) := as.data.table(Wm)]
    
    W      <- t(as.matrix(pro_s[, ..weight_cols_K]))
    cutoff <- as.numeric(pro_s[[cutoff_col]])
    pid    <- pro_s[[program_id_col]]
    
    N <- nrow(stu_s)
    J <- nrow(pro_s)
    X_all <- as.matrix(stu_s[, ..score_cols_K])
    stu_id <- stu_s[[id_col]]
    
    money <- readr::parse_number(as.character(pro_s[[money_col]]))
    money[!is.finite(money)] <- -Inf
    if (all(!is.finite(money))) stop("All money NA/Inf in year=", yr)
    
    pid_to_j <- setNames(seq_len(J), as.character(pid))
    
    req_M2    <- pro_s$M2    > 0
    req_HSCO  <- pro_s$HSCO  > 0
    req_CIEN  <- pro_s$CIEN  > 0
    req_ELECT <- pro_s$ELECT > 0
    
    n_feasible <- integer(N)
    
    best_all_prog <- rep(NA_integer_, N)
    best_all_obj  <- rep(NA_real_,   N)
    
    n_applied <- integer(N)
    n_applied_feasible <- integer(N)
    best_app_prog <- rep(NA_integer_, N)
    best_app_obj  <- rep(NA_real_,    N)
    
    idx_starts <- seq.int(1L, N, by = block_size)
    n_blocks <- length(idx_starts)
    block_marks <- unique(pmax(1L, ceiling(n_blocks * seq(0.2, 1, by = 0.2))))
    
    for (bi in seq_along(idx_starts)) {
      b0 <- idx_starts[bi]
      b1 <- min(b0 + block_size - 1L, N)
      rows <- b0:b1
      nb <- length(rows)
      
      if (print_block_progress && (bi %in% block_marks)) {
        pct <- round(100 * bi / n_blocks)
        cat("  Block progress:", pct, "% for year", yr, "at", format(Sys.time()), "\n")
      }
      
      Sb <- X_all[rows, , drop = FALSE] %*% W
      elig <- sweep(Sb, 2L, cutoff, FUN = ">=")
      elig[is.na(elig)] <- FALSE
      
      has_M2    <- has_M2_all[rows]
      has_HSCO  <- has_HSCO_all[rows]
      has_CIEN  <- has_CIEN_all[rows]
      has_ELECT <- has_ELECT_all[rows]
      
      elig <- elig &
        (tcrossprod(!has_M2,    req_M2)    == 0) &
        (tcrossprod(!has_HSCO,  req_HSCO)  == 0) &
        (tcrossprod(!has_CIEN,  req_CIEN)  == 0) &
        (tcrossprod(!has_ELECT, req_ELECT) == 0)
      
      n_feasible[rows] <- rowSums(elig)
      
      # global max money among eligible
      Obj_all <- matrix(money, nrow = nb, ncol = J, byrow = TRUE)
      Obj_all[!elig] <- -Inf
      argmax_all <- max.col(Obj_all, ties.method = "first")
      mx_all <- Obj_all[cbind(seq_len(nb), argmax_all)]
      mx_all[!is.finite(mx_all)] <- NA_real_
      
      best_all_obj[rows]  <- mx_all
      best_all_prog[rows] <- pid[argmax_all]
      best_all_prog[rows][is.na(mx_all)] <- NA
      
      # applied choice = first acceptable by rank
      stu_ids_block <- stu_id[rows]
      row_in_block  <- setNames(seq_along(stu_ids_block), as.character(stu_ids_block))
      
      apps_block <- apps_y[.(stu_ids_block), on = apps_id_col, nomatch = 0L,
                           .(mrun  = get(apps_id_col),
                             prog_j = pid_to_j[as.character(get(apps_program_col))],
                             rank   = as.integer(get(apps_rank_col)))
      ]
      apps_block <- apps_block[!is.na(prog_j) & !is.na(rank)]
      
      if (nrow(apps_block) > 0L) {
        setorder(apps_block, mrun, rank)
        apps_list <- split(apps_block, apps_block$mrun)
        
        for (sid_chr in names(apps_list)) {
          ii <- row_in_block[[sid_chr]]
          dt <- apps_list[[sid_chr]]
          js <- dt$prog_j
          
          n_applied[rows[ii]] <- length(js)
          
          ok_elig <- elig[ii, js]
          n_applied_feasible[rows[ii]] <- sum(ok_elig)
          if (!any(ok_elig)) next
          
          j_best <- js[which(ok_elig)[1]]
          best_app_prog[rows[ii]] <- pid[j_best]
          best_app_obj[rows[ii]]  <- money[j_best]
          if (!is.finite(best_app_obj[rows[ii]])) best_app_obj[rows[ii]] <- NA_real_
        }
      }
      
      rm(Sb, elig, Obj_all, argmax_all, mx_all, apps_block)
    }
    
    out_i <- out_i + 1L
    out[[out_i]] <- data.table(
      mrun = stu_id,
      year = yr,
      n_feasible = n_feasible,
      best_program_id_all = best_all_prog,
      best_objective_all  = best_all_obj,
      n_applied = n_applied,
      n_applied_feasible = n_applied_feasible,
      best_program_id_applied = best_app_prog,
      best_objective_applied  = best_app_obj
    )
    
    if (print_outer) cat("Finished year", yr, "at", format(Sys.time()), "\n")
  }
  
  res <- rbindlist(out[seq_len(out_i)], use.names = TRUE, fill = TRUE)
  
  end_time <- Sys.time()
  cat("End time:", format(end_time), "\n")
  cat("Total time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")
  
  res
}


###################################
#3) Add preferences

load("./data/clean/psu_applications.RData")

college_apps_2425 <- college_apps %>% 
  filter(year >= 2024,
         TIPO_PREF == "REGULAR") %>% 
  group_by(mrun) %>% 
  filter(year == min(year)) %>% 
  ungroup() %>% 
  select(mrun, year, COD_CARRERA_PREF, ORDEN_PREF)


apps_global_nd <- compute_feasible_sets_max_money(
  students = students_apps_2425,
  programs = all_info_programs,
  apps     = college_apps_2425,
  strata_cols = "year",
  block_size = 20000L
)


# helper: winsorize at p and 1-p
winsor_p1_p99 <- function(x, p = 0.01) {
  lo <- quantile(x, probs = p,   na.rm = TRUE)
  hi <- quantile(x, probs = 1-p, na.rm = TRUE)
  pmin(pmax(x, lo), hi)
}


apps_global_nd <- apps_global_nd %>% 
  mutate(global_nd_raw_improvement  = (best_objective_all - best_objective_applied)) %>% 
  mutate(global_nd_perc_improvement = (best_objective_all - best_objective_applied)/(best_objective_applied)) %>% 
  mutate(small_improvement = as.integer( abs(global_nd_perc_improvement)<= 0.1 ),
         improvement_lt1pp = as.integer( (global_nd_perc_improvement)<= 0.1 ),
         # censor p1/p99
         global_nd_raw_improvement_c  = winsor_p1_p99(global_nd_raw_improvement),
         global_nd_perc_improvement_c = winsor_p1_p99(global_nd_perc_improvement)
  )  


apps_global_nd %>% 
  filter(n_applied_feasible>0) %>% 
  View()

apps_global_nd %>% 
  filter(n_applied_feasible>0) %>% 
  pull(global_nd_raw_improvement_c) %>% 
  summary()



row_global_nd <- apps_global_nd %>%
  filter(n_applied_feasible > 0) %>%
  summarise(
    n = sum(!is.na(global_nd_perc_improvement_c)),
    min   = round(min(global_nd_perc_improvement_c, na.rm = TRUE), 3),
    
    p10   = round(quantile(global_nd_perc_improvement_c, 0.10, na.rm = TRUE), 3),
    p25   = round(quantile(global_nd_perc_improvement_c, 0.25, na.rm = TRUE), 3),
    median= round(median(global_nd_perc_improvement_c, na.rm = TRUE), 3),
    mean  = round(mean(global_nd_perc_improvement_c, na.rm = TRUE), 3),
    p75   = round(quantile(global_nd_perc_improvement_c, 0.75, na.rm = TRUE), 3),
    p90   = round(quantile(global_nd_perc_improvement_c, 0.90, na.rm = TRUE), 3),
    #p99   = round(quantile(global_nd_perc_improvement_c, 0.99, na.rm = TRUE), 3),
    #max   = round(max(global_nd_perc_improvement_c, na.rm = TRUE), 3)
  )

saveRDS(apps_global_nd, "./data/clean/optim_global_nd.RDS")



######################################
#4) Add the field constraints and major constraints 



stu_pref_input  <- students_apps_2425 %>% 
  select(mrun, year, CODIGO_COMUNA_EGRESO, CODIGO_REGION_EGRESO) %>% 
  filter(!is.na(CODIGO_COMUNA_EGRESO))

pro_pref_input <- all_info_programs %>% 
  rename(NOMBRE_COMUNA_SEDE = COMUNA_SEDE) %>% 
  select(COD_CARRERA_PREF , year, AREA_CARRERA_GENERICA, field_merged , NOMBRE_COMUNA_SEDE )


pref_db <- college_apps_2425 %>%  
  left_join(stu_pref_input, by = c("mrun", "year")) %>% 
  left_join(pro_pref_input, by = c("COD_CARRERA_PREF", "year")) %>% 
  left_join(dist_df, by = c("CODIGO_COMUNA_EGRESO", "NOMBRE_COMUNA_SEDE"))



summary(pref_db$distance_km)

dist_pref <- pref_db %>% 
  filter(!is.na(CODIGO_COMUNA_EGRESO)) %>% 
  filter(!is.na(NOMBRE_COMUNA_SEDE)) %>% 
  group_by(mrun, CODIGO_REGION_EGRESO) %>% 
  summarize(max_app_distance = max(distance_km, na.rm = TRUE),
            avg_app_distance = mean(distance_km, na.rm = TRUE),
            median_app_distance = median(distance_km, na.rm = TRUE),
  )  %>% 
  ungroup()



dist_pref %>% 
  group_by(CODIGO_REGION_EGRESO) %>% 
  summarize(mean_region_max = mean(max_app_distance, na.rm = TRUE))
#Make this into a map maybe 


fields_pref <- pref_db %>%
  filter(!is.na(field_merged)) %>%
  arrange(mrun, CODIGO_REGION_EGRESO, ORDEN_PREF) %>%
  group_by(mrun, CODIGO_REGION_EGRESO) %>%
  summarise(
    fields = list(unique(field_merged)),
    first_field  = fields[[1]][1],
    second_field = fields[[1]][2],
    .groups = "drop"
  ) %>%
  select(-fields)


fields_pref  %>% 
  filter(!is.na(CODIGO_REGION_EGRESO)) %>%
  group_by(CODIGO_REGION_EGRESO) %>% 
  summarise(
    fields = list(unique(first_field)),
    region_field_1  = fields[[1]][1],
    region_field_2  = fields[[1]][2],
    .groups = "drop"
  ) %>%
  select(-fields)
#Probably not so relevant but I guess I can still make this into a graph or table

### Major preferences

major_pref <- pref_db %>%
  filter(!is.na(AREA_CARRERA_GENERICA)) %>%
  arrange(mrun, CODIGO_REGION_EGRESO, ORDEN_PREF) %>%
  group_by(mrun, CODIGO_REGION_EGRESO) %>%
  summarise(
    fields = list(unique(AREA_CARRERA_GENERICA)),
    first_major   = fields[[1]][1],
    second_major  = fields[[1]][2],
    third_major   = fields[[1]][3],
    .groups = "drop"
  ) %>%
  select(-fields)



major_pref  %>% 
  filter(!is.na(CODIGO_REGION_EGRESO)) %>%
  group_by(CODIGO_REGION_EGRESO) %>% 
  summarise(
    fields = list(unique(first_major)),
    region_major_1  = fields[[1]][1],
    region_major_2  = fields[[1]][2],
    .groups = "drop"
  ) %>%
  select(-fields)



individual_pref <- stu_pref_input %>% 
  left_join(dist_pref, by = c("mrun", "CODIGO_REGION_EGRESO")) %>% 
  left_join(fields_pref, by = c("mrun", "CODIGO_REGION_EGRESO")) %>% 
  left_join(major_pref, by = c("mrun", "CODIGO_REGION_EGRESO"))  

cap99 <- quantile(individual_pref$max_app_distance, 0.99, na.rm = TRUE)

individual_pref <- individual_pref %>% 
  mutate(max_app_distance = pmin(max_app_distance, cap99)) %>%
  mutate(
    cat_distance = case_when(
      max_app_distance <= 100   ~ "<=100 km",
      max_app_distance <= 500   ~ "100 - 500 km",
      max_app_distance <= 1000  ~ "500 - 1000 km",
      TRUE                      ~ "+ 1000 km"
    ),
    implied_dist_limit = case_when(
      max_app_distance <= 100   ~ 100L,
      max_app_distance <= 500   ~ 500L,
      max_app_distance <= 1000  ~ 1000L,
      TRUE                      ~ 10000L
    )
  )

###########################################
####5) Expand function to allow these multiple objects

library(data.table)
library(readr)

make_allowed_js <- function(stu_ids_block, cs_mode, pref_y, pro_s) {
  if (cs_mode == "none") return(NULL)
  
  pref_block <- pref_y[.(stu_ids_block), on = "mrun", nomatch = 0L]
  out <- vector("list", length(stu_ids_block))
  if (nrow(pref_block) == 0L) return(out)
  
  row_in_block <- setNames(seq_along(stu_ids_block), as.character(stu_ids_block))
  
  if (cs_mode == "maj3") {
    prog_major <- pro_s[["AREA_CARRERA_GENERICA"]]
    for (r in seq_len(nrow(pref_block))) {
      sid <- as.character(pref_block$mrun[r])
      ii  <- row_in_block[[sid]]
      majors <- unlist(pref_block[r, .(first_major, second_major, third_major)], use.names = FALSE)
      majors <- majors[!is.na(majors)]
      if (!length(majors)) next
      js <- which(prog_major %in% majors)
      if (length(js)) out[[ii]] <- js
    }
  } else if (cs_mode == "fld2") {
    prog_field <- pro_s[["field_merged"]]
    for (r in seq_len(nrow(pref_block))) {
      sid <- as.character(pref_block$mrun[r])
      ii  <- row_in_block[[sid]]
      flds <- unlist(pref_block[r, .(first_field, second_field)], use.names = FALSE)
      flds <- flds[!is.na(flds)]
      if (!length(flds)) next
      js <- which(prog_field %in% flds)
      if (length(js)) out[[ii]] <- js
    }
  }
  
  out
}


compute_feasible_sets_cons_optim_fast <- function(
    students,
    programs,
    apps,
    preferences,
    
    cs_mode = c("none","maj3","fld2"),
    use_distance = TRUE,
    
    strata_cols = "year",
    block_size = 20000L,
    
    id_col = "mrun",
    program_id_col = "COD_CARRERA_PREF",
    cutoff_col = "PTJE_corte",
    money_col = "income_imp",
    
    score_cols_K  = c("PTJE_NEM","PTJE_RANKING","leng_max","math_max","math2_max",
                      "hist_max","scien_max","elect_max"),
    
    weight_cols_base = c("NEM","RANKING","CLEC","M1","M2","HSCO","CIEN"),
    
    apps_id_col = "mrun",
    apps_year_col = "year",
    apps_program_col = "COD_CARRERA_PREF",
    apps_rank_col = "ORDEN_PREF",          # <-- NEW
    
    m2_score_col = "math2_max",
    hsco_score_col = "hist_max",
    cien_score_col = "scien_max",
    
    stu_comuna_col = "CODIGO_COMUNA_EGRESO",
    prog_comuna_col = "CODIGO_COMUNA_SEDE",
    dist_dt = dist_sym,
    
    print_outer = TRUE,
    print_block_progress = TRUE
) {
  cs_mode <- match.arg(cs_mode)
  
  start_time <- Sys.time()
  cat("Start time:", format(start_time), "\n")
  
  students     <- as.data.table(copy(students))
  programs     <- as.data.table(copy(programs))
  apps         <- as.data.table(copy(apps))
  preferences  <- as.data.table(copy(preferences))
  dist_dt      <- as.data.table(dist_dt)
  
  stopifnot(all(strata_cols %in% names(students)))
  stopifnot(all(strata_cols %in% names(programs)))
  stopifnot(id_col %in% names(students))
  stopifnot(program_id_col %in% names(programs))
  stopifnot(cutoff_col %in% names(programs))
  stopifnot(money_col %in% names(programs))
  stopifnot(all(score_cols_K %in% names(students)))
  stopifnot(all(weight_cols_base %in% names(programs)))
  stopifnot(all(c(apps_id_col, apps_year_col, apps_program_col, apps_rank_col) %in% names(apps)))
  
  # Keys
  setkeyv(students, strata_cols)
  setkeyv(programs, strata_cols)
  setkeyv(apps, c(apps_year_col, apps_id_col))        # year,mrun
  setkey(preferences, year, mrun)
  if (use_distance) setkey(dist_dt, CODIGO_COMUNA_EGRESO, CODIGO_COMUNA_SEDE)
  
  strata_vals <- unique(students[, ..strata_cols])
  
  out <- vector("list", nrow(strata_vals))
  out_i <- 0L
  
  for (s in seq_len(nrow(strata_vals))) {
    yr <- strata_vals[s][[strata_cols[1]]]
    
    if (print_outer) {
      cat("Starting year", yr, "(", s, "of", nrow(strata_vals), ") at", format(Sys.time()), "\n")
    }
    
    stu_s <- students[.(yr), on = strata_cols, nomatch = 0L]
    pro_s <- programs[.(yr), on = strata_cols, nomatch = 0L]
    if (nrow(stu_s) == 0L || nrow(pro_s) == 0L) next
    
    apps_y <- apps[.(yr), on = apps_year_col, nomatch = 0L]
    pref_y <- preferences[.(yr), on = "year", nomatch = 0L]
    setkey(pref_y, mrun)
    
    # numeric coercion (before NA->0)
    for (cc in score_cols_K)      set(stu_s, j = cc, value = as.numeric(stu_s[[cc]]))
    for (cc in weight_cols_base)  set(pro_s, j = cc, value = as.numeric(pro_s[[cc]]))
    set(pro_s, j = cutoff_col, value = as.numeric(pro_s[[cutoff_col]]))
    
    # requirement flags BEFORE NA->0
    has_M2_all    <- !is.na(stu_s[[m2_score_col]])   & (stu_s[[m2_score_col]]   != 0)
    has_HSCO_all  <- !is.na(stu_s[[hsco_score_col]]) & (stu_s[[hsco_score_col]] != 0)
    has_CIEN_all  <- !is.na(stu_s[[cien_score_col]]) & (stu_s[[cien_score_col]] != 0)
    has_ELECT_all <- has_HSCO_all | has_CIEN_all
    
    # student NA->0
    for (cc in score_cols_K) {
      ii <- which(is.na(stu_s[[cc]]))
      if (length(ii)) set(stu_s, i = ii, j = cc, value = 0)
    }
    
    # program NA weights -> 0
    for (cc in weight_cols_base) {
      ii <- which(is.na(pro_s[[cc]]))
      if (length(ii)) set(pro_s, i = ii, j = cc, value = 0)
    }
    
    # drop NA cutoffs
    pro_s <- pro_s[!is.na(get(cutoff_col))]
    if (nrow(pro_s) == 0L) next
    
    # elective collapse
    pro_s[, ELECT := 0]
    both_pos <- (pro_s$HSCO > 0) & (pro_s$CIEN > 0)
    if (any(both_pos)) {
      pro_s[both_pos, ELECT := HSCO]
      pro_s[both_pos, `:=`(HSCO = 0, CIEN = 0)]
    }
    
    weight_cols_K <- c("NEM","RANKING","CLEC","M1","M2","HSCO","CIEN","ELECT")
    
    # normalize weights
    Wm <- as.matrix(pro_s[, ..weight_cols_K]); Wm[is.na(Wm)] <- 0
    Wm <- Wm / 100
    rs <- rowSums(Wm); rs[rs == 0] <- 1
    Wm <- Wm / rs
    pro_s[, (weight_cols_K) := as.data.table(Wm)]
    
    # matrices/vectors
    W      <- t(as.matrix(pro_s[, ..weight_cols_K]))  # K×J
    cutoff <- as.numeric(pro_s[[cutoff_col]])
    pid    <- pro_s[[program_id_col]]
    
    N <- nrow(stu_s)
    J <- nrow(pro_s)
    X_all  <- as.matrix(stu_s[, ..score_cols_K])
    stu_id <- stu_s[[id_col]]
    
    money <- readr::parse_number(as.character(pro_s[[money_col]]))
    money[!is.finite(money)] <- -Inf
    
    req_M2    <- pro_s$M2    > 0
    req_HSCO  <- pro_s$HSCO  > 0
    req_CIEN  <- pro_s$CIEN  > 0
    req_ELECT <- pro_s$ELECT > 0
    
    pid_to_j <- setNames(seq_len(J), as.character(pid))
    
    # distance precompute
    if (use_distance) {
      stu_com_all <- as.integer(stu_s[[stu_comuna_col]])
      prog_com    <- as.integer(pro_s[[prog_comuna_col]])
      
      comuna_codes <- sort(unique(stu_com_all))
      comuna_codes <- comuna_codes[!is.na(comuna_codes)]
      
      D_cp <- matrix(Inf, nrow = length(comuna_codes), ncol = J)
      rownames(D_cp) <- as.character(comuna_codes)
      
      for (j in seq_along(prog_com)) {
        b <- prog_com[j]
        d <- dist_dt[J(data.table(CODIGO_COMUNA_EGRESO = comuna_codes,
                                  CODIGO_COMUNA_SEDE   = b)),
                     on = .(CODIGO_COMUNA_EGRESO, CODIGO_COMUNA_SEDE),
                     x.distance_km]
        d[is.na(d)] <- Inf
        D_cp[, j] <- d
      }
    }
    
    # outputs
    n_feasible_all <- integer(N)
    
    n_applied <- integer(N)
    n_applied_feasible <- integer(N)
    best_app_prog <- rep(NA_integer_, N)
    best_app_obj  <- rep(NA_real_,   N)
    
    n_cs_eligible <- integer(N)
    best_cs_prog  <- rep(NA_integer_, N)
    best_cs_obj   <- rep(NA_real_,    N)
    
    idx_starts <- seq.int(1L, N, by = block_size)
    n_blocks <- length(idx_starts)
    block_marks <- unique(pmax(1L, ceiling(n_blocks * seq(0.2, 1, by = 0.2))))
    
    for (bi in seq_along(idx_starts)) {
      b0 <- idx_starts[bi]
      b1 <- min(b0 + block_size - 1L, N)
      rows <- b0:b1
      nb <- length(rows)
      
      if (print_block_progress && (bi %in% block_marks)) {
        pct <- round(100 * bi / n_blocks)
        cat("  Block progress:", pct, "% for year", yr, "at", format(Sys.time()), "\n")
      }
      
      Sb <- X_all[rows, , drop = FALSE] %*% W
      elig <- sweep(Sb, 2L, cutoff, FUN = ">=")
      elig[is.na(elig)] <- FALSE
      
      has_M2    <- has_M2_all[rows]
      has_HSCO  <- has_HSCO_all[rows]
      has_CIEN  <- has_CIEN_all[rows]
      has_ELECT <- has_ELECT_all[rows]
      
      elig <- elig &
        (tcrossprod(!has_M2,    req_M2)    == 0) &
        (tcrossprod(!has_HSCO,  req_HSCO)  == 0) &
        (tcrossprod(!has_CIEN,  req_CIEN)  == 0) &
        (tcrossprod(!has_ELECT, req_ELECT) == 0)
      
      n_feasible_all[rows] <- rowSums(elig)
      
      stu_ids_block <- stu_id[rows]
      row_in_block  <- setNames(seq_along(stu_ids_block), as.character(stu_ids_block))
      
      # ---------------- Applied choice = first acceptable by rank ----------------
      apps_block <- apps_y[.(stu_ids_block), on = apps_id_col, nomatch = 0L,
                           .(mrun  = get(apps_id_col),
                             prog_j = pid_to_j[as.character(get(apps_program_col))],
                             rank   = as.integer(get(apps_rank_col)))
      ]
      apps_block <- apps_block[!is.na(prog_j) & !is.na(rank)]
      if (nrow(apps_block) > 0L) {
        setorder(apps_block, mrun, rank)
        apps_list <- split(apps_block, apps_block$mrun)
        
        for (sid_chr in names(apps_list)) {
          ii <- row_in_block[[sid_chr]]
          dt <- apps_list[[sid_chr]]
          js <- dt$prog_j
          
          n_applied[rows[ii]] <- length(js)
          
          ok_elig <- elig[ii, js]
          n_applied_feasible[rows[ii]] <- sum(ok_elig)
          if (!any(ok_elig)) next
          
          j_best <- js[which(ok_elig)[1]]          # first acceptable by rank
          best_app_prog[rows[ii]] <- pid[j_best]
          best_app_obj[rows[ii]]  <- money[j_best]
          if (!is.finite(best_app_obj[rows[ii]])) best_app_obj[rows[ii]] <- NA_real_
        }
      }
      
      # ---------------- Constrained-set best (money) + distance ----------------
      allowed_js_list <- make_allowed_js(stu_ids_block, cs_mode, pref_y, pro_s)
      
      if (use_distance) {
        pref_block <- pref_y[.(stu_ids_block), on="mrun"]
        dist_lim <- pref_block[["implied_dist_limit"]]
        dist_lim[is.na(dist_lim)] <- Inf
        
        stu_com <- as.character(as.integer(stu_s[rows][[stu_comuna_col]]))
        idx <- match(stu_com, rownames(D_cp))
        D <- D_cp[idx, , drop = FALSE]
      }
      
      for (ii in seq_len(nb)) {
        ok <- elig[ii, ]
        
        if (cs_mode != "none") {
          js <- allowed_js_list[[ii]]
          if (length(js) == 0L) { n_cs_eligible[rows[ii]] <- 0L; next }
          ok[-js] <- FALSE
        }
        
        if (use_distance) {
          ok <- ok & (D[ii, ] <= dist_lim[ii])
          ok[is.na(ok)] <- FALSE
        }
        
        n_cs_eligible[rows[ii]] <- sum(ok)
        if (!any(ok)) next
        
        js_ok <- which(ok & is.finite(money))
        if (!length(js_ok)) next
        
        j_best <- js_ok[which.max(money[js_ok])]
        best_cs_prog[rows[ii]] <- pid[j_best]
        best_cs_obj[rows[ii]]  <- money[j_best]
      }
      
      rm(Sb, elig, apps_block)
    }
    
    out_i <- out_i + 1L
    out[[out_i]] <- data.table(
      mrun = stu_id,
      year = yr,
      
      n_feasible_all = n_feasible_all,
      
      n_applied = n_applied,
      n_applied_feasible = n_applied_feasible,
      best_program_id_applied = best_app_prog,
      best_objective_applied  = best_app_obj,
      
      cs_mode = cs_mode,
      use_distance = use_distance,
      n_cs_eligible = n_cs_eligible,
      best_program_id_cs = best_cs_prog,
      best_objective_cs  = best_cs_obj
    )
    
    if (print_outer) cat("Finished year", yr, "at", format(Sys.time()), "\n")
  }
  
  res <- rbindlist(out[seq_len(out_i)], use.names = TRUE, fill = TRUE)
  
  end_time <- Sys.time()
  cat("End time:", format(end_time), "\n")
  cat("Total time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")
  
  res
}


###############
#######TEST

apps_maj3_dist <- compute_feasible_sets_cons_optim_fast(
  students     = students_apps_2425,
  programs     = all_info_programs,
  apps         = college_apps_2425,
  preferences  = individual_pref,
  cs_mode      = "maj3",
  use_distance = TRUE,
  block_size   = 20000L,
  dist_dt      = dist_sym
)


apps_maj3_dist <- apps_maj3_dist %>% 
  mutate(maj3_dist_raw_improvement  = (best_objective_cs - best_objective_applied)) %>% 
  mutate(maj3_dist_perc_improvement = (best_objective_cs - best_objective_applied)/(best_objective_applied)) %>% 
  mutate(small_improvement = as.integer( abs(maj3_dist_perc_improvement)<= 0.1 ),
         improvement_lt1pp = as.integer( (maj3_dist_perc_improvement)<= 0.1 ),
         # censor p1/p99
         maj3_dist_raw_improvement_c  = winsor_p1_p99(maj3_dist_raw_improvement),
         maj3_dist_perc_improvement_c = winsor_p1_p99(maj3_dist_perc_improvement)
  )  



apps_maj3_dist %>% 
    filter(n_applied_feasible>0) %>% 
  pull(maj3_dist_raw_improvement_c) %>% 
  summary()



row_maj3_dist <- apps_maj3_dist %>%
  filter(n_applied_feasible > 0) %>%
  summarise(
    n = sum(!is.na(maj3_dist_perc_improvement_c)),
    min   = round(min(maj3_dist_perc_improvement_c, na.rm = TRUE), 3),
    
    p10   = round(quantile(maj3_dist_perc_improvement_c, 0.10, na.rm = TRUE), 3),
    p25   = round(quantile(maj3_dist_perc_improvement_c, 0.25, na.rm = TRUE), 3),
    median= round(median(maj3_dist_perc_improvement_c, na.rm = TRUE), 3),
    mean  = round(mean(maj3_dist_perc_improvement_c, na.rm = TRUE), 3),
    p75   = round(quantile(maj3_dist_perc_improvement_c, 0.75, na.rm = TRUE), 3),
    p90   = round(quantile(maj3_dist_perc_improvement_c, 0.90, na.rm = TRUE), 3),
    #p99   = round(quantile(maj3_dist_perc_improvement_c, 0.99, na.rm = TRUE), 3),
    #max   = round(max(maj3_dist_perc_improvement_c, na.rm = TRUE), 3)
  )

saveRDS(apps_maj3_dist, "./data/clean/apps_maj3_dist.RDS")


####

apps_maj3_nd <- compute_feasible_sets_cons_optim_fast(
  students     = students_apps_2425,
  programs     = all_info_programs,
  apps         = college_apps_2425,
  preferences  = individual_pref,
  cs_mode      = "maj3",
  use_distance = FALSE,
  block_size   = 20000L,
  dist_dt      = dist_sym
)



apps_maj3_nd <- apps_maj3_nd %>% 
  mutate(maj3_nd_raw_improvement  = (best_objective_cs - best_objective_applied)) %>% 
  mutate(maj3_nd_perc_improvement = (best_objective_cs - best_objective_applied)/(best_objective_applied)) %>% 
  mutate(small_improvement = as.integer( abs(maj3_nd_perc_improvement)<= 0.1 ),
         improvement_lt1pp = as.integer( (maj3_nd_perc_improvement)<= 0.1 ),
         # censor p1/p99
         maj3_nd_raw_improvement_c  = winsor_p1_p99(maj3_nd_raw_improvement),
         maj3_nd_perc_improvement_c = winsor_p1_p99(maj3_nd_perc_improvement)
  )  


apps_maj3_nd %>% 
  filter(n_applied_feasible>0) %>% 
  View()

apps_maj3_nd %>% 
  #  filter(n_applied_feasible>0) %>% 
  pull(maj3_nd_raw_improvement_c) %>% 
  summary()



row_maj3_nd <- apps_maj3_nd %>%
  filter(n_applied_feasible > 0) %>%
  summarise(
    n = sum(!is.na(maj3_nd_perc_improvement_c)),
    min   = round(min(maj3_nd_perc_improvement_c, na.rm = TRUE), 3),
    
    p10   = round(quantile(maj3_nd_perc_improvement_c, 0.10, na.rm = TRUE), 3),
    p25   = round(quantile(maj3_nd_perc_improvement_c, 0.25, na.rm = TRUE), 3),
    median= round(median(maj3_nd_perc_improvement_c, na.rm = TRUE), 3),
    mean  = round(mean(maj3_nd_perc_improvement_c, na.rm = TRUE), 3),
    p75   = round(quantile(maj3_nd_perc_improvement_c, 0.75, na.rm = TRUE), 3),
    p90   = round(quantile(maj3_nd_perc_improvement_c, 0.90, na.rm = TRUE), 3),
    #p99   = round(quantile(maj3_nd_perc_improvement_c, 0.99, na.rm = TRUE), 3),
    #max   = round(max(maj3_nd_perc_improvement_c, na.rm = TRUE), 3)
  )



#apps_maj3_nd %>% 
#  filter(maj3_nd_perc_improvement <= 1) %>% 
#  filter(best_objective_applied >=0 ) %>% 
#  ggplot(aes(x = maj3_nd_perc_improvement)) +
#  stat_ecdf(size = 1) +
#  labs(x = "maj3_dist_improvement", y = "CDF") +
#  theme_minimal()

saveRDS(apps_maj3_nd, "./data/clean/apps_maj3_nd.RDS")


#### 

apps_fld2_nd <- compute_feasible_sets_cons_optim_fast(
  students     = students_apps_2425,
  programs     = all_info_programs,
  apps         = college_apps_2425,
  preferences  = individual_pref,
  cs_mode      = "fld2",
  use_distance = FALSE,
  block_size   = 20000L,
  dist_dt      = dist_sym
)

apps_fld2_nd <- apps_fld2_nd %>% 
  mutate(fld2_nd_raw_improvement  = (best_objective_cs - best_objective_applied)) %>% 
  mutate(fld2_nd_perc_improvement = (best_objective_cs - best_objective_applied)/(best_objective_applied+1)) %>% 
  mutate(small_improvement = as.integer( abs(fld2_nd_perc_improvement)<= 0.1 ),
         improvement_lt1pp = as.integer( (fld2_nd_perc_improvement)<= 0.1 ),
  # censor p1/p99
fld2_nd_raw_improvement_c  = winsor_p1_p99(fld2_nd_raw_improvement),
fld2_nd_perc_improvement_c = winsor_p1_p99(fld2_nd_perc_improvement)
)  



apps_fld2_nd %>% 
  filter(n_applied_feasible      >0) %>% 
  pull(fld2_nd_raw_improvement_c) %>% 
  summary()


row_fld2_nd <- apps_fld2_nd %>%
  filter(n_applied_feasible > 0) %>%
  summarise(
    n = sum(!is.na(fld2_nd_perc_improvement_c)),
    min   = round(min(fld2_nd_perc_improvement_c, na.rm = TRUE), 3),
    
    p10   = round(quantile(fld2_nd_perc_improvement_c, 0.10, na.rm = TRUE), 3),
    p25   = round(quantile(fld2_nd_perc_improvement_c, 0.25, na.rm = TRUE), 3),
    median= round(median(fld2_nd_perc_improvement_c, na.rm = TRUE), 3),
    mean  = round(mean(fld2_nd_perc_improvement_c, na.rm = TRUE), 3),
    p75   = round(quantile(fld2_nd_perc_improvement_c, 0.75, na.rm = TRUE), 3),
    p90   = round(quantile(fld2_nd_perc_improvement_c, 0.90, na.rm = TRUE), 3),
    #p99   = round(quantile(fld2_nd_perc_improvement_c, 0.99, na.rm = TRUE), 3),
    #max   = round(max(fld2_nd_perc_improvement_c, na.rm = TRUE), 3)
  )



saveRDS(apps_fld2_nd, "./data/clean/optim_fld2_nd.RDS")


############

apps_fld2_dist <- compute_feasible_sets_cons_optim_fast(
  students     = students_apps_2425,
  programs     = all_info_programs,
  apps         = college_apps_2425,
  preferences  = individual_pref,
  cs_mode      = "fld2",
  use_distance = TRUE,
  block_size   = 20000L,
  dist_dt      = dist_sym
)



apps_fld2_dist <- apps_fld2_dist %>% 
  mutate(fld2_dist_raw_improvement  = (best_objective_cs - best_objective_applied)) %>% 
  mutate(fld2_dist_perc_improvement = (best_objective_cs - best_objective_applied)/(best_objective_applied+1)) %>% 
  mutate(small_improvement = as.integer( abs(fld2_dist_perc_improvement)<= 0.1 ),
         improvement_lt1pp = as.integer( (fld2_dist_perc_improvement)<= 0.1 ),
         # censor p1/p99
         fld2_dist_raw_improvement_c  = winsor_p1_p99(fld2_dist_raw_improvement),
         fld2_dist_perc_improvement_c = winsor_p1_p99(fld2_dist_perc_improvement)
  )  



apps_fld2_dist %>% 
  filter(n_applied_feasible      >0) %>% 
  pull(fld2_dist_raw_improvement_c) %>% 
  summary()


row_fld2_dist <- apps_fld2_dist %>%
  filter(n_applied_feasible > 0) %>%
  summarise(
    n = sum(!is.na(fld2_dist_perc_improvement_c)),
    min   = round(min(fld2_dist_perc_improvement_c, na.rm = TRUE), 3),
    
    p10   = round(quantile(fld2_dist_perc_improvement_c, 0.10, na.rm = TRUE), 3),
    p25   = round(quantile(fld2_dist_perc_improvement_c, 0.25, na.rm = TRUE), 3),
    median= round(median(fld2_dist_perc_improvement_c, na.rm = TRUE), 3),
    mean  = round(mean(fld2_dist_perc_improvement_c, na.rm = TRUE), 3),
    p75   = round(quantile(fld2_dist_perc_improvement_c, 0.75, na.rm = TRUE), 3),
    p90   = round(quantile(fld2_dist_perc_improvement_c, 0.90, na.rm = TRUE), 3),
    #p99   = round(quantile(fld2_dist_perc_improvement_c, 0.99, na.rm = TRUE), 3),
    #max   = round(max(fld2_dist_perc_improvement_c, na.rm = TRUE), 3)
  )

saveRDS(apps_fld2_dist, "./data/clean/optim_fld2_dist.RDS")


############

apps_global_dist <- compute_feasible_sets_cons_optim_fast(
  students     = students_apps_2425,
  programs     = all_info_programs,
  apps         = college_apps_2425,
  preferences  = individual_pref,
  cs_mode      = "none",
  use_distance = TRUE,
  block_size   = 20000L,
  dist_dt      = dist_sym
)



apps_global_dist <- apps_global_dist %>% 
  mutate(global_dist_raw_improvement  = (best_objective_cs - best_objective_applied)) %>% 
  mutate(global_dist_perc_improvement = (best_objective_cs - best_objective_applied)/(best_objective_applied)) %>% 
  mutate(small_improvement = as.integer( abs(global_dist_perc_improvement)<= 0.1 ),
         improvement_lt1pp = as.integer( (global_dist_perc_improvement)<= 0.1 ),
         # censor p1/p99
         global_dist_raw_improvement_c  = winsor_p1_p99(global_dist_raw_improvement),
         global_dist_perc_improvement_c = winsor_p1_p99(global_dist_perc_improvement)
  )  



apps_global_dist %>% 
  filter(n_applied_feasible>0) %>% 
  pull(global_dist_raw_improvement_c) %>% 
  summary()


row_global_dist <- apps_global_dist %>%
  filter(n_applied_feasible > 0) %>%
  summarise(
    n = sum(!is.na(global_dist_perc_improvement_c)),
    min   = round(min(global_dist_perc_improvement_c, na.rm = TRUE), 3),
    
    p10   = round(quantile(global_dist_perc_improvement_c, 0.10, na.rm = TRUE), 3),
    p25   = round(quantile(global_dist_perc_improvement_c, 0.25, na.rm = TRUE), 3),
    median= round(median(global_dist_perc_improvement_c, na.rm = TRUE), 3),
    mean  = round(mean(global_dist_perc_improvement_c, na.rm = TRUE), 3),
    p75   = round(quantile(global_dist_perc_improvement_c, 0.75, na.rm = TRUE), 3),
    p90   = round(quantile(global_dist_perc_improvement_c, 0.90, na.rm = TRUE), 3),
    #p99   = round(quantile(global_dist_perc_improvement_c, 0.99, na.rm = TRUE), 3),
    #max   = round(max(global_dist_perc_improvement_c, na.rm = TRUE), 3)
  )


saveRDS(apps_global_dist, "./data/clean/optim_global_dist.RDS")


summ_table <- as.data.frame(
  rbind(row_global_nd,
      row_fld2_nd,
      row_maj3_nd,
      row_global_dist,
      row_fld2_dist,
      row_maj3_dist)
)     %>% select (-n)

summ_table$distance_filter <- c("\\cross", "\\cross", "\\cross",
                                "\\check","\\check","\\check")

summ_table$other_constraint <- c("none", "Top 2 fields", "Top 3 majors",
                                "none","Top 2 fields","Top 3 majors")


apps_global_dist

library(scales)

# suppose your gap variable is called gap_pct (edit name)
# and summ_table variables identify the constraint regime
apps_maj3_dist %>%
#  mutate(regime = paste0(other_constraint, " | dist=", distance_filter)) %>%
  filter(!is.na(maj3_dist_perc_improvement_c)) %>%
  ggplot(aes(x = maj3_dist_perc_improvement_c)) +
  stat_ecdf(size = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Percent gap (best feasible vs best constrained-feasible)",
       y = "CDF") +
  theme_minimal()




library(dplyr)

cdf_df <- bind_rows(
  
  apps_global_nd  %>%
    transmute(
      gap = global_nd_perc_improvement_c,
      constraint = "None",
      distance = "Off"
    ),
  
  apps_global_dist %>%
    transmute(
      gap = global_dist_perc_improvement_c,
      constraint = "None",
      distance = "On"
    ),
  
  apps_fld2_nd %>%
    transmute(
      gap = fld2_nd_perc_improvement_c,
      constraint = "Top 2 fields",
      distance = "Off"
    ),
  
  apps_fld2_dist %>%
    transmute(
      gap = fld2_dist_perc_improvement_c,
      constraint = "Top 2 fields",
      distance = "On"
    ),
  
  apps_maj3_nd %>%
    transmute(
      gap = maj3_nd_perc_improvement_c,
      constraint = "Top 3 majors",
      distance = "Off"
    ),
  
  apps_maj3_dist %>%
    transmute(
      gap = maj3_dist_perc_improvement_c,
      constraint = "Top 3 majors",
      distance = "On"
    )
  
) %>%
  filter(!is.na(gap))


library(ggplot2)
library(scales)

cdf_df %>%
  mutate(regime = paste(constraint, "| Distance", distance)) %>%
  ggplot(aes(x = gap, colour = regime)) +
  stat_ecdf(size = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Percent gap (best feasible vs constrained feasible)",
    y = "CDF",
    colour = "Regime"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 2)) +
  theme(
    panel.grid.major = element_line(color = "grey60", size = 0.6),
    panel.grid.minor = element_line(color = "grey80", size = 0.4)
  )




####################################
#6) Ask questions and establish some nice facts :D 

apps_dt <- as.data.table(college_apps_2425) # mrun, year, COD_CARRERA_PREF, ORDEN_PREF

pro_dt <- as.data.table(all_info_programs)[
  , .(year,
      COD_CARRERA_PREF,
      AREA_CARRERA_GENERICA,
      field_merged,
      income_imp)
]

# parse income (or skip parse_number if already numeric)
pro_dt[, wage := readr::parse_number(as.character(income_imp))]
pro_dt[!is.finite(wage), wage := NA_real_]

apps_info <- merge(
  apps_dt,
  pro_dt,
  by = c("year","COD_CARRERA_PREF"),
  all.x = TRUE
)

# keep only rows with a valid rank
apps_info <- apps_info[!is.na(ORDEN_PREF)]



inv_rate <- function(w_pref_order) {
  n <- length(w_pref_order)
  if (n < 2L) return(NA_real_)
  m <- outer(w_pref_order, w_pref_order, FUN = "<")
  sum(m[upper.tri(m)]) / (n*(n-1)/2)
}

diag_rank_by_wage <- function(apps_info_dt, group_var = c("field_merged","AREA_CARRERA_GENERICA")) {
  group_var <- match.arg(group_var)
  
  dt <- as.data.table(copy(apps_info_dt))
  
  # drop missing group label
  dt <- dt[!is.na(dt[[group_var]])]
  
  # order by mrun, year, group_var, rank
  setorderv(dt, cols = c("mrun", "year", group_var, "ORDEN_PREF"))
  
  out <- dt[, {
    # keep only non-missing wages inside the cell
    keep <- !is.na(wage)
    w <- wage[keep]
    r <- ORDEN_PREF[keep]
    
    n_apps <- length(r)
    
    if (n_apps < 2L) {
      .(n_apps = n_apps,
        spearman = NA_real_,
        kendall  = NA_real_,
        inversion_rate = NA_real_,
        top_wage_rank1 = NA_integer_,
        regret_abs = NA_real_,
        regret_pct = NA_real_)
    } else {
      spe <- suppressWarnings(cor(w, -r, method = "spearman"))
      ken <- suppressWarnings(cor(w, -r, method = "kendall"))
      
      # wages in preference order
      w_pref <- w[order(r)]
      inv <- inv_rate(w_pref)
      
      w_rank1 <- w[which.min(r)]
      w_best  <- max(w, na.rm = TRUE)
      
      # is the highest-wage option the one with best (smallest) rank?
      top1 <- as.integer(which.min(r) == which.max(w))
      
      .(n_apps = n_apps,
        spearman = spe,
        kendall  = ken,
        inversion_rate = inv,
        top_wage_rank1 = top1,
        regret_abs = w_best - w_rank1,
        regret_pct = (w_best - w_rank1) / (w_rank1 + 1))
    }
  }, by = .(mrun, year, group = dt[[group_var]])]
  
  out[]
}



diag_field <- diag_rank_by_wage(apps_info, "field_merged")
diag_major <- diag_rank_by_wage(apps_info, "AREA_CARRERA_GENERICA")


diag_field[, .(
  n_cells = .N,
  mean_n_apps = mean(n_apps),
  share_topwage_rank1 = mean(top_wage_rank1, na.rm=TRUE),
  mean_spearman = mean(spearman, na.rm=TRUE),
  mean_inv_rate = mean(inversion_rate, na.rm=TRUE),
  p50_regret_abs = median(regret_abs, na.rm=TRUE),
  p90_regret_abs = quantile(regret_abs, 0.90, na.rm=TRUE)
)]


field_table <- diag_field[, .(
  n_cells = .N,
  mean_n_apps = mean(n_apps),
  share_topwage_rank1 = mean(top_wage_rank1, na.rm=TRUE),
  mean_inv_rate = mean(inversion_rate, na.rm=TRUE),
  p50_regret_abs = median(regret_abs, na.rm=TRUE),
  p90_regret_abs = quantile(regret_abs, 0.90, na.rm=TRUE)
), by = group][order(-n_cells)]

field_table[1:30]


