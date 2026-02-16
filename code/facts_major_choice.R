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
compute_feasible_sets_max_money <- function(
    students,
    programs,
    apps,                        # college_apps_2425
    strata_cols = "year",
    block_size = 20000L,
    
    id_col = "mrun",
    program_id_col = "COD_CARRERA_PREF",
    cutoff_col = "PTJE_corte",
    money_col = "income_imp",
    
    # student components (K)
    score_cols_K  = c("PTJE_NEM","PTJE_RANKING","leng_max","math_max","math2_max",
                      "hist_max","scien_max","elect_max"),
    
    # program weights (ELECT created)
    weight_cols_base = c("NEM","RANKING","CLEC","M1","M2","HSCO","CIEN"),
    
    # apps columns (your table)
    apps_id_col = "mrun",
    apps_year_col = "year",
    apps_program_col = "COD_CARRERA_PREF",
    
    # M2 requirement (your data uses 0 == not taken)
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
  stopifnot(all(c(apps_id_col, apps_year_col, apps_program_col) %in% names(apps)))
  stopifnot(m2_score_col %in% names(students))
  
  # Keys
  setkeyv(students, strata_cols)
  setkeyv(programs, strata_cols)
  setkeyv(apps, c(apps_year_col, apps_id_col))  # year,mrun
  
  strata_vals <- unique(students[, ..strata_cols])
  
  out <- vector("list", nrow(strata_vals))
  out_i <- 0L
  
  for (s in seq_len(nrow(strata_vals))) {
    key_row <- strata_vals[s]
    yr <- key_row[[strata_cols[1]]]  # assumes strata_cols="year"
    
    if (print_outer) {
      cat("Starting stratum", s, "of", nrow(strata_vals),
          "=>", yr, "at", format(Sys.time()), "\n")
    }
    
    stu_s <- students[key_row, nomatch = 0, on = strata_cols]
    pro_s <- programs[key_row, nomatch = 0, on = strata_cols]
    if (nrow(stu_s) == 0L || nrow(pro_s) == 0L) next
    
    # apps for this year
    apps_y <- apps[.(yr), on = apps_year_col, nomatch = 0L]
    
    # ---- Numeric coercion (do BEFORE NA->0) ----
    for (cc in score_cols_K) set(stu_s, j = cc, value = as.numeric(stu_s[[cc]]))
    for (cc in weight_cols_base) set(pro_s, j = cc, value = as.numeric(pro_s[[cc]]))
    set(pro_s, j = cutoff_col, value = as.numeric(pro_s[[cutoff_col]]))
    
    # --- HARD REQUIREMENT FLAGS (must be BEFORE NA->0) ---
    has_M2_all   <- !is.na(stu_s$math2_max)  & (stu_s$math2_max  != 0)
    has_HSCO_all <- !is.na(stu_s$hist_max)  & (stu_s$hist_max   != 0)
    has_CIEN_all <- !is.na(stu_s$scien_max) & (stu_s$scien_max  != 0)
    has_ELECT_all <- has_HSCO_all | has_CIEN_all
    
    
    # ---- Student NA scores -> 0 (keeps Sb numeric) ----
    for (cc in score_cols_K) {
      ii <- which(is.na(stu_s[[cc]]))
      if (length(ii)) set(stu_s, i = ii, j = cc, value = 0)
    }
    
    # ---- Program NA weights -> 0 ----
    for (cc in weight_cols_base) {
      ii <- which(is.na(pro_s[[cc]]))
      if (length(ii)) set(pro_s, i = ii, j = cc, value = 0)
    }
    
    # ---- Drop NA cutoffs ----
    pro_s <- pro_s[!is.na(get(cutoff_col))]
    if (nrow(pro_s) == 0L) next
    
    # ---- ELECTIVE FIX (HSCO & CIEN both positive => equal weights; ELECT * elect_max) ----
    pro_s[, ELECT := 0]
    both_pos <- (pro_s$HSCO > 0) & (pro_s$CIEN > 0)
    if (any(both_pos)) {
      pro_s[both_pos, ELECT := HSCO]
      pro_s[both_pos, `:=`(HSCO = 0, CIEN = 0)]
    }
    
    weight_cols_K <- c("NEM","RANKING","CLEC","M1","M2","HSCO","CIEN","ELECT")
    
    # ---- Percent -> share + renormalize per program ----
    Wm <- as.matrix(pro_s[, ..weight_cols_K])
    Wm[is.na(Wm)] <- 0
    Wm <- Wm / 100
    rs <- rowSums(Wm); rs[rs == 0] <- 1
    Wm <- Wm / rs
    pro_s[, (weight_cols_K) := as.data.table(Wm)]
    
    # ---- Matrices/vectors ----
    W      <- t(as.matrix(pro_s[, ..weight_cols_K]))  # K × J
    cutoff <- as.numeric(pro_s[[cutoff_col]])         # J
    pid    <- pro_s[[program_id_col]]                 # J
    
    N <- nrow(stu_s)
    J <- nrow(pro_s)
    X_all <- as.matrix(stu_s[, ..score_cols_K])       # N × K
    stu_id <- stu_s[[id_col]]
    
    # ---- Objective money ----
    money <- readr::parse_number(as.character(pro_s[[money_col]]))
    money[!is.finite(money)] <- -Inf
    if (all(!is.finite(money))) stop("All money NA/Inf in year=", yr)
    
    # program_id -> column index j
    pid_to_j <- setNames(seq_len(J), as.character(pid))
    
    # ---- M2 requirement vector (per program) ----
    req_M2 <- pro_s$M2 > 0     # length J, TRUE means program requires M2
    req_HSCO  <- pro_s$HSCO  > 0
    req_CIEN  <- pro_s$CIEN  > 0
    req_ELECT <- pro_s$ELECT > 0
    
    # outputs
    n_feasible <- integer(N)
    
    best_all_prog <- rep(NA, N)
    best_all_obj  <- rep(NA_real_, N)
    
    n_applied <- integer(N)
    n_applied_feasible <- integer(N)
    best_app_prog <- rep(NA, N)
    best_app_obj  <- rep(NA_real_, N)
    
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
      
      # scores
      Sb <- X_all[rows, , drop = FALSE] %*% W
      
      # base eligibility by cutoff
      elig <- sweep(Sb, 2L, cutoff, FUN = ">=")
      elig[is.na(elig)] <- FALSE
      
      # =========================================================
      # HARD REQUIREMENT: if program requires M2, student must have M2
      # Your convention: math2_max == 0 means "no M2"
      # Build nb×J mask efficiently via tcrossprod
      # =========================================================
      has_M2    <- has_M2_all[rows]
      has_HSCO  <- has_HSCO_all[rows]
      has_CIEN  <- has_CIEN_all[rows]
      has_ELECT <- has_ELECT_all[rows]
      
      ok_M2    <- (tcrossprod(!has_M2,    req_M2)    == 0)
      ok_HSCO  <- (tcrossprod(!has_HSCO,  req_HSCO)  == 0)
      ok_CIEN  <- (tcrossprod(!has_CIEN,  req_CIEN)  == 0)
      ok_ELECT <- (tcrossprod(!has_ELECT, req_ELECT) == 0)
      
      elig <- elig & ok_M2 & ok_HSCO & ok_CIEN & ok_ELECT
      # =========================================================
      
      n_feasible[rows] <- rowSums(elig)
      
      # ---- Best among ALL eligible ----
      Obj_all <- matrix(money, nrow = nb, ncol = J, byrow = TRUE)
      Obj_all[!elig] <- -Inf
      argmax_all <- max.col(Obj_all, ties.method = "first")
      mx_all <- Obj_all[cbind(seq_len(nb), argmax_all)]
      mx_all[!is.finite(mx_all)] <- NA_real_
      
      best_all_obj[rows]  <- mx_all
      best_all_prog[rows] <- pid[argmax_all]
      best_all_prog[rows][is.na(mx_all)] <- NA
      
      # ---- Best among APPLIED & eligible ----
      stu_ids_block <- stu_id[rows]
      row_in_block <- setNames(seq_along(stu_ids_block), as.character(stu_ids_block))
      
      # pull apps for these students
      apps_block <- apps_y[.(stu_ids_block), on = apps_id_col, nomatch = 0L,
                           .(mrun = get(apps_id_col),
                             prog_j = pid_to_j[as.character(get(apps_program_col))])]
      
      apps_block <- apps_block[!is.na(prog_j)]
      
      if (nrow(apps_block) > 0L) {
        apps_list <- split(apps_block$prog_j, apps_block$mrun)
        
        for (sid_chr in names(apps_list)) {
          ii <- row_in_block[[sid_chr]]
          js <- apps_list[[sid_chr]]
          
          n_applied[rows[ii]] <- length(js)
          
          ok_elig <- elig[ii, js]
          n_applied_feasible[rows[ii]] <- sum(ok_elig)
          
          # also require finite money for objective
          ok <- ok_elig & is.finite(money[js])
          if (!any(ok)) next
          
          js_ok <- js[ok]
          j_best <- js_ok[which.max(money[js_ok])]
          
          best_app_prog[rows[ii]] <- pid[j_best]
          best_app_obj[rows[ii]]  <- money[j_best]
        }
      }
      
      rm(Sb, elig, ok_M2, Obj_all, argmax_all, mx_all, apps_block)
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


test_cf <- compute_feasible_sets_max_money(
  students = students_apps_2425,
  programs = all_info_programs,
  apps     = college_apps_2425,
  strata_cols = "year",
  block_size = 20000L
)

test_cf %>% 
  filter(n_applied > 0) %>% 
  View()

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


####################################
#5) Ask questions and establish some nice facts :D 
