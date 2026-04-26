suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# ------------------------- Configuration -------------------------

data_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_DATA_WD",
  unset = "C:/Users/xd-br/Dropbox/causal_schools"
)

repo_wd <- Sys.getenv(
  "CAUSAL_SCHOOLS_REPO_WD",
  unset = "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"
)

values_path <- file.path(
  data_wd,
  "data/clean/school_rbd_observational_values/school_rbd_observational_values.csv"
)


school_directory_root <- file.path(data_wd, "data/raw/school_directory")

dependency_group_order <- c("1", "2", "3", "4", "5")
dependency_group_labels <- c(
  "1" = "Municipal",
  "2" = "Particular Subvencionado",
  "3" = "Particular Pagado",
  "4" = "Administracion Delegada",
  "5" = "Servicio Local de Educacion"
)
dependency_detail_order <- c("1", "2", "3", "4", "5", "6")
dependency_detail_labels <- c(
  "1" = "Corporacion Municipal",
  "2" = "Municipal DAEM",
  "3" = "Particular Subvencionado",
  "4" = "Particular Pagado",
  "5" = "Administracion Delegada",
  "6" = "Servicio Local de Educacion"
)


# ------------------------- Helpers -------------------------

value_col <- function(value_type, outcome) {
  paste(value_type, outcome, sep = "__")
}

null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}



latest_school_directory <- function(root) {
  if (!dir.exists(root)) {
    return(NULL)
  }
  
  files <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }
  
  years <- str_extract(files, "\\b20[0-9]{2}\\b") %>% as.integer()
  files[which.max(years)]
}

read_school_metadata <- function(root) {
  directory_file <- latest_school_directory(root)
  
  if (is.null(directory_file)) {
    return(NULL)
  }
  
  metadata <- read_delim(
    directory_file,
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "Latin1")
  )
  
  if (!"RBD" %in% names(metadata)) {
    return(NULL)
  }
  
  metadata %>%
    transmute(
      school_rbd = as.numeric(RBD),
      school_name = if ("NOM_RBD" %in% names(metadata)) NOM_RBD else NA_character_,
      region_code = if ("COD_REG_RBD" %in% names(metadata)) as.character(COD_REG_RBD) else NA_character_,
      region_name = if ("NOM_REG_RBD_A" %in% names(metadata)) NOM_REG_RBD_A else NA_character_,
      comuna_code = if ("COD_COM_RBD" %in% names(metadata)) as.character(COD_COM_RBD) else NA_character_,
      comuna_name = if ("NOM_COM_RBD" %in% names(metadata)) NOM_COM_RBD else NA_character_,
      dependency_group_code = if ("COD_DEPE2" %in% names(metadata)) as.character(COD_DEPE2) else NA_character_,
      dependency_group_label = if ("COD_DEPE2" %in% names(metadata)) recode(as.character(COD_DEPE2), !!!dependency_group_labels, .default = NA_character_) else NA_character_,
      dependency_detail_code = if ("COD_DEPE" %in% names(metadata)) as.character(COD_DEPE) else NA_character_,
      dependency_detail_label = if ("COD_DEPE" %in% names(metadata)) recode(as.character(COD_DEPE), !!!dependency_detail_labels, .default = NA_character_) else NA_character_,
      rural = if ("RURAL_RBD" %in% names(metadata)) as.character(RURAL_RBD) else NA_character_
    ) %>%
    distinct(school_rbd, .keep_all = TRUE)
}



# ------------------------- Load values -------------------------

if (!file.exists(values_path)) {
  stop("Values file does not exist. Run 01_construct_school_rbd_values.R first: ", values_path)
}



values <- read_csv(values_path, show_col_types = FALSE)

metadata <- read_school_metadata(school_directory_root)


#outcome_var <- "stem_enrollment_m1"
outcome_var <- "z_year_math_max"

adj_type <- "controlled_value_added"

values %>% 
  filter(base_outcome == outcome_var) %>% 
  filter(n_students_school >= 20) %>% 
  filter(!is.na(.data[[adj_type]])) %>% 
  arrange(desc(.data[[adj_type]])) %>% 
#  head(20) %>% 
  tail(20) %>% 
  left_join(metadata, by = "school_rbd") %>% 
  select(
    school_rbd,
    school_name,
    all_of(adj_type),
    region_code,
    comuna_name,
    dependency_detail_label,
    n_students_school
  ) %>% 
  View()


univ_gr8 <- read_csv("data/clean/univ_gr8_df.csv")

univ_gr8 %>% 
  filter(most_time_RBD == 25524) %>% 
  pull(EDAD_ALU) %>% 
  table()
