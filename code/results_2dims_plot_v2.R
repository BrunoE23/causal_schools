sample_type <- "rcvam"

effects_schools <- read_csv(paste0("./data/clean/effects_schools_long_", sample_type, ".csv")) %>% 
  filter(effect_def == "mosttime")


outcome1<- "std_math"
outcome2<- "std_leng"


comparison_data <- haven::read_dta("./data/clean/df_pop_comparison.dta")


plot_2vars <- function(outcome1, outcome2, this_group = "all", data = effects_schools,
                       my_xlab = outcome1, my_ylab = outcome2) {
  
  effects_wide <- data %>%
    filter(group == this_group, outcome %in% c(outcome1, outcome2)) %>%
    select(outcome, school_id, beta, sig) %>% 
    filter(!is.na(school_id)) %>% 
    pivot_wider(
      names_from  = outcome,
      values_from = c(beta, sig),
      names_glue  = "{.value}_{outcome}"
    )
  
  x_sig <- paste0("sig_", outcome1)
  y_sig <- paste0("sig_", outcome2)
  x_var <- paste0("beta_", outcome1)
  y_var <- paste0("beta_", outcome2)
  
  effects_wide <- effects_wide %>% 
    mutate(
      sig_group = case_when(
        .data[[x_sig]] == 1 & .data[[y_sig]] == 1 ~ "Significant in both axes",
        .data[[x_sig]] == 1 & .data[[y_sig]] == 0 ~ "Significant in x-axis",
        .data[[x_sig]] == 0 & .data[[y_sig]] == 1 ~ "Significant in y-axis",
        TRUE ~ "Insignificant"
      ),
      sig_group = factor(
        sig_group,
        levels = c(
          "Significant in both axes",
          "Significant in x-axis",
          "Significant in y-axis",
          "Insignificant"
        )
      )
    )
  
  sample_sd_xvar <- sd(comparison_data[[outcome1]], na.rm = TRUE)
  sample_sd_yvar <- sd(comparison_data[[outcome2]], na.rm = TRUE)
  
  vals <- c(
    effects_wide[[x_var]] / sample_sd_xvar,
    effects_wide[[y_var]] / sample_sd_yvar
  )
  
  m <- min(quantile(abs(vals), 0.99, na.rm = TRUE), 1.5)
  lims <- c(-m, m)
  
  effects_wide %>% 
    ggplot(aes(
      x = .data[[x_var]] / sample_sd_xvar,
      y = .data[[y_var]] / sample_sd_yvar,
      color = sig_group
    )) +
    geom_point() +
    geom_smooth(
      data = subset(effects_wide, sig_group != "Insignificant"),
      aes(
        x = .data[[x_var]] / sample_sd_xvar,
        y = .data[[y_var]] / sample_sd_yvar
      ),
      color = "black",
      method = "lm",
      se = FALSE
    ) +
    coord_cartesian(xlim = lims, ylim = lims) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      x = my_xlab,
      y = my_ylab,
      title = "Scatter plot of school effects",
      subtitle = "Effects are expressed relative to population s.d.",
      color = ""
    )
}


outcome1<- "std_math"
outcome2<- "std_leng"

plot_2vars("std_math", "f_science_ml", my_xlab = "Effect on math exam", my_ylab = "Effect on prob. matriculating on STEM")
ggsave(paste0(code_output_wd, "/output/figures/2dim_math_mat_stem", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)


plot_2vars("std_math", "std_leng", my_xlab = "Effect on math exam", my_ylab = "Effect on verbal exam")
ggsave(paste0(code_output_wd, "/output/figures/2dim_math_lang", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)

