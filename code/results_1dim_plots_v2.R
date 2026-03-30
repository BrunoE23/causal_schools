####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


setwd(data_wd)
#####################################

library(tidyverse)


#load("./data/clean/final_data.RData")
#pop_ref <- haven::read_dta("./data/clean/df_RC_VAM.dta")

#sample_type <- "V2"
#sample_type <- "P1"
sample_type <- "rcvam"




effects_schools <- read_csv(paste0("./data/clean/effects_schools_long_", sample_type, ".csv")) %>% 
  filter(effect_def == "mosttime")


theme_transparent <- theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )


plot_1dim <- function(this_outcome, param = 1.5){
  
  estimates <- effects_schools %>% 
    filter(!is.na(school_id)) %>% 
    filter(group == "all") %>% 
    filter(outcome == this_outcome) %>% 
    mutate(
      sig_lab = if_else(sig == 1, "Significant", "Not significant"),
      sig_lab = factor(sig_lab, levels = c("Significant", "Not significant")),
      ci_lo   = beta - 1.96 * se,
      ci_hi   = beta + 1.96 * se
    ) %>% 
    arrange(beta) %>% 
    mutate(rank = row_number())
  
  ggplot(estimates, aes(x = beta, y = rank)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    
    geom_errorbarh(
      data = estimates %>% filter(sig_lab == "Not significant"),
      aes(xmin = ci_lo, xmax = ci_hi),
      height = 0,
      color = "gray75",
      linewidth = 0.35
    ) +
    
    geom_errorbarh(
      data = estimates %>% filter(sig_lab == "Significant"),
      aes(xmin = ci_lo, xmax = ci_hi),
      height = 0,
      color = "gray25",
      linewidth = 0.5
    ) +
    
    geom_point(
      data = estimates %>% filter(sig_lab == "Not significant"),
      shape = 1,
      size = 1.4,
      stroke = 0.5,
      color = "gray55"
    ) +
    
    geom_point(
      data = estimates %>% filter(sig_lab == "Significant"),
      shape = 16,
      size = 1.4,
      color = "black"
    ) +
    
    theme_minimal(base_size = 12) +
    labs(
      x = "Estimate (β)",
      y = "School rank (within outcome)"
    ) +
    theme(
      legend.position = "none",
      panel.spacing.y = unit(1, "lines")
    ) +
    theme_transparent
}

plot_1dim("std_math",  param = 2)
ggsave(paste0(code_output_wd, "/output/figures/distr_eff_math_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600,
       bg = "transparent")


plot_1dim("std_leng",  param = 2)
ggsave(paste0(code_output_wd, "/output/figures/distr_eff_leng_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600,
       bg = "transparent")

plot_1dim("f_science_ml", param = 2)
ggsave(paste0(code_output_wd, "/output/figures/distr_eff_mat_sci_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600,
       bg = "transparent")