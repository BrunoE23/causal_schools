####################################
data_wd        <-  "C:/Users/xd-br/Dropbox/causal_schools"
code_output_wd <-  "C:/Users/xd-br/Desktop/PhD/Research/causal_schools"

#data_wd <- "C:/Users/brunem/Dropbox/causal_schools"
#code_output_wd <-  "C:/Users/brunem/Research/causal_schools"


setwd(data_wd)
#####################################

library(tidyverse)


load("./data/clean/final_data.RData")
load("./data/clean/all_apps.RData")

sample_type <- "V2"
sample_type <- "P1"

effects_schools <- read_csv(paste0("./data/clean/effects_schools_long_", sample_type, ".csv"))


plot_1dim <- function(this_outcome, ref_data = final_data, param = 1.5){

  
m <- sd(ref_data[[this_outcome]], na.rm = TRUE) * param
lims <- c(-m, m)


estimates <- effects_schools %>% 
  filter(!is.na(school_id)) %>% 
  filter(group == "all") %>% 
  filter(outcome == this_outcome) %>% 
  arrange((beta))

ids_in_order <- unique(estimates$school_id)

estimates <- estimates %>%
  mutate(
    school_id = factor(school_id, levels = ids_in_order),
    sig_lab   = ifelse(sig == 1, "Significant", "Not significant"),
    ci_lo = beta - 1.96 * se,
    ci_hi = beta + 1.96 * se
  )


est2 <- estimates %>%
  mutate(
    sig_lab = if_else(sig == 1, "Significant", "Not significant"),
    sig_lab = factor(sig_lab, levels = c("Significant", "Not significant")),
    ci_lo = beta - 1.96 * se,
    ci_hi = beta + 1.96 * se
  ) %>%
  arrange(beta) %>%
#  group_by(sig_lab) %>%
  mutate(rank = row_number()) %>%
  ungroup()

ggplot(est2, aes(x = beta, y = rank)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0, color = "gray70") +
  geom_point(aes(color = sig_lab), size = 1.2) +
  facet_wrap(~ sig_lab, ncol = 2) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Estimate (β)",
    y = "School rank (within outcome)",
    color = ""
  ) +
  theme(
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")
  )  + coord_cartesian(xlim = lims) 


}
  

plot_1dim("n_years_rbd_target_post", param = 2)
ggsave(paste0(code_output_wd, "/output/figures/distr_eff_years_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)

plot_1dim("math_max")
ggsave(paste0(code_output_wd, "/output/figures/distr_eff_math_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)

plot_1dim("avg_stem_share")
ggsave(paste0(code_output_wd, "/output/figures/distr_eff_stem_", sample_type, ".png"),
       width = 6.5, height = 4, units = "in",
       dpi = 600)

