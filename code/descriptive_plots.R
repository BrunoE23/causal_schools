setwd("C:/Users/xd-br/Desktop/PhD/Research/causal_schools")
library(tidyverse)


load("./data/clean/final_data.RData")


#Unconditional

final_data %>%
  filter(!(is.na(gender))) %>% 
  ggplot(aes(x = avg_stem_share)) + geom_density(alpha = 0.5) + 
  labs(x = "Avg STEM content in college application", y = "Density") +
  theme_minimal() + 
  theme(legend.position = "bottom")


final_data %>%
filter(!(is.na(gender))) %>% 
  ggplot(aes(x = avg_stem_share, colour = gender, fill = gender)) + geom_density(alpha = 0.5) + 
  labs(x = "Avg STEM content in college application", y = "Density", fill = "Gender") +
  guides(colour = "none")  +
  theme_minimal() + 
  theme(legend.position = "bottom")

ggsave(path = "./output/figures/", filename = "descriptive_hist_outcome.png",
       width = 10, height = 5)

final_data %>%
  mutate(avg_stem_share_exp = ifelse(is.na(avg_stem_share), 0 , avg_stem_share)) %>% 
  filter(!(is.na(gender))) %>% 
  ggplot(aes(x = avg_stem_share_exp, colour = gender, fill = gender)) + geom_density(alpha = 0.5) + 
  labs(x = "Avg STEM content in college application, including 0s", y = "Density", fill = "Gender") +
  guides(colour = "none")  +
  theme_minimal() + 
  theme(legend.position = "bottom")

ggsave(path = "./output/figures/", filename = "descriptive_hist_outcome_w_0s.png",
       width = 10, height = 5)


#final_data %>%
#  filter(!(is.na(gender))) %>% 
#  filter(math_max >= 1) %>% 
#  ggplot(aes(x = math_max)) + geom_density(alpha = 0.5) + 
#  labs(x = "Math Exam Score", y = "Density", fill = "Gender") +
#  guides(colour = "none")  +
#  theme_minimal() + 
#  theme(legend.position = "bottom")


final_data %>%
  filter(!(is.na(gender))) %>% 
  filter(math_max >= 1) %>% 
  ggplot(aes(x = math_max, colour = gender, fill = gender)) + geom_density(alpha = 0.5) + 
  labs(x = "Math Exam Score", y = "Density", fill = "Gender") +
  guides(colour = "none")  +
  theme_minimal() + 
  theme(legend.position = "bottom")


ggsave(path = "./output/figures/", filename = "descriptive_hist_math.png",
       width = 10, height = 5)

#Conditioning on score 


final_data %>%
  ggplot(aes(x = math_max, y = avg_stem_share)) + 
  geom_smooth(formula = y~poly(x,2)) +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  labs(x = "Math Exam Score", y = "Avg STEM content in college application") 


ggsave(path = "./output/figures/", filename = "math_stem_plot.png",
       width = 10, height = 5)


final_data %>%
  ggplot(aes(x = math_max, y = avg_stem_share, color = gender)) + 
  geom_smooth(formula = y~poly(x,2)) +
#  geom_bin2d() +
  theme_minimal() + 
  theme(legend.position = "right") + 
  labs(x = "Math Exam Score", y = "Avg STEM content in college application", color = "Gender") 


ggsave(path = "./output/figures/", filename = "math_stem_gender_plot.png",
       width = 10, height = 5)



final_data %>%
  ggplot(aes(x = math_max, y = avg_stem_share, color = gender_science)) + 
  geom_smooth(formula = y~poly(x,2)) +
  theme_minimal() + 
  theme(legend.position = "right") + 
  labs(x = "Math Exam Score", y = "Avg STEM content in college application", color = "Gender; Took Science Exam?") 

ggsave(path = "./output/figures/", filename = "math_stem_gender_science_exam_plot.png",
       width = 10, height = 5)



# 3D Plots ######
#Are they really necessary? 

install.packages("plotly")
library(plotly)


plot_ly(x = final_data$math_max, y = final_data$leng_max, 
        z = final_data$avg_stem_share,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = final_data$female, colorscale = "Viridis"))



### Chat GPT 

library(plotly)

# Fit smooth surface with LOESS (tune span for smoothness)
lo <- loess(n_stem_low ~ math_max + leng_max, data = final_data, span = 0.6, degree = 2)

# Make a grid to predict on
x_seq <- seq(min(final_data$math_max, na.rm = TRUE),
             max(final_data$math_max, na.rm = TRUE), length.out = 50)
y_seq <-  seq(min(final_data$leng_max, na.rm = TRUE),
              max(final_data$leng_max, na.rm = TRUE), length.out = 50)

grid  <- expand.grid(math_max = x_seq, leng_max = y_seq)

# Predict z on grid and cast to matrix

z_hat <- as.vector(predict(lo, newdata = grid))

# Put on a matrix for surface plotting
Z <- matrix(z_hat, nrow = length(x_seq), ncol = length(y_seq))

#library(plotly)
plot_ly(x = ~x_seq, y = ~y_seq, z = ~Z) %>%
  add_surface(colorscale = "Viridis")



##############################
#### Separate by gender 



library(dplyr)
library(plotly)

# Assume you have a column called "sex" with values "Male"/"Female"
lo_male <- loess(avg_stem_share ~ math_max + leng_max,
                 data = filter(final_data, female == 0),
                 span = 0.6, degree = 2, na.action = na.exclude)

lo_female <- loess(avg_stem_share ~ math_max + leng_max,
                   data = filter(final_data, female == 1),
                   span = 0.6, degree = 2, na.action = na.exclude)

# Build grid
x_seq <- seq(min(final_data$math_max, na.rm = TRUE),
             max(final_data$math_max, na.rm = TRUE), length.out = 50)
y_seq <- seq(min(final_data$leng_max, na.rm = TRUE),
             max(final_data$leng_max, na.rm = TRUE), length.out = 50)

grid <- expand.grid(math_max = x_seq, leng_max = y_seq)

# Predict separately
z_male   <- as.vector(predict(lo_male, newdata = grid))
z_female <- as.vector(predict(lo_female, newdata = grid))

Z_male   <- matrix(z_male,   nrow = length(x_seq), ncol = length(y_seq))
Z_female <- matrix(z_female, nrow = length(x_seq), ncol = length(y_seq))

# Plot both surfaces
plot_ly() %>%
  add_surface(x = ~x_seq, y = ~y_seq, z = ~Z_male,
              colorscale = "Blues", opacity = 0.6,
              name = "Male") %>%
  add_surface(x = ~x_seq, y = ~y_seq, z = ~Z_female,
              colorscale = "Reds", opacity = 0.6,
              name = "Female") %>%
  layout(scene = list(
    xaxis = list(title = "Math Max"),
    yaxis = list(title = "Leng Max"),
    zaxis = list(title = "Avg STEM Share")
  ))