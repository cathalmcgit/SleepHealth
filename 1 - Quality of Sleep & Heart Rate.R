############################################################################
										                            		
## Question 1  - Is there a relationship between quality of sleep and heart rate?					 
											
############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####


##### Q1 possible input - Chihiro ####

# syntax below utilises the dplyr package 
library(dplyr) 

# summary statistics of heart rate (bpm) variable 
summary(sleep_health$Heart.Rate) 

# Load the ggplot2 package 
library(ggplot2) 

# Creating quality of sleep categories using sleep scores 
sleep_health <- sleep_health %>% 
  mutate(sleep_quality = case_when( 
    Quality.of.Sleep %in% c(1, 2) ~ "Terrible", 
    Quality.of.Sleep %in% c(3, 4) ~ "Poor", 
    Quality.of.Sleep %in% c(5, 6) ~ "Fair", 
    Quality.of.Sleep %in% c(7, 8) ~ "Good", 
    Quality.of.Sleep %in% c(9, 10) ~ "Excellent", 
    TRUE ~ NA_character_ 
  )) 

# Frequency count for each sleep quality category 
count(sleep_health, sleep_quality) 

# Creating heart rate vs quality of sleep category boxplot 
ggplot(sleep_health, aes(x = sleep_quality, y = Heart.Rate)) + 
  geom_boxplot(fill = "lightgreen") + 
  labs(title = "Heart rate by sleep quality", 
       x = "Sleep Quality", y = "Heart Rate (bpm)") + 
  theme_minimal(base_size = 14) 

# Save image
ggsave("images/heart_rate_and_sleep_quality.svg", width = 8, height = 6, dpi = 300)

# Conducting Shapiro Wilk Test to test for normality 
by(sleep_health$Heart.Rate, sleep_health$sleep_quality, shapiro.test) 

# Generate QQ plot using ggplot 
ggplot(sleep_health, aes(sample = Heart.Rate)) + 
  stat_qq(size = 2) + 
  stat_qq_line(color = "blue", linetype = "dashed", lwd = 1.1) + 
  facet_wrap(~ sleep_quality) + 
  labs(title = "Normal QQ Plot of Heart Rate by Sleep Quality", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") + 
  theme_minimal(base_size = 14) 

###################################################################