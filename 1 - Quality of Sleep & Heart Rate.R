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
ggsave("images/heart_rate_and_sleep_quality_boxplot.svg", width = 8, height = 6, dpi = 300)

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

# Save image
ggsave("images/heart_rate_and_sleep_quality_QQplot.svg", width = 8, height = 6, dpi = 300)

###################################################################


library(ggplot2)
library(dplyr)
library(car)

theme_set(theme_minimal(base_size = 16))
# ColorBrewer palettes: Set2 for filled areas, Dark2 for points and lines
dark2 <- RColorBrewer::brewer.pal(8, "Dark2")

# ---- Data -------------------------------------------------------------
# cleaned data created in "0 - General Dataset Analysis.R"
load("data/SleepHealth_Updated.rda")

# 1-2 very poor, 3-4 poor, 5-6 fair, 7-8 good, 9-10 excellent
sleep_health$Sleep.Quality.Cat <- cut(sleep_health$Quality.of.Sleep,
                                      breaks = c(0, 2, 4, 6, 8, 10),
                                      labels = c("Terrrible", "Poor", "Fair", "Good", "Excellent"))

# 3-6 terrible/poor, 5-6 fair, 7-8 good, 9-10 excellent
# (poor and fair combined due to small counts in the poor category)
sleep_health$Sleep.Quality.Cat <- cut(sleep_health$Quality.of.Sleep,
                                      breaks = c(0, 2, 6, 8, 10),
                                      labels = c("Terrible", "Poor/Fair", "Good", "Excellent"))

print(table(sleep_health$Sleep.Quality.Cat))

# no observations in "Terrible", so drop the empty level
sleep_health$Sleep.Quality.Cat <- droplevels(sleep_health$Sleep.Quality.Cat)

# H0: mean heart rate is equal across sleep quality groups
# H1: at least one group mean differs

# ---- Summary statistics -----------------------------------------------
hr_summary <- sleep_health %>%
  group_by(Sleep.Quality.Cat) %>%
  summarise(n = n(),
            mean = mean(Heart.Rate),
            sd = sd(Heart.Rate),
            se = sd / sqrt(n),
            median = median(Heart.Rate),
            IQR = IQR(Heart.Rate),
            min = min(Heart.Rate),
            max = max(Heart.Rate)) %>%
  mutate(ci_lower = mean - qt(0.975, n - 1) * se,
         ci_upper = mean + qt(0.975, n - 1) * se)
print(hr_summary)

# ---- Plots ------------------------------------------------------------
p_box <- ggplot(sleep_health, aes(Sleep.Quality.Cat, Heart.Rate, fill = Sleep.Quality.Cat)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.12, height = 0.25, alpha = 0.45, show.legend = FALSE) + # do we need jitter?
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") + #do we need the diamond shape?
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Heart rate by sleep quality category with poor and fair categories combined",
       x = "Sleep quality category", y = "Resting heart rate (bpm)") +
  theme_minimal()
print(p_box)

p_means <- ggplot(hr_summary, aes(Sleep.Quality.Cat, mean, colour = Sleep.Quality.Cat, group = 1)) +
  geom_line(colour = "grey60", linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.12, linewidth = 1) +
  geom_point(size = 4) +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  labs(title = "Mean heart rate with 95% confidence intervals with poor and fair combined",
       x = "Sleep quality category", y = "Mean heart rate (bpm)") +
  theme_minimal()
print(p_means)

# ---- Normality within groups -------------------------------------------
p_hist <- ggplot(sleep_health, aes(Heart.Rate, fill = Sleep.Quality.Cat)) +
  geom_histogram(binwidth = 2, colour = "white", show.legend = FALSE) +
  facet_wrap(~ Sleep.Quality.Cat, nrow = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Histograms of heart rate by group", x = "Heart rate (bpm)", y = "Count")
print(p_hist)

# Shapiro-Wilk per group
print(tapply(sleep_health$Heart.Rate, sleep_health$Sleep.Quality.Cat, shapiro.test))

# ---- Normality of HR data set ------------------------------------------
p_qq_all <- ggplot(sleep_health, aes(sample = Heart.Rate)) +
  stat_qq(colour = dark2[1]) +
  stat_qq_line(colour = "black") +
  labs(title = "Normal Q-Q plot of heart rate",
       x = "Theoretical quantiles", y = "Heart rate (bpm)")
print(p_qq_all)

