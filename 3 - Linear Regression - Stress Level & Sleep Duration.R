############################################################################

## Question 3 - Controlling for other factors, can decreasing your stress level 
##              increase your sleep duration?

############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####


# Libraries used ----------------------------------------------------------
library(tidyverse)

# Load updated Sleep Health dataset
load('data/SleepHealth_Updated.rda')

# Descriptive stats for stress level --------------------------------------
summary(sleep_health$Stress.Level)

# Descriptive stats for sleep duration --------------------------------------
summary(sleep_health$Sleep.Duration)



# Bar chart of stress level -----------------------------------------------

ggplot(sleep_health, aes(x = factor(Stress.Level))) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Self-reported Stress Level(1 = Low, 10 = High)",
    y = "Number of participants",
    title = "Distribution of self-reported Stress Level scores"
  ) + 
  theme_minimal(base_size = 14)

# Save image
ggsave("images/stress_level_chart.svg", width = 8, height = 6, dpi = 300)



# Histogram of sleep duration ---------------------------------------------

ggplot(sleep_health, aes(x = Sleep.Duration)) +
  geom_histogram(fill = "turquoise", 
                 colour = "white",
                 binwidth = 0.25
                 ) +
  labs(
    x = "Sleep duration (hours)",
    y = "Number of participants",
    title = "Distribution of sleep duration"
  ) + 
  theme_minimal(base_size = 14)

# Save image
ggsave("images/sleep_duration_histogram.svg", width = 8, height = 6, dpi = 300)
