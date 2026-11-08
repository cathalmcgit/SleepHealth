############################################################################

## Question 2 - Is there a relationship between sleep disorders and BMI category?			 

############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####

# Import libraries
library(ggplot2)

############################################################################

### Bar Charts & Frequency Tables - Univariate Analysis of Categorical Variables

############################################################################

## Original Dataset 

# Import original CSV file
sleep_health_orig  <- read.csv("data/SleepHealth.csv")

# Bar Chart of Sleep Disorders (Original Dataset) - Displaying values and percentages
ggplot(sleep_health_orig, 
       aes(x = reorder(Sleep.Disorder, -table(Sleep.Disorder)[Sleep.Disorder]), fill = Sleep.Disorder)) + # Arranges bars in order of frequency
  geom_bar(width = 0.65) +
  geom_text(stat = "Count", aes(label = after_stat(paste0(count, " (", round(count / sum(count) * 100, 1), "%)"))), # Add counts & percentages as text to bars
    vjust = -0.5, size = 4.5) + # Formats text
  scale_fill_brewer(palette = "Set1") +    # Allows each bar to be a different colour
  labs(title = "Number of Survey Respondents per Sleep Disorder",
    x = "Sleep Disorder",y = "Number of respondents") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",plot.title = element_text(face = "bold")) # Remove legend and formats text

# Frequency table for Sleep Disorder on Original Dataset
sleep_disorder_ftable_orig  <- cbind(Frequency = table(sleep_health_orig$Sleep.Disorder),
                                     Percentage = paste0(round(prop.table(table(sleep_health_orig$Sleep.Disorder)) * 100, 1),"%"))

# Bar Chart of BMI Category (Original Dataset) - Displaying values and percentages
ggplot(sleep_health_orig, 
       aes(x = reorder(BMI.Category, -table(BMI.Category)[BMI.Category]), fill = BMI.Category)) + # Arranges bars in order of frequency
  geom_bar(width = 0.65) +
  geom_text(stat = "Count", aes(label = after_stat(paste0(count, " (", round(count/sum(count) * 100, 1), "%)"))), # Add counts & percentages as text to bars
            vjust = -0.5, size = 4.5) + # Formats text
  scale_fill_brewer(palette = "Set1") +    # Allows each bar to be a different colour
  labs(title = "Number of Survey Respondents per BMI.Category",
       x = "BMI.Category",y = "Number of respondents") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",plot.title = element_text(face = "bold")) # Remove legend and formats text

# Frequency table for BMI Category on Original Dataset
bmi_ftable_orig <- cbind(Frequency = table(sleep_health_orig$BMI.Category),
                         Percentage = paste0(round(prop.table(table(sleep_health_orig$BMI.Category)) * 100, 1),"%"))

############################################################################

## Updated Dataset

# Load updated Sleep Health dataset
load('data/SleepHealth_Updated.rda')

# Bar Chart of Sleep Disorders (Updated Dataset) - Displaying values and percentages
sleep_disorder_bar_chart <- ggplot(sleep_health, 
  aes(x = reorder(Sleep.Disorder, -table(Sleep.Disorder)[Sleep.Disorder]), fill = Sleep.Disorder)) + # Arranges bars in order of frequency
  geom_bar(width = 0.65) +
  geom_text(stat = "Count", aes(label = after_stat(paste0(count, " (", round(count / sum(count) * 100, 1), "%)"))), # Add counts & percentages as text to bars
            vjust = -0.5, size = 4.5) + # Formats text
  scale_fill_brewer(palette = "Set1") +    # Allows each bar to be a different colour
  labs(title = "Number of Survey Respondents per Sleep Disorder",
       x = "Sleep Disorder",y = "Number of respondents") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",plot.title = element_text(face = "bold")) # Remove legend and formats text

# Save image
ggsave("images/sleep_disorder_chart.png", width = 8, height = 6, dpi = 300)

# Frequency table for Sleep Disorder on Updated Dataset
sleep_disorder_ftable  <- cbind(Frequency = table(sleep_health$Sleep.Disorder),
                                     Percentage = paste0(round(prop.table(table(sleep_health$Sleep.Disorder)) * 100, 1),"%"))

# Bar Chart of BMI Category (Updated Dataset) - Displaying values and percentages
bmi_bar_chart<- ggplot(sleep_health, 
  aes(x = reorder(BMI.Category, -table(BMI.Category)[BMI.Category]), fill = BMI.Category)) + # Arranges bars in order of frequency
  geom_bar(width = 0.65) +
  geom_text(stat = "Count", aes(label = after_stat(paste0(count, " (", round(count/sum(count) * 100, 1), "%)"))), # Add counts & percentages as text to bars
            vjust = -0.5, size = 4.5) + # Formats text
  scale_fill_brewer(palette = "Set1") +    # Allows each bar to be a different colour
  labs(title = "Number of Survey Respondents per BMI.Category",
       x = "BMI.Category",y = "Number of respondents") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",plot.title = element_text(face = "bold")) # Remove legend and formats text

# Save image
ggsave("images/bmi_chart.png", width = 8, height = 6, dpi = 300)

# Frequency table for BMI Category on Updated Dataset
bmi_ftable <- cbind(Frequency = table(sleep_health$BMI.Category),
                         Percentage = paste0(round(prop.table(table(sleep_health$BMI.Category)) * 100, 1),"%"))



## Contingency Table
bmi_disorder_table <- table(Sleep_Disorder = sleep_health$Sleep.Disorder, BMI = sleep_health$BMI.Category)

# adding row and column sums
bmi_disorder_table_margins <- addmargins(table(Sleep_Disorder = sleep_health$Sleep.Disorder, BMI = sleep_health$BMI.Category), c(1,2))

bmi_disorder_table

# Calculate row percentages
percent_ctable <- round(prop.table(bmi_disorder_table[-nrow(bmi_disorder_table), -ncol(bmi_disorder_table)], 1) * 100, 1)

percent_ctable
