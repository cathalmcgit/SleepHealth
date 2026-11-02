############################################################################
###												                            		
###					              General Dataset Analysis - Sleep Health					 
###												
############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####


library(tidyverse)
library(ggplot2)

## Import CSV file
sleep_health_orig  <- read.csv("data/SleepHealth.csv")

# overview of data set
str(sleep_health_orig)
summary(sleep_health_orig)

# Checking the number of unique IDs
length(unique(sleep_health_orig$Person.ID))

# Count missing values in each variable
colSums(is.na(sleep_health_orig))

# Creating a data set with duplicate rows removed --------------------------

sleep_health <- sleep_health_orig %>%  
  distinct(across(-Person.ID), .keep_all = TRUE)

# overview of updated data set
str(sleep_health)
summary(sleep_health)

# Replacing "Normal Weight" with "Normal" in BMI.Category
sleep_health <- sleep_health %>% 
  mutate(BMI.Category = recode(BMI.Category,"Normal Weight" = "Normal"))

# Changing BMI.Category into an ordinal factor
sleep_health <- sleep_health %>% 
  mutate(BMI.Category = factor(BMI.Category,levels = c("Normal", "Overweight", "Obese"),ordered = TRUE))

# Splitting the blood pressure variable into systolic and diastolic
# Will become two numeric variables
sleep_health <- sleep_health %>% 
  separate(col = Blood.Pressure,into = c("Systolic_BP", "Diastolic_BP"),sep = "/",convert = TRUE)

# doing some quick checks on imported data
head(sleep_health) # View the first few rows
nrow(sleep_health) # Check the number of rows
ncol(sleep_health) # Check the number of columns / variables
str(sleep_health) # See the structure

summary(sleep_health)  ## Overview of data types etc 

save(sleep_health, file = 'data/SleepHealth_Updated.rda') 