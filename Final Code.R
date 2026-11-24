#*******************************************************************************
#
#  Group 11 - Sleep Health Data Set Analysis	----
#  See Document Outline (☰) for chapters    ->>>>>
#
#*******************************************************************************

# Load Libraries ----
library(car)
library(dplyr)
library(ggplot2)
library(tidyverse)

#*******************************************************************************

# Importing, Cleaning & Preparing Data ----

### Slide 1 ----

# Import CSV file
sleep_health_original <- read.csv("data/SleepHealth.csv")

# Checks on imported data
head(sleep_health_original) # View the first few rows

# Overview of data set
str(sleep_health_original)
summary(sleep_health_original)

# Checking the number of rows, columns and unique IDs
nrow(sleep_health_original) 
ncol(sleep_health_original)
length(unique(sleep_health_original$Person.ID))

# Count missing values in each variable
colSums(is.na(sleep_health_original))

### Creating a data set with duplicate rows removed

sleep_health <- sleep_health_original %>%  
  distinct(across(-Person.ID), .keep_all = TRUE)

# Replacing "Normal Weight" with "Normal" in BMI.Category
sleep_health <- sleep_health %>% 
  mutate(BMI.Category = recode(BMI.Category,"Normal Weight" = "Normal"))

# Changing BMI.Category into an ordinal factor
sleep_health <- sleep_health %>% 
  mutate(BMI.Category = factor(BMI.Category,levels = c("Normal", "Overweight", "Obese"),ordered = TRUE))

# Changing Sleep.Disorder into an ordinal factor
sleep_health <- sleep_health %>% 
  mutate(Sleep.Disorder = factor(Sleep.Disorder,levels = c("None", "Sleep Apnea", "Insomnia"),ordered = TRUE))

# Creating quality of sleep categories using sleep scores 
sleep_health <- sleep_health %>% 
  mutate(sleep_quality = case_when( 
    Quality.of.Sleep %in% c(1, 2) ~ "Terrible", 
    Quality.of.Sleep %in% c(3, 4) ~ "Poor", 
    Quality.of.Sleep %in% c(5, 6) ~ "Fair", 
    Quality.of.Sleep %in% c(7, 8) ~ "Good", 
    Quality.of.Sleep %in% c(9, 10) ~ "Excellent", 
    TRUE ~ NA_character_ )) 

# overview of updated data set
str(sleep_health)
summary(sleep_health)

# # Splitting the blood pressure variable into systolic and diastolic
# # Will become two numeric variables
# sleep_health <- sleep_health %>% 
#   separate(col = Blood.Pressure,into = c("Systolic_BP", "Diastolic_BP"),sep = "/",convert = TRUE)

# doing some quick checks on imported data   
head(sleep_health) # View the first few rows
nrow(sleep_health) # Check the number of rows
ncol(sleep_health) # Check the number of columns / variables
str(sleep_health) # See the structure

summary(sleep_health)  ## Overview of data types etc 

save(sleep_health, file = 'data/SleepHealth_Updated.rda') 

#*******************************************************************************
#
#  Question 1	- Is there a relationship between quality of sleep and heart rate?	----
#
#*******************************************************************************

## Q1 - Exploratory Data Analysis ----



## Q1 - Hypothesis Testing (ANOVA) ----

# H0: mean heart rate is equal across sleep quality groups
# H1: at least one group mean differs


#*******************************************************************************
#
#  Question 2	- Is there a relationship between sleep disorders and BMI category?	----
#
#*******************************************************************************

## Q2 - Exploratory Data Analysis ----

## Q2 - Hypothesis Testing (Chi-Squared) ----


#*******************************************************************************
#
#  Question 3	- Controlling for other factors, can decreasing your stress level increase your sleep duration?	----
#
#*******************************************************************************

## Q3 - Exploratory Data Analysis ----

## Q3 - Linear Regression ----