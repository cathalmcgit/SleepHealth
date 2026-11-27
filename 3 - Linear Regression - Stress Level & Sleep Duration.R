############################################################################

## Question 3 - Controlling for other factors, can decreasing your stress level 
##              increase your sleep duration?

############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####


# Libraries used ----------------------------------------------------------
library(tidyverse)

# Load updated Sleep Health dataset
load('data/SleepHealth_Updated.rda')

# *** Descriptive stats for stress level --------------------------------------

# Summary statistics
summary(sleep_health$Stress.Level)

# Variance = 3.029
var(sleep_health$Stress.Level)

# Bar chart of stress level -----------------------------------------------
# Using a bar chart as it is an ordinal rating scale
# Shows it is not normally distributed

ggplot(sleep_health, aes(x = factor(Stress.Level))) +
  geom_bar(fill = "#1B9E77") +
  labs(
    x = "Self-reported Stress Level(1 = Low, 10 = High)",
    y = "Number of participants",
    title = "Distribution of self-reported Stress Level scores"
  ) + 
  theme_minimal(base_size = 14)

# Save image
ggsave("images/stress_level_chart.svg", width = 8, height = 6, dpi = 300)





# *** Descriptive stats for sleep duration --------------------------------------

# Summary statistics
summary(sleep_health$Sleep.Duration)

# Variance = 0.6011
var(sleep_health$Sleep.Duration)


# Histogram of sleep duration ---------------------------------------------
# Shows it is not normally distributed

ggplot(sleep_health, aes(x = Sleep.Duration)) +
  geom_histogram(fill = "#7570B3", 
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




# *** Exploration of relationship of sleep duration and stress ------------

# Scatterplot of sleep duration and stress level ---------------------------
# Shows a linear trend of decreasing sleep duration with increased stress level

ggplot(sleep_health, aes(x = Stress.Level, y = Sleep.Duration)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Scatterplot of Sleep Duration and Stress",
       x = "Stress Level (1 = Low, 10 = High)",
       y = "Sleep Duration (hours)") +
  theme_minimal(base_size = 14)

# Save image
ggsave("images/Q3/sleep_stress_scatter.svg", width = 8, height = 6, dpi = 300)



# ** Testing for normality ---------------------------------------------------


# QQ Plot for Sleep Duration ----------------------------------------------

# Generate QQ plot using ggplot for Sleep Duration
ggplot(sleep_health, aes(sample = Sleep.Duration)) +
  stat_qq(size = 2) +
  stat_qq_line(color = "#7570B3", linetype = "dashed", lwd = 1.1) +
  labs(title = "Normal QQ Plot of Sleep Duration",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal(base_size = 14)

# Save image
ggsave("images/Q3/sleep_duration_qqplot.svg", width = 8, height = 6, dpi = 300)

# Plot shows that Sleep Duration has observations which depart from normality


# Formal normality test - Shapiro-Wilk test -------------------------------
# Using Shapiro-Wilk as it is a reliable test of normality for small to 
# medium data sets.

shapiro.test(sleep_health$Sleep.Duration)

# This gives us a p-value of 0.0000368 which would suggest there is enough
# evidence to reject the null hypothesis (data is normally distributed) and 
# accept the alternative hypothesis that the sleep duration data significantly
# departs from normality.


# Calculating the initial correlation -------------------------------------
# As the data is not normally distributed, but there appears to be a linear 
# relationship and stress is ordinal data, Spearman Rank test will be used

cor(sleep_health$Stress.Level, 
    sleep_health$Sleep.Duration, 
    method = "spearman")

# Spearman Rank correlation = -0.8328656
# Indicates a strong correlation as is close to -1



# *** Potential confounders -----------------------------------------------

# Age relationship with stress --------------------------------------------

# Visualisation of the relationship
ggplot(sleep_health, 
       aes(x = Age, y = Stress.Level)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Age and Stress Level",
    x = "Age (years)",
    y = "Stress Level (1–10)"
  ) +
  theme_minimal(base_size = 14)

# Spearman Rank test for correllation 
cor(sleep_health$Age, 
    sleep_health$Stress.Level, 
    method = "spearman")

# Spearman Rank value: -0.382 suggests a moderate negative correlation with stress



# Age relationship with Sleep Duration ------------------------------------

# Visualisation of the relationship
ggplot(sleep_health, 
       aes(x = Age, y = Sleep.Duration)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Age and Sleep Duration",
    x = "Age (years)",
    y = "Sleep Duration (hours)"
  ) +
  theme_minimal(base_size = 14)

# Spearman Rank test for correllation 
cor(sleep_health$Age, 
    sleep_health$Sleep.Duration, 
    method = "spearman")

# Spearman Rank value: 0.312 suggests a moderate positive correlation with stress



# Physical activity relationship with stress --------------------------------

# Visualisation of the relationship
ggplot(sleep_health, 
       aes(x = Physical.Activity.Level, y = Stress.Level)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Physical Activity and Stress Level",
    x = "Physical Activity Level (mins per day)",
    y = "Stress Level (1–10)"
  ) +
  theme_minimal(base_size = 14)

# Spearman Rank test for correlation 
cor(sleep_health$Physical.Activity.Level, 
    sleep_health$Stress.Level, 
    method = "spearman")

# Spearman Rank value: -0.119 suggests a weak negative correlation with stress



# Physical activity relationship with Sleep Duration ------------------

# Visualisation of the relationship
ggplot(sleep_health, 
       aes(x = Physical.Activity.Level, y = Sleep.Duration)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Physical Activity Level and Sleep Duration",
    x = "Physical Activity Level (mins per day)",
    y = "Sleep Duration (hours)"
  ) +
  theme_minimal(base_size = 14)

# Spearman Rank test for correllation 
cor(sleep_health$Physical.Activity.Level, 
    sleep_health$Sleep.Duration, 
    method = "spearman")

# Spearman Rank value: 0.277 suggests a weak positive correlation with stress

# Gender relationship with stress ------------------------------------

# Visualisation of the relationship
ggplot(sleep_health,
       aes(x = Gender, y = Stress.Level)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Stress Level by Gender",
    x = "Gender",
    y = "Stress Level (1 = Low, 10 = High)"
  ) +
  theme_minimal(base_size = 14)

# Mann-Whitney U Test to compare sleeep duration by gender as not normally 
# distributed
wilcox.test(Stress.Level ~ Gender,
            data = sleep_health,
            conf.int = TRUE)

# p-value < 0.001 - significant difference between males and females
# females have a lower score of approximately 1 point

# Gender relationship with sleep duration ------------------------------------

# Visualisation of the relationship
ggplot(sleep_health,
       aes(x = Gender, y = Sleep.Duration)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Sleep Duration by Gender",
    x = "Gender",
    y = "Sleep Duration (hours)"
  ) +
  theme_minimal(base_size = 14)

# Mann-Whitney U Test to compare sleeep duration by gender as not normally 
# distributed
wilcox.test(Sleep.Duration ~ Gender,
            data = sleep_health,
            conf.int = TRUE)

# p-value = 0.3328 - no significant difference between males and females



# BMI relationship with stress ------------------------------------

# Create a new combined variable 
sleep_health$BMI_grouped <- ifelse(sleep_health$BMI.Category %in% c("Overweight", "Obese"), 
                                   "Overweight/Obese", 
                                   "Normal") 

# Visualisation of the relationship
ggplot(sleep_health,
       aes(x = BMI.Category, y = Stress.Level)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Stress Level by BMI Category",
    x = "BMI Category",
    y = "Stress Level (1 = Low, 10 = High)"
  ) +
  theme_minimal(base_size = 14)

# Median is higher in Overweight category than the other 2
# Some variation in IQR - Overweight has greatest variation
# There is a lot of overlap with the boxes - Median for Overweight is overlapped
# by the Obese box, but not the Normal box. Unlikely to be a difference overall.

# BMI Category relationship with sleep duration ------------------------------------

# Visualisation of the relationship
ggplot(sleep_health,
       aes(x = BMI.Category, y = Sleep.Duration)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Sleep Duration by BMI Category",
    x = "BMI Category",
    y = "Sleep Duration (hours)"
  ) +
  theme_minimal(base_size = 14)

# There is greater differences among the medians
# IQR is similar for Normal and Overweight indicating similar variation
# There is no overlap of boxes for Normal and Overweight - limited number of
# data points in the Obese section so not as conclusive.



# *** Linear Regression ---------------------------------------------------


# Fitting univariate models ----------------------------------------------


# Stress model ------------------------------------------------------------
uni_model_stress <- lm(Sleep.Duration ~ Stress.Level, data = sleep_health)

# Summary of the fitted model
summary(uni_model_stress)

# Results indicate that when stress level is at 0, the predicted sleep duration
# is 9.13 hours.
# For every 1 unit increase in stress level, sleep duration will decrease by 
# 0.37 hours.
# There is a highly significant association between stress level and sleep
# duration (p-value < 0.0001)


# Age model ---------------------------------------------------------------
uni_model_age <- lm(Sleep.Duration ~ Age, data = sleep_health)

# Summary of the fitted model
summary(uni_model_age)

# Results indicate that when age is 0, the predicted sleep duration
# is 5.82 hours.
# For every 1 year increase in age, sleep duration will increase by 
# 0.03 hours.
# There is a highly significant association between age and sleep
# duration (p-value < 0.0001)


# Physical activity model -------------------------------------------------
uni_model_activity <- lm(Sleep.Duration ~ Physical.Activity.Level, data = sleep_health)

# Summary of the fitted model
summary(uni_model_activity)

# Results indicate that when activity level is 0, the predicted sleep duration
# is 6.49 hours.
# For every 1 minute increase in activity level, sleep duration will increase by 
# 0.01 hours.
# There is a significant association between age and sleep
# duration (p-value = 0.0018)


# Gender model ------------------------------------------------------------
uni_model_gender <- lm(Sleep.Duration ~ Gender, data = sleep_health)

# Summary of the fitted model
summary(uni_model_gender)

# Results indicate that for females, the predicted sleep duration
# is 7.15 hours.
# Males are likely to have 0.14 hours less sleep than females.
# There is no significant difference between gender and sleep
# duration (p-value = 0.311)


# BMI model ---------------------------------------------------------------
# changing bmi category to an un-ordered factor
sleep_health <- sleep_health |>
  mutate(
    BMI.Category = factor(BMI.Category,
                          ordered = FALSE)
  )

uni_model_bmi <- lm(Sleep.Duration ~ BMI.Category, data = sleep_health)

# Summary of the fitted model
summary(uni_model_bmi)

# A normal BMI would predict a sleep duration of 7.34 hrs
# An overweight BMI would predict a sleep duration of 0.60 hrs less than those
# with a normal BMI which is significantly different (p-value < 0.0001)
# An obese BMI would predict a sleep duration of 0.44 hrs less than those with
# a normal BMI, but this is not a significant difference (p-value = 0.125)

# Type III test to investigate if BMI variable has an overall impact on
# sleep duration

car::Anova(uni_model_bmi, type = "III")

# BMI category has a Type III p-value < 0.0001 so has a highly significant 
# association with sleep duration






#*** Q3 possible input - Chihiro -------------------------



# Creating sleep duration vs stress level category boxplot 
n_levels <- length(unique(sleep_health$Stress.Level)) 

ggplot(sleep_health, aes(x = factor(Stress.Level), y = Sleep.Duration, 
                         fill = factor(Stress.Level))) + 
  geom_boxplot() + 
  scale_fill_manual(values = colorRampPalette(c("limegreen", "yellow", "red"), space = "Lab")(n_levels)) + 
  labs(title = "Sleep Duration by Stress Level", 
       x = "Stress Level", y = "Sleep Duration (hours per day)") + 
  theme_minimal(base_size = 14) + 
  theme(legend.position = "none") 

# Save image
ggsave("images/sleep_duration_and_stress_level_boxplot.svg", width = 8, height = 6, dpi = 300)

# Load the ggplot2 package
library(ggplot2)

# Generate QQ plot using ggplot for Sleep Duration
ggplot(sleep_health, aes(sample = Sleep.Duration)) +
  stat_qq(size = 2) +
  stat_qq_line(color = "blue", linetype = "dashed", lwd = 1.1) +
  labs(title = "Normal QQ Plot of Sleep Duration",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal(base_size = 14)

# Performing Shapiro-Wilk test for normality for Sleed Duration
shapiro.test(sleep_health$Sleep.Duration)

###############################################################################



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



#******* Christ's Linear Regression Code ------------------------------

load('data/SleepHealth_Updated.rda')

library(broom)
library(car)
library(lmtest)
library(ggplot2)

rq3_vars <- sleep_health %>%  select(Sleep.Duration, Stress.Level, Physical.Activity.Level, Age)

rq3_summary <- rq3_vars %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    n      = n(),
    Mean   = mean(Value),
    SD     = sd(Value),
    Median = median(Value),
    IQR    = IQR(Value),
    Min    = min(Value),
    Max    = max(Value),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.double), ~ round(.x, 2)))
cat("\n Descriptive statistics \n"); print(rq3_summary, width = Inf)

# Pearson correlations with Sleep Duration (bivariate, before adjustment)
cat("\n Pearson correlation matrix \n")
print(round(cor(rq3_vars), 3))


p_rq3_scatter <- ggplot(sleep_health, aes(x = Stress.Level, y = Sleep.Duration, colour = Gender)) +
  geom_jitter(alpha = 0.55, size = 2, width = 0.12, height = 0.03) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.1) +
  #scale_colour_manual(values = gender_cols) +
  scale_x_continuous(breaks = 3:8) +
  labs(
    title    = "Stress Level vs Sleep Duration by Gender",
    subtitle = "Points slightly jittered to show overlapping values; lines = least-squares fit with 95% CI",
    x = "Stress Level (scale 1-10)", y = "Sleep Duration (hours)", colour = "Gender"
  )
print(p_rq3_scatter)

ggsave("images/Q3_scatter.png", width = 8, height = 6, dpi = 300)

####Correlation test for the primary predictor 
#  H0: rho = 0 (no linear association)   H1: rho != 0
rq3_cor <- cor.test(sleep_health$Stress.Level, sleep_health$Sleep.Duration, method = "pearson")
cat("\n=== Pearson correlation: Stress Level vs Sleep Duration ===\n"); print(rq3_cor)



### Model: Sleep.Duration = b0 + b1*Stress + b2*Activity + b3*Age + b4*GenderMale + e
#  Female is the reference group, so b4 = difference (Male - Female) in hours

#  Primary hypothesis: H0: b1 = 0   vs   H1: b1 < 0 (higher stress, shorter sleep)

model <- lm(Sleep.Duration ~ Stress.Level + Physical.Activity.Level + Age + Gender,
            data = sleep_health)


cat("\n=== RQ3: Full model summary ===\n"); print(summary(model))

################################################################################
rq3_coefs <- tidy(model, conf.int = TRUE) %>%
  mutate(across(where(is.double), ~ signif(.x, 4)))
rq3_fit <- glance(model) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df, df.residual, nobs) %>%
  rename(F.statistic = statistic, F.p.value = p.value)
cat("\n=== Coefficient table ===\n"); print(rq3_coefs, width = Inf)
cat("\n=== Model fit ===\n");        print(rq3_fit, width = Inf)

# Standardised (beta) coefficients: compare predictors on a common scale
model_std <- lm(scale(Sleep.Duration) ~ scale(Stress.Level) +
                  scale(Physical.Activity.Level) + scale(Age) + Gender, data = sleep_health)
cat("\n Standardised coefficients \n")
print(round(coef(model_std)[2:4], 3))


###################################
rq3_vif <- vif(model)
cat("\n Variance Inflation Factors \n"); print(round(rq3_vif, 3))


###############################################################################
diag_df <- augment(model) %>% mutate(Gender = sleep_health$Gender)


# H0: residuals are normally distributed.
rq3_shapiro <- shapiro.test(residuals(model))
cat("\n Shapiro-Wilk on residuals \n"); print(rq3_shapiro)

p_rq3_qq <- ggplot(diag_df, aes(sample = .std.resid)) +
  stat_qq(colour = "#2471A3", alpha = 0.6) +
  stat_qq_line(colour = "#C0392B", linewidth = 0.9) +
  labs(title = "Normal Q-Q Plot of Standardised Residuals",
       subtitle = sprintf("Shapiro-Wilk W = %.3f, p = %.3g",
                          rq3_shapiro$statistic, rq3_shapiro$p.value),
       x = "Theoretical quantiles", y = "Standardised residuals")
print(p_rq3_qq)

ggsave("images/Q3_qq.png", width = 8, height = 6, dpi = 300)

p_rq3_hist <- ggplot(diag_df, aes(x = .resid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 fill = "#AED6F1", colour = "white") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sigma(model)),
                colour = "#C0392B", linewidth = 1) +
  labs(title = "Histogram of Residuals with Normal Curve",
       subtitle = "Red curve = normal distribution with the model's residual SD",
       x = "Residual (hours)", y = "Density")
print(p_rq3_hist)

ggsave("images/Q3_hist.png", width = 8, height = 6, dpi = 300)

# ## H0: residual variance is constant.
rq3_bp <- bptest(model)
cat("\n Breusch-Pagan test \n"); print(rq3_bp)

p_rq3_resfit <- ggplot(diag_df, aes(x = .fitted, y = .resid, colour = Gender)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 0.8) +
  geom_jitter(alpha = 0.6, size = 2, width = 0.02, height = 0) +
  geom_smooth(aes(group = 1), method = "loess", formula = y ~ x,
              se = FALSE, colour = "black", linewidth = 0.8) +
  #scale_colour_manual(values = gender_cols) +
  labs(title = "Residuals vs Fitted Values",
       subtitle = sprintf("Breusch-Pagan BP = %.2f, df = %d, p = %.3g; black line = loess trend",
                          rq3_bp$statistic, rq3_bp$parameter, rq3_bp$p.value),
       x = "Fitted Sleep Duration (hours)", y = "Residual (hours)", colour = "Gender")
print(p_rq3_resfit)

ggsave("images/Q3_fit.png", width = 8, height = 6, dpi = 300)

# Supporting checks: influential points, and heteroscedasticity-robust (HC3)
# standard errors, which give valid inference when the variance is NOT constant.
cat("Observations with Cook's D > 4/n:", sum(diag_df$.cooksd > 4 / nrow(df)), "\n")
cat("\n=== Coefficients with robust (HC3) standard errors ===\n")
rq3_robust <- coeftest(model, vcov. = hccm(model, type = "hc3"))
print(rq3_robust)


##############################################################################

###############################################################################


