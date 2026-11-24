############################################################################

## Question 3 - Controlling for other factors, can decreasing your stress level 
##              increase your sleep duration?

############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####


# Libraries used ----------------------------------------------------------
library(tidyverse)
library(dplyr)

# Load updated Sleep Health dataset
load('data/SleepHealth_Updated.rda')

# Descriptive stats for stress level --------------------------------------
summary(sleep_health$Stress.Level)

# Descriptive stats for sleep duration --------------------------------------
summary(sleep_health$Sleep.Duration)


##### Q3 possible input - Chihiro ###########

# summary statistics of sleep duration variable 
summary(sleep_health$Sleep.Duration) 

# summary statistics of stress level variable 
summary(sleep_health$Stress.Level) 

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

###############################################################################

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



##### Christ's Linear Regression Code

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
