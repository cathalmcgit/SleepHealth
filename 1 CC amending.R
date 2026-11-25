############################################################################

## Question 1 (ANOVA) - Is there a relationship between quality of sleep and heart rate?

############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####

# IV: sleep quality category, DV: heart rate (bpm). Test: one-way ANOVA.


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

p_qq_all <- ggplot(sleep_health, aes(sample = Heart.Rate)) +
  stat_qq(colour = dark2[1]) +
  stat_qq_line(colour = "black") +
  labs(title = "Normal Q-Q plot of heart rate",
       x = "Theoretical quantiles", y = "Heart rate (bpm)")
print(p_qq_all)

# Shapiro-Wilk per group
print(tapply(sleep_health$Heart.Rate, sleep_health$Sleep.Quality.Cat, shapiro.test))

# ---- ANOVA assumptions -------------------------------------------------
model <- aov(Heart.Rate ~ Sleep.Quality.Cat, data = sleep_health)
resid_df <- data.frame(res = residuals(model), fit = fitted(model),
                       group = sleep_health$Sleep.Quality.Cat)


print(shapiro.test(residuals(model))) # normality of residuals
print(leveneTest(Heart.Rate ~ Sleep.Quality.Cat, data = sleep_health, center = median))  # equal variances
print(bartlett.test(Heart.Rate ~ Sleep.Quality.Cat, data = sleep_health))



p_rqq <- ggplot(resid_df, aes(sample = res)) +
  stat_qq() + stat_qq_line(colour = dark2[2]) +
  labs(title = "Normal Q-Q plot of residuals", x = "Theoretical quantiles", y = "Residuals")
print(p_rqq)

p_rvf <- ggplot(resid_df, aes(fit, res, colour = group, shape = group)) +
  geom_jitter(width = 0.08, height = 0.15, alpha = 0.8, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Residuals vs fitted values", x = "Fitted values (group means)",
       y = "Residuals", colour = "Group", shape = "Group")
print(p_rvf)

# ---- One-way ANOVA ------------------------------------------------------
print(summary(model))

anova_tab <- summary(model)[[1]]
eta_sq <- anova_tab$`Sum Sq`[1] / sum(anova_tab$`Sum Sq`)
print(eta_sq)

# robustness checks (residuals not normal, unequal group sizes)
print(oneway.test(Heart.Rate ~ Sleep.Quality.Cat, data = sleep_health, var.equal = FALSE))
print(kruskal.test(Heart.Rate ~ Sleep.Quality.Cat, data = sleep_health))

# ---- Post-hoc tests -----------------------------------------------------
tukey <- TukeyHSD(model)
print(tukey)

# Games-Howell (does not assume equal variances)
games_howell <- function(y, g) {
  lv <- levels(g); k <- length(lv)
  n <- tapply(y, g, length); m <- tapply(y, g, mean); v <- tapply(y, g, var)
  res <- NULL
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    a <- lv[i]; b <- lv[j]
    se <- sqrt(v[a] / n[a] + v[b] / n[b])
    df <- se^4 / ((v[a] / n[a])^2 / (n[a] - 1) + (v[b] / n[b])^2 / (n[b] - 1))
    p <- ptukey(abs(m[b] - m[a]) / se * sqrt(2), k, df, lower.tail = FALSE)
    res <- rbind(res, data.frame(comparison = paste(b, "-", a),
                                 diff = round(m[b] - m[a], 2), p.adj = signif(p, 3)))
  }
  rownames(res) <- NULL
  res
}
print(games_howell(sleep_health$Heart.Rate, sleep_health$Sleep.Quality.Cat))

print(pairwise.wilcox.test(sleep_health$Heart.Rate, sleep_health$Sleep.Quality.Cat,
                           p.adjust.method = "bonferroni", exact = FALSE))

tk <- as.data.frame(tukey$Sleep.Quality.Cat)
tk$comparison <- factor(rownames(tk), levels = rev(rownames(tk)))
p_tukey <- ggplot(tk, aes(diff, comparison)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2, colour = dark2[3]) +
  geom_point(size = 3, colour = dark2[2]) +
  labs(title = "Tukey HSD: differences in mean heart rate",
       x = "Difference in mean heart rate (bpm)", y = NULL)
print(p_tukey)