############################################################################
										                            		
## Question 1 (ANOVA) - Is there a relationship between quality of sleep and heart rate?
											
############################################################################

### !!!!!! Make sure to PULL from the repository before making any updates !!!!! #####

# IV: sleep quality category, DV: heart rate (bpm). Test: one-way ANOVA.


library(ggplot2)
library(dplyr)
library(car)

out_dir <- "images/Q1"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 16))
# ColorBrewer palettes: Set2 for filled areas, Dark2 for points and lines
dark2 <- RColorBrewer::brewer.pal(8, "Dark2")

save_plot <- function(p, file, w = 9, h = 6) {
  print(p)
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 200, bg = "white")
}

# ---- Data -------------------------------------------------------------
# cleaned data created in "0 - General Dataset Analysis.R"
load("data/SleepHealth_Updated.rda")
sleep <- sleep_health

# 1-2 very poor, 3-4 poor, 5-6 fair, 7-8 good, 9-10 excellent
sleep$Sleep.Quality.Cat <- cut(sleep$Quality.of.Sleep,
                               breaks = c(0, 2, 4, 6, 8, 10),
                               labels = c("Very poor", "Poor", "Fair", "Good", "Excellent"))
table(sleep$Sleep.Quality.Cat)

# no observations in "Very poor", so drop the empty level
sleep$Sleep.Quality.Cat <- droplevels(sleep$Sleep.Quality.Cat)

# H0: mean heart rate is equal across sleep quality groups
# H1: at least one group mean differs

# ---- Summary statistics -----------------------------------------------
hr_summary <- sleep %>%
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
write.csv(hr_summary, file.path(out_dir, "q1_hr_summary.csv"), row.names = FALSE)

# ---- Plots ------------------------------------------------------------
p_box <- ggplot(sleep, aes(Sleep.Quality.Cat, Heart.Rate, fill = Sleep.Quality.Cat)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.12, height = 0.25, alpha = 0.45, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Heart rate by sleep quality category",
       x = "Sleep quality category", y = "Resting heart rate (bpm)")
save_plot(p_box, "q1_boxplots.png")

p_means <- ggplot(hr_summary, aes(Sleep.Quality.Cat, mean, colour = Sleep.Quality.Cat, group = 1)) +
  geom_line(colour = "grey60", linetype = "dashed") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.12, linewidth = 1) +
  geom_point(size = 4) +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  labs(title = "Mean heart rate with 95% confidence intervals",
       x = "Sleep quality category", y = "Mean heart rate (bpm)")
save_plot(p_means, "q1_means_ci.png")

# ---- Normality within groups -------------------------------------------
p_hist <- ggplot(sleep, aes(Heart.Rate, fill = Sleep.Quality.Cat)) +
  geom_histogram(binwidth = 2, colour = "white", show.legend = FALSE) +
  facet_wrap(~ Sleep.Quality.Cat, nrow = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Histograms of heart rate by group", x = "Heart rate (bpm)", y = "Count")
save_plot(p_hist, "q1_histograms.png", 11, 5)

p_qq <- ggplot(sleep, aes(sample = Heart.Rate, colour = Sleep.Quality.Cat)) +
  stat_qq(show.legend = FALSE) + stat_qq_line(colour = "black") +
  facet_wrap(~ Sleep.Quality.Cat, nrow = 1, scales = "free_y") +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Normal Q-Q plots by group", x = "Theoretical quantiles", y = "Heart rate (bpm)")
save_plot(p_qq, "q1_qq_plots.png", 11, 5)

# Shapiro-Wilk per group (note: Poor has n = 3)
tapply(sleep$Heart.Rate, sleep$Sleep.Quality.Cat, shapiro.test)

# ---- ANOVA assumptions -------------------------------------------------
model <- aov(Heart.Rate ~ Sleep.Quality.Cat, data = sleep)

shapiro.test(residuals(model))                                        # normality of residuals
leveneTest(Heart.Rate ~ Sleep.Quality.Cat, data = sleep, center = median)  # equal variances
bartlett.test(Heart.Rate ~ Sleep.Quality.Cat, data = sleep)

resid_df <- data.frame(res = residuals(model), fit = fitted(model),
                       group = sleep$Sleep.Quality.Cat)

p_rqq <- ggplot(resid_df, aes(sample = res)) +
  stat_qq() + stat_qq_line(colour = dark2[2]) +
  labs(title = "Normal Q-Q plot of residuals", x = "Theoretical quantiles", y = "Residuals")
save_plot(p_rqq, "q1_residual_qq.png")

p_rvf <- ggplot(resid_df, aes(fit, res, colour = group, shape = group)) +
  geom_jitter(width = 0.08, height = 0.15, alpha = 0.8, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Residuals vs fitted values", x = "Fitted values (group means)",
       y = "Residuals", colour = "Group", shape = "Group")
save_plot(p_rvf, "q1_residuals_vs_fitted.png")

# ---- One-way ANOVA ------------------------------------------------------
summary(model)

anova_tab <- summary(model)[[1]]
eta_sq <- anova_tab$`Sum Sq`[1] / sum(anova_tab$`Sum Sq`)
eta_sq

# robustness checks (residuals not normal, unequal group sizes)
oneway.test(Heart.Rate ~ Sleep.Quality.Cat, data = sleep, var.equal = FALSE)
kruskal.test(Heart.Rate ~ Sleep.Quality.Cat, data = sleep)

# ---- Post-hoc tests -----------------------------------------------------
tukey <- TukeyHSD(model)
tukey

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
games_howell(sleep$Heart.Rate, sleep$Sleep.Quality.Cat)

pairwise.wilcox.test(sleep$Heart.Rate, sleep$Sleep.Quality.Cat,
                     p.adjust.method = "bonferroni", exact = FALSE)

tk <- as.data.frame(tukey$Sleep.Quality.Cat)
tk$comparison <- factor(rownames(tk), levels = rev(rownames(tk)))
p_tukey <- ggplot(tk, aes(diff, comparison)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2, colour = dark2[3]) +
  geom_point(size = 3, colour = dark2[2]) +
  labs(title = "Tukey HSD: differences in mean heart rate",
       x = "Difference in mean heart rate (bpm)", y = NULL)
save_plot(p_tukey, "q1_tukey.png", 10, 5)
