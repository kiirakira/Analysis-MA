df <- read.csv("/Users/kirahoos/Library/Mobile Documents/com~apple~CloudDocs/Documents/Master Thesis/4 Data/Real_Final_DataSet.xls")

library(data.table)
library(psych)
library(lme4)
library(lmerTest)
library(effectsize)
library(emmeans)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(influence.ME)
library(merTools)

# Descriptives
descriptives <- df %>%
  group_by(stim) %>%
  summarise(
    N = n(),
    Mean_d_prime = mean(d_prime, na.rm = TRUE),
    SD_d_prime = sd(d_prime, na.rm = TRUE),
    Min_d_prime = min(d_prime, na.rm = TRUE),
    Max_d_prime = max(d_prime, na.rm = TRUE),
    Mean_median_rt = mean(median_rt, na.rm = TRUE),
    SD_median_rt = sd(median_rt, na.rm = TRUE),
    Min_median_rt = min(median_rt, na.rm = TRUE),
    Max_median_rt = max(median_rt, na.rm = TRUE)
  )


# Convert columns to factors
df$stim <- factor(df$stim)
df$session <- factor(df$session)
df$participant <- factor(df$participant)

# Set reference levels
df$stim <- relevel(df$stim, ref = "sham")
df$session <- relevel(df$session, ref = "1")

### LMM for d prime ###

# Define models
md0 <- lmer(d_prime ~ (1 | participant), data = df, REML = FALSE) # Baseline model
md1 <- lmer(d_prime ~ stim + (1 | participant), data = df, REML = FALSE)
md2 <- lmer(d_prime ~ stim + session + (1 | participant), data = df, REML = FALSE)
md3 <- lmer(d_prime ~ stim + session + age + (1 | participant), data = df, REML = FALSE)
md4 <- lmer(d_prime ~ stim * age + session + (1 | participant), data = df, REML = FALSE)
md5 <- lmer(d_prime ~ stim * session + age + (1 | participant), data = df, REML = FALSE)
md6 <- lmer(d_prime ~ stim * session * age + (1 | participant), data = df, REML = FALSE)

# Check model fit
anova(md1, md2) # m2 better
anova(md2, md3) # age almost significantly improves the model (p = 0.052)
anova(md3, md4) # m3 better
anova(md3, md5) # m3 better
anova(md3, md6) # m3 better

# m3 is the best fitting model

## Assumptions for m3
# Linearity & Homoscedasticity
# Plot residuals
plot(fitted(md3), residuals(md3),
     pch = 19, col = "blue")
abline(h = 0, lty = 2, col = "red")

# Normality of Residuals
# Q_Q Plot
qqnorm(residuals(md3))
qqline(residuals(md3), col = "red")

# Histogram
model_residuals <- residuals(md3)
hist(model_residuals)

#Kurtosis
describe(residuals_md3)

# Linearity, Homoscedasticity & Normality not violated

## Results model 2
summary(md3)
coef(md3)
confint(md3) # 95% CI
anova(md3) # factor level
eta_squared(md3, partial = TRUE)
emmeans(md3, pairwise ~ stim)




### LMM for reaction time ###

# Define models
mr0 <- lmer(median_rt ~ (1 | participant), data = df, REML = FALSE) # Baseline model
mr1 <- lmer(median_rt ~ stim + (1 | participant), data = df, REML = FALSE) 
mr2 <- lmer(median_rt ~ stim + session + (1 | participant), data = df, REML = FALSE) 
mr3 <- lmer(median_rt ~ stim + session + age + (1 | participant), data = df, REML = FALSE) 
mr4 <- lmer(median_rt ~ stim * session + (1 | participant), data = df, REML = FALSE)
mr5 <- lmer(median_rt ~ stim * age + session + (1 | participant), data = df, REML = FALSE)
mr6 <- lmer(median_rt ~ stim * session * age + (1 | participant), data = df, REML = FALSE) 

anova(mr1, mr2) # m2 better
anova(mr2, mr3) # m2 better, age does not significantly improve the model
anova(mr2, mr4) # m2 better
anova(mr2, mr5) # m2 better
anova(mr2, mr6) # m2 better

# m2 is the best fitting model

## Assumptions
# Linearity & Homoscedasticity
# Residuals
plot(fitted(mr2), residuals(mr2),
     pch = 19, col = "blue")
abline(h = 0, lty = 2, col = "red")

# Normality of Residuals
# Q_Q Plot
qqnorm(residuals(mr2))
qqline(residuals(mr2), col = "red")

# Histogram
model_residuals <- residuals(mr2)
hist(model_residuals)

#Kurtosis
describe(residuals_mr2)

# Linearity, Homoscedasticity & Normality not violated

## Results model 2
summary(mr2)
confint(mr2)
anova(mr2)
eta_squared(mr2, partial = TRUE)




### Plot results ###
## Effect of stim

df$stim <- factor(df$stim, levels = c("theta", "gamma", "sham"))

# EMMs
emm_dprime <- emmeans(md3, ~ stim)
emm_dprime_df <- as.data.frame(emm_dprime)

emm_rt <- emmeans(mr2, ~ stim)
emm_rt_df <- as.data.frame(emm_rt)

emm_dprime_df$stim <- factor(emm_dprime_df$stim, levels = c("theta", "gamma", "sham"))


# d_prime
plot_dprime <- ggplot(emm_dprime_df, aes(x = stim, y = emmean)) +
  geom_bar(stat = "identity", fill = "#99CCFF", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(x = "Stimulation",
       y = "EMM d'") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

emm_rt_df$stim <- factor(emm_rt_df$stim, levels = c("theta", "gamma", "sham"))

# Reaction time
plot_rt <- ggplot(emm_rt_df, aes(x = stim, y = emmean)) +
  geom_bar(stat = "identity", fill = "#FFCC99", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(x = "Stimulation",
       y = "EMM Reaction Time (s)") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Combine plots
grid.arrange(plot_dprime, plot_rt, ncol = 2)


## Effect of Session
# EMMs
emm_dprime_session <- emmeans(md3, ~ session)
emm_dprime_session_df <- as.data.frame(emm_dprime_session)

emm_rt_session <- emmeans(mr2, ~ session)
emm_rt_session_df <- as.data.frame(emm_rt_session)

# d_prime
plot1d <- ggplot(emm_dprime_session_df, aes(x = session, y = emmean)) +
  geom_bar(stat = "identity", fill = "#99CCFF", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(x = "Session",
       y = "EMM d'") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


# Reaction time
plot1rt <- ggplot(emm_rt_session_df, aes(x = session, y = emmean)) +
  geom_bar(stat = "identity", fill = "#FFCC99", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(x = "Session",
       y = "EMM Reaction Time (s)") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Combine plots
grid.arrange(plot1d, plot1rt, ncol = 2)


## Plotting age
ggplot(df, aes(x = age, y = d_prime)) +
  geom_point(alpha = 0.5, color = "darkgrey") +
  geom_smooth(method = "lm", color = "#99CCFF", se = TRUE) + 
  labs(x = "Age", y = "d'") +
  scale_x_continuous(limits = c(NA, 80)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


## Plotting sine wave ##
x <- seq(0, 10 * pi, length.out = 1000)
y <- 0.1 * sin(x)  

df <- data.frame(x = x, y = y)

ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "#D0506D", size = 2) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "transparent", color = NA))


