# Load packages and functions ---------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source("code/functions/group_center_variable.R")

# Load and prepare data ---------------------------------------------------

data <- read_csv("data/database.csv")
data <- data %>% select(subj, time, group, LS_BMD, LS_BMD_adjust)

# Code the group and time variables as factors
data$group <- as.factor(data$group)
data$time <- as.factor(data$time)

# Set contrasts of variable group to deviation
contrasts(data$group) <- matrix(rev(contr.sum(2)), ncol = 1)

# Set contrasts of variable time to polynomial
contrasts(data$time) <- contr.poly(4)

# Center variable
data <- group_center_variable(data, "LS_BMD_adjust")

# Build models ------------------------------------------------------------

# Model 1: correction for baseline differences
LS_LMM <- lmer(
  formula = LS_BMD ~ 1 + group + time + group:time + LS_BMD_adjust + (1 | subj),
  data = data
)

# R-squared
rsquared(LS_LMM)

# Fixed effects test
anova(LS_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(LS_LMM)

# Estimated marginal means for group
group_emm <- emmeans(LS_LMM, ~ group)

# Estimated marginal means for time
time_emm <- emmeans(LS_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_emm  <- emmeans(LS_LMM, ~ group:time)

# Post hocs ---------------------------------------------------------------

ph_none <- pairs(interaction_emm, adjust = "none")
ph_bonf <- pairs(interaction_emm, adjust = "bonferroni")
ph_holm <- pairs(interaction_emm, adjust = "holm")

# Plot models -------------------------------------------------------------

# Model 1
# Put the interaction emmeans into a data frame
interaction_emm_df <- interaction_emm %>% as.data.frame()

# Plot the model
dodge <- position_dodge(0.1)
plot_LS_BMD_adjusted <- ggplot(data = interaction_emm_df) +
  geom_point(
    aes(x = time, y = emmean, colour = group),
    position = dodge, size = 2
  ) +
  geom_line(
    aes(x = time, y = emmean, colour = group, group = group),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, colour = group), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  labs(
    x = "Time",
    y = "Lumbar spine BMD",
    colour = "Group" 
  ) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 2, 0.01))