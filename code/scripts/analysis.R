# Load packages -----------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)

# Load and prepare data ---------------------------------------------------

data <- read_csv("data/database.csv")
data <- data %>% select(subj, time, group, LS_BMD, LS_BMD_adjust)

# Code the group and time variables as factors
data$Group <- as.factor(data$Group)
data$Time <- as.factor(data$Time)

# Set contrasts of variable Time to polynomial
contrasts(data$Time) <- contr.poly(4)

# Set contrasts of variable Group to deviation
contrasts(data$Group) <- matrix(rev(contr.sum(2)), ncol = 1)

# Center variable
data <- center_variable(data, "LS__BMD_adjust")

# Build models ------------------------------------------------------------

# ** Model 1: basic model -------------------------------------------------

model1 <- lmer(
  formula = LS__BMD ~ 1 + Group + Time + Group:Time + (1 | Subj),
  data = data
)

# R-squared
rsquared(model1)

# Fixed effects Omnibus test in jamovi
anova(model1, type = 3, test = "F")

# Random components and fixed effects parameters estimates in jamovi
summary(model1)

# Estimated marginal means for group
emmeans(model1, ~ Group)

# Estimated marginal means for time
emmeans(model1, ~ Time)

# Estimated marginal means for group x time interaction
emmeans(model1, ~ Group:Time)

# ** Model 2: correction for baseline differences -------------------------

model2 <- lmer(
  formula = LS__BMD ~ 1 + Time + Group + Group:Time + LS__BMD_adjust + (1 | Subj),
  data = data
)

# R-squared
rsquared(model2)

# Fixed effects Omnibus test in jamovi
anova(model2, type = 3, test = "F")

# Random components and fixed effects parameters estimates in jamovi
summary(model2)

# Estimated marginal means for group
group_emm <- emmeans(model2, ~ Group)

# Estimated marginal means for time
time_emm <- emmeans(model2, ~ Time)

# Estimated marginal means for group x time interaction
interaction_emm <- emmeans(model2, ~ Group:Time)

# Post hocs ---------------------------------------------------------------

ph_none <- pairs(interaction_emm, adjust = "none")
ph_bonf <- pairs(interaction_emm, adjust = "bonferroni")
ph_holm <- pairs(interaction_emm, adjust = "holm")

# Plot models -------------------------------------------------------------

# Model 2 (adjusted for baseline differences)
# Put the interaction emmeans into a data frame
interaction_emm_df <- interaction_emm %>% as.data.frame()

# Plot the model
dodge <- position_dodge(0.1)
plot_LS_BMD_adjusted <- ggplot(data = interaction_emm_df) +
  geom_point(
    aes(x = Time, y = emmean, colour = Group),
    position = dodge, size = 2
  ) +
  geom_line(
    aes(x = Time, y = emmean, colour = Group, group = Group),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = Time, ymin = lower.CL, ymax = upper.CL, colour = Group), 
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